#![feature(box_patterns)]
pub mod selfconvert;
pub mod typing;
use ast::expr;
use hir::expr::Expr as Hir;
use hir::expr::Value as Hvalue;
use mmmtype::Type;
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;
use utils::error::ReportableError;
use utils::{environment::Environment, error, metadata::WithMeta};

type Evalenv = Environment<Rc<WithMeta<Hvalue>>>;

#[derive(Clone, Debug)]
pub enum ErrorKind {
    InvalidValue(ast::expr::Literal),
    NotFound(String),
    Misc(&'static str),
}
#[derive(Clone, Debug)]
pub struct Error(ErrorKind, utils::metadata::Span);

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.0 {
            ErrorKind::InvalidValue(x) => write!(
                f,
                "The Literal {:?} should not be found in ast evaluation",
                x
            ),
            ErrorKind::NotFound(s) => write!(f, "The value {} not found", s),
            ErrorKind::Misc(s) => write!(f, "{}", s),
        }
    }
}
impl std::error::Error for Error {}

impl error::ReportableError for Error {
    fn get_span(&self) -> std::ops::Range<usize> {
        self.1.clone()
    }
}

fn generate_literal(expr: WithMeta<expr::Literal>) -> hir::expr::Literal {
    match expr.0 {
        expr::Literal::Int(i) => hir::expr::Literal::Int(i),
        expr::Literal::Float(s) => hir::expr::Literal::Float(s),
        expr::Literal::String(s) => hir::expr::Literal::String(s),
        _ => unreachable!(),
    }
}

fn gen_hir(
    expr: WithMeta<expr::Expr>,
    typeenv: &Environment<Type>,
    evalenv: &mut Evalenv,
) -> Result<WithMeta<Hir>, Error> {
    let span = expr.1;

    let hir: Result<Hir, Error> = match expr.0 {
        expr::Expr::Literal(l) => Ok(Hir::Literal(generate_literal(WithMeta::<_>(
            l,
            span.clone(),
        )))),
        expr::Expr::Var(s, opt_time) => {
            let v_opt = evalenv.get_bound_value(s.clone()).map_or_else(
                || {
                    Rc::new(WithMeta(
                        hir::expr::Value {
                            id: s,
                            v: RefCell::new(None),
                        },
                        span.clone(),
                    ))
                },
                |v| v.clone(),
            );
            Ok(Hir::Var(v_opt, opt_time))
        }
        expr::Expr::Tuple(vec) => {
            let hvec: Result<Vec<_>, Error> = vec
                .iter()
                .map(|v| gen_hir(v.clone(), typeenv, evalenv))
                .collect();
            Ok(Hir::Tuple(hvec?))
        }
        expr::Expr::Proj(v, idx) => Ok(Hir::Proj(Box::new(gen_hir(*v, typeenv, evalenv)?), idx)),
        expr::Expr::Apply(fun, callee) => {
            let mut this = |e| Ok(Box::new(gen_hir(e, typeenv, evalenv)?));
            Ok(Hir::Apply(
                this(*fun)?,
                callee
                    .iter()
                    .map(|v| gen_hir(v.clone(), typeenv, evalenv))
                    .collect::<Result<Vec<_>, _>>()?,
            ))
        }
        expr::Expr::Lambda(params, body) => {
            let hparams: Vec<Rc<WithMeta<Hvalue>>> = params
                .iter()
                .map(|WithMeta::<_>(p, s)| {
                    let nv = Rc::new(WithMeta::<_>(Hvalue::new(p.id.clone()), s.clone()));
                    evalenv.add_bind(p.id.clone(), Rc::clone(&nv));
                    nv
                })
                .collect();
            let mut this = |e| Ok(Box::new(gen_hir(e, typeenv, evalenv)?));
            Ok(Hir::Lambda(hparams, this(*body)?))
        }
        expr::Expr::Let(id, body, then) => {
            let hbody = Box::new(gen_hir(*body, typeenv, evalenv)?);
            evalenv.extend();
            let nv = Rc::new(WithMeta::<_>(Hvalue::new(id.id.clone()), span.clone())); //todo add span to typedid
            evalenv.add_bind(id.id, Rc::clone(&nv));
            let hthen = then
                .map(|t| Ok(Box::new(gen_hir(*t, typeenv, evalenv)?)))
                .transpose()?;
            Ok(Hir::Let(Rc::clone(&nv), hbody, hthen))
        }
        expr::Expr::LetRec(id, body, then) => {
            evalenv.extend();
            let nv = Rc::new(WithMeta::<_>(Hvalue::new(id.id.clone()), span.clone())); //todo add span to typedid
            evalenv.add_bind(id.id, Rc::clone(&nv));
            let hbody = Box::new(gen_hir(*body, typeenv, evalenv)?);
            let hthen = then
                .map(|t| Ok(Box::new(gen_hir(*t, typeenv, evalenv)?)))
                .transpose()?;
            Ok(Hir::Let(Rc::clone(&nv), hbody, hthen))
        }
        expr::Expr::Feed(id, body) => {
            evalenv.extend();
            let nv = Rc::new(WithMeta::<_>(Hvalue::new(id.clone()), span.clone())); //todo add span to typedid
            evalenv.add_bind(id, Rc::clone(&nv));
            let hbody = Box::new(gen_hir(*body, typeenv, evalenv)?);
            Ok(Hir::Feed(Rc::clone(&nv), hbody))
        }
        expr::Expr::Block(opt_body) => Ok(Hir::Block(
            opt_body
                .map(|body| Ok(Box::new(gen_hir(*body, typeenv, evalenv)?)))
                .transpose()?,
        )),
        expr::Expr::If(cond, then, opt_else) => {
            let mut this = |e| Ok(Box::new(gen_hir(e, typeenv, evalenv)?));
            Ok(Hir::If(
                this(*cond)?,
                this(*then)?,
                opt_else.map(|e| this(*e)).transpose()?,
            ))
        }
        _ => todo!(),
    };
    hir.map(|h| WithMeta::<_>(h, span))
}

pub fn generate_hir(
    expr: WithMeta<expr::Expr>,
) -> Result<WithMeta<Hir>, Vec<Box<dyn ReportableError>>> {
    let expr_without_self = selfconvert::convert_self_top(expr);
    let mut infer_ctx = typing::InferContext::new();
    let _toptype = typing::infer_type(expr_without_self.clone().0, &mut infer_ctx);
    let mut evalenv = Evalenv::new();
    let mut errs = Vec::<Box::<dyn ReportableError>>::new();
    let res = gen_hir(expr_without_self, &infer_ctx.env, &mut evalenv)
        .map_err(|e: Error| errs.push(Box::new(e.clone())));
    res.map_err(|_e|errs)
}
