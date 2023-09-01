pub mod selfconvert;
pub mod typing;

use super::ast;
use super::hir::expr::Expr as Hir;
use super::hir::expr::Literal as HLiteral;
use super::hir::expr::Value as Hvalue;
use super::types::Type;
use super::utils::error::ReportableError;
use super::utils::{
    environment::Environment,
    error,
    metadata::{Span, WithMeta},
};
use std::fmt;
use std::rc::Rc;

type Evalenv = Environment<Box<WithMeta<Hvalue>>>;

#[derive(Clone, Debug)]
pub enum ErrorKind {
    InvalidValue(ast::Literal),
    NotFound(String),
    Misc(&'static str),
}
#[derive(Clone, Debug)]
pub struct Error(ErrorKind, Span);

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

fn generate_literal(expr: WithMeta<ast::Literal>) -> HLiteral {
    match expr.0 {
        ast::Literal::Int(i) => HLiteral::Int(i),
        ast::Literal::Float(s) => HLiteral::Float(s),
        ast::Literal::String(s) => HLiteral::String(s),
        _ => unreachable!(),
    }
}

fn gen_hir(
    expr_meta: WithMeta<ast::Expr>,
    typeenv: &Environment<Type>,
    evalenv: &mut Evalenv,
) -> Result<Box<WithMeta<Hir>>, Error> {
    let WithMeta(expr, span) = expr_meta;

    let hir: Result<Hir, Error> = match expr {
        ast::Expr::Literal(l) => Ok(Hir::Literal(generate_literal(WithMeta(l, span.clone())))),
        ast::Expr::Var(s, opt_time) => {
            let v_opt = evalenv.get_bound_value(s.clone()).map_or_else(
                || Box::new(WithMeta(Hvalue(s.clone()), span.clone())),
                |v| v.clone(),
            );
            Ok(Hir::Var(WithMeta(Hvalue(s), span.clone()).into(), opt_time))
        }
        ast::Expr::Tuple(vec) => {
            let hvec: Result<Vec<_>, Error> = vec
                .iter()
                .map(|v| gen_hir(v.clone(), typeenv, evalenv))
                .collect();
            Ok(Hir::Tuple(hvec?))
        }
        ast::Expr::Proj(v, idx) => Ok(Hir::Proj(gen_hir(*v, typeenv, evalenv)?, idx)),
        ast::Expr::Apply(fun, callee) => {
            let mut this = |e| Ok(gen_hir(e, typeenv, evalenv)?);
            Ok(Hir::Apply(
                this(*fun)?,
                callee
                    .iter()
                    .map(|v| gen_hir(v.clone(), typeenv, evalenv))
                    .collect::<Result<Vec<_>, _>>()?,
            ))
        }
        ast::Expr::Lambda(params, body) => {
            let hparams: Vec<Box<WithMeta<Hvalue>>> = params
                .iter()
                .map(|WithMeta(p, s)| {
                    // let nv = p.clone();
                    // evalenv.add_bind(p.id.clone(), WithMeta(nv.id, s));
                    Box::new(WithMeta(Hvalue(p.id.clone()), s.clone()))
                })
                .collect();
            let mut this = |e| Ok(Box::new(gen_hir(e, typeenv, evalenv)?));
            Ok(Hir::Lambda(hparams, *this(*body)?))
        }
        ast::Expr::Let(id, body, then) => {
            let hbody = gen_hir(*body, typeenv, evalenv)?;
            // evalenv.extend();
            let nv = Box::new(WithMeta::<_>(Hvalue(id.id.clone()), span.clone())); //todo add span to typedid
                                                                                   // evalenv.add_bind(id.id, Rc::clone(&nv));
            let hthen = then
                .map(|t| Ok(gen_hir(*t, typeenv, evalenv)?))
                .transpose()?;

            Ok(Hir::Let(nv, hbody, hthen))
        }
        ast::Expr::LetRec(id, body, then) => {
            // evalenv.extend();s
            let nv = Box::new(WithMeta::<_>(Hvalue(id.id.clone()), span.clone())); //todo add span to typedid
                                                                                   // evalenv.add_bind(id.id, Rc::clone(&nv));
            let hbody = gen_hir(*body, typeenv, evalenv)?;
            let hthen = then
                .map(|t| Ok(gen_hir(*t, typeenv, evalenv)?))
                .transpose()?;
            Ok(Hir::Let(nv, hbody, hthen))
        }
        ast::Expr::Feed(id, body) => {
            // evalenv.extend();
            let nv = Box::new(WithMeta::<_>(Hvalue(id.clone()), span.clone())); //todo add span to typedid
                                                                                // evalenv.add_bind(id, Rc::clone(&nv));
            let hbody = gen_hir(*body, typeenv, evalenv)?;
            Ok(Hir::Feed(nv, hbody))
        }
        ast::Expr::Block(opt_body) => Ok(Hir::Block(
            opt_body
                .map(|body| Ok(gen_hir(*body, typeenv, evalenv)?))
                .transpose()?,
        )),
        ast::Expr::If(cond, then, opt_else) => {
            let mut this = |e| Ok(gen_hir(e, typeenv, evalenv)?);
            Ok(Hir::If(
                this(*cond)?,
                this(*then)?,
                opt_else.map(|e| this(*e)).transpose()?,
            ))
        }
        _ => todo!(),
    };
    hir.map(|h| Box::new(WithMeta::<_>(h, span)))
}

pub fn generate_hir(
    expr: WithMeta<ast::Expr>,
) -> Result<WithMeta<Hir>, Vec<Box<dyn ReportableError>>> {
    let expr_without_self = selfconvert::convert_self_top(expr);
    let mut infer_ctx = typing::InferContext::new();
    let _toptype = typing::infer_type(expr_without_self.clone().0, &mut infer_ctx);
    let mut evalenv = Evalenv::new();
    let mut errs = Vec::<Box<dyn ReportableError>>::new();
    let res = gen_hir(expr_without_self, &infer_ctx.env, &mut evalenv)
        .map_err(|e: Error| errs.push(Box::new(e.clone())));
    res.map(|h| *h).map_err(|_e| errs)
}
