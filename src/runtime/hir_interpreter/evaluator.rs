use crate::runtime::builtin_fn;
use crate::hir::expr::*;
use crate::utils::{
    error::ReportableError,
    metadata::{Span, WithMeta},
};

use std::fmt;

#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),
    Numeric(f64),
    String(String),
    Function, //label,
    Code(Box<Value>),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> std::fmt::Result {
        match self {
            Value::Int(n) => write!(f, "{}", n.to_string()),
            Value::Numeric(n) => write!(f, "{}", n.to_string()),
            Value::String(s) => write!(f, "{}", s),
            Value::Function => write!(f, "function"),
            Value::Code(v) => write!(f, "<{}>", *v),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ErrorKind {
    InvalidStage,
    InvalidType,
    Unbounded(String),
    FloatParse,
    Multiple(Vec<Error>),
    NotAValue,
    Unknown,
}
#[derive(Debug, Clone, PartialEq)]
pub struct Error(ErrorKind, Span);

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.0 {
            ErrorKind::InvalidStage => write!(f, "Stage will be removed before evaluation"),
            ErrorKind::InvalidType => write!(f, "Internal Type Mismatch"),
            ErrorKind::Unbounded(v) => write!(f, "Value {} not found", v),
            ErrorKind::FloatParse => write!(f, "Invalid floating point number format"),
            ErrorKind::Multiple(v) => write!(f, "{:?}", v),
            ErrorKind::NotAValue => write!(f, "Evaluated result was not a value"),
            ErrorKind::Unknown => write!(f, "Unknown Error"),
        }
    }
}

impl std::error::Error for Error {}
impl ReportableError for Error {
    fn get_span(&self) -> std::ops::Range<usize> {
        self.1.clone()
    }
}

fn eval_v(e: Literal) -> Value {
    match e {
        Literal::Int(i) => Value::Int(i),
        Literal::Float(s) => Value::Numeric(s.parse::<f64>().unwrap()),
        Literal::String(s) => Value::String(s),
    }
}

fn eval_i(expr: WithMeta<Expr>, ctx: &builtin_fn::Context) -> Result<WithMeta<Expr>, Error> {
    let WithMeta(e, span) = expr.clone();

    match e {
        Expr::Literal(_x) => Ok(expr),
        Expr::Lambda(_params, _body) => Ok(expr),
        Expr::Var(var, _time) => {
            let v = var.0.v.borrow().clone();
            match v {
                Some(e) => eval_i(WithMeta(e, span.clone()), ctx),
                None => Err(Error(ErrorKind::Unbounded(var.0.id.clone()), span)),
            }
        }
        Expr::Apply(box WithMeta(Expr::Lambda(params, body), _fspan), callee) => {
            params.iter().zip(callee.iter()).for_each(|(p, e)| {
                let mut myp = p.0.v.borrow_mut();
                *myp = Some(e.0.clone());
            });
            eval_i(*body, ctx)
        }
        Expr::Apply(fun, callee) if fun.0.is_value() => {
            let newcallee = callee
                .iter()
                .map(|c| eval_i(c.clone(), ctx))
                .collect::<Result<Vec<_>, _>>()?;
            let res = WithMeta(Expr::Apply(fun, newcallee), span.clone());
            eval_i(res, ctx)
        }
        Expr::Apply(box WithMeta(Expr::Var(v, _time), _fspan), callee)
            if v.0.v.borrow().is_none() =>
        {
            //eval builtin function
            let newcallee = callee
                .iter()
                .map(|c| eval_i(c.clone(), ctx))
                .collect::<Result<Vec<_>, _>>()?;

            match &newcallee[..] {
                [WithMeta(Expr::Literal(Literal::Float(x)), _)] => {
                    let res = ctx.eval_float1(&v.0.id, x.parse::<f64>().unwrap());
                    match res {
                        Some(x) => Ok(WithMeta(
                            Expr::Literal(Literal::Float(x.to_string())),
                            span.clone(),
                        )),
                        None => Err(Error(ErrorKind::FloatParse, span)),
                    }
                }
                [WithMeta(Expr::Literal(Literal::Float(x)), _), WithMeta(Expr::Literal(Literal::Float(y)), _)] =>
                {
                    let res = ctx.eval_float2(
                        &v.0.id,
                        x.parse::<f64>().unwrap(),
                        y.parse::<f64>().unwrap(),
                    );
                    match res {
                        Some(x) => Ok(WithMeta(
                            Expr::Literal(Literal::Float(x.to_string())),
                            span.clone(),
                        )),
                        None => Err(Error(ErrorKind::FloatParse, span)),
                    }
                }
                _ => Err(Error(ErrorKind::InvalidType, span)),
            }
        }
        Expr::Apply(box fun, callee) => {
            //expand fun until it becomes value
            let res = WithMeta(
                Expr::Apply(Box::new(eval_i(fun, ctx)?), callee.clone()),
                span.clone(),
            );
            eval_i(res, ctx)
        }
        Expr::Bracket(_b) | Expr::Escape(_b) => todo!(),

        _ => {
            let mut evec = Vec::<Error>::new();
            let res = expr.walk(|x| {
                eval_i(x, ctx).unwrap_or_else(|e| {
                    evec.push(e);
                    WithMeta(Expr::Error, 0..0)
                })
            });
            if evec.len() > 0 {
                Err(Error(ErrorKind::Multiple(evec), span))
            } else {
                Ok(res)
            }
        }
    }
}
pub fn eval(expr: WithMeta<Expr>) -> Result<Value, Box<dyn ReportableError>> {
    let ctx = builtin_fn::Context::new();
    let res = eval_i(expr, &ctx);
    match res {
        Ok(WithMeta(e, s)) if e.is_value() => match e {
            Expr::Literal(x) => Ok(eval_v(x)),
            Expr::Lambda(_p, _r) => Ok(Value::Function),
            _ => Err(Box::new(Error(ErrorKind::NotAValue, s.clone()))),
        },
        Ok(WithMeta(_e, s)) => Err(Box::new(Error(ErrorKind::Unknown, s.clone()))),
        Err(e) => Err(Box::new(e)),
    }
}
