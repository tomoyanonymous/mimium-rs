pub mod builder;

pub use crate::types::TypedId;
use crate::types::*;
use crate::utils::metadata::WithMeta;
use crate::utils::miniprint::MiniPrint;
use std::fmt;
pub type Time = i64;

#[derive(Clone, Debug, PartialEq, Hash)]
pub enum Literal {
    String(String),
    Int(i64),
    Float(String),
    SelfLit,
    Now,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Var(Id, Option<Time>),
    Block(Option<Box<WithMeta<Self>>>),
    Tuple(Vec<WithMeta<Self>>),
    Proj(Box<WithMeta<Self>>, i64),
    Apply(Box<WithMeta<Self>>, Vec<WithMeta<Self>>),
    Lambda(Vec<WithMeta<TypedId>>, Box<WithMeta<Self>>), //lambda, maybe information for internal state is needed
    Feed(Id, Box<WithMeta<Self>>), //feedback connection primitive operation. This will be shown only after self-removal stage
    Let(TypedId, Box<WithMeta<Self>>, Option<Box<WithMeta<Self>>>),
    LetRec(TypedId, Box<WithMeta<Self>>, Option<Box<WithMeta<Self>>>),
    LetTuple(
        Vec<TypedId>,
        Box<WithMeta<Self>>,
        Option<Box<WithMeta<Self>>>,
    ),
    If(
        Box<WithMeta<Self>>,
        Box<WithMeta<Self>>,
        Option<Box<WithMeta<Self>>>,
    ),
    //exprimental macro system using multi-stage computation
    Bracket(Box<WithMeta<Self>>),
    Escape(Box<WithMeta<Self>>),

    Error,
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Literal::Float(n) => write!(f, "(float {})", n),
            Literal::Int(n) => write!(f, "(int {})", n),
            Literal::String(s) => write!(f, "\"{}\"", s),
            Literal::Now => write!(f, "now"),
            Literal::SelfLit => write!(f, "self"),
        }
    }
}

impl MiniPrint for Literal {
    fn simple_print(&self) -> String {
        self.to_string()
    }
}

fn concat_vec<T: MiniPrint>(vec: &Vec<T>) -> String {
    let callee_str = vec
        .iter()
        .fold("".to_string(), |a, b| format!("{a} {}", b.simple_print()));
    if vec.len() > 1 {
        callee_str.split_at(1).1.to_string()
    } else {
        callee_str
    }
}

impl MiniPrint for Expr {
    fn simple_print(&self) -> String {
        match self {
            Expr::Literal(l) => l.simple_print(),
            Expr::Var(v, t) => match t {
                Some(t) => format!("{}@{}", v, t),
                None => v.to_string(),
            },
            Expr::Block(e) => e.as_ref().map_or("".to_string(), |box WithMeta(v, _s)| {
                format!("(block {})", v.simple_print())
            }),
            Expr::Tuple(e) => {
                let e1 = e.iter().map(|e| e.0.clone()).collect::<Vec<Expr>>();
                format!("(tuple ({}))", concat_vec(&e1))
            }
            Expr::Proj(e, idx) => format!("(proj {} {})", e.0.simple_print(), idx),
            Expr::Apply(e1, e2) => {
                let es = e2.iter().map(|e| e.0.clone()).collect::<Vec<Expr>>();

                format!("(app {} ({}))", e1.0.simple_print(), concat_vec(&es))
            }
            Expr::Lambda(params, body) => {
                let paramstr = params.iter().map(|e| e.0.clone()).collect::<Vec<_>>();
                format!(
                    "(lambda ({}) {})",
                    concat_vec(&paramstr),
                    body.0.simple_print()
                )
            }
            Expr::Feed(id, body) => format!("(feed {} {})", id, body.0.simple_print()),
            Expr::Let(id, body, then) => format!(
                "(let {} {} {})",
                &id.id,
                body.0.simple_print(),
                then.as_ref().map_or("".into(), |t| t.0.simple_print())
            ),
            Expr::LetRec(id, body, then) => format!(
                "(letrec {} {} {})",
                &id.id,
                body.0.simple_print(),
                then.as_ref().map_or("".into(), |t| t.0.simple_print())
            ),
            Expr::LetTuple(_, _, _) => todo!(),
            Expr::If(cond, then, optelse) => format!(
                "(if {} {} {})",
                cond.0.simple_print(),
                then.0.simple_print(),
                optelse.as_ref().map_or("".into(), |e| e.0.simple_print())
            ),
            Expr::Bracket(_) => todo!(),
            Expr::Escape(_) => todo!(),
            Expr::Error => todo!(),
        }
    }
}
