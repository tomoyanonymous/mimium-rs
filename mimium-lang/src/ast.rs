pub mod builder;

use id_arena::Id;

use crate::ast_interpreter::with_session_globals;
use crate::pattern::{TypedId, TypedPattern};
use crate::types::*;
use crate::utils::metadata::WithMeta;
use crate::utils::miniprint::MiniPrint;
use std::fmt::{self, Display};
pub type Time = i64;

#[derive(Default, Copy, Clone, PartialEq, Debug, Hash, Eq)]
pub struct Symbol(pub usize); //Symbol Trait is implemented on usize

pub trait ToSymbol {
    fn to_symbol(&self) -> Symbol;
}

impl<T: AsRef<str>> ToSymbol for T {
    fn to_symbol(&self) -> Symbol {
        Symbol(with_session_globals(|session_globals| {
            session_globals.symbol_interner.get_or_intern(self.as_ref())
        }))
    }
}

impl Symbol {
    pub fn as_str(&self) -> &str {
        with_session_globals(|session_globals| unsafe {
            // This transmute is needed to convince the borrow checker. Since
            // the session_global should exist until the end of the session,
            // this &str should live sufficiently long.
            std::mem::transmute::<&str, &str>(
                session_globals
                    .symbol_interner
                    .resolve(self.0)
                    .expect("invalid symbol"),
            )
        })
    }
}

// Note: to_string() is auto-implemented by this
impl Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

#[derive(Clone, Debug, PartialEq, Hash)]
pub enum Literal {
    String(String),
    Int(i64),
    Float(String),
    SelfLit,
    Now,
}

pub type ExprId = Id<Expr>;

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Var(Symbol, Option<Time>),
    Block(Option<Box<WithMeta<Self>>>),
    Tuple(Vec<WithMeta<Self>>),
    Proj(Box<WithMeta<Self>>, i64),
    Apply(Box<WithMeta<Self>>, Vec<WithMeta<Self>>),
    Lambda(Vec<WithMeta<TypedId>>, Option<Type>, Box<WithMeta<Self>>), //lambda, maybe information for internal state is needed
    Assign(Symbol, Box<WithMeta<Self>>),
    Then(Box<WithMeta<Self>>, Box<WithMeta<Self>>),
    Feed(Symbol, Box<WithMeta<Self>>), //feedback connection primitive operation. This will be shown only after self-removal stage
    Let(
        WithMeta<TypedPattern>,
        Box<WithMeta<Self>>,
        Option<Box<WithMeta<Self>>>,
    ),
    LetRec(TypedId, Box<WithMeta<Self>>, Option<Box<WithMeta<Self>>>),
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
            Expr::Lambda(params, _, body) => {
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
                id.0,
                body.0.simple_print(),
                then.as_ref().map_or("".into(), |t| t.0.simple_print())
            ),
            Expr::LetRec(id, body, then) => format!(
                "(letrec {} {} {})",
                &id.id,
                body.0.simple_print(),
                then.as_ref().map_or("".into(), |t| t.0.simple_print())
            ),
            Expr::Assign(lid, rhs) => format!("(assign {lid} {})", rhs.0.simple_print()),
            Expr::Then(first, second) => format!(
                "(then {} {})",
                first.0.simple_print(),
                second.0.simple_print()
            ),
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
