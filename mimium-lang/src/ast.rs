pub mod builder;

use crate::interner::{with_session_globals, ExprNodeId, Symbol, TypeNodeId};
use crate::pattern::{TypedId, TypedPattern};
use crate::utils::metadata::Span;
use crate::utils::miniprint::MiniPrint;
use std::fmt::{self};
pub type Time = i64;

#[derive(Clone, Debug, PartialEq, Hash)]
pub enum Literal {
    String(String),
    Int(i64),
    Float(String),
    SelfLit,
    Now,
}

impl Expr {
    fn into_id_inner(self, span: Option<Span>) -> ExprNodeId {
        let span = span.unwrap_or(0..0);
        with_session_globals(|session_globals| session_globals.store_expr_with_span(self, span))
    }

    pub fn into_id(self, span: Span) -> ExprNodeId {
        self.into_id_inner(Some(span))
    }

    // For testing purposes
    pub fn into_id_without_span(self) -> ExprNodeId {
        self.into_id_inner(None)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Var(Symbol),
    Block(Option<ExprNodeId>),
    Tuple(Vec<ExprNodeId>),
    Proj(ExprNodeId, i64),
    Apply(ExprNodeId, Vec<ExprNodeId>),
    Lambda(Vec<TypedId>, Option<TypeNodeId>, ExprNodeId), //lambda, maybe information for internal state is needed
    Assign(Symbol, ExprNodeId),
    Then(ExprNodeId, ExprNodeId),
    Feed(Symbol, ExprNodeId), //feedback connection primitive operation. This will be shown only after self-removal stage
    Let(TypedPattern, ExprNodeId, Option<ExprNodeId>),
    LetRec(TypedId, ExprNodeId, Option<ExprNodeId>),
    If(ExprNodeId, ExprNodeId, Option<ExprNodeId>),
    //exprimental macro system using multi-stage computation
    Bracket(ExprNodeId),
    Escape(ExprNodeId),

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

impl MiniPrint for ExprNodeId {
    fn simple_print(&self) -> String {
        self.to_expr().simple_print()
    }
}

impl MiniPrint for Option<ExprNodeId> {
    fn simple_print(&self) -> String {
        match self {
            Some(e) => e.to_expr().simple_print(),
            None => "".to_string(),
        }
    }
}

impl MiniPrint for Expr {
    fn simple_print(&self) -> String {
        match self {
            Expr::Literal(l) => l.simple_print(),
            Expr::Var(v) => format!("{v}"),
            Expr::Block(e) => e.map_or("".to_string(), |eid| {
                format!("(block {})", eid.to_expr().simple_print())
            }),
            Expr::Tuple(e) => {
                let e1 = e.iter().map(|e| e.to_expr().clone()).collect::<Vec<Expr>>();
                format!("(tuple ({}))", concat_vec(&e1))
            }
            Expr::Proj(e, idx) => format!("(proj {} {})", e.simple_print(), idx),
            Expr::Apply(e1, e2) => {
                let es = e2
                    .iter()
                    .map(|e| e.to_expr().clone())
                    .collect::<Vec<Expr>>();

                format!("(app {} ({}))", e1.simple_print(), concat_vec(&es))
            }
            Expr::Lambda(params, _, body) => {
                let paramstr = params.iter().map(|e| e.clone()).collect::<Vec<_>>();
                format!(
                    "(lambda ({}) {})",
                    concat_vec(&paramstr),
                    body.simple_print()
                )
            }
            Expr::Feed(id, body) => format!("(feed {} {})", id, body.simple_print()),
            Expr::Let(id, body, then) => format!(
                "(let {} {} {})",
                id,
                body.simple_print(),
                then.simple_print()
            ),
            Expr::LetRec(id, body, then) => format!(
                "(letrec {} {} {})",
                &id.id,
                body.simple_print(),
                then.simple_print()
            ),
            Expr::Assign(lid, rhs) => format!("(assign {lid} {})", rhs.simple_print()),
            Expr::Then(first, second) => {
                format!("(then {} {})", first.simple_print(), second.simple_print())
            }
            Expr::If(cond, then, optelse) => format!(
                "(if {} {} {})",
                cond.simple_print(),
                then.simple_print(),
                optelse.simple_print()
            ),
            Expr::Bracket(_) => todo!(),
            Expr::Escape(_) => todo!(),
            Expr::Error => todo!(),
        }
    }
}
