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
    Then(ExprNodeId, Option<ExprNodeId>),
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

fn concat_vec<T: MiniPrint>(vec: &[T]) -> String {
    vec.iter()
        .map(|t| t.simple_print())
        .collect::<Vec<_>>()
        .join(" ")
}

impl MiniPrint for ExprNodeId {
    fn simple_print(&self) -> String {
        let span = self.to_span();
        format!(
            "{}:{}..{}",
            self.to_expr().simple_print(),
            span.start,
            span.end
        )
    }
}

impl MiniPrint for Option<ExprNodeId> {
    fn simple_print(&self) -> String {
        match self {
            Some(e) => e.simple_print(),
            None => "()".to_string(),
        }
    }
}

impl MiniPrint for Expr {
    fn simple_print(&self) -> String {
        match self {
            Expr::Literal(l) => l.simple_print(),
            Expr::Var(v) => format!("{v}"),
            Expr::Block(e) => e.map_or("".to_string(), |eid| {
                format!("(block {})", eid.simple_print())
            }),
            Expr::Tuple(e) => {
                let e1 = e.iter().map(|e| e.to_expr().clone()).collect::<Vec<Expr>>();
                format!("(tuple ({}))", concat_vec(&e1))
            }
            Expr::Proj(e, idx) => format!("(proj {} {})", e.simple_print(), idx),
            Expr::Apply(e1, e2) => {
                format!("(app {} ({}))", e1.simple_print(), concat_vec(e2))
            }
            Expr::Lambda(params, _, body) => {
                format!("(lambda ({}) {})", concat_vec(params), body.simple_print())
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
            Expr::Error => "(error)".to_string(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Statement {
    Let(TypedPattern, ExprNodeId),
    MacroExpand(TypedId, ExprNodeId),
    LetRec(TypedId, ExprNodeId),
    Assign(Symbol, ExprNodeId),
    Single(ExprNodeId),
}

pub(crate) fn into_then_expr(stmts: &[(Statement, Span)]) -> Option<ExprNodeId> {
    let get_span = |spana: Span, spanb: Option<ExprNodeId>| match spanb {
        Some(b) => {
            let start = spana.start;
            start..b.to_span().end
        }
        None => spana,
    };
    let e_pre = stmts
        .iter()
        .rev()
        .fold(None, |then, (stmt, span)| match (then, stmt) {
            (_, Statement::Let(pat, body)) => {
                Some(Expr::Let(pat.clone(), *body, then).into_id(get_span(span.clone(), then)))
            }

            (_, Statement::LetRec(id, body)) => {
                Some(Expr::LetRec(id.clone(), *body, then).into_id(get_span(span.clone(), then)))
            }
            (_, Statement::Assign(name, body)) => Some(
                Expr::Then(Expr::Assign(*name, *body).into_id(span.clone()), then)
                    .into_id(get_span(span.clone(), then)),
            ),
            (_, Statement::MacroExpand(fname, body)) => {
                //todo!
                Some(Expr::LetRec(fname.clone(), *body, then).into_id(get_span(span.clone(), then)))
            }
            (None, Statement::Single(e)) => Some(*e),
            (t, Statement::Single(e)) => {
                Some(Expr::Then(*e, t).into_id(get_span(span.clone(), then)))
            }
        });
    // log::debug!("stmts {:?}, e_pre: {:?}", stmts, e_pre);
    e_pre
}
