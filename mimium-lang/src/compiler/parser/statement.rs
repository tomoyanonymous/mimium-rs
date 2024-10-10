use crate::{
    ast::Expr,
    interner::{ExprNodeId, Symbol},
    pattern::{TypedId, TypedPattern},
};

use super::Span;
// an intermediate representation used in parser.
// Note that this struct do not distinct between a global statement(allows `fn(){}`) and a local statement.
// The distinction is done in the actual parser logic.
#[derive(Clone, Debug, PartialEq)]
pub(super) enum Statement {
    Let(TypedPattern, ExprNodeId),
    MacroExpand(TypedId, ExprNodeId),
    LetRec(TypedId, ExprNodeId),
    Assign(ExprNodeId, ExprNodeId),
    Single(ExprNodeId),
}

// A helper function to convert vector of statements to nested expression

pub(super) fn into_then_expr(stmts: &[(Statement, Span)]) -> Option<ExprNodeId> {
    let get_span = |spana: Span, spanb: Option<ExprNodeId>| match spanb {
        Some(b) => {
            let start = spana.start;
            start..b.to_span().end
        }
        None => spana,
    };
    let e_pre = stmts.iter().rev().fold(None, |then, (stmt, span)| {
        let s = get_span(span.clone(), then);
        match (then, stmt) {
            (_, Statement::Let(pat, body)) => {
                Some(Expr::Let(pat.clone(), *body, then).into_id(s))
            }

            (_, Statement::LetRec(id, body)) => {
                Some(Expr::LetRec(id.clone(), *body, then).into_id(s))
            }
            (_, Statement::Assign(name, body)) => Some(
                Expr::Then(Expr::Assign(*name, *body).into_id(span.clone()), then).into_id(s),
            ),
            (_, Statement::MacroExpand(fname, body)) => {
                //todo!
                Some(Expr::LetRec(fname.clone(), *body, then).into_id(s))
            }
            (None, Statement::Single(e)) => Some(*e),
            (t, Statement::Single(e)) => Some(Expr::Then(*e, t).into_id(s)),
        }
    });
    // log::debug!("stmts {:?}, e_pre: {:?}", stmts, e_pre);
    e_pre
}
