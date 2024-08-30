use crate::ast::{Expr, Literal};
use crate::interner::{ExprNodeId, Symbol, ToSymbol};
use crate::utils::{error::ReportableError, metadata::Span};
use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub enum Error {
    NoParentSelf(Span),
}

type ConvertResult = Result<ExprNodeId, ExprNodeId>;
fn get_content(e: ConvertResult) -> ExprNodeId {
    match e {
        Ok(e) | Err(e) => e,
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::NoParentSelf(_s) => write!(f, "self cannot be used in global context."),
        }
    }
}
impl std::error::Error for Error {}
impl ReportableError for Error {
    fn get_span(&self) -> std::ops::Range<usize> {
        match self {
            Self::NoParentSelf(s) => s.clone(),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum FeedId {
    Global,
    Local(i64),
}

fn get_new_feedid(fid: FeedId) -> i64 {
    match fid {
        FeedId::Global => 0,
        FeedId::Local(i) => i + 1,
    }
}

fn get_feedvar_name(fid: i64) -> Symbol {
    //todo:need to assign true unique name
    format!("feed_id{}", fid).to_symbol()
}

fn convert_self(e_id: ExprNodeId, feedctx: FeedId) -> Result<ConvertResult, Error> {
    let cls = |e: ExprNodeId| -> Result<ConvertResult, Error> { convert_self(e, feedctx) };
    let opt_cls = |opt_e: Option<ExprNodeId>| -> Result<Option<ConvertResult>, Error> {
        opt_e.map(cls).transpose()
    };
    let span = e_id.to_span().clone();
    match e_id.to_expr().clone() {
        Expr::Literal(Literal::SelfLit) => match feedctx {
            FeedId::Global => Err(Error::NoParentSelf(span.clone())),
            FeedId::Local(i) => Ok(ConvertResult::Err(
                Expr::Var(get_feedvar_name(i)).into_id(span.clone()),
            )),
        },
        Expr::Tuple(v) => {
            let elems: Vec<ConvertResult> = v.into_iter().map(&cls).try_collect()?;
            let elems_mapped: Vec<ExprNodeId> = elems.iter().map(|e| get_content(*e)).collect();
            if elems.iter().any(|e| e.is_err()) {
                Ok(ConvertResult::Err(Expr::Tuple(elems_mapped).into_id(span)))
            } else {
                Ok(ConvertResult::Ok(Expr::Tuple(elems_mapped).into_id(span)))
            }
        }

        Expr::Proj(e, idx) => {
            let elem = cls(e)?;
            Ok(elem.map(|e| Expr::Proj(e, idx).into_id(span)))
        }
        Expr::Let(id, body, then) => {
            let body = cls(body)?;
            let then = opt_cls(then)?;
            if let (Ok(b), Ok(t)) = (body, then.transpose()) {
                Ok(ConvertResult::Ok(Expr::Let(id, b, t).into_id(span)))
            } else {
                Ok(ConvertResult::Err(
                    Expr::Let(id, get_content(body), then.map(get_content)).into_id(span),
                ))
            }
        }
        Expr::LetRec(id, body, then) => {
            let body = cls(body)?;
            let then = opt_cls(then)?;
            if let (Ok(b), Ok(t)) = (body, then.transpose()) {
                Ok(ConvertResult::Ok(Expr::LetRec(id, b, t).into_id(span)))
            } else {
                Ok(ConvertResult::Err(e_id))
            }
        }

        Expr::Lambda(params, r_type, body) => {
            let nfctx = get_new_feedid(feedctx);
            let nbody = match convert_self(body, FeedId::Local(nfctx))? {
                ConvertResult::Err(nbody) => {
                    let feedid = get_feedvar_name(nfctx);
                    Expr::Feed(feedid, nbody).into_id(span.clone())
                }
                ConvertResult::Ok(nbody) => nbody,
            };
            Ok(ConvertResult::Ok(
                Expr::Lambda(params, r_type, nbody).into_id(span.clone()),
            ))
        }
        Expr::Apply(fun, callee) => {
            let fun = cls(fun)?;
            let elems: Vec<ConvertResult> = callee.into_iter().map(cls).try_collect()?;
            let elems_mapped: Vec<ExprNodeId> = elems.iter().map(|e| get_content(*e)).collect();
            let content = Expr::Apply(fun.unwrap(), elems_mapped).into_id(span);
            if fun.is_ok() && !elems.iter().any(|e| e.is_err()) {
                Ok(ConvertResult::Ok(content))
            } else {
                Ok(ConvertResult::Err(content))
            }
        }
        Expr::If(cond, then, opt_else) => {
            let cond = cls(cond)?;
            let then = cls(then)?;
            let opt_else = opt_cls(opt_else)?;
            match (cond, then, opt_else.transpose()) {
                (Ok(c), Ok(t), Ok(e)) => Ok(ConvertResult::Ok(Expr::If(c, t, e).into_id(span))),
                (c, t, e) => {
                    let e = match e {
                        Ok(e) => e,
                        Err(e) => Some(e),
                    };
                    Ok(ConvertResult::Err(
                        Expr::If(get_content(c), get_content(t), e).into_id(span),
                    ))
                }
            }
        }
        Expr::Block(body) => {
            if let Some(body) = body {
                Ok(cls(body)?.map(|e| Expr::Block(Some(e)).into_id(span)))
            } else {
                Ok(ConvertResult::Ok(Expr::Block(None).into_id(span)))
            }
        }
        Expr::Feed(_, _) => panic!(
            "Feed should not be shown before conversion at {}..{}",
            span.start, span.end
        ),
        _ => Ok(ConvertResult::Ok(e_id)),
    }
}

pub fn convert_self_top(expr: ExprNodeId) -> Result<ExprNodeId, Error> {
    let res = convert_self(expr, FeedId::Global)?;
    Ok(get_content(res))
}

#[cfg(test)]
mod test {
    use crate::{
        pattern::{Pattern, TypedId, TypedPattern},
        types::Type,
    };

    use super::*;

    #[test]
    pub fn test_selfconvert() {
        let src = Expr::Let(
            TypedPattern {
                pat: Pattern::Single("lowpass".to_symbol()),
                ty: Type::Unknown.into_id_with_span(0..1),
            },
            Expr::Lambda(
                vec![TypedId {
                    id: "input".to_symbol(),
                    ty: Type::Unknown.into_id_with_span(0..1),
                }],
                None,
                Expr::Literal(Literal::SelfLit).into_id(0..1),
            )
            .into_id(0..1),
            None,
        )
        .into_id(0..1);
        let res = convert_self_top(src).unwrap();

        let ans = Expr::Let(
            TypedPattern {
                pat: Pattern::Single("lowpass".to_symbol()),
                ty: Type::Unknown.into_id_with_span(0..1),
            },
            Expr::Lambda(
                vec![TypedId {
                    id: "input".to_symbol(),
                    ty: Type::Unknown.into_id_with_span(0..1),
                }],
                None,
                Expr::Feed(
                    "feed_id0".to_symbol(),
                    Expr::Var("feed_id0".to_symbol()).into_id(0..1),
                )
                .into_id(0..1),
            )
            .into_id(0..1),
            None,
        )
        .into_id(0..1);
        assert_eq!(res, ans);
    }
}
