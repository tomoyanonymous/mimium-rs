use crate::ast::{Expr, Literal};
use crate::interner::{ExprNodeId, Symbol, ToSymbol};
use crate::pattern::TypedId;
use crate::types::Type;
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

// This applies conversion() recursively. This is intended to be used in the `_`
// branch of pattern matching so that particular types of epressions can be
// caught and treated differently.
fn convert_recursively<T>(e_id: ExprNodeId, conversion: T) -> Result<ConvertResult, Error>
where
    T: Fn(ExprNodeId) -> Result<ConvertResult, Error>,
{
    let opt_conversion = |opt_e: Option<ExprNodeId>| -> Result<Option<ConvertResult>, Error> {
        opt_e.map(&conversion).transpose()
    };
    let span = e_id.to_span().clone();
    match e_id.to_expr().clone() {
        Expr::Tuple(v) => {
            let elems: Vec<ConvertResult> = v.into_iter().map(&conversion).try_collect()?;
            let elems_mapped: Vec<ExprNodeId> = elems.iter().map(|e| get_content(*e)).collect();
            if elems.iter().any(|e| e.is_err()) {
                Ok(ConvertResult::Err(Expr::Tuple(elems_mapped).into_id(span)))
            } else {
                Ok(ConvertResult::Ok(Expr::Tuple(elems_mapped).into_id(span)))
            }
        }
        Expr::Proj(e, idx) => {
            let elem = conversion(e)?;
            Ok(elem.map(|e| Expr::Proj(e, idx).into_id(span)))
        }
        Expr::Let(id, body, then) => {
            let body = conversion(body)?;
            let then = opt_conversion(then)?;
            if let (Ok(b), Ok(t)) = (body, then.transpose()) {
                Ok(ConvertResult::Ok(Expr::Let(id, b, t).into_id(span)))
            } else {
                Ok(ConvertResult::Err(
                    Expr::Let(id, get_content(body), then.map(get_content)).into_id(span),
                ))
            }
        }
        Expr::LetRec(id, body, then) => {
            let body = conversion(body)?;
            let then = opt_conversion(then)?;
            if let (Ok(b), Ok(t)) = (body, then.transpose()) {
                Ok(ConvertResult::Ok(Expr::LetRec(id, b, t).into_id(span)))
            } else {
                Ok(ConvertResult::Err(e_id))
            }
        }
        Expr::Lambda(params, r_type, body) => {
            // Note: params and r_type cannot be handled by conversion() because
            //       these are Type, not Expr.
            let body = conversion(body)?;
            if let Ok(b) = body {
                let content = Expr::Lambda(params, r_type, b).into_id(span);
                Ok(ConvertResult::Ok(content))
            } else {
                Ok(ConvertResult::Err(e_id))
            }
        }
        Expr::Apply(fun, callee) => {
            let fun = conversion(fun)?;
            let elems: Vec<ConvertResult> = callee.into_iter().map(conversion).try_collect()?;
            let elems_mapped: Vec<ExprNodeId> = elems.iter().map(|e| get_content(*e)).collect();
            let content = Expr::Apply(fun.unwrap(), elems_mapped).into_id(span);
            if fun.is_ok() && !elems.iter().any(|e| e.is_err()) {
                Ok(ConvertResult::Ok(content))
            } else {
                Ok(ConvertResult::Err(content))
            }
        }
        Expr::PipeApply(callee, fun) => {
            let callee = conversion(callee)?;
            let fun = conversion(fun)?;
            let content = Expr::PipeApply(callee.unwrap(), fun.unwrap()).into_id(span);
            if callee.is_ok() && fun.is_ok() {
                Ok(ConvertResult::Ok(content))
            } else {
                Ok(ConvertResult::Err(content))
            }
        }
        Expr::If(cond, then, opt_else) => {
            let cond = conversion(cond)?;
            let then = conversion(then)?;
            let opt_else = opt_conversion(opt_else)?;
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
                Ok(conversion(body)?.map(|e| Expr::Block(Some(e)).into_id(span)))
            } else {
                Ok(ConvertResult::Ok(Expr::Block(None).into_id(span)))
            }
        }

        _ => Ok(ConvertResult::Ok(e_id)),
    }
}

fn convert_self(e_id: ExprNodeId, feedctx: FeedId) -> Result<ConvertResult, Error> {
    let conversion = |e: ExprNodeId| -> Result<ConvertResult, Error> { convert_self(e, feedctx) };
    let span = e_id.to_span().clone();
    match e_id.to_expr().clone() {
        Expr::Literal(Literal::SelfLit) => match feedctx {
            FeedId::Global => Err(Error::NoParentSelf(span.clone())),
            FeedId::Local(i) => Ok(ConvertResult::Err(
                Expr::Var(get_feedvar_name(i)).into_id(span.clone()),
            )),
        },
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
        Expr::Feed(_, _) => panic!(
            "Feed should not be shown before conversion at {}..{}",
            span.start, span.end
        ),
        _ => convert_recursively(e_id, conversion),
    }
}

fn convert_placeholder(e_id: ExprNodeId) -> Result<ConvertResult, Error> {
    let span = e_id.to_span().clone();
    match e_id.to_expr() {
        // if _ is used outside of pipe, treat it as a usual variable.
        Expr::Literal(Literal::PlaceHolder) => Ok(ConvertResult::Ok(
            Expr::Var("_".to_symbol()).into_id(e_id.to_span()),
        )),
        Expr::Apply(fun, args)
            if args
                .iter()
                .any(|e| matches!(e.to_expr(), Expr::Literal(Literal::PlaceHolder))) =>
        {
            let fun = convert_placeholder(fun)?;
            let mut lambda_args: Vec<TypedId> = Vec::with_capacity(args.len());
            let new_args = args
                .iter()
                .enumerate()
                .map(|(i, e)| {
                    if matches!(e.to_expr(), Expr::Literal(Literal::PlaceHolder)) {
                        let id = format!("__lambda_arg_{i}").to_symbol();
                        lambda_args.push(TypedId {
                            id,
                            ty: Type::Unknown.into_id_with_span(span.clone()),
                        });
                        Expr::Var(id).into_id(e.to_span())
                    } else {
                        *e
                    }
                })
                .collect::<Vec<_>>();
            let body = Expr::Apply(fun.unwrap(), new_args).into_id(span.clone());
            let content = Expr::Lambda(lambda_args, None, body).into_id(span.clone());
            if fun.is_ok() {
                Ok(ConvertResult::Ok(content))
            } else {
                Ok(ConvertResult::Err(content))
            }
        }
        _ => convert_recursively(e_id, convert_placeholder),
    }
}

fn convert_pipe(e_id: ExprNodeId) -> Result<ConvertResult, Error> {
    let span = e_id.to_span().clone();
    match e_id.to_expr() {
        Expr::PipeApply(callee, fun) => {
            let callee = convert_pipe(callee)?;
            let fun = convert_pipe(fun)?;
            let content = Expr::Apply(fun.unwrap(), vec![callee.unwrap()]).into_id(span.clone());
            if callee.is_ok() && fun.is_ok() {
                Ok(ConvertResult::Ok(content))
            } else {
                Ok(ConvertResult::Err(content))
            }
        }
        _ => convert_recursively(e_id, convert_pipe),
    }
}

pub fn convert_pronoun(expr: ExprNodeId) -> Result<ExprNodeId, Error> {
    let expr = convert_placeholder(expr)?;
    let expr = convert_pipe(get_content(expr))?;
    let res = convert_self(get_content(expr), FeedId::Global)?;
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
        let res = convert_pronoun(src).unwrap();

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
