use crate::ast::{Expr, Literal};
use crate::interner::{ExprNodeId, Symbol, ToSymbol};
use crate::pattern::TypedId;
use crate::types::Type;
use crate::utils::error::SimpleError;
use crate::utils::metadata::Location;
use crate::utils::{error::ReportableError, metadata::Span};
use itertools::Itertools;
use std::fmt;
use std::path::PathBuf;
pub type Error = SimpleError;
struct ConvertResult {
    expr: ExprNodeId,
    found_any: bool,
}

// fn collect_result<T,F>(iter:T,f:F)->(ConvertResult,Vec<Error>)
// where T:Iterator<Item=Result<ConvertResult,Error>>,F:Fn(T)->ConvertResult {
//     let mut errs =vec![];
//     iter.map(|res|{
//         match res {
//             Ok(r) => r,
//             Err(_) => todo!(),
//         }
//     })
// }

// This applies conversion() recursively. This is intended to be used in the `_`
// branch of pattern matching so that particular types of epressions can be
// caught and treated differently.
fn convert_recursively<T>(
    e_id: ExprNodeId,
    conversion: T,
    file_path: PathBuf,
) -> (ConvertResult, Vec<Error>)
where
    T: Fn(ExprNodeId) -> (ConvertResult, Vec<Error>),
{
    let loc = Location {
        span: e_id.to_span().clone(),
        path: file_path.to_string_lossy().to_symbol(),
    };

    let opt_conversion = |opt_e: Option<ExprNodeId>| -> (Option<ConvertResult>, Vec<Error>) {
        let mut err_res = vec![];
        let expr = opt_e.map(|e| {
            let (res, errs) = conversion(e);
            err_res = errs;
            res
        });
        (expr, err_res)
    };
    match e_id.to_expr().clone() {
        Expr::Tuple(v) => {
            let (res_vec, errs): (Vec<_>, Vec<_>) = v.into_iter().map(&conversion).unzip();
            let res_e = Expr::Tuple(res_vec.iter().map(|e| e.expr).collect()).into_id(loc);
            let found_any = res_vec.into_iter().any(|e| e.found_any);
            (
                ConvertResult {
                    expr: res_e,
                    found_any,
                },
                errs.into_iter().flatten().collect(),
            )
        }
        Expr::Proj(e, idx) => {
            let (res, errs) = conversion(e);
            (
                ConvertResult {
                    expr: Expr::Proj(res.expr, idx).into_id(loc),
                    found_any: res.found_any,
                },
                errs,
            )
        }
        Expr::Let(id, body, then) => {
            let (bodyres, errs) = conversion(body);
            let (then_res, errs2) = opt_conversion(then);
            let found_any = bodyres.found_any | then_res.as_ref().map_or(false, |r| r.found_any);
            let expr = Expr::Let(id, bodyres.expr, then_res.map(|r| r.expr)).into_id(loc);
            (
                ConvertResult { expr, found_any },
                errs.into_iter().chain(errs2).into_iter().collect(),
            )
        }
        Expr::LetRec(id, body, then) => {
            let (bodyres, errs) = conversion(body);
            let (then_res, errs2) = opt_conversion(then);
            let found_any = bodyres.found_any | then_res.as_ref().map_or(false, |r| r.found_any);
            let expr = Expr::LetRec(id, bodyres.expr, then_res.map(|r| r.expr)).into_id(loc);
            (
                ConvertResult { expr, found_any },
                errs.into_iter().chain(errs2).into_iter().collect(),
            )
        }
        Expr::Lambda(params, r_type, body) => {
            // Note: params and r_type cannot be handled by conversion() because
            //       these are Type, not Expr.
            let (ConvertResult { expr, found_any }, errs) = conversion(body);
            let expr = Expr::Lambda(params, r_type, expr).into_id(loc);
            (ConvertResult { expr, found_any }, errs)
        }
        Expr::Apply(fun, callee) => {
            let (fun, errs) = conversion(fun);
            let (res_vec, errs2): (Vec<_>, Vec<_>) = callee.into_iter().map(&conversion).unzip();
            let expr = Expr::Apply(fun.expr, res_vec.iter().map(|e| e.expr).collect()).into_id(loc);
            let found_any = res_vec.into_iter().any(|e| e.found_any) | fun.found_any;
            (
                ConvertResult { expr, found_any },
                errs2
                    .into_iter()
                    .flatten()
                    .chain(errs.into_iter())
                    .collect(),
            )
        }
        Expr::PipeApply(callee, fun) => {
            let (callee, errs) = conversion(callee);
            let (fun, errs2) = conversion(fun);
            let expr = Expr::PipeApply(callee.expr, fun.expr).into_id(loc);
            let found_any = callee.found_any | fun.found_any;
            (
                ConvertResult { expr, found_any },
                errs.into_iter().chain(errs2).into_iter().collect(),
            )
        }
        Expr::If(cond, then, opt_else) => {
            let (cond, err) = conversion(cond);
            let (then, err2) = conversion(then);
            let (opt_else, errs3) = opt_conversion(opt_else);
            let found_any =
                cond.found_any | then.found_any | opt_else.as_ref().map_or(false, |e| e.found_any);
            let expr = Expr::If(cond.expr, then.expr, opt_else.map(|e| e.expr)).into_id(loc);
            let errs = err
                .into_iter()
                .chain(err2.into_iter().chain(errs3.into_iter()))
                .collect();
            (ConvertResult { expr, found_any }, errs)
        }
        Expr::Block(body) => {
            let (body, errs) = opt_conversion(body);
            let found_any = body.as_ref().map_or(false, |b| b.found_any);
            let expr = Expr::Block(body.map(|e| e.expr)).into_id(loc);
            (ConvertResult { expr, found_any }, errs)
        }

        _ => (
            ConvertResult {
                expr: e_id,
                found_any: false,
            },
            vec![],
        ),
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum FeedId {
    Global,
    Local(i64),
}
impl FeedId {
    pub(self) fn get_next_id(self) -> Self {
        match self {
            Self::Global => Self::Local(0),
            Self::Local(i) => Self::Local(i + 1),
        }
    }
    pub(self) fn get_name(&self) -> Symbol {
        match self {
            FeedId::Global => "feed_global".to_symbol(),
            FeedId::Local(i) => format!("feed_id{i}").to_symbol(),
        }
    }
}

fn convert_self(e_id: ExprNodeId, feedctx: FeedId) -> Result<ConvertResult, Error> {
    let conversion = |e: ExprNodeId| -> Result<ConvertResult, Error> { convert_self(e, feedctx) };
    let span = e_id.to_span().clone();
    match e_id.to_expr().clone() {
        Expr::Literal(Literal::SelfLit) => match feedctx {
            FeedId::Global => Err(Error::NoParentSelf(span.clone())),
            FeedId::Local(i) => Ok(ConvertResult::Err(
                Expr::Var(feedctx.get_name()).into_id(span.clone()),
            )),
        },
        Expr::Lambda(params, r_type, body) => {
            let nfctx = feedctx.get_next_id();
            let nbody = match convert_self(body, nfctx)? {
                ConvertResult::Err(nbody) => {
                    Expr::Feed(nfctx.get_name(), nbody).into_id(span.clone())
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
