use crate::ast::{Expr, Literal, Symbol, ToSymbol};
use crate::utils::{
    error::ReportableError,
    metadata::{Span, WithMeta},
};
use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub enum Error {
    NoParentSelf(Span),
}

type ConvertResult = Result<WithMeta<Expr>, WithMeta<Expr>>;
fn get_content(e: ConvertResult) -> WithMeta<Expr> {
    match e {
        Ok(e) | Err(e) => e.clone(),
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

fn convert_self(expr: WithMeta<Expr>, feedctx: FeedId) -> Result<ConvertResult, Error> {
    let cls = |e: WithMeta<Expr>| -> Result<ConvertResult, Error> { convert_self(e, feedctx) };
    let opt_cls = |opt_e: Option<Box<WithMeta<Expr>>>| -> Result<Option<ConvertResult>, Error> {
        opt_e.map(|t| cls(*t)).transpose()
    };
    let WithMeta(e, span) = expr.clone();
    match e.clone() {
        Expr::Literal(Literal::SelfLit) => match feedctx {
            FeedId::Global => Err(Error::NoParentSelf(span.clone())),
            FeedId::Local(i) => Ok(ConvertResult::Err(WithMeta(
                Expr::Var(get_feedvar_name(i), None),
                span.clone(),
            ))),
        },
        Expr::Tuple(v) => {
            let elems: Vec<ConvertResult> = v.into_iter().map(&cls).try_collect()?;
            let elems_mapped: Vec<WithMeta<Expr>> =
                elems.iter().map(|e| get_content(e.clone())).collect();
            if elems.iter().any(|e| e.is_err()) {
                Ok(ConvertResult::Err(WithMeta(
                    Expr::Tuple(elems_mapped),
                    span,
                )))
            } else {
                Ok(ConvertResult::Ok(WithMeta(Expr::Tuple(elems_mapped), span)))
            }
        }

        Expr::Proj(e, idx) => {
            let elem = cls(*e.make_withmeta())?;
            Ok(elem.map(|e| (WithMeta(Expr::Proj(e.into_id(), idx), span))))
        }
        Expr::Let(id, body, then) => {
            let body = cls(*body.make_withmeta())?;
            let then = opt_cls(then.map(|x| x.make_withmeta()))?;
            if let (Ok(b), Ok(t)) = (body.clone(), then.clone().transpose()) {
                Ok(ConvertResult::Ok(WithMeta(
                    Expr::Let(id, b.into_id(), t.map(|e| e.into_id())),
                    span,
                )))
            } else {
                Ok(ConvertResult::Err(WithMeta(
                    Expr::Let(
                        id,
                        get_content(body).into_id(),
                        then.map(|t| get_content(t).into_id()),
                    ),
                    span,
                )))
            }
        }
        Expr::LetRec(id, body, then) => {
            let body = cls(*body.make_withmeta())?;
            let then = opt_cls(then.map(|x| x.make_withmeta()))?;
            if let (Ok(b), Ok(t)) = (body, then.transpose()) {
                Ok(ConvertResult::Ok(WithMeta(
                    Expr::LetRec(id, b.into_id(), t.map(|e| e.into_id())),
                    span,
                )))
            } else {
                Ok(ConvertResult::Err(expr.clone()))
            }
        }

        Expr::Lambda(params, r_type, body) => {
            let nfctx = get_new_feedid(feedctx);
            let nbody = match convert_self(*body.make_withmeta(), FeedId::Local(nfctx))? {
                ConvertResult::Err(nbody) => {
                    let feedid = get_feedvar_name(nfctx);
                    WithMeta(Expr::Feed(feedid, nbody.into_id()), span.clone())
                }
                ConvertResult::Ok(nbody) => nbody.clone(),
            };
            Ok(ConvertResult::Ok(WithMeta(
                Expr::Lambda(params, r_type, nbody.into_id()),
                span.clone(),
            )))
        }
        Expr::Apply(fun, callee) => {
            let fun = cls(*fun.make_withmeta())?;
            let elems: Vec<ConvertResult> = callee.into_iter().map(|e| cls(e)).try_collect()?;
            let elems_mapped: Vec<WithMeta<Expr>> =
                elems.iter().map(|e| get_content(e.clone())).collect();
            let content = WithMeta(
                Expr::Apply(fun.clone().unwrap().into_id(), elems_mapped),
                span,
            );
            if fun.is_ok() && elems.iter().find(|e| e.is_err()).is_none() {
                Ok(ConvertResult::Ok(content))
            } else {
                Ok(ConvertResult::Err(content))
            }
        }
        Expr::If(cond, then, opt_else) => {
            let cond = cls(*cond)?;
            let then = cls(*then)?;
            let opt_else = opt_cls(opt_else)?;
            match (cond, then, opt_else.transpose()) {
                (Ok(c), Ok(t), Ok(e)) => Ok(ConvertResult::Ok(WithMeta(
                    Expr::If(Box::new(c), Box::new(t), e.map(|e| Box::new(e))),
                    span,
                ))),
                (c, t, e) => {
                    let e = match e {
                        Ok(e) => e.map(|e| Box::new(e)),
                        Err(e) => Some(Box::new(e)),
                    };
                    Ok(ConvertResult::Err(WithMeta(
                        Expr::If(Box::new(get_content(c)), Box::new(get_content(t)), e),
                        span,
                    )))
                }
            }
        }
        Expr::Block(body) => {
            if let Some(body) = body {
                Ok(cls(*body.make_withmeta())?
                    .map(|e| WithMeta(Expr::Block(Some(e.0.into_id(e.1))), span)))
            } else {
                Ok(ConvertResult::Ok(WithMeta(Expr::Block(None), span)))
            }
        }
        Expr::Feed(_, _) => panic!(
            "Feed should not be shown before conversion at {}..{}",
            span.start, span.end
        ),
        _ => Ok(ConvertResult::Ok(expr.clone())),
    }
}

pub fn convert_self_top(expr: WithMeta<Expr>) -> Result<WithMeta<Expr>, Error> {
    let res = convert_self(expr, FeedId::Global)?;
    Ok(get_content(res))
}

#[cfg(test)]
mod test {
    use crate::pattern::{Pattern, TypedId, TypedPattern};

    use super::*;

    #[test]
    pub fn test_selfconvert() {
        let src = WithMeta(
            Expr::Let(
                WithMeta(
                    TypedPattern {
                        pat: Pattern::Single("lowpass".to_symbol()),
                        ty: None,
                    },
                    0..1,
                ),
                Expr::Lambda(
                    vec![WithMeta::<_>(
                        TypedId {
                            id: "input".to_symbol(),
                            ty: None,
                        },
                        0..1,
                    )],
                    None,
                    Expr::Literal(Literal::SelfLit).into_id(0..1),
                )
                .into_id(0..1),
                None,
            ),
            0..1,
        );
        let WithMeta(res, _) = convert_self_top(src).unwrap();

        let ans = Expr::Let(
            WithMeta(
                TypedPattern {
                    pat: Pattern::Single("lowpass".to_symbol()),
                    ty: None,
                },
                0..1,
            ),
            Expr::Lambda(
                vec![WithMeta::<_>(
                    TypedId {
                        ty: None,
                        id: "input".to_symbol(),
                    },
                    0..1,
                )],
                None,
                Expr::Feed(
                    "feed_id0".to_symbol(),
                    Expr::Var("feed_id0".to_symbol(), None).into_id(0..1),
                )
                .into_id(0..1),
            )
            .into_id(0..1),
            None,
        );
        assert_eq!(res, ans);
    }
}
