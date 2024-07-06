use crate::ast::{Expr, Literal};
use crate::utils::{
    error::ReportableError,
    metadata::{Span, WithMeta},
};
use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub enum Error {
    NoParentSelf(Span),
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

fn convert_literal(e: Literal) -> bool {
    match e {
        Literal::SelfLit => true,
        _ => false,
    }
}

fn try_find_self(e: Expr) -> bool {
    match e {
        Expr::Literal(l) => convert_literal(l),
        Expr::Let(_id, body, then) => {
            try_find_self(body.0) || then.map_or(false, |e| try_find_self(e.0))
        }
        Expr::LetRec(_id, body, then) => {
            //todo: detect self in recursive function in same stage
            try_find_self(body.0) || then.map_or(false, |e| try_find_self(e.0))
        }
        Expr::Lambda(_ids, _, _body) => {
            // convert_self(body)
            false
        }
        Expr::Proj(body, _idx) => try_find_self(body.0),
        Expr::Block(body) => body.map_or(false, |b| try_find_self(b.0)),
        Expr::Apply(fun, callee) => {
            try_find_self(fun.0) || callee.into_iter().any(|v| try_find_self(v.0))
        }
        Expr::Tuple(vec) => vec.into_iter().any(|v| try_find_self(v.0)),
        Expr::If(cond, then, opt_else) => {
            try_find_self(cond.0)
                || try_find_self(then.0)
                || opt_else.map_or(false, |e| try_find_self(e.0))
        }
        Expr::Feed(_x, _body) => panic!("feed should not be shown in self_conversion process"),
        _ => false,
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

fn get_feedvar_name(fid: i64) -> String {
    //todo:need to assign true unique name
    format!("feed_id{}", fid)
}
fn convert_self(expr: WithMeta<Expr>, feedctx: FeedId) -> Result<WithMeta<Expr>, Error> {
    let cls = |e: WithMeta<Expr>| -> Result<WithMeta<Expr>, Error> { convert_self(e, feedctx) };
    let opt_cls =
        |opt_e: Option<Box<WithMeta<Expr>>>| -> Result<Option<Box<WithMeta<Expr>>>, Error> {
            Ok(opt_e.map(|t| cls(*t)).transpose()?.map(|e| Box::new(e)))
        };
    let WithMeta(e, span) = expr.clone();
    let new_e_res = match e.clone() {
        Expr::Literal(Literal::SelfLit) => match feedctx {
            FeedId::Global => Err(Error::NoParentSelf(span.clone())),
            FeedId::Local(i) => Ok(Expr::Var(get_feedvar_name(i), None)),
        },
        Expr::Tuple(v) => Ok(Expr::Tuple(v.into_iter().map(|e| cls(e)).try_collect()?)),

        Expr::Proj(e, idx) => Ok(Expr::Proj(Box::new(cls(*e)?), idx)),
        Expr::Let(id, body, then) => Ok(Expr::Let(id, Box::new(cls(*body)?), opt_cls(then)?)),
        Expr::LetRec(id, body, then) => Ok(Expr::LetRec(id, Box::new(cls(*body)?), opt_cls(then)?)),

        Expr::Lambda(params, r_type, body) => {
            let nfctx = get_new_feedid(feedctx);
            let feedid = get_feedvar_name(nfctx);
            if try_find_self(body.clone().0) {
                let nbody = convert_self(*body, FeedId::Local(nfctx))?;
                // the conversion must be lambda(x).feed(self).e , not feed(self).lambda(x).e
                Ok(Expr::Lambda(
                    params,
                    r_type,
                    WithMeta(Expr::Feed(feedid, nbody.into()).into(), span.clone()).into(),
                ))
            } else {
                Ok(e.clone())
            }
        }
        Expr::Apply(fun, callee) => Ok(Expr::Apply(
            Box::new(cls(*fun)?),
            callee.into_iter().map(|e| cls(e)).try_collect()?,
        )),
        Expr::If(cond, then, opt_else) => Ok(Expr::If(
            Box::new(cls(*cond)?),
            Box::new(cls(*then)?),
            opt_cls(opt_else)?,
        )),
        Expr::Block(body) => Ok(Expr::Block(opt_cls(body)?)),
        Expr::Feed(_, _) => panic!(
            "Feed should not be shown before conversion at {}..{}",
            span.start, span.end
        ),
        _ => Ok(e.clone()),
    };
    new_e_res.map(|e| WithMeta(e, span))
}

pub fn convert_self_top(expr: WithMeta<Expr>) -> Result<WithMeta<Expr>, Error> {
    convert_self(expr, FeedId::Global)
}

#[cfg(test)]
mod test {
    use crate::ast::TypedId;

    use super::*;

    #[test]
    pub fn test_selfconvert() {
        let src = WithMeta(
            Expr::Let(
                TypedId {
                    id: "lowpass".to_string(),
                    ty: None,
                },
                Box::new(WithMeta::<_>(
                    Expr::Lambda(
                        vec![WithMeta::<_>(
                            TypedId {
                                id: "input".to_string(),
                                ty: None,
                            },
                            0..1,
                        )],
                        None,
                        Box::new(WithMeta::<_>(Expr::Literal(Literal::SelfLit), 0..1)),
                    ),
                    0..1,
                )),
                None,
            ),
            0..1,
        );
        let WithMeta(res, _) = convert_self(src, FeedId::Global).unwrap();
        let ans = Expr::Let(
            TypedId {
                ty: None,
                id: "lowpass".to_string(),
            },
            Box::new(WithMeta::<_>(
                Expr::Lambda(
                    vec![WithMeta::<_>(
                        TypedId {
                            ty: None,
                            id: "input".to_string(),
                        },
                        0..1,
                    )],
                    None,
                    Box::new(WithMeta::<_>(
                        Expr::Feed(
                            "feed_id0".to_string(),
                            Box::new(WithMeta::<_>(Expr::Var("feed_id0".to_string(), None), 0..1)),
                        ),
                        0..1,
                    )),
                ),
                0..1,
            )),
            None,
        );
        assert_eq!(res, ans);
    }
}
