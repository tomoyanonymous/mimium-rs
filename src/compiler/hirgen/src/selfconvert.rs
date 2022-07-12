use ast::expr::*;

use utils::metadata::*;

#[derive(Clone, Debug, PartialEq)]
pub enum Error {
    NoParentSelf(Span),
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
        Expr::Lambda(_ids, _body) => {
            // convert_self(body)
            false
        }
        Expr::Proj(body, _idx) => try_find_self(body.0),
        Expr::Block(body) => body.map_or(false, |b| try_find_self(b.0)),
        Expr::Apply(fun, callee) => try_find_self(fun.0) || try_find_self(callee.0),
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
    "feed_id".to_string() + &fid.to_string()
}

pub fn convert_self(expr: WithMeta<Expr>, feedctx: FeedId) -> WithMeta<Expr> {
    let cls = |e: WithMeta<Expr>| -> WithMeta<Expr> { convert_self(e, feedctx) };
    let WithMeta::<_>(e, span) = expr.clone();
    match e {
        Expr::Literal(l) => match l {
            Literal::SelfLit => {
                let res = match feedctx {
                    FeedId::Global => Expr::Error,
                    FeedId::Local(i) => Expr::Var(get_feedvar_name(i), None),
                };
                WithMeta::<_>(res, span.clone())
            }
            _ => expr.clone(),
        },
        Expr::Tuple(v) => WithMeta::<_>(
            Expr::Tuple(v.into_iter().map(|e| cls(e)).collect()),
            span.clone(),
        ),
        Expr::Proj(e, idx) => WithMeta::<_>(Expr::Proj(Box::new(cls(*e)), idx), span.clone()),
        Expr::Let(id, body, then) => WithMeta::<_>(
            Expr::Let(id, Box::new(cls(*body)), then.map(|t| Box::new(cls(*t)))),
            span.clone(),
        ),
        Expr::LetRec(id, body, then) => WithMeta::<_>(
            Expr::LetRec(id, Box::new(cls(*body)), then.map(|t| Box::new(cls(*t)))),
            span.clone(),
        ),
        Expr::Lambda(params, body) => {
            let nfctx = get_new_feedid(feedctx);
            let feedid = get_feedvar_name(nfctx);
            if try_find_self(body.clone().0) {
                let nbody = convert_self(*body, FeedId::Local(nfctx));
                WithMeta::<_>(
                    Expr::Feed(
                        feedid,
                        Box::new(WithMeta::<_>(
                            Expr::Lambda(params, Box::new(nbody)),
                            span.clone(),
                        )),
                    ),
                    span.clone(),
                )
            } else {
                expr
            }
        }
        Expr::Apply(fun, callee) => WithMeta::<_>(
            Expr::Apply(Box::new(cls(*fun)), Box::new(cls(*callee))),
            span.clone(),
        ),
        Expr::If(cond, then, opt_else) => WithMeta::<_>(
            Expr::If(
                Box::new(cls(*cond)),
                Box::new(cls(*then)),
                opt_else.map(|e| Box::new(cls(*e))),
            ),
            span.clone(),
        ),
        Expr::Block(body) => {
            WithMeta::<_>(Expr::Block(body.map(|b| Box::new(cls(*b)))), span.clone())
        }
        _ => todo!(),
    }
}
#[cfg(test)]
mod test {
    use super::*;

    #[test]
    pub fn test_selfconvert() {
        let src = WithMeta::<_>(
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
                        Box::new(WithMeta::<_>(Expr::Literal(Literal::SelfLit), 0..1)),
                    ),
                    0..1,
                )),
                None,
            ),
            0..1,
        );
        let res = convert_self(src, FeedId::Global).0;
        let ans = Expr::Let(
            TypedId {
                ty: None,
                id: "lowpass".to_string(),
            },
            Box::new(WithMeta::<_>(
                Expr::Feed(
                    "feed_id0".to_string(),
                    Box::new(WithMeta::<_>(
                        Expr::Lambda(
                            vec![WithMeta::<_>(
                                TypedId {
                                    ty: None,
                                    id: "input".to_string(),
                                },
                                0..1,
                            )],
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
