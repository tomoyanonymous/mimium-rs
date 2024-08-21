///remove redundunt letrec definition and convert them to plain let
use crate::{
    ast::{make_withmeta_vec, Expr, ExprId, Symbol},
    pattern::TypedPattern,
    utils::metadata::WithMeta,
};

fn try_find_recurse(e_s: &WithMeta<Expr>, name: &Symbol) -> bool {
    let WithMeta(e, _span) = e_s;
    match e {
        Expr::Var(n, _) => n == name,
        Expr::Let(_id, body, then) => {
            try_find_recurse(&body.make_withmeta(), name)
                || then.map_or(false, |e| try_find_recurse(&e.make_withmeta(), name))
        }
        Expr::LetRec(_id, _body, _then) => {
            //todo: start new search so we return false here
            false
        }
        Expr::Lambda(_ids, _opt_type, body) => {
            // convert_self(body)
            try_find_recurse(&body.make_withmeta(), name)
        }
        Expr::Proj(body, _idx) => try_find_recurse(&body.make_withmeta(), name),
        Expr::Block(body) => body.map_or(false, |b| try_find_recurse(&b.make_withmeta(), name)),
        Expr::Apply(fun, callee) => {
            try_find_recurse(&fun.make_withmeta(), name)
                || callee
                    .into_iter()
                    .any(|v| try_find_recurse(&v.make_withmeta(), name))
        }
        Expr::Tuple(vec) => vec
            .into_iter()
            .any(|v| try_find_recurse(&v.make_withmeta(), name)),
        Expr::If(cond, then, opt_else) => {
            try_find_recurse(&cond.make_withmeta(), name)
                || try_find_recurse(&then.make_withmeta(), name)
                || opt_else.map_or(false, |e| try_find_recurse(&e.make_withmeta(), name))
        }
        Expr::Feed(_x, _body) => panic!("feed should not be shown in recurse removal process"),
        _ => false,
    }
}

pub fn convert_recurse(e_s: &WithMeta<Expr>) -> WithMeta<Expr> {
    let convert_vec = |v: &[ExprId]| {
        v.iter()
            .map(|e| convert_recurse(&e.make_withmeta()).into_id())
            .collect::<Vec<ExprId>>()
    };
    let WithMeta(e, span) = e_s;
    let res = match e {
        Expr::LetRec(id, body, then) => {
            if !try_find_recurse(&body.make_withmeta(), &id.id) {
                Expr::Let(
                    WithMeta(TypedPattern::from(id.clone()), span.clone()),
                    convert_recurse(&body.make_withmeta()).into_id(),
                    then.map(|b| convert_recurse(&b.make_withmeta()).into_id()),
                )
            } else {
                Expr::LetRec(
                    id.clone(),
                    convert_recurse(&body.make_withmeta()).into_id(),
                    then.map(|b| convert_recurse(&b.make_withmeta()).into_id()),
                )
            }
        }
        Expr::Let(id, body, then) => Expr::Let(
            id.clone(),
            convert_recurse(&body.make_withmeta()).into_id(),
            then.map(|t| convert_recurse(&t.make_withmeta()).into_id()),
        ),
        Expr::Tuple(es) => Expr::Tuple(convert_vec(es)),
        Expr::Proj(t, idx) => Expr::Proj(convert_recurse(&t.make_withmeta()).into_id(), *idx),
        Expr::Block(body) => Expr::Block(
            body.map(|b| Box::new(convert_recurse(&b.make_withmeta())))
                .map(|b| b.into_id()),
        ),
        Expr::Apply(fun, callee) => Expr::Apply(
            convert_recurse(&fun.make_withmeta()).into_id(),
            convert_vec(callee),
        ),
        Expr::If(cond, then, opt_else) => Expr::If(
            convert_recurse(&cond.make_withmeta()).into_id(),
            convert_recurse(&then.make_withmeta()).into_id(),
            opt_else.map(|e| convert_recurse(&e.make_withmeta()).into_id()),
        ),
        Expr::Lambda(ids, opt_type, body) => Expr::Lambda(
            ids.clone(),
            opt_type.clone(),
            convert_recurse(&body.make_withmeta()).into_id(),
        ),
        Expr::Feed(_x, _body) => panic!("feed should not be shown in recurse removal process"),
        _ => e.clone(),
    };
    WithMeta(res, span.clone())
}

#[cfg(test)]
mod test {

    use crate::{
        app,
        ast::{Expr, Literal},
        ifexpr, lambda, let_, letrec, number,
        pattern::TypedId,
        var,
    };

    use super::*;
    #[test]
    fn recurse_remove() {
        let sample = letrec!(
            "testfn",
            lambda!(
                ["count"],
                ifexpr!(
                    var!("test"),
                    app!(var!("testfn"), vec![number!("10.0")]),
                    //this letrec should be converted to plain let
                    letrec!("lettest", number!("12.0"), Some(number!("2.0")))
                )
            ),
            None
        );
        // top letrec should not be converted
        let ans = letrec!(
            "testfn",
            lambda!(
                ["count"],
                ifexpr!(
                    var!("test"),
                    app!(var!("testfn"), vec![number!("10.0")]),
                    // this
                    let_!("lettest", number!("12.0"), number!("2.0"))
                )
            ),
            None
        );

        assert_eq!(
            convert_recurse(&sample.make_withmeta()),
            *ans.make_withmeta()
        )
    }
}
