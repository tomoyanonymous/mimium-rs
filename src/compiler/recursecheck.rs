///remove redundunt letrec definition and convert them to plain let
use crate::{ast::Expr, utils::metadata::WithMeta};

fn try_find_recurse(e_s: &WithMeta<Expr>, name: &String) -> bool {
    let WithMeta(e, _span) = e_s;
    match e {
        Expr::Var(n, _) => n == name,
        Expr::Let(_id, body, then) => {
            try_find_recurse(body, name)
                || then.as_ref().map_or(false, |e| try_find_recurse(&e, name))
        }
        Expr::LetRec(_id, _body, _then) => {
            //todo: start new search so we return false here
            false
        }
        Expr::Lambda(_ids, body) => {
            // convert_self(body)
            try_find_recurse(body, name)
        }
        Expr::Proj(body, _idx) => try_find_recurse(body, name),
        Expr::Block(body) => body.as_ref().map_or(false, |b| try_find_recurse(&b, name)),
        Expr::Apply(fun, callee) => {
            try_find_recurse(fun, name) || callee.into_iter().any(|v| try_find_recurse(&v, name))
        }
        Expr::Tuple(vec) => vec.into_iter().any(|v| try_find_recurse(v, name)),
        Expr::If(cond, then, opt_else) => {
            try_find_recurse(cond, name)
                || try_find_recurse(then, name)
                || opt_else
                    .as_ref()
                    .map_or(false, |e| try_find_recurse(&e, name))
        }
        Expr::Feed(_x, _body) => panic!("feed should not be shown in recurse removal process"),
        _ => false,
    }
}

pub fn convert_recurse(e_s: &WithMeta<Expr>) -> WithMeta<Expr> {
    let convert_vec = |v: &Vec<_>| v.iter().map(|e| convert_recurse(e)).collect::<Vec<_>>();
    let WithMeta(e, span) = e_s;
    let res = match e {
        Expr::LetRec(id, body, then) => {
            if !try_find_recurse(body, &id.id) {
                Expr::Let(
                    id.clone(),
                    convert_recurse(body).into(),
                    then.as_ref().map(|b| Box::new(convert_recurse(b))),
                )
            } else {
                Expr::LetRec(
                    id.clone(),
                    convert_recurse(body).into(),
                    then.as_ref().map(|b| Box::new(convert_recurse(b))),
                )
            }
        }
        Expr::Let(id, body, then) => Expr::Let(
            id.clone(),
            convert_recurse(body).into(),
            then.as_ref().map(|t| convert_recurse(t).into()),
        ),
        Expr::Tuple(es) => Expr::Tuple(convert_vec(es)),
        Expr::Proj(t, idx) => Expr::Proj(convert_recurse(t).into(), *idx),
        Expr::Block(body) => Expr::Block(body.as_ref().map(|b| Box::new(convert_recurse(b)))),
        Expr::Apply(fun, callee) => Expr::Apply(convert_recurse(fun).into(), convert_vec(callee)),
        Expr::If(cond, then, opt_else) => Expr::If(
            convert_recurse(cond).into(),
            convert_recurse(then).into(),
            opt_else.as_ref().map(|e| Box::new(convert_recurse(&e))),
        ),
        Expr::Lambda(ids, body) => Expr::Lambda(ids.clone(), convert_recurse(body).into()),
        Expr::Feed(_x, _body) => panic!("feed should not be shown in recurse removal process"),
        _ => e.clone(),
    };
    WithMeta(res, span.clone())
}

#[cfg(test)]
mod test {

    use crate::{
        app,
        ast::{Expr, Literal, TypedId},
        ifexpr, lambda, let_, letrec, number, var,
    };

    use super::*;
    #[test]
    fn recurse_remove() {
        let sample = letrec!(
            "testfn",
            lambda!(
                vec![WithMeta(
                    TypedId {
                        id: "count".to_string(),
                        ty: None
                    },
                    0..0
                )],
                ifexpr!(
                    var!("test"),
                    app!(var!("testfn"), number!("10.0")),
                    //this letrec should be converted to plain let
                    letrec!("lettest", number!("12.0"), Some(number!("2.0").into()))
                )
            )
            .into(),
            None
        );
        // top letrec should not be converted
        let ans = letrec!(
            "testfn",
            lambda!(
                vec![WithMeta(
                    TypedId {
                        id: "count".to_string(),
                        ty: None
                    },
                    0..0
                )],
                ifexpr!(
                    var!("test"),
                    app!(var!("testfn"), number!("10.0")),
                    // this
                    let_!("lettest", number!("12.0"), Some(number!("2.0").into()))
                )
            )
            .into(),
            None
        );

        assert_eq!(convert_recurse(&sample), ans)
    }
}
