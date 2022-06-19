use ast::{expr::*, metadata::*};
use chumsky::prelude::*;
use lexer::*;
use token::*;

// pub mod chumsky_test;
pub mod lexer;

pub fn parser() -> impl Parser<Token, WithMeta<Expr>, Error = Simple<Token>> + Clone {
    let lvar = select! { Token::Ident(s) => TypedId { id: s, ty: None } }.labelled("lvar");
    let val = select! {
        Token::Ident(s) => Expr::Var(s,None),
        Token::Int(x) => Expr::Literal(Literal::Int(x)),
        Token::Float(x) =>Expr::Literal(Literal::Float(x.parse().unwrap())),
        Token::Str(s) => Expr::Literal(Literal::String(s))
    }
    .map_with_span(|e, s| (e, s));
    let expr = recursive(|expr| {
        // let lambda = ident
        //     .separated_by(just(','))
        //     .delimited_by(just('|'), just('|'))
        //     .then(expr.clone())
        //     .map_with_span(|(params, body), span| {
        //         let f = Expr::Function {
        //             parameters: params,
        //             body: Box::new(body),
        //         };
        //         add_meta(f, span)
        //     })
        //     .padded();;
        let parenexpr = expr
            .clone()
            .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd));

        let atom = val.map_with_span(|e, span: Span| (e, span)).or(parenexpr);

        let apply = atom
            .clone()
            .then_ignore(just(Token::ParenBegin))
            .then(atom.clone())
            .then_ignore(just(Token::ParenEnd))
            .map(|((fun, s1), (callee, s2))| {
                (
                    Expr::Apply(Box::new(fun), Box::new(callee)),
                    s1.start..s2.end,
                )
            });
        let let_e = just(Token::Let)
            .ignore_then(lvar)
            .then_ignore(just(Token::Assign))
            .then(atom.clone().map(|(e, _s)| e))
            .then_ignore(just(Token::LineBreak))
            .then(atom.clone().map(|(e, _s)| e))
            .map_with_span(|((ident, body), then), span| {
                (Expr::Let(ident, Box::new(body), Box::new(then)), span)
            });

        let_e.or(apply).map_with_span(|e, s| (e, s))
        // atom.or(apply)
        // .or(lambda)
        // .or(add)
        // .or(r#let)
    });
    expr.then_ignore(end()).map(|(e, _s)| e)
}
#[cfg(test)]
mod tests {
    use super::*;
    use chumsky::stream::Stream;

    macro_rules! test_string {
        ($src:literal, $ans:expr) => {
            let len = $src.chars().count();
            let (tokens, errs) = lexer().parse_recovery($src);
            // dbg!(tokens.clone());
            let parse_errs = match tokens {
                Some(token) => {
                    let (ast, parse_errs) =
                        parser().parse_recovery(Stream::from_iter(len..len + 1, token.into_iter()));
                    // dbg!(ast.clone());
                    if let Some(asts) = ast.filter(|_| errs.len() + parse_errs.len() == 0) {
                        println!("{:#?}", asts);
                        assert_eq!($ans, asts);
                    }
                    parse_errs
                }
                None => Vec::new(),
            };
            if (errs.len() > 0) {
                errs.into_iter()
                    .map(|e| e.map(|c| c.to_string()))
                    .chain(parse_errs.into_iter().map(|e| e.map(|tok| tok.to_string())))
                    .for_each(|e| println!("{:#?}", e));
                panic!()
            }
        };
    }

    #[test]
    pub fn test_let() {
        let ans = (
            Expr::Let(
                TypedId {
                    id: "goge".to_string(),
                    ty: None,
                },
                Box::new((Expr::Literal(Literal::Int(3466)), 12..13)),
                Box::new((Expr::Var("goge".to_string(), None), 16..20)),
            ),
            0..4,
        );
        test_string!("let goge = 36\n goge", ans);
    }
    #[test]
    pub fn test_int() {
        let ans = (Expr::Literal(Literal::Int(3466)), 0..4);
        test_string!("3466", ans);
    }
    #[test]
    pub fn test_string() {
        let ans = (Expr::Literal(Literal::String("teststr".to_string())), 0..4);
        test_string!("\"teststr\"", ans);
    }
    // #[test]
    // pub fn test_add() {
    //     let ans = WithMeta {
    //         location: 0..4,
    //         value: Expr::Literal(WithMeta {
    //             location: 0..4,
    //             value: Literal::Int(3466),
    //         }),
    //     };
    //     test_string!("3466+2000", ans);
    // }
    #[test]
    pub fn test_var() {
        let ans = (Expr::Var("hoge".to_string(), None), 0..4);
        test_string!("hoge", ans);
    }
    #[test]
    pub fn test_apply() {
        let ans = (
            Expr::Apply(
                Box::new((Expr::Var("myfun".to_string(), None), 0..5)),
                Box::new((Expr::Var("callee".to_string(), None), 6..12)),
            ),
            0..12,
        );
        test_string!("myfun(callee)", ans);
    }
}
