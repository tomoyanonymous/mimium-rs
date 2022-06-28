use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use ast::{expr::*, metadata::*};
use chumsky::prelude::*;
use lexer::*;
use std::fmt::Display;
use std::hash::Hash;
use token::*;
// pub mod chumsky_test;
pub mod lexer;

pub fn report_error<E: Hash + Eq + Display>(src: &String, errs: Vec<Simple<E>>) {
    for e in errs {
        let message = match e.reason() {
            chumsky::error::SimpleReason::Unexpected
            | chumsky::error::SimpleReason::Unclosed { .. } => {
                format!(
                    "{}{}, expected {}",
                    if e.found().is_some() {
                        "unexpected token"
                    } else {
                        "unexpected end of input"
                    },
                    if let Some(label) = e.label() {
                        format!(" while parsing {}", label.fg(Color::Green))
                    } else {
                        " something else".to_string()
                    },
                    if e.expected().count() == 0 {
                        "somemething else".to_string()
                    } else {
                        e.expected()
                            .map(|expected| match expected {
                                Some(expected) => expected.to_string(),
                                None => "end of input".to_string(),
                            })
                            .collect::<Vec<_>>()
                            .join(", ")
                    }
                )
            }
            chumsky::error::SimpleReason::Custom(msg) => msg.clone(),
        };

        Report::build(ReportKind::Error, (), e.span().start)
            .with_message(message)
            .with_label(Label::new(e.span()).with_message(match e.reason() {
                chumsky::error::SimpleReason::Custom(msg) => msg.clone(),
                _ => format!(
                        "Unexpected {}",
                        e.found()
                            .map(|c| format!("token {}", c.fg(Color::Red)))
                            .unwrap_or_else(|| "end of input".to_string())
                    ),
            }))
            .finish()
            .print(Source::from(src))
            .unwrap();
    }
}

pub fn parser() -> impl Parser<Token, WithMeta<Expr>, Error = Simple<Token>> + Clone {
    let lvar = select! { Token::Ident(s) => TypedId { id: s, ty: None } }.labelled("lvar");
    let fnparams = lvar
        .map_with_span(|e, s| (e, s))
        .separated_by(just(Token::Comma))
        .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd));

    let val = select! {
        Token::Ident(s) => Expr::Var(s,None),
        Token::Int(x) => Expr::Literal(Literal::Int(x)),
        Token::Float(x) =>Expr::Literal(Literal::Float(x.parse().unwrap())),
        Token::Str(s) => Expr::Literal(Literal::String(s)),
        Token::SelfLit => Expr::Literal(Literal::SelfLit()),
        Token::Now => Expr::Literal(Literal::Now()),
    }
    .labelled("value");

    let expr = recursive(|expr| {
        let parenexpr = expr
            .clone()
            .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd));

        let atom = val.or(parenexpr).map_with_span(|e, span: Span| (e, span));

        let apply = atom
            .clone()
            .then_ignore(just(Token::ParenBegin))
            .then(atom.clone())
            .then_ignore(just(Token::ParenEnd))
            .map(|(fun, callee)| {
                (
                    Expr::Apply(Box::new(fun.clone()), Box::new(callee.clone())),
                    fun.1.start..callee.1.end,
                )
            })
            .labelled("apply");
        let lambda = lvar
            .map_with_span(|id, span| (id, span))
            .separated_by(just(Token::Comma))
            .delimited_by(
                just(Token::LambdaArgBeginEnd),
                just(Token::LambdaArgBeginEnd),
            )
            .then(expr.clone().map_with_span(|e, s| (e, s)))
            .map_with_span(|(ids, block), s| (Expr::Function(ids, Box::new(block)), s))
            .labelled("lambda");

        let let_e = just(Token::Let)
            .ignore_then(lvar)
            .then_ignore(just(Token::Assign))
            .then(expr.clone().map_with_span(|e, s| Box::new((e, s))))
            .then_ignore(just(Token::LineBreak))
            .then(expr.clone().map_with_span(|e, s| Box::new((e, s))).or_not())
            .map_with_span(|((ident, body), then), span| (Expr::Let(ident, body, then), span))
            .labelled("let");
        let block = expr
            .clone()
            .delimited_by(just(Token::BlockBegin), just(Token::BlockEnd))
            .map_with_span(|e, span: Span| (Expr::Block(Some(Box::new((e, span.clone())))), span))
            .recover_with(nested_delimiters(
                Token::BlockBegin,
                Token::BlockEnd,
                [
                    (Token::ArrayBegin, Token::ArrayEnd),
                    (Token::ParenBegin, Token::ParenEnd),
                ],
                |span| (Expr::Error, span),
            ))
            .labelled("block");
        let function_s = just(Token::Function)
            .ignore_then(lvar)
            .then(fnparams.clone())
            .then(block.clone())
            .then(expr.clone().map_with_span(|e, s| Box::new((e, s))).or_not())
            .map_with_span(|(((fname, ids), block), then), _s: Span| {
                (
                    Expr::LetRec(
                        fname,
                        Box::new((Expr::Function(ids, Box::new(block)), _s.clone())),
                        then,
                    ),
                    _s,
                )
            });

        let macro_s = just(Token::Macro)
            .ignore_then(lvar)
            .then(fnparams.clone())
            .then(
                block
                    .clone()
                    .map_with_span(|e, s| (Expr::Bracket(Box::new(e)), s)),
            )
            .then(expr.clone().map_with_span(|e, s| Box::new((e, s))).or_not())
            .map_with_span(|(((fname, ids), block), then), _s: Span| {
                (
                    Expr::LetRec(
                        fname,
                        Box::new((Expr::Function(ids, Box::new(block)), _s.clone())),
                        then,
                    ),
                    _s,
                )
            });

        let_e
            .or(apply)
            .or(lambda)
            .or(val.map_with_span(|e, s| (e, s)))
            .or(block)
            .or(function_s)
            .or(macro_s)
            .map(|(e, _s)| e)
        // atom.or(apply)
        // .or(lambda)
        // .or(add)
        // .or(r#let)
    });
    expr.then_ignore(end()).map_with_span(|e, s| (e, s))
}
#[cfg(test)]
mod tests {
    use super::*;
    use chumsky::stream::Stream;

    fn parse(
        src: String,
    ) -> (
        Option<WithMeta<Expr>>,
        Vec<Simple<char>>,
        Vec<Simple<Token>>,
    ) {
        let len = src.chars().count();
        let (tokens, lex_errs) = lexer().parse_recovery(src.clone());
        dbg!(tokens.clone());
        let res = match tokens {
            Some(token) => {
                let (ast, parse_errs) =
                    parser().parse_recovery(Stream::from_iter(len..len + 1, token.into_iter()));
                // dbg!(ast.clone());
                (ast, lex_errs, parse_errs)
            }
            None => (None, lex_errs, Vec::new()),
        };
        res
    }

    macro_rules! test_string {
        ($src:literal, $ans:expr) => {
            let srcstr = $src.to_string();
            let (ast, lex_err, parse_err) = parse(srcstr.clone());
            if (lex_err.len() > 0 || parse_err.len() > 0) {
                report_error(&srcstr, parse_err);
                panic!();
            }
            if let Some(a) = ast {
                assert_eq!(a, $ans);
            } else {
                panic!();
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
                Box::new((Expr::Literal(Literal::Int(36)), 11..13)),
                Some(Box::new((Expr::Var("goge".to_string(), None), 15..19))),
            ),
            0..19,
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
        let ans = (Expr::Literal(Literal::String("teststr".to_string())), 0..9);
        test_string!("\"teststr\"", ans);
    }
    #[test]
    pub fn test_block() {
        let ans = (
            Expr::Block(Some(Box::new((
                Expr::Let(
                    TypedId {
                        ty: None,
                        id: "hoge".to_string(),
                    },
                    Box::new((Expr::Literal(Literal::Int(100)), 12..15)),
                    Some(Box::new((Expr::Var("hoge".to_string(), None), 18..22))),
                ),
                0..23,
            )))),
            0..23,
        );
        test_string!("{let hoge = 100 \n hoge}", ans);
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
            0..13,
        );
        test_string!("myfun(callee)", ans);
    }
    #[test]
    #[should_panic]
    pub fn test_fail() {
        let src = "let 100 == hoge\n fuga";
        let (_ans, lexerr, parse_err) = parse(src.clone().to_string());
        dbg!(_ans);
        let is_lex_err = lexerr.len() > 0;
        let is_par_err = parse_err.len() > 0;
        dbg!(lexerr.clone());
        dbg!(parse_err.clone());
        if is_lex_err {
            report_error(&src.to_string(), lexerr.clone());
        }
        if is_par_err {
            report_error(&src.to_string(), parse_err.clone());
        }
        if is_lex_err || is_par_err {
            panic!()
        }
    }
}
