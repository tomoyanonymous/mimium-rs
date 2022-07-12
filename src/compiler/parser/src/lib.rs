use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use ast::expr::*;
use chumsky::prelude::*;
use chumsky::Parser;
use std::fmt::Display;
use std::hash::Hash;
use token::*;
use utils::metadata::*;
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

fn parser() -> impl Parser<Token, WithMeta<Expr>, Error = Simple<Token>> + Clone {
    let lvar = select! { Token::Ident(s) => TypedId { id: s, ty: None } }.labelled("lvar");
    let fnparams = lvar
        .map_with_span(|e, s| WithMeta::<_>(e, s))
        .separated_by(just(Token::Comma))
        .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd));

    let val = select! {
        Token::Ident(s) => Expr::Var(s,None),
        Token::Int(x) => Expr::Literal(Literal::Int(x)),
        Token::Float(x) =>Expr::Literal(Literal::Float(x.parse().unwrap())),
        Token::Str(s) => Expr::Literal(Literal::String(s)),
        Token::SelfLit => Expr::Literal(Literal::SelfLit),
        Token::Now => Expr::Literal(Literal::Now),
    }
    .labelled("value");

    let expr = recursive(|expr| {
        let parenexpr = expr
            .clone()
            .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd));

        let atom = val
            .or(parenexpr)
            .map_with_span(|e, span: Span| WithMeta::<_>(e, span));

        let apply = atom
            .clone()
            .then_ignore(just(Token::ParenBegin))
            .then(atom.clone())
            .then_ignore(just(Token::ParenEnd))
            .map(|(fun, callee)| {
                WithMeta::<_>(
                    Expr::Apply(Box::new(fun.clone()), Box::new(callee.clone())),
                    fun.1.start..callee.1.end,
                )
            })
            .labelled("apply");
        let lambda = lvar
            .map_with_span(|id, span| WithMeta::<TypedId>(id, span))
            .separated_by(just(Token::Comma))
            .delimited_by(
                just(Token::LambdaArgBeginEnd),
                just(Token::LambdaArgBeginEnd),
            )
            .then(expr.clone().map_with_span(|e, s| WithMeta::<_>(e, s)))
            .map_with_span(|(ids, block), s| WithMeta::<_>(Expr::Lambda(ids, Box::new(block)), s))
            .labelled("lambda");

        let let_e = just(Token::Let)
            .ignore_then(lvar)
            .then_ignore(just(Token::Assign))
            .then(
                expr.clone()
                    .map_with_span(|e, s| Box::new(WithMeta::<_>(e, s))),
            )
            .then_ignore(just(Token::LineBreak))
            .then(
                expr.clone()
                    .map_with_span(|e, s| Box::new(WithMeta::<_>(e, s)))
                    .or_not(),
            )
            .map_with_span(|((ident, body), then), span| {
                WithMeta::<_>(Expr::Let(ident, body, then), span)
            })
            .labelled("let");
        let block = expr
            .clone()
            .delimited_by(
                just(Token::BlockBegin).then_ignore(just(Token::LineBreak).repeated()),
                just(Token::LineBreak)
                    .repeated()
                    .ignore_then(just(Token::BlockEnd)),
            )
            .map_with_span(|e, span: Span| {
                WithMeta::<_>(
                    Expr::Block(Some(Box::new(WithMeta::<_>(e, span.clone())))),
                    span,
                )
            })
            .recover_with(nested_delimiters(
                Token::BlockBegin,
                Token::BlockEnd,
                [
                    (Token::ArrayBegin, Token::ArrayEnd),
                    (Token::ParenBegin, Token::ParenEnd),
                ],
                |span| WithMeta::<_>(Expr::Error, span),
            ))
            .labelled("block");
        let function_s = just(Token::Function)
            .ignore_then(lvar)
            .then(fnparams.clone())
            .then(block.clone())
            .then(
                expr.clone()
                    .map_with_span(|e, s| Box::new(WithMeta::<_>(e, s)))
                    .or_not(),
            )
            .map_with_span(|(((fname, ids), block), then), _s: Span| {
                WithMeta::<_>(
                    Expr::LetRec(
                        fname,
                        Box::new(WithMeta::<_>(
                            Expr::Lambda(ids, Box::new(block)),
                            _s.clone(),
                        )),
                        then,
                    ),
                    _s,
                )
            });
        //todo:add bracket to return type
        let macro_s = just(Token::Macro)
            .ignore_then(lvar)
            .then(fnparams.clone())
            .then(block.clone().map(|WithMeta::<_>(e, s)| {
                let content = match e {
                    Expr::Block(Some(x)) => Ok(Expr::Bracket(x)),
                    _ => Err(()),
                };
                WithMeta::<_>(
                    Expr::Block(Some(Box::new(WithMeta::<_>(content.unwrap(), s.clone())))),
                    s,
                )
            }))
            .then(
                expr.clone()
                    .map_with_span(|e, s| Box::new(WithMeta::<_>(e, s)))
                    .or_not(),
            )
            .map_with_span(|(((fname, ids), block), then), _s: Span| {
                WithMeta::<_>(
                    Expr::LetRec(
                        fname,
                        Box::new(WithMeta::<_>(
                            Expr::Lambda(ids, Box::new(block)),
                            _s.clone(),
                        )),
                        then,
                    ),
                    _s,
                )
            });
        let macro_expand = select! { Token::MacroExpand(s) => Expr::Var(s,None) }
            .map_with_span(|e, s| WithMeta::<_>(e, s))
            .then_ignore(just(Token::ParenBegin))
            .then(expr.clone().map_with_span(|e, s| WithMeta::<_>(e, s)))
            .then_ignore(just(Token::ParenEnd))
            .map_with_span(|(id, then), s: Span| {
                WithMeta::<_>(
                    Expr::Escape(Box::new(WithMeta::<_>(
                        Expr::Apply(Box::new(id), Box::new(then)),
                        s.clone(),
                    ))),
                    s,
                )
            })
            .labelled("macroexpand");
        let_e
            .or(macro_expand)
            .or(apply)
            .or(lambda)
            .or(val.map_with_span(|e, s| WithMeta::<_>(e, s)))
            .or(block)
            .or(function_s)
            .or(macro_s)
            .map(|WithMeta::<_>(e, _s)| e)
        // atom.or(apply)
        // .or(lambda)
        // .or(add)
        // .or(r#let)
    });
    expr.then_ignore(end())
        .map_with_span(|e, s| WithMeta::<_>(e, s))
}

#[derive(Debug, Clone)]
pub enum Error {
    ParserError(Simple<Token>),
    LexerError(Simple<char>),
}

impl From<Simple<Token>> for Error {
    fn from(e: Simple<Token>) -> Self {
        Error::ParserError(e)
    }
}
impl From<Simple<char>> for Error {
    fn from(e: Simple<char>) -> Self {
        Error::LexerError(e)
    }
}

#[derive(Debug, Clone)]
pub struct Errors(Vec<Error>);

pub fn parse(src: String) -> Result<WithMeta<Expr>, Errors> {
    let len = src.chars().count();
    let mut errs = Errors(vec![]);
    let (tokens, lex_errs) = lexer::lexer().parse_recovery(src.clone());
    lex_errs
        .iter()
        .for_each(|e| errs.0.push(Error::LexerError(e.clone())));

    let t = match tokens {
        Some(t) => Ok(t),
        None => Err(errs.clone()),
    }?;
    let (ast, parse_errs) =
        parser().parse_recovery(chumsky::Stream::from_iter(len..len + 1, t.into_iter()));
    ast.ok_or_else(|| {
        let mut ne = parse_errs
            .into_iter()
            .map(|e: Simple<Token>| Error::ParserError(e))
            .collect();
        errs.0.append(&mut ne);
        errs
    })
}
#[cfg(test)]
mod tests {
    use super::*;
    use chumsky::stream::Stream;
    use lexer::*;

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
        let ans = WithMeta::<_>(
            Expr::Let(
                TypedId {
                    id: "goge".to_string(),
                    ty: None,
                },
                Box::new(WithMeta::<_>(Expr::Literal(Literal::Int(36)), 11..13)),
                Some(Box::new(WithMeta::<_>(
                    Expr::Var("goge".to_string(), None),
                    15..19,
                ))),
            ),
            0..19,
        );
        test_string!("let goge = 36\n goge", ans);
    }
    #[test]
    pub fn test_int() {
        let ans = WithMeta::<_>(Expr::Literal(Literal::Int(3466)), 0..4);
        test_string!("3466", ans);
    }
    #[test]
    pub fn test_string() {
        let ans = WithMeta::<_>(Expr::Literal(Literal::String("teststr".to_string())), 0..9);
        test_string!("\"teststr\"", ans);
    }
    #[test]
    pub fn test_block() {
        let ans = WithMeta::<_>(
            Expr::Block(Some(Box::new(WithMeta::<_>(
                Expr::Let(
                    TypedId {
                        ty: None,
                        id: "hoge".to_string(),
                    },
                    Box::new(WithMeta::<_>(Expr::Literal(Literal::Int(100)), 12..15)),
                    Some(Box::new(WithMeta::<_>(
                        Expr::Var("hoge".to_string(), None),
                        18..22,
                    ))),
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
        let ans = WithMeta::<_>(Expr::Var("hoge".to_string(), None), 0..4);
        test_string!("hoge", ans);
    }
    #[test]
    pub fn test_apply() {
        let ans = WithMeta::<_>(
            Expr::Apply(
                Box::new(WithMeta::<_>(Expr::Var("myfun".to_string(), None), 0..5)),
                Box::new(WithMeta::<_>(Expr::Var("callee".to_string(), None), 6..12)),
            ),
            0..13,
        );
        test_string!("myfun(callee)", ans);
    }
    #[test]
    pub fn test_macroexpand() {
        let ans = WithMeta::<_>(
            Expr::Escape(Box::new(WithMeta::<_>(
                Expr::Apply(
                    Box::new(WithMeta::<_>(Expr::Var("myfun".to_string(), None), 0..6)),
                    Box::new(WithMeta::<_>(Expr::Var("callee".to_string(), None), 7..13)),
                ),
                0..14,
            ))),
            0..14,
        );
        test_string!("myfun!(callee)", ans);
    }
    #[test]
    pub fn test_fndef() {
        let ans = WithMeta::<_>(
            Expr::LetRec(
                TypedId {
                    ty: None,
                    id: "hoge".to_string(),
                },
                Box::new(WithMeta::<_>(
                    Expr::Lambda(
                        vec![
                            WithMeta::<_>(
                                TypedId {
                                    ty: None,
                                    id: "input".to_string(),
                                },
                                8..13,
                            ),
                            WithMeta::<_>(
                                TypedId {
                                    ty: None,
                                    id: "gue".to_string(),
                                },
                                14..17,
                            ),
                        ],
                        Box::new(WithMeta::<_>(
                            Expr::Block(Some(Box::new(WithMeta::<_>(
                                Expr::Var("input".to_string(), None),
                                18..28,
                            )))),
                            18..28,
                        )),
                    ),
                    0..28,
                )),
                None,
            ),
            0..28,
        );
        test_string!("fn hoge(input,gue){\n input\n}", ans);
    }
    #[test]
    pub fn test_macrodef() {
        let ans = WithMeta::<_>(
            Expr::LetRec(
                TypedId {
                    ty: None,
                    id: "hoge".to_string(),
                },
                Box::new(WithMeta::<_>(
                    Expr::Lambda(
                        vec![
                            WithMeta::<_>(
                                TypedId {
                                    ty: None,
                                    id: "input".to_string(),
                                },
                                11..16,
                            ),
                            WithMeta::<_>(
                                TypedId {
                                    ty: None,
                                    id: "gue".to_string(),
                                },
                                17..20,
                            ),
                        ],
                        Box::new(WithMeta::<_>(
                            Expr::Block(Some(Box::new(WithMeta::<_>(
                                Expr::Bracket(Box::new(WithMeta::<_>(
                                    Expr::Var("input".to_string(), None),
                                    21..31,
                                ))),
                                21..31,
                            )))),
                            21..31,
                        )),
                    ),
                    0..31,
                )),
                None,
            ),
            0..31,
        );
        test_string!("macro hoge(input,gue){\n input\n}", ans);
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
