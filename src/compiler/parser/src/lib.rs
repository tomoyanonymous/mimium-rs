use ariadne::{Color, Fmt};
use ast::expr::*;
use chumsky::prelude::*;
use chumsky::Parser;
use std::fmt;
use std::hash::Hash;
use token::*;
use utils::error::ReportableError;
use utils::metadata::*;
// pub mod chumsky_test;
pub mod lexer;

// pub struct LexError(chumsky::error::Simple<char>);
#[derive(Debug)]
pub struct ParseError<T>(chumsky::error::Simple<T>)
where
    T: Hash + std::cmp::Eq + fmt::Debug + fmt::Display;

impl<T> Into<chumsky::error::Simple<T>> for ParseError<T>
where
    T: Hash + std::cmp::Eq + fmt::Debug + fmt::Display,
{
    fn into(self) -> chumsky::error::Simple<T> {
        self.0
    }
}
impl<T> fmt::Display for ParseError<T>
where
    T: Hash + std::cmp::Eq + fmt::Debug + fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl<T> std::error::Error for ParseError<T> where T: Hash + std::cmp::Eq + fmt::Debug + fmt::Display {}

impl<T> ReportableError for ParseError<T>
where
    T: Hash + std::cmp::Eq + fmt::Debug + fmt::Display,
{
    fn get_span(&self) -> utils::metadata::Span {
        self.0.span()
    }
    fn get_message(&self) -> String {
        match self.0.reason() {
            chumsky::error::SimpleReason::Unexpected
            | chumsky::error::SimpleReason::Unclosed { .. } => {
                format!(
                    "{}{}, expected {}",
                    if self.0.found().is_some() {
                        "unexpected token"
                    } else {
                        "unexpected end of input"
                    },
                    if let Some(label) = self.0.label() {
                        format!(" while parsing {}", label.fg(Color::Green))
                    } else {
                        " something else".to_string()
                    },
                    if self.0.expected().count() == 0 {
                        "somemething else".to_string()
                    } else {
                        self.0
                            .expected()
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
        }
    }
    fn get_label(&self) -> String {
        match self.0.reason() {
            chumsky::error::SimpleReason::Custom(msg) => msg.clone(),
            _ => format!(
                "Unexpected {}",
                self.0
                    .found()
                    .map(|c| format!("token {}", c.fg(Color::Red)))
                    .unwrap_or_else(|| "end of input".to_string())
            ),
        }
    }
}

fn parser() -> impl Parser<Token, WithMeta<Expr>, Error = Simple<Token>> + Clone {
    let lvar = select! { Token::Ident(s) => TypedId { id: s, ty: None } }.labelled("lvar");
    let fnparams = lvar
        .map_with_span(|e, s| WithMeta::<_>(e, s))
        .separated_by(just(Token::Comma))
        .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd))
        .labelled("fnparams");

    let val = select! {
        Token::Ident(s) => Expr::Var(s,None),
        Token::Int(x) => Expr::Literal(Literal::Int(x)),
        Token::Float(x) =>Expr::Literal(Literal::Float(x.parse().unwrap())),
        Token::Str(s) => Expr::Literal(Literal::String(s)),
        Token::SelfLit => Expr::Literal(Literal::SelfLit),
        Token::Now => Expr::Literal(Literal::Now),
    }
    .map_with_span(|e, s| WithMeta(e, s))
    .labelled("value");
    recursive(|expr| {
        let parenexpr = expr
            .clone()
            .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd))
            .map_with_span(|e, s| WithMeta::<_>(e, s))
            .labelled("paremexpr");

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
            })
            .labelled("function decl");
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
            })
            .labelled("macro definition");
        let macro_expand = select! { Token::MacroExpand(s) => Expr::Var(s,None) }
            .map_with_span(|e, s| WithMeta::<_>(e, s))
            .then_ignore(just(Token::ParenBegin))
            .then(expr.clone().map_with_span(|e, s| WithMeta::<_>(e, s)))
            .then_ignore(just(Token::ParenEnd))
            .map_with_span(|(id, then), s: Span| {
                WithMeta::<_>(
                    Expr::Escape(Box::new(WithMeta::<_>(
                        Expr::Apply(Box::new(id), vec![then]),
                        s.clone(),
                    ))),
                    s,
                )
            })
            .labelled("macroexpand");

        let atom = val
            .or(lambda)
            .or(let_e)
            .or(block)
            .or(function_s)
            .or(macro_s)
            .or(macro_expand)
            .or(parenexpr)
            .labelled("atoms");
        let items = expr
            .clone()
            .map_with_span(|e, s| WithMeta(e, s))
            .separated_by(just(Token::Comma))
            .allow_trailing();
        let apply = atom
            .clone()
            .then(
                items
                    .clone()
                    .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd))
                    .map_with_span(|e, s: Span| (e, s)),
            )
            .map(|(fun, (callee, cspan))| {
                WithMeta::<_>(
                    Expr::Apply(Box::new(fun.clone()), callee),
                    fun.1.start..cspan.end,
                )
            })
            .labelled("apply");
        apply.or(atom).map(|WithMeta(e, _s)| e)
        // atom.or(apply)
        // .or(lambda)
        // .or(add)
        // .or(r#let)
    })
    .then_ignore(end())
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

impl std::fmt::Display for Errors {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?},", self)
    }
}

impl std::error::Error for Errors {}

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
    use std::path::PathBuf;
    fn parse(src: String) -> (Option<WithMeta<Expr>>, Vec<Box<dyn ReportableError>>) {
        let len = src.chars().count();
        let mut errs = Vec::<Box<dyn ReportableError>>::new();
        let (tokens, lex_errs) = lexer().parse_recovery(src.clone());
        lex_errs
            .iter()
            .for_each(|e| errs.push(Box::new(ParseError::<_>(e.clone()))));

        dbg!(tokens.clone());
        let res = match tokens {
            Some(token) => {
                let (ast, parse_errs) =
                    parser().parse_recovery(Stream::from_iter(len..len + 1, token.into_iter()));
                parse_errs
                    .iter()
                    .for_each(|e| errs.push(Box::new(ParseError::<_>(e.clone()))));
                // dbg!(ast.clone());
                (ast, errs)
            }
            None => (None, errs),
        };
        res
    }

    macro_rules! test_string {
        ($src:literal, $ans:expr) => {
            let srcstr = $src.to_string();
            let (ast, errs) = parse(srcstr.clone());
            if (errs.len() > 0) {
                utils::error::report(&srcstr, PathBuf::new(), errs);
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
                Box::new(WithMeta(Expr::Var("myfun".to_string(), None), 0..5)),
                vec![WithMeta(Expr::Var("callee".to_string(), None), 6..12)],
            ),
            0..13,
        );
        test_string!("myfun(callee)", ans);
    }
    #[test]
    pub fn test_applynested() {
        let ans = WithMeta::<_>(
            Expr::Apply(
                Box::new(WithMeta(Expr::Var("myfun".to_string(), None), 0..5)),
                vec![WithMeta(
                    Expr::Apply(
                        Box::new(WithMeta(Expr::Var("myfun2".to_string(), None), 6..12)),
                        vec![WithMeta(Expr::Var("callee".to_string(), None), 13..19)],
                    ),
                    6..20,
                )],
            ),
            0..21,
        );
        test_string!("myfun(myfun2(callee))", ans);
    }
    #[test]
    pub fn test_macroexpand() {
        let ans = WithMeta(
            Expr::Escape(Box::new(WithMeta(
                Expr::Apply(
                    Box::new(WithMeta(Expr::Var("myfun".to_string(), None), 0..6)),
                    vec![WithMeta(Expr::Var("callee".to_string(), None), 7..13)],
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
        let (_ans, errs) = parse(src.clone().to_string());
        if errs.len() > 0 {
            utils::error::report(&src.to_string(), PathBuf::new(), errs);
            panic!()
        }
    }
}
