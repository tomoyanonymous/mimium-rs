use ast::expr::*;
use chumsky::prelude::*;
use chumsky::Parser;
use token::*;
use utils::error::ReportableError;
use utils::metadata::*;
// pub mod chumsky_test;
mod error;
pub mod lexer;

fn lvar_parser() -> impl Parser<Token, TypedId, Error = Simple<Token>> + Clone {
    select! { Token::Ident(s) => TypedId { id: s, ty: None } }.labelled("lvar")
}
fn val_parser() -> impl Parser<Token, Expr, Error = Simple<Token>> + Clone {
    select! {
        Token::Ident(s) => Expr::Var(s,None),
        Token::Int(x) => Expr::Literal(Literal::Int(x)),
        Token::Float(x) =>Expr::Literal(Literal::Float(x.parse().unwrap())),
        Token::Str(s) => Expr::Literal(Literal::String(s)),
        Token::SelfLit => Expr::Literal(Literal::SelfLit),
        Token::Now => Expr::Literal(Literal::Now),
    }
    // .map_with_span(|e, s| WithMeta(e, s))
    .labelled("value")
}
fn expr_parser() -> impl Parser<Token, WithMeta<Expr>, Error = Simple<Token>> + Clone {
    let lvar = lvar_parser();
    let val = val_parser();
    let expr_group = recursive(|expr_group| {
        let expr = recursive(|expr| {
            let parenexpr = expr
                .clone()
                .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd))
                .map(|WithMeta(e, _s)| e)
                .labelled("paren_expr");
            let let_e = just(Token::Let)
                .ignore_then(lvar.clone())
                .then_ignore(just(Token::Assign))
                .then(expr.clone())
                .then_ignore(just(Token::LineBreak))
                .then(expr_group.clone().map(|e| Box::new(e)).or_not())
                .map(|((ident, body), then)| Expr::Let(ident, Box::new(body), then))
                .boxed()
                .labelled("let");
            let lambda = lvar
                .clone()
                .map_with_span(|id, span| WithMeta::<TypedId>(id, span))
                .separated_by(just(Token::Comma))
                .delimited_by(
                    just(Token::LambdaArgBeginEnd),
                    just(Token::LambdaArgBeginEnd),
                )
                .then(expr.clone())
                .map(|(ids, body)| Expr::Lambda(ids, Box::new(body)))
                .labelled("lambda");

            let macro_expand = select! { Token::MacroExpand(s) => Expr::Var(s,None) }
                .map_with_span(|e, s| WithMeta(e, s))
                .then_ignore(just(Token::ParenBegin))
                .then(expr.clone())
                .then_ignore(just(Token::ParenEnd))
                .map_with_span(|(id, then), s| {
                    Expr::Escape(Box::new(WithMeta(
                        Expr::Apply(Box::new(id), vec![then]),
                        s.clone(),
                    )))
                })
                .labelled("macroexpand");

            let atom = val
                .or(lambda)
                .or(macro_expand)
                .or(let_e)
                .or(parenexpr)
                .boxed()
                .labelled("atoms");

            let items = expr
                .clone()
                .separated_by(just(Token::Comma))
                .allow_trailing();

            let apply = atom
                .clone()
                .map_with_span(|e, s| WithMeta(e, s))
                .then(
                    items
                        .clone()
                        .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd))
                        .or_not(),
                )
                .map(|(e, callee)| match callee {
                    Some(c) => Expr::Apply(Box::new(e.clone()), c),
                    None => e.0,
                })
                .labelled("apply");

            let op_cls = |x: WithMeta<_>, y: WithMeta<_>, op: Op, opspan: Span| {
                WithMeta(
                    Expr::Apply(
                        Box::new(WithMeta(
                            Expr::Var(op.get_associated_fn_name(), None),
                            opspan,
                        )),
                        vec![x.clone(), y.clone()],
                    ),
                    x.1.start..y.1.end,
                )
            };
            let optoken = move |o: Op| {
                just(Token::Op(o)).map_with_span(|e, s| {
                    (
                        match e {
                            Token::Op(o) => o,
                            _ => Op::Unknown(String::from("invalid")),
                        },
                        s,
                    )
                })
            };

            let op = optoken(Op::Exponent);
            let exponent = apply
                .clone()
                .map_with_span(|e, s| WithMeta(e, s))
                .then(
                    op.then(apply.map_with_span(|e, s| WithMeta(e, s)))
                        .repeated(),
                )
                .foldl(move |x, ((op, opspan), y)| op_cls(x, y, op, opspan))
                .boxed();
            let op = choice((
                optoken(Op::Product),
                optoken(Op::Divide),
                optoken(Op::Modulo),
            ));
            let product = exponent
                .clone()
                .then(op.then(exponent).repeated())
                .foldl(move |x, ((op, opspan), y)| op_cls(x, y, op, opspan))
                .boxed();
            let op = optoken(Op::Sum).or(optoken(Op::Minus));
            let add = product
                .clone()
                .then(op.then(product).repeated())
                .foldl(move |x, ((op, opspan), y)| op_cls(x, y, op, opspan))
                .boxed();

            let op = optoken(Op::Equal).or(optoken(Op::NotEqual));

            let cmp = add
                .clone()
                .then(op.then(add).repeated())
                .foldl(move |x, ((op, opspan), y)| op_cls(x, y, op, opspan))
                .boxed();
            let op = optoken(Op::And);
            let cmp = cmp
                .clone()
                .then(op.then(cmp).repeated())
                .foldl(move |x, ((op, opspan), y)| op_cls(x, y, op, opspan))
                .boxed();
            let op = optoken(Op::Or);
            let cmp = cmp
                .clone()
                .then(op.then(cmp).repeated())
                .foldl(move |x, ((op, opspan), y)| op_cls(x, y, op, opspan))
                .boxed();
            let op = choice((
                optoken(Op::LessThan),
                optoken(Op::LessEqual),
                optoken(Op::GreaterThan),
                optoken(Op::GreaterEqual),
            ));
            let cmp = cmp
                .clone()
                .then(op.then(cmp).repeated())
                .foldl(move |x, ((op, opspan), y)| op_cls(x, y, op, opspan))
                .boxed();
            let op = optoken(Op::Pipe);
            let pipe = cmp
                .clone()
                .then(op.then(cmp).repeated())
                .foldl(|lhs, ((_, _), rhs)| {
                    let span = lhs.1.start..rhs.1.end;
                    WithMeta(Expr::Apply(Box::new(rhs), vec![lhs]), span)
                })
                .boxed();

            pipe
            // atom.or(apply)
            // .or(lambda)
            // .or(add)
            // .or(r#let)
        });
        // expr_group contains let statement, assignment statement, function definiton,... they cannot be placed as an argument for apply directly.
        let block = expr
            .clone()
            .delimited_by(just(Token::BlockBegin), just(Token::BlockEnd))
            .map(|e: WithMeta<Expr>| Expr::Block(Some(Box::new(e))));

        //todo:add bracket to return type

        block.map_with_span(|e, s| WithMeta(e, s)).or(expr.clone())
    });
    expr_group
    // .then_ignore(end())
    // .map_with_span(|e, s| WithMeta(e, s));
}

fn func_parser() -> impl Parser<Token, WithMeta<Expr>, Error = Simple<Token>> + Clone {
    let expr = expr_parser();
    let lvar = lvar_parser();
    let fnparams = lvar
        .clone()
        .map_with_span(|e, s| WithMeta(e, s))
        .separated_by(just(Token::Comma))
        .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd))
        .labelled("fnparams");
    let blockstart = just(Token::BlockBegin).then_ignore(just(Token::LineBreak).repeated());
    let blockend = just(Token::LineBreak)
        .repeated()
        .ignore_then(just(Token::BlockEnd));
    let function_s = just(Token::Function)
        .ignore_then(lvar.clone())
        .then(fnparams.clone())
        .then(
            expr.clone()
                .delimited_by(blockstart.clone(), blockend.clone()),
        )
        .then(expr.clone().map(|e| Box::new(e)).or_not())
        .map_with_span(|(((fname, ids), block), then), _s: Span| {
            WithMeta(
                Expr::LetRec(
                    fname,
                    Box::new(WithMeta(Expr::Lambda(ids, Box::new(block)), _s.clone())),
                    then,
                ),
                _s,
            )
        })
        .labelled("function decl");
    let macro_s = just(Token::Macro)
        .ignore_then(lvar)
        .then(fnparams.clone())
        .then(
            expr.clone()
                .delimited_by(blockstart.clone(), blockend.clone())
                .map(|WithMeta(e, s)| WithMeta(Expr::Bracket(Box::new(WithMeta(e, s.clone()))), s)),
        )
        .then(expr.clone().map(|e| Box::new(e)).or_not())
        .map_with_span(|(((fname, ids), block), then), _s: Span| {
            WithMeta(
                Expr::LetRec(
                    fname,
                    Box::new(WithMeta(Expr::Lambda(ids, Box::new(block)), _s.clone())),
                    then,
                ),
                _s,
            )
        })
        .labelled("macro definition");

    function_s.or(macro_s).or(expr_parser()).then_ignore(end())
    // expr_parser().then_ignore(end())
}

fn parser() -> impl Parser<Token, WithMeta<Expr>, Error = Simple<Token>> + Clone {
    func_parser()
}

pub fn parse(src: String) -> Result<WithMeta<Expr>, Vec<Box<dyn ReportableError>>> {
    let len = src.chars().count();
    let mut errs = Vec::<Box<dyn ReportableError>>::new();
    let (tokens, lex_errs) = lexer::lexer().parse_recovery(src.clone());
    lex_errs
        .iter()
        .for_each(|e| errs.push(Box::new(error::ParseError::<char>(e.clone()))));

    if let Some(t) = tokens {
        let (ast, parse_errs) =
            parser().parse_recovery(chumsky::Stream::from_iter(len..len + 1, t.into_iter()));
        ast.ok_or_else(|| {
            parse_errs
                .iter()
                .for_each(|e| errs.push(Box::new(error::ParseError::<Token>(e.clone()))));
            errs
        })
    } else {
        Err(errs)
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    use chumsky::stream::Stream;
    use error::ParseError;
    use lexer::*;
    use std::path::PathBuf;
    use utils::error::ReportableError;
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
        let ans = WithMeta(
            Expr::Let(
                TypedId {
                    id: "goge".to_string(),
                    ty: None,
                },
                Box::new(WithMeta(Expr::Literal(Literal::Int(36)), 11..13)),
                Some(Box::new(WithMeta(
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
        let ans = WithMeta(Expr::Literal(Literal::Int(3466)), 0..4);
        test_string!("3466", ans);
    }
    #[test]
    pub fn test_string() {
        let ans = WithMeta(Expr::Literal(Literal::String("teststr".to_string())), 0..9);
        test_string!("\"teststr\"", ans);
    }
    #[test]
    pub fn test_block() {
        let ans = WithMeta(
            Expr::Block(Some(Box::new(WithMeta(
                Expr::Let(
                    TypedId {
                        ty: None,
                        id: "hoge".to_string(),
                    },
                    Box::new(WithMeta(Expr::Literal(Literal::Int(100)), 12..15)),
                    Some(Box::new(WithMeta(
                        Expr::Var("hoge".to_string(), None),
                        18..22,
                    ))),
                ),
                1..22,
            )))),
            0..23,
        );
        test_string!("{let hoge = 100 \n hoge}", ans);
    }
    #[test]
    pub fn test_add() {
        let ans = WithMeta(
            Expr::Apply(
                Box::new(WithMeta(Expr::Var("add".to_string(), None), 6..7)),
                vec![
                    WithMeta(Expr::Literal(Literal::Float("3466.0".to_string())), 0..6),
                    WithMeta(Expr::Literal(Literal::Float("2000.0".to_string())), 7..13),
                ],
            ),
            0..13,
        );
        test_string!("3466.0+2000.0", ans);
    }
    #[test]
    pub fn test_var() {
        let ans = WithMeta(Expr::Var("hoge".to_string(), None), 0..4);
        test_string!("hoge", ans);
    }
    #[test]
    pub fn test_apply() {
        let ans = WithMeta(
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
        let ans = WithMeta(
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
        let ans = WithMeta(
            Expr::LetRec(
                TypedId {
                    ty: None,
                    id: "hoge".to_string(),
                },
                Box::new(WithMeta(
                    Expr::Lambda(
                        vec![
                            WithMeta(
                                TypedId {
                                    ty: None,
                                    id: "input".to_string(),
                                },
                                8..13,
                            ),
                            WithMeta(
                                TypedId {
                                    ty: None,
                                    id: "gue".to_string(),
                                },
                                14..17,
                            ),
                        ],
                        Box::new(WithMeta(Expr::Var("input".to_string(), None), 21..26)),
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
        let ans = WithMeta(
            Expr::LetRec(
                TypedId {
                    ty: None,
                    id: "hoge".to_string(),
                },
                Box::new(WithMeta(
                    Expr::Lambda(
                        vec![
                            WithMeta(
                                TypedId {
                                    ty: None,
                                    id: "input".to_string(),
                                },
                                11..16,
                            ),
                            WithMeta(
                                TypedId {
                                    ty: None,
                                    id: "gue".to_string(),
                                },
                                17..20,
                            ),
                        ],
                        Box::new(WithMeta(
                            Expr::Bracket(Box::new(WithMeta(
                                Expr::Var("input".to_string(), None),
                                24..29,
                            ))),
                            24..29,
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
