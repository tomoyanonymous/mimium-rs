use ast::{expr::*, metadata::*};
use chumsky::prelude::*;

type Error = Simple<char>;

fn make_op_function(opspan: Span) -> Box<WithMeta<Expr>> {
    Box::new(WithMeta::<Expr> {
        value: Expr::Var {
            id: WithMeta::<Id> {
                value: String::from("add"),
                location: opspan,
            },
            t: None,
        },
        location: opspan,
    })
}

pub fn parser() -> impl Parser<char, Expr, Error = Error> {
    let ident = text::ident()
        .map_with_span(|s, span| WithMeta::<TypedId> {
            value: TypedId { id: s, ty: None },
            location: span,
        })
        .padded();

    let expr = recursive(|expr| {
        let int = text::int(10)
            .map_with_span(|s: String, span| {
                Expr::Literal(WithMeta::<Literal> {
                    value: Literal::Int(s.parse().unwrap()),
                    location: span,
                })
            })
            .padded();
        let literal = int;

        let var = text::ident()
            .map_with_span(|s, span| Expr::Var {
                id: WithMeta::<Id> {
                    value: s,
                    location: span,
                },
                t: None,
            })
            .padded();
        let atom = literal.or(var).padded();

        let apply = expr
            .clone()
            .map_with_span(|fun, span| {
                Box::new(WithMeta::<Expr> {
                    value: fun,
                    location: span,
                })
            })
            .then_ignore(just('('))
            .then(expr.clone())
            .then_ignore(just(')'))
            .map_with_span(|(fun, callee), span| Expr::Apply {
                function: fun,
                callee: Box::new(WithMeta::<Expr> {
                    value: callee,
                    location: span,
                }),
            })
            .padded();
        let lambda = just('|')
            .ignore_then(ident)
            .repeated()
            .then_ignore(just('|'))
            .then(expr.clone())
            .map_with_span(|(params, body), span| Expr::Function {
                parameters: params,
                body: Box::new(WithMeta::<Expr> {
                    value: body,
                    location: span, //todo fix span
                }),
            });

        let opspan = |c| just(c).map_with_span(|s, span| span).padded();

        let expr_loc = expr.map_with_span(|s, span| {
            Box::new(WithMeta::<Expr> {
                value: s,
                location: span,
            })
        });
        let add = expr_loc
            .clone()
            .then(opspan('+').then(expr_loc).repeated())
            .foldl(
                |lhs: Box<WithMeta<Expr>>, (opspan, rhs): (Span, Box<WithMeta<Expr>>)| {
                    Box::new(WithMeta::<Expr> {
                        value: Expr::Apply {
                            function: make_op_function(opspan),
                            callee: Box::new(WithMeta::<Expr> {
                                value: Expr::Apply {
                                    function: lhs.clone(),
                                    callee: rhs.clone(),
                                },
                                location: lhs.location.start..rhs.location.end,
                            }),
                        },
                        location: lhs.location.start..rhs.location.end,
                    })
                },
            )
            .map(|s| s.value);

        atom.or(apply.clone()).or(lambda.clone()).or(add.clone())
    });
    let decl = recursive(|decl| {
        let r#let = text::keyword("let")
            .ignore_then(ident)
            .then_ignore(just('='))
            .then(expr.clone())
            .then_ignore(just(';'))
            .then(decl)
            .map_with_span(|((name, rhs), then), span: Span| Expr::Let {
                id: name,
                rhs: Box::new(WithMeta::<Expr> {
                    value: rhs,
                    location: span.clone(),
                }),
                body: Box::new(WithMeta::<Expr> {
                    value: then,
                    location: span,
                }),
            });
        r#let
            // Must be later in the chain than `r#let` to avoid ambiguity
            .or(expr)
            .padded()
    });
    decl.then_ignore(end())
}

#[test]
pub fn test1() {
    let src = String::from("let hoge = 36; 36");

    match parser().parse(src) {
        Ok(ast) => {
            println!("ast: {:?}", ast)
        }
        Err(parse_errs) => parse_errs
            .into_iter()
            .for_each(|e| println!("Parse error: {}", e)),
    }
}
