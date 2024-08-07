use crate::ast::*;
use crate::types::{PType, Type, TypedId};
use crate::utils::error::ReportableError;
use crate::utils::metadata::*;
use chumsky::prelude::*;
use chumsky::Parser;
mod token;
use token::{Comment, Op, Token};
mod error;
mod lexer;

#[cfg(test)]
mod test;

fn type_parser() -> impl Parser<Token, Type, Error = Simple<Token>> + Clone {
    recursive(|ty| {
        let primitive = select! {
           Token::FloatType => Type::Primitive(PType::Numeric),
           Token::IntegerType => Type::Primitive(PType::Int),
           Token::StringType => Type::Primitive(PType::String)
        };

        let tuple = ty
            .clone()
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd))
            .map(|e| Type::Tuple(e))
            .boxed()
            .labelled("Tuple");
        // let _struct_t = todo!();
        let atom = primitive.or(tuple);
        let func = atom
            .clone()
            .separated_by(just(Token::Comma))
            .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd))
            .then(just(Token::Arrow).ignore_then(ty.clone()))
            .map(|(a, e)| Type::Function(a, e.into(), None))
            .boxed()
            .labelled("function");

        // .map_with_span(|e, s| WithMeta(e, s))

        func.or(atom).boxed()
    })
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
fn lvar_parser() -> impl Parser<Token, TypedId, Error = Simple<Token>> + Clone {
    select! { Token::Ident(s) => s }
        .then(just(Token::Colon).ignore_then(type_parser()).or_not())
        .map(|(id, t)| TypedId { id: id, ty: t })
        .labelled("lvar")
}

fn expr_parser() -> impl Parser<Token, WithMeta<Expr>, Error = Simple<Token>> + Clone {
    let lvar = lvar_parser();
    let val = val_parser();
    let expr_group = recursive(|expr_group| {
        let expr = recursive(|expr| {
            let parenexpr = expr
                .clone()
                .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd))
                .labelled("paren_expr");
            let let_e = just(Token::Let)
                .ignore_then(lvar.clone())
                .then_ignore(just(Token::Assign))
                .then(expr.clone())
                .then_ignore(just(Token::LineBreak).or(just(Token::SemiColon)).repeated())
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
                .then(just(Token::Arrow).ignore_then(type_parser()).or_not())
                .then(expr_group.clone())
                .map(|((ids, r_type), body)| Expr::Lambda(ids, r_type, Box::new(body)))
                .labelled("lambda");

            let macro_expand = select! { Token::MacroExpand(s) => Expr::Var(s,None) }
                .map_with_span(|e, s| WithMeta(e, s))
                .then_ignore(just(Token::ParenBegin))
                .then(expr_group.clone())
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
                .map_with_span(|e, s| WithMeta(e, s))
                .or(parenexpr)
                .boxed()
                .labelled("atoms");

            let items = expr
                .clone()
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect::<Vec<_>>();

            let parenitems = items
                .clone()
                .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd))
                .map_with_span(|e, s| WithMeta(e, s))
                .repeated();
            let folder = |f: WithMeta<Expr>, args: WithMeta<Vec<WithMeta<Expr>>>| {
                WithMeta(
                    Expr::Apply(Box::new(f.clone()), args.0),
                    f.1.start..args.1.end,
                )
            };
            let apply = atom.then(parenitems).foldl(folder).labelled("apply");

            let op_cls = |x: WithMeta<_>, y: WithMeta<_>, op: Op, opspan: Span| {
                WithMeta(
                    Expr::Apply(
                        Box::new(WithMeta(
                            Expr::Var(op.get_associated_fn_name().to_string(), None),
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
                .then(op.then(apply).repeated())
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
        });
        // expr_group contains let statement, assignment statement, function definiton,... they cannot be placed as an argument for apply directly.

        let block = expr_group
            .clone()
            .padded_by(just(Token::LineBreak).or_not())
            .delimited_by(just(Token::BlockBegin), just(Token::BlockEnd))
            .map(|e: WithMeta<Expr>| Expr::Block(Some(Box::new(e))));

        //todo: should be recursive(to paranthes be not needed)
        let if_ = just(Token::If)
            .ignore_then(
                expr_group
                    .clone()
                    .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd)),
            )
            .then(expr_group.clone())
            .then(
                just(Token::Else)
                    .ignore_then(expr_group.clone().map(|e| Box::new(e)))
                    .or_not(),
            )
            .map_with_span(|((cond, then), opt_else), s| {
                WithMeta(Expr::If(cond.into(), then.into(), opt_else), s)
            })
            .labelled("if");

        block
            .map_with_span(|e, s| WithMeta(e, s))
            .or(if_)
            .or(expr.clone())
    });
    expr_group
}
fn comment_parser() -> impl Parser<Token, (), Error = Simple<Token>> + Clone {
    select! {Token::Comment(Comment::SingleLine(_))=>(),
    Token::Comment(Comment::MultiLine(_))=>()}
}
fn func_parser() -> impl Parser<Token, WithMeta<Expr>, Error = Simple<Token>> + Clone {
    let expr = expr_parser();
    let lvar = lvar_parser();
    let blockstart = just(Token::BlockBegin)
        .then_ignore(just(Token::LineBreak).or(just(Token::SemiColon)).repeated());
    let blockend = just(Token::LineBreak)
        .or(just(Token::SemiColon))
        .repeated()
        .ignore_then(just(Token::BlockEnd));
    let fnparams = lvar
        .clone()
        .map_with_span(|e, s| WithMeta(e, s))
        .separated_by(just(Token::Comma))
        .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd))
        .labelled("fnparams");

    let stmt = recursive(|stmt| {
        let function_s = just(Token::Function)
            .ignore_then(lvar.clone())
            .then(fnparams.clone())
            .then(just(Token::Arrow).ignore_then(type_parser()).or_not())
            .then(
                expr.clone()
                    .delimited_by(blockstart.clone(), blockend.clone()),
            )
            .then_ignore(just(Token::LineBreak).or(just(Token::SemiColon)).repeated())
            .then(stmt.clone().map(|e| Box::new(e)).or_not())
            .map_with_span(|((((fname, ids), r_type), block), then), s| {
                let atypes = ids
                    .iter()
                    .map(|WithMeta(TypedId { ty, id: _ }, _)| ty.clone().unwrap_or(Type::Unknown))
                    .collect::<Vec<_>>();
                let fname = TypedId {
                    ty: Some(Type::Function(
                        atypes,
                        Box::new(r_type.clone().unwrap_or(Type::Unknown)),
                        None,
                    )),
                    id: fname.id.clone(),
                };
                WithMeta(
                    Expr::LetRec(
                        fname,
                        Box::new(WithMeta(
                            Expr::Lambda(ids, r_type, Box::new(block)),
                            s.clone(),
                        )),
                        then,
                    ),
                    s,
                )
            })
            .labelled("function decl");
        let macro_s = just(Token::Macro)
            .ignore_then(lvar.clone())
            .then(fnparams.clone())
            .then(
                expr.clone()
                    .delimited_by(blockstart.clone(), blockend.clone())
                    .map(|WithMeta(e, s)| {
                        WithMeta(Expr::Bracket(Box::new(WithMeta(e, s.clone()))), s)
                    }),
            )
            .then(expr.clone().map(|e| Box::new(e)).or_not())
            .map_with_span(|(((fname, ids), block), then), s| {
                WithMeta(
                    Expr::LetRec(
                        fname,
                        Box::new(WithMeta(
                            Expr::Lambda(ids, None, Box::new(block)),
                            s.clone(),
                        )),
                        then,
                    ),
                    s,
                )
            })
            .labelled("macro definition");
        let let_stmt = just(Token::Let)
            .ignore_then(lvar.clone())
            .then_ignore(just(Token::Assign))
            .then(expr.clone())
            .then_ignore(just(Token::LineBreak).or(just(Token::SemiColon)).repeated())
            .then(stmt.clone().map(|e| Box::new(e)).or_not())
            .map_with_span(|((ident, body), then), span| {
                WithMeta(Expr::Let(ident, Box::new(body), then), span)
            })
            .boxed()
            .labelled("let_stmt");
        function_s.or(macro_s).or(let_stmt).or(expr_parser())
    });
    stmt
    // expr_parser().then_ignore(end())
}

fn parser() -> impl Parser<Token, WithMeta<Expr>, Error = Simple<Token>> + Clone {
    func_parser()
        .padded_by(comment_parser().repeated().ignored())
        .then_ignore(end())
}
pub(crate) fn add_global_context(ast: WithMeta<Expr>) -> WithMeta<Expr> {
    let WithMeta(_, ref span) = ast;
    let res = Expr::Let(
        TypedId {
            ty: None,
            id: GLOBAL_LABEL.to_string(),
        },
        Box::new(WithMeta(
            Expr::Lambda(vec![], None, Box::new(ast.clone())),
            span.clone(),
        )),
        None,
    );
    WithMeta(res, span.clone())
}
pub fn parse(src: &str) -> Result<WithMeta<Expr>, Vec<Box<dyn ReportableError>>> {
    let len = src.chars().count();
    let mut errs = Vec::<Box<dyn ReportableError>>::new();

    let (tokens, lex_errs) = lexer::lexer().parse_recovery(src);
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
