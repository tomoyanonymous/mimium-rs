use crate::ast::*;
use crate::types::{Type, TypedId};
use crate::utils::error::ReportableError;
use crate::utils::metadata::*;
use chumsky::prelude::*;
use chumsky::Parser;
mod token;
use token::{Op, Token};
mod error;
mod lexer;

#[cfg(test)]
mod test;

fn type_parser() -> impl Parser<Token, Type, Error = Simple<Token>> + Clone {
    recursive(|ty| {
        let primitive = select! {
           Token::FloatType => Type::Numeric,
           Token::IntegerType => Type::Int,
           Token::StringType => Type::String
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
        let func = ty
            .clone()
            .separated_by(just(Token::Comma))
            .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd))
            .then_ignore(just(Token::Arrow))
            .then(ty.clone())
            .map(|(from, to)| Type::Function(from, Box::new(to), None))
            .boxed()
            .labelled("function");

        // .map_with_span(|e, s| WithMeta(e, s))

        primitive.or(tuple).or(func).boxed()
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
                .map(|WithMeta(e, _s)| e)
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
            let if_ = just(Token::If)
                .ignore_then(
                    expr.clone()
                        .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd)),
                )
                .then(expr.clone())
                .then(
                    just(Token::Else)
                        .ignore_then(expr.clone().map(|e| Box::new(e)))
                        .or_not(),
                )
                .map(|((cond, then), opt_else)| Expr::If(cond.into(), then.into(), opt_else))
                .labelled("if");

            let atom = val
                .or(lambda)
                .or(macro_expand)
                .or(let_e)
                .or(if_)
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
        });
        // expr_group contains let statement, assignment statement, function definiton,... they cannot be placed as an argument for apply directly.
        let block = expr
            .clone()
            .padded_by(just(Token::LineBreak).or_not())
            .delimited_by(just(Token::BlockBegin), just(Token::BlockEnd))
            .map(|e: WithMeta<Expr>| Expr::Block(Some(Box::new(e))));

        //todo:add bracket to return type

        block.map_with_span(|e, s| WithMeta(e, s)).or(expr.clone())
    });
    expr_group
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
    let blockstart = just(Token::BlockBegin)
        .then_ignore(just(Token::LineBreak).or(just(Token::SemiColon)).repeated());
    let blockend = just(Token::LineBreak)
        .or(just(Token::SemiColon))
        .repeated()
        .ignore_then(just(Token::BlockEnd));
    let function_s = just(Token::Function)
        .ignore_then(lvar.clone())
        .then(fnparams.clone())
        .then(
            expr.clone()
                .delimited_by(blockstart.clone(), blockend.clone()),
        )
        .then_ignore(just(Token::LineBreak).or(just(Token::SemiColon)).repeated())
        .then(expr.clone().map(|e| Box::new(e)).or_not())
        .map_with_span(|(((fname, ids), block), then), s| {
            WithMeta(
                Expr::LetRec(
                    fname,
                    Box::new(WithMeta(Expr::Lambda(ids, Box::new(block)), s.clone())),
                    then,
                ),
                s,
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
        .map_with_span(|(((fname, ids), block), then), s| {
            WithMeta(
                Expr::LetRec(
                    fname,
                    Box::new(WithMeta(Expr::Lambda(ids, Box::new(block)), s.clone())),
                    then,
                ),
                s,
            )
        })
        .labelled("macro definition");

    function_s.or(macro_s).or(expr_parser()).then_ignore(end())
    // expr_parser().then_ignore(end())
}

fn parser() -> impl Parser<Token, WithMeta<Expr>, Error = Simple<Token>> + Clone {
    func_parser()
}

pub fn parse(src: &String) -> Result<WithMeta<Expr>, Vec<Box<dyn ReportableError>>> {
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
