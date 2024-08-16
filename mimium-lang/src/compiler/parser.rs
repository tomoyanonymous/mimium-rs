use crate::ast::*;
use crate::types::{PType, Type, TypedId};
use crate::utils::error::ReportableError;
use crate::utils::metadata::*;
use chumsky::combinator::Or;
use chumsky::prelude::*;
use chumsky::primitive::Just;
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
            .map(Type::Tuple)
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

fn lvar_parser() -> impl Parser<Token, TypedId, Error = Simple<Token>> + Clone {
    select! { Token::Ident(s) => s }
        .then(just(Token::Colon).ignore_then(type_parser()).or_not())
        .map(|(id, ty)| TypedId { id, ty })
        .labelled("lvar")
}

fn comment_parser() -> impl Parser<Token, (), Error = Simple<Token>> + Clone {
    select! {Token::Comment(Comment::SingleLine(_))=>(),
    Token::Comment(Comment::MultiLine(_))=>()}
}

#[allow(clippy::type_complexity)]
fn eol() -> Or<Just<Token, Token, Simple<Token>>, Just<Token, Token, Simple<Token>>> {
    just(Token::LineBreak).or(just(Token::SemiColon))
}

fn stmt_parser() -> impl Parser<Token, WithMeta<Expr>, Error = Simple<Token>> + Clone {
    recursive(|stmt| {
        // Note: this needs to be declare() first instead of recursive()
        // directly because otherwise the dependency cannot be described.
        let mut expr = Recursive::<Token, WithMeta<Expr>, Simple<Token>>::declare();
        let mut non_block_expr = Recursive::<Token, WithMeta<Expr>, Simple<Token>>::declare();
        let mut block_expr = Recursive::<Token, WithMeta<Expr>, Simple<Token>>::declare();

        expr.define(non_block_expr.clone().or(block_expr.clone()));
        non_block_expr.define(non_block_expr_parser(expr.clone()));
        block_expr.define(block_expr_parser(expr.clone()));

        let expr_stmt = expr_stmt_parser(expr.clone(), stmt.clone());
        let let_stmt = let_stmt_parser(expr.clone(), stmt.clone());
        let func_decl = func_decl_parser(block_expr.clone(), stmt.clone());
        let macro_decl = macro_decl_parser(block_expr.clone(), expr.clone());

        expr_stmt.or(let_stmt).or(func_decl).or(macro_decl)
    })
}

fn op_parser<'a, P>(
    expr: P,
    ops: Vec<Op>,
) -> impl Parser<Token, WithMeta<Expr>, Error = Simple<Token>> + Clone + 'a
where
    P: Parser<Token, WithMeta<Expr>, Error = Simple<Token>> + Clone + 'a,
{
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

    let lhs = expr.clone();
    let rhs = expr;

    let ops: Vec<_> = ops.into_iter().map(optoken).collect();

    lhs.then(choice(ops).then(rhs).repeated())
        .foldl(move |x, ((op, opspan), y)| op_cls(x, y, op, opspan))
        .boxed()
}

fn non_block_expr_parser<'a>(
    expr: impl Parser<Token, WithMeta<Expr>, Error = Simple<Token>> + Clone + 'a,
) -> impl Parser<Token, WithMeta<Expr>, Error = Simple<Token>> + Clone + 'a {
    let atom = literal_expr_parser()
        .or(symbol_expr_parser())
        .or(grouped_expr_parser(expr.clone()))
        .or(tuple_expr_parser(expr.clone()))
        .or(lambda_expr_parser(expr.clone()))
        .or(macro_expand_expr_parser(expr.clone()))
        .boxed()
        .labelled("atom");

    // call expression

    let parenitem = atom
        .clone()
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd))
        .map_with_span(WithMeta);

    let folder = |f: WithMeta<Expr>, args: WithMeta<Vec<WithMeta<Expr>>>| {
        WithMeta(
            Expr::Apply(Box::new(f.clone()), args.0),
            f.1.start..args.1.end,
        )
    };
    let apply = atom
        // Note: repeated() means including 0, so this contains non-apply expression
        .then(parenitem.repeated())
        .foldl(folder)
        .labelled("apply");

    // unary operator expression

    let unary = select! { Token::Op(Op::Minus) => {} }
        .repeated()
        .then(apply)
        .foldr(|_op, rhs| {
            // TODO: ideally, span should include the minus, but it
            // seems there's no information about how many spaces
            // between the minus and the value.
            let span = rhs.1.clone();

            let neg_op = Box::new(WithMeta(Expr::Var("neg".to_string(), None), span.clone()));
            WithMeta(Expr::Apply(neg_op, vec![rhs]), span.clone())
        })
        .labelled("unary");

    // binary operator expression

    let exponent = op_parser(unary, vec![Op::Exponent]);
    let product = op_parser(exponent, vec![Op::Product, Op::Divide, Op::Modulo]);
    let add = op_parser(product, vec![Op::Sum, Op::Minus]);
    let cmp_eq = op_parser(add, vec![Op::Equal, Op::NotEqual]);
    let cnd_and = op_parser(cmp_eq, vec![Op::And]);
    let cnd_or = op_parser(cnd_and, vec![Op::Or]);
    let cmp = op_parser(
        cnd_or,
        vec![
            Op::LessThan,
            Op::LessEqual,
            Op::GreaterThan,
            Op::GreaterEqual,
        ],
    );

    let pipe_op = just(Token::Op(Op::Pipe)).map_with_span(|e, s| {
        (
            match e {
                Token::Op(o) => o,
                _ => Op::Unknown(String::from("invalid")),
            },
            s,
        )
    });
    cmp.clone()
        .then(pipe_op.then(cmp).repeated())
        .foldl(|lhs, ((_, _), rhs)| {
            let span = lhs.1.start..rhs.1.end;
            WithMeta(Expr::Apply(Box::new(rhs), vec![lhs]), span)
        })
        .boxed()
}

fn block_expr_parser<'a>(
    expr: impl Parser<Token, WithMeta<Expr>, Error = Simple<Token>> + Clone + 'a,
    // stmt: impl Parser<Token, WithMeta<Expr>, Error = Simple<Token>> + Clone + 'a,
) -> impl Parser<Token, WithMeta<Expr>, Error = Simple<Token>> + Clone + 'a {
    let blockstart = just(Token::BlockBegin).then_ignore(eol().repeated());
    let blockend = eol().repeated().ignore_then(just(Token::BlockEnd));

    // TODO?: Semantically, this should be { stmt* expr eol? } | { expr eol? } | {}
    //        But, using expr instead of stmt makes this simple and just works.
    let bracketed_block = expr
        .clone()
        .separated_by(eol())
        .delimited_by(blockstart, blockend)
        .map_with_span(|e, s| {
            match e.as_slice() {
                [] => WithMeta(Expr::Block(None), s),
                [e] => WithMeta(Expr::Block(Some(Box::new(e.clone()))), s),
                // fold into a chain of ExprStmt
                [head @ .., last] => head.iter().rfold(last.clone(), |acc, x| {
                    WithMeta(
                        Expr::ExprStmt(Box::new(x.clone()), Some(Box::new(acc))),
                        x.1.clone(),
                    )
                }),
            }
        })
        .labelled("bracket");

    let if_block = just(Token::If)
        .ignore_then(
            expr.clone()
                .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd)),
        )
        .then(bracketed_block.clone())
        .then(
            just(Token::Else)
                .ignore_then(bracketed_block.clone().map(Box::new))
                .or_not(),
        )
        .map_with_span(|((cond, then), opt_else), s| {
            WithMeta(Expr::If(cond.into(), then.into(), opt_else), s)
        })
        .labelled("if");

    bracketed_block.or(if_block)
}

fn literal_expr_parser() -> impl Parser<Token, WithMeta<Expr>, Error = Simple<Token>> + Clone {
    select! {
        Token::Int(x) => Expr::Literal(Literal::Int(x)),
        Token::Float(x) =>Expr::Literal(Literal::Float(x.parse().unwrap())),
        Token::Str(s) => Expr::Literal(Literal::String(s)),
    }
    .map_with_span(WithMeta)
    .labelled("literal")
}

fn symbol_expr_parser() -> impl Parser<Token, WithMeta<Expr>, Error = Simple<Token>> + Clone {
    select! {
        Token::Ident(s) => Expr::Var(s,None),
        Token::SelfLit => Expr::Literal(Literal::SelfLit),
        Token::Now => Expr::Literal(Literal::Now),
    }
    .map_with_span(WithMeta)
    .labelled("expr")
}

fn grouped_expr_parser<'a>(
    expr: impl Parser<Token, WithMeta<Expr>, Error = Simple<Token>> + Clone + 'a,
) -> impl Parser<Token, WithMeta<Expr>, Error = Simple<Token>> + Clone + 'a {
    expr.delimited_by(just(Token::ParenBegin), just(Token::ParenEnd))
        .labelled("paren_expr")
}

fn tuple_expr_parser<'a>(
    expr: impl Parser<Token, WithMeta<Expr>, Error = Simple<Token>> + Clone + 'a,
) -> impl Parser<Token, WithMeta<Expr>, Error = Simple<Token>> + Clone + 'a {
    expr.separated_by(just(Token::Comma))
        .allow_trailing()
        .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd))
        .map_with_span(|e, s| WithMeta(Expr::Tuple(e), s))
        .labelled("tuple")
}

fn lambda_expr_parser<'a>(
    expr: impl Parser<Token, WithMeta<Expr>, Error = Simple<Token>> + Clone + 'a,
) -> impl Parser<Token, WithMeta<Expr>, Error = Simple<Token>> + Clone + 'a {
    lvar_parser()
        .map_with_span(WithMeta)
        .separated_by(just(Token::Comma))
        .delimited_by(
            just(Token::LambdaArgBeginEnd),
            just(Token::LambdaArgBeginEnd),
        )
        .then(just(Token::Arrow).ignore_then(type_parser()).or_not())
        .then(expr)
        .map_with_span(|((ids, r_type), body), s| {
            WithMeta(Expr::Lambda(ids, r_type, Box::new(body)), s)
        })
        .labelled("lambda")
}

fn macro_expand_expr_parser<'a>(
    expr: impl Parser<Token, WithMeta<Expr>, Error = Simple<Token>> + Clone + 'a,
) -> impl Parser<Token, WithMeta<Expr>, Error = Simple<Token>> + Clone + 'a {
    select! { Token::MacroExpand(s) => Expr::Var(s,None) }
        .map_with_span(WithMeta)
        .then_ignore(just(Token::ParenBegin))
        .then(expr)
        .then_ignore(just(Token::ParenEnd))
        .map_with_span(|(id, then), s| {
            WithMeta(
                Expr::Escape(Box::new(WithMeta(
                    Expr::Apply(Box::new(id), vec![then]),
                    s.clone(),
                ))),
                s,
            )
        })
        .labelled("macroexpand")
}

fn expr_stmt_parser<'a>(
    expr: impl Parser<Token, WithMeta<Expr>, Error = Simple<Token>> + Clone + 'a,
    stmt: impl Parser<Token, WithMeta<Expr>, Error = Simple<Token>> + Clone + 'a,
) -> impl Parser<Token, WithMeta<Expr>, Error = Simple<Token>> + Clone + 'a {
    expr.then_ignore(eol().repeated().at_least(1))
        .then(stmt.map(Box::new).or_not())
        .map_with_span(|(expr_body, then), span| {
            WithMeta(Expr::ExprStmt(Box::new(expr_body), then), span)
        })
        .boxed()
        .labelled("expression statement")
}

fn let_stmt_parser<'a>(
    expr: impl Parser<Token, WithMeta<Expr>, Error = Simple<Token>> + Clone + 'a,
    stmt: impl Parser<Token, WithMeta<Expr>, Error = Simple<Token>> + Clone + 'a,
) -> impl Parser<Token, WithMeta<Expr>, Error = Simple<Token>> + Clone + 'a {
    just(Token::Let)
        .ignore_then(lvar_parser())
        .then_ignore(just(Token::Assign))
        .then(expr)
        .then_ignore(eol().repeated().at_least(1))
        .then(stmt.map(Box::new).or_not())
        .map_with_span(|((ident, body), then), span| {
            WithMeta(Expr::Let(ident, Box::new(body), then), span)
        })
        .boxed()
        .labelled("let_stmt")
}

fn fnparams_parser<'a>(
    lvar: impl Parser<Token, TypedId, Error = Simple<Token>> + Clone + 'a,
) -> impl Parser<Token, Vec<WithMeta<TypedId>>, Error = Simple<Token>> + Clone + 'a {
    lvar.map_with_span(WithMeta)
        .separated_by(just(Token::Comma))
        .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd))
        .labelled("fnparams")
}

fn func_decl_parser<'a>(
    block_expr: impl Parser<Token, WithMeta<Expr>, Error = Simple<Token>> + Clone + 'a,
    stmt: impl Parser<Token, WithMeta<Expr>, Error = Simple<Token>> + Clone + 'a,
) -> impl Parser<Token, WithMeta<Expr>, Error = Simple<Token>> + Clone + 'a {
    let lvar = lvar_parser();
    let fnparams = fnparams_parser(lvar.clone());

    just(Token::Function)
        .ignore_then(lvar)
        .then(fnparams)
        .then(just(Token::Arrow).ignore_then(type_parser()).or_not())
        .then(block_expr.clone())
        .then_ignore(just(Token::LineBreak).or(just(Token::SemiColon)).repeated())
        .then(stmt.clone().map(Box::new).or_not())
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
        .labelled("function decl")
}

fn macro_decl_parser<'a>(
    block_expr: impl Parser<Token, WithMeta<Expr>, Error = Simple<Token>> + Clone + 'a,
    expr: impl Parser<Token, WithMeta<Expr>, Error = Simple<Token>> + Clone + 'a,
) -> impl Parser<Token, WithMeta<Expr>, Error = Simple<Token>> + Clone + 'a {
    let lvar = lvar_parser();
    let fnparams = fnparams_parser(lvar.clone());

    just(Token::Macro)
        .ignore_then(lvar)
        .then(fnparams)
        .then(
            block_expr
                .map(|WithMeta(e, s)| WithMeta(Expr::Bracket(Box::new(WithMeta(e, s.clone()))), s)),
        )
        .then(expr.clone().map(Box::new).or_not())
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
        .labelled("macro definition")
}

fn parser() -> impl Parser<Token, WithMeta<Expr>, Error = Simple<Token>> + Clone {
    let ignored = comment_parser()
        .or(just(Token::LineBreak).ignored())
        .or(just(Token::SemiColon).ignored());
    stmt_parser()
        .padded_by(ignored.repeated())
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

#[cfg(test)]
mod tests {
    use super::*;

    fn print_without_span<O>(tokens: &[(Token, O)]) {
        print!("\nTokens: [");
        for (t, _) in tokens {
            print!("{t:?}, ");
        }
        println!("]");
    }

    #[test]
    fn test_stmt_parser() {
        let cases = [
            // literal
            "1.0\n",
            "1\n",
            // r#""foo""#,
            // symbol
            "foo\n",
            "self\n",
            "now\n",
            // grouped expr
            "(1.0)\n",
            // tuple expr
            "(1.0, )\n",
            // call
            "foo()\n",
            "foo(1.0, 2.0)\n",
            "foo()()()\n",
            // unary and binary op
            "-1\n",
            "1+1\n",
            "1*1+3^6\n",
            // let statement
            "let x = 1\n",
            // multiline
            "1.0\n1.0\n",
            " 1.0\n  1.0\n",
            // block
            "{\n1.0\n}\n",
            // fn
            "fn foo(hoge){\n1\n2\n}\n",
            "fn foo(hoge){\n1\n2}\n",
            "fn test(hoge){
    sin(1.0)
    1.0
}
fn dsp(){
    test(2.0)
}",
            // if-else
            // "if (t) { 1.0 } else { 2.0 }\n",
        ];
        for src in cases {
            let (tokens, _) = lexer::lexer().parse_recovery(src);
            let tokens = tokens.expect("Failed to tokenize");
            print_without_span(&tokens);

            // let stmt = expr_stmt_parser(literal_expr_parser(), literal_expr_parser());
            // let block = block_expr_parser(literal_expr_parser(), stmt);
            // let parser = func_stmt_parser(block, literal_expr_parser());

            let parser = parser();
            let len = src.chars().count();
            let (ast, _) =
                parser.parse_recovery(chumsky::Stream::from_iter(len..len + 1, tokens.into_iter()));
            println!("{:#?}", ast.expect("Failed to parse"));
        }
    }
}
