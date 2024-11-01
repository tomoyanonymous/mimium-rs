use std::path::PathBuf;

use crate::ast::*;
use crate::interner::{ExprNodeId, Symbol, ToSymbol, TypeNodeId};
use crate::pattern::{Pattern, TypedId, TypedPattern};
use crate::types::{PType, Type};
use crate::utils::error::ReportableError;
use crate::utils::metadata::*;
use chumsky::{prelude::*, Parser};
// use chumsky::Parser;
mod token;
use resolve_include::resolve_include;
use token::{Comment, Op, Token};
mod error;
mod lexer;
mod resolve_include;
mod statement;
use statement::{into_then_expr, stmt_from_expr_top, Statement};

use super::intrinsics;

#[cfg(test)]
mod test;

fn type_parser() -> impl Parser<Token, TypeNodeId, Error = Simple<Token>> + Clone {
    recursive(|ty| {
        let primitive = select! {
           Token::FloatType => Type::Primitive(PType::Numeric),
           Token::IntegerType => Type::Primitive(PType::Int),
           Token::StringType => Type::Primitive(PType::String)
        }
        .map_with_span(|t, s| t.into_id_with_span(s));

        let tuple = ty
            .clone()
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd))
            .map_with_span(|t: Vec<TypeNodeId>, s: Span| Type::Tuple(t).into_id_with_span(s))
            .boxed()
            .labelled("Tuple");

        // let _struct_t = todo!();
        let atom = primitive.or(tuple);
        let func = atom
            .clone()
            .separated_by(just(Token::Comma))
            .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd))
            .then(just(Token::Arrow).ignore_then(ty.clone()))
            .map_with_span(|(a, e), s| Type::Function(a, e, None).into_id_with_span(s))
            .boxed()
            .labelled("function");

        func.or(atom).boxed().labelled("Type")
    })
}
fn ident_parser() -> impl Parser<Token, Symbol, Error = Simple<Token>> + Clone {
    select! { Token::Ident(s) => s }.labelled("ident")
}
fn literals_parser() -> impl Parser<Token, ExprNodeId, Error = Simple<Token>> + Clone {
    select! {
        //Currently Integer literals are treated as float until the integer type is introduced in type system.
        // Token::Int(x) => Literal::Int(x),
        Token::Int(x)=>Literal::Float(x.to_string().to_symbol()),
        Token::Float(x) =>Literal::Float(x.to_symbol()),
        Token::Str(s) => Literal::String(s.to_symbol()),
        Token::SelfLit => Literal::SelfLit,
        Token::Now => Literal::Now,
        Token::PlaceHolder => Literal::PlaceHolder,
    }
    .map_with_span(|e, s| Expr::Literal(e).into_id(s))
    .labelled("literal")
}
fn var_parser() -> impl Parser<Token, ExprNodeId, Error = Simple<Token>> + Clone {
    ident_parser().map_with_span(|e, s| Expr::Var(e).into_id(s))
}
fn with_type_annotation<P, O>(
    parser: P,
) -> impl Parser<Token, (O, Option<TypeNodeId>), Error = Simple<Token>> + Clone
where
    P: Parser<Token, O, Error = Simple<Token>> + Clone,
{
    parser
        .then(just(Token::Colon).ignore_then(type_parser()).or_not())
        .map(|(id, t)| (id, t))
}

fn lvar_parser_typed() -> impl Parser<Token, TypedId, Error = Simple<Token>> + Clone {
    with_type_annotation(ident_parser())
        .map_with_span(|(sym, t), span| match t {
            Some(ty) => TypedId { id: sym, ty },
            None => TypedId {
                id: sym,
                ty: Type::Unknown.into_id_with_span(span),
            },
        })
        .labelled("lvar_typed")
}
fn pattern_parser() -> impl Parser<Token, TypedPattern, Error = Simple<Token>> + Clone {
    let pat = recursive(|pat| {
        pat.clone()
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd))
            .map(Pattern::Tuple)
            .or(select! {
                Token::Ident(s) => Pattern::Single(s),
                // Note: _ represents an unused variable, but it is treated as
                // an ordinary symbol here.
                Token::PlaceHolder => Pattern::Single("_".to_symbol()),
            })
            .labelled("Pattern")
    });
    with_type_annotation(pat).map_with_span(|(pat, ty), s| match ty {
        Some(ty) => TypedPattern { pat, ty },
        None => TypedPattern {
            pat,
            ty: Type::Unknown.into_id_with_span(s),
        },
    })
}
fn binop_folder<'a, I, OP>(prec: I, op: OP) -> BoxedParser<'a, Token, ExprNodeId, Simple<Token>>
where
    I: Parser<Token, ExprNodeId, Error = Simple<Token>> + Clone + 'a,
    OP: Parser<Token, (Op, Span), Error = Simple<Token>> + Clone + 'a,
{
    prec.clone()
        .then(
            op.then_ignore(just(Token::LineBreak).or(just(Token::SemiColon)).repeated())
                .then(prec)
                .repeated(),
        )
        .foldl(move |x, ((op, opspan), y)| {
            let apply_span = x.to_span().start..y.to_span().end;
            let arg = match op {
                Op::Pipe => return Expr::PipeApply(x, y).into_id(apply_span),
                // A@B is a syntactic sugar of _mimium_schedule_at(B, A)
                Op::At => vec![y, x],
                _ => vec![x, y],
            };
            Expr::Apply(Expr::Var(op.get_associated_fn_name()).into_id(opspan), arg)
                .into_id(apply_span)
        })
        .boxed()
}

type ExprParser<'a> = Recursive<'a, Token, ExprNodeId, Simple<Token>>;

fn items_parser(
    expr: ExprParser<'_>,
) -> impl Parser<Token, Vec<ExprNodeId>, Error = Simple<Token>> + Clone + '_ {
    expr.separated_by(just(Token::Comma))
        .allow_trailing()
        .collect::<Vec<_>>()
}

fn op_parser<'a, I>(apply: I) -> impl Parser<Token, ExprNodeId, Error = Simple<Token>> + Clone + 'a
where
    I: Parser<Token, ExprNodeId, Error = Simple<Token>> + Clone + 'a,
{
    let unary = select! { Token::Op(Op::Minus) => {} }
        .map_with_span(|e, s| (e, s))
        .repeated()
        .then(apply.clone())
        .foldr(|(_op, op_span), rhs| {
            let rhs_span = rhs.to_span();
            let neg_op = Expr::Var("neg".to_symbol()).into_id(op_span.start..rhs_span.start);
            Expr::Apply(neg_op, vec![rhs]).into_id(op_span.start..rhs_span.end)
        })
        .labelled("unary");

    let optoken = move |o: Op| {
        just(Token::Op(o))
            .try_map(|e, s| match e {
                Token::Op(o) => Ok((o, s)),
                _ => Err(Simple::custom(s, "Invalid operator used")),
            })
            .boxed()
    };
    // allow pipe opertor to absorb linebreaks so that it can be also used at
    // the head of the line.
    let pipe = just(Token::LineBreak)
        .repeated()
        .then(just(Token::Op(Op::Pipe)))
        .map_with_span(|_, s| (Op::Pipe, s))
        .boxed();
    //defining binary operators in order of precedence.
    let ops = [
        optoken(Op::Exponent),
        choice((
            optoken(Op::Product),
            optoken(Op::Divide),
            optoken(Op::Modulo),
        ))
        .boxed(),
        optoken(Op::Sum).or(optoken(Op::Minus)).boxed(),
        optoken(Op::Equal).or(optoken(Op::NotEqual)).boxed(),
        optoken(Op::And),
        optoken(Op::Or),
        choice((
            optoken(Op::LessThan),
            optoken(Op::LessEqual),
            optoken(Op::GreaterThan),
            optoken(Op::GreaterEqual),
        ))
        .boxed(),
        pipe,
        optoken(Op::At),
    ];
    ops.into_iter().fold(unary.boxed(), binop_folder)
}
fn atom_parser<'a>(
    expr: ExprParser<'a>,
    expr_group: ExprParser<'a>,
) -> impl Parser<Token, ExprNodeId, Error = Simple<Token>> + Clone + 'a {
    let lambda = lvar_parser_typed()
        .separated_by(just(Token::Comma))
        .delimited_by(
            just(Token::LambdaArgBeginEnd),
            just(Token::LambdaArgBeginEnd),
        )
        .then(just(Token::Arrow).ignore_then(type_parser()).or_not())
        .then(expr_group.clone())
        .map_with_span(|((ids, r_type), body), span| Expr::Lambda(ids, r_type, body).into_id(span))
        .labelled("lambda");
    let macro_expand = select! { Token::MacroExpand(s) => Expr::Var(s) }
        .map_with_span(|e, s| e.into_id(s))
        .then_ignore(just(Token::ParenBegin))
        .then(expr_group.clone())
        .then_ignore(just(Token::ParenEnd))
        .map_with_span(|(id, then), s| {
            Expr::Escape(Expr::Apply(id, vec![then]).into_id(s.clone())).into_id(s)
        })
        .labelled("macroexpand");

    let tuple = items_parser(expr.clone())
        .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd))
        .map_with_span(|e, s| Expr::Tuple(e).into_id(s))
        .labelled("tuple");
    let parenexpr = expr
        .clone()
        .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd))
        .labelled("paren_expr");
    //tuple must  lower precedence than parenexpr, not to parse single element tuple without trailing comma
    choice((
        literals_parser(),
        var_parser(),
        lambda,
        macro_expand,
        parenexpr,
        tuple,
    ))
}
fn expr_parser(expr_group: ExprParser<'_>) -> ExprParser<'_> {
    recursive(|expr: Recursive<Token, ExprNodeId, Simple<Token>>| {
        enum FoldItem {
            Args(Vec<ExprNodeId>),
            ArrayIndex(ExprNodeId),
        }
        let parenitems = items_parser(expr.clone())
            .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd))
            .map_with_span(|args, args_span| (FoldItem::Args(args), args_span));
        let angle_paren_expr = expr
            .clone()
            .delimited_by(just(Token::ArrayBegin), just(Token::ArrayEnd))
            .map_with_span(|e, s| (FoldItem::ArrayIndex(e), s));

        let folder = |f: ExprNodeId, (item, args_span): (FoldItem, Span)| {
            let f_span = f.to_span();
            let span = f_span.start..args_span.end;
            match item {
                FoldItem::Args(args) => Expr::Apply(f, args).into_id(span),
                FoldItem::ArrayIndex(index) => Expr::ArrayAccess(f, index).into_id(span),
            }
        };

        let apply = atom_parser(expr.clone(), expr_group)
            .then(angle_paren_expr.or(parenitems).repeated())
            .foldl(folder)
            .labelled("apply");

        op_parser(apply)
    })
}
// fn expr_statement_parser<'a>(
//     expr_group: ExprParser<'a>,
//     then: ExprParser<'a>,
// ) -> impl Parser<Token, ExprNodeId, Error = Simple<Token>> + Clone + 'a {
//     let let_stmt = just(Token::Let)
//         .ignore_then(pattern_parser().clone())
//         .then_ignore(just(Token::Assign))
//         .then(expr_group.clone())
//         .then_ignore(just(Token::LineBreak).or(just(Token::SemiColon)).repeated())
//         .then(then.clone().or_not())
//         .map_with_span(|((ident, body), then), span| Expr::Let(ident, body, then).into_id(span))
//         .labelled("let_stmt");
//     let assign = placement_parser()
//         .then_ignore(just(Token::Assign))
//         .then(expr_group.clone())
//         .then_ignore(just(Token::LineBreak).or(just(Token::SemiColon)).repeated())
//         .then(then.or_not())
//         .map_with_span(|((ident, body), then), span| {
//             Expr::Then(Expr::Assign(ident, body).into_id(span.clone()), then).into_id(span)
//         })
//         .labelled("assign");
//     let_stmt.or(assign)
// }
fn validate_reserved_pat(id: &TypedPattern, span: Span) -> Result<(), Simple<Token>> {
    match &id.pat {
        Pattern::Single(symbol) => validate_reserved_ident(*symbol, span),
        _ => Ok(()),
    }
}

fn validate_reserved_ident(id: Symbol, span: Span) -> Result<(), Simple<Token>> {
    if intrinsics::BUILTIN_SYMS.with(|syms| syms.binary_search(&id).is_ok()) {
        Err(Simple::custom(
            span,
            "Builtin functions cannot be re-defined.",
        ))
    } else {
        Ok(())
    }
}

fn statement_parser(
    expr: ExprParser<'_>,
) -> impl Parser<Token, (Statement, Span), Error = Simple<Token>> + Clone + '_ {
    let let_ = just(Token::Let)
        .ignore_then(pattern_parser().clone().validate(|pat, span, emit| {
            if let Err(e) = validate_reserved_pat(&pat, span.clone()) {
                emit(e);
            }
            pat
        }))
        .then_ignore(just(Token::Assign))
        .then(expr.clone())
        .map_with_span(|(ident, body), span| (Statement::Let(ident, body), span))
        .labelled("let");
    let letrec = just(Token::LetRec)
        .ignore_then(lvar_parser_typed().validate(|ident, span, emit| {
            if let Err(e) = validate_reserved_ident(ident.id, span.clone()) {
                emit(e);
            }
            ident
        }))
        .then_ignore(just(Token::Assign))
        .then(expr.clone())
        .map_with_span(|(ident, body), span| (Statement::LetRec(ident, body), span))
        .labelled("letrec");
    let assign = var_parser()
        .then_ignore(just(Token::Assign))
        .then(expr.clone())
        .map_with_span(|(lvar, body), span| (Statement::Assign(lvar, body), span))
        .labelled("assign");
    let single = expr.map_with_span(|e, span| (Statement::Single(e), span));
    let_.or(letrec).or(assign).or(single)
}
fn statements_parser(
    expr: ExprParser<'_>,
) -> impl Parser<Token, Option<ExprNodeId>, Error = Simple<Token>> + Clone + '_ {
    statement_parser(expr)
        .separated_by(just(Token::LineBreak).or(just(Token::SemiColon)).repeated())
        .allow_leading()
        .allow_trailing()
        .map(|stmts| into_then_expr(&stmts))
}

fn block_parser(
    expr: ExprParser<'_>,
) -> impl Parser<Token, ExprNodeId, Error = Simple<Token>> + Clone + '_ {
    let stmts = statements_parser(expr);
    stmts
        .delimited_by(just(Token::BlockBegin), just(Token::BlockEnd))
        .map_with_span(|stmts, span| Expr::Block(stmts).into_id(span))
}
// expr_group contains let statement, assignment statement, function definiton,... they cannot be placed as an argument for apply directly.
fn exprgroup_parser<'a>() -> ExprParser<'a> {
    recursive(|expr_group: ExprParser<'a>| {
        let expr = expr_parser(expr_group.clone());

        let block = block_parser(expr_group.clone());
        //todo: should be recursive(to paranthes be not needed)
        let if_ = just(Token::If)
            .ignore_then(
                expr_group
                    .clone()
                    .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd)),
            )
            .then(expr_group.clone())
            .then(just(Token::Else).ignore_then(expr_group.clone()).or_not())
            .map_with_span(|((cond, then), opt_else), s| Expr::If(cond, then, opt_else).into_id(s))
            .labelled("if");

        block
            .or(if_)
            // .or(expr_statement_parser(expr_group.clone(), expr_group))
            .or(expr.clone())
    })
}
fn comment_parser() -> impl Parser<Token, (), Error = Simple<Token>> + Clone {
    select! {Token::Comment(Comment::SingleLine(_t))=>(),
    Token::Comment(Comment::MultiLine(_t))=>()}
}
fn gen_unknown_function_type(
    ids: &[TypedId],
    r_type: Option<TypeNodeId>,
    span: Span,
) -> TypeNodeId {
    let atypes = ids
        .iter()
        .map(|tid| {
            if !tid.is_unknown() {
                tid.ty
            } else {
                Type::Unknown.into_id_with_span(span.clone())
            }
        })
        .collect::<Vec<_>>();
    Type::Function(
        atypes,
        r_type.unwrap_or_else(|| Type::Unknown.into_id_with_span(span.clone())),
        None,
    )
    .into_id_with_span(span.clone())
}
fn func_parser(
    current_file: Option<PathBuf>,
) -> impl Parser<Token, ExprNodeId, Error = Simple<Token>> + Clone {
    let exprgroup = exprgroup_parser();
    let lvar = lvar_parser_typed();
    let blockstart = just(Token::BlockBegin)
        .then_ignore(just(Token::LineBreak).or(just(Token::SemiColon)).repeated());
    let blockend = just(Token::LineBreak)
        .or(just(Token::SemiColon))
        .repeated()
        .ignore_then(just(Token::BlockEnd));
    let fnparams = lvar
        .clone()
        .separated_by(just(Token::Comma))
        .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd))
        .labelled("fnparams");

    let function_s = just(Token::Function)
        .ignore_then(lvar.clone().validate(|ident, span, emit| {
            if let Err(e) = validate_reserved_ident(ident.id, span) {
                emit(e);
            }
            ident
        }))
        .then(fnparams.clone())
        .then(just(Token::Arrow).ignore_then(type_parser()).or_not())
        .then(block_parser(exprgroup.clone()).map(|e| match e.to_expr() {
            Expr::Block(e) => e.unwrap(),
            _ => e,
        }))
        .map_with_span(|(((fname, ids), r_type), block), s| {
            let fname = TypedId {
                id: fname.id,
                ty: gen_unknown_function_type(&ids, r_type, s.clone()),
            };
            (
                Statement::LetRec(fname, Expr::Lambda(ids, r_type, block).into_id(s.clone())),
                s,
            )
        })
        .labelled("function decl");

    let macro_s = just(Token::Macro)
        .ignore_then(lvar.clone())
        .then(fnparams.clone())
        .then(
            exprgroup
                .clone()
                .delimited_by(blockstart.clone(), blockend.clone())
                .map(Expr::Bracket),
        )
        .map_with_span(|((fname, ids), block), s| {
            (
                Statement::MacroExpand(
                    fname,
                    Expr::Lambda(ids, None, block.into_id(s.clone())).into_id(s.clone()),
                ),
                s,
            )
        })
        .labelled("macro definition");
    let global_stmt = statement_parser(exprgroup.clone());
    let stmt = function_s.or(macro_s).or(global_stmt);
    let stmts = stmt
        .map(|s: (Statement, Span)| vec![s])
        .or(
            preprocess_parser(current_file.unwrap_or_default()).map_with_span(|e, s| {
                stmt_from_expr_top(e)
                    .into_iter()
                    .map(|st| (st, s.clone()))
                    .collect()
            }),
        )
        .separated_by(just(Token::LineBreak).or(just(Token::SemiColon)).repeated())
        .allow_leading()
        .allow_trailing()
        .flatten()
        .map(|stmt| into_then_expr(&stmt));
    stmts.try_map(|e: Option<ExprNodeId>, span| e.ok_or(Simple::custom(span, "empty expressions")))
}
fn preprocess_parser(
    current_file: PathBuf,
) -> impl Parser<Token, ExprNodeId, Error = Simple<Token>> + Clone {
    just(Token::Include)
        .ignore_then(
            select! {Token::Str(s) => s}
                .delimited_by(just(Token::ParenBegin), just(Token::ParenEnd)),
        )
        .try_map(move |filename, span: Span| {
            let cfile = current_file.to_str().unwrap();
            resolve_include(cfile, &filename, span.clone()).map_err(|_e| {
                Simple::<Token>::custom(span, format!("failed to resolve include for {filename}"))
            })
        })
}
fn parser(
    current_file: Option<PathBuf>,
) -> impl Parser<Token, ExprNodeId, Error = Simple<Token>> + Clone {
    let ignored = comment_parser()
        .or(just(Token::LineBreak).ignored())
        .or(just(Token::SemiColon).ignored());
    func_parser(current_file)
        .padded_by(ignored.repeated())
        .then_ignore(end())
}

pub(crate) fn add_global_context(ast: ExprNodeId) -> ExprNodeId {
    let span = ast.to_span();
    let res = Expr::Let(
        TypedPattern {
            pat: Pattern::Single(GLOBAL_LABEL.to_symbol()),
            ty: Type::Unknown.into_id_with_span(span.clone()),
        },
        Expr::Lambda(vec![], None, ast).into_id(span.clone()),
        None,
    );
    res.into_id(span.clone())
}
pub fn parse(
    src: &str,
    current_file: Option<PathBuf>,
) -> Result<ExprNodeId, Vec<Box<dyn ReportableError>>> {
    let len = src.chars().count();
    let mut errs = Vec::<Box<dyn ReportableError>>::new();

    let (tokens, lex_errs) = lexer::lexer().parse_recovery(src);
    lex_errs
        .iter()
        .for_each(|e| errs.push(Box::new(error::ParseError::<char>(e.clone()))));

    if let Some(t) = tokens {
        let (ast, parse_errs) = parser(current_file)
            .parse_recovery(chumsky::Stream::from_iter(len..len + 1, t.into_iter()));
        match ast {
            Some(ast) if parse_errs.is_empty() => Ok(ast),
            _ => {
                parse_errs
                    .iter()
                    .for_each(|e| errs.push(Box::new(error::ParseError::<Token>(e.clone()))));
                Err(errs)
            }
        }
    } else {
        Err(errs)
    }
}
