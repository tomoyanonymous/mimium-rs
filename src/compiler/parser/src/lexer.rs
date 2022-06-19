use ast::metadata::*;
use chumsky::prelude::*;
use token::*;

pub fn lexer() -> impl Parser<char, Vec<(Token, Span)>, Error = Simple<char>> {
    // A parser for numbers
    let int = text::int(10).map(|s: String| Token::Int(s.parse().unwrap()));

    let float = text::int(10)
        .chain::<char, _, _>(just('.').chain(text::digits(10)).or_not().flatten())
        .collect::<String>()
        .map(|s| Token::Float(s.parse().unwrap()));

    // A parser for strings
    let str_ = just('"')
        .ignore_then(filter(|c| *c != '"').repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .map(Token::Str);

    // A parser for operators
    let op = one_of("+-*/!=&|%")
        .repeated()
        .at_least(1)
        .collect::<String>()
        .map(|s: String| {
            Token::Op(match s.as_str() {
                "+" => Op::Sum,
                "-" => Op::Minus,
                "*" => Op::Product,
                "/" => Op::Divide,
                "==" => Op::Equal,
                "!=" => Op::NotEqual,
                "<" => Op::LessThan,
                "<=" => Op::LessEqual,
                ">" => Op::GreaterThan,
                ">=" => Op::GreaterEqual,
                "%" => Op::Modulo,
                "^" => Op::Exponent,
                "&&" => Op::And,
                "||" => Op::Or,
                "|>" => Op::Pipe,
                _ => Op::Unknown(s),
            })
        });

    // A parser for identifiers and keywords
    let ident = text::ident().map(|ident: String| match ident.as_str() {
        "fn" => Token::Function,
        "->" => Token::Arrow,
        "self" => Token::SelfLit,
        "now" => Token::Now,
        "@" => Token::At,

        "let" => Token::Let,
        "if" => Token::If,
        "then" => Token::Then,
        "else" => Token::Else,
        // "true" => Token::Bool(true),
        // "false" => Token::Bool(false),
        // "null" => Token::Null,
        _ => Token::Ident(ident),
    });
    let parens = one_of::<_, _, Simple<char>>("(){}[]")
        .repeated()
        .at_least(1)
        .collect::<String>()
        .map(|s: String| match s.as_str() {
            "|" => Token::LambdaArgBeginEnd,
            "(" => Token::ParenBegin,
            ")" => Token::ParenEnd,
            "{{" => Token::BlockBegin,
            "}}" => Token::BlockEnd,
            "[" => Token::ArrayBegin,
            "]" => Token::ArrayEnd,
            _ => Token::Ident(s),
        });
    let linebreak = text::newline().repeated().at_least(1).to(Token::LineBreak);
    // A single token can be one of the above
    let token = int
        .or(float)
        .or(str_)
        // .or(ctrl)
        .or(parens)
        .or(ident)
        .or(op)
        .or(linebreak)
        .recover_with(skip_then_retry_until([]));

    let comment = just("//").then(take_until(just('\n'))).padded();

    token
        .map_with_span(|tok, span| (tok, span))
        .padded_by(comment.repeated())
        .padded()
        .repeated()
}
