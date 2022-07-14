use chumsky::prelude::*;
use chumsky::Parser;
use token::*;
use utils::metadata::*;

pub fn lexer() -> impl Parser<char, Vec<(Token, Span)>, Error = Simple<char>> {
    // A parser for numbers
    let int = text::int(10).map(|s: String| Token::Int(s.parse().unwrap()));

    let float = text::int(10)
        .then(just('.'))
        .then(text::digits(10).or_not())
        .map(|((s, c), opt_n)| Token::Float(s + &c.to_string() + &opt_n.unwrap_or("".to_string())));

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
        .map(|s: String| match s.as_str() {
            "=" => Token::Assign,
            "->" => Token::Arrow,
            "|" => Token::LambdaArgBeginEnd,
            "+" => Token::Op(Op::Sum),
            "-" => Token::Op(Op::Minus),
            "*" => Token::Op(Op::Product),
            "/" => Token::Op(Op::Divide),
            "==" => Token::Op(Op::Equal),
            "!=" => Token::Op(Op::NotEqual),
            "<" => Token::Op(Op::LessThan),
            "<=" => Token::Op(Op::LessEqual),
            ">" => Token::Op(Op::GreaterThan),
            ">=" => Token::Op(Op::GreaterEqual),
            "%" => Token::Op(Op::Modulo),
            "^" => Token::Op(Op::Exponent),
            "&&" => Token::Op(Op::And),
            "||" => Token::Op(Op::Or),
            "|>" => Token::Op(Op::Pipe),
            _ => Token::Op(Op::Unknown(s)),
        });
    let separator = one_of::<_, _, Simple<char>>(",.").map(|c| match c {
        ',' => Token::Comma,
        '.' => Token::Dot,
        _ => Token::Ident(c.to_string()),
    });
    // A parser for identifiers and keywords
    let ident = text::ident().map(|ident: String| match ident.as_str() {
        "fn" => Token::Function,
        "macro" => Token::Macro,
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
    let macro_expand = text::ident::<_, Simple<char>>()
        .then_ignore(just('!'))
        .map(|ident: String| Token::MacroExpand(ident));

    let parens = one_of::<_, _, Simple<char>>("(){}[]").map(|c| match c {
        '(' => Token::ParenBegin,
        ')' => Token::ParenEnd,
        '{' => Token::BlockBegin,
        '}' => Token::BlockEnd,
        '[' => Token::ArrayBegin,
        ']' => Token::ArrayEnd,
        _ => Token::Ident(c.to_string()),
    });
    let linebreak = text::newline::<Simple<char>>()
        .repeated()
        .at_least(1)
        .map(|_s| Token::LineBreak);
    // A single token can be one of the above
    let token = float
        .or(int)
        .or(str_)
        // .or(ctrl)
        .or(macro_expand)
        .or(separator)
        .or(ident)
        .or(op)
        .or(parens)
        .or(linebreak)
        .recover_with(skip_then_retry_until([]));

    // let comment = just("//").then(take_until(just('\n'))).padded();

    token
        .map_with_span(|tok, span| (tok, span))
        // .padded_by(comment.repeated())
        .padded_by(just(' ').or(just('\t').or(just('\u{0020}'))).or_not())
        .repeated()
        .then_ignore(end())
}

#[test]
pub fn test_let() {
    let src = "let hoge = 36\nfuga";
    let (res, _errs) = lexer().parse_recovery(src.clone());
    let ans = [
        (Token::Let, 0..3),
        (Token::Ident("hoge".to_string()), 4..8),
        (Token::Assign, 9..10),
        (Token::Int(36), 11..13),
        (Token::LineBreak, 13..14),
        (Token::Ident("fuga".to_string()), 14..18),
    ];
    // dbg!(res.clone());
    if let Some(tok) = res {
        assert_eq!(tok, ans);
    } else {
        println!("{:#?}", _errs);
        panic!()
    }
}
