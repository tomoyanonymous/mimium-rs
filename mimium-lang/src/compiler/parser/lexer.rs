use super::token::*;
use crate::utils::metadata::*;
use chumsky::prelude::*;
use chumsky::Parser;

fn comment_parser() -> impl Parser<char, Comment, Error = Simple<char>> + Clone {
    // comment parser that keep its contents length, not to break line number for debugging.
    // replaces all characters except for newline.
    let single_line = (just("//"))
        .ignore_then(take_until(text::newline().ignored()))
        .map(|(c, _)| Comment::SingleLine(String::from_iter(c.iter())));

    let multi_line = just("/*")
        .ignore_then(take_until(just("*/").ignored()))
        .map(|(c, _)| Comment::MultiLine(String::from_iter(c.iter())));

    single_line.or(multi_line)
}

pub fn lexer() -> impl Parser<char, Vec<(Token, Span)>, Error = Simple<char>> {
    // A parser for numbers
    let int = text::int(10).map(|s: String| Token::Int(s.parse().unwrap()));

    let float = text::int(10)
        .then(just('.'))
        .then(text::digits(10).or_not())
        .map(|((s, _dot), opt_n)| Token::Float(format!("{}.{}", s, opt_n.unwrap_or_default())));

    // A parser for strings
    let str_ = just('"')
        .ignore_then(filter(|c| *c != '"').repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .map(Token::Str);

    // A parser for operators
    let op = one_of("+-*/!=&|%><^")
        .repeated()
        .at_least(1)
        .collect::<String>()
        .map(|s: String| match s.as_str() {
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
            "=" => Token::Assign,
            "%" => Token::Op(Op::Modulo),
            "^" => Token::Op(Op::Exponent),
            "&&" => Token::Op(Op::And),
            "||" => Token::Op(Op::Or),
            "|>" => Token::Op(Op::Pipe),
            _ => Token::Op(Op::Unknown(s)),
        });
    let separator = one_of(",.:;").map(|c| match c {
        ',' => Token::Comma,
        '.' => Token::Dot,
        ':' => Token::Colon,
        ';' => Token::SemiColon,
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
        "else" => Token::Else,
        // "true" => Token::Bool(true),
        // "false" => Token::Bool(false),
        // "null" => Token::Null,
        "float" => Token::FloatType,
        "int" => Token::IntegerType,
        "string" => Token::StringType,
        "structt" => Token::StructType,
        _ => Token::Ident(ident),
    });
    let macro_expand = text::ident()
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
    let linebreak = text::newline()
        .map(|_| '\n')
        // .or(just::<_, _, Simple<char>>(';'))
        .repeated()
        .at_least(1)
        .map(|_s| Token::LineBreak);
    // A single token can be one of the above
    let token = comment_parser()
        .map(Token::Comment)
        .or(float)
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

    token
        .map_with_span(|tok, span| (tok, span))
        // .padded_by(comment_parser().repeated())
        .padded_by(just(' ').or(just('\t').or(just('\u{0020}'))).or_not())
        .repeated()
        .then_ignore(end())
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_let() {
        let src = "let hoge = 36\nfuga";
        let (res, _errs) = lexer().parse_recovery(src);
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
    #[test]
    fn comment() {
        let src = "test
//comment start
conrains src
/*multiline comment
here */
another line
";
        let ans = vec![
            (Token::Ident("test".into()), 0..4),
            (Token::LineBreak, 4..5),
            (
                Token::Comment(Comment::SingleLine("comment start".into())),
                5..21,
            ),
            (Token::Ident("conrains".into()), 21..29),
            (Token::Ident("src".into()), 30..33),
            (Token::LineBreak, 33..34),
            (
                Token::Comment(Comment::MultiLine("multiline comment\nhere ".into())),
                34..61,
            ),
            (Token::LineBreak, 61..62),
            (Token::Ident("another".into()), 62..69),
            (Token::Ident("line".into()), 70..74),
            (Token::LineBreak, 74..75),
        ];
        let (res, errs) = lexer().parse_recovery(src);
        assert!(errs.is_empty());
        assert!(res.is_some());
        assert_eq!(ans, res.unwrap());
        assert_eq!(src.len(), 75);
    }
}
