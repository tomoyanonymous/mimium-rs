use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Comment {
    SingleLine(String),
    MultiLine(String),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Op {
    Sum,     // +
    Minus,   // -
    Product, // *
    Divide,  // /

    Equal,        // ==
    NotEqual,     // !=
    LessThan,     // <
    LessEqual,    // <=
    GreaterThan,  // >
    GreaterEqual, // >=

    Modulo,   // %
    Exponent, // ^

    And, // &&
    Or,  // ||

    Pipe, // |>
    Unknown(String),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    Ident(String),
    MacroExpand(String),

    FloatType,
    IntegerType,
    StringType,
    StructType,

    Float(String),
    Int(i64),
    Str(String),

    Op(Op),
    SelfLit,
    Now,
    At,

    Comma,
    Dot,

    Colon,
    SemiColon,

    Let,
    Assign,

    Reference, //?

    ParenBegin,
    ParenEnd,
    ArrayBegin,
    ArrayEnd,
    BlockBegin,
    BlockEnd,
    LambdaArgBeginEnd,

    Function, //"fn"
    Macro,    //"macro"
    Arrow,    // ->

    If,
    Then,
    Else,

    Return,
    Type,
    Alias,

    LineBreak,

    Comment(Comment),

    EndOfInput,
}
impl Op {
    pub fn get_associated_fn_name(&self) -> String {
        match self {
            Op::Sum => "add".to_string(),
            Op::Minus => "sub".to_string(),
            Op::Product => "mult".to_string(),
            Op::Divide => "div".to_string(),
            Op::Equal => "eq".to_string(),
            Op::NotEqual => "ne".to_string(),
            Op::LessThan => "lt".to_string(),
            Op::LessEqual => "le".to_string(),
            Op::GreaterThan => "gt".to_string(),
            Op::GreaterEqual => "ge".to_string(),
            Op::Modulo => "mod".to_string(),
            Op::Exponent => "exp".to_string(),
            Op::And => "and".to_string(),
            Op::Or => "or".to_string(),
            Op::Pipe => "pipe".to_string(),
            Op::Unknown(x) => x.clone(),
        }
    }
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Op::Sum => write!(f, "+"),
            Op::Minus => write!(f, "-"),
            Op::Product => write!(f, "*"),
            Op::Divide => write!(f, "/"),
            Op::Equal => write!(f, "=="),
            Op::NotEqual => write!(f, "!="),
            Op::LessThan => write!(f, "<"),
            Op::LessEqual => write!(f, "<="),
            Op::GreaterThan => write!(f, ">"),
            Op::GreaterEqual => write!(f, ">="),
            Op::Modulo => write!(f, "%"),
            Op::Exponent => write!(f, "^"),
            Op::And => write!(f, "&&"),
            Op::Or => write!(f, "||"),
            Op::Pipe => write!(f, "|>"),
            Op::Unknown(x) => write!(f, "{}", x),
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Ident(x) => write!(f, "{}", x),
            Token::MacroExpand(x) => write!(f, "{}!", x),
            Token::FloatType => write!(f, "float"),
            Token::IntegerType => write!(f, "int"),
            Token::StringType => write!(f, "string"),
            Token::StructType => write!(f, "struct"),
            Token::Int(x) => write!(f, "{}", x),
            Token::Float(x) => write!(f, "{}", x),
            Token::Str(x) => write!(f, "\"{}\"", x),
            Token::Op(x) => write!(f, "{}", x),
            Token::SelfLit => write!(f, "self"),
            Token::Now => write!(f, "now"),
            Token::At => write!(f, "@"),
            Token::Comma => write!(f, ","),
            Token::Dot => write!(f, "."),
            Token::Colon => write!(f, ":"),
            Token::SemiColon => write!(f, ";"),
            Token::Let => write!(f, "let"),
            Token::Assign => write!(f, "="),
            Token::Reference => write!(f, "&"),
            Token::ParenBegin => write!(f, "("),
            Token::ParenEnd => write!(f, ")"),
            Token::ArrayBegin => write!(f, "["),
            Token::ArrayEnd => write!(f, "]"),
            Token::BlockBegin => write!(f, "{{"),
            Token::BlockEnd => write!(f, "}}"),
            Token::LambdaArgBeginEnd => write!(f, "|"),

            Token::Function => write!(f, "fn"),
            Token::Macro => write!(f, "macro"),
            Token::Arrow => write!(f, "->"),

            Token::If => write!(f, "if"),
            Token::Then => write!(f, "then"),
            Token::Else => write!(f, "else"),

            Token::Return => write!(f, "return"),
            Token::Type => write!(f, "type"),
            Token::Alias => write!(f, "newtype"),
            Token::LineBreak => write!(f, "linebreak"),
            Token::Comment(_) => write!(f, "comment"),
            Token::EndOfInput => write!(f, "endofinput"),
        }
    }
}
