#[derive(Clone, Debug, PartialEq, Eq, Hash)]

pub enum Binop {
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
}

pub enum Token {
    Comment(String),
    Ident(String),
    Int(i64),
    Float(f64),
    Str(String),

    Binop(Binop),
    SelfLit,
    Now,
    At,

    Comma,
    Dot,
    Apply,
    Let,
    Assign,

    Reference, //?

    ParenBegin,
    ParenEnd,
    ArrayBegin,
    ArrayEnd,
    BlockBegin,
    BlockEnd,
    LambdaArgBegin,
    LambdaArgEnd,

    Function,
    Arrow, // ->

    If,
    Then,
    Else,

    Return,
    Type,
    Alias,
}
