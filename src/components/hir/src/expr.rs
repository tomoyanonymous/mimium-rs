use mmmtype::*;
use utils::metadata::WithMeta;

pub type Time = i64;

// High-Level Intermediate Representation is mostly W-calculus based format without multi-stage computation factors.
// Some mimium-specific literal like "now" and "self" is now removed

#[derive(Clone, Debug, PartialEq, Hash)]
pub enum Literal {
    String(String),
    Int(i64),
    Float(String),
}

type UniqueId = (String,i64);

#[derive(Clone, Debug, PartialEq, Hash)]
pub enum Expr {
    Literal(Literal),
    Var(Id, Option<Time>),
    Block(Option<Box<WithMeta<Self>>>),
    Tuple(Vec<WithMeta<Self>>),
    Proj(Box<WithMeta<Self>>, i64),
    Apply(Box<WithMeta<Self>>, Box<WithMeta<Self>>),
    Function(Vec<WithMeta<TypedId>>, Box<WithMeta<Self>>), //lambda
    Feed(Id, Box<WithMeta<Self>>),                         //feedback connection primitive
    Let(TypedId, Box<WithMeta<Self>>, Option<Box<WithMeta<Self>>>),
    LetRec(TypedId, Box<WithMeta<Self>>, Option<Box<WithMeta<Self>>>),
    LetTuple(
        Vec<TypedId>,
        Box<WithMeta<Self>>,
        Option<Box<WithMeta<Self>>>,
    ),
    If(
        Box<WithMeta<Self>>,
        Box<WithMeta<Self>>,
        Option<Box<WithMeta<Self>>>,
    ),
    Error,
}
