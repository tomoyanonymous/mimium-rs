use mmmtype::*;
use std::rc::Rc;
use utils::metadata::WithMeta;
pub type Time = i64;

// High-Level Intermediate Representation is mostly W-calculus based format without multi-stage computation factors.
// Values are bound not by name but shared references of ast-nodes.
// Some mimium-specific literal like "now" and "self" is now removed

#[derive(Clone, Debug, PartialEq, Hash)]
pub enum Literal {
    String(String),
    Int(i64),
    Float(String),
}

// type UniqueId = (String, i64);

#[derive(Clone, Debug, PartialEq, Hash)]
pub struct Value(pub String);

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Var(Rc<WithMeta<Value>>, Option<Time>),
    Block(Option<Box<WithMeta<Self>>>),
    Tuple(Vec<WithMeta<Self>>),
    Proj(Box<WithMeta<Self>>, i64),
    Apply(Box<WithMeta<Self>>, Box<WithMeta<Self>>),
    Lambda(Vec<Rc<WithMeta<Value>>>, Box<WithMeta<Self>>), //lambda
    Feed(Rc<WithMeta<Value>>, Box<WithMeta<Self>>),                         //feedback connection primitive
    Let(
        Rc<WithMeta<Value>>,
        Box<WithMeta<Self>>,
        Option<Box<WithMeta<Self>>>,
    ),
    LetRec(
        Rc<WithMeta<Value>>,
        Box<WithMeta<Self>>,
        Option<Box<WithMeta<Self>>>,
    ),
    LetTuple(
        Vec<Rc<WithMeta<Value>>>,
        Box<WithMeta<Self>>,
        Option<Box<WithMeta<Self>>>,
    ),
    If(
        Box<WithMeta<Self>>,
        Box<WithMeta<Self>>,
        Option<Box<WithMeta<Self>>>,
    ),
    Bracket(Box<WithMeta<Self>>),
    Escape(Box<WithMeta<Self>>),
    Error,
}
