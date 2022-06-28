use crate::{metadata::WithMeta, ty::Type};

pub type Id = String;

#[derive(Clone, Debug, PartialEq)]
pub struct TypedId {
    pub ty: Option<Type>,
    pub id: Id,
}
pub type Time = i64;

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    String(String),
    Int(i64),
    Float(f64),
    SelfLit(),
    Now(),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Var(Id, Option<Time>),
    Block(Vec<WithMeta<Self>>, Option<Box<WithMeta<Self>>>),
    Tuple(Vec<WithMeta<Self>>),
    Proj(Box<WithMeta<Self>>, i64),
    Apply(Box<WithMeta<Self>>, Box<WithMeta<Self>>),
    Function(Vec<WithMeta<TypedId>>, Box<WithMeta<Self>>),//lambda
    Let(TypedId, Box<WithMeta<Self>>, Box<WithMeta<Self>>),
    LetRec(TypedId, Box<WithMeta<Self>>),
    LetTuple(Vec<TypedId>, Box<WithMeta<Self>>, Box<WithMeta<Self>>),
    If(
        Box<WithMeta<Self>>,
        Box<WithMeta<Self>>,
        Option<Box<WithMeta<Self>>>,
    ),
    //exprimental macro system using multi-stage computation
    Bracket(Box<WithMeta<Self>>),
    Escape(Box<WithMeta<Self>>),

}
