use mmmtype::*;
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

type UniqueId = (String,i64);

#[derive(Clone, Debug, PartialEq, Hash)]
pub enum Expr {
    Literal(Literal),
    Var(Rc<WithMeta<Self>>, Option<Time>),
    Block(Option<Rc<WithMeta<Self>>>),
    Tuple(Vec<WithMeta<Self>>),
    Proj(Rc<WithMeta<Self>>, i64),
    Apply(Rc<WithMeta<Self>>, Rc<WithMeta<Self>>),
    Function(Vec<WithMeta<TypedId>>, Rc<WithMeta<Self>>), //lambda
    Feed(Id, Rc<WithMeta<Self>>),                         //feedback connection primitive
    Let(TypedId, Rc<WithMeta<Self>>, Option<Rc<WithMeta<Self>>>),
    LetRec(TypedId, Rc<WithMeta<Self>>, Option<Rc<WithMeta<Self>>>),
    LetTuple(
        Vec<TypedId>,
        Rc<WithMeta<Self>>,
        Option<Rc<WithMeta<Self>>>,
    ),
    If(
        Rc<WithMeta<Self>>,
        Rc<WithMeta<Self>>,
        Option<Rc<WithMeta<Self>>>,
    ),
    Error,
}
