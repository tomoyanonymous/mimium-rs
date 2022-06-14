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
    SelfLit(()),
    Now(()),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Literal(WithMeta<Literal>),
    Var {
        id: WithMeta<Id>,
        t: Time,
    }, // variable with history
    Block {
        statements: Vec<(WithMeta<Id>, WithMeta<Self>)>,
        ret: Option<Box<WithMeta<Self>>>,
    },
    Tuple(Vec<WithMeta<Self>>),
    Proj(Box<WithMeta<Self>>, i64),
    Apply {
        function: Box<WithMeta<Self>>,
        callee: Box<WithMeta<Self>>,
    },
    Function {
        parameters: Vec<WithMeta<Id>>,
        body: Box<WithMeta<Self>>,
    },
    Let {
        id: TypedId,
        body: Box<WithMeta<Self>>,
    },
    LetTuple {
        ids: Vec<TypedId>,
        body: Box<WithMeta<Self>>,
    },
    If {
        condition: Box<WithMeta<Self>>,
        then_expr: Box<WithMeta<Self>>,
        else_expr: Option<Box<WithMeta<Self>>>,
    },
}
