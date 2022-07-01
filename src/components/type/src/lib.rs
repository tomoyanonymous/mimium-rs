use std::{cell::RefCell, fmt, rc::Rc};
use utils::metadata::WithMeta;

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    //basic types
    Unit,
    Int,
    Numeric,
    String,
    //aggregate types
    Array(Box<WithMeta<Self>>),
    Tuple(Vec<WithMeta<Self>>),
    Struct(Vec<(String, WithMeta<Self>)>),
    //Function that has a vector of parameters, return type, and type for internal states.
    Function(
        Vec<WithMeta<Self>>,
        Box<WithMeta<Self>>,
        Option<Box<WithMeta<Self>>>,
    ),
    //(experimental) code-type for multi-stage computation that will be evaluated on the next stage
    Code(Box<WithMeta<Self>>),
    Intermediate(i64),
    Unknown,
}

pub type Id = String;

#[derive(Clone, Debug, PartialEq)]
pub struct TypedId {
    pub ty: Option<Type>,
    pub id: Id,
}

impl Type {
    pub fn apply_fn<F>(&self, closure: F) -> Self
    where
        F: Fn(Self) -> Self,
    {
        let apply_box = |a: &Box<WithMeta<Self>>| -> Box<WithMeta<Self>> {
            Box::new((closure(a.0.clone()), a.1.clone()))
        };
        let apply_vec = |v: &Vec<WithMeta<Self>>| -> Vec<WithMeta<Self>> {
            v.iter()
                .map(|a| (closure(a.0.clone()), a.1.clone()))
                .collect()
        };
        match self {
            Type::Unit => Type::Unit,
            Type::Int => Type::Int,
            Type::Numeric => Type::Numeric,
            Type::String => Type::String,
            Type::Array(a) => Type::Array(apply_box(a)),
            Type::Tuple(v) => Type::Tuple(apply_vec(v)),
            Type::Struct(s) => todo!(),
            Type::Function(p, r, s) => {
                Type::Function(apply_vec(p), apply_box(r), s.as_ref().map(|a| apply_box(a)))
            }
            Type::Code(c) => todo!(),
            Type::Intermediate(id) => Type::Intermediate(*id),
            Type::Unknown => Type::Unknown,
        }
    }
    pub fn fold<F, R>(&self, closure: F) -> R
    where
        F: Fn(Self, Self) -> R,
    {
        todo!()
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Unit => write!(f, "()"),
            Type::Int => write!(f, "int"),
            Type::Numeric => write!(f, "num"),
            Type::String => write!(f, "string"),
            Type::Array(a) => write!(f, "[{}]", a.0),
            Type::Tuple(v) => write!(f, "({:?})", v),
            Type::Struct(s) => write!(f, "{{{:?}}}", s),
            Type::Function(p, r, s) => {
                write!(f, "({:?})->", p)?;
                write!(f, "{:?}[{:?}]", r, s)
            }
            Type::Code(c) => write!(f, "<{:?}>", c),
            Type::Intermediate(id) => {
                write!(f, "intermediate[{}]", id,)
            }
            Type::Unknown => write!(f, "unknown"),
        }
    }
}

// #[cfg(test)]
// mod type_test {
//     use super::*;
// #[test]

// }
