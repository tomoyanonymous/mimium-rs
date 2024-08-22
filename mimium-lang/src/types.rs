use std::fmt;

use crate::{
    ast::Symbol,
    format_vec,
    interner::{with_session_globals, TypeNodeId},
    utils::metadata::Span,
};

/// Basic types that are not boxed.
/// They should be splitted semantically as the type of `feed x.e`cannot take function type.
#[derive(Clone, Debug, PartialEq)]
pub enum PType {
    Unit,
    Int,
    Numeric,
    String,
}
#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Primitive(PType),
    //aggregate types
    Array(TypeNodeId),
    Tuple(Vec<Self>),
    Struct(Vec<(Symbol, Box<Self>)>),
    //Function that has a vector of parameters, return type, and type for internal states.
    Function(Vec<Self>, Box<Self>, Option<Box<Self>>),
    Ref(Box<Self>),
    //(experimental) code-type for multi-stage computation that will be evaluated on the next stage
    Code(Box<Self>),
    Intermediate(i64),
    Unknown,
}

// currently, this refers to the number of registers
pub type TypeSize = u8;

impl Type {
    pub fn is_primitive(&self) -> bool {
        if let Type::Primitive(_) = self {
            true
        } else {
            false
        }
    }
    pub fn is_function(&self) -> bool {
        match self {
            Type::Function(_, _, _) => true,
            _ => false,
        }
    }
    pub fn apply_fn<F>(&self, closure: F) -> Self
    where
        F: Fn(Self) -> Self,
    {
        let apply_box = |a: &Self| -> Box<Self> { Box::new(closure(a.clone())) };
        let apply_scalar =
            |a: TypeNodeId| -> TypeNodeId { closure(a.to_type().clone()).into_id_without_span() };
        let apply_vec =
            |v: &Vec<Self>| -> Vec<Self> { v.iter().map(|a| closure(a.clone())).collect() };
        match self {
            Type::Array(a) => Type::Array(apply_scalar(*a)),
            Type::Tuple(v) => Type::Tuple(apply_vec(v)),
            Type::Struct(_s) => todo!(),
            Type::Function(p, r, s) => {
                Type::Function(apply_vec(p), apply_box(r), s.as_ref().map(|a| apply_box(a)))
            }
            Type::Ref(x) => Type::Ref(apply_box(x)),
            Type::Code(_c) => todo!(),
            Type::Intermediate(id) => Type::Intermediate(*id),
            _ => self.clone(),
        }
    }
    pub fn fold<F, R>(&self, _closure: F) -> R
    where
        F: Fn(Self, Self) -> R,
    {
        todo!()
    }

    pub fn get_as_tuple(&self) -> Option<&[Type]> {
        match self {
            Type::Tuple(types) => Some(&types),
            _ => None,
        }
    }

    fn into_id_inner(self, span: Option<Span>) -> TypeNodeId {
        let span = span.unwrap_or(0..0);
        with_session_globals(|session_globals| session_globals.store_type_with_span(self, span))
    }

    pub fn into_id(self, span: Span) -> TypeNodeId {
        self.into_id_inner(Some(span))
    }

    pub fn into_id_without_span(self) -> TypeNodeId {
        self.into_id_inner(None)
    }
}
impl fmt::Display for PType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PType::Unit => write!(f, "()"),
            PType::Int => write!(f, "int"),
            PType::Numeric => write!(f, "number"),
            PType::String => write!(f, "string"),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Primitive(p) => write!(f, "{p}"),
            Type::Array(a) => write!(f, "[{}]", a.to_type()),
            Type::Tuple(v) => {
                let vf = format_vec!(v, ",");
                write!(f, "({vf})")
            }
            Type::Struct(v) => {
                write!(f, "{v:?}")
            }
            Type::Function(p, r, _s) => {
                let args = format_vec!(p, ",");
                write!(f, "({args})->{r}")
            }
            Type::Ref(x) => write!(f, "&{x}"),

            Type::Code(c) => write!(f, "<{c}>"),
            Type::Intermediate(id) => {
                write!(f, "intermediate[{}]", id,)
            }
            Type::Unknown => write!(f, "unknown"),
        }
    }
}

pub mod builder;

// #[cfg(test)]
// mod type_test {
//     use super::*;
// #[test]

// }
