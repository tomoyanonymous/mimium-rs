use std::fmt;

use id_arena::Id;

use crate::{
    ast::{NodeId, Symbol, ToNodeId},
    ast_interpreter::with_session_globals,
    format_vec,
    utils::metadata::Span,
};

use super::utils::miniprint::MiniPrint;

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
    Array(Box<Self>),
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

#[derive(Debug, Clone, Copy, PartialOrd, Ord)]
pub struct TypeNodeId(pub Id<Type>);

impl TypeNodeId {
    pub fn to_type(&self) -> &Type {
        with_session_globals(|session_globals| unsafe {
            std::mem::transmute::<&Type, &Type>(session_globals.get_type(*self))
        })
    }

    pub fn to_span(&self) -> &Span {
        with_session_globals(|session_globals| unsafe {
            std::mem::transmute::<&Span, &Span>(session_globals.get_span(*self))
        })
    }
}

// ExprNodeId needs to implement Ord in order to be a key of BTreeMap. To implement
// Ord, Rust requires PartialEq, Eq, and PartialOrd. PartialEq needs to be
// implemented manually so that the actual expressions and spans are compared.

impl PartialEq for TypeNodeId {
    fn eq(&self, other: &Self) -> bool {
        self.to_type() == other.to_type() && self.to_span() == self.to_span()
    }
}

impl Eq for TypeNodeId {}

impl ToNodeId for TypeNodeId {
    fn to_node_id(&self) -> NodeId {
        NodeId::TypeArena(self.0.index())
    }
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
        let apply_vec =
            |v: &Vec<Self>| -> Vec<Self> { v.iter().map(|a| closure(a.clone())).collect() };
        match self {
            Type::Array(a) => Type::Array(apply_box(a)),
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
            Type::Array(a) => write!(f, "[{a}]"),
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
