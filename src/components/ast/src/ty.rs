use crate::metadata::WithMeta;
use std::fmt;

#[derive(Clone, Debug, PartialEq, Hash)]
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
    //intermediate type for type inference
    Intermediate(i64),
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
                write!(f, "{:?}{:?}", r, s)
            }
            Type::Code(c) => write!(f, "<{:?}>", c),
            Type::Intermediate(id) => write!(f, "intermediate({})", id),
        }
    }
}
