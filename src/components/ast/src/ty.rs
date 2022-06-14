use crate::{
    metadata::{WithMeta},
};

#[derive(Clone, Debug, PartialEq)]
pub enum Type{
    Int(i64),
    Float(f64),
    String(String),
    Tuple(Vec<WithMeta<Self>>),
    Struct(Vec<(String,WithMeta<Self>)>),
    Function{
        parameters: Vec<WithMeta<Self>>,
        body: Box<WithMeta<Self>>,
    }
}