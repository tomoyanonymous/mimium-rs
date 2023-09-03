use crate::types::*;
use crate::{function, numeric};
use std::collections::HashMap;

#[derive(Clone)]
pub struct BuiltinFn(fn(f64) -> f64, Type);

#[derive(Clone)]
pub struct Context {
    fns: HashMap<String, BuiltinFn>,
}
#[macro_export]
macro_rules! ftoftype {
    () => {
        function!(vec![numeric!()], numeric!())
    };
}
#[macro_export]
macro_rules! float1 {
    ($name:ident) => {
        (
            stringify!(name),
            BuiltinFn(move |x: f64| x.$name(), ftoftype!()),
        )
    };
}

impl Context {
    pub fn new() -> Self {
        let v = vec![float1!(sin), float1!(cos)];
        let mut ctx = Context {
            fns: HashMap::<String, BuiltinFn>::new(),
        };
        v.iter().for_each(|(name, f)| {
            ctx.fns.insert(name.to_string(), f.clone());
        });
        ctx
    }

}

pub fn eval_float1(name: &str, x: f64) -> Option<f64> {
    match name {
        "sin" => Some(x.sin()),
        "cos" => Some(x.cos()),
        "not" => Some(if x == 0.0 { 1.0 } else { 0.0 }),
        "round" => Some(x.round()),
        "floor" => Some(x.floor()),
        "ceil" => Some(x.ceil()),
        "atan" => Some(x.atan()),
        "sqrt" => Some(x.sqrt()),
        "abs" => Some(x.abs()),
        _ => None,
    }
}
pub fn eval_float2(name: &str, x: f64, y: f64) -> Option<f64> {
    match name {
        "add" => Some(x + y),
        "sub" => Some(x - y),
        "mult" => Some(x * y),
        "div" => Some(x / y),
        "mod" => Some(x % y),
        "eq" => Some(if x == y { 1. } else { 0. }),
        "ne" => Some(if x != y { 1. } else { 0. }),
        "le" => Some(if x <= y { 1. } else { 0. }),
        "lt" => Some(if x < y { 1. } else { 0. }),
        "ge" => Some(if x >= y { 1. } else { 0. }),
        "gt" => Some(if x > y { 1. } else { 0. }),
        "atan2" => Some(x.atan2(y)),
        _ => None,
    }
}