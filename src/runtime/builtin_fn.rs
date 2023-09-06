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
    let b_to_f = move |b| if b { 1. } else { 0. };
    match name {
        "add" => Some(x + y),
        "sub" => Some(x - y),
        "mult" => Some(x * y),
        "div" => Some(x / y),
        "mod" => Some(x % y),
        "eq" => Some(b_to_f(x == y)),
        "ne" => Some(b_to_f(x != y)),
        "le" => Some(b_to_f(x <= y)),
        "lt" => Some(b_to_f(x < y)),
        "ge" => Some(b_to_f(x >= y)),
        "gt" => Some(b_to_f(x > y)),
        "pow" => Some(x.powf(y)),
        "log" => Some(x.log(y)),
        "atan2" => Some(x.atan2(y)),
        "max" => Some(x.max(y)),
        "min" => Some(x.min(y)),
        _ => None,
    }
}

pub fn eval_int1(name: &str, x: i64) -> Option<i64> {
    match name {
        "not" => Some(if x == 0 { 1 } else { 0 }),
        "abs" => Some(x.abs()),
        _ => None,
    }
}

pub fn eval_int2(name: &str, x: i64, y: i64) -> Option<i64> {
    let b_to_i = move |b| if b { 1 } else { 0 };
    match name {
        "add" => Some(x + y),
        "sub" => Some(x - y),
        "mult" => Some(x * y),
        "div" => Some(x / y),
        "mod" => Some(x % y),
        "eq" => Some(b_to_i(x == y)),
        "ne" => Some(b_to_i(x != y)),
        "le" => Some(b_to_i(x <= y)),
        "lt" => Some(b_to_i(x < y)),
        "ge" => Some(b_to_i(x >= y)),
        "gt" => Some(b_to_i(x > y)),
        "pow" => Some(x.pow(y as u32)),
        "max" => Some(x.max(y)),
        "min" => Some(x.min(y)),
        _ => None,
    }
}
