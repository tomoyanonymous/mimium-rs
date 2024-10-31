use std::cell::LazyCell;

use crate::interner::{Symbol, ToSymbol};

// unary
pub(crate) const NEG: &'static str = "neg";
pub(crate) const TOFLOAT: &'static str = "tofloat";

// binary
pub(crate) const ADD: &'static str = "add";
pub(crate) const SUB: &'static str = "sub";
pub(crate) const MULT: &'static str = "mult";
pub(crate) const DIV: &'static str = "div";
pub(crate) const EQ: &'static str = "eq";
pub(crate) const NE: &'static str = "ne";
pub(crate) const LE: &'static str = "le";
pub(crate) const LT: &'static str = "lt";
pub(crate) const GE: &'static str = "ge";
pub(crate) const GT: &'static str = "gt";
pub(crate) const MODULO: &'static str = "modulo";
pub(crate) const POW: &'static str = "pow";
pub(crate) const AND: &'static str = "and";
pub(crate) const OR: &'static str = "or";

// arithmetics
pub(crate) const SIN: &'static str = "sin";
pub(crate) const COS: &'static str = "cos";
pub(crate) const TAN: &'static str = "tan";
pub(crate) const ATAN: &'static str = "atan";
pub(crate) const ATAN2: &'static str = "atan2";
pub(crate) const SQRT: &'static str = "sqrt";
pub(crate) const ABS: &'static str = "abs";
pub(crate) const LOG: &'static str = "log";
pub(crate) const MIN: &'static str = "min";
pub(crate) const MAX: &'static str = "max";
pub(crate) const CEIL: &'static str = "ceil";
pub(crate) const FLOOR: &'static str = "floor";
pub(crate) const ROUND: &'static str = "round";

// other operations
pub(crate) const DELAY: &'static str = "delay";
pub(crate) const MEM: &'static str = "mem";
const BUILTIN_SYMS_UNSORTED: [&str; 31] = [
    NEG, TOFLOAT, ADD, SUB, MULT, DIV, EQ, NE, LE, LT, GE, GT, MODULO, POW, AND, OR, SIN, COS, TAN,
    ATAN, ATAN2, SQRT, ABS, LOG, MIN, MAX, CEIL, FLOOR, ROUND, DELAY, MEM,
];
thread_local!(pub (crate) static BUILTIN_SYMS: LazyCell<Vec<Symbol>> = LazyCell::new(|| {
    let mut v = BUILTIN_SYMS_UNSORTED
        .iter()
        .map(|s| s.to_symbol())
        .collect::<Vec<_>>();
    v.sort();
    v
}));
