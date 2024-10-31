use crate::compiler::ExtFunTypeInfo;
use crate::interner::{Symbol, ToSymbol, TypeNodeId};
use crate::types::{PType, Type};
use crate::{function, numeric};

use super::{ExtFnInfo, Machine, ReturnCode};

fn probef(machine: &mut Machine) -> ReturnCode {
    let rv = machine.get_stack(0);
    let i = super::Machine::get_as::<f64>(rv);
    print!("{i}");
    machine.set_stack(0, rv);
    1
}

fn probelnf(machine: &mut Machine) -> ReturnCode {
    let rv = machine.get_stack(0);
    let i = super::Machine::get_as::<f64>(rv);
    println!("{} ", i);
    machine.set_stack(0, rv);
    1
}
fn min(machine: &mut Machine) -> ReturnCode {
    let lhs = machine.get_stack(0);
    let rhs = machine.get_stack(1);

    machine.set_stack(0, super::Machine::to_value(lhs.min(rhs)));
    1
}
fn max(machine: &mut Machine) -> ReturnCode {
    let lhs = machine.get_stack(0);
    let rhs = machine.get_stack(1);

    machine.set_stack(0, super::Machine::to_value(lhs.max(rhs)));
    1
}

pub fn get_builtin_fns() -> [ExtFnInfo; 4] {
    [
        (
            "probe".to_symbol(),
            probef,
            function!(vec![numeric!()], numeric!()),
        ),
        (
            "probeln".to_symbol(),
            probelnf,
            function!(vec![numeric!()], numeric!()),
        ),
        (
            "min".to_symbol(),
            min,
            function!(vec![numeric!(), numeric!()], numeric!()),
        ),
        (
            "max".to_symbol(),
            max,
            function!(vec![numeric!(), numeric!()], numeric!()),
        ),
    ]
}

pub fn get_builtin_fn_types() -> Vec<ExtFunTypeInfo> {
    get_builtin_fns()
        .iter()
        .map(|(name, _f, t)| ExtFunTypeInfo {
            name: *name,
            ty: *t,
        })
        .collect()
}
