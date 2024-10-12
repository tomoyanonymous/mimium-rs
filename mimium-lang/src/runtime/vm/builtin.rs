use crate::interner::{Symbol, ToSymbol, TypeNodeId};
use crate::runtime::scheduler;
use crate::types::{PType, Type};
use crate::{function, integer, numeric, unit};

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

pub fn get_builtin_fns() -> [ExtFnInfo; 3] {
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
            "_mimium_schedule_at".to_symbol(),
            scheduler::mimium_schedule_at,
            function!(vec![numeric!(), function!(vec![], unit!())], unit!()),
        ),
    ]
}

pub fn get_builtin_fn_types() -> Vec<(Symbol, TypeNodeId)> {
    get_builtin_fns()
        .iter()
        .map(|(name, _f, t)| (*name, *t))
        .collect()
}
