use crate::types::{PType, Type};
use crate::{function, integer, numeric, unit};

use std::cell::LazyCell;

use super::{ExtFunType, Machine, ReturnCode};
pub type BulitinInfo = (&'static str, ExtFunType, Type);

pub fn probef(machine: &mut Machine) -> ReturnCode {
    let rv = machine.get_stack(0);
    let i = super::Machine::get_as::<f64>(rv);
    print!("{i}");
    machine.set_stack(0, rv);
    1
}

pub fn probelnf(machine: &mut Machine) -> ReturnCode {
    let rv = machine.get_stack(0);
    let i = super::Machine::get_as::<f64>(rv);
    print!("{i}\n");
    machine.set_stack(0, rv);
    1
}

pub const BUILTIN_FNS: LazyCell<[BulitinInfo; 2]> = LazyCell::new(|| {
    [
        ("probe", probef, function!(vec![numeric!()], numeric!())),
        ("probeln", probelnf, function!(vec![numeric!()], numeric!())),
    ]
});
