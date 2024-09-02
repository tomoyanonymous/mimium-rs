use std::sync::{
    atomic::{AtomicU64, Ordering},
    Arc,
};

use mimium_lang::runtime::vm::{ExtClsType, Machine};

pub(crate) fn gen_getnowfn(count: Arc<AtomicU64>) -> ExtClsType {
    Arc::new(move |machine: &mut Machine| {
        let count = count.load(Ordering::Relaxed) as f64;
        machine.set_stack(-1, Machine::to_value(count));
        1
    })
}
