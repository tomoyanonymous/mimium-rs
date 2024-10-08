use std::sync::{
    atomic::{AtomicU64, Ordering},
    Arc,
};

use mimium_lang::{
    function, numeric,
    runtime::vm::{ExtClsInfo, Machine},
    types::{PType, Type},
};

pub fn gen_getnowfn(count: Arc<AtomicU64>) -> ExtClsInfo {
    let func = Arc::new(move |machine: &mut Machine| {
        let count = count.load(Ordering::Relaxed) as f64;
        machine.set_stack(-1, Machine::to_value(count));
        1
    });
    ("_mimium_getnow", func, function!(vec![], numeric!()))
}
