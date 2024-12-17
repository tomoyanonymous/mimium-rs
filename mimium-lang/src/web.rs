extern crate wee_alloc;

#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

use wasm_bindgen::prelude::*;

#[wasm_bindgen]
struct ExecContext(super::ExecContext);

#[wasm_bindgen]
impl ExecContext {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        Self(super::ExecContext::new([].into_iter(), None))
    }
    #[wasm_bindgen]
    pub fn compile(&mut self, src: String) -> i64 {
        let res = self.0.prepare_machine(&src);
        if res.is_err() {
            -1
        } else {
            self.0.run_main()
        }
    }
    #[wasm_bindgen]
    pub fn process(&mut self, input: &[f64], out: &mut [f64]) {
        self.0.vm.as_mut().unwrap().execute_idx(0);
    }
}
