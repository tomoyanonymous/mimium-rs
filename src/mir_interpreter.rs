use crate::mir;

struct MirIntepreter {
    stack: Vec<u64>,
    base: u64,
    upvalues: Vec<mir::Value>,
}

impl MirIntepreter {
    pub fn execute(&mut self, func: &mir::Function) -> usize {
        let mut pc = 0;
        // let instructions = func.body;
        loop {
            // match  instructions[pc]{
        }
        pc += 1;
    }

    pub fn exec_block(&mut self, block: &mir::Block) {}

    pub fn call_fun(&mut self, nargs: u64) {}
}
