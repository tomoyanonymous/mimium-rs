use crate::{ast_interpreter, eval_top, utils::error};
use std::{io::stdin, path::PathBuf};
pub struct ReplAppData {
    line_count: u64,
    global_env: Vec<(String, ast_interpreter::Value)>,
}
impl ReplAppData {
    pub fn new() -> Self {
        Self {
            line_count: 0,
            global_env: Vec::new(),
        }
    }
}

fn repl(data: &mut ReplAppData) {
    print!("> ");
    loop {
        let mut src = String::new();
        let _size = stdin().read_line(&mut src).expect("stdin read error.");
        let _ = src.pop();//remove end of input
        match eval_top(src.clone(), &mut data.global_env) {
            Ok(v) => {
                println!("{:?}", v);
            }
            Err(e) => error::report(&src, PathBuf::new(), &e),
        }
        data.line_count += 1;
        print!("> ");
    }
}

pub fn run_repl() {
    repl(&mut ReplAppData::new())
}
