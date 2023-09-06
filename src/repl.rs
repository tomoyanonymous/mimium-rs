use crate::{
    ast_interpreter, compiler, eval_top, utils::environment::Environment, utils::error,
    utils::miniprint::MiniPrint,
};
use std::{
    io::{stdin, stdout, Write},
    path::PathBuf,
};

pub enum ReplMode {
    Eval,
    ShowAST,
}
pub struct ReplAppData {
    line_count: u64,
    global_env: Environment<ast_interpreter::Value>,
    mode: ReplMode,
}
impl ReplAppData {
    pub fn new() -> Self {
        Self {
            line_count: 0,
            global_env: Environment::new(),
            mode: ReplMode::Eval,
        }
    }
}

fn process_command(mode_str: &str) -> Option<ReplMode> {
    match mode_str {
        ":e" => {
            println!("Mode:Eval");
            Some(ReplMode::Eval)
        }
        ":a" => {
            println!("Mode:AST");
            Some(ReplMode::ShowAST)
        }
        _ => None,
    }
}

fn repl(data: &mut ReplAppData) -> ! {
    loop {
        print!("> ");
        let _ = stdout().flush();
        let mut src = String::new();
        let _size = stdin().read_line(&mut src).expect("stdin read error.");
        if let Some(mode) = process_command(&src[0..2]) {
            data.mode = mode;
        } else {
            let _ = src.pop(); //remove last linebreak
            match data.mode {
                ReplMode::Eval => match eval_top(src.clone(), &mut data.global_env) {
                    Ok(v) => {
                        println!("{:?}", v);
                    }
                    Err(e) => error::report(&src, PathBuf::new(), &e),
                },
                ReplMode::ShowAST => match compiler::emit_ast(&src) {
                    Ok(ast) => {
                        println!("{}", ast.0.pretty_print());
                    }
                    Err(e) => error::report(&src, PathBuf::new(), &e),
                },
            }

            data.line_count += 1;
        }
    }
}

pub fn run_repl() {
    repl(&mut ReplAppData::new())
}
