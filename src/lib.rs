#![feature(box_patterns)]
#![feature(iterator_try_collect)]

// pub mod wcalculus;
pub mod ast;
pub mod compiler;
pub mod hir;
pub mod mir;
pub mod runtime;
pub mod types;
pub mod utils;

pub mod ast_interpreter;
pub mod repl;

use compiler::parser;
use utils::environment::Environment;
use utils::error::ReportableError;

pub fn eval_top(
    content: String,
    global_env: &mut Environment<ast_interpreter::Value>,
) -> Result<ast_interpreter::Value, Vec<Box<dyn ReportableError>>> {
    let ast = parser::parse(content)?;
    ast_interpreter::eval_ast(ast.into(), global_env).map_err(|e| {
        let eb: Box<dyn ReportableError> = Box::new(e);
        vec![eb]
    })
}
