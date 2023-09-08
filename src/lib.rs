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

use utils::environment::Environment;
use utils::error::ReportableError;

