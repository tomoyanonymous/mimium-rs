#![feature(box_patterns)]
#![feature(iterator_try_collect)]
#![feature(lazy_cell)]
// pub mod wcalculus;
pub mod ast;
pub mod mir;

pub mod types;
pub mod utils;

pub mod compiler;
pub mod runtime;

pub mod ast_interpreter;
pub mod mir_interpreter;

pub mod repl;
