#![feature(box_patterns)]
#![feature(iterator_try_collect)]
#![feature(iter_collect_into)]
#![feature(if_let_guard)]
#![feature(lazy_cell)]
// pub mod wcalculus;
pub(crate) mod pattern;
pub mod ast;
pub mod mir;

pub mod types;
pub mod utils;

pub mod compiler;
pub mod runtime;
pub mod audio;

pub mod ast_interpreter;
pub mod mir_interpreter;

pub mod repl;
