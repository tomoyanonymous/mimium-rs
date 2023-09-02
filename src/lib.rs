#![feature(box_patterns)]
#![feature(iterator_try_collect)]

// pub mod wcalculus;
pub mod ast;
pub mod hir;
pub mod types;
pub mod mir;
pub mod utils;
pub mod compiler;
pub mod runtime;

pub mod ast_interpreter;