pub mod parser;
pub mod recursecheck;
pub mod selfconvert;

use crate::{
    ast,
    utils::{error::ReportableError, metadata::WithMeta},
};
pub fn emit_ast(src: &String) -> Result<WithMeta<ast::Expr>, Vec<Box<dyn ReportableError>>> {
    let rawast = parser::parse(src)?;
    let res1 = recursecheck::convert_recurse(&rawast);
    selfconvert::convert_self_top(res1).map_err(|e| {
        let bres = Box::new(e) as Box<dyn ReportableError>;
        vec![bres]
    })
}

pub mod typing;

// pub mod hirgen;
pub mod mirgen;
