pub mod parser;
pub mod recursecheck;
pub mod selfconvert;
pub mod typing;
// pub mod hirgen;
pub mod mirgen;
pub mod bytecodegen;


#[derive(Debug, Clone)]
pub enum ErrorKind {
    TypeMismatch(Type, Type),
    CircularType,
    IndexOutOfRange(u16, u16),
    IndexForNonTuple(Type),
    VariableNotFound(String),
    NonPrimitiveInFeed,
    NotApplicable, //need?
}
#[derive(Debug, Clone)]
pub struct Error(pub ErrorKind, pub Span);

impl std::fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorKind::VariableNotFound(_) => {
                write!(f, "Variable Not Found.")
            }
            ErrorKind::TypeMismatch(expect, actual) => {
                write!(
                    f,
                    "Type Mismatch, expected {}, but the actual was {}.",
                    expect.to_string(),
                    actual.to_string()
                )
            }
            ErrorKind::IndexForNonTuple(t) => {
                write!(f, "Index access for non tuple-type {}.", t.to_string())
            }
            ErrorKind::IndexOutOfRange(r, a) => {
                write!(
                    f,
                    "Tuple index out of range, number of elements are {} but accessed with {}.",
                    r, a
                )
            }
            ErrorKind::NotApplicable => {
                write!(f, "Application to non-function type value.")
            }
            ErrorKind::CircularType => write!(f, "Circular loop of type definition"),
            ErrorKind::NonPrimitiveInFeed => write!(f, "Feed can take only non-funtion type."),
        }
    }
}
impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl std::error::Error for Error {}

impl ReportableError for Error {
    fn get_span(&self) -> std::ops::Range<usize> {
        self.1.clone()
    }
}

use crate::{
    ast, ast_interpreter, mir::Mir, runtime::vm, types::Type, utils::{
        error::ReportableError,
        metadata::{Span, WithMeta},
    }
};
pub fn emit_ast(src: &String) -> Result<WithMeta<ast::Expr>, Vec<Box<dyn ReportableError>>> {
    let rawast = parser::parse(src)?;
    let res1 = recursecheck::convert_recurse(&rawast);
    selfconvert::convert_self_top(res1).map_err(|e| {
        let bres = Box::new(e) as Box<dyn ReportableError>;
        vec![bres]
    })
}

pub fn emit_mir(src: &String)->Result<Mir,Vec<Box<dyn ReportableError>>>{
    let ast = parser::parse(src)?;
    mirgen::compile(ast).map_err(|e| {
        let bres = e as Box<dyn ReportableError>;
        vec![bres]
    })
}
pub fn emit_bytecode(src:&String)->Result<vm::Program,Vec<Box<dyn ReportableError>>>{
    let mir  = emit_mir(src)?;  
    bytecodegen::gen_bytecode(mir)
}

pub fn eval_top(
    content: String,
    global_ctx:&mut ast_interpreter::Context
) -> Result<ast_interpreter::Value, Vec<Box<dyn ReportableError>>> {
    let ast = emit_ast(&content)?;
    ast_interpreter::eval_ast(&Box::new(ast), global_ctx).map_err(|e| {
        let eb: Box<dyn ReportableError> = Box::new(e);
        vec![eb]
    })
}

