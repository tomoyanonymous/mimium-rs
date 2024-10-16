pub mod parser;
pub mod typing;
// pub mod hirgen;
pub mod bytecodegen;
mod intrinsics;
pub mod mirgen;

#[derive(Debug, Clone)]
pub enum ErrorKind {
    TypeMismatch(Type, Type),
    CircularType,
    IndexOutOfRange(u16, u16),
    IndexForNonTuple(Type),
    VariableNotFound(String),
    NonPrimitiveInFeed,
    NotApplicable, //need?
    Unknown,
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
            ErrorKind::Unknown => write!(f, "unknwon error."),
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

use mirgen::recursecheck;

use crate::{
    ast_interpreter,
    interner::{ExprNodeId, Symbol, TypeNodeId},
    mir::Mir,
    runtime::vm,
    types::Type,
    utils::{error::ReportableError, metadata::Span},
};
pub fn emit_ast(src: &str) -> Result<ExprNodeId, Vec<Box<dyn ReportableError>>> {
    let ast = parser::parse(src).map(|ast| parser::add_global_context(ast))?;
    Ok(recursecheck::convert_recurse(ast))
}

pub struct Context {
    builtin_fns: Vec<(Symbol, TypeNodeId)>,
    file_path: Option<Symbol>,
}
impl Context {
    pub fn new(builtin_fns: &[(Symbol, TypeNodeId)], file_path: Option<Symbol>) -> Self {
        Self {
            builtin_fns: builtin_fns.to_vec(),
            file_path,
        }
    }
    pub fn emit_mir(&self, src: &str) -> Result<Mir, Vec<Box<dyn ReportableError>>> {
        let ast = parser::parse(src).map(|ast| parser::add_global_context(ast))?;
        mirgen::compile(ast, &self.builtin_fns, self.file_path).map_err(|e| {
            let bres = e as Box<dyn ReportableError>;
            vec![bres]
        })
    }
    pub fn emit_bytecode(&self, src: &str) -> Result<vm::Program, Vec<Box<dyn ReportableError>>> {
        let mir = self.emit_mir(src)?;
        bytecodegen::gen_bytecode(mir)
    }
}

pub fn interpret_top(
    content: String,
    global_ctx: &mut ast_interpreter::Context,
) -> Result<ast_interpreter::Value, Vec<Box<dyn ReportableError>>> {
    let ast = emit_ast(&content)?;
    ast_interpreter::eval_ast(ast, global_ctx).map_err(|e| {
        let eb: Box<dyn ReportableError> = Box::new(e);
        vec![eb]
    })
}
