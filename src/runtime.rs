use crate::compiler;
use crate::utils::{error::ReportableError, metadata::Span};

pub mod scheduler;
// pub mod hir_interpreter;
pub mod builtin_fn;

pub mod vm;

#[derive(Debug)]
pub enum ErrorKind {
    Unknown,
}

impl std::fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorKind::Unknown => write!(f, "Unknown Error"),
        }
    }
}
#[derive(Debug)]
pub struct Error(ErrorKind, Span);
impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let _ = write!(f, "Runtime Error: ");
        self.0.fmt(f)
    }
}

impl std::error::Error for Error {}

impl ReportableError for Error {
    fn get_span(&self) -> std::ops::Range<usize> {
        self.1.clone()
    }
}

pub fn run_bytecode_test(
    machine: &mut vm::Machine,
    bytecodes: &vm::Program,
) -> Result<f64, Vec<Box<dyn ReportableError>>> {
    let retcode = machine.execute_main(bytecodes);
    if retcode >= 0 {
        Ok(vm::Machine::get_as::<f64>(*machine.get_top()))
    } else {
        Err(vec![Box::new(Error(ErrorKind::Unknown, 0..0))])
    }
}

pub fn run_bytecode_test_multiple(
    bytecodes: &vm::Program,
    times: u64,
) -> Result<f64, Vec<Box<dyn ReportableError>>> {
    let mut machine = vm::Machine::new();
    let mut ret = 0.0;

    for i in 0..times {
        ret = run_bytecode_test(&mut machine, bytecodes)?;
        println!("time:{}, res: {}", i, ret)
    }
    Ok(ret)
}

pub fn run_source_test(src: &str) -> Result<f64, Vec<Box<dyn ReportableError>>> {
    let bytecode = compiler::emit_bytecode(src)?;
    run_bytecode_test_multiple(&bytecode, 10)
}
