use crate::ast::ToSymbol;
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

pub fn run_bytecode_test<'a>(
    machine: &'a mut vm::Machine,
    bytecodes: &'a vm::Program,
    n: usize,
) -> Result<&'a [f64], Vec<Box<dyn ReportableError>>> {
    let retcode = machine.execute_entry(bytecodes, &"dsp".to_symbol());
    if retcode >= 0 {
        Ok(vm::Machine::get_as_array::<f64>(machine.get_top_n(n)))
    } else {
        Err(vec![Box::new(Error(ErrorKind::Unknown, 0..0))])
    }
}

pub fn run_bytecode_test_multiple(
    bytecodes: &vm::Program,
    times: u64,
    stereo: bool,
) -> Result<Vec<f64>, Vec<Box<dyn ReportableError>>> {
    let mut machine = vm::Machine::new();
    machine.link_functions(bytecodes);
    let _retcode = machine.execute_entry(bytecodes, &"_mimium_global".to_symbol());
    let n = if stereo { 2 } else { 1 };
    let mut ret = Vec::with_capacity(times as usize * n);
    for i in 0..times {
        let res = run_bytecode_test(&mut machine, bytecodes, n)?;
        ret.extend_from_slice(res);
        println!("time:{}, res: {:?}", i, res)
    }
    Ok(ret)
}

// if stereo, this returns values in flattened form [L1, R1, L2, R2, ...]
pub fn run_source_test(
    src: &str,
    times: u64,
    stereo: bool,
) -> Result<Vec<f64>, Vec<Box<dyn ReportableError>>> {
    let bytecode = compiler::emit_bytecode(src)?;
    run_bytecode_test_multiple(&bytecode, times, stereo)
}
