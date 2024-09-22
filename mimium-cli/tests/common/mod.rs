extern crate mimium_lang;
use std::{collections::HashMap, path::PathBuf};

use mimium_audiodriver::{backends::mock::MockDriver, driver::Driver};
use mimium_lang::{
    compiler,
    interner::ToSymbol,
    runtime::{
        self,
        scheduler::{Scheduler, SyncScheduler},
        vm,
    },
    utils::{
        error::{report, ReportableError},
        fileloader,
    },
};

fn run_bytecode_test<'a>(
    machine: &'a mut vm::Machine,
    bytecodes: &'a vm::Program,
    n: usize,
) -> Result<&'a [f64], Vec<Box<dyn ReportableError>>> {
    let retcode = machine.execute_entry(bytecodes, &"dsp".to_symbol());
    if retcode >= 0 {
        Ok(vm::Machine::get_as_array::<f64>(machine.get_top_n(n)))
    } else {
        Err(vec![Box::new(runtime::Error(
            runtime::ErrorKind::Unknown,
            0..0,
        ))])
    }
}

fn run_bytecode_test_multiple(
    bytecodes: &vm::Program,
    times: u64,
    stereo: bool,
) -> Result<Vec<f64>, Vec<Box<dyn ReportableError>>> {
    let mut machine = vm::Machine::new_without_scheduler();

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

fn run_source_with_scheduler(
    src: &str,
    times: u64,
) -> Result<Vec<f64>, Vec<Box<dyn ReportableError>>> {
    let bytecode = compiler::emit_bytecode(src)?;

    let mut driver = MockDriver::new();
    driver.init(bytecode, None);
    Ok(driver.play_times(times as _).to_vec())
}

// if stereo, this returns values in flattened form [L1, R1, L2, R2, ...]
pub(crate) fn run_source_test(
    src: &str,
    times: u64,
    stereo: bool,
) -> Result<Vec<f64>, Vec<Box<dyn ReportableError>>> {
    let bytecode = compiler::emit_bytecode(src)?;
    run_bytecode_test_multiple(&bytecode, times, stereo)
}
pub(crate) fn run_file_with_scheduler(path: &str, times: u64) -> Result<Vec<f64>, ()> {
    let (file, src) = load_src(path);
    let res = run_source_with_scheduler(&src, times);
    match res {
        Ok(res) => Ok(res),
        Err(errs) => {
            report(&src, file, &errs);
            Err(())
        }
    }
}
pub(crate) fn run_file_test(path: &str, times: u64, stereo: bool) -> Result<Vec<f64>, ()> {
    let (file, src) = load_src(path);
    let res = run_source_test(&src, times, stereo);
    match res {
        Ok(res) => Ok(res),
        Err(errs) => {
            report(&src, file, &errs);
            Err(())
        }
    }
}

pub(crate) fn load_src(path: &str) -> (PathBuf, String) {
    let file: PathBuf = [env!("CARGO_MANIFEST_DIR"), "tests/mmm", path]
        .iter()
        .collect();
    println!("{}", file.to_str().unwrap());
    let (src, _path) = fileloader::load(file.to_string_lossy().to_string()).unwrap();
    (file, src)
}

pub(crate) fn run_file_test_mono(path: &str, times: u64) -> Result<Vec<f64>, ()> {
    run_file_test(path, times, false)
}

pub(crate) fn run_file_test_stereo(path: &str, times: u64) -> Result<Vec<f64>, ()> {
    run_file_test(path, times, true)
}

pub(crate) fn test_state_sizes<T: IntoIterator<Item = (&'static str, u64)>>(path: &str, ans: T) {
    let state_sizes: HashMap<&str, u64> = HashMap::from_iter(ans);
    let (file, src) = load_src(path);
    let bytecode = match compiler::emit_bytecode(&src) {
        Ok(res) => res,
        Err(errs) => {
            report(&src, file, &errs);
            panic!("failed to emit bytecode");
        }
    };

    for (sym, proto) in bytecode.global_fn_table {
        let fn_name = sym.as_str();

        if fn_name == "_mimium_global" {
            continue;
        }

        let actual = proto.state_size;
        match state_sizes.get(fn_name) {
            Some(&expected) => {
                assert_eq!(
                    actual, expected,
                    "state size of function `{fn_name}` is wrong"
                );
            }
            None => panic!("no such function: {fn_name}"),
        };
    }
}
