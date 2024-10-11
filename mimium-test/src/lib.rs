extern crate mimium_lang;
use std::{collections::HashMap, path::PathBuf};

use mimium_audiodriver::{
    backends::local_buffer::LocalBufferDriver, driver::Driver, runtime_fn::gen_getnowfn,
};
use mimium_lang::{
    interner::{Symbol, ToSymbol},
    runtime::{
        self,
        vm,
    },
    utils::{
        error::{report, ReportableError},
        fileloader,
    },
    ExecContext,
};

pub fn run_bytecode_test<'a>(
    machine: &'a mut vm::Machine,
    n: usize,
) -> Result<&'a [f64], Vec<Box<dyn ReportableError>>> {
    let retcode = machine.execute_entry(&"dsp".to_symbol());
    if retcode >= 0 {
        Ok(vm::Machine::get_as_array::<f64>(machine.get_top_n(n)))
    } else {
        Err(vec![Box::new(runtime::Error(
            runtime::ErrorKind::Unknown,
            0..0,
        ))])
    }
}

pub fn run_bytecode_test_multiple(
    bytecodes: &vm::Program,
    times: u64,
    stereo: bool,
) -> Result<Vec<f64>, Vec<Box<dyn ReportableError>>> {
    let mut machine = vm::Machine::new(None, bytecodes.clone(), &[], &[]);

    let _retcode = machine.execute_main();
    let n = if stereo { 2 } else { 1 };
    let mut ret = Vec::with_capacity(times as usize * n);
    for i in 0..times {
        let res = run_bytecode_test(&mut machine, n)?;
        ret.extend_from_slice(res);
        println!("time:{}, res: {:?}", i, res)
    }
    Ok(ret)
}

pub fn run_source_with_scheduler(
    src: &str,
    times: u64,
) -> Result<Vec<f64>, Vec<Box<dyn ReportableError>>> {
    let mut driver = LocalBufferDriver::new(times as _);
    let getnowfn = gen_getnowfn(driver.count.clone());
    let mut ctx = ExecContext::new(&[], &[getnowfn], None);
    let mut vm = ctx.prepare_machine(src);

    driver.init(vm, None);
    driver.play();
    Ok(driver.get_generated_samples().to_vec())
}

// if stereo, this returns values in flattened form [L1, R1, L2, R2, ...]
pub fn run_source_test(
    src: &str,
    times: u64,
    stereo: bool,
    path: Option<Symbol>,
) -> Result<Vec<f64>, Vec<Box<dyn ReportableError>>> {
    let ctx = ExecContext::new(&[], &[], path);

    let bytecode = ctx.compiler.emit_bytecode(src)?;
    run_bytecode_test_multiple(&bytecode, times, stereo)
}
pub fn run_file_with_scheduler(path: &str, times: u64) -> Result<Vec<f64>, ()> {
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
pub fn run_file_test(path: &str, times: u64, stereo: bool) -> Result<Vec<f64>, ()> {
    let (file, src) = load_src(path);
    let path_sym = file.to_string_lossy().to_symbol();
    let res = run_source_test(&src, times, stereo, Some(path_sym));
    match res {
        Ok(res) => Ok(res),
        Err(errs) => {
            report(&src, file, &errs);
            Err(())
        }
    }
}

pub fn load_src(path: &str) -> (PathBuf, String) {
    let file: PathBuf = [env!("CARGO_MANIFEST_DIR"), "tests/mmm", path]
        .iter()
        .collect();
    println!("{}", file.to_str().unwrap());
    let (src, _path) = fileloader::load(file.to_string_lossy().to_string()).unwrap();
    (file, src)
}

pub fn run_file_test_mono(path: &str, times: u64) -> Result<Vec<f64>, ()> {
    run_file_test(path, times, false)
}

pub fn run_file_test_stereo(path: &str, times: u64) -> Result<Vec<f64>, ()> {
    run_file_test(path, times, true)
}

pub fn test_state_sizes<T: IntoIterator<Item = (&'static str, u64)>>(path: &str, ans: T) {
    let state_sizes: HashMap<&str, u64> = HashMap::from_iter(ans);
    let (file, src) = load_src(path);
    let ctx = ExecContext::new(&[], &[], Some(file.to_str().unwrap().to_symbol()));
    let bytecode = match ctx.compiler.emit_bytecode(&src) {
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
