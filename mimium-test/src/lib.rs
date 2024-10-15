extern crate mimium_lang;
use std::{collections::HashMap, path::PathBuf};

use mimium_audiodriver::{backends::local_buffer::LocalBufferDriver, driver::Driver};
use mimium_lang::{
    ast::builder,
    interner::{Symbol, ToSymbol},
    plugin::Plugin,
    runtime::{self, vm},
    utils::{
        error::{report, ReportableError},
        fileloader,
    },
    ExecContext,
};

pub fn run_bytecode_test(
    machine: &mut vm::Machine,
    n: usize,
) -> Result<&[f64], Vec<Box<dyn ReportableError>>> {
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
    bytecodes: vm::Program,
    times: u64,
    stereo: bool,
) -> Result<Vec<f64>, Vec<Box<dyn ReportableError>>> {
    let mut ctx = ExecContext::new([].into_iter(), None);
    let _ = ctx.prepare_machine_with_bytecode(bytecodes);
    let mut machine = ctx.vm.unwrap();
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

pub fn run_source_with_plugins(
    src: &str,
    path: Option<&str>,
    times: u64,
    plugins: impl Iterator<Item = Box<dyn Plugin>>,
    with_scheduler: bool,
) -> Result<Vec<f64>, Vec<Box<dyn ReportableError>>> {
    let mut driver = LocalBufferDriver::new(times as _);
    let mut ctx = ExecContext::new(plugins, path.map(|s| s.to_symbol()));
    if with_scheduler {
        ctx.add_system_plugin(mimium_scheduler::get_default_scheduler_plugin());
    }
    ctx.prepare_machine(src);

    driver.init(ctx, None);
    driver.play();
    Ok(driver.get_generated_samples().to_vec())
}

pub fn run_source_with_scheduler(
    src: &str,
    times: u64,
) -> Result<Vec<f64>, Vec<Box<dyn ReportableError>>> {
    run_source_with_plugins(src, None, times, [].into_iter(), true)
}

// if stereo, this returns values in flattened form [L1, R1, L2, R2, ...]
pub fn run_source_test(
    src: &str,
    times: u64,
    stereo: bool,
    path: Option<Symbol>,
) -> Result<Vec<f64>, Vec<Box<dyn ReportableError>>> {
    let mut ctx = ExecContext::new([].into_iter(), path);

    ctx.prepare_machine(src);
    let bytecode = ctx.vm.unwrap().prog;
    run_bytecode_test_multiple(bytecode, times, stereo)
}

pub fn run_file_with_plugins(
    path: &str,
    times: u64,
    plugins: impl Iterator<Item = Box<dyn Plugin>>,
    with_scheduler: bool,
) -> Result<Vec<f64>, ()> {
    let (file, src) = load_src(path);
    let res = run_source_with_plugins(
        &src,
        Some(&file.to_string_lossy()),
        times,
        plugins,
        with_scheduler,
    );
    match res {
        Ok(res) => Ok(res),
        Err(errs) => {
            report(&src, file, &errs);
            Err(())
        }
    }
}
pub fn run_file_with_scheduler(path: &str, times: u64) -> Result<Vec<f64>, ()> {
    run_file_with_plugins(path, times, [].into_iter(), true)
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
    let crate_root = std::env::var("TEST_ROOT").expect(
        r#"You must set TEST_ROOT environment variable to run test.
You should put the line like below to your build.rs.
fn main() {
    println!("cargo:rustc-env=TEST_ROOT={}", env!("CARGO_MANIFEST_DIR"));
}
"#,
    );
    let file: PathBuf = [crate_root.as_str(), "tests/mmm", path].iter().collect();
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
    let mut ctx = ExecContext::new([].into_iter(), Some(file.to_str().unwrap().to_symbol()));
    ctx.prepare_machine(&src);
    let bytecode =ctx.vm.expect("failed to emit bytecode").prog;
    // let bytecode = match ctx.compiler.emit_bytecode(&src) {
    //     Ok(res) => res,
    //     Err(errs) => {
    //         report(&src, file, &errs);
    //         panic!("failed to emit bytecode");
    //     }
    // };

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
