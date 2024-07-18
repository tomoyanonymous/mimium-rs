extern crate mimium_rs;
use std::path::PathBuf;

use mimium_rs::{
    compiler,
    runtime::{self, vm},
    utils::{
        error::{report, ReportableError},
        fileloader,
    },
};

fn run_multiple(src: &str, times: u64) -> Result<Vec<f64>, Vec<Box<dyn ReportableError>>> {
    let bytecode = compiler::emit_bytecode(src)?;
    let mut machine = vm::Machine::new();
    let mut res: Vec<f64> = vec![];
    let mut ret;
    machine.link_functions(&bytecode);
    for _i in 0..times {
        ret = runtime::run_bytecode_test(&mut machine, &bytecode)?;
        res.push(ret);
    }
    Ok(res)
}

fn run_multiple_file(path: &str, times: u64) -> Result<Vec<f64>, ()> {
    let file: PathBuf = [env!("CARGO_MANIFEST_DIR"), "tests/mmm", path]
        .iter()
        .collect();
    println!("{}", file.to_str().unwrap());
    let (src, _path) = fileloader::load(file.to_string_lossy().to_string()).unwrap();
    let res = run_multiple(&src, times);
    match res {
        Ok(res) => Ok(res),
        Err(errs) => {
            report(&src, file, &errs);
            Err(())
        }
    }
}

#[test]
fn adder() {
    let res = run_multiple_file("adder.mmm", 3).unwrap();
    let ans = vec![1.0, 1.0, 1.0];
    assert_eq!(res, ans);
}

#[test]
fn counter() {
    let res = run_multiple_file("counter.mmm", 10).unwrap();
    let ans = vec![0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0];
    assert_eq!(res, ans);
}
#[test]
fn statefn() {
    let res = run_multiple_file("statefn.mmm", 10).unwrap();
    let ans = vec![0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0];
    assert_eq!(res, ans);
}
#[test]
fn statefn2_same() {
    let res = run_multiple_file("statefn2_same.mmm", 3).unwrap();
    let ans = vec![0.0, 6.0, 12.0];
    assert_eq!(res, ans);
}

#[test]
fn statefn2() {
    let res = run_multiple_file("statefn2.mmm", 3).unwrap();
    let ans = vec![0.0, 8.0, 16.0];
    assert_eq!(res, ans);
}

#[test]
fn loopcounter() {
    let res = run_multiple_file("loopcounter.mmm", 10).unwrap();
    let ans = vec![0.0, 1.0, 2.0, 3.0, 4.0, 0.0, 1.0, 2.0, 3.0, 4.0];
    assert_eq!(res, ans);
}

#[test]
fn sinewave() {
    let res = run_multiple_file("sinwave.mmm", 10).unwrap();
    let ans = vec![
        0.0,
        0.021408545756451732,
        0.04281382071922544,
        0.06421255470540224,
        0.08560147875345994,
        0.10697732573366744,
        0.12833683095811457,
        0.1496767327902555,
        0.17099377325384513,
        0.19228469864114656,
    ];
    assert_eq!(res, ans);
}

#[test]
fn ifblock() {
    let res = run_multiple_file("if.mmm", 1).unwrap();
    let ans = vec![4120.0];
    assert_eq!(res, ans);
}

#[test]
fn letmulti() {
    let res = run_multiple_file("let_multi.mmm", 1).unwrap();
    let ans = vec![3.0];
    assert_eq!(res, ans);
}

#[test]
fn closure_open() {
    let res = run_multiple_file("closure_open.mmm", 1).unwrap();
    let ans = vec![4.0];
    assert_eq!(res, ans);
}

#[test]
fn closure_open_3nested() {
    let res = run_multiple_file("closure_open_3nested.mmm", 1).unwrap();
    let ans = vec![2.0];
    assert_eq!(res, ans);
}