extern crate mimium_lang;
use std::path::PathBuf;

use mimium_lang::{
    runtime::run_source_test,
    utils::{error::report, fileloader},
};

fn run_file_test(path: &str, times: u64) -> Result<Vec<f64>, ()> {
    let file: PathBuf = [env!("CARGO_MANIFEST_DIR"), "tests/mmm", path]
        .iter()
        .collect();
    println!("{}", file.to_str().unwrap());
    let (src, _path) = fileloader::load(file.to_string_lossy().to_string()).unwrap();
    let res = run_source_test(&src, times);
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
    let res = run_file_test("adder.mmm", 3).unwrap();
    let ans = vec![1.0, 1.0, 1.0];
    assert_eq!(res, ans);
}

#[test]
fn recursion() {
    let res = run_file_test("recursion.mmm", 1).unwrap();
    let ans = vec![5.0];
    assert_eq!(res, ans);
}

#[test]
fn counter() {
    let res = run_file_test("counter.mmm", 10).unwrap();
    let ans = vec![0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0];
    assert_eq!(res, ans);
}
#[test]
fn statefn() {
    let res = run_file_test("statefn.mmm", 10).unwrap();
    let ans = vec![0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0];
    assert_eq!(res, ans);
}
#[test]
fn statefn2_same() {
    let res = run_file_test("statefn2_same.mmm", 3).unwrap();
    let ans = vec![0.0, 6.0, 12.0];
    assert_eq!(res, ans);
}

#[test]
fn statefn2() {
    let res = run_file_test("statefn2.mmm", 3).unwrap();
    let ans = vec![0.0, 8.0, 16.0];
    assert_eq!(res, ans);
}

#[test]
fn loopcounter() {
    let res = run_file_test("loopcounter.mmm", 10).unwrap();
    let ans = vec![0.0, 1.0, 2.0, 3.0, 4.0, 0.0, 1.0, 2.0, 3.0, 4.0];
    assert_eq!(res, ans);
}

#[test]
fn primitive_sin() {
    let res = run_file_test("primitive_sin.mmm", 1).unwrap();
    let ans = vec![0.0];
    let r = (res[0] - ans[0]).abs() < std::f64::EPSILON;
    assert!(r);
}

#[test]
fn sinewave() {
    let res = run_file_test("sinwave.mmm", 10).unwrap();
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
    let res = run_file_test("if.mmm", 1).unwrap();
    let ans = vec![4120.0];
    assert_eq!(res, ans);
}

#[test]
fn letmulti() {
    let res = run_file_test("let_multi.mmm", 1).unwrap();
    let ans = vec![3.0];
    assert_eq!(res, ans);
}

#[test]
fn closure_open() {
    let res = run_file_test("closure_open.mmm", 1).unwrap();
    let ans = vec![4.0];
    assert_eq!(res, ans);
}

#[test]
fn closure_open_3nested() {
    let res = run_file_test("closure_open_3nested.mmm", 1).unwrap();
    let ans = vec![2.0];
    assert_eq!(res, ans);
}
#[test]
fn closure_open_inline() {
    let res = run_file_test("closure_open_inline.mmm", 1).unwrap();
    let ans = vec![2.0];
    assert_eq!(res, ans);
}

#[test]
fn closure_closed() {
    let res = run_file_test("closure_closed.mmm", 1).unwrap();
    let ans = vec![-6.0];
    assert_eq!(res, ans);
}

#[test]
fn closure_argument() {
    let res = run_file_test("closure_argument.mmm", 1).unwrap();
    let ans = vec![24.0];
    assert_eq!(res, ans);
}

#[test]
fn stateful_closure() {
    let res = run_file_test("stateful_closure.mmm", 10).unwrap();
    let ans = vec![
        20.0,
        20.3,
        20.599999999999998,
        20.900000000000002,
        21.2,
        21.5,
        21.8,
        22.099999999999998,
        22.400000000000002,
        22.7,
    ];
    assert_eq!(res, ans);
}

#[test]
fn hof_state() {
    let res = run_file_test("hof_state.mmm", 10).unwrap();
    let ans = vec![
        0.0,
        0.6000000000000001,
        1.2000000000000002,
        1.8000000000000003,
        2.4000000000000004,
        3.0,
        3.5999999999999996,
        4.199999999999999,
        4.8,
        5.3999999999999995,
    ];
    assert_eq!(res, ans);
}
