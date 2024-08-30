extern crate mimium_lang;
use std::path::{Path, PathBuf};

use mimium_lang::{
    runtime::run_source_test,
    utils::{error::report, fileloader},
};

fn run_simple_test(expr: &str, expect: f64, times: u64) {
    let src = format!(
        "fn test(hoge){{
    {expr}
}}
fn dsp(){{
    test(2.0)
}}"
    );
    let res = run_source_test(&src, times, false);
    match res {
        Ok(res) => {
            let ans = [expect].repeat(times as usize);
            assert_eq!(res, ans, "expr: {expr}");
        }
        Err(errs) => {
            report(&src, Path::new("(from template)"), &errs);
            panic!("invalid syntax");
        }
    }
}

#[test]
fn simple_arithmetic() {
    // unary
    run_simple_test("1.0", 1.0, 3);
    run_simple_test("-1.0", -1.0, 3);
    run_simple_test("- -1.0", 1.0, 3);
    run_simple_test("-hoge", -2.0, 3);
    run_simple_test("-(-hoge)", 2.0, 3);
    run_simple_test("-cos(0.0)", -1.0, 3);

    // binary
    run_simple_test("hoge+1.0", 3.0, 3);
    run_simple_test("hoge-1.0", 1.0, 3);
    run_simple_test("hoge*3.0", 6.0, 3);
    run_simple_test("hoge/2.0", 1.0, 3);
    run_simple_test("hoge^3.0", 8.0, 3);

    // complex expression to test the evaluation order
    run_simple_test("hoge*10.0+hoge/10.0+1.0", 21.2, 3);
    run_simple_test("1.0+hoge^2.0*1.5", 7.0, 3);
}

fn run_file_test(path: &str, times: u64, stereo: bool) -> Result<Vec<f64>, ()> {
    let file: PathBuf = [env!("CARGO_MANIFEST_DIR"), "tests/mmm", path]
        .iter()
        .collect();
    println!("{}", file.to_str().unwrap());
    let (src, _path) = fileloader::load(file.to_string_lossy().to_string()).unwrap();
    let res = run_source_test(&src, times, stereo);
    match res {
        Ok(res) => Ok(res),
        Err(errs) => {
            report(&src, file, &errs);
            Err(())
        }
    }
}

fn run_file_test_mono(path: &str, times: u64) -> Result<Vec<f64>, ()> {
    run_file_test(path, times, false)
}

fn run_file_test_stereo(path: &str, times: u64) -> Result<Vec<f64>, ()> {
    run_file_test(path, times, true)
}

#[test]
fn parser_firstbreak() {
    let res = run_file_test_mono("parser_firstbreak.mmm", 1).unwrap();
    let ans = vec![0.0];
    assert_eq!(res, ans);
}

#[test]
fn recursion() {
    let res = run_file_test_mono("recursion.mmm", 1).unwrap();
    let ans = vec![5.0];
    assert_eq!(res, ans);
}

#[test]
fn counter() {
    let res = run_file_test_mono("counter.mmm", 10).unwrap();
    let ans = vec![0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0];
    assert_eq!(res, ans);
}
#[test]
fn statefn() {
    let res = run_file_test_mono("statefn.mmm", 10).unwrap();
    let ans = vec![0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0];
    assert_eq!(res, ans);
}
#[test]
fn statefn2_same() {
    let res = run_file_test_mono("statefn2_same.mmm", 3).unwrap();
    let ans = vec![0.0, 6.0, 12.0];
    assert_eq!(res, ans);
}

#[test]
fn statefn2() {
    let res = run_file_test_mono("statefn2.mmm", 3).unwrap();
    let ans = vec![0.0, 8.0, 16.0];
    assert_eq!(res, ans);
}

#[test]
fn loopcounter() {
    let res = run_file_test_mono("loopcounter.mmm", 10).unwrap();
    let ans = vec![0.0, 1.0, 2.0, 3.0, 4.0, 0.0, 1.0, 2.0, 3.0, 4.0];
    assert_eq!(res, ans);
}

#[test]
fn primitive_sin() {
    let res = run_file_test_mono("primitive_sin.mmm", 1).unwrap();
    let ans = vec![0.0];
    let r = (res[0] - ans[0]).abs() < std::f64::EPSILON;
    assert!(r);
}

#[test]
fn sinewave() {
    let res = run_file_test_mono("sinwave.mmm", 10).unwrap();
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
    let res = run_file_test_mono("if.mmm", 1).unwrap();
    let ans = vec![4120.0];
    assert_eq!(res, ans);
}

#[test]
fn letmulti() {
    let res = run_file_test_mono("let_multi.mmm", 1).unwrap();
    let ans = vec![3.0];
    assert_eq!(res, ans);
}
#[test]
fn let_tuple() {
    let res = run_file_test_mono("let_tuple.mmm", 1).unwrap();
    let ans = vec![11.0];
    assert_eq!(res, ans);
}
#[test]
fn let_tuple_nested() {
    let res = run_file_test_mono("let_tuple_nested.mmm", 1).unwrap();
    let ans = vec![34.0];
    assert_eq!(res, ans);
}
#[test]
fn closure_tuple_escape() {
    let res = run_file_test_mono("closure_tuple_escape.mmm", 2).unwrap();
    let ans = vec![44.0, 44.0];
    assert_eq!(res, ans);
}
#[test]
fn state_tuple() {
    let res = run_file_test_stereo("state_tuple.mmm", 3).unwrap();
    let ans = vec![0.0, 0.0, 1.0, 2.0, 2.0, 4.0];
    assert_eq!(res, ans);
}
#[test]
fn closure_open() {
    let res = run_file_test_mono("closure_open.mmm", 1).unwrap();
    let ans = vec![4.0];
    assert_eq!(res, ans);
}

#[test]
fn closure_open_3nested() {
    let res = run_file_test_mono("closure_open_3nested.mmm", 2).unwrap();
    let ans = vec![2.0, 2.0];
    assert_eq!(res, ans);
}
#[test]
fn closure_open_inline() {
    let res = run_file_test_mono("closure_open_inline.mmm", 2).unwrap();
    let ans = vec![2.0, 2.0];
    assert_eq!(res, ans);
}

#[test]
fn closure_closed() {
    let res = run_file_test_mono("closure_closed.mmm", 2).unwrap();
    let ans = vec![-6.0, -6.0];
    assert_eq!(res, ans);
}

#[test]
fn closure_argument() {
    let res = run_file_test_mono("closure_argument.mmm", 1).unwrap();
    let ans = vec![24.0];
    assert_eq!(res, ans);
}

#[test]
fn stateful_closure() {
    let res = run_file_test_mono("stateful_closure.mmm", 10).unwrap();
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
fn closure_counter() {
    let res = run_file_test_mono("closure_counter.mmm", 5).unwrap();
    let ans = vec![0.0, 1.0, 2.0, 3.0, 4.0];
    assert_eq!(res, ans);
}

#[test]
fn hof_state() {
    let res = run_file_test_mono("hof_state.mmm", 10).unwrap();
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

#[test]
fn simple_stereo() {
    let res = run_file_test_stereo("simple_stereo.mmm", 3).unwrap();
    let ans = vec![1.0, 2.0, 1.0, 2.0, 1.0, 2.0];
    assert_eq!(res, ans);
}

#[test]
fn tuple_args() {
    let res = run_file_test_stereo("tuple_args.mmm", 3).unwrap();
    let ans = vec![30.0, 50.0, 30.0, 50.0, 30.0, 50.0];
    assert_eq!(res, ans);
}

// implement one-sample delay on mimium with `self`
#[test]
fn fb_mem() {
    let res = run_file_test_stereo("fb_mem.mmm", 10).unwrap();
    let ans = vec![
        0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 2.0, 1.0, 3.0, 2.0, 4.0, 3.0, 5.0, 4.0, 6.0, 5.0, 7.0, 6.0,
        8.0, 7.0,
    ];
    assert_eq!(res, ans);
}

#[test]
fn fb_mem2() {
    let res = run_file_test_stereo("fb_mem2.mmm", 10).unwrap();
    let ans = vec![
        0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 2.0, 0.0, 3.0, 1.0, 4.0, 2.0, 5.0, 3.0, 6.0, 4.0, 7.0, 5.0,
        8.0, 6.0,
    ];
    assert_eq!(res, ans);
}

#[test]
fn fb_mem3_uninitialized_memory() {
    // It seems a function with a long state size might refer to an
    // uninitialized memory. Since this happens randomly, we need to repeat the
    // same test several times to reproduce the error.
    for _ in 0..100 {
        let res = run_file_test_stereo("fb_mem3.mmm", 1).unwrap();
        let ans = vec![0.0, 0.0];
        assert_eq!(res, ans);
    }
}
