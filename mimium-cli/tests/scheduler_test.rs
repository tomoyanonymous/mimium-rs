mod common;
use common::*;
use mimium_audiodriver::backends::mock::MockDriver;
use mimium_lang::compiler;

#[test]
fn scheduler_global_recursion() {
    let res = run_file_with_scheduler("scheduler_global_recursion.mmm", 10).unwrap();
    let ans = vec![0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0];
    assert_eq!(res, ans);
}

#[test]
fn scheduler_multiple_at_sametime() {
    let res = run_file_with_scheduler("scheduler_multiple_at_sametime.mmm", 5).unwrap();
    let ans = vec![0.0, 2.0, 4.0, 6.0, 8.0];
    assert_eq!(res, ans);
}

#[test]
#[should_panic]
fn scheduler_invalid() {
    // recursion with @now would cause infinite recursion, so this should be errored.
    let _ = run_file_with_scheduler("scheduler_invalid.mmm", 10);
}

#[test]
fn scheduler_counter() {
    let res = run_file_with_scheduler("scheduler_counter.mmm", 10).unwrap();
    let ans = vec![0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0];
    assert_eq!(res, ans);
}
#[test]
fn scheduler_counter_indirect() {
    let res = run_file_with_scheduler("scheduler_counter_indirect.mmm", 10).unwrap();
    let ans = vec![0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0];
    assert_eq!(res, ans);
}

//check if the number of closure does not change over times.
#[test]
fn scheduler_gc_test() {
    let (_, src) = load_src("scheduler_counter_indirect.mmm");
    let bytecode = compiler::emit_bytecode(&src).unwrap();

    let mut driver = MockDriver::new(bytecode.clone(), None);
    let _ = driver.play_times(2 as _);
    let first = driver.vmdata.vm.closures.len();

    let mut driver = MockDriver::new(bytecode.clone(), None);
    let _ = driver.play_times(3 as _);
    let second = driver.vmdata.vm.closures.len();

    let mut driver = MockDriver::new(bytecode, None);
    let _ = driver.play_times(4 as _);
    let third = driver.vmdata.vm.closures.len();
    assert!(first == second && second == third)
}
