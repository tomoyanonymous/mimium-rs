mod common;
use common::*;
use mimium_audiodriver::{backends::local_buffer::LocalBufferDriver, driver::Driver};
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

    let mut driver2 = LocalBufferDriver::new(2);
    driver2.init(bytecode.clone(), None);
    driver2.play();
    let first = driver2.vmdata.unwrap().vm.closures.len();

    let mut driver3 = LocalBufferDriver::new(3);
    driver3.init(bytecode.clone(), None);
    driver3.play();
    let second = driver3.vmdata.unwrap().vm.closures.len();

    let mut driver4 = LocalBufferDriver::new(4);
    driver4.init(bytecode.clone(), None);
    driver4.play();
    let third = driver4.vmdata.unwrap().vm.closures.len();
    assert!(first == second && second == third)
}
