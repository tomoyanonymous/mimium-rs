mod common;
use common::*;

#[test]
fn scheduler_global_recursion() {
    let res = run_file_with_scheduler("scheduler_global_recursion.mmm", 10).unwrap();
    let ans = vec![0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0];
    assert_eq!(res, ans);
}

// #[test]
// fn scheduler_counter() {
//     let res = run_file_with_scheduler("scheduler_counter.mmm", 10).unwrap();
//     let ans = vec![0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0];
//     assert_eq!(res, ans);
// }
