use mimium_test::*;

fn run_file_with_symphonia(path: &str, times: u64) -> Result<Vec<f64>, ()> {
    run_file_with_plugins(path, times, &[mimium_symphonia::get_signature()], &[])
}

#[test]
fn test_readwav() {
    let res = run_file_with_symphonia("loadwav.mmm", 101).expect("failed to evaluate");
    let res_int = res
        .iter()
        .map(|f| (*f * 100.0).round() as u32)
        .collect::<Vec<_>>();
    let mut ans = (0u32..100).into_iter().collect::<Vec<_>>();
    ans.push(0); //0 should be returned when the index exceeds the boundary
    assert_eq!(res_int, ans);
}
