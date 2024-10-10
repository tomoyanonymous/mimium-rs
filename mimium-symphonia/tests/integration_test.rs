use std::path::PathBuf;

use mimium_lang::{
    interner::{Symbol, ToSymbol},
    runtime::vm::Machine,
    utils::fileloader,
    ExecContext,
};

//todo: create common utility module for testing over multiple crate to prevent from duplicating code.
fn load_src(path: &str) -> (PathBuf, String) {
    let file: PathBuf = [env!("CARGO_MANIFEST_DIR"), "tests/mmm", path]
        .iter()
        .collect();
    println!("{}", file.to_str().unwrap());
    let (src, _path) = fileloader::load(file.to_string_lossy().to_string()).unwrap();
    (file, src)
}

fn render(path: &str, len: usize) -> Vec<f64> {
    let (abspath, src) = load_src(path);
    let mut ctx = ExecContext::new(
        &[mimium_symphonia::get_signature()],
        &[],
        Some(abspath.to_str().unwrap().to_symbol()),
    );
    let mut vm = ctx.prepare_machine(&src);
    let ret = vm.execute_main();
    assert_eq!(ret, 0);
    let mut res = vec![0.0f64; len];
    res.iter_mut().for_each(|r| {
        let _ = vm.execute_entry(&"dsp".to_symbol());
        *r = Machine::get_as::<f64>(vm.get_stack(0));
    });
    res
}

#[test]
fn test_readwav() {
    let res = render("loadwav.mmm", 101);
    let res_int = res.iter().map(|f| (*f * 100.0).round() as u32).collect::<Vec<_>>();
    let mut ans = (0u32..100).into_iter().collect::<Vec<_>>();
    ans.push(0);//0 should be returned when the index exceeds the boundary
    assert_eq!(res_int, ans);
}
