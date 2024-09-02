use mimium_audiodriver::{backends::mock::MockDriver, driver::SampleRate};
use mimium_lang::{
    interner::ToSymbol as _,
    numeric,
    runtime::vm::{FuncProto, Instruction, Machine, Program},
    types::{PType, Type},
};

#[test]
fn getnow_test() {
    // fn dsp(){
    //   now
    // }
    let inner_insts_main = vec![Instruction::Return0];
    let main_f = FuncProto {
        nparam: 0,
        nret: 0,
        upindexes: vec![],
        bytecodes: inner_insts_main,
        constants: vec![], //cls, int 4
        delay_sizes: vec![],
        state_size: 0,
    };
    let inner_insts = vec![
        Instruction::CallExtCls(0, 0, 1), //call getnow, 7 should be set at reg 0
        Instruction::Return(0, 1),        // return single value at 1
    ];
    let dsp_f = FuncProto {
        nparam: 0,
        nret: 1,
        upindexes: vec![],
        bytecodes: inner_insts,
        constants: vec![], //cls, int 4
        delay_sizes: vec![],
        state_size: 0,
    };
    let fns = vec![("main".to_symbol(), main_f), ("dsp".to_symbol(), dsp_f)];

    let prog = Program {
        global_fn_table: fns,
        ext_fun_table: vec![],
        ext_cls_table: vec![("_mimium_getnow".to_symbol(), numeric!())],
        global_vals: vec![],
    };

    let times = 10;
    let mut driver = MockDriver::new(prog, Some(SampleRate(48000)));

    let res = driver.play_times(times);
    let answer = vec![0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0];
    assert_eq!(res, answer);
}
