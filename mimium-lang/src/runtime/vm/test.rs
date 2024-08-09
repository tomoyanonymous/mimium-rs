use super::*;
use crate::{
    function,
    mir::OpenUpValue,
    numeric,
    types::{PType, Type},
};
#[test]
fn size_of_intern_func() {
    let s = std::mem::size_of::<std::rc::Rc<FuncProto>>();
    assert_eq!(s, 8);
}
#[test]
fn size_of_extern_func() {
    let s = std::mem::size_of::<ExtFunType>();
    assert_eq!(s, 8);
}

//single print function
fn lib_printi(state: &mut Machine) -> i64 {
    let v = state.get_top();
    let i = Machine::get_as::<i64>(*v);
    println!("{}", i);
    return 1;
}

#[test]
fn closuretest() {
    //fn makeCounter(beg,inc){
    // let n = beg+1;
    // return |x| { //local 0:x
    //              n = n+inc+x;
    //              n
    //             }
    //}
    //fn main(){
    //  let c = makeCounter(13,7);
    //  print(c(0)); //print 21
    //  print(c(0)); // print 28
    //}

    let inner_insts = vec![
        Instruction::GetUpValue(0, 0), //load n
        Instruction::GetUpValue(1, 1), //load inc
        Instruction::AddI(0, 0, 1),    // store n+inc in new n
        Instruction::Move(1, 0),       //load x
        Instruction::AddI(0, 0, 1),    // store n+inc+x in new n
        Instruction::SetUpValue(0, 0), //store new n in upvalue index 0
        Instruction::Return(0, 1),     // return single value at 1
    ];
    let inner_f = FuncProto {
        nparam: 0,
        nret: 1,
        upindexes: Vec::from_slice(&[OpenUpValue(1), OpenUpValue(2)]),
        bytecodes: inner_insts.into(),
        constants: Default::default(), //no constants in the inner function
        state_size: 0,
        delay_sizes: Default::default(),
    };
    let inner_insts2 = vec![
        // reg0:beg, reg1: inc
        Instruction::MoveConst(2, 0), //load 1 in reg2
        Instruction::AddI(3, 0, 2),   // beg+1, n is in reg0
        Instruction::MoveConst(4, 1), //load posf in reg4
        Instruction::Closure(5, 4), // make closure of constant table 1(which is the function table 0)
        // Instruction::Close(),       // convert n(at 0) and inc(at 1) into closed value
        Instruction::Return(5, 1), // return 1 value
    ];
    let makecounter_f = FuncProto {
        nparam: 2,
        nret: 1,
        upindexes: Default::default(),
        bytecodes: inner_insts2.into(),
        constants:Vec::from_slice(&[1u64, 2]), // 1, position of inner in global table
        delay_sizes: Default::default(),
        state_size: 0,
    };
    let main_inst = vec![
        // no stack in the entry
        Instruction::MoveConst(0, 2), //load makecounter
        Instruction::MoveConst(1, 0), //load 2
        Instruction::MoveConst(2, 1), //load 3 [makecounter, 2, 3]
        Instruction::Call(0, 2, 1), // [(closure)]  call makecounter on register 2 with 2 arguments and 1 return value.return value (inner closure)is on reg 0
        //print(c())
        Instruction::Move(1, 0),          // move closure 0 to 1
        Instruction::MoveConst(2, 3),     //load 0
        Instruction::CallCls(1, 0, 1), // call inner closure with 0 args and 1 return value.(result is in 0)
        Instruction::Move(2, 1),       // load result to reg 2
        Instruction::MoveConst(1, 3),  //set print into reg1
        Instruction::CallExtFun(1, 1, 1), //print result
        //repeat precvous 4 step : print(c())
        Instruction::Move(1, 0),      // move closure 0 to 1
        Instruction::MoveConst(2, 3), //load 0
        Instruction::CallCls(1, 0, 1),
        Instruction::Move(2, 1),
        Instruction::MoveConst(1, 3),
        Instruction::CallExtFun(1, 1, 1),
        Instruction::Return0,
    ];
    let main_f = FuncProto {
        nparam: 0,
        nret: 1,
        upindexes: Default::default(),
        bytecodes: main_inst.into(),
        constants: Vec::from_slice(&[13u64, 7u64, 1, 0]), //13,7, makecounter, print_f
        delay_sizes: Default::default(),
        state_size: 0,
    };
    let global_fn_table = vec![
        ("main".to_string(), main_f),
        ("makecounter".to_string(), makecounter_f),
        ("inner".to_string(), inner_f),
    ].into();
    let mut machine = Machine::new();

    // machine.install_extern_fn("lib_printi".to_string(), lib_printi);
    let prog = Program {
        global_fn_table,
        ext_fun_table: Vec::from_elem(("probe".to_string(), function!(vec![numeric!()], numeric!())),1),
        ext_cls_table: Default::default(),
        global_vals: Default::default(),
    };
    // let mut feedstate = FeedState::default();
    let res = machine.execute_main(&prog);
    assert_eq!(res, 0);
}

#[test]
fn rust_closure_test() {
    //fn main()->int{
    // return rust_closure(4)
    //}
    let inner_insts = Vec::from_slice(&[
        Instruction::MoveConst(0, 0),     //load closure
        Instruction::MoveConst(1, 1),     //load const int 4
        Instruction::CallExtCls(0, 1, 1), //call closure, 7 should be set at reg 0
        Instruction::Return0,             // return single value at 1
    ]);
    let main_f = FuncProto {
        nparam: 0,
        nret: 1,
        upindexes: Default::default(),
        bytecodes: inner_insts,
        constants: Vec::from_slice(&[0u64, 4u64]), //cls, int 4
        delay_sizes: Default::default(),
        state_size: 0,
    };
    let fns = Vec::from_elem(main_f,1);
    let fnames = Vec::from_elem("main".to_string(),1);
    let global_fn_table = fnames.into_iter().zip(fns.into_iter()).collect::<Vec<_>>();
    // let mut count = 0;
    let cls = Arc::new(Mutex::new(|m: &mut Machine| {
        let v = m.get_top();
        let i = Machine::get_as::<u64>(*v) + 3;
        println!("Call from closure: {}", i);
        //?????
        m.set_stack(-1, Machine::to_value(i));
        return 1;
    }));
    let mut machine = Machine::new();
    machine.install_extern_fn("lib_printi".to_string(), lib_printi);
    machine.install_extern_cls("rustclosure".to_string(), cls.clone());
    let prog = Program {
        global_fn_table,
        ext_fun_table: Vec::from_elem(("lib_printi".to_string(), Type::Unknown), 1),
        ext_cls_table: Vec::from_elem(("rustclosure".to_string(), Type::Unknown), 1),
        global_vals: Default::default(),
    };
    // let mut feedstate = FeedState::default();
    let res = machine.execute_main(&prog);
    assert_eq!(res, 0);
}
