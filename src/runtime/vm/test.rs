use super::*;
use crate::runtime::bytecode::*;
use std::cell::RefCell;
use std::rc::Rc;
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
    let i = state.get_as::<i64>(*v);
    println!("{}", i);
    return 0;
}

#[test]
fn closuretest() {
    //fn makeCounter(beg,inc){
    // let n = beg+1;
    // return | | { //upvalue: 0:beg,1:inc,2:n
    //              n = n+inc;
    //              n
    //             }
    //}
    //fn main(){
    //  let c = makeCounter(2,3);
    //  print(c()); //print 6
    //  print(c()); // print 9
    //}

    let inner_insts = vec![
        Instruction::GetUpValue(0, 0), //load n
        Instruction::GetUpValue(1, 1), //load inc
        Instruction::AddI(2, 0, 1),    // store n+inc in new n
        Instruction::SetUpValue(0, 2), //store new n in upvalue index 0
        Instruction::GetUpValue(0, 1), //load n as return value
        Instruction::Return(1, 1),     // return single value at 1
    ];
    let inner_f = FuncProto {
        nparam: 0,
        upindexes: vec![2, 1],
        bytecodes: inner_insts,
        constants: vec![], //no constants in the inner function
    };
    let inner_insts2 = vec![
        // reg0:beg, reg1: inc
        Instruction::MoveConst(2, 0), //load 1 in reg2
        Instruction::MoveConst(3, 1), //load posf in reg3
        Instruction::AddI(0, 0, 2),   // beg+1, n is in reg0
        Instruction::Closure(4, 3), // make closure of constant table 1(which is the function table 0)
        // Instruction::Close(),       // convert n(at 0) and inc(at 1) into closed value
        Instruction::Return(4, 1), // return 1 value
    ];
    let makecounter_f = FuncProto {
        nparam: 2,
        upindexes: vec![],
        bytecodes: inner_insts2,
        constants: vec![1u64, 0], // 1, position of inner in global table
    };
    let inner_inst3 = vec![
        // no stack in the entry
        Instruction::MoveConst(0, 2), //load makecounter
        Instruction::MoveConst(1, 0), //load 2
        Instruction::MoveConst(2, 1), //load 3 [makecounter, 2, 3]
        Instruction::Call(0, 2, 1), // [(closure)]  call makecounter on register 2 with 2 arguments and 1 return value.return value (inner closure)is on reg 0
        //print(c())
        Instruction::CallCls(0, 0, 1), // call inner closure with 0 args and 1 return value.(result is in 0)
        Instruction::Move(1, 0),       // load result to reg 1
        Instruction::MoveConst(0, 3),  //set print into reg0
        Instruction::CallExtFun(0, 1, 0), //print result
        //repeat precious 4 step : print(c())
        Instruction::CallCls(0, 0, 1),
        Instruction::Move(1, 0),
        Instruction::MoveConst(0, 3),
        Instruction::CallExtFun(0, 1, 0),
        Instruction::Return0,
    ];
    let main_f = FuncProto {
        nparam: 0,
        upindexes: vec![],
        bytecodes: inner_inst3,
        constants: vec![2u64, 3u64, 1, 0], //2,3, makecounter, print_f
    };
    let global_fn_table = vec![inner_f, makecounter_f, main_f];
    let ext_fun_table: Vec<ExtFunType> = vec![lib_printi];
    let mut machine = Machine::new();
    let code = Program {
        global_fn_table,
        ext_fun_table,
    };
    let res = machine.execute(2, &code, None);
    assert_eq!(res, 0);
}
