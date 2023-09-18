use super::*;
use crate::runtime::bytecode::*;


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
    // let n = beg+1; // local- 0:beg, 1:inc 2: 1 3:n
    // return | | { //upvalue: n:3 inc:1
    //              n = n+inc;
    //              n
    //             }
    //}
    //fn main(){
    //  let c = makeCounter(13,7);
    //  print(c()); //print 21
    //  print(c()); // print 28
    //}

    let inner_insts = vec![
        Instruction::GetUpValue(0, 0), //load n
        Instruction::GetUpValue(1, 1), //load inc
        Instruction::AddI(0, 0, 1),    // store n+inc in new n
        Instruction::SetUpValue(0, 0), //store new n in upvalue index 0
        Instruction::Return(0, 1),     // return single value at 1
    ];
    let inner_f = FuncProto {
        nparam: 0,
        upindexes: vec![UpIndex::Local(3), UpIndex::Local(1)],
        bytecodes: inner_insts,
        constants: vec![], //no constants in the inner function
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
        Instruction::Move(1,0), // move closure 0 to 1
        Instruction::CallCls(1, 0, 1), // call inner closure with 0 args and 1 return value.(result is in 0)
        Instruction::Move(2, 1),       // load result to reg 2
        Instruction::MoveConst(1, 3),  //set print into reg1
        Instruction::CallExtFun(1, 1, 0), //print result
        //repeat precious 4 step : print(c())
        Instruction::Move(1,0), // move closure 0 to 1
        Instruction::CallCls(1, 0, 1),
        Instruction::Move(2, 1),
        Instruction::MoveConst(1, 3),
        Instruction::CallExtFun(1, 1, 0),
        Instruction::Return0,
    ];
    let main_f = FuncProto {
        nparam: 0,
        upindexes: vec![],
        bytecodes: inner_inst3,
        constants: vec![13u64, 7u64, 1, 0], //13,7, makecounter, print_f
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
