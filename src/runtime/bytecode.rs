pub type Reg = u8; // register position
pub type ConstPos = u8;
pub type Offset = i16;

#[derive(Debug)]
pub enum Instruction {
    // Destination / Source
    Move(Reg, Reg),
    MoveConst(Reg, ConstPos),
    // call internal function
    // Function Address,Nargs,Nret
    Call(Reg, u8, u8),
    //call internal closure
    CallCls(Reg, u8, u8),
    // external function
    // destination, function address, Nargs
    CallExtFun(Reg,u8,u8),
    // destination, index of inner function prototype in global function table.
    Closure(Reg, ConstPos),

    // Close(), // currently not implemented as it is not required unless loop/break is used
    Return0,
    // Nrets, value start position
    Return(Reg, Reg),

    //destination,source
    GetUpValue(Reg, Reg),
    SetUpValue(Reg, Reg),

    Feed(),

    //jump label
    Jmp(Offset),
    JmpIf(Reg, Offset),

    // Primitive Operations.
    // Destination, Src1, Src2
    AddF(Reg, Reg, Reg),
    SubF(Reg, Reg, Reg),
    MulF(Reg, Reg, Reg),
    DivF(Reg, Reg, Reg),
    ModF(Reg, Reg, Reg),
    NegF(Reg, Reg),
    AbsF(Reg, Reg),
    SinF(Reg, Reg),
    CosF(Reg, Reg),
    PowF(Reg, Reg, Reg),
    LogF(Reg, Reg, Reg),

    // Primitive Operations for int
    AddI(Reg, Reg, Reg),
    SubI(Reg, Reg, Reg),
    MulI(Reg, Reg, Reg),
    DivI(Reg, Reg, Reg),
    ModI(Reg, Reg, Reg),
    NegI(Reg, Reg),
    AbsI(Reg, Reg),

    PowI(Reg, Reg),
    LogI(Reg, Reg, Reg),
    // primitive Operations for bool
    Not(Reg, Reg),
    Eq(Reg, Reg, Reg),
    Ne(Reg, Reg, Reg),
    Gt(Reg, Reg, Reg),
    Ge(Reg, Reg, Reg),
    Lt(Reg, Reg, Reg),
    Le(Reg, Reg, Reg),
    And(Reg, Reg, Reg),
    Or(Reg, Reg, Reg),

    CastFtoI(Reg, Reg),
    CastItoF(Reg, Reg),
    CastItoB(Reg, Reg),
}

#[cfg(test)]
#[test]
fn ensure_bytecode_size() {
    let size = std::mem::size_of::<Instruction>();
    assert!(size == 8 || size == 4);
}
