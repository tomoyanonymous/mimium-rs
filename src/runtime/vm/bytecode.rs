pub type Reg = u8; // register position
pub type ConstPos = u8;
pub type Offset = i16;

#[derive(Debug, Clone, Copy, PartialEq)]
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
    // Function Address,Nargs,Nret
    CallExtFun(Reg, u8, u8),
    //call rust closure
    // Function Address,Nargs,Nret
    CallExtCls(Reg, u8, u8),
    // destination, index of inner function prototype in global function table.
    Closure(Reg, ConstPos),

    //destination,source
    GetUpValue(Reg, Reg),
    SetUpValue(Reg, Reg),
    //call internal state over time, destination,source
    GetState(Reg),
    SetState(Reg),
    ShiftStatePos(i16),

    // Close(), // currently not implemented as it is not required unless loop/break is used
    Return0,
    // value start position, Nrets
    Return(Reg, Reg),

    //jump label
    Jmp(Offset),
    JmpIfNeg(Reg, Offset),

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

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Return0 => write!(f, "ret0"),
            Instruction::Jmp(dst) => write!(f, "{:<10} {}", "jmp", dst),
            Instruction::GetState(dst) => write!(f, "{:<10} {}", "getstate", dst),
            Instruction::SetState(src) => write!(f, "{:<10} {}", "setstate", src),
            Instruction::ShiftStatePos(_) => todo!(),
            Instruction::Move(dst, src) => write!(f, "{:<10} {} {}", "mov", dst, src),
            Instruction::MoveConst(dst, num) => write!(f, "{:<10} {} {}", "movc", dst, num),

            Instruction::Closure(dst, src) => {
                write!(f, "{:<10} {} {}", "closure", dst, src)
            }

            Instruction::Return(iret, nret) => write!(f, "{:<10} {} {}", "ret", iret, nret),
            Instruction::GetUpValue(dst, srcup) => write!(f, "{:<10} {} {}", "getupv", dst, srcup),
            Instruction::SetUpValue(dstup, src) => write!(f, "{:<10} {} {}", "setupv", dstup, src),
            Instruction::JmpIfNeg(dst, cond) => write!(f, "{:<10} {} {}", "jmpif", dst, cond),
            Instruction::AbsF(dst, src) => write!(f, "{:<10} {} {}", "absf", dst, src),
            Instruction::NegF(dst, src) => write!(f, "{:<10} {} {}", "negf", dst, src),
            Instruction::SinF(dst, src) => write!(f, "{:<10} {} {}", "sin", dst, src),
            Instruction::CosF(dst, src) => write!(f, "{:<10} {} {}", "cos", dst, src),
            Instruction::PowI(dst, src) => write!(f, "{:<10} {} {}", "powi", dst, src),
            Instruction::AbsI(dst, src) => write!(f, "{:<10} {} {}", "abs", dst, src),
            Instruction::NegI(dst, src) => write!(f, "{:<10} {} {}", "neg", dst, src),
            Instruction::Not(dst, src) => write!(f, "{:<10} {} {}", "not", dst, src),
            Instruction::CastFtoI(dst, src) => write!(f, "{:<10} {} {}", "f2i", dst, src),
            Instruction::CastItoF(dst, src) => write!(f, "{:<10} {} {}", "i2f", dst, src),
            Instruction::CastItoB(dst, src) => write!(f, "{:<10} {} {}", "i2b", dst, src),
            //3 arguments
            Instruction::Call(dst, nargs, nret_req) => {
                write!(f, "{:<10} {} {} {}", "call", dst, nargs, nret_req)
            }
            Instruction::CallCls(dst, nargs, nret_req) => {
                write!(f, "{:<10} {} {} {}", "callcls", dst, nargs, nret_req)
            }
            Instruction::CallExtFun(dst, nargs, nret_req) => {
                write!(f, "{:<10} {} {} {}", "callext", dst, nargs, nret_req)
            }
            Instruction::CallExtCls(dst, nargs, nret_req) => {
                write!(f, "{:<10} {} {} {}", "callextcls", dst, nargs, nret_req)
            }

            Instruction::LogI(dst, lhs, rhs) => write!(f, "{:<10} {} {} {}", "logi", dst, lhs, rhs),
            Instruction::AddF(dst, lhs, rhs) => write!(f, "{:<10} {} {} {}", "addf", dst, lhs, rhs),
            Instruction::SubF(dst, lhs, rhs) => write!(f, "{:<10} {} {} {}", "subf", dst, lhs, rhs),
            Instruction::MulF(dst, lhs, rhs) => write!(f, "{:<10} {} {} {}", "mulf", dst, lhs, rhs),
            Instruction::DivF(dst, lhs, rhs) => write!(f, "{:<10} {} {} {}", "divf", dst, lhs, rhs),
            Instruction::ModF(dst, lhs, rhs) => write!(f, "{:<10} {} {} {}", "modf", dst, lhs, rhs),
            Instruction::PowF(dst, lhs, rhs) => write!(f, "{:<10} {} {} {}", "powf", dst, lhs, rhs),
            Instruction::LogF(dst, lhs, rhs) => write!(f, "{:<10} {} {} {}", "logf", dst, lhs, rhs),
            Instruction::AddI(dst, lhs, rhs) => write!(f, "{:<10} {} {} {}", "add", dst, lhs, rhs),
            Instruction::SubI(dst, lhs, rhs) => write!(f, "{:<10} {} {} {}", "sub", dst, lhs, rhs),
            Instruction::MulI(dst, lhs, rhs) => write!(f, "{:<10} {} {} {}", "mul", dst, lhs, rhs),
            Instruction::DivI(dst, lhs, rhs) => write!(f, "{:<10} {} {} {}", "div", dst, lhs, rhs),
            Instruction::ModI(dst, lhs, rhs) => write!(f, "{:<10} {} {} {}", "mod", dst, lhs, rhs),
            Instruction::Eq(dst, lhs, rhs) => write!(f, "{:<10} {} {} {}", "eq", dst, lhs, rhs),
            Instruction::Ne(dst, lhs, rhs) => write!(f, "{:<10} {} {} {}", "ne", dst, lhs, rhs),
            Instruction::Gt(dst, lhs, rhs) => write!(f, "{:<10} {} {} {}", "gt", dst, lhs, rhs),
            Instruction::Ge(dst, lhs, rhs) => write!(f, "{:<10} {} {} {}", "ge", dst, lhs, rhs),
            Instruction::Lt(dst, lhs, rhs) => write!(f, "{:<10} {} {} {}", "le", dst, lhs, rhs),
            Instruction::Le(dst, lhs, rhs) => write!(f, "{:<10} {} {} {}", "lt", dst, lhs, rhs),
            Instruction::And(dst, lhs, rhs) => write!(f, "{:<10} {} {} {}", "and", dst, lhs, rhs),
            Instruction::Or(dst, lhs, rhs) => write!(f, "{:<10} {} {} {}", "or", dst, lhs, rhs),
        }
    }
}

#[cfg(test)]
#[test]
fn ensure_bytecode_size() {
    let size = std::mem::size_of::<Instruction>();
    assert!(size == 8 || size == 4);
}
