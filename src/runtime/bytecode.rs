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
    CallExtFun(Reg, u8, u8),
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

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Move(dst, src) => write!(f, "mov  {} {}", dst, src),
            Instruction::MoveConst(dst, num) => write!(f, "movc {} {}", dst, num),
            Instruction::Call(dst, nargs, nret_req) => {
                write!(f, "call    {} {} {}", dst, nargs, nret_req)
            }
            Instruction::CallCls(dst, nargs, nret_req) => {
                write!(f, "callcls {} {} {}", dst, nargs, nret_req)
            }
            Instruction::CallExtFun(dst, nargs, nret_req) => {
                write!(f, "callext {} {} {}", dst, nargs, nret_req)
            }
            Instruction::Closure(dst, src) => {
                write!(f, "closure {} {}", dst, src)
            }
            Instruction::Return0 => write!(f, "ret0"),
            Instruction::Return(iret, nret) => write!(f, "ret     {} {}", iret, nret),
            Instruction::GetUpValue(dst, srcup) => write!(f, "getupv   {} {}", dst, srcup),
            Instruction::SetUpValue(dstup, src) => write!(f, "setupv   {} {}", dstup, src),
            Instruction::Feed() => write!(f, "feed"),
            Instruction::Jmp(dst) => write!(f, "jmp     {}", dst),
            Instruction::JmpIf(dst, cond) => write!(f, "jmpif   {} {}", dst, cond),
            Instruction::AddF(dst, lhs, rhs) => write!(f, "addf    {} {} {}", dst, lhs, rhs),
            Instruction::SubF(dst, lhs, rhs) => write!(f, "subf    {} {} {}", dst, lhs, rhs),
            Instruction::MulF(dst, lhs, rhs) => write!(f, "mulf    {} {} {}", dst, lhs, rhs),
            Instruction::DivF(dst, lhs, rhs) => write!(f, "divf    {} {} {}", dst, lhs, rhs),
            Instruction::ModF(dst, lhs, rhs) => write!(f, "modf    {} {} {}", dst, lhs, rhs),
            Instruction::AbsF(dst, src) => write!(f, "absf    {} {}", dst, src),
            Instruction::NegF(dst, src) => write!(f, "negf    {} {}", dst, src),
            Instruction::SinF(dst, src) => write!(f, "sin     {} {}", dst, src),
            Instruction::CosF(dst, src) => write!(f, "cos     {} {}", dst, src),
            Instruction::PowF(_, _, _) => todo!(),
            Instruction::LogF(_, _, _) => todo!(),
            Instruction::AddI(dst, lhs, rhs) => write!(f, "add     {} {} {}", dst, lhs, rhs),
            Instruction::SubI(dst, lhs, rhs) => write!(f, "sub     {} {} {}", dst, lhs, rhs),
            Instruction::MulI(dst, lhs, rhs) => write!(f, "mul     {} {} {}", dst, lhs, rhs),
            Instruction::DivI(dst, lhs, rhs) => write!(f, "div     {} {} {}", dst, lhs, rhs),
            Instruction::ModI(dst, lhs, rhs) => write!(f, "mod     {} {} {}", dst, lhs, rhs),
            Instruction::AbsI(dst, src) => write!(f, "abs     {} {}", dst, src),
            Instruction::NegI(dst, src) => write!(f, "neg     {} {}", dst, src),
            Instruction::PowI(_, _) => todo!(),
            Instruction::LogI(_, _, _) => todo!(),
            Instruction::Not(dst, src) => write!(f, "not     {} {}", dst, src),
            Instruction::Eq(dst, lhs, rhs) => write!(f, "eq      {} {} {}", dst, lhs, rhs),
            Instruction::Ne(dst, lhs, rhs) => write!(f, "ne      {} {} {}", dst, lhs, rhs),
            Instruction::Gt(dst, lhs, rhs) => write!(f, "gt      {} {} {}", dst, lhs, rhs),
            Instruction::Ge(dst, lhs, rhs) => write!(f, "ge      {} {} {}", dst, lhs, rhs),
            Instruction::Lt(dst, lhs, rhs) => write!(f, "le      {} {} {}", dst, lhs, rhs),
            Instruction::Le(dst, lhs, rhs) => write!(f, "lt      {} {} {}", dst, lhs, rhs),
            Instruction::And(dst, lhs, rhs) => write!(f, "and     {} {} {}", dst, lhs, rhs),
            Instruction::Or(dst, lhs, rhs) => write!(f, "or      {} {} {}", dst, lhs, rhs),
            Instruction::CastFtoI(dst, src) => write!(f, "f2i     {} {}", dst, src),
            Instruction::CastItoF(dst, src) => write!(f, "i2f     {} {}", dst, src),
            Instruction::CastItoB(dst, src) => write!(f, "i2b     {} {}", dst, src),
        }
    }
}

#[cfg(test)]
#[test]
fn ensure_bytecode_size() {
    let size = std::mem::size_of::<Instruction>();
    assert!(size == 8 || size == 4);
}
