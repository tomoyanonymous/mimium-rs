use super::*;

impl std::fmt::Display for Mir {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for fun in self.functions.iter() {
            let _ = write!(f, "fn {} {:?} \n", fun.label.0, fun.args);
            for (i,block) in fun.body.iter().enumerate(){
                let _ = write!(f, "  block {i}\n");
                for insts in block.0.iter(){
                    let _ = write!(f,"    {insts}\n");
                }
            }
        }
        Ok(())
    }
}

impl std::fmt::Display for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::fmt::Display for Argument {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Argument(label, _t) = self;
        write!(f, "arg {}", label)
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Global(_) => todo!(),
            Value::Argument(v) => write!(f, "{v}"),
            Value::Register(r) => write!(f, "reg {r}"),
            Value::Float(n) => write!(f, "float {n}"),
            Value::Integer(i) => write!(f, "int {i}"),
            Value::Bool(_) => todo!(),
            Value::Function(id) => write!(f, "function {id}"),
            Value::ExtFunction(_) => todo!(),
            Value::Closure(_) => todo!(),
            Value::FixPoint => write!(f, "fixpoint"),
            Value::None => write!(f, "none"),
        }
    }
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Integer(i) => write!(f, "int {i}\n"),
            Instruction::Float(n) => write!(f, "int {n}\n"),
            Instruction::Alloc(_) => todo!(),
            Instruction::Load(_) => todo!(),
            Instruction::Store(_, _) => todo!(),
            Instruction::Call(fptr, args) => {
                let _ = write!(f, "call {}  ", *fptr,);
                for a in args.iter() {
                    let _ = write!(f, "{} ", *a);
                }
                write!(f, "\n")
            }
            Instruction::Closure(_) => todo!(),
            Instruction::GetUpValue(_, _) => todo!(),
            Instruction::SetUpValue(_, _) => todo!(),
            Instruction::PushStateOffset(_) => todo!(),
            Instruction::PopStateOffset(_) => todo!(),
            Instruction::GetState(_) => todo!(),
            Instruction::SetState(_) => todo!(),
            Instruction::JmpIf(_, _, _) => todo!(),
            Instruction::Return(_) => todo!(),
            Instruction::AddF(a, b) => write!(f, "addf {} {}", *a, *b),
            Instruction::SubF(a, b) => write!(f, "subf {} {}", *a, *b),
            Instruction::MulF(a, b) => write!(f, "mulf {} {}", *a, *b),
            Instruction::DivF(a, b) => write!(f, "divf {} {}", *a, *b),
            Instruction::ModF(_, _) => todo!(),
            Instruction::NegF(_) => todo!(),
            Instruction::AbsF(_) => todo!(),
            Instruction::SinF(_) => todo!(),
            Instruction::CosF(_) => todo!(),
            Instruction::PowF(_, _) => todo!(),
            Instruction::LogF(_, _) => todo!(),
            Instruction::AddI(_, _) => todo!(),
            Instruction::SubI(_, _) => todo!(),
            Instruction::MulI(_, _) => todo!(),
            Instruction::DivI(_, _) => todo!(),
            Instruction::ModI(_, _) => todo!(),
            Instruction::NegI(_) => todo!(),
            Instruction::AbsI(_) => todo!(),
            Instruction::PowI(_) => todo!(),
            Instruction::LogI(_, _) => todo!(),
            Instruction::Not(_) => todo!(),
            Instruction::Eq(_) => todo!(),
            Instruction::Ne(_) => todo!(),
            Instruction::Gt(_, _) => todo!(),
            Instruction::Ge(_, _) => todo!(),
            Instruction::Lt(_, _) => todo!(),
            Instruction::Le(_, _) => todo!(),
            Instruction::And(_, _) => todo!(),
            Instruction::Or(_, _) => todo!(),
            Instruction::CastFtoI(_) => todo!(),
            Instruction::CastItoF(_) => todo!(),
            Instruction::CastItoB(_) => todo!(),
        }
    }
}
