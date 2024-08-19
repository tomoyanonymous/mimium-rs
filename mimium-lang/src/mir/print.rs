use crate::format_vec;

use super::*;

impl std::fmt::Display for Mir {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for fun in self.functions.iter() {
            let af = format_vec!(fun.args);
            let _ = write!(f, "fn {} [{af}]\n", fun.label.0);
            let upi = format_vec!(fun.upindexes);
            let _ = write!(f, "upindexes:[{upi}]");
            if let Some(upper_i) = fun.upperfn_i {
                let _ = write!(f, "upper:{upper_i}");
            }
            let _ = write!(f, " state_size: {}", fun.state_size);
            for (i, block) in fun.body.iter().enumerate() {
                let _ = write!(f, "\n  block {i}\n");
                for (v, insts) in block.0.iter() {
                    let _ = match v.as_ref() {
                        Value::None => write!(f, "  {: <10} {insts}\n", " "),
                        _ => write!(f, "  {:<7} := {insts}\n", format!("{}", *v)),
                    };
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
        write!(f, "arg {}", label.0)
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Global(gv) => write!(f, "global({})", *gv),
            Value::Argument(_, v) => write!(f, "{}", v.0),
            Value::Register(r) => write!(f, "reg({r})"),
            Value::Function(id, _statesize, nret) => write!(f, "function {id} (nret: {nret})"),
            Value::ExtFunction(label, t) => write!(f, "extfun {label} {t}"),
            Value::FixPoint(i) => write!(f, "fixpoint {i}"),
            Value::State(v) => write!(f, "state({})", *v),
            Value::None => write!(f, "none"),
        }
    }
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Uinteger(u) => write!(f, "uint {u}"),
            Instruction::Integer(i) => write!(f, "int {i}"),
            Instruction::Float(n) => write!(f, "float {n}"),
            Instruction::Alloc(t) => write!(f, "alloc {t}"),
            Instruction::Load(src, ty) => write!(f, "load {src}, {ty}"),
            Instruction::Store(dst, src, ty) => write!(f, "store {dst}, {src}, {ty}"),
            Instruction::GetElement {
                value,
                ty,
                array_idx,
                tuple_offset,
            } => {
                write!(f, "getelement {value}, {ty}, {tuple_offset}[{array_idx}]")
            }
            Instruction::Call(fptr, args, rty) => {
                write!(f, "call {} [{}] ->{rty}", *fptr, format_vec!(args))
            }
            Instruction::CallCls(cls, args, rty) => {
                write!(f, "callcls {} [{}] ->{rty}", *cls, format_vec!(args))
            }
            Instruction::Closure(fun) => {
                if let Value::Function(idx, _, nret) = fun.as_ref() {
                    write!(f, "closure {idx} (nret: {nret})")
                } else {
                    write!(f, "closure {}", *fun)
                }
            }
            Instruction::GetUpValue(idx) => write!(f, "getupval {idx}"),
            Instruction::SetUpValue(idx) => write!(f, "setupval {idx}"),
            Instruction::GetGlobal(v) => write!(f, "getglobal {}", *v),
            Instruction::SetGlobal(dst, src) => write!(f, "setglobal {} {}", *dst, *src),
            Instruction::PushStateOffset(v) => write!(f, "pushstateidx {}", *v),
            Instruction::PopStateOffset(v) => write!(f, "popstateidx  {}", *v),

            Instruction::GetState => write!(f, "getstate"),
            Instruction::JmpIf(cond, tbb, ebb) => write!(f, "jmpif {cond} {tbb} {ebb}"),
            Instruction::Jmp(bb) => write!(f, "jmp {bb}"),
            Instruction::Phi(t, e) => write!(f, "phi {t} {e}"),
            Instruction::Return(a, rty) => write!(f, "ret {} {rty}", *a),
            Instruction::ReturnFeed(v, rty) => write!(f, "retfeed {} {rty}", *v),
            Instruction::Delay(max, a, b) => write!(f, "delay {max} {} {}", *a, *b),
            Instruction::Mem(a) => write!(f, "mem {}", *a),
            Instruction::AddF(a, b) => write!(f, "addf {} {}", *a, *b),
            Instruction::SubF(a, b) => write!(f, "subf {} {}", *a, *b),
            Instruction::MulF(a, b) => write!(f, "mulf {} {}", *a, *b),
            Instruction::DivF(a, b) => write!(f, "divf {} {}", *a, *b),
            Instruction::ModF(a, b) => write!(f, "modf {} {}", *a, *b),
            Instruction::NegF(a) => write!(f, "negf {}", *a),
            Instruction::AbsF(a) => write!(f, "absf {}", *a),
            Instruction::SinF(a) => write!(f, "sinf {}", *a),
            Instruction::CosF(a) => write!(f, "sinf {}", *a),
            Instruction::SqrtF(a) => write!(f, "sqrtf {}", *a),
            Instruction::PowF(a, b) => write!(f, "powf {} {}", *a, *b),
            Instruction::LogF(a, b) => write!(f, "logf {} {}", *a, *b),
            Instruction::AddI(a, b) => write!(f, "addi {} {}", *a, *b),
            Instruction::SubI(a, b) => write!(f, "subi {} {}", *a, *b),
            Instruction::MulI(a, b) => write!(f, "muli {} {}", *a, *b),
            Instruction::DivI(a, b) => write!(f, "divi {} {}", *a, *b),
            Instruction::ModI(a, b) => write!(f, "modi {} {}", *a, *b),
            Instruction::NegI(a) => write!(f, "negi {}", *a),
            Instruction::AbsI(a) => write!(f, "absi {}", *a),
            Instruction::PowI(a) => write!(f, "powi {}", *a),
            Instruction::LogI(_, _) => todo!(),
            Instruction::Not(_) => todo!(),
            Instruction::Eq(a, b) => write!(f, "eq {} {}", *a, *b),
            Instruction::Ne(a, b) => write!(f, "ne {} {}", *a, *b),
            Instruction::Gt(a, b) => write!(f, "gt {} {}", *a, *b),
            Instruction::Ge(a, b) => write!(f, "ge {} {}", *a, *b),
            Instruction::Lt(a, b) => write!(f, "lt {} {}", *a, *b),
            Instruction::Le(a, b) => write!(f, "le {} {}", *a, *b),
            Instruction::And(a, b) => write!(f, "and {} {}", *a, *b),
            Instruction::Or(a, b) => write!(f, "or {} {}", *a, *b),
            Instruction::CastFtoI(_) => todo!(),
            Instruction::CastItoF(_) => todo!(),
            Instruction::CastItoB(_) => todo!(),
        }
    }
}
