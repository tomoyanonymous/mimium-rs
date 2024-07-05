use std::collections::HashMap;
use std::sync::Arc;

use crate::mir::{self, Mir, VReg};
use crate::runtime::vm::bytecode::Reg;
use crate::runtime::vm::{self, bytecode};
use crate::utils::error::ReportableError;
use vm::bytecode::Instruction as VmInstruction;

#[derive(Debug, Default)]
struct VStack(Reg);

impl VStack {
    pub fn push(&mut self) -> Reg {
        self.0 += 1;
        self.0 - 1
    }
    pub fn pop(&mut self) -> Reg {
        self.0 -= 1;
        self.0
    }
    pub fn pop_2(&mut self) -> Reg {
        self.0 -= 2;
        self.0
    }
    pub fn get_top(&self) -> Reg {
        self.0 - 1
    }
}

#[derive(Debug, Default)]
pub struct ByteCodeGenerator {
    vstack: VStack,
    vreg_map: HashMap<VReg, Reg>,
}

fn gen_raw_int(n: &i64) -> vm::RawVal {
    let raw = {
        let iptr = n as *const i64;
        iptr as *const vm::RawVal
    };
    unsafe { *raw }
}

fn gen_raw_float(n: &f64) -> vm::RawVal {
    let raw = {
        let iptr = n as *const f64;
        iptr as *const vm::RawVal
    };
    unsafe { *raw }
}

impl ByteCodeGenerator {
    fn get_value(&mut self, func: &mut vm::FuncProto, v: &Arc<mir::Value>) -> Reg {
        match v.as_ref() {
            mir::Value::Global(_) => todo!(),
            mir::Value::Argument(idx, _arg) => *idx as Reg,
            mir::Value::Register(vreg) => {
                //todo: consolidate infinite register
                *vreg as Reg
            }
            mir::Value::Float(_) => todo!(),
            mir::Value::Integer(_) => todo!(),
            mir::Value::Bool(_) => todo!(),
            mir::Value::Function(_, _) => todo!(),
            mir::Value::ExtFunction(_) => todo!(),
            mir::Value::Closure(_) => todo!(),
            mir::Value::FixPoint => todo!(),
            mir::Value::State => {
                let dst = self.vstack.push();
                func.bytecodes.push(bytecode::Instruction::GetState(dst));
                func.state_size += 1;
                dst
            }
            mir::Value::None => todo!(),
        }
    }
    fn get_binop(
        &mut self,
        funcproto: &mut vm::FuncProto,
        v1: &Arc<mir::Value>,
        v2: &Arc<mir::Value>,
    ) -> (Reg, Reg, Reg) {
        let r1 = self.get_value(funcproto, v1);
        let r2 = self.get_value(funcproto, v2);
        self.vstack.0 += 1;
        (self.vstack.get_top(), r1, r2)
    }
    fn emit_instruction(
        &mut self,
        funcproto: &mut vm::FuncProto,
        mirinst: &mir::Instruction,
    ) -> VmInstruction {
        match mirinst {
            mir::Instruction::Uinteger(u) => {
                let pos = funcproto.add_new_constant(*u);
                let reg = self.vstack.push();
                VmInstruction::MoveConst(reg, pos as u8)
            }
            mir::Instruction::Integer(i) => {
                let pos = funcproto.add_new_constant(gen_raw_int(i));
                let reg = self.vstack.push();
                VmInstruction::MoveConst(reg, pos as u8)
            }
            mir::Instruction::Float(n) => {
                let pos = funcproto.add_new_constant(gen_raw_float(n));
                let reg = self.vstack.push();
                VmInstruction::MoveConst(reg, pos as u8)
            }
            mir::Instruction::Alloc(_) => todo!(),
            mir::Instruction::Load(rawv) => todo!(),
            mir::Instruction::Store(_, _) => todo!(),
            mir::Instruction::Call(v, args) => {
                let nargs = args.len() as u8;
                let res = match v.as_ref() {
                    mir::Value::Register(address) => VmInstruction::Call(*address as Reg, nargs, 1),
                    mir::Value::Function(idx, state_size) => {
                        unreachable!();
                    }
                    mir::Value::ExtFunction(idx) => {
                        todo!()
                        // VmInstruction::CallExtFun(idx as Reg, nargs, 1)
                    }
                    mir::Value::Closure(reg) => {
                        todo!()
                        // VmInstruction::CallCls(reg as Reg, nargs, 1)
                    }
                    mir::Value::FixPoint => todo!(),
                    _ => unreachable!(),
                };
                self.vstack.0 -= nargs;
                res
            }
            mir::Instruction::Closure(_) => todo!(),
            mir::Instruction::GetUpValue(_, _) => todo!(),
            mir::Instruction::SetUpValue(_, _) => todo!(),
            mir::Instruction::PushStateOffset(v) => VmInstruction::ShiftStatePos(*v as i16),
            mir::Instruction::PopStateOffset(v) => VmInstruction::ShiftStatePos(- (*v as i16)),
            mir::Instruction::GetState(v) => {
                let res = VmInstruction::GetState(self.get_value(funcproto, v));
                self.vstack.push();
                res
            }
            mir::Instruction::SetState(v) => VmInstruction::SetState(self.get_value(funcproto, v)),
            mir::Instruction::JmpIf(_, _, _) => todo!(),
            mir::Instruction::Return(v) => VmInstruction::Return(self.get_value(funcproto, v), 1),
            mir::Instruction::AddF(v1, v2) => {
                let (dst, r1, r2) = self.get_binop(funcproto, v1, v2);
                VmInstruction::AddF(dst, r1, r2)
            }
            mir::Instruction::SubF(v1, v2) => {
                let (dst, r1, r2) = self.get_binop(funcproto, v1, v2);
                VmInstruction::SubF(dst, r1, r2)
            }
            mir::Instruction::MulF(v1, v2) => {
                let (dst, r1, r2) = self.get_binop(funcproto, v1, v2);
                VmInstruction::MulF(dst, r1, r2)
            }
            mir::Instruction::DivF(v1, v2) => {
                let (dst, r1, r2) = self.get_binop(funcproto, v1, v2);
                VmInstruction::DivF(dst, r1, r2)
            }
            _ => todo!(),
        }
    }
    fn generate_funcproto(&mut self, mirfunc: &mir::Function) -> (String, vm::FuncProto) {
        let mut func = vm::FuncProto::from(mirfunc);
        self.vstack.0 = func.nparam as Reg;
        mirfunc.body.iter().for_each(|block| {
            block.0.iter().for_each(|inst| {
                let newinst = self.emit_instruction(&mut func, inst);
                func.bytecodes.push(newinst);
            });
        });
        (mirfunc.label.0.clone(), func)
    }
    pub fn generate(&mut self, mir: Mir) -> vm::Program {
        let mut program = vm::Program::default();
        program.global_fn_table = mir
            .functions
            .iter()
            .map(|func| self.generate_funcproto(func))
            .collect();

        program
    }
}

pub fn gen_bytecode(mir: mir::Mir) -> Result<vm::Program, Vec<Box<dyn ReportableError>>> {
    let mut generator = ByteCodeGenerator::default();
    Ok(generator.generate(mir))
}

mod test {

    use crate::types::Type;
    use mir::Label;

    use super::*;
    #[test]
    fn build() {
        let mut src = mir::Mir::default();
        let arg = Arc::new(mir::Argument(Label("hoge".to_string()), Type::Unknown));
        let mut func = mir::Function::new("test", &[arg.clone()]);
        let mut block = mir::Block::default();
        block.0.push(mir::Instruction::Integer(1));
        let resint = Arc::new(mir::Value::Register(1));
        block.0.push(mir::Instruction::AddF(
            Arc::new(mir::Value::Argument(0, arg)),
            resint,
        ));
        block
            .0
            .push(mir::Instruction::Return(Arc::new(mir::Value::Register(2))));
        func.body = vec![block];
        src.functions.push(func);
        let mut generator = ByteCodeGenerator::default();
        let res = generator.generate(src);

        let mut answer = vm::Program::default();
        let mut main = vm::FuncProto::new(1, 1);

        main.constants.push(1);
        main.bytecodes = vec![
            VmInstruction::MoveConst(1, 0),
            VmInstruction::AddF(2, 0, 1),
            VmInstruction::Return(2, 1),
        ];
        answer.global_fn_table.push(("test".to_string(), main));
        assert_eq!(res, answer);
    }
}
