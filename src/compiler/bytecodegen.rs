use std::collections::HashMap;
use std::sync::Arc;

use crate::mir::{self, Mir, VReg};
use crate::runtime::vm;
use crate::runtime::vm::bytecode::Reg;
use vm::bytecode::Instruction as VmInstruction;

#[derive(Debug, Default)]
struct VStack(Reg);

impl VStack {
    pub fn push(&mut self) -> Reg {
        self.0 += 1;
        self.0
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
        self.0
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
    fn get_value(&mut self, v: &Arc<mir::Value>) -> Reg {
        match v.as_ref() {
            mir::Value::Global(_) => todo!(),
            mir::Value::Argument(idx,_arg) => {
                *idx as Reg
            },
            mir::Value::Register(vreg) => {
                let reg = self.vstack.pop() as Reg;
                self.vreg_map.insert(*vreg, reg);
                reg
            }
            mir::Value::Float(_) => todo!(),
            mir::Value::Integer(_) => todo!(),
            mir::Value::Bool(_) => todo!(),
            mir::Value::Function(_) => todo!(),
            mir::Value::ExtFunction(_) => todo!(),
            mir::Value::Closure(_) => todo!(),
            mir::Value::FixPoint => todo!(),
            mir::Value::None => todo!(),
        }
    }
    fn get_binop(&mut self, v1: &Arc<mir::Value>, v2: &Arc<mir::Value>) -> (Reg, Reg, Reg) {
        let r1 = self.get_value(v1);
        let r2 = self.get_value(v2);
        let dst = self.vstack.push();
        (dst, r1, r2)
    }
    fn emit_instruction(
        &mut self,
        funcproto: &mut vm::FuncProto,
        mirinst: &mir::Instruction,
    ) -> VmInstruction {
        match mirinst {
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
            mir::Instruction::Load(_) => todo!(),
            mir::Instruction::Store(_, _) => todo!(),
            mir::Instruction::Call(_, _) => todo!(),
            mir::Instruction::Closure(_) => todo!(),
            mir::Instruction::GetUpValue(_, _) => todo!(),
            mir::Instruction::SetUpValue(_, _) => todo!(),
            mir::Instruction::PushStateOffset(_) => todo!(),
            mir::Instruction::PopStateOffset(_) => todo!(),
            mir::Instruction::GetState(_) => todo!(),
            mir::Instruction::SetState(_) => todo!(),
            mir::Instruction::JmpIf(_, _, _) => todo!(),
            mir::Instruction::Return(_) => todo!(),
            mir::Instruction::AddF(v1, v2) => {
                let (dst, r1, r2) = self.get_binop(v1, v2);
                VmInstruction::AddF(dst, r1, r2)
            }
            mir::Instruction::SubF(v1, v2) => {
                let (dst, r1, r2) = self.get_binop(v1, v2);
                VmInstruction::SubF(dst, r1, r2)
            }
            mir::Instruction::MulF(v1, v2) => {
                let (dst, r1, r2) = self.get_binop(v1, v2);
                VmInstruction::MulF(dst, r1, r2)
            }
            mir::Instruction::DivF(v1, v2) => {
                let (dst, r1, r2) = self.get_binop(v1, v2);
                VmInstruction::DivF(dst, r1, r2)
            }
            _ => todo!(),
        }
    }
    fn generate_funcproto(&mut self, mirfunc: &mir::Function) -> vm::FuncProto {
        let nargs = mirfunc.args.len();
        let nret = 1;
        let mut func = vm::FuncProto::new(nargs, nret);

        mirfunc.body.iter().for_each(|block| {
            block.0.iter().for_each(|inst| {
                let newinst = self.emit_instruction(&mut func, inst);
                func.bytecodes.push(newinst);
            });
        });
        func
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

mod test {

    use mir::{Label};
    use crate::types::Type;

    use super::*;
    #[test]
    fn build() {
        let mut src = mir::Mir::default();
        let arg = Arc::new(mir::Argument(Label("hoge".to_string()),Type::Unknown));
        let mut func = mir::Function::new("test", &[arg.clone()]);
        let mut block = mir::Block::default();
        block.0.push(mir::Instruction::Integer(1));
        let resint = Arc::new(mir::Value::Register(0));
        block.0.push(mir::Instruction::AddF(Arc::new(mir::Value::Argument(0,arg)),resint));
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
            VmInstruction::Return(2, 1)
        ];
        answer.global_fn_table.push(main);
        assert_eq!(res,answer);


    }
}
