
use std::sync::Arc;

use crate::mir::{self, Mir};
use crate::runtime::vm::bytecode::Reg;
use crate::runtime::vm::{self};
use crate::utils::error::ReportableError;
use vm::bytecode::Instruction as VmInstruction;
#[derive(Debug)]
struct VRegister(Vec<Option<Arc<mir::Value>>>);

impl Default for VRegister {
    fn default() -> Self {
        Self(vec![None; 256])
    }
}

impl VRegister {
    pub fn reset(&mut self) {
        self.0.fill(None);
    }
    pub fn add_newvalue(&mut self, v: Arc<mir::Value>) -> Reg {
        // println!("add  reg:{v} {:?}", self.0.as_slice()[0..10].to_vec());
        let pos = self.0.iter().position(|v| v.is_none()).unwrap();
        self.0[pos] = Some(v.clone());
        pos as Reg
    }
    pub fn remove_value(&mut self, v: Arc<mir::Value>) {
        // println!("rm   reg:{v} {:?}", self.0.as_slice()[0..10].to_vec());
        match self.0.iter().position(|v1| match v1 {
            Some(v1) => *v1 == v,
            None => false,
        }) {
            Some(i) => self.0[i] = None,
            None => {
                panic!("value does not exist in virtual registers")
            }
        }
    }
    pub fn find(&mut self, v: Arc<mir::Value>) -> Option<Reg> {
        // println!("find reg:{v} {:?}", self.0.as_slice()[0..10].to_vec());
        //todo: Error handling
        let res = self.0.iter().position(|v1| match v1 {
            Some(v1_c) => *v1_c == v,
            _ => false,
        });
        match res {
            Some(pos) => {
                //mir is SSA form, so the value will be used only once.
                self.0[pos] = None;
                Some(pos as Reg)
            }
            None => None,
        }
    }
}

#[derive(Debug, Default)]
pub struct ByteCodeGenerator {
    vregister: VRegister,
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
    fn get_binop(
        &mut self,
        _funcproto: &mut vm::FuncProto,
        v1: &Arc<mir::Value>,
        v2: &Arc<mir::Value>,
    ) -> (Reg, Reg) {
        let r1 = self.vregister.find(v1.clone()).unwrap();
        let r2 = self.vregister.find(v2.clone()).unwrap();
        (r1, r2)
    }
    fn get_destination(&mut self, dst: Arc<mir::Value>) -> Reg {
        self.vregister.add_newvalue(dst)
    }
    fn emit_instruction(
        &mut self,
        funcproto: &mut vm::FuncProto,
        dst: Arc<mir::Value>,
        mirinst: &mir::Instruction,
    ) -> VmInstruction {
        match mirinst {
            mir::Instruction::Uinteger(u) => {
                let pos = funcproto.add_new_constant(*u);
                VmInstruction::MoveConst(self.get_destination(dst), pos as u8)
            }
            mir::Instruction::Integer(i) => {
                let pos = funcproto.add_new_constant(gen_raw_int(i));

                VmInstruction::MoveConst(self.get_destination(dst), pos as u8)
            }
            mir::Instruction::Float(n) => {
                let pos = funcproto.add_new_constant(gen_raw_float(n));

                VmInstruction::MoveConst(self.get_destination(dst), pos as u8)
            }
            mir::Instruction::Alloc(_) => todo!(),
            mir::Instruction::Load(_) => todo!(),
            mir::Instruction::Store(_, _) => todo!(),
            mir::Instruction::Call(v, args) => {
                let nargs = args.len() as u8;
                let res = match v.as_ref() {
                    mir::Value::Register(_address) => {
                        let faddress = self.vregister.find(v.clone()).unwrap();
                        let dst = self.get_destination(dst);
                        if dst != faddress{
                            funcproto
                            .bytecodes
                            .push(VmInstruction::Move(dst as Reg, faddress));
                        }
                        for (i, a) in args.iter().enumerate() {
                            let src = self.vregister.find(a.clone()).unwrap();
                            let adst = dst as usize + i + 1;
                            if src as usize != adst {
                                funcproto
                                    .bytecodes
                                    .push(VmInstruction::Move(adst as Reg, src));
                            }
                        }
                        VmInstruction::Call(dst, nargs, 1)
                    }
                    mir::Value::Function(_idx, _state_size) => {
                        unreachable!();
                    }
                    mir::Value::ExtFunction(_idx) => {
                        todo!()
                        // VmInstruction::CallExtFun(idx as Reg, nargs, 1)
                    }
                    mir::Value::Closure(_reg) => {
                        todo!()
                        // VmInstruction::CallCls(reg as Reg, nargs, 1)
                    }
                    mir::Value::FixPoint => todo!(),
                    _ => unreachable!(),
                };
                res
            }
            mir::Instruction::Closure(_) => todo!(),
            mir::Instruction::GetUpValue(_, _) => todo!(),
            mir::Instruction::SetUpValue(_, _) => todo!(),
            mir::Instruction::PushStateOffset(v) => VmInstruction::ShiftStatePos(*v as i16),
            mir::Instruction::PopStateOffset(v) => VmInstruction::ShiftStatePos(-(*v as i16)),
            mir::Instruction::GetState => VmInstruction::GetState(self.get_destination(dst)),

            mir::Instruction::JmpIf(_, _, _) => todo!(),
            mir::Instruction::Return(v) => {
                VmInstruction::Return(self.vregister.find(v.clone()).unwrap(), 1)
            }
            mir::Instruction::ReturnFeed(new) => {
                let old = self.vregister.add_newvalue(dst);
                funcproto.bytecodes.push(VmInstruction::GetState(old));
                let new= self.vregister.find(new.clone()).unwrap();
                funcproto.bytecodes.push(VmInstruction::SetState(new));
                VmInstruction::Return(old, 1)
            }
            mir::Instruction::AddF(v1, v2) => {
                let (r1, r2) = self.get_binop(funcproto, v1, v2);
                VmInstruction::AddF(self.get_destination(dst), r1, r2)
            }
            mir::Instruction::SubF(v1, v2) => {
                let (r1, r2) = self.get_binop(funcproto, v1, v2);
                VmInstruction::SubF(self.get_destination(dst), r1, r2)
            }
            mir::Instruction::MulF(v1, v2) => {
                let (r1, r2) = self.get_binop(funcproto, v1, v2);
                VmInstruction::MulF(self.get_destination(dst), r1, r2)
            }
            mir::Instruction::DivF(v1, v2) => {
                let (r1, r2) = self.get_binop(funcproto, v1, v2);
                VmInstruction::DivF(self.get_destination(dst), r1, r2)
            }
            mir::Instruction::ModF(v1,v2)=> {
                let (r1, r2) = self.get_binop(funcproto, v1, v2);
                VmInstruction::ModF(self.get_destination(dst), r1, r2)
            }
            mir::Instruction::AddI(v1, v2) => {
                let (r1, r2) = self.get_binop(funcproto, v1, v2);
                VmInstruction::AddI(self.get_destination(dst), r1, r2)
            }
            mir::Instruction::SubI(v1, v2) => {
                let (r1, r2) = self.get_binop(funcproto, v1, v2);
                VmInstruction::SubI(self.get_destination(dst), r1, r2)
            }
            mir::Instruction::MulI(v1, v2) => {
                let (r1, r2) = self.get_binop(funcproto, v1, v2);
                VmInstruction::MulI(self.get_destination(dst), r1, r2)
            }
            mir::Instruction::DivI(v1, v2) => {
                let (r1, r2) = self.get_binop(funcproto, v1, v2);
                VmInstruction::DivI(self.get_destination(dst), r1, r2)
            }
            mir::Instruction::ModI(v1,v2)=> {
                let (r1, r2) = self.get_binop(funcproto, v1, v2);
                VmInstruction::ModI(self.get_destination(dst), r1, r2)
            }
            _=>{
                unimplemented!()
            }
        }
    }
    fn generate_funcproto(&mut self, mirfunc: &mir::Function) -> (String, vm::FuncProto) {
        // println!("generating function {}", mirfunc.label.0);
        let mut func = vm::FuncProto::from(mirfunc);
        self.vregister.reset();
        for a in mirfunc.args.iter() {
            self.vregister.add_newvalue(a.clone());
        }
        mirfunc.body.iter().for_each(|block| {
            block.0.iter().for_each(|(dst, inst)| {
                let newinst = self.emit_instruction(&mut func, dst.clone(), inst);
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

    #[test]

    fn build() {
        use super::*;
        use crate::types::Type;
        use mir::Label;

        let mut src = mir::Mir::default();
        let arg = Arc::new(mir::Value::Argument(
            0,
            Arc::new(mir::Argument(Label("hoge".to_string()), Type::Unknown)),
        ));
        let mut func = mir::Function::new("test", &[arg.clone()]);
        let mut block = mir::Block::default();
        let resint = Arc::new(mir::Value::Register(1));
        block.0.push((resint.clone(), mir::Instruction::Integer(1)));
        let res = Arc::new(mir::Value::Register(2));
        block
            .0
            .push((res.clone(), mir::Instruction::AddF(arg, resint)));
        block.0.push((
            Arc::new(mir::Value::None),
            mir::Instruction::Return(res.clone()),
        ));
        func.body = vec![block];
        src.functions.push(func);
        let mut generator = ByteCodeGenerator::default();
        let res = generator.generate(src);

        let mut answer = vm::Program::default();
        let mut main = vm::FuncProto::new(1, 1);

        main.constants.push(1);
        main.bytecodes = vec![
            VmInstruction::MoveConst(1, 0),
            VmInstruction::AddF(0, 0, 1),
            VmInstruction::Return(0, 1),
        ];
        answer.global_fn_table.push(("test".to_string(), main));
        assert_eq!(res, answer);
    }
}
