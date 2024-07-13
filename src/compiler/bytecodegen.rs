use std::borrow::Borrow;
use std::collections::HashMap;
use std::sync::Arc;

use crate::mir::{self, Mir};
use crate::runtime::vm::bytecode::{ConstPos, Reg};
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
    pub fn add_newvalue(&mut self, v: &Arc<mir::Value>) -> Reg {
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
    pub fn find(&mut self, v: &Arc<mir::Value>) -> Option<Reg> {
        // println!("find reg:{v} {:?}", self.0.as_slice()[0..10].to_vec());
        //todo: Error handling
        let res = self.0.iter().position(|v1| match v1 {
            Some(v1_c) => *v1_c == *v,
            _ => false,
        });
        match (res, v.as_ref()) {
            //argument is registered in absolute position
            (Some(pos), mir::Value::Argument(_, _)) => Some(pos as Reg),
            (Some(pos), _) => {
                //mir is SSA form, so the value will be used only once.
                self.0[pos] = None;
                Some(pos as Reg)
            }
            _ => None,
        }
    }
    //find for load instruction
    pub fn find_keep(&mut self, v: &Arc<mir::Value>) -> Option<Reg> {
        self.0
            .iter()
            .position(|v1| match v1 {
                Some(v1_c) => *v1_c == *v,
                _ => false,
            })
            .map(|pos| pos as Reg)
    }
    pub fn find_upvalue(&mut self, v: Arc<mir::Value>) -> Option<Reg> {
        // println!("find reg:{v} {:?}", self.0.as_slice()[0..10].to_vec());
        //todo: Error handling
        let res = self.0.iter().position(|v1| match v1 {
            Some(v1_c) => *v1_c == v,
            _ => false,
        });
        match (res, v.as_ref()) {
            //argument is registered in absolute position
            (Some(pos), mir::Value::Argument(_, _)) => Some(pos as Reg),
            (Some(pos), _) => Some(pos as Reg),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub struct VRegisterStack(Vec<VRegister>);
impl VRegisterStack {
    fn get_top_mut(&mut self) -> &mut VRegister {
        self.0.last_mut().unwrap()
    }
    fn add_newvalue(&mut self, v: &Arc<mir::Value>) -> Reg {
        self.get_top_mut().add_newvalue(v)
    }
    fn find(&mut self, v: &Arc<mir::Value>) -> Option<Reg> {
        // self.0.iter_mut().find(|vreg| vreg.find(v))
        todo!()
    }
}

#[derive(Debug, Default)]
pub struct ByteCodeGenerator {
    vregister: VRegister,
    bb_index: usize,
    fnmap: HashMap<String, usize>,
    program: vm::Program,
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
        let r1 = self.vregister.find(v1).unwrap();
        let r2 = self.vregister.find(v2).unwrap();
        (r1, r2)
    }
    fn get_destination(&mut self, dst: Arc<mir::Value>) -> Reg {
        self.vregister.add_newvalue(&dst)
    }
    fn find_upvalue(&mut self, fnid: usize, v: &Arc<mir::Value>) -> Reg {
        match v.as_ref() {
            mir::Value::Global(_) => todo!(),
            mir::Value::Argument(_, _) | mir::Value::Register(_) => {
                self.vregister.find_keep(v).unwrap()
            }
            mir::Value::UpValue(_f, upv) => {
                panic!()
                // self.find_upvalue(funcproto, upv)
            }
            _ => {
                unreachable!()
            }
        }
    }
    fn insert_move(
        &mut self,
        dst: &Arc<mir::Value>,
        src: &Arc<mir::Value>,
    ) -> Option<VmInstruction> {
        let d = self.vregister.find(dst).unwrap();
        let s = self.vregister.find(src).unwrap();
        (d != s).then(|| VmInstruction::Move(s, d))
    }
    fn prepare_function(
        &mut self,
        funcproto: &mut vm::FuncProto,
        faddress: Reg,
        args: &[Arc<mir::Value>],
    ) -> Reg {
        let mut adsts = vec![];
        let dst = faddress;
        for (i, a) in args.iter().enumerate() {
            let src = self.vregister.find(a).unwrap();
            let adst = dst as usize + i + 1;
            adsts.push((adst, src))
        }
        for (adst, src) in adsts.iter() {
            if *adst as Reg != *src {
                funcproto
                    .bytecodes
                    .push(VmInstruction::Move(*adst as Reg, *src));
            }
        }
        dst
    }
    fn emit_instruction(
        &mut self,
        funcproto: &mut vm::FuncProto,
        mirfunc: &mir::Function,
        dst: Arc<mir::Value>,
        mirinst: &mir::Instruction,
    ) -> Option<VmInstruction> {
        match mirinst {
            mir::Instruction::Uinteger(u) => {
                let pos = funcproto.add_new_constant(*u);
                Some(VmInstruction::MoveConst(
                    self.get_destination(dst),
                    pos as ConstPos,
                ))
            }
            mir::Instruction::Integer(i) => {
                let pos = funcproto.add_new_constant(gen_raw_int(i));
                Some(VmInstruction::MoveConst(
                    self.get_destination(dst),
                    pos as ConstPos,
                ))
            }
            mir::Instruction::Float(n) => {
                let pos = funcproto.add_new_constant(gen_raw_float(n));
                Some(VmInstruction::MoveConst(
                    self.get_destination(dst),
                    pos as ConstPos,
                ))
            }
            mir::Instruction::Alloc(_t) => {
                let _ = self.get_destination(dst);
                None
            }
            mir::Instruction::Load(ptr) => {
                let d = self.get_destination(dst);
                let s = self.vregister.find_keep(ptr).unwrap();
                (d != s).then(|| VmInstruction::Move(d, s))
            }
            mir::Instruction::Store(dst, src) => {
                let d = self.vregister.find_keep(dst).unwrap();
                let s = self.vregister.find(src).unwrap();
                (d != s).then(|| VmInstruction::Move(d, s))
            }
            mir::Instruction::Call(v, args) => {
                let nargs = args.len() as u8;
                let res = match v.as_ref() {
                    mir::Value::Register(_address) => {
                        let faddress = self.vregister.find(v).unwrap();
                        let fadd = self.prepare_function(funcproto, faddress, args);
                        let dst = self.get_destination(dst);
                        funcproto
                            .bytecodes
                            .push(VmInstruction::Call(fadd, nargs, 1));
                        if dst != fadd {
                            Some(VmInstruction::Move(dst, fadd))
                        } else {
                            None
                        }
                    }
                    mir::Value::Function(_idx, _state_size) => {
                        unreachable!();
                    }
                    mir::Value::ExtFunction(_idx) => {
                        todo!()
                        // VmInstruction::CallExtFun(idx as Reg, nargs, 1)
                    }
                    mir::Value::Closure(_cls, _reg) => {
                        todo!()
                        // VmInstruction::CallCls(reg as Reg, nargs, 1)
                    }
                    mir::Value::FixPoint => todo!(),
                    _ => unreachable!(),
                };
                res
            }
            mir::Instruction::CallCls(f, args) => {
                let nargs = args.len() as u8;
                match f.as_ref() {
                    mir::Value::Register(_address) => {
                        let faddress = self.vregister.find(f).unwrap();
                        let fadd = self.prepare_function(funcproto, faddress, args);
                        funcproto
                            .bytecodes
                            .push(VmInstruction::CallCls(fadd, nargs, 1));
                        let dst = self.get_destination(dst);
                        Some(VmInstruction::Move(dst, fadd))
                    }
                    mir::Value::Function(idx, state_size) => {
                        unreachable!();
                    }
                    mir::Value::ExtFunction(_idx) => {
                        todo!()
                        // VmInstruction::CallExtFun(idx as Reg, nargs, 1)
                    }
                    mir::Value::Closure(_cls, _reg) => {
                        todo!()
                        // VmInstruction::CallCls(reg as Reg, nargs, 1)
                    }
                    mir::Value::FixPoint => todo!(),
                    _ => unreachable!(),
                }
            }
            mir::Instruction::Closure(idxcell) => {
                let dst = self.get_destination(dst);
                let idx = self.vregister.find(idxcell).unwrap();
                Some(VmInstruction::Closure(dst, idx))
            }
            mir::Instruction::GetUpValue(_f, i) => {
                // let fnidx = self.fnmap.get(f).unwrap();
                Some(VmInstruction::GetUpValue(
                    self.get_destination(dst),
                    *i as Reg,
                ))
            }
            mir::Instruction::SetUpValue(_f, i) => todo!(),
            mir::Instruction::PushStateOffset(v) => Some(VmInstruction::ShiftStatePos(*v as i16)),
            mir::Instruction::PopStateOffset(v) => Some(VmInstruction::ShiftStatePos(-(*v as i16))),
            mir::Instruction::GetState => Some(VmInstruction::GetState(self.get_destination(dst))),

            mir::Instruction::JmpIf(cond, tbb, ebb) => {
                let c = self.vregister.find(cond).unwrap();
                let mut then_bytecodes: Vec<VmInstruction> = vec![];
                let mut else_bytecodes: Vec<VmInstruction> = vec![];

                mirfunc.body[*tbb as usize]
                    .0
                    .iter()
                    .for_each(|(dst, t_inst)| {
                        if let Some(inst) =
                            self.emit_instruction(funcproto, mirfunc, dst.clone(), t_inst)
                        {
                            then_bytecodes.push(inst);
                        }
                    });
                let else_offset = then_bytecodes.len() + 3; //add offset to jmp inst and loading phi

                mirfunc.body[*ebb as usize]
                    .0
                    .iter()
                    .for_each(|(dst, t_inst)| {
                        if let Some(inst) =
                            self.emit_instruction(funcproto, mirfunc, dst.clone(), t_inst)
                        {
                            else_bytecodes.push(inst);
                        };
                    });
                let (phidst, pinst) = mirfunc.body[(*ebb + 1) as usize].0.first().unwrap();
                let phi = self.vregister.add_newvalue(phidst);
                if let mir::Instruction::Phi(t, e) = pinst {
                    let t = self.vregister.find(t).unwrap();
                    then_bytecodes.push(VmInstruction::Move(phi, t));
                    let e = self.vregister.find(e).unwrap();
                    else_bytecodes.push(VmInstruction::Move(phi, e));
                } else {
                    unreachable!();
                }
                funcproto
                    .bytecodes
                    .push(VmInstruction::JmpIfNeg(c, else_offset as i16));

                let ret_offset = else_bytecodes.len() + 1;

                then_bytecodes.push(VmInstruction::Jmp(ret_offset as i16));

                funcproto.bytecodes.append(&mut then_bytecodes);
                funcproto.bytecodes.append(&mut else_bytecodes);

                Some(VmInstruction::Return(phi, 1))
            }
            mir::Instruction::Jmp(offset) => Some(VmInstruction::Jmp(*offset)),
            mir::Instruction::Phi(_, _) => {
                unreachable!()
            }
            mir::Instruction::Return(v) => {
                Some(VmInstruction::Return(self.vregister.find(v).unwrap(), 1))
            }
            mir::Instruction::ReturnFeed(new) => {
                let old = self.vregister.add_newvalue(&dst);
                funcproto.bytecodes.push(VmInstruction::GetState(old));
                let new = self.vregister.find(new).unwrap();
                funcproto.bytecodes.push(VmInstruction::SetState(new));
                Some(VmInstruction::Return(old, 1))
            }
            mir::Instruction::AddF(v1, v2) => {
                let (r1, r2) = self.get_binop(funcproto, v1, v2);
                Some(VmInstruction::AddF(self.get_destination(dst), r1, r2))
            }
            mir::Instruction::SubF(v1, v2) => {
                let (r1, r2) = self.get_binop(funcproto, v1, v2);
                Some(VmInstruction::SubF(self.get_destination(dst), r1, r2))
            }
            mir::Instruction::MulF(v1, v2) => {
                let (r1, r2) = self.get_binop(funcproto, v1, v2);
                Some(VmInstruction::MulF(self.get_destination(dst), r1, r2))
            }
            mir::Instruction::DivF(v1, v2) => {
                let (r1, r2) = self.get_binop(funcproto, v1, v2);
                Some(VmInstruction::DivF(self.get_destination(dst), r1, r2))
            }
            mir::Instruction::ModF(v1, v2) => {
                let (r1, r2) = self.get_binop(funcproto, v1, v2);
                Some(VmInstruction::ModF(self.get_destination(dst), r1, r2))
            }
            mir::Instruction::SinF(v1) => {
                let src = self.vregister.find(v1).unwrap();
                Some(VmInstruction::SinF(self.get_destination(dst), src))
            }
            mir::Instruction::CosF(v1) => {
                let src = self.vregister.find(v1).unwrap();
                Some(VmInstruction::CosF(self.get_destination(dst), src))
            }
            mir::Instruction::AbsF(v1) => {
                let src = self.vregister.find(v1).unwrap();
                Some(VmInstruction::AbsF(self.get_destination(dst), src))
            }
            mir::Instruction::SqrtF(v1) => {
                let src = self.vregister.find(v1).unwrap();
                Some(VmInstruction::SqrtF(self.get_destination(dst), src))
            }
            mir::Instruction::AddI(v1, v2) => {
                let (r1, r2) = self.get_binop(funcproto, v1, v2);
                Some(VmInstruction::AddI(self.get_destination(dst), r1, r2))
            }
            mir::Instruction::SubI(v1, v2) => {
                let (r1, r2) = self.get_binop(funcproto, v1, v2);
                Some(VmInstruction::SubI(self.get_destination(dst), r1, r2))
            }
            mir::Instruction::MulI(v1, v2) => {
                let (r1, r2) = self.get_binop(funcproto, v1, v2);
                Some(VmInstruction::MulI(self.get_destination(dst), r1, r2))
            }
            mir::Instruction::DivI(v1, v2) => {
                let (r1, r2) = self.get_binop(funcproto, v1, v2);
                Some(VmInstruction::DivI(self.get_destination(dst), r1, r2))
            }
            mir::Instruction::ModI(v1, v2) => {
                let (r1, r2) = self.get_binop(funcproto, v1, v2);
                Some(VmInstruction::ModI(self.get_destination(dst), r1, r2))
            }
            _ => {
                unimplemented!()
            }
        }
    }
    fn generate_funcproto(&mut self, mirfunc: &mir::Function) -> (String, vm::FuncProto) {
        // println!("generating function {}", mirfunc.label.0);
        let mut func = vm::FuncProto::from(mirfunc);
        self.vregister.reset();
        for a in mirfunc.args.iter() {
            self.vregister.add_newvalue(a);
        }
        // succeeding block will be compiled recursively
        let block = &mirfunc.body[0];
        block.0.iter().enumerate().for_each(|(i, (dst, inst))| {
            let newinst = self.emit_instruction(&mut func, mirfunc, dst.clone(), inst);
            if let Some(i) = newinst {
                func.bytecodes.push(i);
            }
        });
        (mirfunc.label.0.clone(), func)
    }
    pub fn generate(&mut self, mir: Mir) -> vm::Program {
        self.program.global_fn_table = mir
            .functions
            .iter()
            .enumerate()
            .map(|(i, func)| {
                let label = &func.label.0;
                self.fnmap.insert(label.clone(), i);
                self.generate_funcproto(func)
            })
            .collect();

        self.program.clone()
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
        // fn test(hoge){
        //   hoge+1
        //}
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
            VmInstruction::AddF(1, 0, 1),
            VmInstruction::Return(1, 1),
        ];
        answer.global_fn_table.push(("test".to_string(), main));
        assert_eq!(res, answer);
    }
}
