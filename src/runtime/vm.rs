use std::{cell::RefCell, rc::Rc};

use super::bytecode::{ConstPos, Instruction, Reg};

type RawVal = u64;
type ReturnCode = i64;

type ExtFunType = fn(&mut Machine) -> ReturnCode;

pub struct Machine {
    stack: Vec<RawVal>,
    base_pointer: u64,
}

#[derive(Debug, Default)]
pub struct FuncProto {
    pub nparam: usize,
    pub upindexes: Vec<usize>,
    pub bytecodes: Vec<Instruction>,
    pub constants: Vec<RawVal>,
}

pub enum UpValue {
    Open(Reg),
    Closed(RawVal),
}
pub(crate) struct Closure {
    fn_proto_pos: usize, //position of function prototype in global_ftable
    upvalues: Vec<Rc<RefCell<UpValue>>>,
}

pub struct Program {
    pub global_fn_table: Vec<FuncProto>,
    pub ext_fun_table: Vec<ExtFunType>,
}

macro_rules! binop {
    ($op:tt,$t:ty, $dst:expr,$src1:expr,$src2:expr,$self:ident) => {
        *$self.get_stack_mut($dst) =
            $self.to_value::<$t>(
                $self.get_as::<$t>($self.get_stack($src1))
            $op $self.get_as::<$t>($self.get_stack($src2)))
    };
}
macro_rules! uniop {
    ($op:tt,$t:ty, $dst:expr,$src:expr,$self:ident) => {
        *$self.get_stack_mut($dst) =
            $self.to_value::<$t>(
            $op $self.get_as::<$t>($self.get_stack($src)))
    };
}

impl Machine {
    fn get_stack(&self, offset: Reg) -> RawVal {
        self.stack[(self.base_pointer + offset as u64) as usize]
    }
    fn get_stack_mut(&mut self, offset: Reg) -> &mut RawVal {
        unsafe {
            self.stack
                .get_unchecked_mut((self.base_pointer + offset as u64) as usize)
        }
    }
    fn get_current_func(&mut self) -> std::rc::Rc<FuncProto> {
        let ptr = self.get_as::<*const FuncProto>(self.stack[(self.base_pointer - 1) as usize]);
        unsafe { std::rc::Rc::from_raw(ptr) }
    }
    fn get_top(&self) -> &RawVal {
        self.stack.last().unwrap()
    }
    fn get_as<T>(&self, v: RawVal) -> T {
        unsafe { std::mem::transmute_copy::<RawVal, T>(&v) }
    }
    fn to_value<T>(&self, v: T) -> RawVal {
        unsafe { std::mem::transmute_copy::<T, RawVal>(&v) }
    }
    fn call_function<F>(&mut self, func_pos: u8, nargs: u8, nret_req: u8, mut action: F)
    where
        F: FnMut(&mut Self) -> ReturnCode,
    {
        self.base_pointer += func_pos as u64 + 1;
        let nret = action(self) as u8;
        //now return value is stored from base+nargs, move return value to base
        for i in 0..nret {
            *self.get_stack_mut(i - 1) = self.get_stack(nargs + i)
        }
        if nret_req > nret {
            panic!("invalid number of return value required.");
        }
        // shrink stack so as to match with number of return values
        self.stack
            .truncate((self.base_pointer + nret_req as u64) as usize);
        self.base_pointer -= func_pos as u64 + 1;
    }
    /// Execute function, return retcode.
    pub fn execute(
        &mut self,
        func_i: usize,
        prog: &Program,
        upvalues: &Vec<Rc<RefCell<UpValue>>>,
    ) -> ReturnCode {
        let func = &prog.global_fn_table[func_i];
        let mut pcounter = 0;

        loop {
            match func.bytecodes[pcounter] {
                Instruction::Move(dst, src) => {
                    *self.get_stack_mut(dst) = self.get_stack(src);
                }
                Instruction::MoveConst(dst, pos) => {
                    *self.get_stack_mut(dst) = func.constants[pos as usize];
                }
                Instruction::CallCls(func, nargs, nret_req) => {
                    let cls = self.get_as::<Rc<RefCell<Closure>>>(self.get_stack(func));
                    let pos_of_f = cls.borrow().fn_proto_pos;
                    self.call_function(func, nargs, nret_req, |machine| {
                        machine.execute(pos_of_f, prog, &cls.borrow().upvalues)
                    });
                }
                Instruction::Call(func, nargs, nret_req) => {
                    let pos_of_f = self.get_as::<usize>(self.get_stack(func));
                    // let f = prog.global_fn_table[pos_of_f];
                    self.call_function(pos_of_f as u8, nargs, nret_req, |machine| {
                        machine.execute(pos_of_f, prog, &vec![])
                    });
                }
                Instruction::CallExtFun(func, nargs, nret_req) => {
                    let f = self.get_as::<ExtFunType>(self.get_stack(func));
                    self.call_function(func, nargs, nret_req, |machine| f(machine));
                }
                Instruction::Closure(dst, fn_index) => {
                    let c = Closure {
                        fn_proto_pos: fn_index as usize,
                        upvalues: vec![],
                    };
                    // is the reference count really work for shis?
                    *self.get_stack_mut(dst) = self.to_value(Rc::new(RefCell::new(c)));
                }
                Instruction::Return0 => {
                    return 0;
                }
                Instruction::Return(iret, nret) => {
                    // convert relative address to absolute address
                    let iret = self.base_pointer as usize + iret as usize;

                    // clean up temporary variables to ensure that `nret`
                    // at the top of the stack is the return value
                    self.stack.truncate(iret + nret as usize);
                    return nret.into();
                }
                Instruction::GetUpValue(dst, index) => {
                    let rv: &UpValue = &upvalues[index as usize].borrow();
                    let rawv: RawVal = match rv {
                        UpValue::Open(i) => self.stack[*i as usize],
                        UpValue::Closed(rawval) => *rawval,
                    };
                    *self.get_stack_mut(dst) = rawv;
                }
                Instruction::SetUpValue(src, index) => {
                    let rv: &mut UpValue = &mut upvalues[index as usize].borrow_mut();
                    match rv {
                        UpValue::Open(i) => {
                            *self.get_stack_mut(*i) = self.get_stack(src);
                        }
                        UpValue::Closed(_) => {
                            *rv = UpValue::Closed(self.get_stack(src));
                        }
                    };
                }
                // Instruction::Close() => todo!(),
                Instruction::Feed() => todo!(),
                Instruction::Jmp(offset) => {
                    pcounter = (pcounter as isize + offset as isize) as usize;
                }
                Instruction::JmpIf(cond, offset) => {
                    let cond_v = self.get_stack(cond);
                    if self.get_as::<bool>(cond_v) {
                        pcounter = (pcounter as isize + offset as isize) as usize;
                    }
                }
                Instruction::AddF(dst, src1, src2) => {
                    binop!(+,f64,dst,src1,src2,self)
                }
                Instruction::SubF(dst, src1, src2) => {
                    binop!(-,f64,dst,src1,src2,self)
                }
                Instruction::MulF(dst, src1, src2) => {
                    binop!(*,f64,dst,src1,src2,self)
                }
                Instruction::DivF(dst, src1, src2) => {
                    binop!(/,f64,dst,src1,src2,self)
                }
                Instruction::ModF(dst, src1, src2) => {
                    binop!(%,f64,dst,src1,src2,self)
                }
                Instruction::NegF(dst, src) => {
                    uniop!(-,i64,dst,src,self)
                }
                Instruction::AbsF(dst, src) => {
                    *self.get_stack_mut(dst) =
                        self.to_value::<f64>(self.get_as::<f64>(self.get_stack(src)).abs())
                }
                Instruction::SinF(dst, src) => {
                    *self.get_stack_mut(dst) =
                        self.to_value::<f64>(self.get_as::<f64>(self.get_stack(src)).sin())
                }
                Instruction::CosF(dst, src) => {
                    *self.get_stack_mut(dst) =
                        self.to_value::<f64>(self.get_as::<f64>(self.get_stack(src)).cos())
                }
                Instruction::PowF(_, _, _) => todo!(),
                Instruction::LogF(_, _, _) => todo!(),
                Instruction::AddI(dst, src1, src2) => {
                    binop!(+,i64,dst,src1,src2,self)
                }
                Instruction::SubI(dst, src1, src2) => {
                    binop!(-,i64,dst,src1,src2,self)
                }
                Instruction::MulI(dst, src1, src2) => {
                    binop!(*,i64,dst,src1,src2,self)
                }
                Instruction::DivI(dst, src1, src2) => {
                    binop!(/,i64,dst,src1,src2,self)
                }
                Instruction::ModI(dst, src1, src2) => {
                    binop!(%,i64,dst,src1,src2,self)
                }
                Instruction::NegI(dst, src) => {
                    uniop!(-,i64,dst,src,self)
                }
                Instruction::AbsI(dst, src) => {
                    *self.get_stack_mut(dst) =
                        self.to_value::<i64>(self.get_as::<i64>(self.get_stack(src)).abs())
                }
                Instruction::PowI(_, _) => todo!(),
                Instruction::LogI(_, _, _) => todo!(),
                Instruction::Not(dst, src) => {
                    uniop!(!, bool, dst, src, self)
                }
                Instruction::Eq(dst, src1, src2) => {
                    binop!(==,bool,dst,src1,src2,self)
                }
                Instruction::Ne(dst, src1, src2) => {
                    binop!(!=,bool,dst,src1,src2,self)
                }
                Instruction::Gt(dst, src1, src2) => {
                    binop!(>,bool,dst,src1,src2,self)
                }
                Instruction::Ge(dst, src1, src2) => {
                    binop!(>=,bool,dst,src1,src2,self)
                }
                Instruction::Lt(dst, src1, src2) => {
                    binop!(<,bool,dst,src1,src2,self)
                }
                Instruction::Le(dst, src1, src2) => {
                    binop!(<=,bool,dst,src1,src2,self)
                }
                Instruction::And(dst, src1, src2) => {
                    binop!(&&,bool,dst,src1,src2,self)
                }
                Instruction::Or(dst, src1, src2) => {
                    binop!(||,bool,dst,src1,src2,self)
                }
                Instruction::CastFtoI(dst, src) => {
                    *self.get_stack_mut(dst) =
                        self.to_value::<i64>(self.get_as::<f64>(self.get_stack(src)) as i64)
                }
                Instruction::CastItoF(dst, src) => {
                    *self.get_stack_mut(dst) =
                        self.to_value::<f64>(self.get_as::<i64>(self.get_stack(src)) as f64)
                }
                Instruction::CastItoB(dst, src) => {
                    *self.get_stack_mut(dst) =
                        self.to_value::<bool>(self.get_as::<i64>(self.get_stack(src)) != 0)
                }
            }
            pcounter += 1;
        }
    }
}

#[cfg(test)]
mod test;
