use std::{cell::RefCell, cmp::Ordering, collections::HashMap, rc::Rc, sync::Arc, sync::Mutex};
pub mod bytecode;
pub mod program;
use bytecode::*;
use program::Closure;
pub use program::{FuncProto, Program, UpIndex, UpValue};

pub type RawVal = u64;
pub type ReturnCode = i64;

pub type ExtFunType = fn(&mut Machine) -> ReturnCode;
pub type ExtClsType = Arc<Mutex<dyn FnMut(&mut Machine) -> ReturnCode>>;

pub struct Machine {
    stack: Vec<RawVal>,
    base_pointer: u64,
    closures: Vec<Closure>,
    pub ext_fun_table: Vec<(String, ExtFunType)>,
    fn_map: HashMap<usize, usize>, //index from fntable index of program to it of machine.
    pub ext_cls_table: Vec<(String, ExtClsType)>,
    cls_map: HashMap<usize, usize>, //index from fntable index of program to it of machine.
    internal_states: Vec<f64>,
    state_idx: usize,
}

macro_rules! binop {
    ($op:tt,$t:ty, $dst:expr,$src1:expr,$src2:expr,$self:ident) => {
        $self.set_stack($dst as i64, Self::to_value::<$t>(
            Self::get_as::<$t>($self.get_stack($src1 as i64))
        $op Self::get_as::<$t>($self.get_stack($src2 as i64))))
    };
}
macro_rules! uniop {
    ($op:tt,$t:ty, $dst:expr,$src:expr,$self:ident) => {
        $self.set_stack($dst as i64,
            Self::to_value::<$t>(
            $op Self::get_as::<$t>($self.get_stack($src as i64))))
    };
}

fn set_vec(vec: &mut Vec<RawVal>, i: usize, value: RawVal) {
    match i.cmp(&vec.len()) {
        Ordering::Less => vec[i] = value,
        Ordering::Equal => vec.push(value),
        Ordering::Greater => {
            vec.resize(i, 0u64);
            vec.push(value);
        }
    }
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct FeedState {
    pub feed: Option<RawVal>,
    pub delays: Vec<Vec<RawVal>>, //vector of ring buffer
    pub calltree: Vec<FeedState>,
}

impl Machine {
    pub fn new() -> Self {
        Self {
            stack: vec![],
            base_pointer: 0,
            closures: vec![],
            ext_fun_table: vec![],
            ext_cls_table: vec![],
            fn_map: HashMap::new(),
            cls_map: HashMap::new(),
            internal_states: vec![],
            state_idx: 0,
        }
    }
    fn get_stack(&self, offset: i64) -> RawVal {
        self.stack[(self.base_pointer + offset as u64) as usize]
    }

    fn set_stack(&mut self, offset: i64, v: RawVal) {
        set_vec(
            &mut self.stack,
            (self.base_pointer as i64 + offset) as usize,
            v,
        );
    }
    pub fn get_top(&self) -> &RawVal {
        self.stack.last().unwrap()
    }
    fn return_general(
        &mut self,
        iret: Reg,
        nret: Reg,
        local_upvalues: &mut Vec<Rc<RefCell<UpValue>>>,
    ) -> &[u64] {
        self.close_upvalues(local_upvalues);
        let base = self.base_pointer as usize;
        let iret_abs = base + iret as usize;
        self.stack
            .copy_within(iret_abs..(iret_abs + nret as usize), base - 1);
        // clean up temporary variables to ensure that `nret`
        // at the top of the stack is the return value
        self.stack.truncate(base - 1 as usize + nret as usize);
        let res_slice = self.stack.split_at(base as usize).1;
        res_slice
    }

    pub(crate) fn get_as<T>(v: RawVal) -> T {
        unsafe { std::mem::transmute_copy::<RawVal, T>(&v) }
    }
    pub(crate) fn to_value<T>(v: T) -> RawVal {
        unsafe { std::mem::transmute_copy::<T, RawVal>(&v) }
    }
    fn call_function<F>(&mut self, func_pos: u8, nargs: u8, nret_req: u8, mut action: F)
    where
        F: FnMut(&mut Self) -> ReturnCode,
    {
        let offset = (func_pos + 1) as u64;

        self.base_pointer += offset;
        let nret = action(self) as u8;

        if nret_req > nret {
            panic!("invalid number of return value required.");
        }
        // shrink stack so as to match with number of return values
        self.stack
            .truncate((self.base_pointer as i64 + nret_req as i64) as usize);
        self.base_pointer -= offset;
    }
    fn close_upvalues(&self, broker: &mut Vec<Rc<RefCell<UpValue>>>) {
        for v in broker.iter_mut() {
            let mut v = v.borrow_mut();
            if let UpValue::Open(i) = *v {
                *v = UpValue::Closed(self.get_stack(i.into()));
            }
        }
    }
    /// Execute function, return retcode.
    pub fn execute(&mut self, func_i: usize, prog: &Program, cls_i: Option<usize>) -> ReturnCode {
        let (_fname, func) = &prog.global_fn_table[func_i];
        let mut local_upvalues = Vec::<Rc<RefCell<UpValue>>>::new();
        let mut pcounter = 0;
        if cfg!(test) {
            println!("{:?}", func);
        }
        loop {
            if cfg!(debug_assertions) || cfg!(test) {
                print!("{: <20} {}", func.bytecodes[pcounter], ": [");
                for i in 0..self.stack.len() {
                    if i == self.base_pointer as usize {
                        print!("!");
                    }
                    print!("{:?}", self.stack[i]);
                    if i < self.stack.len() - 1 {
                        print!(", ");
                    }
                }
                println!("]");
            }

            match func.bytecodes[pcounter] {
                Instruction::Move(dst, src) => {
                    self.set_stack(dst as i64, self.get_stack(src as i64));
                }
                Instruction::MoveConst(dst, pos) => {
                    self.set_stack(dst as i64, func.constants[pos as usize]);
                }
                Instruction::CallCls(func, nargs, nret_req) => {
                    let addr = self.get_stack(func as i64);
                    let cls_i = Self::get_as::<usize>(addr);
                    let cls = &self.closures[cls_i];
                    let pos_of_f = cls.fn_proto_pos;

                    self.call_function(func, nargs, nret_req, move |machine| {
                        machine.execute(pos_of_f, prog, Some(cls_i))
                    });
                }
                Instruction::Call(func, nargs, nret_req) => {
                    let pos_of_f = Self::get_as::<usize>(self.get_stack(func as i64));
                    self.call_function(func, nargs, nret_req, move |machine| {
                        machine.execute(pos_of_f, prog, None)
                    });
                }
                Instruction::CallExtFun(func, nargs, nret_req) => {
                    let ext_fn_idx = self.get_stack(func as i64) as usize;
                    let f = self.ext_fun_table[ext_fn_idx].1;
                    self.call_function(func, nargs, nret_req, move |machine| f(machine));
                }
                Instruction::CallExtCls(func, nargs, nret_req) => {
                    // todo: load closure index via constant for the case of more than 255 closures in program
                    // let cls_idx = self
                    //     .cls_map
                    //     .get(&(self.get_stack(func as i64) as usize))
                    //     .expect("closure map not resolved.");
                    let (_name, cls_mutex) = self.ext_cls_table[func as usize].clone();
                    self.call_function(func, nargs, nret_req, move |machine| {
                        if let Ok(mut cls) = cls_mutex.lock() {
                            cls(machine)
                        } else {
                            0
                        }
                    });
                }
                Instruction::Closure(dst, fn_index) => {
                    let fn_proto_pos = self.get_stack(fn_index as i64) as usize;
                    let (name, f_proto) = &prog.global_fn_table[fn_proto_pos];

                    let inner_upvalues: Vec<Rc<RefCell<UpValue>>> = f_proto
                        .upindexes
                        .iter()
                        .map(|u_i| {
                            match u_i {
                                UpIndex::Local(i) => {
                                    let res = Rc::new(RefCell::new(UpValue::Open(*i as u8)));
                                    local_upvalues.push(res.clone());
                                    res
                                }
                                UpIndex::Upvalue(u_i) => {
                                    let up_i = cls_i.unwrap();
                                    let upvalues = &self.closures[up_i].upvalues;
                                    //clone
                                    Rc::clone(&upvalues[*u_i])
                                }
                            }
                        })
                        .collect();

                    //todo! garbage collection
                    self.closures.push(Closure {
                        fn_proto_pos,
                        upvalues: inner_upvalues,
                    });

                    let vaddr = self.closures.len() - 1;
                    self.set_stack(dst as i64, Self::to_value(vaddr));
                }
                Instruction::Return0 => {
                    return 0;
                }
                Instruction::Return(iret, nret) => {
                    let _ = self.return_general(iret, nret, &mut local_upvalues);
                    return nret.into();
                }
                Instruction::GetUpValue(dst, index) => {
                    let rawv = {
                        let up_i = cls_i.unwrap();
                        let upvalues = &self.closures[up_i].upvalues;
                        let rv: &UpValue = &upvalues[index as usize].borrow();
                        match rv {
                            UpValue::Open(i) => self.get_stack(*i as i64),
                            UpValue::Closed(rawval) => *rawval,
                        }
                    };
                    self.set_stack(dst as i64, rawv);
                }
                Instruction::SetUpValue(index, src) => {
                    let v = self.get_stack(src as i64);
                    let up_i = cls_i.unwrap();
                    let upvalues = &self.closures[up_i].upvalues;
                    let res = {
                        let rv: &mut UpValue = &mut upvalues[index as usize].borrow_mut();
                        match rv {
                            UpValue::Open(i) => Some(*i),
                            UpValue::Closed(i) => {
                                *i = v;
                                // *rv = UpValue::Closed(v);
                                None
                            }
                        }
                    };
                    if let Some(i) = res {
                        self.set_stack(i as i64, v);
                    }
                }
                // Instruction::Close() => todo!(),
                Instruction::Jmp(offset) => {
                    pcounter = (pcounter as isize + offset as isize) as usize;
                }
                Instruction::JmpIfNeg(cond, offset) => {
                    let cond_v = self.get_stack(cond as i64);
                    if Self::get_as::<bool>(cond_v) {
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
                    self.set_stack(
                        dst as i64,
                        Self::to_value::<f64>(
                            Self::get_as::<f64>(self.get_stack(src as i64)).abs(),
                        ),
                    );
                }
                Instruction::SinF(dst, src) => {
                    self.set_stack(
                        dst as i64,
                        Self::to_value::<f64>(
                            Self::get_as::<f64>(self.get_stack(src as i64)).sin(),
                        ),
                    );
                }
                Instruction::CosF(dst, src) => {
                    self.set_stack(
                        dst as i64,
                        Self::to_value::<f64>(
                            Self::get_as::<f64>(self.get_stack(src as i64)).cos(),
                        ),
                    );
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
                    self.set_stack(
                        dst as i64,
                        Self::to_value::<i64>(
                            Self::get_as::<i64>(self.get_stack(src as i64)).abs(),
                        ),
                    );
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
                Instruction::CastFtoI(dst, src) => self.set_stack(
                    dst as i64,
                    Self::to_value::<i64>(Self::get_as::<f64>(self.get_stack(src as i64)) as i64),
                ),
                Instruction::CastItoF(dst, src) => self.set_stack(
                    dst as i64,
                    Self::to_value::<f64>(Self::get_as::<i64>(self.get_stack(src as i64)) as f64),
                ),
                Instruction::CastItoB(dst, src) => self.set_stack(
                    dst as i64,
                    Self::to_value::<bool>(Self::get_as::<i64>(self.get_stack(src as i64)) != 0),
                ),
                Instruction::GetState(dst) => {
                    let v = self.internal_states[self.state_idx];
                    self.set_stack(dst as i64, Self::to_value(v));
                }
                Instruction::SetState(src) => {
                    let v = self.get_stack(src as i64);
                    self.internal_states[self.state_idx] = Self::get_as::<f64>(v)
                }
                Instruction::ShiftStatePos(_) => todo!(),
            }
            pcounter += 1;
        }
    }
    pub fn install_extern_fn(&mut self, name: String, f: ExtFunType) {
        self.ext_fun_table.push((name, f));
    }
    pub fn install_extern_cls(&mut self, name: String, f: ExtClsType) {
        self.ext_cls_table.push((name, f));
    }
    pub fn link_functions(&mut self, prog: &Program) {
        //link external functions
        prog.ext_fun_table
            .iter()
            .enumerate()
            .for_each(|(i, (name, _ty))| {
                if let Some((j, _)) = self
                    .ext_fun_table
                    .iter()
                    .enumerate()
                    .find(|(j, (fname, _fn))| name == fname)
                {
                    self.fn_map.insert(i, j);
                } else {
                    panic!("external function {} cannot be found", name);
                };
            });
        prog.ext_cls_table
            .iter()
            .enumerate()
            .for_each(|(i, (name, _ty))| {
                if let Some((j, _)) = self
                    .ext_cls_table
                    .iter()
                    .enumerate()
                    .find(|(j, (fname, _fn))| name == fname)
                {
                    self.cls_map.insert(i, j);
                } else {
                    panic!("external closure {} cannot be found", name);
                };
            });
    }
    pub fn execute_entry(&mut self, prog: &Program, entry: &str) -> ReturnCode {
        if let Some((i, (_name, func))) = prog
            .global_fn_table
            .iter()
            .enumerate()
            .find(|(i, (name, _))| name == entry)
        {
            self.internal_states.resize(func.state_size as usize, 0.0);
            // 0 is always base pointer to the main function
            if self.stack.len() > 0 {
                self.stack[0] = 0;
            }
            self.base_pointer = 1;
            self.execute(i, &prog, None)
        } else {
            -1
        }
    }
    pub fn execute_main(&mut self, prog: &Program) -> ReturnCode {
        //internal function table 0 is always mimium_main
        self.internal_states
            .resize(prog.global_fn_table[0].1.state_size as usize, 0.0);
        // 0 is always base pointer to the main function
        self.base_pointer += 1;
        self.execute(0, &prog, None)
    }
}

#[cfg(test)]
mod test;
