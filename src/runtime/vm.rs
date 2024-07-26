use std::{
    cell::RefCell,
    cmp::Ordering,
    collections::HashMap,
    rc::Rc,
    sync::{Arc, Mutex},
};
pub mod builtin;
pub mod bytecode;
pub mod program;
use bytecode::*;

use program::OpenUpValue;
pub use program::{FuncProto, Program};
pub type RawVal = u64;
pub type ReturnCode = i64;

pub type ExtFunType = fn(&mut Machine) -> ReturnCode;
pub type ExtClsType = Arc<Mutex<dyn FnMut(&mut Machine) -> ReturnCode>>;

#[derive(Debug, Default, PartialEq)]
struct StateStorage {
    pos: usize,
    data: Vec<f64>,
}
impl StateStorage {
    fn resize(&mut self, size: usize) {
        self.data.resize(size, 0.0)
    }
    fn get(&self) -> f64 {
        self.data[self.pos]
    }
    fn get_mut(&mut self) -> &mut f64 {
        unsafe { self.data.get_unchecked_mut(self.pos) }
    }
    fn shift_pos(&mut self, offset: i16) {
        self.pos = (self.pos as i64 + offset as i64) as usize;
    }
}

#[derive(Clone, Copy)]
pub struct ClosureIdx(pub u64);

#[derive(Clone, Default)]
struct StateStorageStack(Vec<ClosureIdx>);

impl StateStorageStack {
    pub fn push(&mut self, i: ClosureIdx) {
        self.0.push(i)
    }
    pub fn pop(&mut self) {
        let _ = self.0.pop();
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum UpValue {
    Open(OpenUpValue),
    Closed(Rc<RefCell<RawVal>>),
}
impl From<OpenUpValue> for UpValue {
    fn from(value: OpenUpValue) -> Self {
        Self::Open(value)
    }
}

#[derive(Debug, Default, PartialEq)]
//closure object dynamically allocated
pub(crate) struct Closure {
    pub fn_proto_pos: usize, //position of function prototype in global_ftable
    pub base_ptr: u64,       //base pointer to current closure, to calculate open upvalue
    pub upvalues: Vec<UpValue>,
    state_storage: StateStorage,
}
impl Closure {
    pub fn new(program: &Program, base_ptr: u64, fn_i: usize) -> Self {
        let fnproto = &program.global_fn_table[fn_i].1;
        let upvalues = fnproto
            .upindexes
            .iter()
            .map(|i| UpValue::Open(*i))
            .collect::<Vec<_>>();
        let mut state_storage = StateStorage::default();
        state_storage.resize(fnproto.state_size as usize);
        Self {
            fn_proto_pos: fn_i,
            upvalues,
            base_ptr,
            state_storage,
        }
    }
}

#[derive(Clone, Copy)]
enum RawValType {
    Float,
    Int,
    UInt,
}
impl Default for RawValType {
    fn default() -> Self {
        RawValType::Int
    }
}

pub struct Machine {
    stack: Vec<RawVal>,
    base_pointer: u64,
    closures: Vec<Closure>,
    pub ext_fun_table: Vec<(String, ExtFunType)>,
    fn_map: HashMap<usize, usize>, //index from fntable index of program to it of machine.
    pub ext_cls_table: Vec<(String, ExtClsType)>,
    cls_map: HashMap<usize, usize>, //index from fntable index of program to it of machine.
    global_states: StateStorage,
    states_stack: StateStorageStack,
    global_vals: Vec<RawVal>,
    debug_stacktype: Vec<RawValType>,
}

macro_rules! binop {
    ($op:tt,$t:ty, $dst:expr,$src1:expr,$src2:expr,$self:ident) => {
        {
        $self.set_stacktype($dst as i64, RawValType::Float);
        $self.set_stack($dst as i64, Self::to_value::<$t>(
            Self::get_as::<$t>($self.get_stack($src1 as i64))
        $op Self::get_as::<$t>($self.get_stack($src2 as i64))))
    }
    };
}
macro_rules! binopmethod {
    ($op:ident,$t:ty, $dst:expr,$src1:expr,$src2:expr,$self:ident) => {{
        $self.set_stacktype($dst as i64, RawValType::Float);
        $self.set_stack(
            $dst as i64,
            Self::to_value::<$t>(
                Self::get_as::<$t>($self.get_stack($src1 as i64))
                    .$op(Self::get_as::<$t>($self.get_stack($src2 as i64))),
            ),
        )
    }};
}
macro_rules! uniop {
    ($op:tt,$t:ty, $dst:expr,$src:expr,$self:ident) => {
        $self.set_stack($dst as i64,
            Self::to_value::<$t>(
            $op Self::get_as::<$t>($self.get_stack($src as i64))))
    };
}
macro_rules! uniopmethod {
    ($op:tt,$t:ty, $dst:expr,$src:expr,$self:ident) => {{
        $self.set_stack(
            $dst as i64,
            Self::to_value::<$t>(Self::get_as::<$t>($self.get_stack($src as i64)).$op()),
        )
    }};
}

fn set_vec<T>(vec: &mut Vec<T>, i: usize, value: T)
where
    T: Clone + std::default::Default,
{
    match i.cmp(&vec.len()) {
        Ordering::Less => vec[i] = value,
        Ordering::Equal => vec.push(value),
        Ordering::Greater => {
            vec.resize(i, T::default());
            vec.push(value);
        }
    }
}

impl Machine {
    pub fn new() -> Self {
        let ext_fun_table = builtin::BUILTIN_FNS
            .iter()
            .map(|(name, f, _t)| (name.to_string(), *f))
            .collect::<Vec<_>>();
        Self {
            stack: vec![],
            base_pointer: 0,
            closures: vec![],
            ext_fun_table,
            ext_cls_table: vec![],
            fn_map: HashMap::new(),
            cls_map: HashMap::new(),
            global_states: Default::default(),
            states_stack: Default::default(),
            global_vals: vec![],
            debug_stacktype: vec![RawValType::Int; 255],
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
    fn set_stacktype(&mut self, offset: i64, t: RawValType) {
        set_vec(
            &mut self.debug_stacktype,
            (self.base_pointer as i64 + offset) as usize,
            t,
        );
    }
    pub fn get_top(&self) -> &RawVal {
        self.stack.last().unwrap()
    }
    fn get_upvalue_offset(upper_base: usize, offset: OpenUpValue) -> usize {
        upper_base + offset.0
    }
    pub fn get_open_upvalue(&self, upper_base: usize, offset: OpenUpValue) -> RawVal {
        println!("upper base:{}, upvalue:{}", upper_base, offset.0);
        let abs_pos = Self::get_upvalue_offset(upper_base, offset);
        self.stack[abs_pos]
    }

    fn get_current_state(&mut self) -> &mut StateStorage {
        if self.states_stack.0.is_empty() {
            &mut self.global_states
        } else {
            let idx = unsafe { self.states_stack.0.last().unwrap_unchecked().0 as usize };
            &mut self.closures[idx].state_storage
        }
    }
    fn get_local_closure(&self, clsidx: ClosureIdx) -> &Closure {
        &self.closures[clsidx.0 as usize]
    }
    fn get_local_closure_mut(&mut self, clsidx: ClosureIdx) -> &mut Closure {
        &mut self.closures[clsidx.0 as usize]
    }
    fn return_general(&mut self, iret: Reg, nret: Reg) -> &[u64] {
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
        assert_eq!(std::mem::size_of::<T>(), 8);
        unsafe { std::mem::transmute_copy::<T, RawVal>(&v) }
    }
    fn call_function<F>(
        &mut self,
        func_pos: u8,
        _nargs: u8,
        nret_req: u8,
        mut action: F,
    ) -> ReturnCode
    where
        F: FnMut(&mut Self) -> ReturnCode,
    {
        let offset = (func_pos + 1) as u64;

        self.base_pointer += offset;
        let nret = action(self);

        if nret_req > nret as u8 {
            panic!("invalid number of return value required.");
        }
        // shrink stack so as to match with number of return values
        self.stack
            .truncate((self.base_pointer as i64 + nret_req as i64) as usize);
        self.base_pointer -= offset;
        nret
    }
    fn close_upvalues(&mut self, _iret: Reg, _nret: Reg, local_closures: &[(usize, ClosureIdx)]) {
        //todo! drop local closure which wont escape from local

        for (_base_ptr_cls, clsidx) in local_closures.iter() {
            let cls = self.get_local_closure(*clsidx);

            let newupvls = cls
                .upvalues
                .iter()
                .map(|upv| {
                    if let UpValue::Open(i) = upv {
                        let ov = self.get_open_upvalue(self.base_pointer as usize, *i);
                        UpValue::Closed(Rc::new(RefCell::new(ov)))
                    } else {
                        upv.clone()
                    }
                })
                .collect::<Vec<_>>();
            self.get_local_closure_mut(*clsidx).upvalues = newupvls;
        }
    }
    /// Execute function, return retcode.
    pub fn execute(
        &mut self,
        func_i: usize,
        prog: &Program,
        cls_i: Option<ClosureIdx>,
    ) -> ReturnCode {
        let (_fname, func) = &prog.global_fn_table[func_i];
        let mut local_closures: Vec<(usize, ClosureIdx)> = vec![];
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
                    match self.debug_stacktype[i] {
                        RawValType::Float => print!("{}f", Self::get_as::<f64>(self.stack[i])),
                        RawValType::Int => print!("{}i", Self::get_as::<i64>(self.stack[i])),
                        RawValType::UInt => print!("{}u", Self::get_as::<u64>(self.stack[i])),
                    }
                    if i < self.stack.len() - 1 {
                        print!(", ");
                    }
                }
                println!("]");
            }
            let mut increment = 1;
            match func.bytecodes[pcounter] {
                Instruction::Move(dst, src) => {
                    self.set_stack(dst as i64, self.get_stack(src as i64));
                }
                Instruction::MoveConst(dst, pos) => {
                    self.set_stack(dst as i64, func.constants[pos as usize]);
                }
                Instruction::CallCls(func, nargs, nret_req) => {
                    let addr = self.get_stack(func as i64);
                    let cls_i = Self::get_as::<ClosureIdx>(addr);
                    let cls = &self.closures[cls_i.0 as usize];
                    let pos_of_f = cls.fn_proto_pos;
                    self.states_stack.push(cls_i);
                    self.call_function(func, nargs, nret_req, move |machine| {
                        machine.execute(pos_of_f, prog, Some(cls_i))
                    });
                    self.states_stack.pop();
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
                    let nret = self.call_function(func, nargs, nret_req, move |machine| f(machine));
                    // return
                    let base = self.base_pointer as usize;
                    let iret = base + func as usize + 1;
                    self.stack
                        .copy_within(iret..(iret + nret as usize), base + func as usize);
                    self.stack.truncate(base + func as usize + nret as usize);
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
                    self.closures
                        .push(Closure::new(prog, self.base_pointer, fn_proto_pos));
                    let vaddr = ClosureIdx((self.closures.len() - 1) as u64);
                    local_closures.push((dst as usize, vaddr));
                    self.set_stack(dst as i64, Self::to_value(vaddr));
                }
                Instruction::Return0 => {
                    return 0;
                }
                Instruction::Return(iret, nret) => {
                    self.close_upvalues(iret, nret, &local_closures);
                    let _ = self.return_general(iret, nret);
                    return nret.into();
                }
                Instruction::GetUpValue(dst, index) => {
                    let rawv = {
                        let up_i = cls_i.unwrap();
                        let cls = &self.closures[up_i.0 as usize];
                        let upvalues = &cls.upvalues;
                        let rv: &UpValue = &upvalues[index as usize];
                        match rv {
                            UpValue::Open(i) => {
                                let upper_base = cls.base_ptr as usize;
                                self.get_open_upvalue(upper_base, *i)
                            }
                            UpValue::Closed(rawval) => *rawval.borrow(),
                        }
                    };
                    self.set_stack(dst as i64, rawv);
                }
                Instruction::SetUpValue(index, src) => {
                    let v = self.get_stack(src as i64);
                    let up_i = cls_i.unwrap();
                    let cls = &self.closures[up_i.0 as usize];
                    let upper_base = cls.base_ptr as usize;
                    let upvalues = &cls.upvalues;
                    let rv: &UpValue = &upvalues[index as usize];
                    match rv {
                        UpValue::Open(OpenUpValue(i)) => {
                            self.stack[upper_base + i] = v;
                        }
                        UpValue::Closed(i) => {
                            let mut uv = i.borrow_mut();
                            *uv = v;
                        }
                    }
                }
                Instruction::GetGlobal(dst, gid) => {
                    self.set_stack(dst as i64, self.global_vals[gid as usize])
                }
                Instruction::SetGlobal(gid, src) => {
                    self.global_vals[gid as usize] = self.get_stack(src as i64);
                }
                // Instruction::Close() => todo!(),
                Instruction::Jmp(offset) => {
                    // -1 is for the offset in last increment
                    increment = offset;
                }
                Instruction::JmpIfNeg(cond, offset) => {
                    let cond_v = self.get_stack(cond as i64);
                    if Self::get_as::<f64>(cond_v) < 0.0 {
                        increment = offset;
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
                    uniop!(-,f64,dst,src,self)
                }
                Instruction::AbsF(dst, src) => {
                    uniopmethod!(abs, f64, dst, src, self)
                }
                Instruction::SqrtF(dst, src) => {
                    uniopmethod!(sqrt, f64, dst, src, self)
                }
                Instruction::SinF(dst, src) => {
                    uniopmethod!(sin, f64, dst, src, self)
                }
                Instruction::CosF(dst, src) => {
                    uniopmethod!(cos, f64, dst, src, self)
                }

                Instruction::PowF(dst, src1, src2) => {
                    binopmethod!(powf, f64, dst, src1, src2, self)
                }
                Instruction::LogF(dst, src1, src2) => {
                    binopmethod!(log, f64, dst, src1, src2, self)
                }
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
                    uniopmethod!(abs, i64, dst, src, self)
                }
                Instruction::PowI(dst, lhs, rhs) => {
                    binop!(^,i64,dst,lhs,rhs,self)
                }
                Instruction::LogI(_, _, _) => {
                    //?
                    todo!();
                }
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
                    let v = self.get_current_state().get();
                    self.set_stack(dst as i64, Self::to_value(v));
                }
                Instruction::SetState(src) => {
                    let v = self.get_stack(src as i64);
                    let ptr = self.get_current_state().get_mut();
                    *ptr = Self::get_as::<f64>(v)
                }
                Instruction::ShiftStatePos(v) => self.get_current_state().shift_pos(v),
                Instruction::Dummy => {
                    unreachable!()
                }
            }
            pcounter = (pcounter as i64 + increment as i64) as usize;
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
        self.global_vals = prog.global_vals.clone();
        prog.ext_fun_table
            .iter()
            .enumerate()
            .for_each(|(i, (name, _ty))| {
                if let Some((j, _)) = self
                    .ext_fun_table
                    .iter()
                    .enumerate()
                    .find(|(_j, (fname, _fn))| name == fname)
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
                    .find(|(_j, (fname, _fn))| name == fname)
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
            .find(|(_i, (name, _))| name == entry)
        {
            if !func.bytecodes.is_empty() {
                self.global_states.resize(func.state_size as usize);
                // 0 is always base pointer to the main function
                if self.stack.len() > 0 {
                    self.stack[0] = 0;
                }
                self.base_pointer = 1;
                self.execute(i, &prog, None)
            } else {
                0
            }
        } else {
            -1
        }
    }
    pub fn execute_main(&mut self, prog: &Program) -> ReturnCode {
        //internal function table 0 is always mimium_main
        self.global_states
            .resize(prog.global_fn_table[0].1.state_size as usize);
        // 0 is always base pointer to the main function
        self.base_pointer += 1;
        self.execute(0, &prog, None)
    }
}

#[cfg(test)]
mod test;
