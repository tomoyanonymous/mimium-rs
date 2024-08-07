// Mid-level intermediate representation that is more like imperative form than hir.
use crate::types::Type;
use std::{cell::RefCell, rc::Rc, sync::Arc};

pub mod print;
#[derive(Default, Debug, Clone, PartialEq)]
pub struct Label(pub String);

// #[derive(Debug, Clone, PartialEq)]
// pub struct Global(VPtr);

#[derive(Debug, Clone, PartialEq)]
pub struct Argument(pub Label, pub Type);

pub type VReg = u64;
#[derive(Debug, PartialEq)]
pub enum Value {
    Global(VPtr),
    Argument(usize, Arc<Argument>), //index,
    // holds SSA index(position in infinite registers)
    Register(VReg),
    State(VPtr),
    // idx of the function in the program, size of internal state
    Function(usize, u64),
    ExtFunction(Label,Type),
    FixPoint(usize),//function id
    //internal state
    None, //??
}

pub type VPtr = Arc<Value>;

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    Uinteger(u64),
    Integer(i64),
    //constant float
    Float(f64),
    // allocate memory from stack depending on the size
    Alloc(Type),
    // load value to register from the pointer type
    Load(VPtr),
    // store value to pointer
    Store(VPtr, VPtr),
    // Tuple(Vec<Value>),
    // Proj(Value, u64),
    // call function , arguments
    Call(VPtr, Vec<VPtr>),
    CallCls(VPtr, Vec<VPtr>),
    GetGlobal(VPtr),
    SetGlobal(VPtr,VPtr),
    // make closure with upindexes
    Closure(VPtr),
    //label to funcproto  and localvar offset?
    GetUpValue(u64),
    SetUpValue(u64),
    //internal state: feed and delay
    PushStateOffset(u64),
    PopStateOffset(u64),
    //load internal state to register(destination)
    GetState,

    //condition,  basic block index for then else statement
    JmpIf(VPtr, u64, u64),
    // basic block index (for return statement)
    Jmp(i16),
    //merge
    Phi(VPtr, VPtr),

    Return(VPtr),
    //value to update state
    ReturnFeed(VPtr),

    // Primitive Operations
    AddF(VPtr, VPtr),
    SubF(VPtr, VPtr),
    MulF(VPtr, VPtr),
    DivF(VPtr, VPtr),
    ModF(VPtr, VPtr),
    NegF(VPtr),
    AbsF(VPtr),
    SinF(VPtr),
    CosF(VPtr),
    PowF(VPtr, VPtr),
    LogF(VPtr, VPtr),
    SqrtF(VPtr),

    // Primitive Operations for int
    AddI(VPtr, VPtr),
    SubI(VPtr, VPtr),
    MulI(VPtr, VPtr),
    DivI(VPtr, VPtr),
    ModI(VPtr, VPtr),
    NegI(VPtr),
    AbsI(VPtr),

    PowI(VPtr),
    LogI(VPtr, VPtr),
    // primitive Operations for bool
    Not(VPtr),
    Eq(VPtr,VPtr),
    Ne(VPtr,VPtr),
    Gt(VPtr, VPtr),
    Ge(VPtr, VPtr),
    Lt(VPtr, VPtr),
    Le(VPtr, VPtr),
    And(VPtr, VPtr),
    Or(VPtr, VPtr),

    CastFtoI(VPtr),
    CastItoF(VPtr),
    CastItoB(VPtr),
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Block(pub Vec<(VPtr, Instruction)>);

#[derive(Debug, Clone, PartialEq)]
pub enum UpIndex {
    Local(usize),   // index of local variables in upper functions
    Upvalue(usize), // index of upvalues in upper functions
}

#[derive(Clone, Debug)]
pub struct Local {
    pub name: String,
    pub depth: usize,
    pub is_captured: bool,
}
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct OpenUpValue(pub usize);

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub label: Label,
    pub args: Vec<Arc<Value>>,
    pub upindexes: Vec<Arc<Value>>,
    pub upperfn_i: Option<usize>,
    pub body: Vec<Block>,
    pub state_size: u64,
}
impl Function {
    pub fn new(name: &str, args: &[VPtr], upperfn_i: Option<usize>) -> Self {
        Self {
            label: Label(name.to_string()),
            args: args.to_vec(),
            upindexes: vec![],
            upperfn_i,
            body: vec![Block::default()],
            state_size: 0,
        }
    }
    pub fn add_new_basicblock(&mut self) -> usize {
        self.body.push(Block(vec![]));
        self.body.len() - 1
    }
}

#[derive(Debug, Clone, Default)]
pub struct Mir {
    pub functions: Vec<Function>,
}
