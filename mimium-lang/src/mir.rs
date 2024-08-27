// Mid-level intermediate representation that is more like imperative form than hir.
use crate::{
    interner::{Symbol, TypeNodeId},
    types::TypeSize,
};
use std::{cell::OnceCell, sync::Arc};

pub mod print;

// #[derive(Debug, Clone, PartialEq)]
// pub struct Global(VPtr);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Argument(pub Symbol, pub TypeNodeId);

pub type VReg = u64;
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Value {
    Global(VPtr),
    Argument(usize, Arc<Argument>), //index,
    // holds SSA index(position in infinite registers)
    Register(VReg),
    State(VPtr),
    // idx of the function in the program
    Function(usize),
    ExtFunction(Symbol, TypeNodeId),
    FixPoint(usize), //function id
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
    Alloc(TypeNodeId),
    // load value to register from the pointer type
    Load(VPtr, TypeNodeId),
    // store value to pointer
    Store(VPtr, VPtr, TypeNodeId),
    // Instruction for computing destination address like LLVM's GetElementPtr.
    // This instruction does no actual computation on runtime.
    GetElement {
        value: VPtr,
        ty: TypeNodeId,
        array_idx: u64,
        tuple_offset: u64,
    },
    // call function, arguments
    Call(VPtr, Vec<(VPtr, TypeNodeId)>, TypeNodeId),
    CallCls(VPtr, Vec<(VPtr, TypeNodeId)>, TypeNodeId),
    GetGlobal(VPtr, TypeNodeId),
    SetGlobal(VPtr, VPtr, TypeNodeId),
    // make closure with upindexes
    Closure(VPtr),
    //label to funcproto  and localvar offset?
    GetUpValue(u64, TypeNodeId),
    SetUpValue(u64, TypeNodeId),
    //internal state: feed and delay
    PushStateOffset(Vec<StateSize>),
    PopStateOffset(Vec<StateSize>),
    //load internal state to register(destination)
    GetState(TypeNodeId),

    //condition,  basic block index for then else statement
    JmpIf(VPtr, u64, u64),
    // basic block index (for return statement)
    Jmp(i16),
    //merge
    Phi(VPtr, VPtr),

    Return(VPtr, TypeNodeId),
    //value to update state
    ReturnFeed(VPtr, TypeNodeId),

    Delay(u64, VPtr, VPtr),
    Mem(VPtr),

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
    Eq(VPtr, VPtr),
    Ne(VPtr, VPtr),
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

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct OpenUpValue(pub usize, pub TypeSize);

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub label: Symbol,
    pub args: Vec<Arc<Value>>,
    pub argtypes: Vec<TypeNodeId>,
    pub return_type: OnceCell<TypeNodeId>, // TODO: None is the state when the type is not inferred yet.
    pub upindexes: Vec<Arc<Value>>,
    pub upperfn_i: Option<usize>,
    pub body: Vec<Block>,
    state_sizes: Vec<StateSize>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StateSize {
    Typed(u64, TypeNodeId),
    Word(u64),
}

impl Function {
    pub fn new(
        name: Symbol,
        args: &[VPtr],
        argtypes: &[TypeNodeId],
        upperfn_i: Option<usize>,
    ) -> Self {
        Self {
            label: name,
            args: args.to_vec(),
            argtypes: argtypes.to_vec(),
            return_type: OnceCell::new(),
            upindexes: vec![],
            upperfn_i,
            body: vec![Block::default()],
            state_sizes: vec![],
        }
    }
    pub fn add_new_basicblock(&mut self) -> usize {
        self.body.push(Block(vec![]));
        self.body.len() - 1
    }

    // for the case where the state size can be determined immediately
    pub fn add_state_size(&mut self, size: u64) {
        self.state_sizes.push(StateSize::Word(1))
    }

    // for the case where the state size will be calculated from the type later
    pub fn add_typed_state_size(&mut self, size: u64, ty: TypeNodeId) {
        self.state_sizes.push(StateSize::Typed(1, ty))
    }

    pub fn get_state_sizes(&self) -> &[StateSize] {
        &self.state_sizes
    }
}

#[derive(Debug, Clone, Default)]
pub struct Mir {
    pub functions: Vec<Function>,
}
