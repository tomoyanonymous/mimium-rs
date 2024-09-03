// Mid-level intermediate representation that is more like imperative form than hir.
use crate::{
    interner::{Symbol, ToSymbol, TypeNodeId},
    types::TypeSize,
    utils::metadata::{DSP_LABEL, GLOBAL_LABEL},
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
    None,
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
    // Store value to stack(destination,source, type)
    Store(VPtr, VPtr, TypeNodeId),
    // Instruction for computing destination address like LLVM's GetElementPtr.
    // This instruction does no actual computation on runtime.
    GetElement {
        value: VPtr,
        ty: TypeNodeId,
        array_idx: u64,
        tuple_offset: u64,
    },
    // call function, arguments, type of return value
    Call(VPtr, Vec<(VPtr, TypeNodeId)>, TypeNodeId),
    CallCls(VPtr, Vec<(VPtr, TypeNodeId)>, TypeNodeId),
    GetGlobal(VPtr, TypeNodeId),
    SetGlobal(VPtr, VPtr, TypeNodeId),
    // make closure with upindexes
    Closure(VPtr),
    //closes upvalues of specific closure. Always inserted right before Return instruction.
    CloseUpValue(VPtr),
    //label to funcproto  and localvar offset?
    GetUpValue(u64, TypeNodeId),
    SetUpValue(u64, VPtr, TypeNodeId),
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
    pub state_sizes: Vec<StateSize>,
    pub is_defined: bool,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct StateSize {
    pub size: u64,
    pub ty: TypeNodeId,
}

impl Function {
    fn new_inner(
        name: Symbol,
        args: &[VPtr],
        argtypes: &[TypeNodeId],
        upperfn_i: Option<usize>,
        is_defined: bool,
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
            is_defined,
        }
    }
    pub fn new(
        name: Symbol,
        args: &[VPtr],
        argtypes: &[TypeNodeId],
        upperfn_i: Option<usize>,
    ) -> Self {
        Self::new_inner(name, args, argtypes, upperfn_i, true)
    }
    pub fn new_stub(name: Symbol) -> Self {
        Self::new_inner(name, &[], &[], None, false)
    }

    pub fn add_new_basicblock(&mut self) -> usize {
        self.body.push(Block(vec![]));
        self.body.len() - 1
    }
    pub fn get_or_insert_upvalue(&mut self, v: &Arc<Value>) -> usize {
        self.upindexes
            .iter()
            .position(|vt| v == vt)
            .unwrap_or_else(|| {
                self.upindexes.push(v.clone());
                self.upindexes.len() - 1
            })
    }
}

#[derive(Debug, Clone)]
pub struct Mir {
    functions: Vec<Function>,
}

pub const FN_INDEX_GLOBAL: usize = 0;
pub const FN_INDEX_DSP: usize = 1;

impl Default for Mir {
    fn default() -> Self {
        Self {
            // preserve index for the main entry points
            functions: vec![
                Function::new_stub(GLOBAL_LABEL.to_symbol()),
                Function::new_stub(DSP_LABEL.to_symbol()),
            ],
        }
    }
}

impl Mir {
    pub fn add_function(&mut self, func: Function) -> usize {
        match func.label.as_str() {
            GLOBAL_LABEL => {
                self.functions[FN_INDEX_GLOBAL] = func;
                FN_INDEX_GLOBAL
            }
            DSP_LABEL => {
                self.functions[FN_INDEX_DSP] = func;
                FN_INDEX_DSP
            }
            _ => {
                self.functions.push(func);
                self.functions.len() - 1
            }
        }
    }

    pub fn into_functions(mut self, with_entry_point: bool) -> Vec<Function> {
        if !with_entry_point {
            self.functions.drain(0..=FN_INDEX_DSP);
        }

        self.functions
    }

    pub fn get_function_ref(&self, idx: usize) -> &Function {
        self.functions.get(idx).unwrap()
    }

    pub fn get_function_ref_mut(&mut self, idx: usize) -> &mut Function {
        self.functions.get_mut(idx).unwrap()
    }

    pub fn is_empty(&self) -> bool {
        !self.functions.iter().any(|f| f.is_defined)
    }

    pub fn cur_idx(&self) -> usize {
        self.functions.len() - 1
    }

    pub fn next_idx(&self) -> usize {
        self.functions.len()
    }
}
