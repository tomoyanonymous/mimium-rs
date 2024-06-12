// Mid-level intermediate representation that is more like imperative form than hir.
use crate::types::Type;
use std::sync::Arc;

pub mod print;
#[derive(Default, Debug, Clone)]
pub struct Label(pub String);

#[derive(Debug)]
pub struct Global(Label, Type);

#[derive(Debug, Clone)]
pub struct Argument(pub Label, pub Type);

pub type VReg = u64;
#[derive(Debug)]
pub enum Value {
    Global(Global),
    Argument(Argument),
    // holds SSA index(position in infinite registers)
    Register(VReg),
    // immidiate mode floating point value
    Float(f64),
    Integer(i64),
    Bool(bool),
    // idx of the function in the program
    Function(usize),
    ExtFunction(Label),
    Closure(Arc<Function>),
    FixPoint,
    None, //??
}

pub type VPtr = Arc<Value>;

#[derive(Debug)]
pub enum Instruction {
    Integer(i64),
    //constant float
    Float(f64),
    // allocate appropreate memory size depending on the type and return its pointer address
    Alloc(Type),
    // load value from the pointer type
    Load(VPtr),
    // store value to pointer
    Store(VPtr, VPtr),
    // Tuple(Vec<Value>),
    // Proj(Value, u64),
    // call function , arguments
    Call(Arc<Value>, Vec<VPtr>),
    // make closure with upindexes
    Closure(Arc<Function>),
    //function offset  and localvar offset?
    GetUpValue(u64, u64),
    SetUpValue(u64, u64),
    //internal state: feed and delay
    PushStateOffset(u64),
    PopStateOffset(u64),
    //load internal state to register
    GetState(VPtr),
    SetState(VPtr),
    //jump label
    JmpIf(VPtr, Label, Label),
    Return(VPtr),

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
    Eq(VPtr),
    Ne(VPtr),
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

#[derive(Debug, Default)]
pub struct Block(pub Vec<Instruction>);

#[derive(Debug)]
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

#[derive(Debug, Default)]
pub struct Function {
    pub label: Label,
    pub args: Vec<Argument>,
    // pub locals: Vec<Local>,
    pub upindexes: Vec<UpIndex>,
    // pub upperfn: Option<Arc<Self>>,
    pub body: Vec<Block>,
}

#[derive(Debug, Default)]
pub struct Mir {
    pub functions: Vec<Function>,
    pub globals: Vec<Global>,
}
