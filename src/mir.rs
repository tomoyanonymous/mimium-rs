// Mid-level intermediate representation that is more like imperative form than hir.
use std::rc::Rc;
pub struct Label(String);

// is it OK as same as mimium base typing?
pub enum Type{
    Unit
}


pub enum Value{
    // holds SSA index(position in infinite registers)
    Register(u64,Type,Option<Label>),
    // immidiate mode floating point value
    Float(f64),
    Integer(i64),
    Bool(bool),
    // pointer to the top-level functions
    Function(Label),
    //
    Closure(Label,Vec<Box<Value>>),
    //??
}

pub enum Instruction {
    // allocate appropreate memory size depending on the type and return its pointer address
    Alloc(Type),
    // load value from the pointer type
    Load(Value),
    // store value to pointer
    Store(Value, Value),
    // Tuple(Vec<Value>),
    // Proj(Value, u64),
    // call function 
    Call(Value, Vec<Value>),
    CallIntrinsic(Intrinsic, Vec<Value>),
    Closure(Label, Vec<Value>),
    //function offset  and localvar offset?
    UpValue(u64,u64),
    JmpIf(Value,Label,Label),
    Return(Value),

    // Primitive Operations
    AddF(Value,Value),
    SubF(Value,Value),
    MulF(Value,Value),
    DivF(Value,Value),
    ModF(Value,Value),
    NegF(Value),
    SinF(Value),
    CosF(Value),
    PowF(Value,Value),
    LogF(Value,Value),

    // Primitive Operations for int
    AddI(Value,Value), 
    SubI(Value,Value),
    MulI(Value,Value),
    DivI(Value,Value),
    ModI(Value,Value),
    NegI(Value),
    PowI(Value),
    LogI(Value,Value),
    // primitive Operations for bool
    Not(Value),
    Eq(Value),
    Ne(Value),
    Gt(Value,Value),
    Ge(Value,Value),
    Lt(Value,Value),
    Le(Value,Value),
    And(Value,Value),
    Or(Value,Value),


}

pub struct Block(Vec<Instruction>);

pub struct Function {
    pub body: Vec<Block>,
}

pub enum TopLevel{
    Function(Function),
    //other global declaration continues...
}
pub struct Mir(pub Vec<TopLevel>);