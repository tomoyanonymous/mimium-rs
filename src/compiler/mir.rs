// Mid-level intermediate representation that is more like imperative form than hir.
use std::rc::Rc;
pub struct Label(String);

pub enum Value {
    Register(Rc<Label>),
    Address(u64),
    Integer(i64),
    Numeric(String),
    FuncRef(Rc<Label>),
    FParam(Rc<Label>),
    External(Rc<Label>),
}
pub enum Intrinsic {
    Add,
    Sub,
    Mult,
    Div,
    Mod,
    Ge,
    Gt,
    Le,
    Lt,
}
pub enum Instruction {
    Const(Value),
    Load(Value),
    Store(Value, Value), //from,to
    Tuple(Vec<Value>),
    Proj(Value, u64),
    Call(Label, Vec<Value>),
    CallIntrinsic(Intrinsic, Vec<Value>),
    MakeClosure(Label, Vec<Value>),
    If(Value,Block,Block),
    Return(Value)
}

pub struct Block(Vec<Instruction>);

pub struct Function {
    pub params: Vec<Rc<Label>>,
    pub clsref: Option<Rc<Label>>,
    pub selfref: Option<Rc<Label>>,
    pub body: Block,
}

pub enum TopLevel{
    Function(Function),
    //other declaration continues...
}
pub struct Mir(pub Vec<TopLevel>);