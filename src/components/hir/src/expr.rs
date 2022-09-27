use std::cell::RefCell;
use std::rc::Rc;
use utils::metadata::WithMeta;
pub type Time = i64;

// High-Level Intermediate Representation is mostly W-calculus based format without multi-stage computation factors.
// Values are bound not by name but shared references of ast-nodes.
// Some mimium-specific literal like "now" and "self" is now removed

#[derive(Clone, Debug, PartialEq, Hash)]
pub enum Literal {
    String(String),
    Int(i64),
    Float(String),
}

// type UniqueId = (String, i64);

#[derive(Clone, Debug, PartialEq)]
pub struct Value {
    pub id: String,
    pub v: RefCell<Option<Expr>>,
}
impl Value {
    pub fn new(s: String) -> Self {
        Self {
            id: s,
            v: RefCell::new(None),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Var(Rc<WithMeta<Value>>, Option<Time>),
    Block(Option<Box<WithMeta<Self>>>),
    Tuple(Vec<WithMeta<Self>>),
    Proj(Box<WithMeta<Self>>, i64),
    Apply(Box<WithMeta<Self>>, Vec<WithMeta<Self>>),
    
    Lambda(Vec<Rc<WithMeta<Value>>>, Box<WithMeta<Self>>), //lambda
    Feed(Rc<WithMeta<Value>>, Box<WithMeta<Self>>),        //feedback connection primitive
    Let(
        Rc<WithMeta<Value>>,
        Box<WithMeta<Self>>,
        Option<Box<WithMeta<Self>>>,
    ),
    LetRec(
        Rc<WithMeta<Value>>,
        Box<WithMeta<Self>>,
        Option<Box<WithMeta<Self>>>,
    ),
    LetTuple(
        Vec<Rc<WithMeta<Value>>>,
        Box<WithMeta<Self>>,
        Option<Box<WithMeta<Self>>>,
    ),
    If(
        Box<WithMeta<Self>>,
        Box<WithMeta<Self>>,
        Option<Box<WithMeta<Self>>>,
    ),
    //id of function,free variables
    MakeClosure(Rc<WithMeta<Value>>, Vec<Rc<WithMeta<Value>>>),
    ApplyDir(Rc<WithMeta<Value>>, Vec<WithMeta<Self>>),
    Bracket(Box<WithMeta<Self>>),
    Escape(Box<WithMeta<Self>>),
    Error,
}
impl Expr {
    pub fn is_value(&self) -> bool {
        match self {
            Expr::Lambda(_, _) | Expr::Feed(_, _) | Expr::Literal(_) | Expr::Tuple(_) => true,
            _ => false,
        }
    }
    pub fn eval_condition(&self) -> bool {
        match self {
            Expr::Literal(Literal::Int(i)) => (*i > 0),
            Expr::Literal(Literal::Float(s)) => s.parse::<f64>().unwrap() > 0.0,
            _ => panic!(),
        }
    }
}
pub trait TreeWalk {
    fn walk<F>(self, f: F) -> Self
    where
        Self: Sized,
        F: FnMut(Self) -> Self;
}

impl TreeWalk for WithMeta<Expr> {
    fn walk<F>(self, mut f: F) -> Self
    where
        Self: Sized,
        F: FnMut(Self) -> Self,
    {
        let res: Expr = match self.0 {
            Expr::Block(x) => Expr::Block(x.map(|b| Box::new(f(*b)))),
            Expr::Tuple(vec) => Expr::Tuple(vec.into_iter().map(|b| f(b)).collect()),
            Expr::Proj(x, idx) => Expr::Proj(Box::new(f(*x)), idx),
            Expr::Apply(e1, e2) => {
                Expr::Apply(Box::new(f(*e1)), e2.into_iter().map(|b| f(b)).collect())
            }
            Expr::Lambda(params, body) => Expr::Lambda(params, Box::new(f(*body))),
            Expr::Feed(id, body) => Expr::Feed(id, Box::new(f(*body))),
            Expr::Let(id, body, opt_then) => Expr::Let(
                id.clone(),
                Box::new(f(*body)),
                opt_then.map(|b| Box::new(f(*b))),
            ),
            Expr::LetRec(id, body, opt_then) => Expr::LetRec(
                id.clone(),
                Box::new(f(*body)),
                opt_then.map(|b| Box::new(f(*b))),
            ),
            Expr::LetTuple(id, body, opt_then) => {
                Expr::LetTuple(id, Box::new(f(*body)), opt_then.map(|b| Box::new(f(*b))))
            }
            Expr::If(cond, then, opt_else) => Expr::If(
                Box::new(f(*cond)),
                Box::new(f(*then)),
                opt_else.map(|b| Box::new(f(*b))),
            ),
            Expr::MakeClosure(id, fvs) => Expr::MakeClosure(id.clone(),fvs.clone()),
            Expr::Bracket(x) => Expr::Bracket(Box::new(f(*x))),
            Expr::Escape(x) => Expr::Escape(Box::new(f(*x))),
            _ => self.0,
        };
        WithMeta::<_>(res, self.1.clone())
    }
}
