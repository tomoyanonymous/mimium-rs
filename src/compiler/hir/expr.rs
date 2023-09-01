use crate::compiler::utils::metadata::{Span, WithMeta};
use std::rc::Rc;
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
pub struct Value(pub String);

pub type NodeRef<T> = Box<WithMeta<T>>;

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Var(NodeRef<Value>, Option<Time>),
    Block(Option<NodeRef<Self>>),
    Tuple(Vec<NodeRef<Self>>),
    Proj(NodeRef<Self>, i64),
    Apply(NodeRef<Self>, Vec<NodeRef<Self>>),

    Lambda(Vec<NodeRef<Value>>, NodeRef<Self>), //lambda
    Feed(NodeRef<Value>, NodeRef<Self>),        //feedback connection primitive
    Let(NodeRef<Value>, NodeRef<Self>, Option<NodeRef<Self>>),
    LetRec(NodeRef<Value>, NodeRef<Self>, Option<NodeRef<Self>>),
    LetTuple(Vec<NodeRef<Value>>, NodeRef<Self>, Option<NodeRef<Self>>),
    If(NodeRef<Self>, NodeRef<Self>, Option<NodeRef<Self>>),
    //id of function,free variables
    MakeClosure(NodeRef<Value>, Vec<NodeRef<Value>>),
    ApplyDir(NodeRef<Value>, Vec<NodeRef<Self>>),
    Bracket(NodeRef<Self>),
    Escape(NodeRef<Self>),
    Error,
}
impl Expr {
    pub fn gen_node(e: Self, span: Span) -> NodeRef<Self> {
        NodeRef::new(WithMeta(e, span))
    }
    pub fn is_value(&self) -> bool {
        match self {
            Expr::Lambda(_, _) | Expr::Feed(_, _) | Expr::Literal(_) | Expr::Tuple(_) => true,
            _ => false,
        }
    }
    pub fn eval_condition(&self) -> bool {
        match self {
            Expr::Literal(Literal::Int(i)) => *i > 0,
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

impl TreeWalk for NodeRef<Expr> {
    fn walk<F>(self, mut f: F) -> Self
    where
        Self: Sized,
        F: FnMut(Self) -> Self,
    {
        let res: Expr = match self.0 {
            Expr::Block(x) => Expr::Block(x.map(|b| f(b))),
            Expr::Tuple(vec) => Expr::Tuple(vec.into_iter().map(|b| f(b)).collect()),
            Expr::Proj(x, idx) => Expr::Proj(f(x), idx),
            Expr::Apply(e1, e2) => Expr::Apply(f(e1), e2.into_iter().map(|b| f(b)).collect()),
            Expr::Lambda(params, body) => Expr::Lambda(params, f(body)),
            Expr::Feed(id, body) => Expr::Feed(id, f(body)),
            Expr::Let(id, body, opt_then) => Expr::Let(id.clone(), f(body), opt_then.map(|b| f(b))),
            Expr::LetRec(id, body, opt_then) => {
                Expr::LetRec(id.clone(), f(body), opt_then.map(|b| f(b)))
            }
            Expr::LetTuple(id, body, opt_then) => {
                Expr::LetTuple(id, f(body), opt_then.map(|b| f(b)))
            }
            Expr::If(cond, then, opt_else) => Expr::If(f(cond), f(then), opt_else.map(|b| f(b))),
            Expr::MakeClosure(id, fvs) => Expr::MakeClosure(id.clone(), fvs.clone()),
            Expr::Bracket(x) => Expr::Bracket(f(x)),
            Expr::Escape(x) => Expr::Escape(f(x)),
            _ => self.0,
        };
        Box::new(WithMeta::<_>(res, self.1.clone()))
    }
}
