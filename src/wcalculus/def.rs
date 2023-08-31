use std::rc::Rc;
pub type Natural = i64;
pub type Real = f64;
pub type Id = String;

enum Env<T> {
    Nil,
    Some((Id, Rc<T>), Rc<Env<T>>),
}
impl<T> Env<T> {
    fn new() -> Self {
        Self::Nil
    }
    fn prepend(Rc<Self>, other: (Id, Rc<T>)) -> Rc<Self> {
        Self::Some(other, self.clone())
    }
    fn lookup(&self, target: Id) -> Option<Rc<T>> {
        match &self {
            Nil => None,
            Some((id, t), next) => {
                if id == target {
                    Some(t.clone)
                } else {
                    next.lookup(target)
                }
            }
        }
    }
}

pub enum Time {
    Constant(Natural),
    RuntimeV { offset: Natural },
}
pub enum Expression {
    NumberLit(Real),
    Var {
        id: Id,
        t: Time,
    }, // variable with history
    Lambda {
        a: Id,
        e: Rc<Expression>,
    },
    App {
        e: Rc<Expression>,
        arg: Rc<Expression>,
    },
    Scale {
        c: Real,
        e: Rc<Expression>,
    },
    Add {
        e1: Rc<Expression>,
        e2: Rc<Expression>,
    },
    Prod {
        e1: Rc<Expression>,
        e2: Rc<Expression>,
    }, //tuple
    Proj {
        e: Rc<Expression>,
        idx: Natural,
    }, //tuple indice
    Feed {
        s: Id,
        e: Rc<Expression>,
    }, //feedback connection
}
pub type Expr = Expression;

use std::collections::HashMap;

use std::collections::VecDeque;
pub type NatList = VecDeque<Real>;

pub type Env = HashMap<Id, NatList>;

#[macro_export]
macro_rules! dumpenv {
    ($map:expr) => {
        for (key, value) in &*$map {
            print!("{} : ", key);
            for v in value {
                print!("{} ", v);
            }
            println!()
        }
    };
}

#[macro_export]
macro_rules! var {
    ($name:literal,$offset:literal) => {
        Rc::new(Expr::Var {
            id: String::from($name),
            t: wcalculus::Time::RuntimeV { offset: $offset },
        })
    };
}

#[macro_export]
macro_rules! scale {
    ($c:expr,$e:expr) => {
        Rc::new(Expr::Scale { c: $c, e: $e })
    };
}

#[macro_export]
macro_rules! add {
    ($e1:expr,$e2:expr) => {
        Rc::new(Expr::Add { e1: $e1, e2: $e2 })
    };
}

#[macro_export]
macro_rules! feed {
    ($id:literal,$e:expr) => {
        Rc::new(Expr::Feed {
            s: String::from($id),
            e: $e,
        })
    };
}

#[macro_export]
macro_rules! lambda {
    ($id:literal,$e:expr) => {
        std::rc::Rc::new(Expr::Lambda {
            a: String::from($id),
            e: $e,
        })
    };
}

#[macro_export]
macro_rules! app {
    ($ef:expr,$ea:expr) => {
        std::rc::Rc::new(Expr::App { e: $ef, arg: $ea })
    };
}
