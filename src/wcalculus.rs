pub mod wcalculus {
    type Natural = u64;
    type Real = f64;
    type Id = String;
    use std::rc::Rc;

    pub enum Expression {
        NumberLit(Real),
        Var {
            id: Id,
            t: Natural,
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
    type Expr = Expression;

    use std::collections::HashMap;

    use std::collections::VecDeque;
    type NatList = VecDeque<Real>;

    pub type Env = HashMap<Id, NatList>;
    pub struct LamCls {
        // e: &'a Expression,
        id: Rc<Id>,
        e: Rc<Expression>,
    }
    pub trait CallCls {
        fn invoke<'a>(&mut self, time: Natural, history: NatList, env: &'a mut Env) -> EvalType;
    }
    pub enum EvalType {
        WFn(Box<dyn CallCls>),
        Number(Real),
        List(NatList),
    }
    impl CallCls for LamCls {
        fn invoke<'a>(&mut self, time: Natural, history: NatList, env: &'a mut Env) -> EvalType {
            env.insert(self.id.to_string(), history);
            interpret(time, self.e.clone(), env)
        }
    }
    pub fn interpret_n<'a>(time: Natural, e: Rc<Expression>, env: &'a mut Env) -> NatList {
        if time <= 0 {
            let res = NatList::new();
            res
        } else {
            let res_now = interpret(time, e.clone(), env);
            match res_now {
                EvalType::List(mut list) => {
                    let mut res_past = interpret_n(time - 1, e, env);
                    list.append(&mut res_past);
                    list
                }
                _ => panic!("result of interpret were not a list of float"),
            }
        }
    }
    macro_rules! dumpenv {
        ($map:expr) => {
            for (key, value) in &*$map {
                println!("{} / {}", key, value.front().unwrap_or(&0.001));
            }
        };
    }
    pub fn interpret<'a>(time: Natural, expr: Rc<Expression>, env: &'a mut Env) -> EvalType {
        match &*expr {
            Expr::Lambda { a, e } => EvalType::WFn(Box::new(LamCls {
                id: Rc::new(a.to_string()),
                e: e.clone(),
            })),
            Expr::Var { id, t } => {
                let elist = env.get(id);
                match elist {
                    Some(l) => match l.get(*t as usize) {
                        Some(n) => EvalType::Number(*n),
                        _ => panic!(),
                    },
                    _ => {
                        dumpenv!(env);
                        panic!("Variable {} not found, environment", id)
                    }
                }
            }
            Expr::NumberLit(n) => {
                let mut l = NatList::new();
                l.push_back(*n);
                EvalType::List(l)
            }
            Expr::Scale { c, e } => {
                let e1 = interpret(time, e.clone(), env);
                match e1 {
                    EvalType::Number(n) => EvalType::Number(n * c),
                    _ => panic!("failed at scale"),
                }
            }
            Expr::Add { e1, e2 } => {
                let e1 = interpret_n(time, e1.clone(), env);
                let e2 = interpret_n(time, e2.clone(), env);
                match (e1.front(), e2.front()) {
                    (Some(e1l), Some(e2l)) => EvalType::Number(e1l + e2l),
                    _ => panic!("failed at add"),
                }
            }
            Expr::App { e, arg } => {
                let a_hist = interpret_n(time, arg.clone(), env);
                let cls = interpret(time, e.clone(), env);
                let res: EvalType = match cls {
                    EvalType::WFn(mut f) => f.invoke(time, a_hist, env),
                    _ => panic!("callee is not a function"),
                };
                res
            }
            Expr::Feed { s, e } => {
                let mut e_hist = interpret_n(time - 1, e.clone(), env);
                match env.get_mut(s) {
                    None => {
                        env.insert(s.to_string(), e_hist);
                    }
                    Some(l) => {
                        l.append(&mut e_hist);
                    }
                };
                interpret(time, e.clone(), env)
            }
            _ => panic!("unknown node"),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::wcalculus::*;
    type Expr = wcalculus::Expression;
    use std::rc::Rc;

    macro_rules! makeString {
        ($name:literal) => {
            Rc::new(Expr::Var {
                id: String::from($name),
                t: 0,
            })
        };
    }
    #[test]
    pub fn make_onepole() {
        // let gain = Rc::new(Expr::NumberLit(0.99));
        let factor: f64 = 0.99;
        let rfactor = 1.0 - factor;

        let scaler = Rc::new(Expr::Scale {
            c: factor,
            e: makeString!("onepole_self"),
        });
        let rscaler = Rc::new(Expr::Scale {
            c: rfactor,
            e: makeString!("onepole_x"),
        });
        let content = Rc::new(Expr::Add {
            e1: scaler,
            e2: rscaler,
        });
        let feed = Rc::new(Expr::Feed {
            s: String::from("onepole_self"),
            e: content,
        });
        let lam = Rc::new(Expr::Lambda {
            a: String::from("onepole_x"),
            e: feed,
        });
        use std::collections::VecDeque;

        let input = vec![1.0, 0.2, 0.2, 0.5];
        let mut env = wcalculus::Env::new();
        let res = wcalculus::interpret(input.len() as u64, lam, &mut env);
        if let wcalculus::EvalType::WFn(mut cls) = res {
            let indeque = VecDeque::from(input);
            let res = cls.invoke(indeque.len() as u64, indeque, &mut env);
            if let wcalculus::EvalType::List(list) = res {
                println!("{:?}", list);
            } else {
                panic!("the result was not a list");
            }
        }
    }
}
