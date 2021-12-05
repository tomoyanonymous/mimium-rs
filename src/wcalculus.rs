pub mod wcalculus {
    type Natural = i64;
    type Real = f64;
    type Id = String;
    use std::rc::Rc;
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
    type Expr = Expression;

    use std::collections::HashMap;

    use std::collections::VecDeque;
    type NatList = VecDeque<Real>;

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
    pub struct LamCls {
        // e: &'a Expression,
        id: Rc<Id>,
        e: Rc<Expression>,
    }
    pub trait CallCls {
        fn get_name(&self) -> String;
        fn invoke<'a>(&mut self, time: Natural, history: NatList, env: &'a mut Env) -> EvalType;
    }
    use std::fmt;
    // #[derive(Debug)]
    pub enum EvalType {
        WFn(Box<dyn CallCls>),
        Number(Real),
    }
    impl EvalType{
        fn get_num(&self) -> Option<Real>{
            match self{
                EvalType::Number(n) => Some(*n),
                _ => None
            }
        }
    }

    
    impl CallCls for LamCls {
        fn get_name(&self) -> String {
            self.id.to_string()
        }
        fn invoke<'a>(&mut self, time: Natural, history: NatList, env: &'a mut Env) -> EvalType {
            env.insert(self.id.to_string(), history);
            interpret(time, self.e.clone(), env)
        }
    }

    fn type_of<T>(_: T) -> String {
        let a = std::any::type_name::<T>();
        return a.to_string();
    }
    pub fn interpret_n<'a>(time: Natural, e: Rc<Expression>, env: &'a mut Env) -> NatList {
        if time < 0 {
            NatList::new()
        } else {
            let res_now = interpret(time, e.clone(), env);
            match res_now {
                EvalType::Number(n) => {
                    let mut res_past = interpret_n(time - 1, e, env);
                    res_past.push_front(n);
                    assert_eq!(res_past.len(), 1 + time as usize);
                    res_past
                }
                EvalType::WFn(f) => {
                    dumpenv!(env);
                    panic!(
                        "result of interpret were not a list of float but a closure: {} at time {}",
                        f.get_name(),
                        time
                    )
                }
            }
        }
    }

    pub fn interpret<'a>(time: Natural, expr: Rc<Expression>, env: &'a mut Env) -> EvalType {
        match &*expr {
            Expr::Lambda { a, e } => EvalType::WFn(Box::new(LamCls {
                id: Rc::new(a.to_string()),
                e: e.clone(),
            })),
            Expr::Var { id, t } => {
                let elist = env.get(id);
                let timeidx = match t {
                    Time::Constant(t) => *t,
                    Time::RuntimeV { offset } => time-offset,
                };
                match elist {
                    Some(l) => match l.iter().nth(timeidx as usize) {
                        Some(n) => EvalType::Number(*n),
                        None => {
                            if l.len() == 0 || timeidx < 0{
                                EvalType::Number(0.0)
                            } else {
                                panic!(
                            "out of bound access of variable {}, index was {}, length was {}",
                            id,
                            timeidx,
                            l.len()
                        )
                            }
                        }
                    },
                    _ => {
                        dumpenv!(env);
                        panic!("Variable {} not found, environment", id)
                    }
                }
            }
            Expr::NumberLit(n) => EvalType::Number(*n),
            Expr::Scale { c, e } => {
                let e1 = interpret(time, e.clone(), env);
                match e1 {
                    EvalType::Number(n) => {
                        println!("scale {} * {}", n, c);
                        EvalType::Number(n * c)
                    }
                    _ => panic!("failed at scale"),
                }
            }
            Expr::Add { e1, e2 } => {
                let e1 = interpret(time, e1.clone(), env);
                let e2 = interpret(time, e2.clone(), env);
                match (e1, e2) {
                    (EvalType::Number(e1l), EvalType::Number(e2l)) => EvalType::Number(e1l + e2l),
                    _ => panic!("failed at add"),
                }
            }
            Expr::App { e, arg } => {
                let mut a_hist = interpret_n(time, arg.clone(), env);
                assert_eq!(a_hist.len() as i64, time + 1);
                println!("app eval at {}, env is {:?}", time, a_hist);

                let cls = interpret(time, e.clone(), env);
                let res: EvalType = match cls {
                    EvalType::WFn(mut f) => f.invoke(time, a_hist, env),
                    _ => panic!("callee is not a function"),
                };
                res
            }
            Expr::Feed { s, e } => {
                println!("feed history start at {}", time);
                let mut e_hist = interpret_n(
                    time - 1,
                    Rc::new(Expr::Feed {
                        s: s.to_string(),
                        e: e.clone(),
                    }),
                    env,
                );
                assert_eq!(e_hist.len() as i64, time);
                println!("feed history at {}:  {:?}", time, e_hist);
                // if time is 5, e_hist should holds the result from 0 to 4...
                match env.get_mut(s) {
                    None => {
                        if e_hist.is_empty() {
                            e_hist.push_front(0.0);
                        }
                        env.insert(s.to_string(), e_hist);
                    }
                    Some(mut l) => {
                        if let Some(n) = e_hist.front() {
                            l.clear();
                            l.append(&mut e_hist);
                        }
                    }
                }
                let res = interpret(time, e.clone(), env);
                println!("feed eval at {}, res is {}", time, res.get_num().unwrap_or(-99.0));
                res
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

    macro_rules! makeId {
        ($name:literal,$offset:literal) => {
            Rc::new(Expr::Var {
                id: String::from($name),
                t: wcalculus::Time::RuntimeV { offset: $offset },
            })
        };
    }

    #[test]
    pub fn make_onepole() {
        // let gain = Rc::new(Expr::NumberLit(0.99));
        let factor: f64 = 0.5;
        let rfactor = 1.0 - factor;

        let scaler = Rc::new(Expr::Scale {
            c: factor,
            e: makeId!("onepole_x", 0),
        });
        let rscaler = Rc::new(Expr::Scale {
            c: rfactor,
            e: makeId!("onepole_self", 1),
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

        let input = vec![0.0, 0.0, 0.0, 0.0, 1.0];
        let inputdeque = VecDeque::from(input);
        let lam = Rc::new(Expr::App {
            e: lam,
            arg: Rc::new(Expr::Var {
                id: String::from("input".to_string()),
                t: wcalculus::Time::RuntimeV { offset: 0 },
            }),
        });
        let mut env = wcalculus::Env::new();
        let len = inputdeque.len() as i64 - 1;
        env.insert("input".to_string(), inputdeque);
        let res = wcalculus::interpret_n(len, lam, &mut env);

        println!("Output: {:?}", res);
        assert_eq!(res.len(), (len + 1) as usize);
        crate::dumpenv!(&env);
    }
}
