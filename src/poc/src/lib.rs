use std::rc::Rc;
pub type Natural = i64;
pub type Real = f64;
pub type Id = String;

#[derive(Clone)]
enum Env<T> {
    Nil,
    Some((Id, Rc<T>), Rc<Env<T>>),
}
impl<T> Env<T> {
    pub fn new() -> Self {
        Self::Nil
    }
    pub fn prepend(other: (Id, Rc<T>), s: Rc<Self>) -> Rc<Self> {
        Rc::new(Self::Some(other, s.clone()))
    }
    pub fn lookup(&self, target: Id) -> Option<Rc<T>> {
        match &self {
            Self::Nil => None,
            Self::Some((id, t), next) => {
                if *id == target {
                    Some(t.clone())
                } else {
                    next.lookup(target)
                }
            }
        }
    }
}

pub mod Signal {
    use super::Env;
    use super::Id;
    use std::cell::RefCell;
    use std::ops::Deref;
    use std::rc::Rc;
    enum Type {
        Real,
        Int,
        Fun(Box<Type>, Box<Type>),
        Ptr,
        Code(Box<Type>),
    }
    #[derive(Clone)]
    enum Error {
        OutOfTimeBound,
        VariableNotFound,
        TypeMismatch,
        TerminateWithFun,
        SelfNotAllocated,
    }
    #[derive(Clone)]
    enum Value {
        Real(f64),
        Fun(Rc<dyn Fn(Rc<Value>) -> Result<Rc<Value>, Error>>),
        Ptr(RefCell<Rc<Value>>),
    }
    #[derive(Clone)]
    enum Expr {
        /// Variable accessed with delay index
        Literal(Value),
        Var(Id, u64),
        Lambda(Id, Box<Expr>),
        ///ptr, function, argument
        App(Rc<Value>, Box<Expr>, Box<Expr>),
        // AppExt(Id,Box<Expr>)
        Feed(Id, Box<Expr>),
    }
    type VEnv = Env<Vec<Rc<Value>>>;
    #[derive(Clone)]
    struct Context {
        heap: VEnv,
    }

    fn eval(expr: Expr, env: Rc<VEnv>, ctx: Context) -> Result<Rc<Value>, Error> {
        match expr {
            Expr::Literal(l) => Ok(Rc::new(l)),
            Expr::Var(id, time) => match env.lookup(id) {
                Some(v) => {
                    let resopt = v.get(time as usize).map(|v| Rc::clone(v));
                    resopt.ok_or(Error::OutOfTimeBound)
                }
                None => Err(Error::VariableNotFound),
            },
            Expr::Lambda(arg, body) => Ok(Rc::new(Value::Fun(Rc::new(move |x| {
                //x should be a pointer
                eval(
                    *body.clone(),
                    Env::prepend((arg.clone(), Rc::new(vec![x.clone()])), env.clone()),
                    ctx.clone(),
                )
            })))),
            Expr::App(ptr, ef, ea) => {
                let a = eval(*ea, env.clone(), ctx.clone())?;
                let _ = match ptr.clone().as_ref() {
                    Value::Ptr(p) => {
                        *p.borrow_mut() = a.clone();
                        Ok(())
                    }
                    _ => Err(Error::TypeMismatch),
                };
                let v = eval(*ef, env, ctx.clone())?;
                match v.as_ref() {
                    Value::Fun(f) => f(ptr.clone()),
                    _ => Err(Error::TypeMismatch),
                }
            }
            Expr::Feed(id, body) => {
                let cell = RefCell::new(Rc::new(Value::Real(0.0)));
                let v = Rc::new(Value::Ptr(cell.clone()));
                let new_env = Env::prepend((id, Rc::new(vec![v.clone()])), env.clone());
                let res = eval(*body.clone(), new_env, ctx.clone())?;
                *cell.borrow_mut() = res;
                Ok(v.clone())
            }
        }
    }
}
