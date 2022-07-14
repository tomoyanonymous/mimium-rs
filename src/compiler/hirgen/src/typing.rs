use ast::expr::*;
use mmmtype::*;
use std::collections::HashMap;
use utils::{environment::*, metadata::*};

#[derive(Clone, Debug, PartialEq)]
pub struct Error(String);

#[derive(Clone, Debug, PartialEq)]
pub struct InferContext {
    interm_idx: i64,
    subst_map: HashMap<i64, Type>,
    pub env: Environment<Type>, // interm_map:HashMap<i64,Type>
}

impl InferContext {
    pub fn new() -> Self {
        Self {
            interm_idx: 0,
            subst_map: HashMap::<i64, Type>::new(),
            env: Environment::<Type>::new(),
        }
    }
    pub fn gen_intermediate_type(&mut self) -> Type {
        let res = Type::Intermediate(self.interm_idx);
        self.interm_idx += 1;
        res
    }
    // return true when the circular loop of intermediate variable exists.
    pub fn occur_check(&self, id1: i64, t2: Type) -> bool {
        let cls = |t2dash: Type| -> bool { self.occur_check(id1, t2dash) };

        let vec_cls = |t: Vec<_>| -> bool { t.iter().all(|a: &Type| cls(a.clone())) };

        match t2 {
            Type::Intermediate(id2) => {
                if id1 == id2 {
                    true
                } else {
                    self.subst_map
                        .get(&id1)
                        .map_or(false, |r| self.occur_check(id1, r.clone()))
                }
            }
            Type::Array(a) => cls(*a),
            Type::Tuple(t) => vec_cls(t),
            Type::Function(p, r, s) => {
                vec_cls(p) && cls(*r) && cls(*s.unwrap_or(Box::new(Type::Unknown)))
            }
            Type::Struct(_s) => todo!(),
            _ => false,
        }
    }

    fn substitute_intermediate_type(&self, id: i64) -> Option<Type> {
        match self.subst_map.get(&id) {
            Some(t) => match t {
                Type::Intermediate(i) => self.substitute_intermediate_type(*i),
                _ => Some(t.clone()),
            },
            None => None,
        }
    }
    pub fn substitute_type(&self, t: Type) -> Option<Type> {
        match t {
            Type::Intermediate(id) => self.substitute_intermediate_type(id),
            _ => Some(t.apply_fn(|e: Type| self.substitute_type(e).unwrap_or(Type::Unknown))),
        }
    }

    fn unify_types(&mut self, t1: Type, t2: Type) -> Result<Type, Error> {
        let mut unify_vec = |a1: Vec<Type>, a2: Vec<Type>| -> Result<Vec<_>, Error> {
            a1.clone()
                .iter()
                .zip(a2.iter())
                .map(|(v1, v2)| self.unify_types(v1.clone(), v2.clone()))
                .collect()
        };
        match (t1.clone(), t2.clone()) {
            (Type::Intermediate(i1), Type::Intermediate(i2)) => {
                if self.occur_check(i1, t2.clone()) {
                    Err(Error(
                        "Circular Loop of Type Definition Detected.".to_string(),
                    ))
                } else {
                    if i1 < i2 {
                        self.subst_map.insert(i1, t2);
                        Ok(t1)
                    } else {
                        self.subst_map.insert(i2, t1);
                        Ok(t2)
                    }
                }
            }
            (Type::Intermediate(i), t) => {
                self.subst_map.insert(i, t.clone());
                Ok(t)
            }
            (t, Type::Intermediate(i)) => {
                self.subst_map.insert(i, t.clone());
                Ok(t)
            }
            (Type::Array(box a1), Type::Array(box a2)) => {
                Ok(Type::Array(Box::new(self.unify_types(a1, a2)?)))
            }
            (Type::Ref(box x1), Type::Ref(box x2)) => {
                Ok(Type::Ref(Box::new(self.unify_types(x1, x2)?)))
            }
            (Type::Tuple(a1), Type::Tuple(a2)) => Ok(Type::Tuple(unify_vec(a1, a2)?)),
            (Type::Struct(_a1), Type::Struct(_a2)) => todo!(), //todo
            (Type::Function(p1, box r1, s1), Type::Function(p2, box r2, s2)) => Ok(Type::Function(
                unify_vec(p1, p2)?,
                Box::new(self.unify_types(r1, r2)?),
                match (s1, s2) {
                    (Some(box e1), Some(box e2)) => Some(Box::new(self.unify_types(e1, e2)?)),
                    (None, None) => None,
                    (_, _) => todo!("error handling"),
                },
            )),
            (Type::Numeric, Type::Numeric) => Ok(Type::Numeric),
            (Type::Int, Type::Int) => Ok(Type::Int),
            (Type::String, Type::String) => Ok(Type::String),
            (Type::Unit, Type::Unit) => Ok(Type::Unit),
            (Type::Code(_p1), Type::Code(_p2)) => {
                todo!("type system for multi-stage computation has not implemented yet")
            }
            (_p1, _p2) => Err(Error("type unification error".to_string())),
        }
    }
}

fn infer_type_literal(e: Literal) -> Result<Type, Error> {
    match e {
        Literal::Float(_s) => Ok(Type::Numeric),
        Literal::Int(_s) => Ok(Type::Int),
        Literal::String(_s) => Ok(Type::String),
        Literal::Now => Ok(Type::Numeric),
        Literal::SelfLit => Err(Error(
            "\"self\" should not be shown at type inference stage".to_string(),
        )),
    }
}

pub fn infer_type(e: Expr, ctx: &mut InferContext) -> Result<Type, Error> {
    let mut infer_vec = |e: Vec<WithMeta<Expr>>| {
        e.iter()
            .map(|WithMeta(el, span)| Ok(infer_type(el.clone(), ctx)?))
            .collect::<Result<Vec<_>, Error>>()
    };

    match e {
        Expr::Literal(l) => infer_type_literal(l),
        Expr::Tuple(e) => Ok(Type::Tuple(infer_vec(e)?)),
        Expr::Proj(e, idx) => {
            let tup = infer_type(e.0, ctx)?;
            match tup {
                Type::Tuple(vec) => {
                    if vec.len() < idx as usize {
                        Err(Error("index for tuple is out of bound".to_string()))
                    } else {
                        Ok(vec[idx as usize].clone())
                    }
                }
                _ => Err(Error("Index access for non-tuple type".to_string())),
            }
        }
        Expr::Feed(id, body) => {
            ctx.env.extend();
            let feedv = ctx.gen_intermediate_type();
            ctx.env.add_bind(id, feedv.clone());
            let res = infer_type(body.0, ctx)?;
            ctx.unify_types(res, feedv)
        }
        Expr::Lambda(p, r) => {
            let c = ctx;
            let mut infer_params = |e: Vec<WithMeta<TypedId>>| {
                e.iter()
                    .map(|WithMeta(id, span)| id.ty.clone().unwrap_or(c.gen_intermediate_type()))
                    .collect()
            };
            Ok(Type::Function(
                infer_params(p),
                Box::new(infer_type(r.0, c)?),
                None,
            ))
        }
        Expr::Let(id, body, then) => {
            let c = ctx;
            let bodyt = infer_type(body.0, c)?;
            let idt = id.ty.unwrap_or(c.gen_intermediate_type());
            let bodyt_u = c.unify_types(idt, bodyt)?;
            c.env.extend();
            c.env.add_bind(id.id, bodyt_u);
            match then {
                Some(e) => infer_type(e.0, c),
                None => Ok(Type::Unit),
            }
        }
        Expr::LetTuple(_ids, _body, _then) => {
            todo!("should be de-sugared before type inference")
        }
        Expr::LetRec(id, body, then) => {
            let c = ctx;
            let idt = id.ty.unwrap_or(c.gen_intermediate_type());
            c.env.extend();
            let body_i = c.gen_intermediate_type();
            c.env.add_bind(id.id, body_i);
            let bodyt = infer_type(body.0, c)?;
            let _ = c.unify_types(idt, bodyt)?;

            match then {
                Some(e) => infer_type(e.0, c),
                None => Ok(Type::Unit),
            }
        }
        Expr::Var(name, _time) => ctx
            .env
            .get_bound_value(name)
            .map_or(Err(Error("variable not found".to_string())), |v| {
                Ok(v.clone())
            }),
        Expr::Apply(fun, callee) => {
            let c = ctx;
            let fnl = infer_type(fun.0, c)?;
            let callee_t = infer_type(callee.0, c)?;
            let res_t = c.gen_intermediate_type();
            let fntype = Type::Function(vec![callee_t], Box::new(res_t), None);
            c.unify_types(fnl, fntype)
        }
        Expr::If(cond, then, opt_else) => {
            let condt = infer_type(cond.0, ctx)?;
            let _bt = ctx.unify_types(Type::Int, condt); //todo:boolean type
            let thent = infer_type(then.0, ctx)?;
            let elset = opt_else.map_or(Ok(Type::Unit), |e| infer_type(e.0, ctx))?;
            ctx.unify_types(thent, elset)
        }
        Expr::Block(expr) => expr.map_or(Ok(Type::Unit), |e| infer_type(e.0, ctx)),
        _ => {
            // todo!();
            Ok(Type::Unit)
        }
    }
}
