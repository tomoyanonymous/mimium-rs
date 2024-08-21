use crate::ast::{Expr, ExprId, Literal, Symbol, ToSymbol};
use crate::compiler::intrinsics;
use crate::pattern::{Pattern, TypedPattern};
use crate::runtime::vm::builtin;
use crate::types::{PType, Type};
use crate::utils::{
    environment::Environment,
    error::ReportableError,
    metadata::{Span, WithMeta},
};
use crate::{function, numeric};
use std::collections::HashMap;
use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub enum ErrorKind {
    TypeMismatch(Type, Type),
    PatternMismatch(Type, Pattern),
    NonFunction(Type),
    CircularType,
    IndexOutOfRange(u16, u16),
    IndexForNonTuple,
    VariableNotFound(String),
    NonPrimitiveInFeed,
}
#[derive(Clone, Debug, PartialEq)]
pub struct Error(pub ErrorKind, pub Span);

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            ErrorKind::TypeMismatch(e, a) => write!(f, "Type Mismatch, between {e} and {a}"),
            ErrorKind::PatternMismatch(e, p) => write!(f, "Pattern {p} cannot have {e} type."),

            ErrorKind::CircularType => write!(f, "Circular loop of type definition"),
            ErrorKind::IndexOutOfRange(len, idx) => write!(
                f,
                "Length of tuple elements is {} but index was {}",
                len, idx
            ),
            ErrorKind::IndexForNonTuple => write!(f, "Index access for non-tuple variable"),
            ErrorKind::VariableNotFound(v) => {
                write!(f, "Variable {} not found in this scope", v)
            }
            ErrorKind::NonPrimitiveInFeed => {
                write!(f, "Function that uses self cannot be return function type.")
            }
            ErrorKind::NonFunction(t) => write!(f, "{t} is not a function type."),
        }
    }
}
impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}
impl std::error::Error for Error {}
impl ReportableError for Error {
    fn get_span(&self) -> std::ops::Range<usize> {
        self.1.clone()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct InferContext {
    interm_idx: i64,
    subst_map: HashMap<i64, Type>,
    pub env: Environment<Type>, // interm_map:HashMap<i64,Type>
}

impl InferContext {
    pub fn new() -> Self {
        let mut res = Self {
            interm_idx: 0,
            subst_map: HashMap::<i64, Type>::new(),
            env: Environment::<Type>::new(),
        };
        res.env.extend();
        Self::register_intrinsics(&mut res.env);
        Self::register_builtin(&mut res.env);

        res
    }
    fn register_intrinsics(env: &mut Environment<Type>) {
        let binop_ty = function!(vec![numeric!(), numeric!()], numeric!());
        let binop_names = vec![
            intrinsics::ADD,
            intrinsics::SUB,
            intrinsics::MULT,
            intrinsics::DIV,
            intrinsics::MODULO,
            intrinsics::EXP,
            intrinsics::GT,
            intrinsics::LT,
            intrinsics::GE,
            intrinsics::LE,
            intrinsics::EQ,
            intrinsics::NE,
        ];
        let uniop_ty = function!(vec![numeric!()], numeric!());
        let uniop_names = vec![
            intrinsics::NEG,
            intrinsics::MEM,
            intrinsics::SIN,
            intrinsics::COS,
            intrinsics::ABS,
            intrinsics::SQRT,
        ];
        let mut binds = binop_names
            .iter()
            .map(|n| (n.to_symbol(), binop_ty.clone()))
            .collect::<Vec<(Symbol, Type)>>();
        uniop_names
            .iter()
            .map(|n| (n.to_symbol(), uniop_ty.clone()))
            .collect_into(&mut binds);
        binds.push((
            intrinsics::DELAY.to_symbol(),
            function!(vec![numeric!(), numeric!(), numeric!()], numeric!()),
        ));
        env.add_bind(&binds);
    }
    fn register_builtin(env: &mut Environment<Type>) {
        let binds = builtin::get_builtin_fns()
            .iter()
            .map(|(name, _, t)| (name.to_symbol(), t.clone()))
            .collect::<Vec<_>>();
        env.add_bind(&binds);
    }
    pub fn gen_intermediate_type(&mut self) -> Type {
        let res = Type::Intermediate(self.interm_idx);
        self.interm_idx += 1;
        res
    }
    pub fn convert_unknown_to_intermediate(&mut self, t: &Type) -> Type {
        match t {
            Type::Unknown => self.gen_intermediate_type(),
            _ => t.clone(),
        }
    }
    pub fn convert_unknown_function(
        &mut self,
        atypes: &[Type],
        rty: &Type,
        s: &Option<Box<Type>>,
    ) -> Type {
        let a = atypes
            .iter()
            .map(|a| self.convert_unknown_to_intermediate(a))
            .collect();
        let r = self.convert_unknown_to_intermediate(rty);
        Type::Function(a, Box::new(r), s.clone())
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
                        .map_or(false, |r| self.occur_check(id2, r.clone()))
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

    pub fn unify_types(&mut self, t1: Type, t2: Type) -> Result<Type, Error> {
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
                    Err(Error(ErrorKind::CircularType, 0..0)) //todo:span
                } else if i1 < i2 {
                    self.subst_map.insert(i1, t2);
                    Ok(t1)
                } else {
                    self.subst_map.insert(i2, t1);
                    Ok(t2)
                }
            }
            (Type::Intermediate(i), t) | (t, Type::Intermediate(i)) => {
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
            (Type::Primitive(p1), Type::Primitive(p2)) if p1 == p2 => Ok(Type::Primitive(p1)),

            (Type::Code(_p1), Type::Code(_p2)) => {
                todo!("type system for multi-stage computation has not implemented yet")
            }
            (p1, p2) => Err(Error(ErrorKind::TypeMismatch(p1.clone(), p2.clone()), 0..0)), //todo:span
        }
    }
    pub fn bind_pattern(&mut self, t: &Type, pat: &WithMeta<TypedPattern>) -> Result<Type, Error> {
        let WithMeta(TypedPattern { pat, ty: _ }, span) = pat;
        match pat {
            Pattern::Single(id) => {
                self.env.add_bind(&[(*id, t.clone())]);
                Ok(t.clone())
            }
            Pattern::Tuple(pats) => {
                let res = pats
                    .iter()
                    .map(|p| {
                        let ity = self.gen_intermediate_type();
                        let p = WithMeta(
                            TypedPattern {
                                pat: p.clone(),
                                ty: Some(ity.clone()),
                            },
                            span.clone(),
                        );
                        self.bind_pattern(&ity, &p)
                    })
                    .try_collect::<Vec<_>>()?;

                Ok(Type::Tuple(res))
            }
        }
    }
}

pub fn infer_type_literal(e: &Literal) -> Result<Type, Error> {
    let pt = match e {
        Literal::Float(_s) => PType::Numeric,
        Literal::Int(_s) => PType::Int,
        Literal::String(_s) => PType::String,
        Literal::Now => PType::Numeric,
        Literal::SelfLit => panic!("\"self\" should not be shown at type inference stage"),
    };
    Ok(Type::Primitive(pt))
}
pub fn lookup(name: &Symbol, ctx: &mut InferContext, span: &Span) -> Result<Type, Error> {
    ctx.env.lookup(name).map_or_else(
        || {
            println!("{:#?}", ctx.env);
            Err(Error(
                ErrorKind::VariableNotFound(name.to_string()),
                span.clone(),
            ))
        }, //todo:Span
        |v| Ok(v.clone()),
    )
}

pub fn infer_type(e_meta: ExprId, ctx: &mut InferContext) -> Result<Type, Error> {
    let span = e_meta.to_span().clone();
    let infer_vec = |e: &[ExprId], ctx: &mut InferContext| {
        e.iter()
            .map(|e| infer_type(*e, ctx))
            .collect::<Result<Vec<_>, Error>>()
    };

    match e_meta.to_expr() {
        Expr::Literal(l) => infer_type_literal(l),
        Expr::Tuple(e) => Ok(Type::Tuple(infer_vec(e.as_slice(), ctx)?)),
        Expr::Proj(e, idx) => {
            let tup = infer_type(*e, ctx)?;
            match tup {
                Type::Tuple(vec) => {
                    if vec.len() < *idx as usize {
                        Err(Error(
                            ErrorKind::IndexOutOfRange(vec.len() as u16, *idx as u16),
                            e.to_span().clone(),
                        ))
                    } else {
                        Ok(vec[*idx as usize].clone())
                    }
                }
                _ => Err(Error(ErrorKind::IndexForNonTuple, e.to_span().clone())),
            }
        }
        Expr::Feed(id, body) => {
            let feedv = ctx.gen_intermediate_type();
            ctx.env.add_bind(&[(*id, feedv.clone())]);
            let b = infer_type(*body, ctx);
            let res = ctx.unify_types(b?, feedv)?;
            if res.is_primitive() {
                Ok(res)
            } else {
                Err(Error(ErrorKind::NonPrimitiveInFeed, body.to_span().clone()))
            }
        }
        Expr::Lambda(p, rtype, body) => {
            ctx.env.extend();
            let ptypes: Vec<Type> = p
                .iter()
                .map(|WithMeta(id, _s)| {
                    let pt = id.ty.clone().unwrap_or(ctx.gen_intermediate_type());
                    ctx.env.add_bind(&[(id.id, pt.clone())]);
                    pt
                })
                .collect();
            let bty = if let Some(r) = rtype {
                let bty = infer_type(*body, ctx)?;
                ctx.unify_types(r.clone(), bty)?
            } else {
                infer_type(*body, ctx)?
            };
            ctx.env.to_outer();
            Ok(Type::Function(ptypes, Box::new(bty), None))
        }
        Expr::Let(tpat, body, then) => {
            let c = ctx;
            let bodyt = infer_type(*body, c)?;
            let idt = match tpat.0.ty.as_ref() {
                Some(Type::Function(atypes, box rty, s)) => {
                    c.convert_unknown_function(atypes, rty, s)
                }
                Some(t) => t.clone(),
                None => c.gen_intermediate_type(),
            };

            let bodyt_u = c.unify_types(idt, bodyt)?;
            let _ = c.bind_pattern(&bodyt_u, tpat)?;
            let res = match then {
                Some(e) => infer_type(*e, c),
                None => Ok(Type::Primitive(PType::Unit)),
            };
            res
        }
        Expr::LetRec(id, body, then) => {
            let c = ctx;
            let idt = match id.ty.as_ref() {
                Some(Type::Function(atypes, box rty, s)) => {
                    c.convert_unknown_function(atypes, rty, s)
                }
                _ => panic!("type for letrec is limited to function type in mimium."),
            };

            let body_i = c.gen_intermediate_type();
            c.env.add_bind(&[(id.id, body_i)]);
            let bodyt = infer_type(*body, c)?;
            let _ = c.unify_types(idt, bodyt)?;

            let res = match then {
                Some(e) => infer_type(*e, c),
                None => Ok(Type::Primitive(PType::Unit)),
            };
            res
        }
        Expr::Var(name, _time) => lookup(name, ctx, &span),
        Expr::Apply(fun, callee) => {
            let fnl = infer_type(*fun, ctx)?;
            let callee_t = infer_vec(callee.as_slice(), ctx)?;
            let res_t = ctx.gen_intermediate_type();
            let fntype = Type::Function(callee_t, Box::new(res_t), None);
            let restype = ctx.unify_types(fnl, fntype)?;
            if let Type::Function(_, r, _) = restype {
                Ok(*r)
            } else {
                unreachable!();
            }
        }
        Expr::If(cond, then, opt_else) => {
            let condt = infer_type(*cond, ctx)?;
            let _bt = ctx.unify_types(Type::Primitive(PType::Int), condt); //todo:boolean type
            let thent = infer_type(*then, ctx)?;
            let elset =
                opt_else.map_or(Ok(Type::Primitive(PType::Unit)), |e| infer_type(e, ctx))?;
            ctx.unify_types(thent, elset)
        }
        Expr::Block(expr) => expr.map_or(Ok(Type::Primitive(PType::Unit)), |e| infer_type(e, ctx)),
        _ => {
            // todo!();
            Ok(Type::Primitive(PType::Unit))
        }
    }
}
