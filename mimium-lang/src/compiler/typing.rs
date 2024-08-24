use crate::ast::{Expr, Literal};
use crate::interner::{ExprNodeId, Symbol, ToSymbol, TypeNodeId};
use crate::pattern::{Pattern, TypedPattern};
use crate::predefined::symbols::{intrinsic, special_fn};
use crate::runtime::vm::builtin;
use crate::types::{PType, Type};
use crate::utils::{environment::Environment, error::ReportableError, metadata::Span};
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
    subst_map: HashMap<i64, TypeNodeId>,
    pub env: Environment<TypeNodeId>, // interm_map:HashMap<i64,Type>
}

impl InferContext {
    pub fn new() -> Self {
        let mut res = Self {
            interm_idx: 0,
            subst_map: HashMap::<i64, TypeNodeId>::new(),
            env: Environment::<TypeNodeId>::new(),
        };
        res.env.extend();
        Self::register_intrinsics(&mut res.env);
        Self::register_builtin(&mut res.env);

        res
    }
    fn register_intrinsics(env: &mut Environment<TypeNodeId>) {
        let binop_ty = function!(vec![numeric!(), numeric!()], numeric!());
        let binop_names = vec![
            intrinsic::ADD,
            intrinsic::SUB,
            intrinsic::MULT,
            intrinsic::DIV,
            intrinsic::MODULO,
            intrinsic::EXP,
            intrinsic::GT,
            intrinsic::LT,
            intrinsic::GE,
            intrinsic::LE,
            intrinsic::EQ,
            intrinsic::NE,
        ];
        let uniop_ty = function!(vec![numeric!()], numeric!());
        let uniop_names = vec![
            intrinsic::NEG,
            special_fn::MEM,
            intrinsic::SIN,
            intrinsic::COS,
            intrinsic::ABS,
            intrinsic::SQRT,
        ];
        let mut binds = binop_names
            .iter()
            .map(|n| (*n, binop_ty))
            .collect::<Vec<(Symbol, TypeNodeId)>>();
        uniop_names
            .iter()
            .map(|n| (*n, uniop_ty))
            .collect_into(&mut binds);
        binds.push((
            special_fn::DELAY,
            function!(vec![numeric!(), numeric!(), numeric!()], numeric!()),
        ));
        env.add_bind(&binds);
    }
    fn register_builtin(env: &mut Environment<TypeNodeId>) {
        let binds = builtin::get_builtin_fns()
            .iter()
            .map(|(name, _, t)| (name.to_symbol(), *t))
            .collect::<Vec<_>>();
        env.add_bind(&binds);
    }
    pub fn gen_intermediate_type(&mut self) -> TypeNodeId {
        let res = Type::Intermediate(self.interm_idx).into_id();
        self.interm_idx += 1;
        res
    }
    pub fn gen_intermediate_type_with_span(&mut self, span: Span) -> TypeNodeId {
        let res = Type::Intermediate(self.interm_idx).into_id_with_span(span);
        self.interm_idx += 1;
        res
    }
    pub fn convert_unknown_to_intermediate(&mut self, t: TypeNodeId) -> TypeNodeId {
        match t.to_type() {
            Type::Unknown => self.gen_intermediate_type(),
            _ => t,
        }
    }
    pub fn convert_unknown_function(
        &mut self,
        atypes: &[TypeNodeId],
        rty: TypeNodeId,
        s: Option<TypeNodeId>,
    ) -> TypeNodeId {
        let a = atypes
            .iter()
            .map(|a| self.convert_unknown_to_intermediate(*a))
            .collect();
        let r = self.convert_unknown_to_intermediate(rty);
        Type::Function(a, r, s).into_id()
    }
    // return true when the circular loop of intermediate variable exists.
    pub fn occur_check(&self, id1: i64, t2: TypeNodeId) -> bool {
        let cls = |t2dash: TypeNodeId| -> bool { self.occur_check(id1, t2dash) };

        let vec_cls = |t: &[_]| -> bool { t.iter().all(|a| cls(*a)) };

        match &t2.to_type() {
            Type::Intermediate(id2) => {
                if id1 == *id2 {
                    true
                } else {
                    self.subst_map
                        .get(&id1)
                        .map_or(false, |r| self.occur_check(*id2, *r))
                }
            }
            Type::Array(a) => cls(*a),
            Type::Tuple(t) => vec_cls(t),
            Type::Function(p, r, s) => {
                vec_cls(p)
                    && cls(*r)
                    && cls(s.map(|x| x).unwrap_or_else(|| Type::Unknown.into_id()))
            }
            Type::Struct(_s) => todo!(),
            _ => false,
        }
    }

    fn substitute_intermediate_type(&self, id: i64) -> Option<TypeNodeId> {
        match self.subst_map.get(&id) {
            Some(t) => match t.to_type() {
                Type::Intermediate(i) => self.substitute_intermediate_type(i),
                _ => Some(*t),
            },
            None => None,
        }
    }
    pub fn substitute_type(&self, t: TypeNodeId) -> Option<TypeNodeId> {
        match t.to_type() {
            Type::Intermediate(id) => self.substitute_intermediate_type(id),
            _ => Some(t.apply_fn(|e| match self.substitute_type(e) {
                Some(tid) => tid,
                None => Type::Unknown.into_id(),
            })),
        }
    }

    pub fn unify_types(&mut self, t1: TypeNodeId, t2: TypeNodeId) -> Result<TypeNodeId, Error> {
        let mut unify_vec = |a1: Vec<TypeNodeId>, a2: Vec<TypeNodeId>| -> Result<Vec<_>, Error> {
            a1.clone()
                .iter()
                .zip(a2.iter())
                .map(|(v1, v2)| self.unify_types(*v1, *v2))
                .collect()
        };
        match &(t1.to_type(), t2.to_type()) {
            (Type::Intermediate(i1), Type::Intermediate(i2)) => {
                if self.occur_check(*i1, t2) {
                    Err(Error(ErrorKind::CircularType, 0..0)) //todo:span
                } else if i1 < i2 {
                    self.subst_map.insert(*i1, t2);
                    Ok(t1)
                } else {
                    self.subst_map.insert(*i2, t1);
                    Ok(t2)
                }
            }
            (Type::Intermediate(i), t) | (t, Type::Intermediate(i)) => {
                let tid = t.clone().into_id();
                self.subst_map.insert(*i, tid);
                Ok(tid)
            }
            (Type::Array(a1), Type::Array(a2)) => {
                Ok(Type::Array(self.unify_types(*a1, *a2)?).into_id())
            }
            (Type::Ref(x1), Type::Ref(x2)) => Ok(Type::Ref(self.unify_types(*x1, *x2)?).into_id()),
            (Type::Tuple(a1), Type::Tuple(a2)) => {
                Ok(Type::Tuple(unify_vec(a1.clone(), a2.clone())?).into_id())
            }
            (Type::Struct(_a1), Type::Struct(_a2)) => todo!(), //todo
            (Type::Function(p1, r1, s1), Type::Function(p2, r2, s2)) => Ok(Type::Function(
                unify_vec(p1.clone(), p2.clone())?,
                self.unify_types(*r1, *r2)?,
                match (s1, s2) {
                    (Some(e1), Some(e2)) => Some(self.unify_types(*e1, *e2)?),
                    (None, None) => None,
                    (_, _) => todo!("error handling"),
                },
            )
            .into_id()),
            (Type::Primitive(p1), Type::Primitive(p2)) if p1 == p2 => {
                Ok(Type::Primitive(p1.clone()).into_id())
            }

            (Type::Code(_p1), Type::Code(_p2)) => {
                todo!("type system for multi-stage computation has not implemented yet")
            }
            (p1, p2) => Err(Error(ErrorKind::TypeMismatch(p1.clone(), p2.clone()), 0..0)), //todo:span
        }
    }
    pub fn bind_pattern(
        &mut self,
        t: TypeNodeId,
        ty_pat: &TypedPattern,
    ) -> Result<TypeNodeId, Error> {
        let TypedPattern { pat, .. } = ty_pat;
        let span = ty_pat.to_span();
        match pat {
            Pattern::Single(id) => {
                self.env.add_bind(&[(*id, t)]);
                Ok(t)
            }
            Pattern::Tuple(pats) => {
                let res = pats
                    .iter()
                    .map(|p| {
                        let ity = self.gen_intermediate_type_with_span(span.clone());
                        let p = TypedPattern {
                            pat: p.clone(),
                            ty: ity,
                        };
                        self.bind_pattern(ity, &p)
                    })
                    .try_collect::<Vec<_>>()?;

                Ok(Type::Tuple(res).into_id())
            }
        }
    }
}

pub fn infer_type_literal(e: &Literal) -> Result<TypeNodeId, Error> {
    let pt = match e {
        Literal::Float(_s) => PType::Numeric,
        Literal::Int(_s) => PType::Int,
        Literal::String(_s) => PType::String,
        Literal::Now => PType::Numeric,
        Literal::SelfLit => panic!("\"self\" should not be shown at type inference stage"),
    };
    Ok(Type::Primitive(pt).into_id())
}
pub fn lookup(name: &Symbol, ctx: &mut InferContext, span: &Span) -> Result<TypeNodeId, Error> {
    ctx.env.lookup(name).map_or_else(
        || {
            println!("{:#?}", ctx.env);
            Err(Error(
                ErrorKind::VariableNotFound(name.to_string()),
                span.clone(),
            ))
        }, //todo:Span
        |v| Ok(*v),
    )
}

pub fn infer_type(e_meta: ExprNodeId, ctx: &mut InferContext) -> Result<TypeNodeId, Error> {
    let span = e_meta.to_span().clone();
    let infer_vec = |e: &[ExprNodeId], ctx: &mut InferContext| {
        e.iter()
            .map(|e| infer_type(*e, ctx))
            .collect::<Result<Vec<_>, Error>>()
    };

    match &e_meta.to_expr() {
        Expr::Literal(l) => infer_type_literal(l),
        Expr::Tuple(e) => Ok(Type::Tuple(infer_vec(e.as_slice(), ctx)?).into_id()),
        Expr::Proj(e, idx) => {
            let tup = infer_type(*e, ctx)?;
            match tup.to_type() {
                Type::Tuple(vec) => {
                    if vec.len() < *idx as usize {
                        Err(Error(
                            ErrorKind::IndexOutOfRange(vec.len() as u16, *idx as u16),
                            e.to_span().clone(),
                        ))
                    } else {
                        Ok(vec[*idx as usize])
                    }
                }
                _ => Err(Error(ErrorKind::IndexForNonTuple, e.to_span().clone())),
            }
        }
        Expr::Feed(id, body) => {
            let feedv = ctx.gen_intermediate_type();
            ctx.env.add_bind(&[(*id, feedv)]);
            let b = infer_type(*body, ctx);
            let res = ctx.unify_types(b?, feedv)?;
            if res.to_type().is_primitive() {
                Ok(res)
            } else {
                Err(Error(ErrorKind::NonPrimitiveInFeed, body.to_span().clone()))
            }
        }
        Expr::Lambda(p, rtype, body) => {
            ctx.env.extend();
            let ptypes: Vec<TypeNodeId> = p
                .iter()
                .map(|id| {
                    let pt = if !id.is_unknown() {
                        id.ty
                    } else {
                        ctx.gen_intermediate_type()
                    };
                    ctx.env.add_bind(&[(id.id, pt)]);
                    pt
                })
                .collect();
            let bty = if let Some(r) = rtype {
                let bty = infer_type(*body, ctx)?;
                ctx.unify_types(*r, bty)?
            } else {
                infer_type(*body, ctx)?
            };
            ctx.env.to_outer();
            Ok(Type::Function(ptypes, bty, None).into_id())
        }
        Expr::Let(tpat, body, then) => {
            let c = ctx;
            let bodyt = infer_type(*body, c)?;
            let idt = if !tpat.is_unknown() {
                match tpat.ty.to_type() {
                    Type::Function(atypes, rty, s) => c.convert_unknown_function(&atypes, rty, s),
                    _ => tpat.ty,
                }
            } else {
                c.gen_intermediate_type()
            };

            let bodyt_u = c.unify_types(idt, bodyt)?;
            let _ = c.bind_pattern(bodyt_u, tpat)?;
            let res = match then {
                Some(e) => infer_type(*e, c),
                None => Ok(Type::Primitive(PType::Unit).into_id()),
            };
            res
        }
        Expr::LetRec(id, body, then) => {
            let c = ctx;
            let idt = match (id.is_unknown(), id.ty.to_type()) {
                (false, Type::Function(atypes, rty, s)) => {
                    c.convert_unknown_function(&atypes, rty, s)
                }
                _ => panic!("type for letrec is limited to function type in mimium."),
            };

            let body_i = c.gen_intermediate_type();
            c.env.add_bind(&[(id.id, body_i)]);
            let bodyt = infer_type(*body, c)?;
            let _ = c.unify_types(idt, bodyt)?;

            let res = match then {
                Some(e) => infer_type(*e, c),
                None => Ok(Type::Primitive(PType::Unit).into_id()),
            };
            res
        }
        Expr::Var(name, _time) => lookup(name, ctx, &span),
        Expr::Apply(fun, callee) => {
            let fnl = infer_type(*fun, ctx)?;
            let callee_t = infer_vec(callee.as_slice(), ctx)?;
            let res_t = ctx.gen_intermediate_type();
            let fntype = Type::Function(callee_t, res_t, None).into_id();
            let restype = ctx.unify_types(fnl, fntype)?;
            if let Type::Function(_, r, _) = restype.to_type() {
                Ok(r)
            } else {
                unreachable!();
            }
        }
        Expr::If(cond, then, opt_else) => {
            let condt = infer_type(*cond, ctx)?;
            let _bt = ctx.unify_types(Type::Primitive(PType::Int).into_id(), condt); //todo:boolean type
            let thent = infer_type(*then, ctx)?;
            let elset = opt_else.map_or(Ok(Type::Primitive(PType::Unit).into_id()), |e| {
                infer_type(e, ctx)
            })?;
            ctx.unify_types(thent, elset)
        }
        Expr::Block(expr) => expr.map_or(Ok(Type::Primitive(PType::Unit).into_id()), |e| {
            infer_type(e, ctx)
        }),
        _ => {
            // todo!();
            Ok(Type::Primitive(PType::Unit).into_id())
        }
    }
}
