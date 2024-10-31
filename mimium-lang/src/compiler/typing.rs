use crate::ast::{Expr, Literal};
use crate::compiler::intrinsics;
use crate::interner::{ExprKey, ExprNodeId, Symbol, ToSymbol, TypeNodeId};
use crate::pattern::{Pattern, TypedPattern};
use crate::types::{PType, Type, TypeVar};
use crate::utils::{environment::Environment, error::ReportableError, metadata::Span};
use crate::{function, integer, numeric, unit};
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::fmt;
use std::rc::Rc;

//todo: span for 2 different locations
#[derive(Clone, Debug, PartialEq)]
pub enum ErrorKind {
    TypeMismatch(Type, Type),
    PatternMismatch(Type, Pattern),
    NonFunctionForLetRec(Type),
    NonFunctionForApply(Type),
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
            ErrorKind::TypeMismatch(e, a) => write!(
                f,
                "Type Mismatch, between {} and {}",
                e.to_string_for_error(),
                a.to_string_for_error()
            ),
            ErrorKind::PatternMismatch(e, p) => write!(
                f,
                "Pattern {p} cannot have {} type.",
                e.to_string_for_error()
            ),

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
            ErrorKind::NonFunctionForApply(t) => write!(
                f,
                "{} is not applicable because it is not a function type.",
                t.to_string_for_error()
            ),
            ErrorKind::NonFunctionForLetRec(t) => write!(
                f,
                "\"letrec\" requires the expression to be function type but it was {} type.",
                t.to_string_for_error()
            ),
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
    interm_idx: u64,
    typescheme_idx: u64,
    instantiated_idx: u64,
    level: u64,
    subst_map: BTreeMap<i64, TypeNodeId>,
    generalize_map: BTreeMap<u64, u64>,
    instantiate_map: BTreeMap<u64, u64>,
    result_map: BTreeMap<ExprKey, TypeNodeId>,
    pub env: Environment<TypeNodeId>, // interm_map:HashMap<i64,Type>
}
impl InferContext {
    fn new(builtins: &[(Symbol, TypeNodeId)]) -> Self {
        let mut res = Self {
            interm_idx: 0,
            typescheme_idx: 0,
            instantiated_idx: 0,
            level: 0,
            subst_map: Default::default(),
            generalize_map: Default::default(),
            instantiate_map: Default::default(),
            result_map: Default::default(),
            env: Environment::<TypeNodeId>::new(),
        };
        res.env.extend();
        res.env.add_bind(&Self::intrinsic_types());
        res.env.add_bind(builtins);
        res
    }
}
impl InferContext {
    fn intrinsic_types() -> Vec<(Symbol, TypeNodeId)> {
        let binop_ty = function!(vec![numeric!(), numeric!()], numeric!());
        let binop_names = [
            intrinsics::ADD,
            intrinsics::SUB,
            intrinsics::MULT,
            intrinsics::DIV,
            intrinsics::MODULO,
            intrinsics::POW,
            intrinsics::GT,
            intrinsics::LT,
            intrinsics::GE,
            intrinsics::LE,
            intrinsics::EQ,
            intrinsics::NE,
        ];
        let uniop_ty = function!(vec![numeric!()], numeric!());
        let uniop_names = [
            intrinsics::NEG,
            intrinsics::MEM,
            intrinsics::SIN,
            intrinsics::COS,
            intrinsics::ABS,
            intrinsics::SQRT,
        ];

        let mut binds = binop_names
            .iter()
            .map(|n| (n.to_symbol(), binop_ty))
            .collect::<Vec<(Symbol, TypeNodeId)>>();
        uniop_names
            .iter()
            .map(|n| (n.to_symbol(), uniop_ty))
            .collect_into(&mut binds);
        binds.extend_from_slice(&[
            (
                intrinsics::DELAY.to_symbol(),
                function!(vec![numeric!(), numeric!(), numeric!()], numeric!()),
            ),
            (
                intrinsics::TOFLOAT.to_symbol(),
                function!(vec![integer!()], numeric!()),
            ),
        ]);

        binds
    }
    fn gen_intermediate_type(&mut self) -> TypeNodeId {
        let res = Type::Intermediate(Rc::new(RefCell::new(TypeVar::new(
            self.interm_idx,
            self.level,
        ))))
        .into_id();
        self.interm_idx += 1;
        res
    }
    fn get_typescheme(&mut self, tvid: u64) -> TypeNodeId {
        self.generalize_map.get(&tvid).cloned().map_or_else(
            || self.gen_typescheme(),
            |id| Type::TypeScheme(id).into_id(),
        )
    }
    fn gen_typescheme(&mut self) -> TypeNodeId {
        let res = Type::TypeScheme(self.typescheme_idx).into_id();
        self.typescheme_idx += 1;
        res
    }
    fn gen_instantiated(&mut self) -> TypeNodeId {
        let res = Type::Instantiated(self.instantiated_idx).into_id();
        self.instantiated_idx += 1;
        res
    }
    fn gen_intermediate_type_with_span(&mut self, span: Span) -> TypeNodeId {
        let res = Type::Intermediate(Rc::new(RefCell::new(TypeVar::new(
            self.interm_idx,
            self.level,
        ))))
        .into_id_with_span(span);
        self.interm_idx += 1;
        res
    }
    fn convert_unknown_to_intermediate(&mut self, t: TypeNodeId) -> TypeNodeId {
        match t.to_type() {
            Type::Unknown => self.gen_intermediate_type(),
            _ => t,
        }
    }
    fn convert_unknown_function(
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
    fn occur_check(id1: u64, t2: TypeNodeId) -> bool {
        let cls = |t2dash: TypeNodeId| -> bool { Self::occur_check(id1, t2dash) };

        let vec_cls = |t: &[_]| -> bool { t.iter().any(|a| cls(*a)) };

        match &t2.to_type() {
            Type::Intermediate(cell) => cell
                .try_borrow()
                .map(|tv2| match tv2.parent {
                    Some(tid2) => id1 == tv2.var || Self::occur_check(id1, tid2),
                    None => id1 == tv2.var,
                })
                .unwrap_or(true),
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

    fn substitute_type(t: TypeNodeId) -> TypeNodeId {
        match t.to_type() {
            Type::Intermediate(cell) => {
                let TypeVar { parent, .. } = &cell.borrow() as &TypeVar;
                match parent {
                    Some(p) => Self::substitute_type(*p),
                    None => t,
                }
            }
            _ => t.apply_fn(Self::substitute_type),
        }
    }
    fn substitute_all_intermediates(&mut self) {
        let mut e_list = self
            .result_map
            .iter()
            .map(|(e, t)| (*e, Self::substitute_type(*t)))
            .collect::<Vec<_>>();

        e_list.iter_mut().for_each(|(e, t)| {
            log::trace!("e: {:?} t: {}", e, t.to_type());
            let _old = self.result_map.insert(*e, *t);
        })
    }

    fn unify_types(t1: TypeNodeId, t2: TypeNodeId, span: Span) -> Result<TypeNodeId, Error> {
        let unify_vec = |a1: &[TypeNodeId], a2: &[TypeNodeId]| -> Result<Vec<_>, Error> {
            if a1.len() != a2.len() {
                return Err(Error(
                    ErrorKind::TypeMismatch(t1.to_type(), t2.to_type()),
                    span.clone(),
                ));
            }

            a1.iter()
                .zip(a2.iter())
                .map(|(v1, v2)| Self::unify_types(*v1, *v2, span.clone()))
                .try_collect()
        };
        log::trace!("unify {} and {}", t1.to_type(), t2.to_type());
        let t1r = t1.get_root();
        let t2r = t2.get_root();
        match &(t1r.to_type(), t2r.to_type()) {
            (Type::Intermediate(i1), Type::Intermediate(i2)) => {
                let tv1 = &mut i1.borrow_mut() as &mut TypeVar;
                if Self::occur_check(tv1.var, t2) {
                    return Ok(t1r);
                }
                let tv2 = &mut i2.borrow_mut() as &mut TypeVar;
                if tv1.level != tv2.level {
                    let l = tv1.level.min(tv2.level);
                    tv1.level = l;
                    tv2.level = l;
                }
                match (tv1.parent, tv2.parent) {
                    (None, None) => {
                        if tv1.var > tv2.var {
                            tv2.parent = Some(t1r);
                            Ok(t1r)
                        } else {
                            tv1.parent = Some(t2r);
                            Ok(t2r)
                        }
                    }
                    (_, Some(p2)) => {
                        tv1.parent = Some(p2);
                        Ok(p2)
                    }
                    (Some(p1), _) => {
                        tv2.parent = Some(p1);
                        Ok(p1)
                    }
                }
            }
            (Type::Intermediate(i1), _) => {
                let tv1 = &mut i1.borrow_mut() as &mut TypeVar;
                tv1.parent = Some(t2r);
                Ok(t2r)
            }
            (_, Type::Intermediate(i2)) => {
                let tv2 = &mut i2.borrow_mut() as &mut TypeVar;
                tv2.parent = Some(t1r);
                Ok(t1r)
            }
            (t1, Type::Instantiated(_)) => Ok(t1.clone().into_id_with_span(span)),
            (Type::Instantiated(_), t2) => Ok(t2.clone().into_id_with_span(span)),
            (Type::Array(a1), Type::Array(a2)) => {
                Ok(Type::Array(Self::unify_types(*a1, *a2, span)?).into_id())
            }
            (Type::Ref(x1), Type::Ref(x2)) => {
                Ok(Type::Ref(Self::unify_types(*x1, *x2, span)?).into_id())
            }
            (Type::Tuple(a1), Type::Tuple(a2)) => {
                Ok(Type::Tuple(unify_vec(a1, a2)?).into_id_with_span(span))
            }
            (Type::Struct(_a1), Type::Struct(_a2)) => todo!(), //todo
            (Type::Function(p1, r1, s1), Type::Function(p2, r2, s2)) => Ok(Type::Function(
                unify_vec(p1, p2)?,
                Self::unify_types(*r1, *r2, span.clone())?,
                match (s1, s2) {
                    (Some(e1), Some(e2)) => Some(Self::unify_types(*e1, *e2, span)?),
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
            (p1, p2) => Err(Error(ErrorKind::TypeMismatch(p1.clone(), p2.clone()), span)),
        }
    }
    fn generalize(&mut self, t: TypeNodeId) -> TypeNodeId {
        match t.to_type() {
            Type::Intermediate(tvar) => {
                let &TypeVar { level, var, .. } = &tvar.borrow() as _;
                if level > self.level {
                    self.get_typescheme(var)
                } else {
                    t
                }
            }
            _ => t.apply_fn(|t| self.generalize(t)),
        }
    }
    fn instantiate(&mut self, t: TypeNodeId) -> TypeNodeId {
        let mut g_i_map = BTreeMap::<u64, TypeNodeId>::default();
        self.instantiate_in(t, &mut g_i_map)
    }
    fn instantiate_in(
        &mut self,
        t: TypeNodeId,
        g_i_map: &mut BTreeMap<u64, TypeNodeId>,
    ) -> TypeNodeId {
        match t.to_type() {
            Type::TypeScheme(id) => g_i_map
                .get(&id)
                .cloned()
                .unwrap_or_else(|| self.gen_instantiated()),
            _ => t.apply_fn(|t| self.instantiate_in(t, g_i_map)),
        }
    }

    // Note: the third argument `span` is used for the error location in case of
    // type mismatch. This is needed because `t`'s span refers to the location
    // where it originally defined (e.g. the explicit return type of the
    // function) and is not necessarily the same as where the error happens.
    fn bind_pattern(
        &mut self,
        t: TypeNodeId,
        ty_pat: &TypedPattern,
        span: Span,
    ) -> Result<TypeNodeId, Error> {
        let TypedPattern { pat, .. } = ty_pat;
        let pat_t = match pat {
            Pattern::Single(id) => {
                let gt = self.generalize(t);
                self.env.add_bind(&[(*id, gt)]);
                Ok(t)
            }
            Pattern::Tuple(pats) => {
                let res = pats
                    .iter()
                    .map(|p| {
                        let ity = self.gen_intermediate_type_with_span(ty_pat.to_span());
                        let p = TypedPattern {
                            pat: p.clone(),
                            ty: ity,
                        };
                        self.bind_pattern(ity, &p, span.clone())
                    })
                    .try_collect::<Vec<_>>()?;
                Ok(Type::Tuple(res).into_id())
            }
        }?;
        Self::unify_types(t, pat_t, span)
    }

    pub fn lookup(&self, name: &Symbol, span: &Span) -> Result<TypeNodeId, Error> {
        self.env.lookup(name).map_or_else(
            || {
                Err(Error(
                    ErrorKind::VariableNotFound(name.to_string()),
                    span.clone(),
                ))
            }, //todo:Span
            |v| Ok(*v),
        )
    }
    pub(crate) fn infer_type_literal(e: &Literal) -> Result<TypeNodeId, Error> {
        let pt = match e {
            Literal::Float(_s) => PType::Numeric,
            Literal::Int(_s) => PType::Int,
            Literal::String(_s) => PType::String,
            Literal::Now => PType::Numeric,
            Literal::SelfLit => panic!("\"self\" should not be shown at type inference stage"),
            Literal::PlaceHolder => panic!("\"_\" should not be shown at type inference stage"),
        };
        Ok(Type::Primitive(pt).into_id())
    }
    fn infer_vec(&mut self, e: &[ExprNodeId]) -> Result<Vec<TypeNodeId>, Error> {
        e.iter().map(|e| self.infer_type(*e)).try_collect()
    }
    fn infer_type_levelup(&mut self, e: ExprNodeId) -> Result<TypeNodeId, Error> {
        self.level += 1;
        let res = self.infer_type(e);
        self.level -= 1;
        res
    }
    fn infer_type(&mut self, e: ExprNodeId) -> Result<TypeNodeId, Error> {
        let span = e.to_span().clone();
        let res = match &e.to_expr() {
            Expr::Literal(l) => Self::infer_type_literal(l),
            Expr::Tuple(e) => Ok(Type::Tuple(self.infer_vec(e.as_slice())?).into_id()),
            Expr::Proj(e, idx) => {
                let tup = self.infer_type(*e)?;
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
                let feedv = self.gen_intermediate_type();
                self.env.add_bind(&[(*id, feedv)]);
                let b = self.infer_type(*body)?;
                let res = Self::unify_types(b, feedv, span)?;
                if res.to_type().contains_function() {
                    Err(Error(ErrorKind::NonPrimitiveInFeed, body.to_span().clone()))
                } else {
                    Ok(res)
                }
            }
            Expr::Lambda(p, rtype, body) => {
                self.env.extend();
                let ptypes: Vec<TypeNodeId> = p
                    .iter()
                    .map(|id| {
                        let pt = if !id.is_unknown() {
                            id.ty
                        } else {
                            self.gen_intermediate_type()
                        };
                        self.env.add_bind(&[(id.id, pt)]);
                        pt
                    })
                    .collect();
                let bty = if let Some(r) = rtype {
                    let bty = self.infer_type(*body)?;
                    Self::unify_types(*r, bty, body.to_span())?
                } else {
                    self.infer_type(*body)?
                };
                self.env.to_outer();
                Ok(Type::Function(ptypes, bty, None).into_id())
            }
            Expr::Let(tpat, body, then) => {
                let bodyt = self.infer_type_levelup(*body)?;

                let _ = self.bind_pattern(bodyt, tpat, body.to_span())?;

                match then {
                    Some(e) => self.infer_type(*e),
                    None => Ok(Type::Primitive(PType::Unit).into_id()),
                }
            }
            Expr::LetRec(id, body, then) => {
                let t = id.ty.to_type();
                let idt = match (id.is_unknown(), &t) {
                    (false, Type::Function(atypes, rty, s)) => {
                        self.convert_unknown_function(atypes, *rty, *s)
                    }
                    (false, _) => {
                        return Err(Error(ErrorKind::NonFunctionForLetRec(t.clone()), span))
                    }
                    (true, _) => self.gen_intermediate_type(),
                };
                self.env.add_bind(&[(id.id, idt)]);
                //polymorphic inference is not allowed in recursive function.
                let bodyt = self.infer_type_levelup(*body)?;
                let _ = Self::unify_types(idt, bodyt, body.to_span())?;
                match then {
                    Some(e) => self.infer_type(*e),
                    None => Ok(Type::Primitive(PType::Unit).into_id()),
                }
            }
            Expr::Assign(assignee, expr) => {
                let name = match assignee.to_expr() {
                    Expr::Var(v) => v,
                    Expr::ArrayAccess(_, _) => {
                        unimplemented!("Assignment to array is not implemented yet.")
                    }
                    _ => unreachable!(),
                };
                let assignee_t = self.lookup(&name, &span)?;
                let e_t = self.infer_type(*expr)?;
                Self::unify_types(assignee_t, e_t, expr.to_span())?;
                Ok(unit!())
            }
            Expr::Then(e, then) => {
                let _ = self.infer_type(*e)?;
                then.map_or(Ok(unit!()), |t| self.infer_type(t))
            }
            Expr::Var(name) => {
                let res = self.lookup(name, &span)?;
                // log::debug!("{} {} /level{}", name.as_str(), res, self.level);
                Ok(self.instantiate(res))
            }
            Expr::Apply(fun, callee) => {
                let fnl = self.infer_type(*fun)?;
                let callee_t = self.infer_vec(callee.as_slice())?;
                let res_t = self.gen_intermediate_type();
                let fntype = Type::Function(callee_t, res_t, None).into_id();
                let restype = Self::unify_types(fnl, fntype, span.clone())?;
                if let Type::Function(_, r, _) = restype.to_type() {
                    Ok(r)
                } else {
                    Err(Error(
                        ErrorKind::NonFunctionForApply(restype.to_type().clone()),
                        span.clone(),
                    ))
                }
            }
            Expr::If(cond, then, opt_else) => {
                let condt = self.infer_type(*cond)?;
                let cond_span = cond.to_span();
                let _bt = Self::unify_types(
                    Type::Primitive(PType::Numeric).into_id_with_span(cond_span.clone()),
                    condt,
                    cond_span,
                ); //todo:boolean type
                let thent = self.infer_type(*then)?;
                let elset = opt_else.map_or(Ok(Type::Primitive(PType::Unit).into_id()), |e| {
                    self.infer_type(e)
                })?;
                let else_span = opt_else.map_or(span.end..span.end, |e| e.to_span());
                log::trace!("then: {}, else: {}", thent.to_type(), elset.to_type());
                Self::unify_types(thent, elset, else_span)
            }
            Expr::Block(expr) => expr.map_or(Ok(Type::Primitive(PType::Unit).into_id()), |e| {
                self.infer_type(e)
            }),
            _ => {
                // todo!();
                Ok(Type::Primitive(PType::Unit).into_id())
            }
        };
        res.inspect(|ty| {
            self.result_map.insert(e.0, *ty);
        })
    }
    pub fn lookup_res(&self, e: ExprNodeId) -> TypeNodeId {
        *self.result_map.get(&e.0).expect("type inference failed")
    }
}

pub fn infer_root(e: ExprNodeId,builtin_types:&[(Symbol,TypeNodeId)]) -> Result<InferContext, Error> {
    let mut ctx = InferContext::new(builtin_types);
    let _ = ctx.infer_type(e)?;
    ctx.substitute_all_intermediates();
    Ok(ctx)
}
