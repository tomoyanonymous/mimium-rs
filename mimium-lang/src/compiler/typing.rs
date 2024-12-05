use crate::ast::{Expr, Literal};
use crate::compiler::intrinsics;
use crate::interner::{ExprKey, ExprNodeId, Symbol, ToSymbol, TypeNodeId};
use crate::pattern::{Pattern, TypedPattern};
use crate::types::{PType, Type, TypeVar};
use crate::utils::metadata::Location;
use crate::utils::{environment::Environment, error::ReportableError};
use crate::{function, integer, numeric, unit};
use itertools::{EitherOrBoth, Itertools};
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::fmt;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub enum Error {
    TypeMismatch {
        left: (TypeNodeId, Location),
        right: (TypeNodeId, Location),
    },
    LengthMismatch {
        left: (usize, Location),
        right: (usize, Location),
    },
    PatternMismatch((TypeNodeId, Location), (Pattern, Location)),
    NonFunctionForLetRec(TypeNodeId, Location),
    NonFunctionForApply(TypeNodeId, Location),
    CircularType(Location, Location),
    IndexOutOfRange {
        len: u16,
        idx: u16,
        loc: Location,
    },
    IndexForNonTuple(Location),
    VariableNotFound(Symbol, Location),
    NonPrimitiveInFeed(Location),
}
impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Type Inference Error")
    }
}

impl std::error::Error for Error {}
impl ReportableError for Error {
    fn get_message(&self) -> String {
        match self {
            Error::TypeMismatch { .. } => format!("Type mismatch"),
            Error::PatternMismatch(..) => format!("Pattern mismatch"),
            Error::LengthMismatch { .. } => format!("Length of the elements are different"),
            Error::NonFunctionForLetRec(_, _) => format!("`letrec` can take only function type."),
            Error::NonFunctionForApply(_, _) => {
                format!("This is not applicable because it is not a function type.")
            }
            Error::CircularType(_, _) => format!("Circular loop of type definition detected."),
            Error::IndexOutOfRange { len, idx, .. } => {
                format!("Length of tuple elements is {len} but index was {idx}")
            }
            Error::IndexForNonTuple(_) => format!("Index access for non-tuple variable."),
            Error::VariableNotFound(symbol, _) => {
                format!("Variable {} not found in this scope", symbol.to_string())
            }
            Error::NonPrimitiveInFeed(_) => {
                format!("Function that uses `self` cannot return function type.")
            }
        }
    }
    fn get_labels(&self) -> Vec<(Location, String)> {
        match self {
            Error::TypeMismatch {
                left: (lty, locl),
                right: (rty, locr),
            } => vec![
                (locl.clone(), lty.to_type().to_string_for_error()),
                (locr.clone(), rty.to_type().to_string_for_error()),
            ],
            Error::PatternMismatch((ty, loct), (pat, locp)) => vec![
                (loct.clone(), ty.to_type().to_string_for_error()),
                (locp.clone(), pat.to_string()),
            ],
            Error::LengthMismatch {
                left: (l, locl),
                right: (r, locr),
            } => vec![
                (locl.clone(), format!("The length is {l}")),
                (locr.clone(), format!("but the length for here is {r}")),
            ],
            Error::NonFunctionForLetRec(ty, loc) => {
                vec![(loc.clone(), ty.to_type().to_string_for_error())]
            }
            Error::NonFunctionForApply(ty, loc) => {
                vec![(loc.clone(), ty.to_type().to_string_for_error())]
            }
            Error::CircularType(loc1, loc2) => vec![
                (loc1.clone(), format!("Circular type happens here")),
                (loc2.clone(), format!("and here")),
            ],
            Error::IndexOutOfRange { loc, len, .. } => {
                vec![(loc.clone(), format!("Length for this tuple is {len}"))]
            }
            Error::IndexForNonTuple(loc) => vec![(loc.clone(), format!("This is not tuple type."))],
            Error::VariableNotFound(symbol, loc) => {
                vec![(loc.clone(), format!("{} is not defined", symbol))]
            }
            Error::NonPrimitiveInFeed(loc) => {
                vec![(loc.clone(), format!("This cannot be function type."))]
            }
        }
    }
}

pub struct InferResult {
    ty: TypeNodeId,
    errs: Vec<Error>,
}

#[derive(Clone, Debug)]
pub struct InferContext {
    interm_idx: u64,
    typescheme_idx: u64,
    instantiated_idx: u64,
    level: u64,
    subst_map: BTreeMap<i64, TypeNodeId>,
    generalize_map: BTreeMap<u64, u64>,
    instantiate_map: BTreeMap<u64, u64>,
    result_map: BTreeMap<ExprKey, TypeNodeId>,
    pub env: Environment<TypeNodeId>,
    errors: Vec<Error>,
}
impl InferContext {
    fn new(builtins: &[(Symbol, TypeNodeId)]) -> Self {
        let mut res = Self {
            interm_idx: Default::default(),
            typescheme_idx: Default::default(),
            instantiated_idx: Default::default(),
            level: Default::default(),
            subst_map: Default::default(),
            generalize_map: Default::default(),
            instantiate_map: Default::default(),
            result_map: Default::default(),
            env: Environment::<TypeNodeId>::default(),
            errors: Default::default(),
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

        let binds = binop_names.map(|n| (n.to_symbol(), binop_ty));
        let unibinds = uniop_names.map(|n| (n.to_symbol(), uniop_ty));
        [
            (
                intrinsics::DELAY.to_symbol(),
                function!(vec![numeric!(), numeric!(), numeric!()], numeric!()),
            ),
            (
                intrinsics::TOFLOAT.to_symbol(),
                function!(vec![integer!()], numeric!()),
            ),
        ]
        .into_iter()
        .chain(binds)
        .chain(unibinds)
        .collect()
    }
    fn unwrap_result(&mut self, res: Result<TypeNodeId, Vec<Error>>) -> TypeNodeId {
        match res {
            Ok(t) => t,
            Err(mut e) => {
                let loc = &e[0].get_labels()[0].0; //todo
                self.errors.append(&mut e);
                Type::Failure.into_id_with_location(loc.clone())
            }
        }
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
    fn gen_intermediate_type_with_location(&mut self, loc: Location) -> TypeNodeId {
        let res = Type::Intermediate(Rc::new(RefCell::new(TypeVar::new(
            self.interm_idx,
            self.level,
        ))))
        .into_id_with_location(loc);
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
    fn unify_vec(
        a1: &[TypeNodeId],
        loc1: Location,
        a2: &[TypeNodeId],
        loc2: Location,
    ) -> (Vec<TypeNodeId>, Vec<Error>) {
        let (res, errs): (Vec<_>, Vec<_>) = a1
            .into_iter()
            .zip_longest(a2.into_iter())
            .map(|pair| match pair {
                EitherOrBoth::Both(a1, a2) => Self::unify_types(a1.clone(), a2.clone()),
                EitherOrBoth::Left(t) | EitherOrBoth::Right(t) => Ok(t.clone()),
            })
            .partition_result();
        let mut errs: Vec<_> = errs.into_iter().flatten().collect();
        if a1.len() != a2.len() {
            errs.push(Error::LengthMismatch {
                left: (a1.len(), loc1.clone()),
                right: (a2.len(), loc2.clone()),
            });
        }
        (res, errs)
    }
    fn unify_types(t1: TypeNodeId, t2: TypeNodeId) -> Result<TypeNodeId, Vec<Error>> {
        let loc1 = Location::new(t1.to_span(), "".to_symbol()); //todo file
        let loc2 = Location::new(t2.to_span(), "".to_symbol());

        log::trace!("unify {} and {}", t1.to_type(), t2.to_type());
        let t1r = t1.get_root();
        let t2r = t2.get_root();
        match &(t1r.to_type(), t2r.to_type()) {
            (Type::Intermediate(i1), Type::Intermediate(i2)) => {
                let tv1 = &mut i1.borrow_mut() as &mut TypeVar;
                if Self::occur_check(tv1.var, t2) {
                    return Err(vec![Error::CircularType(loc1, loc2)]);
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
            //currently monomorphize all generic types.
            (t1, Type::Instantiated(_)) => Ok(t1.clone().into_id()),
            (Type::Instantiated(_), t2) => Ok(t2.clone().into_id()),
            (Type::Array(a1), Type::Array(a2)) => {
                Ok(Type::Array(Self::unify_types(*a1, *a2)?).into_id())
            }
            (Type::Ref(x1), Type::Ref(x2)) => Ok(Type::Ref(Self::unify_types(*x1, *x2)?).into_id()),
            (Type::Tuple(a1), Type::Tuple(a2)) => {
                let (vec, err) = Self::unify_vec(&a1, loc1, &a2, loc2);
                if err.is_empty() {
                    Ok(Type::Tuple(vec).into_id())
                } else {
                    Err(err) //todo:return both partial result and err
                }
            }
            (Type::Struct(_a1), Type::Struct(_a2)) => todo!(), //todo
            (Type::Function(p1, r1, _s1), Type::Function(p2, r2, _s2)) => {
                let (param, mut errs) = Self::unify_vec(p1, loc1, p2, loc2);
                let ret = Self::unify_types(*r1, *r2).map_err(|mut e| {
                    errs.append(&mut e);
                    errs
                })?;
                Ok(Type::Function(param, ret, None).into_id())
            }
            (Type::Primitive(p1), Type::Primitive(p2)) if p1 == p2 => {
                Ok(Type::Primitive(p1.clone()).into_id())
            }
            (Type::Failure, t) => Ok(t.clone().into_id_with_location(loc1.clone())),
            (t, Type::Failure) => Ok(t.clone().into_id_with_location(loc2.clone())),
            (Type::Code(_p1), Type::Code(_p2)) => {
                todo!("type system for multi-stage computation has not implemented yet")
            }
            (_p1, _p2) => Err(vec![Error::TypeMismatch {
                left: (t1, loc1),
                right: (t2, loc2),
            }]),
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
        loc: Location,
    ) -> Result<TypeNodeId, Vec<Error>> {
        let TypedPattern { pat, .. } = ty_pat;
        let pat_t = match pat {
            Pattern::Single(id) => {
                let gt = self.generalize(t);
                self.env.add_bind(&[(*id, gt)]);
                Ok::<TypeNodeId, Vec<Error>>(t)
            }
            Pattern::Tuple(pats) => {
                let res = pats
                    .iter()
                    .map(|p| {
                        let ity = self.gen_intermediate_type_with_location(Location::new(
                            ty_pat.to_span(),
                            loc.path,
                        ));
                        let p = TypedPattern {
                            pat: p.clone(),
                            ty: ity,
                        };
                        self.bind_pattern(ity, &p, loc.clone())
                    })
                    .try_collect()?; //todo
                Ok(Type::Tuple(res).into_id())
            }
        }?;
        Self::unify_types(t, pat_t)
    }

    pub fn lookup(&self, name: Symbol, loc: Location) -> Result<TypeNodeId, Error> {
        self.env.lookup(&name).map_or_else(
            || Err(Error::VariableNotFound(name, loc)), //todo:Span
            |v| Ok(*v),
        )
    }
    pub(crate) fn infer_type_literal(e: &Literal) -> Result<TypeNodeId, Error> {
        let pt = match e {
            Literal::Float(_) | Literal::Now | Literal::SampleRate => PType::Numeric,
            Literal::Int(_s) => PType::Int,
            Literal::String(_s) => PType::String,
            Literal::SelfLit => panic!("\"self\" should not be shown at type inference stage"),
            Literal::PlaceHolder => panic!("\"_\" should not be shown at type inference stage"),
        };
        Ok(Type::Primitive(pt).into_id())
    }
    fn infer_vec(&mut self, e: &[ExprNodeId]) -> Result<Vec<TypeNodeId>, Vec<Error>> {
        e.iter().map(|e| self.infer_type(*e)).try_collect()
    }
    fn infer_type_levelup(&mut self, e: ExprNodeId) -> TypeNodeId {
        self.level += 1;
        let res = self.infer_type(e);
        let r = self.unwrap_result(res);
        self.level -= 1;
        r
    }
    fn infer_type(&mut self, e: ExprNodeId) -> Result<TypeNodeId, Vec<Error>> {
        let loc = Location::new(e.to_span(), "".to_symbol()); //todo file
        let res: Result<TypeNodeId, Vec<Error>> = match &e.to_expr() {
            Expr::Literal(l) => Self::infer_type_literal(l).map_err(|e| vec![e]),
            Expr::Tuple(e) => Ok(Type::Tuple(self.infer_vec(e.as_slice())?).into_id()),
            Expr::Proj(e, idx) => {
                let tup = self.infer_type(*e)?;
                match tup.to_type() {
                    Type::Tuple(vec) => {
                        if vec.len() < *idx as usize {
                            Err(vec![Error::IndexOutOfRange {
                                len: vec.len() as u16,
                                idx: *idx as u16,
                                loc,
                            }])
                        } else {
                            Ok(vec[*idx as usize])
                        }
                    }
                    _ => Err(vec![Error::IndexForNonTuple(loc)]),
                }
            }
            Expr::Feed(id, body) => {
                let feedv = self.gen_intermediate_type();
                self.env.add_bind(&[(*id, feedv)]);
                let b = self.infer_type(*body)?;
                let res = Self::unify_types(b, feedv);
                match res {
                    Ok(res) if res.to_type().contains_function() => {
                        Err(vec![Error::NonPrimitiveInFeed(Location::new(
                            body.to_span().clone(),
                            loc.path,
                        ))])
                    }
                    Ok(r) => Ok(r),
                    Err(e) => Err(e),
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
                    Self::unify_types(*r, bty)?
                } else {
                    self.infer_type(*body)?
                };
                self.env.to_outer();
                Ok(Type::Function(ptypes, bty, None).into_id())
            }
            Expr::Let(tpat, body, then) => {
                let bodyt = self.infer_type_levelup(*body);

                let _ = self.bind_pattern(bodyt, tpat, loc)?;

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
                        return Err(vec![Error::NonFunctionForLetRec(id.ty, loc.clone())])
                    }
                    (true, _) => self.gen_intermediate_type(),
                };
                self.env.add_bind(&[(id.id, idt)]);
                //polymorphic inference is not allowed in recursive function.
                let bodyt = self.infer_type_levelup(*body);
                let _res = Self::unify_types(idt, bodyt);
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
                let assignee_t = self.unwrap_result(self.lookup(name, loc).map_err(|e| vec![e]));
                let t = self.infer_type(*expr);
                let e_t = self.unwrap_result(t);
                Self::unify_types(assignee_t, e_t)?;
                Ok(unit!())
            }
            Expr::Then(e, then) => {
                let _ = self.infer_type(*e)?;
                then.map_or(Ok(unit!()), |t| self.infer_type(t))
            }
            Expr::Var(name) => {
                let res = self.unwrap_result(self.lookup(*name, loc).map_err(|e| vec![e]));
                // log::debug!("{} {} /level{}", name.as_str(), res, self.level);
                Ok(self.instantiate(res))
            }
            Expr::Apply(fun, callee) => {
                let fnl = self.infer_type(*fun)?;
                let callee_t = self.infer_vec(callee.as_slice())?;
                let res_t = self.gen_intermediate_type();
                let fntype = Type::Function(callee_t, res_t, None).into_id();
                let restype = Self::unify_types(fnl, fntype)?;
                if let Type::Function(_, r, _) = restype.to_type() {
                    Ok(r)
                } else {
                    Err(vec![Error::NonFunctionForApply(
                        restype,
                        Location::new(fun.to_span(), loc.path),
                    )])
                }
            }
            Expr::If(cond, then, opt_else) => {
                let condt = self.infer_type(*cond)?;
                let _bt = Self::unify_types(
                    Type::Primitive(PType::Numeric)
                        .into_id_with_location(Location::new(cond.to_span(), loc.path)),
                    condt,
                ); //todo:boolean type
                let thent = self.infer_type(*then)?;
                let elset = opt_else.map_or(Ok(Type::Primitive(PType::Unit).into_id()), |e| {
                    self.infer_type(e)
                })?;
                log::trace!("then: {}, else: {}", thent.to_type(), elset.to_type());
                Self::unify_types(thent, elset)
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

pub fn infer_root(
    e: ExprNodeId,
    builtin_types: &[(Symbol, TypeNodeId)],
) -> InferContext {
    let mut ctx = InferContext::new(builtin_types);
    let _t = ctx.infer_type(e).unwrap_or(Type::Failure.into_id());
    ctx.substitute_all_intermediates();
    ctx
}
