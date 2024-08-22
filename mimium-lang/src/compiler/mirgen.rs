use super::intrinsics;
use super::typing::{self, infer_type_literal, InferContext};
use crate::interner::{ExprNodeId, TypeNodeId};
use crate::pattern::{Pattern, TypedId, TypedPattern};
use crate::{numeric, unit};
pub(crate) mod recursecheck;
pub mod selfconvert;
use crate::mir::{self, Argument, Instruction, Mir, VPtr, VReg, Value};

use std::sync::Arc;

use crate::types::{PType, Type};
use crate::utils::environment::{Environment, LookupRes};
use crate::utils::error::ReportableError;
use crate::utils::metadata::{Span, WithMeta};

use crate::ast::{Expr, Literal, Symbol, ToSymbol};
// pub mod closure_convert;
// pub mod feedconvert;
// pub mod hir_solve_stage;

const DELAY_ADDITIONAL_OFFSET: u64 = 3;

#[derive(Debug, Default)]
struct ContextData {
    pub func_i: usize,
    pub current_bb: usize,
    pub state_offset: u64,
    pub push_sum: u64,
}
#[derive(Debug)]
pub struct Context {
    typeenv: InferContext,
    valenv: Environment<VPtr>,
    fn_label: Option<Symbol>,
    anonymous_fncount: u64,
    reg_count: VReg,
    program: Mir,
    data: Vec<ContextData>,
    data_i: usize,
}

impl Context {
    pub fn new() -> Self {
        Self {
            typeenv: InferContext::new(),
            valenv: Environment::new(),
            program: Default::default(),
            reg_count: 0,
            fn_label: None,
            anonymous_fncount: 0,
            data: vec![ContextData::default()],
            data_i: 0,
        }
    }
    fn get_ctxdata(&mut self) -> &mut ContextData {
        self.data.get_mut(self.data_i).unwrap()
    }

    fn consume_fnlabel(&mut self) -> Symbol {
        let res = self.fn_label.clone().unwrap_or_else(|| {
            let res = format!("lambda_{}", self.anonymous_fncount);
            self.anonymous_fncount += 1;
            res.to_symbol()
        });
        self.fn_label = None;
        res
    }

    fn get_current_fn(&mut self) -> &mut mir::Function {
        let i = self.get_ctxdata().func_i;
        &mut self.program.functions[i]
    }
    fn make_delay(&mut self, f: &VPtr, args: &[ExprNodeId]) -> Result<Option<VPtr>, CompileError> {
        let (max, src, time) = match args {
            [max, src, time] => (max, src, time),
            _ => return Ok(None),
        };

        match (f.as_ref(), max.to_expr()) {
            (
                Value::ExtFunction(name, Type::Primitive(PType::Numeric)),
                Expr::Literal(Literal::Float(max)),
            ) if *name == "delay".to_symbol() => {
                let max_time = max.parse::<f64>().unwrap();
                let shift_size = max_time as u64 + DELAY_ADDITIONAL_OFFSET;
                self.get_current_fn().state_size += shift_size;
                let coffset = self.get_ctxdata().state_offset;
                if coffset > 0 {
                    self.get_current_basicblock()
                        .0
                        .push((Arc::new(Value::None), Instruction::PushStateOffset(coffset)));
                    self.get_ctxdata().push_sum += coffset;
                }
                let args = self.eval_args(&[*src, *time])?;
                let (args, _types): (Vec<VPtr>, Vec<TypeNodeId>) = args.into_iter().unzip();
                let res = Ok(Some(self.push_inst(Instruction::Delay(
                    max_time as u64,
                    args[0].clone(),
                    args[1].clone(),
                ))));
                res
            }
            (Value::ExtFunction(name, _ty), _) if *name == "delay".to_symbol() => Err(
                CompileError(CompileErrorKind::UnboundedDelay, max.to_span().clone()),
            ),

            _ => Ok(None),
        }
    }
    fn make_intrinsics(
        &mut self,
        label: Symbol,
        args: Vec<VPtr>,
    ) -> Result<Option<VPtr>, CompileError> {
        let inst = match (label.as_str(), args.len()) {
            (intrinsics::NEG, 1) => Instruction::NegF(args[0].clone()),
            (intrinsics::ADD, 2) => Instruction::AddF(args[0].clone(), args[1].clone()),
            (intrinsics::SUB, 2) => Instruction::SubF(args[0].clone(), args[1].clone()),
            (intrinsics::MULT, 2) => Instruction::MulF(args[0].clone(), args[1].clone()),
            (intrinsics::DIV, 2) => Instruction::DivF(args[0].clone(), args[1].clone()),
            (intrinsics::EXP, 2) => Instruction::PowF(args[0].clone(), args[1].clone()),
            (intrinsics::MODULO, 2) => Instruction::ModF(args[0].clone(), args[1].clone()),
            (intrinsics::SQRT, 1) => Instruction::SqrtF(args[0].clone()),
            (intrinsics::ABS, 1) => Instruction::AbsF(args[0].clone()),
            (intrinsics::SIN, 1) => Instruction::SinF(args[0].clone()),
            (intrinsics::COS, 1) => Instruction::CosF(args[0].clone()),
            (intrinsics::LOG, 2) => Instruction::LogF(args[0].clone(), args[1].clone()),
            (intrinsics::GT, 2) => Instruction::Gt(args[0].clone(), args[1].clone()),
            (intrinsics::GE, 2) => Instruction::Ge(args[0].clone(), args[1].clone()),
            (intrinsics::LT, 2) => Instruction::Lt(args[0].clone(), args[1].clone()),
            (intrinsics::LE, 2) => Instruction::Le(args[0].clone(), args[1].clone()),
            (intrinsics::EQ, 2) => Instruction::Eq(args[0].clone(), args[1].clone()),
            (intrinsics::NE, 2) => Instruction::Ne(args[0].clone(), args[1].clone()),
            (intrinsics::AND, 2) => Instruction::And(args[0].clone(), args[1].clone()),
            (intrinsics::OR, 2) => Instruction::Or(args[0].clone(), args[1].clone()),
            (intrinsics::MEM, 1) => {
                self.get_current_fn().state_size += 1;
                Instruction::Mem(args[0].clone())
            }
            _ => return Ok(None),
        };
        Ok(Some(self.push_inst(inst)))
    }
    fn get_current_basicblock(&mut self) -> &mut mir::Block {
        let bbid = self.get_ctxdata().current_bb;
        self.get_current_fn()
            .body
            .get_mut(bbid)
            .expect("no basic block found")
    }
    fn add_new_basicblock(&mut self) {
        let idx = self.get_current_fn().add_new_basicblock();
        self.get_ctxdata().current_bb = idx;
    }
    fn gen_new_register(&mut self) -> VPtr {
        let res = Arc::new(Value::Register(self.reg_count));
        self.reg_count += 1;
        res
    }
    fn push_inst(&mut self, inst: Instruction) -> VPtr {
        let res = self.gen_new_register();
        self.get_current_basicblock().0.push((res.clone(), inst));
        res
    }
    fn add_bind(&mut self, bind: (Symbol, VPtr)) {
        self.valenv.add_bind(&mut [bind]);
    }
    fn add_bind_pattern(
        &mut self,
        pattern: &WithMeta<TypedPattern>,
        v: VPtr,
        ty: &Type,
    ) -> Result<(), CompileError> {
        let WithMeta(TypedPattern { pat, ty: _ }, span) = pattern;
        match pat {
            Pattern::Single(id) => Ok(self.add_bind((*id, v))),
            Pattern::Tuple(patterns) => {
                let interm_vec = patterns
                    .iter()
                    .map(|_| self.typeenv.gen_intermediate_type().into_id())
                    .collect::<Vec<_>>();
                let tvec = self
                    .typeenv
                    .unify_types(ty.clone(), Type::Tuple(interm_vec))?;
                let tvec = tvec.get_as_tuple().ok_or(CompileError::from(typing::Error(
                    typing::ErrorKind::PatternMismatch(ty.clone(), pat.clone()),
                    span.clone(),
                )))?;
                for (i, pat) in patterns.iter().enumerate() {
                    let cty = tvec[i].to_type();
                    let v = if i == 0 {
                        v.clone()
                    } else {
                        self.push_inst(Instruction::GetElement {
                            value: v.clone(),
                            ty: ty.clone().into_id(),
                            array_idx: 0,
                            tuple_offset: i as u64,
                        })
                    };

                    let tpat = WithMeta(
                        TypedPattern {
                            pat: pat.clone(),
                            ty: None,
                        },
                        span.clone(),
                    );
                    self.add_bind_pattern(&tpat, v, cty)?;
                }
                Ok(())
            }
        }
    }
    fn make_new_function(
        &mut self,
        name: Symbol,
        args: &[VPtr],
        argtypes: &[TypeNodeId],
        parent_i: Option<usize>,
    ) -> usize {
        let newf = mir::Function::new(name, args, argtypes, parent_i);
        self.program.functions.push(newf);
        let idx = self.program.functions.len() - 1;
        idx
    }
    fn do_in_child_ctx<F: FnMut(&mut Self, usize) -> Result<(VPtr, Type), CompileError>>(
        &mut self,
        fname: Symbol,
        binds: &[(Symbol, VPtr, Type)],
        mut action: F,
    ) -> Result<(usize, VPtr, Type), CompileError> {
        //pre action
        let mut atbinds = binds
            .iter()
            .map(|(name, _a, t)| (name.clone(), t.clone()))
            .collect::<Vec<_>>();
        let mut abinds = binds
            .iter()
            .map(|(name, a, _)| (name.clone(), a.clone()))
            .collect::<Vec<_>>();
        let vbinds = binds.iter().map(|(_, a, t)| a.clone()).collect::<Vec<_>>();
        let tbinds = binds
            .iter()
            .map(|(_, _, t)| t.clone().into_id())
            .collect::<Vec<_>>();
        self.valenv.extend();
        self.valenv.add_bind(&mut abinds);
        let label = self.get_ctxdata().func_i;
        let c_idx =
            self.make_new_function(fname, vbinds.as_slice(), tbinds.as_slice(), Some(label));

        self.typeenv.env.extend();
        self.typeenv.env.add_bind(&mut atbinds);

        self.data.push(ContextData {
            func_i: c_idx,
            current_bb: 0,
            state_offset: 0,
            push_sum: 0,
        });
        self.data_i += 1;
        //do action
        let (fptr, ty) = action(self, c_idx)?;

        // TODO: ideally, type should be infered before the actual action
        let f = self.program.functions.get_mut(c_idx).unwrap();
        f.return_type.get_or_init(|| ty.clone().into_id());

        //post action
        let _ = self.data.pop();
        self.data_i -= 1;
        self.valenv.to_outer();
        self.typeenv.env.to_outer();
        Ok((c_idx, fptr, ty))
    }
    fn lookup(&self, key: &Symbol) -> LookupRes<VPtr> {
        match self.valenv.lookup_cls(key) {
            LookupRes::Local(v) => LookupRes::Local(v.clone()),
            LookupRes::UpValue(level, v) => LookupRes::UpValue(level, v.clone()),
            LookupRes::Global(v) => LookupRes::Global(v.clone()),
            LookupRes::None => LookupRes::None,
        }
    }

    pub fn eval_literal(&mut self, lit: &Literal, _span: &Span) -> Result<VPtr, CompileError> {
        let v = match lit {
            Literal::String(_) => todo!(),
            Literal::Int(i) => self.push_inst(Instruction::Integer(*i)),
            Literal::Float(f) => self.push_inst(Instruction::Float(
                f.parse::<f64>().expect("illegal float format"),
            )),
            Literal::SelfLit => unreachable!(),
            Literal::Now => todo!(),
        };
        Ok(v)
    }
    pub fn eval_var(&mut self, name: Symbol, span: &Span) -> Result<(VPtr, Type), CompileError> {
        let t = self
            .typeenv
            .env
            .lookup(&name)
            .expect("failed to find type for variable")
            .clone();
        let v = match self.lookup(&name) {
            LookupRes::Local(v) => match v.as_ref() {
                Value::Function(i, _s, _nret) => {
                    let reg = self.push_inst(Instruction::Uinteger(*i as u64));
                    self.push_inst(Instruction::Closure(reg))
                }
                _ => self.push_inst(Instruction::Load(v.clone(), t.clone())),
            },
            LookupRes::UpValue(level, v) => (0..level).into_iter().rev().fold(v, |upv, i| {
                let res = self.gen_new_register();
                let current = self.data.get_mut(self.data_i - i).unwrap();
                let currentf = self.program.functions.get_mut(current.func_i).unwrap();
                let currentbb = currentf.body.get_mut(current.current_bb).unwrap();
                currentf.upindexes.push(upv.clone());
                let upi = (currentf.upindexes.len() - 1) as u64;

                currentbb
                    .0
                    .push((res.clone(), Instruction::GetUpValue(upi, t.clone())));
                res
            }),
            LookupRes::Global(v) => match v.as_ref() {
                Value::Global(_gv) => self.push_inst(Instruction::GetGlobal(v.clone(), t.clone())),
                Value::Function(_, _, _) | Value::Register(_) | Value::FixPoint(_) => v.clone(),
                _ => unreachable!("non global_value"),
            },
            LookupRes::None => {
                let ty =
                    typing::lookup(&name, &mut self.typeenv, span).map_err(CompileError::from)?;
                Arc::new(Value::ExtFunction(name, ty))
            }
        };

        let ty = typing::lookup(&name, &mut self.typeenv, span).map_err(CompileError::from)?;
        Ok((v, ty.clone()))
    }
    fn emit_fncall(&mut self, idx: u64, statesize: u64, args: Vec<VPtr>, ret_t: &Type) -> VPtr {
        let f = {
            self.get_current_fn().state_size += statesize;
            self.push_inst(Instruction::Uinteger(idx))
        };
        //insert pushstateoffset
        if self.get_ctxdata().state_offset > 0 {
            self.get_current_basicblock().0.push((
                Arc::new(Value::None),
                Instruction::PushStateOffset(statesize),
            ));
            self.get_ctxdata().push_sum += statesize;
        }

        let res = self.push_inst(Instruction::Call(f.clone(), args, ret_t.clone()));
        if statesize > 0 {
            self.get_ctxdata().state_offset += statesize;
        }
        res
    }
    fn eval_args(&mut self, args: &[ExprNodeId]) -> Result<Vec<(VPtr, TypeNodeId)>, CompileError> {
        args.iter()
            .map(|a_meta| -> Result<_, CompileError> {
                let (v, t) = self.eval_expr(*a_meta)?;
                let res = match v.as_ref() {
                    // for the higher order function, make closure regardless it is global function
                    Value::Function(idx, _, _) => {
                        let f = self.push_inst(Instruction::Uinteger(*idx as u64));
                        self.push_inst(Instruction::Closure(f))
                    }
                    _ => v.clone(),
                };
                Ok((res, t.into_id()))
            })
            .try_collect::<Vec<_>>()
    }
    pub fn eval_expr(&mut self, e_meta: ExprNodeId) -> Result<(VPtr, Type), CompileError> {
        let span = e_meta.to_span();
        match e_meta.to_expr() {
            Expr::Literal(lit) => {
                let v = self.eval_literal(lit, span)?;
                let t = infer_type_literal(lit).map_err(CompileError::from)?;
                Ok((v, t))
            }
            Expr::Var(name, _time) => self.eval_var(*name, span),
            Expr::Block(b) => {
                if let Some(block) = b {
                    self.eval_expr(*block)
                } else {
                    //todo?
                    Ok((Arc::new(Value::None), unit!().to_type().clone()))
                }
            }
            Expr::Tuple(items) => {
                let len = items.len();
                if len == 0 {
                    unreachable!("0-length tuple is not supported");
                }
                let alloc_insert_point = self.get_current_basicblock().0.len();
                let dst = self.gen_new_register();
                // let mut types = Vec::with_capacity(len);
                let mut types = vec![];
                let mut inst_refs: Vec<usize> = vec![];
                for (i, e) in items.iter().enumerate() {
                    let (v, ty) = self.eval_expr(*e)?;
                    let ptr = if i == 0 {
                        dst.clone()
                    } else {
                        inst_refs.push(self.get_current_basicblock().0.len());
                        self.push_inst(Instruction::GetElement {
                            value: dst.clone(),
                            ty: Type::Unknown.into_id(), // lazyly set after loops
                            array_idx: 0,
                            tuple_offset: i as u64,
                        })
                    };
                    self.push_inst(Instruction::Store(ptr, v, ty.clone()));
                    types.push(ty.into_id());
                }
                let tup_t = Type::Tuple(types.clone());
                for inst_i in inst_refs.iter() {
                    if let Some((
                        _,
                        Instruction::GetElement {
                            value: _,
                            ref mut ty,
                            array_idx: _,
                            tuple_offset: _,
                        },
                    )) = self.get_current_basicblock().0.get_mut(*inst_i)
                    {
                        *ty = tup_t.clone().into_id();
                    }
                }
                self.get_current_basicblock().0.insert(
                    alloc_insert_point,
                    (dst.clone(), Instruction::Alloc(tup_t.clone())),
                );
                // TODO: validate if the types are all identical?
                // let first_ty = &types[0];
                // if !types.iter().all(|x| x == first_ty) {
                //     todo!("Return error");
                // }

                // pass only the head of the tuple, and the length can be known
                // from the type information.
                Ok((dst, tup_t))
            }
            Expr::Proj(_, _) => todo!(),
            Expr::Apply(f, args) => {
                let (f, ft) = self.eval_expr(*f)?;
                let del = self.make_delay(&f, args)?;
                if let Some(d) = del {
                    Ok((d, Type::Primitive(PType::Numeric)))
                } else {
                    let atvvec = self.eval_args(&args)?;
                    let (a_regs, atvec): (Vec<VPtr>, Vec<TypeNodeId>) = atvvec.into_iter().unzip();
                    let rt = self.typeenv.gen_intermediate_type().into_id();
                    let ftype = self
                        .typeenv
                        .unify_types(ft, Type::Function(atvec, rt, None))?;
                    let rt = if let Type::Function(_, rt, _) = ftype {
                        Ok(rt.clone())
                    } else {
                        Err(CompileError(
                            CompileErrorKind::TypingFailure(typing::ErrorKind::NonFunction(
                                ftype.clone(),
                            )),
                            span.clone(),
                        ))
                    }?;
                    let res = match f.as_ref() {
                        Value::Global(v) => match v.as_ref() {
                            Value::Function(idx, statesize, _rty) => {
                                self.emit_fncall(*idx as u64, *statesize, a_regs, rt.to_type())
                            }
                            Value::Register(_) => self.push_inst(Instruction::CallCls(
                                v.clone(),
                                a_regs.clone(),
                                rt.to_type().clone(),
                            )),
                            Value::FixPoint(fnid) => {
                                let clspos = self.push_inst(Instruction::Uinteger(*fnid as u64));
                                let cls = self.push_inst(Instruction::Closure(clspos));
                                self.push_inst(Instruction::CallCls(
                                    cls,
                                    a_regs.clone(),
                                    rt.to_type().clone(),
                                ))
                            }
                            _ => {
                                panic!("calling non-function global value")
                            }
                        },
                        Value::Register(_) => {
                            //closure
                            //do not increment state size for closure
                            let res = self.push_inst(Instruction::CallCls(
                                f.clone(),
                                a_regs.clone(),
                                rt.to_type().clone(),
                            ));
                            res
                        }
                        Value::FixPoint(fnid) => {
                            let clspos = self.push_inst(Instruction::Uinteger(*fnid as u64));
                            let cls = self.push_inst(Instruction::Closure(clspos));
                            self.push_inst(Instruction::CallCls(
                                cls,
                                a_regs.clone(),
                                rt.to_type().clone(),
                            ))
                        }

                        Value::Function(idx, statesize, ret_t) => {
                            self.emit_fncall(*idx as u64, *statesize, a_regs, ret_t)
                        }
                        Value::ExtFunction(label, ty) => {
                            if let Some(res) = self.make_intrinsics(*label, a_regs.clone())? {
                                res
                            } else {
                                self.push_inst(Instruction::Call(
                                    f.clone(),
                                    a_regs.clone(),
                                    ty.clone(),
                                ))
                            }
                        }
                        // Value::ExternalClosure(i) => todo!(),
                        Value::None => unreachable!(),
                        _ => todo!(),
                    };
                    Ok((res, rt.to_type().clone()))
                }
            }
            Expr::Lambda(ids, rett, body) => {
                let binds_to_be_deleted = ids
                    .iter()
                    .enumerate()
                    .map(|(idx, name)| {
                        let label = name.0.id.clone();
                        let t = name.0.ty.clone().unwrap_or_else(|| {
                            let tenv = &mut self.typeenv;
                            tenv.gen_intermediate_type().into_id()
                        });
                        let a = Argument(label, t);
                        let res = (
                            label.clone(),
                            Arc::new(Value::Argument(idx, Arc::new(a))),
                            t.to_type().clone(),
                        );
                        res
                    })
                    .collect::<Vec<_>>();

                let binds = ids
                    .iter()
                    .enumerate()
                    .map(|(idx, name)| {
                        let label = name.0.id.clone();
                        let t = name.0.ty.clone().unwrap_or_else(|| {
                            let tenv = &mut self.typeenv;
                            tenv.gen_intermediate_type().into_id()
                        });
                        let a = Argument(label, t);
                        let res = (
                            label.clone(),
                            Arc::new(Value::Argument(idx, Arc::new(a))),
                            t,
                        );
                        res
                    })
                    .collect::<Vec<_>>();

                let name = self.consume_fnlabel();
                let (c_idx, f, res_type) =
                    self.do_in_child_ctx(name, &binds_to_be_deleted, |ctx, c_idx| {
                        let (res, mut res_type) = ctx.eval_expr(*body)?;
                        res_type = if let Some(rt) = rett {
                            let tenv = &mut ctx.typeenv;
                            tenv.unify_types(rt.to_type().clone(), res_type.clone())
                                .map_err(CompileError::from)?
                        } else {
                            res_type
                        };
                        let state_size = {
                            let child = ctx.program.functions.get_mut(c_idx).unwrap();
                            child.state_size
                        };
                        let push_sum = ctx.get_ctxdata().push_sum;
                        if push_sum > 0 {
                            ctx.get_current_basicblock().0.push((
                                Arc::new(mir::Value::None),
                                Instruction::PopStateOffset(push_sum),
                            )); //todo:offset size
                        }
                        match (res.as_ref(), res_type.clone()) {
                            (_, Type::Primitive(PType::Unit) | Type::Unknown) => {
                                let _ = ctx.push_inst(Instruction::Return(
                                    Arc::new(Value::None),
                                    res_type.clone(),
                                ));
                            }
                            (Value::State(v), _) => {
                                let _ = ctx.push_inst(Instruction::ReturnFeed(
                                    v.clone(),
                                    res_type.clone(),
                                ));
                            }
                            (Value::Function(i, _, _), _) => {
                                let idx = ctx.push_inst(Instruction::Uinteger(*i as u64));
                                let cls = ctx.push_inst(Instruction::Closure(idx));
                                let _ = ctx.push_inst(Instruction::Return(cls, res_type.clone()));
                            }
                            (_, _) => {
                                let _ = ctx
                                    .push_inst(Instruction::Return(res.clone(), res_type.clone()));
                            }
                        };

                        let f = Arc::new(Value::Function(c_idx, state_size, res_type.clone()));
                        Ok((f, res_type))
                    })?;
                let child = self.program.functions.get_mut(c_idx).unwrap();
                let res = if child.upindexes.is_empty() {
                    //todo:make Closure
                    f
                } else {
                    let idxcell = self.push_inst(Instruction::Uinteger(c_idx as u64));
                    self.push_inst(Instruction::Closure(idxcell))
                };
                let atypes = binds
                    .iter()
                    .map(|(_name, _a, t)| t.clone())
                    .collect::<Vec<_>>();
                let fty = Type::Function(atypes, res_type.into_id(), None);
                Ok((res, fty))
            }
            Expr::Feed(id, expr) => {
                let insert_pos = self.get_current_basicblock().0.len();
                //set typesize lazily
                let res = self.push_inst(Instruction::GetState(Type::Unknown));

                self.get_ctxdata().state_offset += 1;

                self.add_bind((id.clone(), res.clone()));
                let tf = {
                    let tf = self.typeenv.gen_intermediate_type();
                    self.typeenv
                        .env
                        .add_bind(&mut vec![(id.clone(), tf.clone())]);
                    tf
                };
                let (retv, t) = self.eval_expr(*expr)?;

                if let (_, Instruction::GetState(ty)) =
                    self.get_current_basicblock().0.get_mut(insert_pos).unwrap()
                {
                    *ty = t.clone();
                }
                self.typeenv.unify_types(tf, t.clone())?;
                self.get_current_fn().state_size += 1;
                Ok((Arc::new(Value::State(retv)), t))
            }
            Expr::Let(pat, body, then) => {
                if let Ok(tid) = TypedId::try_from(pat.0.clone()) {
                    self.fn_label = Some(tid.id.clone());
                };
                let insert_pos = if self.program.functions.is_empty() {
                    0
                } else {
                    self.get_current_basicblock().0.len()
                };
                let is_global = self.get_ctxdata().func_i == 0;
                let (bodyv, t) = self.eval_expr(*body)?;
                //todo:need to boolean and insert cast
                let idt = match pat.0.ty.as_ref() {
                    Some(Type::Function(atypes, rty, s)) => self.typeenv.convert_unknown_function(
                        &atypes
                            .iter()
                            .map(|x| x.to_type().clone())
                            .collect::<Vec<_>>(),
                        rty.to_type(),
                        &s.map(|x| Box::new(x.to_type().clone())),
                    ),
                    Some(t) => t.clone(),
                    None => self.typeenv.gen_intermediate_type(),
                };
                self.typeenv.unify_types(idt, t.clone())?;

                let t = self.typeenv.bind_pattern(&t, pat)?;
                self.fn_label = None;

                match (
                    is_global,
                    matches!(bodyv.as_ref(), Value::Function(_, _, _)),
                    then,
                ) {
                    (true, false, Some(then_e)) => {
                        let gv = Arc::new(Value::Global(bodyv.clone()));
                        let _greg = self.push_inst(Instruction::SetGlobal(
                            gv.clone(),
                            bodyv.clone(),
                            t.clone(),
                        ));
                        self.add_bind_pattern(pat, gv, &t)?;
                        self.eval_expr(*then_e)
                    }
                    (false, false, Some(then_e)) => {
                        let alloc_res = self.gen_new_register();
                        let block = &mut self.get_current_basicblock().0;
                        block.insert(
                            insert_pos,
                            (alloc_res.clone(), Instruction::Alloc(t.clone())),
                        );
                        let _ = self.push_inst(Instruction::Store(
                            alloc_res.clone(),
                            bodyv.clone(),
                            t.clone(),
                        ));

                        self.add_bind_pattern(pat, alloc_res, &t)?;
                        self.eval_expr(*then_e)
                    }
                    (_, _, Some(then_e)) => {
                        self.add_bind_pattern(pat, bodyv, &t)?;
                        self.eval_expr(*then_e)
                    }
                    (_, _, None) => Ok((Arc::new(Value::None), unit!().to_type().clone())),
                }
            }
            Expr::LetRec(id, body, then) => {
                self.fn_label = Some(id.id.clone());
                let t = {
                    let tenv = &mut self.typeenv;
                    let idt = match id.ty.map(|x| x.to_type().clone()) {
                        Some(Type::Function(atypes, rty, s)) => tenv.convert_unknown_function(
                            &atypes
                                .iter()
                                .map(|x| x.to_type().clone())
                                .collect::<Vec<_>>(),
                            rty.to_type(),
                            &s.map(|x| Box::new(x.to_type().clone())),
                        ),
                        Some(t) => t.clone(),
                        None => tenv.gen_intermediate_type(),
                    };
                    tenv.env.add_bind(&mut vec![(id.id.clone(), idt.clone())]);
                    idt
                };
                let nextfunid = self.program.functions.len();
                let fix = Arc::new(Value::FixPoint(nextfunid));

                // let alloc = self.push_inst(Instruction::Alloc(t.clone()));
                // let _ = self.push_inst(Instruction::Store(alloc.clone(), fix));
                let bind = (id.id.clone(), fix);
                self.add_bind(bind);
                let (b, bt) = self.eval_expr(*body)?;
                let rest = self.typeenv.unify_types(t, bt)?;

                self.typeenv.env.add_bind(&mut vec![(id.id.clone(), rest)]);
                //set bind from fixpoint to computed lambda
                let (_, v) = self
                    .valenv
                    .0
                    .iter_mut()
                    .find_map(|lenv| lenv.iter_mut().find(|(name, _)| *name == id.id.clone()))
                    .unwrap();
                *v = b;
                if let Some(then_e) = then {
                    self.eval_expr(*then_e)
                } else {
                    Ok((Arc::new(Value::None), unit!().to_type().clone()))
                }
            }
            Expr::If(cond, then, else_) => {
                let (c, t_cond) = self.eval_expr(*cond)?;
                //todo:need to boolean and insert cast
                self.typeenv
                    .unify_types(t_cond, numeric!().to_type().clone())?;

                let bbidx = self.get_ctxdata().current_bb;
                let _ = self.push_inst(Instruction::JmpIf(
                    c,
                    (bbidx + 1) as u64,
                    (bbidx + 2) as u64,
                ));
                //insert then block
                self.add_new_basicblock();
                let (t, thent) = self.eval_expr(*then)?;
                let t = match t.as_ref() {
                    Value::Function(idx, _, _) => {
                        let cpos = self.push_inst(Instruction::Uinteger(*idx as u64));
                        self.push_inst(Instruction::Closure(cpos))
                    }
                    _ => t,
                };
                //jmp to ret is inserted in bytecodegen
                //insert else block
                self.add_new_basicblock();
                let (e, elset) = match else_ {
                    Some(e) => self.eval_expr(*e),
                    None => Ok((Arc::new(Value::None), unit!().to_type().clone())),
                }?;
                //if returning non-closure function, make closure
                let e = match e.as_ref() {
                    Value::Function(idx, _, _) => {
                        let cpos = self.push_inst(Instruction::Uinteger(*idx as u64));
                        self.push_inst(Instruction::Closure(cpos))
                    }
                    _ => e,
                };

                self.typeenv.unify_types(thent.clone(), elset)?;
                //insert return block
                self.add_new_basicblock();
                let res = self.push_inst(Instruction::Phi(t, e));
                Ok((res, thent))
            }
            Expr::Bracket(_) => todo!(),
            Expr::Escape(_) => todo!(),
            Expr::Error => todo!(),
            Expr::Assign(_, _) => todo!(),
            Expr::Then(_, _) => todo!(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum CompileErrorKind {
    TypingFailure(typing::ErrorKind),
    UnboundedDelay,
    TooManyConstants,
    VariableNotFound(String),
}
#[derive(Clone, Debug)]
pub struct CompileError(CompileErrorKind, Span);

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let CompileError(kind, _span) = self;
        match kind {
            CompileErrorKind::TypingFailure(k) => write!(f, "{k}"),
            CompileErrorKind::UnboundedDelay => {
                write!(f, "Maximium delay time needs to be a number literal.")
            }

            CompileErrorKind::TooManyConstants => write!(f, "too many constants."),
            CompileErrorKind::VariableNotFound(s) => write!(f, "Variable {s} not found."),
        }
    }
}
impl std::error::Error for CompileError {}
impl From<typing::Error> for CompileError {
    fn from(value: typing::Error) -> Self {
        Self(CompileErrorKind::TypingFailure(value.0), value.1)
    }
}
impl ReportableError for CompileError {
    fn get_span(&self) -> std::ops::Range<usize> {
        self.1.clone()
    }
}

pub fn compile(root_expr_id: ExprNodeId) -> Result<Mir, Box<dyn ReportableError>> {
    let ast2 = recursecheck::convert_recurse(root_expr_id);
    let expr2 = selfconvert::convert_self_top(ast2).map_err(|e| {
        let eb: Box<dyn ReportableError> = Box::new(e);
        eb
    })?;
    let mut ctx = Context::new();
    let _res = ctx.eval_expr(expr2).map_err(|e| {
        let eb: Box<dyn ReportableError> = Box::new(e);
        eb
    })?;
    Ok(ctx.program.clone())
}
