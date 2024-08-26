use super::intrinsics;
use super::typing::{self, infer_root, InferContext};
use crate::interner::{ExprNodeId, Symbol, ToSymbol, TypeNodeId};
use crate::pattern::{Pattern, TypedId, TypedPattern};
use crate::{numeric, unit};
pub(crate) mod recursecheck;
pub mod selfconvert;
use crate::mir::{self, Argument, Instruction, Mir, VPtr, VReg, Value};

use std::sync::Arc;

use crate::types::{PType, Type};
use crate::utils::environment::{Environment, LookupRes};
use crate::utils::error::ReportableError;
use crate::utils::metadata::Span;

use crate::ast::{Expr, Literal};
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
    pub fn new(typeenv: InferContext) -> Self {
        Self {
            typeenv,
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
        let res = self.fn_label.unwrap_or_else(|| {
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
        let rt = match f.as_ref() {
            Value::ExtFunction(name, rt) if *name != "delay".to_symbol() => rt,
            _ => return Ok(None),
        };

        let (max, src, time) = match args {
            [max, src, time] => (max, src, time),
            _ => return Ok(None),
        };

        match (rt.to_type(), max.to_expr()) {
            (Type::Primitive(PType::Numeric), Expr::Literal(Literal::Float(max))) => {
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
            _ => Err(CompileError(
                CompileErrorKind::UnboundedDelay,
                max.to_span().clone(),
            )),
        }
    }
    fn make_binop_intrinsic(
        &self,
        label: Symbol,
        args: &[(VPtr, TypeNodeId)],
    ) -> Option<Instruction> {
        debug_assert_eq!(args.len(), 2);
        let a0 = args[0].0.clone();
        let a1 = args[1].0.clone();
        match label.as_str() {
            intrinsics::ADD => Some(Instruction::AddF(a0, a1)),
            intrinsics::SUB => Some(Instruction::SubF(a0, a1)),
            intrinsics::MULT => Some(Instruction::MulF(a0, a1)),
            intrinsics::DIV => Some(Instruction::DivF(a0, a1)),
            intrinsics::EXP => Some(Instruction::PowF(a0, a1)),
            intrinsics::MODULO => Some(Instruction::ModF(a0, a1)),
            intrinsics::LOG => Some(Instruction::LogF(a0, a1)),
            intrinsics::GT => Some(Instruction::Gt(a0, a1)),
            intrinsics::GE => Some(Instruction::Ge(a0, a1)),
            intrinsics::LT => Some(Instruction::Lt(a0, a1)),
            intrinsics::LE => Some(Instruction::Le(a0, a1)),
            intrinsics::EQ => Some(Instruction::Eq(a0, a1)),
            intrinsics::NE => Some(Instruction::Ne(a0, a1)),
            intrinsics::AND => Some(Instruction::And(a0, a1)),
            intrinsics::OR => Some(Instruction::Or(a0, a1)),
            _ => None,
        }
    }
    fn make_uniop_intrinsic(
        &mut self,
        label: Symbol,
        args: &[(VPtr, TypeNodeId)],
    ) -> Option<Instruction> {
        debug_assert_eq!(args.len(), 1);
        let a0 = args[0].0.clone();
        match label.as_str() {
            intrinsics::NEG => Some(Instruction::NegF(a0)),
            intrinsics::SQRT => Some(Instruction::SqrtF(a0)),
            intrinsics::ABS => Some(Instruction::AbsF(a0)),
            intrinsics::SIN => Some(Instruction::SinF(a0)),
            intrinsics::COS => Some(Instruction::CosF(a0)),
            intrinsics::MEM => {
                self.get_current_fn().state_size += 1;
                Some(Instruction::Mem(a0))
            }
            _ => None,
        }
    }

    fn make_intrinsics(
        &mut self,
        label: Symbol,
        args: &[(VPtr, TypeNodeId)],
    ) -> Result<Option<VPtr>, CompileError> {
        let inst = match args.len() {
            1 => self.make_uniop_intrinsic(label, args),
            2 => self.make_binop_intrinsic(label, args),
            _ => return Ok(None),
        };
        Ok(inst.map(|i| self.push_inst(i)))
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
        self.valenv.add_bind(&[bind]);
    }
    fn add_bind_pattern(
        &mut self,
        pattern: &TypedPattern,
        v: VPtr,
        ty: TypeNodeId,
    ) -> Result<(), CompileError> {
        let TypedPattern { pat, .. } = pattern;
        let span = pattern.to_span();
        match (pat, ty.to_type()) {
            (Pattern::Single(id), _) => Ok(self.add_bind((*id, v))),
            (Pattern::Tuple(patterns), Type::Tuple(tvec)) => {
                let v = if matches!(v.as_ref(), Value::Global(_)) {
                    self.push_inst(Instruction::GetGlobal(v.clone(), ty))
                } else {
                    v
                };
                for ((i, pat), cty) in patterns.iter().enumerate().zip(tvec.iter()) {
                    let v = self.push_inst(Instruction::GetElement {
                        value: v.clone(),
                        ty,
                        array_idx: 0,
                        tuple_offset: i as u64,
                    });
                    let tid = Type::Unknown.into_id_with_span(span.clone());
                    let tpat = TypedPattern {
                        pat: pat.clone(),
                        ty: tid,
                    };
                    self.add_bind_pattern(&tpat, v, *cty)?;
                }
                Ok(())
            }
            _ => {
                panic!("typing error in the previous stage")
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
    fn do_in_child_ctx<F: FnMut(&mut Self, usize) -> Result<(VPtr, TypeNodeId), CompileError>>(
        &mut self,
        fname: Symbol,
        abinds: &[(Symbol, VPtr)],
        types: &[TypeNodeId],
        mut action: F,
    ) -> Result<(usize, VPtr), CompileError> {
        self.valenv.extend();
        self.valenv.add_bind(abinds);
        let args = abinds.iter().map(|(_, a)| a.clone()).collect::<Vec<_>>();
        let label = self.get_ctxdata().func_i;
        let c_idx = self.make_new_function(fname, &args, types, Some(label));

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
        f.return_type.get_or_init(|| ty);

        //post action
        let _ = self.data.pop();
        self.data_i -= 1;
        self.valenv.to_outer();
        Ok((c_idx, fptr))
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
    pub fn eval_var(
        &mut self,
        name: Symbol,
        t: TypeNodeId,
        span: &Span,
    ) -> Result<VPtr, CompileError> {
        let v = match self.lookup(&name) {
            LookupRes::Local(v) => match v.as_ref() {
                Value::Function(i, _s, _nret) => {
                    let reg = self.push_inst(Instruction::Uinteger(*i as u64));
                    self.push_inst(Instruction::Closure(reg))
                }
                _ => self.push_inst(Instruction::Load(v.clone(), t)),
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
                    .push((res.clone(), Instruction::GetUpValue(upi, t)));
                res
            }),
            LookupRes::Global(v) => match v.as_ref() {
                Value::Global(_gv) => self.push_inst(Instruction::GetGlobal(v.clone(), t)),
                Value::Function(_, _, _) | Value::Register(_) | Value::FixPoint(_) => v.clone(),
                _ => unreachable!("non global_value"),
            },
            LookupRes::None => {
                let ty = self
                    .typeenv
                    .lookup(&name, span)
                    .map_err(CompileError::from)?;
                Arc::new(Value::ExtFunction(name, ty))
            }
        };
        Ok(v)
    }
    fn emit_fncall(
        &mut self,
        idx: u64,
        statesize: u64,
        args: Vec<(VPtr, TypeNodeId)>,
        ret_t: TypeNodeId,
    ) -> VPtr {
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

        let res = self.push_inst(Instruction::Call(f.clone(), args, ret_t));
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
                Ok((res, t))
            })
            .try_collect::<Vec<_>>()
    }
    fn eval_block(
        &mut self,
        block: Option<ExprNodeId>,
    ) -> Result<(VPtr, TypeNodeId), CompileError> {
        self.add_new_basicblock();
        let (e, rt) = match block {
            Some(e) => self.eval_expr(e),
            None => Ok((Arc::new(Value::None), unit!())),
        }?;
        //if returning non-closure function, make closure
        let e = match e.as_ref() {
            Value::Function(idx, _, _) => {
                let cpos = self.push_inst(Instruction::Uinteger(*idx as u64));
                self.push_inst(Instruction::Closure(cpos))
            }
            _ => e,
        };
        Ok((e, rt))
    }
    pub fn eval_expr(&mut self, e: ExprNodeId) -> Result<(VPtr, TypeNodeId), CompileError> {
        let span = e.to_span();
        let ty = self.typeenv.lookup_res(e);
        match &e.to_expr() {
            Expr::Literal(lit) => {
                let v = self.eval_literal(lit, &span)?;
                let t = InferContext::infer_type_literal(lit).map_err(CompileError::from)?;
                Ok((v, t))
            }
            Expr::Var(name, _time) => Ok((self.eval_var(*name, ty, &span)?, ty)),
            Expr::Block(b) => {
                if let Some(block) = b {
                    self.eval_expr(*block)
                } else {
                    Ok((Arc::new(Value::None), unit!()))
                }
            }
            Expr::Tuple(items) => {
                let len = items.len();
                if len == 0 {
                    unreachable!("0-length tuple is not supported");
                }
                let alloc_insert_point = self.get_current_basicblock().0.len();
                let dst = self.gen_new_register();
                for (i, e) in items.iter().enumerate() {
                    let (v, elem_ty) = self.eval_expr(*e)?;
                    let ptr = self.push_inst(Instruction::GetElement {
                        value: dst.clone(),
                        ty, // lazyly set after loops
                        array_idx: 0,
                        tuple_offset: i as u64,
                    });

                    self.push_inst(Instruction::Store(ptr, v, elem_ty));
                }
                self.get_current_basicblock()
                    .0
                    .insert(alloc_insert_point, (dst.clone(), Instruction::Alloc(ty)));
                // TODO: validate if the types are all identical?
                // let first_ty = &types[0];
                // if !types.iter().all(|x| x == first_ty) {
                //     todo!("Return error");
                // }

                // pass only the head of the tuple, and the length can be known
                // from the type information.
                Ok((dst, ty))
            }
            Expr::Proj(_, _) => todo!(),
            Expr::Apply(f, args) => {
                let (f, ft) = self.eval_expr(*f)?;
                let del = self.make_delay(&f, args)?;
                if let Some(d) = del {
                    Ok((d, Type::Primitive(PType::Numeric).into_id()))
                } else {
                    let atvvec = self.eval_args(args)?;
                    let rt = if let Type::Function(_, rt, _) = ft.to_type() {
                        rt
                    } else {
                        panic!("non function type {} {} ", ft.to_type(), ty.to_type());
                    };
                    let res = match f.as_ref() {
                        Value::Global(v) => match v.as_ref() {
                            Value::Function(idx, statesize, _rty) => {
                                self.emit_fncall(*idx as u64, *statesize, atvvec.clone(), rt)
                            }
                            Value::Register(_) => {
                                self.push_inst(Instruction::CallCls(v.clone(), atvvec.clone(), rt))
                            }
                            Value::FixPoint(fnid) => {
                                let clspos = self.push_inst(Instruction::Uinteger(*fnid as u64));
                                let cls = self.push_inst(Instruction::Closure(clspos));
                                self.push_inst(Instruction::CallCls(cls, atvvec.clone(), rt))
                            }
                            _ => {
                                panic!("calling non-function global value")
                            }
                        },
                        Value::Register(_) => {
                            //closure
                            //do not increment state size for closure
                            self.push_inst(Instruction::CallCls(f.clone(), atvvec.clone(), rt))
                        }
                        Value::FixPoint(fnid) => {
                            let clspos = self.push_inst(Instruction::Uinteger(*fnid as u64));
                            let cls = self.push_inst(Instruction::Closure(clspos));
                            self.push_inst(Instruction::CallCls(cls, atvvec.clone(), rt))
                        }

                        Value::Function(idx, statesize, _ret_t) => {
                            self.emit_fncall(*idx as u64, *statesize, atvvec.clone(), rt)
                        }
                        Value::ExtFunction(label, _ty) => {
                            if let Some(res) = self.make_intrinsics(*label, &atvvec)? {
                                res
                            } else {
                                self.push_inst(Instruction::Call(f.clone(), atvvec.clone(), rt))
                            }
                        }
                        // Value::ExternalClosure(i) => todo!(),
                        Value::None => unreachable!(),
                        _ => todo!(),
                    };
                    Ok((res, rt))
                }
            }
            Expr::Lambda(ids, _rett, body) => {
                let (atypes, rt) = match ty.to_type() {
                    Type::Function(atypes, rt, _) => (atypes.clone(), rt),
                    _ => panic!(),
                };
                let binds = ids
                    .iter()
                    .enumerate()
                    .zip(atypes.iter())
                    .map(|((idx, name), t)| {
                        let label = name.id;
                        let a = Argument(label, *t);
                        let res = (label, Arc::new(Value::Argument(idx, Arc::new(a))));
                        res
                    })
                    .collect::<Vec<_>>();

                let name = self.consume_fnlabel();
                let (c_idx, f) = self.do_in_child_ctx(name, &binds, &atypes, |ctx, c_idx| {
                    let (res, _) = ctx.eval_expr(*body)?;

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
                    match (res.as_ref(), rt.to_type()) {
                        (_, Type::Primitive(PType::Unit)) => {
                            let _ = ctx.push_inst(Instruction::Return(Arc::new(Value::None), rt));
                        }
                        (Value::State(v), _) => {
                            let _ = ctx.push_inst(Instruction::ReturnFeed(v.clone(), rt));
                        }
                        (Value::Function(i, _, _), _) => {
                            let idx = ctx.push_inst(Instruction::Uinteger(*i as u64));
                            let cls = ctx.push_inst(Instruction::Closure(idx));
                            let _ = ctx.push_inst(Instruction::Return(cls, rt));
                        }
                        (_, _) => {
                            let _ = ctx.push_inst(Instruction::Return(res.clone(), rt));
                        }
                    };

                    let f = Arc::new(Value::Function(c_idx, state_size, rt));
                    Ok((f, rt))
                })?;
                let child = self.program.functions.get_mut(c_idx).unwrap();
                let res = if child.upindexes.is_empty() {
                    //todo:make Closure
                    f
                } else {
                    let idxcell = self.push_inst(Instruction::Uinteger(c_idx as u64));
                    self.push_inst(Instruction::Closure(idxcell))
                };
                Ok((res, ty))
            }
            Expr::Feed(id, expr) => {
                //set typesize lazily
                let res = self.push_inst(Instruction::GetState(ty));
                self.get_ctxdata().state_offset += 1;
                self.add_bind((*id, res.clone()));
                let (retv, _t) = self.eval_expr(*expr)?;
                self.get_current_fn().state_size += 1;
                Ok((Arc::new(Value::State(retv)), ty))
            }
            Expr::Let(pat, body, then) => {
                if let Ok(tid) = TypedId::try_from(pat.clone()) {
                    self.fn_label = Some(tid.id);
                };
                let insert_pos = if self.program.functions.is_empty() {
                    0
                } else {
                    self.get_current_basicblock().0.len()
                };
                let is_global = self.get_ctxdata().func_i == 0;
                let (bodyv, t) = self.eval_expr(*body)?;
                //todo:need to boolean and insert cast
                self.fn_label = None;

                match (
                    is_global,
                    matches!(bodyv.as_ref(), Value::Function(_, _, _)),
                    then,
                ) {
                    (true, false, Some(then_e)) => {
                        let gv = Arc::new(Value::Global(bodyv.clone()));
                        if t.to_type().is_function() {
                            //globally allocated closures are immidiately closed, not to be disposed
                            let b = self.push_inst(Instruction::CloseUpValue(bodyv.clone()));
                            let _greg =
                                self.push_inst(Instruction::SetGlobal(gv.clone(), b, t.clone()));
                        } else {
                            let _greg = self.push_inst(Instruction::SetGlobal(
                                gv.clone(),
                                bodyv.clone(),
                                t.clone(),
                            ));
                        }
                        self.add_bind_pattern(pat, gv, t)?;
                        self.eval_expr(*then_e)
                    }
                    (false, false, Some(then_e)) => {
                        let alloc_res = self.gen_new_register();
                        let block = &mut self.get_current_basicblock().0;
                        block.insert(insert_pos, (alloc_res.clone(), Instruction::Alloc(t)));
                        let _ =
                            self.push_inst(Instruction::Store(alloc_res.clone(), bodyv.clone(), t));

                        self.add_bind_pattern(pat, alloc_res, t)?;
                        self.eval_expr(*then_e)
                    }
                    (_, _, Some(then_e)) => {
                        self.add_bind_pattern(pat, bodyv, t)?;
                        self.eval_expr(*then_e)
                    }
                    (_, _, None) => Ok((Arc::new(Value::None), unit!())),
                }
            }
            Expr::LetRec(id, body, then) => {
                self.fn_label = Some(id.id);
                let nextfunid = self.program.functions.len();
                let fix = Arc::new(Value::FixPoint(nextfunid));

                // let alloc = self.push_inst(Instruction::Alloc(t.clone()));
                // let _ = self.push_inst(Instruction::Store(alloc.clone(), fix));
                let bind = (id.id, fix);
                self.add_bind(bind);
                let (b, _bt) = self.eval_expr(*body)?;
                //set bind from fixpoint to computed lambda
                let (_, v) = self
                    .valenv
                    .0
                    .iter_mut()
                    .find_map(|lenv| lenv.iter_mut().find(|(name, _)| *name == id.id))
                    .unwrap();
                *v = b;
                if let Some(then_e) = then {
                    self.eval_expr(*then_e)
                } else {
                    Ok((Arc::new(Value::None), unit!()))
                }
            }
            Expr::If(cond, then, else_) => {
                let (c, _) = self.eval_expr(*cond)?;
                let bbidx = self.get_ctxdata().current_bb;
                let _ = self.push_inst(Instruction::JmpIf(
                    c,
                    (bbidx + 1) as u64,
                    (bbidx + 2) as u64,
                ));
                //insert then block
                let (t, _) = self.eval_block(Some(*then))?;
                //jmp to ret is inserted in bytecodegen
                //insert else block
                let (e, _) = self.eval_block(*else_)?;
                //insert return block
                self.add_new_basicblock();
                let res = self.push_inst(Instruction::Phi(t, e));
                Ok((res, ty))
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
    let infer_ctx = infer_root(expr2)
        .map_err(|err| Box::new(CompileError::from(err)) as Box<dyn ReportableError>)?;
    let mut ctx = Context::new(infer_ctx);
    let _res = ctx.eval_expr(expr2).map_err(|e| {
        let eb: Box<dyn ReportableError> = Box::new(e);
        eb
    })?;
    Ok(ctx.program.clone())
}
