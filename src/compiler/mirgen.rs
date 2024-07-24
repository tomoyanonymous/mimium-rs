use super::intrinsics;
use super::typing::{self, infer_type_literal, InferContext};
use crate::{numeric, unit};
pub(crate) mod recursecheck;
pub mod selfconvert;
use crate::mir::{self, Argument, Instruction, Label, Mir, UpIndex, VPtr, VReg, Value};

use std::sync::Arc;

use crate::types::{PType, Type};
use crate::utils::environment::{Environment, LookupRes};
use crate::utils::error::ReportableError;
use crate::utils::metadata::{Span, WithMeta, GLOBAL_LABEL};

use crate::ast::{Expr, Literal};
// pub mod closure_convert;
// pub mod feedconvert;
// pub mod hir_solve_stage;

#[derive(Debug, Default)]
struct ContextData {
    pub func_i: usize,
    pub current_bb: usize,
    pub state_offset: u64,
}
#[derive(Debug)]
pub struct Context {
    typeenv: InferContext,
    valenv: Environment<VPtr>,
    fn_label: Option<String>,
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

    fn consume_fnlabel(&mut self) -> String {
        let res = self.fn_label.clone().unwrap_or_else(|| {
            let res = format!("lambda_{}", self.anonymous_fncount);
            self.anonymous_fncount += 1;
            res
        });
        self.fn_label = None;
        res
    }

    fn get_current_fn(&mut self) -> &mut mir::Function {
        let i = self.get_ctxdata().func_i;
        &mut self.program.functions[i]
    }
    fn make_intrinsics(&mut self, label: &String, args: Vec<VPtr>) -> Option<VPtr> {
        let inst = match (label.as_str(), args.len()) {
            (intrinsics::ADD, 2) => Instruction::AddF(args[0].clone(), args[1].clone()),
            (intrinsics::SUB, 2) => Instruction::SubF(args[0].clone(), args[1].clone()),
            (intrinsics::MULT, 2) => Instruction::MulF(args[0].clone(), args[1].clone()),
            (intrinsics::DIV, 2) => Instruction::DivF(args[0].clone(), args[1].clone()),
            (intrinsics::EXP, 2) => Instruction::PowF(args[0].clone(), args[1].clone()),
            (intrinsics::MODULO, 2) => Instruction::ModF(args[0].clone(), args[1].clone()),
            (intrinsics::ABS, 1) => Instruction::AbsF(args[0].clone()),
            (intrinsics::SIN, 1) => Instruction::SinF(args[0].clone()),
            (intrinsics::COS, 1) => Instruction::CosF(args[0].clone()),
            (intrinsics::LOG, 2) => Instruction::LogF(args[0].clone(), args[1].clone()),

            _ => return None,
        };
        Some(self.push_inst(inst))
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
    fn push_inst(&mut self, inst: Instruction) -> VPtr {
        let res = Arc::new(Value::Register(self.reg_count));
        self.reg_count += 1;
        self.get_current_basicblock().0.push((res.clone(), inst));
        res
    }
    // fn push_upindex(&mut self, _v: mir::UpIndex) {
    //     // let fnproto = self.get_current_fn().unwrap();
    //     // fnproto.upindexes.push(v)
    // }
    fn add_bind(&mut self, bind: (String, VPtr)) {
        self.valenv.add_bind(&mut vec![bind]);
    }
    fn make_new_function(&mut self, name: &str, args: &[VPtr], parent_i: Option<usize>) -> usize {
        let newf = mir::Function::new(name, args, parent_i);
        self.program.functions.push(newf);
        let idx = self.program.functions.len() - 1;
        idx
    }
    fn do_in_child_ctx<F: FnMut(&mut Self, usize) -> Result<(VPtr, Type), CompileError>>(
        &mut self,
        fname: &String,
        binds: &[(String, VPtr, Type)],
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
        let vbinds = binds.iter().map(|(_, a, _)| a.clone()).collect::<Vec<_>>();
        self.valenv.extend();
        self.valenv.add_bind(&mut abinds);
        let label = self.get_ctxdata().func_i.clone();
        let c_idx = self.make_new_function(&fname, vbinds.as_slice(), Some(label));

        self.typeenv.env.extend();
        self.typeenv.env.add_bind(&mut atbinds);

        self.data.push(ContextData {
            func_i: c_idx,
            current_bb: 0,
            state_offset: 0,
        });
        self.data_i += 1;
        //do action
        let (fptr, rest) = action(self, c_idx)?;

        //post action
        let _ = self.data.pop();
        self.data_i -= 1;
        self.valenv.to_outer();
        self.typeenv.env.to_outer();
        Ok((c_idx, fptr, rest))
    }
    fn lookup(&self, key: &str) -> LookupRes<VPtr> {
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
    pub fn eval_var(&mut self, name: &str, span: &Span) -> Result<(VPtr, Type), CompileError> {
        let v = match self.lookup(name) {
            LookupRes::Local(v) => match v.as_ref() {
                Value::Function(i, _s) => {
                    let reg = self.push_inst(Instruction::Uinteger(*i as u64));
                    self.push_inst(Instruction::Closure(reg))
                }
                _ => self.push_inst(Instruction::Load(v.clone())),
            },
            LookupRes::UpValue(level, v) => (0..level).into_iter().rev().fold(v, |upv, i| {
                let current = self.data.get_mut(self.data_i - i).unwrap();
                let currentf = self.program.functions.get_mut(current.func_i).unwrap();
                let currentbb = currentf.body.get_mut(current.current_bb).unwrap();
                currentf.upindexes.push(upv.clone());
                let upi = (currentf.upindexes.len() - 1) as u64;

                let res = Arc::new(Value::Register(self.reg_count));
                self.reg_count += 1;
                currentbb
                    .0
                    .push((res.clone(), Instruction::GetUpValue(upi)));
                res
            }),
            LookupRes::Global(v) => Arc::new(Value::Global(v.clone())),
            LookupRes::None => {
                let ty =
                    typing::lookup(&name, &mut self.typeenv, span).map_err(CompileError::from)?;
                Arc::new(Value::ExtFunction(Label(name.to_string()), ty))
            }
        };

        let ty = typing::lookup(&name, &mut self.typeenv, span).map_err(CompileError::from)?;
        Ok((v, ty.clone()))
    }
    fn emit_fncall(&mut self, idx: u64, statesize: u64, args: Vec<VPtr>) -> VPtr {
        let f = {
            self.get_current_fn().state_size += statesize;
            self.push_inst(Instruction::Uinteger(idx))
        };
        //insert pushstateoffset
        if self.get_ctxdata().state_offset > 0 {
            self.get_current_basicblock()
                .0
                .push((Arc::new(Value::None), Instruction::PushStateOffset(1)));
        }

        let res = self.push_inst(Instruction::Call(f.clone(), args));
        if statesize > 0 {
            self.get_ctxdata().state_offset += 1;
        }
        res
    }
    pub fn eval_expr(&mut self, e_meta: &WithMeta<Expr>) -> Result<(VPtr, Type), CompileError> {
        let WithMeta(e, span) = e_meta;
        match e {
            Expr::Literal(lit) => {
                let v = self.eval_literal(lit, span)?;
                let t = infer_type_literal(lit).map_err(CompileError::from)?;
                Ok((v, t))
            }
            Expr::Var(name, _time) => self.eval_var(name, span),
            Expr::Block(b) => {
                if let Some(block) = b {
                    self.eval_expr(block)
                } else {
                    //todo?
                    Ok((Arc::new(Value::None), unit!()))
                }
            }
            Expr::Tuple(_) => todo!(),
            Expr::Proj(_, _) => todo!(),
            Expr::Apply(f, args) => {
                // skip type inference for now.
                // let ftype = infer_type(func, &mut self.typeenv).map_err(|typing::Error(e, span)| {
                //     CompileError(CompileErrorKind::TypingFailure(e), span)
                // })?;
                // let nret = if let Type::Primitive(PType::Unit) = ftype {
                //     0
                // } else {
                //     1
                // };
                let (f, t) = self.eval_expr(f)?;
                let rtres: Result<Type, CompileError> = if let Type::Function(_a, box r, _s) = t {
                    Ok(r)
                } else {
                    let terr = typing::ErrorKind::NonFunction(t);
                    Err(CompileError(
                        CompileErrorKind::TypingFailure(terr),
                        span.clone(),
                    ))
                };
                let rt = rtres?;
                let makeargs = |args: &Vec<WithMeta<Expr>>, ctx: &mut Context| {
                    args.iter()
                        .map(|a_meta| -> Result<Arc<Value>, CompileError> {
                            let (v, _t) = ctx.eval_expr(a_meta)?;
                            let res = v;
                            Ok(res)
                        })
                        .try_collect::<Vec<_>>()
                };
                let res = match f.as_ref() {
                    Value::Global(v) =>{
                        let a_regs = makeargs(args, self)?;
                        match v.as_ref(){
                            Value::Function(idx,statesize) =>{
                                self.emit_fncall(*idx as u64, *statesize,a_regs)
                            }
                            Value::Register(_c)=>{
                                self.push_inst(Instruction::CallCls(v.clone(), a_regs.clone()))
                            },
                            _ => {
                                panic!("calling non-function global value")
                            }
                        }
                    },
                    Value::Register(_c) => {
                        //closure
                        let a_regs = makeargs(args,self)?;
                        //do not increment state size for closure
                        let res = self.push_inst(Instruction::CallCls(f.clone(), a_regs.clone()));
                        res
                    }

                    Value::Function(idx, statesize) => {
                        let a_regs = makeargs(args, self)?;
                        self.emit_fncall(*idx as u64, *statesize,a_regs)
                    }
                    Value::Closure(v, _upindexes) if let Value::Function(idx, statesize) = v.as_ref() =>{
                        unreachable!()
                    }
                    Value::ExtFunction(label,_ty) => {
                        let a_regs = makeargs(args,self)?;

                        if let Some(res) = self.make_intrinsics(&label.0, a_regs.clone())
                        {
                        res
                        } else {
                            self.push_inst(Instruction::Call(f.clone(), a_regs.clone()))
                        }
                    }
                    // Value::ExternalClosure(i) => todo!(),
                    Value::None => unreachable!(),
                    _ => todo!(),
                };
                Ok((res, rt))
            }
            Expr::Lambda(ids, rett, body) => {
                let binds = ids
                    .iter()
                    .enumerate()
                    .map(|(idx, name)| {
                        let label = name.0.id.clone();
                        let a = Argument(
                            Label(label.clone()),
                            name.0.ty.clone().unwrap_or(Type::Unknown),
                        );
                        let t = name.0.ty.clone().unwrap_or_else(|| {
                            let tenv = &mut self.typeenv;
                            tenv.gen_intermediate_type()
                        });
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
                    self.do_in_child_ctx(&name, binds.as_slice(), |ctx, c_idx| {
                        let (res, mut res_type) = ctx.eval_expr(&body)?;
                        res_type = if let Some(rt) = rett {
                            let tenv = &mut ctx.typeenv;
                            tenv.unify_types(rt.clone(), res_type.clone())
                                .map_err(CompileError::from)?
                        } else {
                            res_type
                        };
                        let state_size = {
                            let child = ctx.program.functions.get_mut(c_idx).unwrap();
                            child.state_size
                        };
                        if ctx.get_ctxdata().state_offset > 1 && state_size > 0 {
                            ctx.get_current_basicblock().0.push((
                                Arc::new(mir::Value::None),
                                Instruction::PopStateOffset(state_size - 1),
                            )); //todo:offset size
                        }
                        match (res.as_ref(), res_type.clone()) {
                            (_, Type::Primitive(PType::Unit) | Type::Unknown) => {}
                            (Value::State(v), _) => {
                                let _ = ctx.push_inst(Instruction::ReturnFeed(v.clone()));
                            }
                            (_, _) => {
                                let _ = ctx.push_inst(Instruction::Return(res.clone()));
                            }
                        };

                        let f = Arc::new(Value::Function(c_idx, state_size));
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
                let fty = Type::Function(atypes, Box::new(res_type), None);
                Ok((res, fty))
            }
            Expr::Feed(id, expr) => {
                // self.reg_count += 1;
                let res = self.push_inst(Instruction::GetState);
                self.add_bind((id.clone(), res.clone()));
                let tf = {
                    let tf = self.typeenv.gen_intermediate_type();
                    self.typeenv
                        .env
                        .add_bind(&mut vec![(id.clone(), tf.clone())]);
                    tf
                };
                let (retv, t) = self.eval_expr(expr)?;
                self.typeenv.unify_types(tf, t.clone())?;
                self.get_current_fn().state_size += 1;
                Ok((Arc::new(Value::State(retv)), t))
            }
            Expr::Let(id, body, then) => {
                self.fn_label = Some(id.id.clone());
                let insert_pos = if self.program.functions.is_empty() {
                    0
                } else {
                    self.get_current_basicblock().0.len()
                };
                let (bodyv, t) = self.eval_expr(body)?;
                //todo:need to boolean and insert cast
                if let Some(idt) = id.ty.clone() {
                    self.typeenv.unify_types(idt, t.clone())?;
                }

                if let Some(then_e) = then {
                    self.typeenv
                        .env
                        .add_bind(&mut vec![(id.id.clone(), t.clone())]);
                    let res = if (t.is_primitive())
                        || (t.is_function() && self.get_current_fn().upperfn_i != Some(0))
                    {
                        let alloc_res = Arc::new(Value::Register(self.reg_count));
                        let block = &mut self.get_current_basicblock().0;
                        block.insert(insert_pos, (alloc_res.clone(), Instruction::Alloc(t)));
                        self.reg_count += 1;
                        let _ =
                            self.push_inst(Instruction::Store(alloc_res.clone(), bodyv.clone()));
                        alloc_res
                    } else {
                        bodyv
                    };
                    self.add_bind((id.id.clone(), res));
                    self.eval_expr(then_e)
                } else {
                    Ok((Arc::new(Value::None), unit!()))
                }
            }
            Expr::LetRec(id, body, then) => {
                self.fn_label = Some(id.id.clone());
                let t = {
                    let tenv = &mut self.typeenv;
                    //todo:need to boolean and insert cast
                    let t = tenv.gen_intermediate_type();
                    if let Some(idt) = id.ty.clone() {
                        tenv.unify_types(idt, t.clone())?;
                    }
                    tenv.env.add_bind(&mut vec![(id.id.clone(), t.clone())]);
                    t
                };
                let fix = Arc::new(Value::FixPoint);
                let alloc = self.push_inst(Instruction::Alloc(t.clone()));
                let _ = self.push_inst(Instruction::Store(alloc.clone(), fix));
                let bind = (id.id.clone(), alloc);
                self.add_bind(bind);
                let _ = self.eval_expr(body)?;
                if let Some(then_e) = then {
                    self.eval_expr(then_e)
                } else {
                    Ok((Arc::new(Value::None), unit!()))
                }
            }
            Expr::LetTuple(_, _, _) => todo!(),
            Expr::If(cond, then, else_) => {
                let (c, t_cond) = self.eval_expr(cond)?;
                //todo:need to boolean and insert cast
                self.typeenv.unify_types(t_cond, numeric!())?;

                let bbidx = self.get_ctxdata().current_bb;
                let _ = self.push_inst(Instruction::JmpIf(
                    c,
                    (bbidx + 1) as u64,
                    (bbidx + 2) as u64,
                ));
                //insert then block
                self.add_new_basicblock();
                let (t, thent) = self.eval_expr(then)?;
                //jmp to ret is inserted in bytecodegen

                //insert else block
                self.add_new_basicblock();
                let (e, elset) = match else_ {
                    Some(box e) => self.eval_expr(&e),
                    None => Ok((Arc::new(Value::None), unit!())),
                }?;
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

pub fn compile(src: WithMeta<Expr>) -> Result<Mir, Box<dyn ReportableError>> {
    let ast2 = recursecheck::convert_recurse(&src);
    let expr2 = selfconvert::convert_self_top(ast2).map_err(|e| {
        let eb: Box<dyn ReportableError> = Box::new(e);
        eb
    })?;
    let mut ctx = Context::new();
    let _res = ctx.eval_expr(&expr2).map_err(|e| {
        let eb: Box<dyn ReportableError> = Box::new(e);
        eb
    })?;
    Ok(ctx.program.clone())
}

fn resolve_upvalue(v: &Arc<Value>, ctx: &mut Context) {
    match v.as_ref() {
        Value::Argument(i, v) => {}
        Value::Register(i) => todo!(),
        Value::State(v) => todo!(),
        _ => unreachable!(),
    }
}
