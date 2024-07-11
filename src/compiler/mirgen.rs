use super::intrinsics;
use crate::{numeric, unit};
use super::typing::{self, infer_type, infer_type_literal, lookup, InferContext};
mod recursecheck;
mod selfconvert;
use crate::mir::{self, Argument, Instruction, Label, Mir, UpIndex, VPtr, VReg, Value};
use std::rc::Rc;
use std::sync::Arc;

use crate::types::{PType, Type};
use crate::utils::environment::LookupRes;
use crate::utils::error::ReportableError;
use crate::utils::metadata::{Span, WithMeta};

use crate::ast::{Expr, Literal};
use std::cell::{OnceCell, RefCell};
// pub mod closure_convert;
// pub mod feedconvert;
// pub mod hir_solve_stage;

#[derive(Debug)]
pub struct Context {
    parent: Option<Rc<RefCell<Self>>>,
    pub typeenv: Rc<RefCell<InferContext>>,
    valenv: Vec<(String, VPtr)>,
    pub program: Rc<RefCell<Mir>>,
    func: Option<mir::Function>,
    current_bb: usize,
    upvalue_counts: Vec<usize>,
    reg_count: VReg,
    fn_label: Option<String>,
    anonymous_fncount: OnceCell<u64>,
    state_offset: u64,
}

fn make_child_ctx(parentcell: Rc<RefCell<Context>>, arg_binds: &[(String, VPtr,Type)]) -> Context {
    let parent = parentcell.borrow_mut();
    let afncount = *parent.anonymous_fncount.get().unwrap();
    let name = parent
        .fn_label
        .clone()
        .unwrap_or_else(|| format!("lambda_{}", afncount));
    let args = arg_binds.iter().map(|(_, v,_t)| v.clone()).collect::<Vec<_>>();
    let newf = mir::Function::new(&name, &args);
    let parent2 = Some(parentcell.clone());
    let tenvcell = parent.typeenv.clone();
    let mut tenv = tenvcell.borrow_mut();
    tenv.env.extend();
    let mut atbinds = arg_binds.iter().map(|(a, _,t)| (a.clone(),t.clone())).collect::<Vec<_>>();
    tenv.env.add_bind(&mut atbinds);
    let avbinds = arg_binds.iter().map(|(a, v,_)| (a.clone(),v.clone())).collect::<Vec<_>>();
    Context {
        parent: parent2,
        typeenv: tenvcell.clone(),
        valenv: avbinds,
        program: parent.program.clone(),
        func: Some(newf),
        current_bb: 0,
        upvalue_counts: vec![],
        reg_count: args.len() as u64,
        fn_label: name.into(),
        anonymous_fncount: afncount.into(),
        state_offset: 0,
    }
}

impl Context {
    pub fn new() -> Self {
        Self {
            parent: None,
            typeenv: Rc::new(RefCell::new(InferContext::new())),
            valenv: vec![],
            program: Rc::new(RefCell::new(Mir::default())),
            func: None,
            current_bb: 0,
            upvalue_counts: vec![],
            reg_count: 0,
            fn_label: String::from("_mimium_global").into(),
            anonymous_fncount: 0.into(),
            state_offset: 0,
        }
    }

    fn get_current_fn(&mut self) -> &mut mir::Function {
        self.func.as_mut().unwrap()
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
        let bbid = self.current_bb;
        self.get_current_fn()
            .body
            .get_mut(bbid)
            .expect("no basic block found")
    }
    fn add_new_basicblock(&mut self) {
        let idx = self.get_current_fn().add_new_basicblock();
        self.current_bb = idx;
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
        self.valenv.push(bind);
    }
    fn lookup(&self, key: &str) -> LookupRes<VPtr> {
        let local = self.valenv.iter().find(|(name, v)| name == key);
        let parent = &self.parent;
        match (local, parent) {
            (Some((_localname, v)), None) => LookupRes::Global(v.clone()),
            (Some((_localname, v)), _) => LookupRes::Local(v.clone()),
            (None, None) => LookupRes::None,
            (None, Some(p)) => p.borrow().lookup_upvalue(key),
        }
    }
    fn lookup_upvalue(&self, key: &str) -> LookupRes<VPtr> {
        let local = self.valenv.iter().find(|(name, v)| name == key);
        let parent = &self.parent;
        match (local, parent) {
            (Some((_localname, v)), None) => LookupRes::Global(v.clone()),
            (Some((_upname, v)), _) => LookupRes::UpValue(v.clone()),
            (None, None) => LookupRes::None,
            (None, Some(p)) => p.borrow().lookup_upvalue(key),
        }
    }
    fn eval_literal(&mut self, lit: &Literal, _span: &Span) -> Result<VPtr, CompileError> {
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
}

fn eval_expr(
    ctx_cell: &Rc<RefCell<Context>>,
    e_meta: &WithMeta<Expr>,
) -> Result<(VPtr,Type), CompileError> {
    let WithMeta(e, span) = e_meta;
    match e {
        Expr::Literal(lit) => {
            let v = ctx_cell.borrow_mut().eval_literal(lit, span)?;
            let t = infer_type_literal(lit).map_err(CompileError::from)?;
            Ok((v,t))
        }
        Expr::Var(name, _time) => {
            let mut ctx = ctx_cell.borrow_mut();
            
            let v = match ctx.lookup(name) {
                LookupRes::Local(v) => v.clone(),
                LookupRes::UpValue(v) => {
                    let fnlabel = ctx.parent.clone().unwrap().borrow_mut().fn_label.clone().unwrap();
                    let upindexes = &mut ctx.get_current_fn().upindexes;
                    upindexes.push(v.clone());
                    let i = (upindexes.len()-1) as u64;
                    let upv = ctx.push_inst(Instruction::GetUpValue(fnlabel, i));
                    upv
                }
                LookupRes::Global(v) => v.clone(),
                LookupRes::None => {
                    // let t = infer_type(e, &mut self.typeenv).expect("type infer error");
                    // program.ext_cls_table.push((v.clone(),t));
                    Arc::new(Value::ExtFunction(Label(name.clone())))
                }
            };
            let mut tenv = ctx.typeenv.borrow_mut();
            let ty = typing::lookup(&name,&mut tenv , span).map_err(CompileError::from)?;
            Ok((v,ty.clone()))
        }
        Expr::Block(b) => {
            if let Some(block) = b {
                eval_expr(ctx_cell, block)
            } else {
                //todo?
                Ok((Arc::new(Value::None),unit!()))
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
            let (f,t) = eval_expr(ctx_cell, f)?;
            let rtres: Result<Type,CompileError> = if let Type::Function(_a,box r,_s) = t {
                Ok(r)
            }else{
                let terr  = typing::ErrorKind::NonFunction(t);
                Err(CompileError(CompileErrorKind::TypingFailure(terr), span.clone()))
            };
            let rt = rtres?;
            let makeargs = |args: &Vec<WithMeta<Expr>>,ctx_cell:&Rc<RefCell<Context>>| {
                args.iter()
                    .map(|a_meta| eval_expr(ctx_cell, a_meta).map(|t| t.0))
                    .try_collect::<Vec<_>>()
            };
            let res = match f.as_ref() {
                Value::Register(_c) => {
                    //closure
                    let a_regs = makeargs(args,ctx_cell)?;
                    let mut ctx = ctx_cell.borrow_mut();

                    //do not increment state size for closure
                    let res = ctx.push_inst(Instruction::CallCls(f.clone(), a_regs.clone()));
                    res
                }
                Value::Function(idx, statesize) => {
                    let f =  {
                        let mut ctx = ctx_cell.borrow_mut();
                        ctx.get_current_fn().state_size += statesize;
                        ctx.push_inst(Instruction::Uinteger(*idx as u64))
                    };
                    let a_regs = makeargs(args,ctx_cell)?;
                    let mut ctx = ctx_cell.borrow_mut();
                    //insert pushstateoffset
                    if ctx.state_offset > 0 {
                        ctx.get_current_basicblock()
                            .0
                            .push((Arc::new(Value::None), Instruction::PushStateOffset(1)));
                    }

                    let res = ctx.push_inst(Instruction::Call(f.clone(), a_regs.clone()));
                    if *statesize > 0 {
                        ctx.state_offset += 1;
                    }
                    res
                }
                Value::Closure(v, upindexes) if let Value::Function(idx, statesize) =v.as_ref()
                    => 
                {
                    unreachable!()
                }
                Value::ExtFunction(label) => {
                    let a_regs = makeargs(args,ctx_cell)?;
                    if let Some(res) = ctx_cell
                        .borrow_mut()
                        .make_intrinsics(&label.0, a_regs.clone())
                    {
                    res
                    } else {
                        todo!()
                    }
                }

                // Value::ExternalClosure(i) => todo!(),
                Value::None => unreachable!(),
                _ => todo!(),
            };
            Ok((res,rt))
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
                    let t = name.0.ty.clone().unwrap_or_else(||{
                        let ctx = ctx_cell.borrow_mut();
                        let mut tenv = ctx.typeenv.borrow_mut();
                        tenv.gen_intermediate_type()
                        });
                    let res = (label.clone(), Arc::new(Value::Argument(idx, Arc::new(a))),t);
                    res
                })
                .collect::<Vec<_>>();
            let atypes = binds.iter().map(|(name,a,t)| t.clone()).collect::<Vec<_>>();
            let child = make_child_ctx(ctx_cell.clone(), &binds);
            let child_cell = Rc::new(RefCell::new(child));

 
            let (res,res_type) = eval_expr(&child_cell, &body)?;
            if let Some(rt) = rett{
                let ctx = ctx_cell.borrow_mut();
                let mut tenv = ctx.typeenv.borrow_mut();
                tenv.unify_types(rt.clone(), res_type.clone()).map_err(CompileError::from)?;
            }
            let mut c_ctx = child_cell.borrow_mut();

            let state_size = c_ctx.get_current_fn().state_size;
            if c_ctx.state_offset > 1 && state_size > 0 {
                c_ctx.get_current_basicblock().0.push((
                    Arc::new(mir::Value::None),
                    Instruction::PopStateOffset(state_size - 1),
                )); //todo:offset size
            }
            match (res.as_ref(), res_type.clone()) {
                (_, Type::Primitive(PType::Unit) | Type::Unknown) => {}
                (Value::State(v), _) => {
                    let _ = c_ctx.push_inst(Instruction::ReturnFeed(v.clone()));
                }
                (_, _) => {
                    let _ = c_ctx.push_inst(Instruction::Return(res.clone()));
                }
            };
            let idx = {
                let fns = &mut c_ctx.program.borrow_mut().functions;
                fns.push(c_ctx.func.as_ref().unwrap().clone());
                fns.len() - 1
            };
            let f = Arc::new(Value::Function(idx, state_size));
            let res = if c_ctx.get_current_fn().upindexes.is_empty() {
                //todo:make Closure
                f
            } else {
                let mut ctx = ctx_cell.borrow_mut();
                let idxcell = ctx.push_inst(Instruction::Uinteger(idx as u64));
                ctx.push_inst(Instruction::Closure(idxcell))
            };
            let fty = Type::Function(atypes,Box::new(res_type),None);
            Ok((res,fty))
        }
        Expr::Feed(id, expr) => {
            // self.reg_count += 1;
            let res = ctx_cell.borrow_mut().push_inst(Instruction::GetState);
            ctx_cell.borrow_mut().add_bind((id.clone(), res.clone()));
            let tf = {
                let ctx = ctx_cell.borrow_mut();
                let mut tenv = ctx.typeenv.borrow_mut();
                let tf = tenv.gen_intermediate_type();
                tenv.env.add_bind(&mut vec![(id.clone(),tf.clone())]);
                tf
            };
            let (retv,t) = eval_expr(ctx_cell, expr)?;
            {
                let ctx = ctx_cell.borrow_mut();
                let mut tenv = ctx.typeenv.borrow_mut();
                tenv.unify_types(tf,t.clone())?;
            }
            ctx_cell.borrow_mut().get_current_fn().state_size += 1;
            Ok((Arc::new(Value::State(retv)),t))
        }
        Expr::Let(id, body, then) => {
            ctx_cell.borrow_mut().fn_label = Some(id.id.clone());
            let (bodyv,t) = eval_expr(ctx_cell, body)?;
            {
                let ctx = ctx_cell.borrow_mut();
                let mut tenv = ctx.typeenv.borrow_mut();
                //todo:need to boolean and insert cast
                if let Some(idt) = id.ty.clone(){
                    tenv.unify_types(idt, t.clone())?;
                }
                tenv.env.add_bind(&mut vec![(id.id.clone(),t)]);
            }
            ctx_cell.borrow_mut().add_bind((id.id.clone(), bodyv));
            if let Some(then_e) = then {
                eval_expr(ctx_cell, then_e)
            } else {
                Ok((Arc::new(Value::None),unit!()))
            }
        }
        Expr::LetRec(id, body, then) => {
            let bind = (id.id.clone(), Arc::new(Value::FixPoint));
            ctx_cell.borrow_mut().fn_label = Some(id.id.clone());
            ctx_cell.borrow_mut().add_bind(bind);
            {
                let ctx = ctx_cell.borrow_mut();
                let mut tenv = ctx.typeenv.borrow_mut();
                //todo:need to boolean and insert cast
                let t = tenv.gen_intermediate_type();
                tenv.env.add_bind(&mut vec![(id.id.clone(),t)]);
            }
            let _ = eval_expr(ctx_cell, body)?;
            if let Some(then_e) = then {
                eval_expr(ctx_cell, then_e)
            } else {
                Ok((Arc::new(Value::None),unit!()))
            }
        }
        Expr::LetTuple(_, _, _) => todo!(),
        Expr::If(cond, then, else_) => {
            let (c,t_cond) = eval_expr(ctx_cell, cond)?;
            {
                let ctx = ctx_cell.borrow_mut();
                let mut tenv = ctx.typeenv.borrow_mut();
                //todo:need to boolean and insert cast
                tenv.unify_types(t_cond, numeric!())?;
            }
            let bbidx = ctx_cell.borrow().current_bb;
            let _ = ctx_cell.borrow_mut().push_inst(Instruction::JmpIf(
                c,
                (bbidx + 1) as u64,
                (bbidx + 2) as u64,
            ));
            //insert then block
            ctx_cell.borrow_mut().add_new_basicblock();
            let (t,thent) = eval_expr(ctx_cell, then)?;
            //jmp to ret is inserted in bytecodegen

            //insert else block
            ctx_cell.borrow_mut().add_new_basicblock();

            let (e,elset) = match else_ {
                Some(box e) => eval_expr(ctx_cell, &e),
                None =>                 Ok((Arc::new(Value::None),unit!()))

            }?;
            {
                let ctx = ctx_cell.borrow_mut();
                let mut tenv = ctx.typeenv.borrow_mut();
                tenv.unify_types(thent.clone(),elset)?;
            }
            //insert return block
            ctx_cell.borrow_mut().add_new_basicblock();
            let res = ctx_cell.borrow_mut().push_inst(Instruction::Phi(t, e));
            Ok((res,thent))
        }
        Expr::Bracket(_) => todo!(),
        Expr::Escape(_) => todo!(),
        Expr::Error => todo!(),
        Expr::Assign(_, _) => todo!(),
        Expr::Then(_, _) => todo!(),
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
    let ctxcell = Rc::new(RefCell::new(Context::new()));
    let ast2 = recursecheck::convert_recurse(&src);
    let expr2 = selfconvert::convert_self_top(ast2).map_err(|e| {
        let eb: Box<dyn ReportableError> = Box::new(e);
        eb
    })?;
    let _res = eval_expr(&ctxcell, &expr2).map_err(|e| {
        let eb: Box<dyn ReportableError> = Box::new(e);
        eb
    })?;
    let ctx = ctxcell.borrow();
    let program = ctx.program.borrow();
    Ok(program.clone())
}

fn resolve_upvalue(v: &Arc<Value>, ctx: &mut Context) {
    match v.as_ref() {
        Value::Argument(i, v) => {}
        Value::Register(i) => todo!(),
        Value::State(v) => todo!(),
        _ => unreachable!(),
    }
}
