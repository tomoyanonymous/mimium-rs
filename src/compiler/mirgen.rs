use super::intrinsics;
use super::typing::{self, infer_type, InferContext};
mod recursecheck;
mod selfconvert;
use crate::mir::{self, Argument, Function, Instruction, Label, Mir, VPtr, VReg, Value};
use std::sync::Arc;

use crate::types::{PType, Type};
use crate::utils::environment::{Environment, LookupRes};
use crate::utils::error::ReportableError;
use crate::utils::metadata::{Span, WithMeta};

use crate::ast::{Expr, Literal};
// pub mod closure_convert;
// pub mod feedconvert;
// pub mod hir_solve_stage;

#[derive(Debug)]
pub struct Context {
    pub typeenv: typing::InferContext,
    pub program: Mir,
    valenv: Environment<VPtr>,
    current_fn_idx: usize,
    current_bb: usize,
    upvalue_counts: Vec<usize>,
    reg_count: VReg,
    fn_label: Option<String>,
    anonymous_fncount: u64,
    state_offset: u64,
}

impl Context {
    pub fn new() -> Self {
        Self {
            typeenv: InferContext::new(),
            valenv: Environment::new(),
            program: Mir::default(),
            current_fn_idx: 0,
            current_bb: 0,
            upvalue_counts: vec![],
            reg_count: 0,
            fn_label: None,
            anonymous_fncount: 0,
            state_offset: 0,
        }
    }
    pub fn get_current_fn(&mut self) -> Option<&mut mir::Function> {
        self.program.functions.get_mut(self.current_fn_idx)
    }
    pub fn get_fn_from_id(&mut self, id: usize) -> Option<&mut mir::Function> {
        self.program.functions.get_mut(id)
    }
    fn make_new_fname(&mut self) -> String {
        let res = self.fn_label.clone().unwrap_or_else(|| {
            let res = format!("lambda_{}", self.anonymous_fncount);
            self.anonymous_fncount += 1;
            res
        });
        self.fn_label = None;
        res
    }
    pub fn make_new_fn(&mut self, args: &[VPtr]) -> (&mut Function, usize) {
        let name = self.make_new_fname();
        let newf = mir::Function::new(name.as_str(), args);
        self.program.functions.push(newf);
        let idx = self.program.functions.len() - 1;
        self.current_bb = 0;
        self.current_fn_idx = idx;
        (self.program.functions.last_mut().unwrap(), idx)
    }
    pub fn make_intrinsics(&mut self, label: &String, args: Vec<VPtr>) -> Option<VPtr> {
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
            .unwrap()
            .body
            .get_mut(bbid)
            .expect("no basic block found")
    }
    pub fn push_inst(&mut self, inst: Instruction) -> VPtr {
        let res = Arc::new(Value::Register(self.reg_count));
        self.reg_count += 1;
        self.get_current_basicblock().0.push((res.clone(), inst));
        res
    }
    pub fn push_upindex(&mut self, _v: mir::UpIndex) {
        // let fnproto = self.get_current_fn().unwrap();
        // fnproto.upindexes.push(v)
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
    let mut ctx = Context::new();
    let ast2 = recursecheck::convert_recurse(&src);
    let expr2 = selfconvert::convert_self_top(ast2).map_err(|e| {
        let eb: Box<dyn ReportableError> = Box::new(e);
        eb
    })?;
    let _res = eval_expr(&expr2, &mut ctx).map_err(|e| {
        let eb: Box<dyn ReportableError> = Box::new(e);
        eb
    })?;
    Ok(ctx.program)
}
// fn load_new_rawv(rawv: RawVal, func: &mut FuncProto) -> Result<u8, CompileError> {
//     let idx = func.constants.binary_search(&rawv).unwrap_or_else(|_| {
//         func.constants.push(rawv);
//         if func.constants.is_empty() {
//             panic!("failed to push constant to funcproto")
//         }
//         func.constants.len() - 1
//     });
//     if idx > u8::MAX as usize {
//         Err(Box::<dyn ReportableError>::new(CompileError(CompileErrorKind::TooManyConstants,0..=0)))
//     } else {
//         Ok(idx as u8)
//     }
// }
// fn load_new_float(v: f64, func: &mut FuncProto) -> Result<u8, CompileError> {
//     let rawv: RawVal = unsafe { std::mem::transmute(v) };
//     load_new_rawv(rawv, func)
// }
// fn load_new_int(v: i64, func: &mut FuncProto) -> Result<u8, CompileError> {
//     let rawv: RawVal = unsafe { std::mem::transmute(v) };
//     load_new_rawv(rawv, func)
// }

fn eval_literal(lit: &Literal, _span: &Span, ctx: &mut Context) -> Result<VPtr, CompileError> {
    let v = match lit {
        Literal::String(_) => todo!(),
        Literal::Int(i) => ctx.push_inst(Instruction::Integer(*i)),
        Literal::Float(f) => ctx.push_inst(Instruction::Float(
            f.parse::<f64>().expect("illegal float format"),
        )),
        Literal::SelfLit => unreachable!(),
        Literal::Now => todo!(),
    };
    Ok(v)
}

fn eval_expr(e_meta: &WithMeta<Expr>, ctx: &mut Context) -> Result<VPtr, CompileError> {
    let WithMeta(e, span) = e_meta;
    match e {
        Expr::Literal(lit) => Ok(eval_literal(lit, span, ctx)?),
        Expr::Var(v, _time) => {
            match ctx.valenv.lookup_cls(v) {
                LookupRes::Local(v) => Ok(v.clone()),
                LookupRes::UpValue(_v) => {
                    todo!();
                }
                LookupRes::Global(v) => Ok(v.clone()),
                LookupRes::None => {
                    // let t = infer_type(e, &mut ctx.typeenv).expect("type infer error");
                    // program.ext_cls_table.push((v.clone(),t));
                    Ok(Arc::new(Value::ExtFunction(Label(v.clone()))))
                }
            }
        }
        Expr::Block(b) => {
            if let Some(block) = b {
                eval_expr(block, ctx)
            } else {
                //todo?
                Ok(Arc::new(Value::None))
            }
        }
        Expr::Tuple(_) => todo!(),
        Expr::Proj(_, _) => todo!(),
        Expr::Apply(f, args) => {
            // skip type inference for now.
            // let ftype = infer_type(func, &mut ctx.typeenv).map_err(|typing::Error(e, span)| {
            //     CompileError(CompileErrorKind::TypingFailure(e), span)
            // })?;
            // let nret = if let Type::Primitive(PType::Unit) = ftype {
            //     0
            // } else {
            //     1
            // };
            let f = eval_expr(f, ctx)?;

            let makeargs = |args: &Vec<WithMeta<Expr>>, ctx: &mut Context| {
                args.iter()
                    .map(|a_meta| eval_expr(a_meta, ctx))
                    .try_collect::<Vec<_>>()
            };
            let res = match f.as_ref() {
                Value::Register(_p) => {
                    todo!();
                }
                Value::Function(idx, statesize) => {
                    ctx.get_current_fn().unwrap().state_size += statesize;
                    //insert pushstateoffset

                    let f = ctx.push_inst(Instruction::Uinteger(*idx as u64));
                    let a_regs = makeargs(args, ctx)?;
                    if ctx.state_offset > 0 {
                        ctx.get_current_basicblock()
                            .0
                            .push((Arc::new(Value::None), Instruction::PushStateOffset(1)));
                    }

                    let res = ctx.push_inst(Instruction::Call(f.clone(), a_regs.clone()));
                    if *statesize > 0 {
                        ctx.state_offset += 1;
                    }
                    Ok(res)
                }
                Value::ExtFunction(label) => {
                    let a_regs = makeargs(args, ctx)?;
                    if let Some(res) = ctx.make_intrinsics(&label.0, a_regs.clone()) {
                        Ok(res)
                    } else {
                        todo!()
                    }
                }
                // Value::ExternalClosure(i) => todo!(),
                Value::None => unreachable!(),
                _ => todo!(),
            };
            res
        }
        Expr::Lambda(ids, _types, body) => {
            let tmp_reg = ctx.reg_count;

            let mut binds = ids
                .iter()
                .enumerate()
                .map(|(idx, name)| {
                    let label = name.0.id.clone();
                    let a = Argument(
                        Label(label.clone()),
                        name.0.ty.clone().unwrap_or(Type::Unknown),
                    );
                    let res = (label.clone(), Arc::new(Value::Argument(idx, Arc::new(a))));
                    res
                })
                .collect::<Vec<_>>();

            ctx.reg_count = 0;
            let (_newf, fnid) = ctx.make_new_fn(
                binds
                    .iter()
                    .map(|(_, v)| v.clone())
                    .collect::<Vec<_>>()
                    .as_slice(),
            );

            let mut tbinds = ids
                .iter()
                .map(|name| {
                    (
                        name.0.id.clone(),
                        name.0
                            .ty
                            .clone()
                            .unwrap_or_else(|| ctx.typeenv.gen_intermediate_type()),
                    )
                })
                .collect::<Vec<_>>();
            ctx.typeenv.env.extend();
            ctx.valenv.extend();
            ctx.reg_count += binds.len() as u64;

            ctx.valenv.add_bind(&mut binds);
            ctx.typeenv.env.add_bind(&mut tbinds);
            let res_type =
                infer_type(&body.0, &mut ctx.typeenv).map_err(|e| CompileError::from(e))?;
            let res = eval_expr(&body, ctx)?;
            let state_size = ctx.get_current_fn().unwrap().state_size;
            if ctx.state_offset > 0 {
                ctx.get_current_basicblock().0.push((
                    Arc::new(mir::Value::None),
                    Instruction::PopStateOffset(state_size - 1),
                )); //todo:offset size
            }
            match (res.as_ref(), res_type) {
                (_, Type::Primitive(PType::Unit) | Type::Unknown) => {}
                (Value::State(v), _) => {
                    let _ = ctx.push_inst(Instruction::ReturnFeed(v.clone()));
                }
                (_, _) => {
                    let _ = ctx.push_inst(Instruction::Return(res.clone()));
                }
            };
            ctx.reg_count = tmp_reg;
            ctx.typeenv.env.to_outer();
            ctx.valenv.to_outer();
            Ok(Arc::new(Value::Function(fnid, state_size)))
        }
        Expr::Feed(id, expr) => {
            // ctx.reg_count += 1;
            let res = ctx.push_inst(Instruction::GetState);

            ctx.valenv.add_bind(&mut vec![(id.clone(), res.clone())]);
            let retv = eval_expr(expr, ctx)?;
            ctx.get_current_fn().unwrap().state_size += 1;
            Ok(Arc::new(Value::State(retv)))
        }
        Expr::Let(id, body, then) => {
            ctx.fn_label = Some(id.id.clone());
            let bodyv = eval_expr(body, ctx)?;
            let bodyt = infer_type(&body.0, &mut ctx.typeenv)?;
            ctx.valenv.add_bind(&mut vec![(id.id.clone(), bodyv)]);
            ctx.typeenv.env.add_bind(&mut vec![(id.id.clone(), bodyt)]);
            if let Some(then_e) = then {
                eval_expr(then_e, ctx)
            } else {
                Ok(Arc::new(Value::None))
            }
        }
        Expr::LetRec(id, body, then) => {
            let bind = (id.id.clone(), Arc::new(Value::FixPoint));
            let bodyt = infer_type(&body.0, &mut ctx.typeenv)?;

            ctx.fn_label = Some(id.id.clone());
            ctx.valenv.add_bind(&mut vec![bind]);

            ctx.typeenv.env.add_bind(&mut vec![(id.id.clone(), bodyt)]);
            let _ = eval_expr(body, ctx)?;
            if let Some(then_e) = then {
                eval_expr(then_e, ctx)
            } else {
                Ok(Arc::new(Value::None))
            }
        }
        Expr::LetTuple(_, _, _) => todo!(),
        Expr::If(_, _, _) => todo!(),
        Expr::Bracket(_) => todo!(),
        Expr::Escape(_) => todo!(),
        Expr::Error => todo!(),
        Expr::Assign(_, _) => todo!(),
        Expr::Then(_, _) => todo!(),
    }
}
