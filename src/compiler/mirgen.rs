use super::typing::{self, infer_type, InferContext};

use std::default;
use std::sync::Arc;
// use crate::runtime::vm::bytecode::Instruction;
// use crate::runtime::vm::{FuncProto, RawVal, UpIndex};
use crate::mir::{self, Argument, Function, Instruction, Label, Local, Mir, VPtr, VReg, Value};

use crate::types::{PType, Type};
use crate::utils::environment::{Environment, LookupRes};
use crate::utils::error::ReportableError;
use crate::utils::metadata::{Span, WithMeta};

use crate::ast::{Expr, Literal};
use crate::runtime::{vm, vm::bytecode::*, vm::Program};
// pub mod closure_convert;
// pub mod feedconvert;
// pub mod hir_solve_stage;
#[derive(Clone, Debug)]
enum Val {
    Register(Reg),
    Closure(Reg, Type),
    Function(u8),
    ExternalFun(u8),
    ExternalClosure(u8),
    None,
}
#[derive(Debug)]
pub struct Context {
    pub typeenv: typing::InferContext,
    pub program: Mir,
    valenv: Environment<VPtr>,
    current_fn_idx: usize,
    current_bb: usize,
    upvalue_counts: Vec<usize>,
    pub stack_pos: usize,
    reg_count: VReg,
    fn_label: Option<String>,
    anonymous_fncount: u64,
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
            stack_pos: 0,
            reg_count: 0,
            fn_label: None,
            anonymous_fncount: 0,
        }
    }
    pub fn get_current_fn(&mut self) -> Option<&mut mir::Function> {
        self.program.functions.get_mut(self.current_fn_idx)
    }
    pub fn get_fn_from_id(&mut self, id: usize) -> Option<&mut mir::Function> {
        self.program.functions.get_mut(id)
    }
    pub fn make_new_fn(&mut self) -> (&mut Function, usize) {
        let label = self.fn_label.clone().unwrap_or_else(|| {
            let res = format!("lambda_{}", self.anonymous_fncount);
            self.anonymous_fncount += 1;
            res
        });
        self.fn_label = None;
        let mut newf = Function::default();
        newf.label = Label(label);
        newf.body.push(mir::Block::default());
        self.program.functions.push(newf);
        let idx = self.program.functions.len() - 1;

        self.current_bb = 0;
        self.current_fn_idx = idx;
        (self.program.functions.last_mut().unwrap(), idx)
    }
    pub fn make_intrinsics(&mut self, label: &String, args: Vec<VPtr>) -> Option<VPtr> {
        let inst = match (label.as_str(), args.len()) {
            ("add", 2) => Instruction::AddF(args[0].clone(), args[1].clone()),
            ("sub", 2) => Instruction::SubF(args[0].clone(), args[1].clone()),
            ("mul", 2) => Instruction::MulF(args[0].clone(), args[1].clone()),
            ("div", 2) => Instruction::DivF(args[0].clone(), args[1].clone()),
            _ => return None,
        };
        Some(Arc::new(Value::Register(self.push_inst(inst))))
    }
    fn get_current_basicblock(&mut self) -> &mut mir::Block {
        let bbid = self.current_bb;
        self.get_current_fn()
            .unwrap()
            .body
            .get_mut(bbid)
            .expect("no basic block found")
    }
    pub fn push_inst(&mut self, inst: Instruction) -> VReg {
        let res = self.reg_count;
        self.reg_count += 1;
        self.get_current_basicblock().0.push(inst);
        res
    }
    pub fn push_upindex(&mut self, v: mir::UpIndex) {
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
impl ReportableError for CompileError {
    fn get_span(&self) -> std::ops::Range<usize> {
        self.1.clone()
    }
}

pub fn compile(src: WithMeta<Expr>) -> Result<Mir, Box<dyn ReportableError>> {
    let mut ctx = Context::new();
    let _res = eval_expr(&src, &mut ctx).map_err(|e| Box::new(e) as Box<dyn ReportableError>)?;
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

fn eval_literal(lit: &Literal, span: &Span, ctx: &mut Context) -> Result<VReg, CompileError> {
    ctx.stack_pos += 1;
    let stack_pos = ctx.stack_pos;
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
        Expr::Literal(lit) => Ok(Arc::new(Value::Register(eval_literal(lit, span, ctx)?))),
        Expr::Var(v, _time) => {
            match ctx.valenv.lookup_cls(v) {
                LookupRes::Local(v) => Ok(v.clone()),
                LookupRes::UpValue(v) => {
                    todo!();
                }
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
            let box WithMeta(func, span) = f;
            // skip type inference for now.
            // let ftype = infer_type(func, &mut ctx.typeenv).map_err(|typing::Error(e, span)| {
            //     CompileError(CompileErrorKind::TypingFailure(e), span)
            // })?;
            // let nret = if let Type::Primitive(PType::Unit) = ftype {
            //     0
            // } else {
            //     1
            // };
            let nargs = args.len();
            let stack_base = ctx.stack_pos + 1;
            let f = eval_expr(f, ctx)?;
            let a_regs = args
                .iter()
                .map(|a_meta| eval_expr(a_meta, ctx))
                .try_collect::<Vec<_>>()?;
            match f.as_ref() {
                Value::ExtFunction(label) => {
                    if let Some(res) = ctx.make_intrinsics(&label.0, a_regs.clone()) {
                        return Ok(res);
                    }
                }
                _ => {}
            };

            // let inst =
            match f.as_ref() {
                Value::Register(p) => Ok(Arc::new(Value::Register(
                    ctx.push_inst(Instruction::Call(f.clone(), a_regs)),
                ))),
                Value::Function(i) => Ok(Arc::new(Value::Register(
                    ctx.push_inst(Instruction::Call(f.clone(), a_regs)),
                ))),
                // Value::ExternalFun(i) => todo!(),
                // Value::ExternalClosure(i) => todo!(),
                Value::None => unreachable!(),
                _ => todo!(),
            }
        }
        Expr::Lambda(ids, types, body) => {
            let (mut newf, fnid) = ctx.make_new_fn();

            let mut binds = ids
                .iter()
                .map(|name| {
                    let label = name.0.id.clone();
                    let a = Argument(Label(label.clone()), Type::Unknown);
                    let res = (label.clone(), Arc::new(Value::Argument(a)));
                    res
                })
                .collect::<Vec<_>>();
            binds.iter().for_each(|(_, a)| {
                if let Value::Argument(a) = a.as_ref() {
                    newf.args.push(a.clone());
                }
            });
            ctx.valenv.extend();
            ctx.valenv.add_bind(&mut binds);

            let _res = eval_expr(&body, ctx);
            Ok(Arc::new(Value::Function(fnid)))
        }
        Expr::Feed(_, _) => todo!(),
        Expr::Let(id, body, then) => {
            ctx.valenv.extend();
            let bodyv = eval_expr(body, ctx)?;
            ctx.valenv.add_bind(&mut vec![(id.id.clone(), bodyv)]);
            if let Some(then_e) = then {
                eval_expr(then_e, ctx)
            } else {
                Ok(Arc::new(Value::None))
            }
        }
        Expr::LetRec(id, body, then) => {
            ctx.valenv.extend();
            let bind = (id.id.clone(), Arc::new(Value::FixPoint));
            ctx.fn_label = Some(id.id.clone());
            ctx.valenv.add_bind(&mut vec![bind]);
            eval_expr(body, ctx);
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
