use super::typing;
use crate::runtime::vm::bytecode::Instruction;
use crate::runtime::vm::{FuncProto, RawVal};
use crate::utils::environment::Environment;
use crate::utils::metadata::{Span, WithMeta};

use crate::ast::{Expr, Literal};
use crate::runtime::{vm, vm::bytecode::*};
// pub mod closure_convert;
// pub mod feedconvert;
// pub mod hir_solve_stage;
use vm::bytecode::Reg;
#[derive(Clone, Debug)]
enum Val {
    Register(Reg),
    Function(u8),
    ExternalFun(u8),
    ExternalClosure(u8),
    None
}
pub struct Context {
    pub typeenv: typing::InferContext,
    valenv: Environment<Val>,
    pub program: vm::Program,
    current_fn_idx: usize,
    pub stack_pos: usize,
}

impl Context {
    pub fn get_current_fnproto(&mut self) -> &mut FuncProto {
        self.program
            .global_fn_table
            .get_mut(self.current_fn_idx)
            .expect("invalid func_proto index")
    }
}

pub enum CompileError {
    TypeMismatch,
    TooManyConstants,
    VariableNotFound,
}

pub fn compile(src: WithMeta<Expr>) -> Result<vm::Program, CompileError> {
    todo!();
    // let mut ctx = 
    // Ok(ctx.program)
}
fn load_new_rawv(rawv: RawVal, func: &mut FuncProto) -> Result<u8, CompileError> {
    let idx = func.constants.binary_search(&rawv).unwrap_or_else(|_| {
        func.constants.push(rawv);
        if func.constants.is_empty() {
            panic!("failed to push constant to funcproto")
        }
        func.constants.len() - 1
    });
    if idx > u8::MAX as usize {
        Err(CompileError::TooManyConstants)
    } else {
        Ok(idx as u8)
    }
}
fn load_new_float(v: f64, func: &mut FuncProto) -> Result<u8, CompileError> {
    let rawv: RawVal = unsafe { std::mem::transmute(v) };
    load_new_rawv(rawv, func)
}
fn load_new_int(v: i64, func: &mut FuncProto) -> Result<u8, CompileError> {
    let rawv: RawVal = unsafe { std::mem::transmute(v) };
    load_new_rawv(rawv, func)
}

fn eval_literal(lit: &Literal, span: &Span, ctx: &mut Context) -> Result<Val, CompileError> {

    ctx.stack_pos += 1;
    let stack_pos = ctx.stack_pos;
    let mut func = ctx.get_current_fnproto();
    match lit {
        Literal::String(_) => todo!(),
        Literal::Int(i) => {
            let const_pos = load_new_int(*i, func)?;
            func.bytecodes
                .push(Instruction::MoveConst(stack_pos as Reg, const_pos));
        }
        Literal::Float(f) => {
            let fv: f64 = f.parse().expect("invalid float format");
            let const_pos = load_new_float(fv, func)?;
            func.bytecodes
                .push(Instruction::MoveConst(stack_pos as Reg, const_pos));
        }
        Literal::SelfLit => unreachable!(),
        Literal::Now => todo!(),
    };
    Ok(Val::Register(stack_pos as Reg))
}

fn eval_expr(e_meta: &WithMeta<Expr>, ctx: &mut Context) -> Result<Val, CompileError> {
    let WithMeta(e, span) = e_meta;
    match e {
        Expr::Literal(lit) => eval_literal(lit, span, ctx),
        Expr::Var(v, _time) => {
            if let Some(v) = ctx.valenv.lookup(v) {
                Ok(*v)
            } else {
                Err(CompileError::VariableNotFound)
            }
        }
        Expr::Block(b) => {
            if let Some(block) = b {
                eval_expr(block, ctx)
            } else {
                //todo?
                Ok(Val::None)
            }
        }
        Expr::Tuple(_) => todo!(),
        Expr::Proj(_, _) => todo!(),
        Expr::Apply(func, args) => {
            let a_regs = args
                .iter()
                .map(|a_meta| eval_expr(a_meta, ctx))
                .try_collect::<Vec<_>>()?;
            let f = eval_expr(func, ctx)?;
            // let inst =
            match f{
                Val::Register(p) => {
                // 
                },
                Val::Function(i) => {

                },
                Val::ExternalFun(i) => ,
                Val::ExternalClosure(i) => todo!(),
                Val::None => unreachable!(),
            }

            ctx.get_current_fnproto().bytecodes.push(Instruc)
        }
        Expr::Lambda(ids, types, body) => {
            let newf = FuncProto::new(ids.len());
            let res = eval_expr(&body, ctx);
            ctx.program.global_fn_table.push(newf);
            ctx.current_fn_idx = ctx.program.global_fn_table.len() - 1;
            if !ctx.valenv.is_global() {}
        }
        Expr::Feed(_, _) => todo!(),
        Expr::Let(_, _, _) => todo!(),
        Expr::LetRec(_, _, _) => todo!(),
        Expr::LetTuple(_, _, _) => todo!(),
        Expr::If(_, _, _) => todo!(),
        Expr::Bracket(_) => todo!(),
        Expr::Escape(_) => todo!(),
        Expr::Error => todo!(),
    }
}
