use std::borrow::Borrow;

use crate::hir::expr::*;
use crate::utils::metadata::WithMeta;

pub struct EvalContext {
    pub stage: u64,
}

pub fn eval_stage_hir(expr: WithMeta<Expr>, ctx: &mut EvalContext) -> WithMeta<Expr> {
    let WithMeta(e, span) = expr.clone();

    match e {
        Expr::Var(var, _time) if ctx.stage == 0 => {
            // let r = var.0.0.borrow().as_ref().unwrap().clone();
           expr.clone()
        }
        Expr::Apply(box WithMeta(Expr::Lambda(params, body), _fspan), callee) if ctx.stage == 0 => {
            params.iter().zip(callee.iter()).for_each(|(p, e)| {
                // let mut myp = p.0.v.borrow_mut();
                // *myp = Some(e.0.clone());
            });
            eval_stage_hir(*body, ctx)
        }
        Expr::Apply(fun, callee) if fun.0.is_value() => {
            let newcallee = callee
                .iter()
                .map(|e| Box::new(eval_stage_hir(**e, ctx)))
                .collect();
            let res = WithMeta(Expr::Apply(fun, newcallee), span.clone());
            eval_stage_hir(res, ctx)
        }
        Expr::Apply(fun, callee) => {
            let res = WithMeta(
                Expr::Apply(Box::new(eval_stage_hir(*fun, ctx)), callee.clone()),
                span.clone(),
            );
            eval_stage_hir(res, ctx)
        }
        Expr::If(box cond, box then, opt_else) => {
            let cond = eval_stage_hir(cond, ctx);
            let then = eval_stage_hir(then, ctx);
            let opt_else = opt_else.map(|e| Box::new(eval_stage_hir(*e, ctx)));
            if ctx.stage == 0 {
                if cond.0.eval_condition() {
                    then
                } else {
                    *opt_else.unwrap()
                }
            } else {
                WithMeta(
                    Expr::If(Box::new(cond), Box::new(then), opt_else),
                    span.clone(),
                )
            }
        }
        Expr::Bracket(b) => {
            ctx.stage += 1;
            eval_stage_hir(*b, ctx)
        }
        Expr::Escape(b) => {
            ctx.stage -= 1;
            eval_stage_hir(*b, ctx)
        }
        Expr::Lambda(params, body) if ctx.stage == 0 => WithMeta(
            Expr::Lambda(params, Box::new(eval_stage_hir(*body, ctx))),
            span.clone(),
        ),
        _ if ctx.stage == 0 && e.is_value() => expr.clone(),
        _ => * Box::new(expr).walk(|x| Box::new(eval_stage_hir(*x, ctx))),
    }
}
