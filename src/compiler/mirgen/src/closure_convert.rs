use hir::expr::Expr;
use mir::*;
use mmmtype::{Id, Type};
use std::cell::RefCell;
use std::rc::Rc;
use std::{collections::HashSet, convert};
use utils::{environment::Environment, metadata::WithMeta};

pub fn get_fv_list(hir: hir::expr::Expr) -> HashSet<Id> {
    match hir {
        Expr::Literal(_) => HashSet::new(),
        Expr::Let(id, body, then) => {
            let body_fv = get_fv_list(body.0);
            let mut then_fv = get_fv_list(then.unwrap().0);
            then_fv.remove(&id.0.id);
            body_fv.union(&then_fv).cloned().collect()
        }
        Expr::LetRec(id, body, then) => {
            let mut body_fv = get_fv_list(body.0);
            body_fv.remove(&id.0.id);
            let mut then_fv = get_fv_list(then.unwrap().0);
            then_fv.remove(&id.0.id);
            body_fv.union(&then_fv).cloned().collect()
        }
        Expr::LetTuple(ids, body, then) => {
            let ids2 = ids.iter().map(|x| x.0.id.clone()).collect::<HashSet<_>>();
            let body_fv = get_fv_list(body.0);
            let then_fv_proto = get_fv_list(then.unwrap().0);
            let then_fv = then_fv_proto.difference(&ids2).cloned().collect();
            body_fv.union(&then_fv).cloned().collect()
        }
        Expr::Var(id, _) => HashSet::from([id.0.id.clone()]),
        Expr::Block(b) => b.map_or(HashSet::new(), |e| get_fv_list(e.0)),
        Expr::Tuple(_) => todo!(),
        Expr::Proj(_, _) => todo!(),
        Expr::Apply(callee, args) => {
            let argset: HashSet<Id> = args.iter().fold(HashSet::new(), |acc, x| {
                acc.union(&get_fv_list(x.0.clone())).cloned().collect()
            });
            get_fv_list(callee.0).union(&argset).cloned().collect()
        }
        Expr::Lambda(args, body) => {
            let body_fv = get_fv_list(body.0);
            let ids = args.iter().map(|x| x.0.id.clone()).collect::<HashSet<_>>();
            body_fv.difference(&ids).cloned().collect()
        }

        Expr::If(cond, then, else_opt) => {
            let cfv = get_fv_list(cond.0);
            let tfv = get_fv_list(then.0).union(&cfv).cloned().collect();
            else_opt
                .map_or(HashSet::new(), |e| get_fv_list(e.0))
                .union(&tfv)
                .cloned()
                .collect()
        }
        Expr::MakeClosure(_, _) => unreachable!(),
        Expr::ApplyDir(_, _) => unreachable!(),

        Expr::Feed(_, _) | Expr::Bracket(_) | Expr::Escape(_) => unreachable!(),
        Expr::Error => todo!(),
    }
}

pub fn convert_closure(
    hir: hir::expr::Expr,
    typeenv: &Environment<Type>,
    known_funs: &Vec<Id>,
    toplevel: &mut Vec<(Id, Type)>,
) -> hir::expr::Expr {
    match hir {
        Expr::Literal(_) => hir,
        Expr::Var(_, _) => hir,
        Expr::Block(b_opt) => b_opt.map_or(Expr::Error, |b| {
            convert_closure(b.0, typeenv, known_funs, toplevel)
        }),
        Expr::Tuple(_) => todo!(),
        Expr::Proj(_, _) => todo!(),
        Expr::Apply(box WithMeta(Expr::Var(id, _), s), args) if known_funs.contains(&id.0.id) => {
            let nargs = args
                .iter()
                .map(|WithMeta(a, span)| {
                    WithMeta(
                        convert_closure(a.clone(), typeenv, known_funs, toplevel),
                        span.clone(),
                    )
                })
                .collect();
            Expr::ApplyDir(id.clone(), nargs)
        }
        Expr::Apply(callee, args) => {
            let callee_ = WithMeta(
                convert_closure(callee.0, typeenv, known_funs, toplevel),
                callee.1,
            );
            let nargs = args
                .iter()
                .map(|WithMeta(a, span)| {
                    WithMeta(
                        convert_closure(a.clone(), typeenv, known_funs, toplevel),
                        span.clone(),
                    )
                })
                .collect();
            Expr::Apply(Box::new(callee_), nargs)
        }
        Expr::ApplyDir(_, _) => unreachable!(),
        Expr::MakeClosure(_, _) => unreachable!(),
        Expr::Feed(_, _) => todo!(),
        Expr::Let(id, body, then) => Expr::Let(
            id,
            Box::new(WithMeta(
                convert_closure(body.0, typeenv, known_funs, toplevel),
                body.1,
            )),
            then.map(|t| {
                Box::new(WithMeta(
                    convert_closure(t.0, typeenv, known_funs, toplevel),
                    t.1,
                ))
            }),
        ),
        Expr::LetRec(id, box WithMeta(Expr::Lambda(args, body), span), then) => {
            //first, assume that the function does not have free variables.
            let mut new_known_funs = known_funs.clone();
            new_known_funs.push(id.0.id.clone());
            let body_converted =
                convert_closure(body.0.clone(), typeenv, &new_known_funs, toplevel);
            let argslist = args.iter().map(|a| a.0.id.clone()).collect::<HashSet<_>>();
            let tmp = get_fv_list(body_converted.clone());
            let mut fvlist = tmp.difference(&argslist).collect::<HashSet<_>>();
            fvlist.remove(&id.0.id);
            let (body_, known_funs) = if fvlist.is_empty() {
                (body_converted, new_known_funs)
            } else {
                dbg!("free variable found");
                let body_converted = convert_closure(body.0, typeenv, known_funs, toplevel);
                (body_converted, known_funs.clone())
            };
            let fvthen = argslist.clone();
            fvthen.clone().insert(id.0.id.clone());
            let fvthen = get_fv_list(body_)
                .difference(&fvthen)
                .map(|id| {
                    Rc::new(WithMeta(
                        hir::expr::Value {
                            id: id.clone(),
                            v: RefCell::new(None),
                        },
                        0..0,
                    ))
                })
                .collect::<Vec<_>>();
            toplevel.push((
                id.0.id.clone(),
                typeenv.get_bound_value(id.0.id.clone()).unwrap().clone(),
            ));

            let then_converted = then.map_or(Expr::Error, |t| {
                convert_closure(t.0, typeenv, &known_funs, toplevel)
            });
            if get_fv_list(then_converted.clone()).contains(&id.0.id) {
                Expr::MakeClosure(id.clone(), fvthen)
            } else {
                then_converted
            }
        }
        Expr::LetRec(id, body, then) => unreachable!(),
        Expr::Lambda(id, _) => unreachable!(),
        Expr::LetTuple(_, _, _) => todo!(),
        Expr::If(_, _, _) => todo!(),
        Expr::Bracket(_) => todo!(),
        Expr::Escape(_) => todo!(),
        Expr::Error => todo!(),
    }
}
