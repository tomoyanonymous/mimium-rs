use crate::{
    ast,
    compiler::Error as CompileError,
    compiler::{typing::infer_type, ErrorKind},
    integer, numeric,
    runtime::builtin_fn,
    string_t,
    types::{Id, PType, Type, TypedId},
    unit,
    utils::{
        environment::Environment,
        metadata::{Span, WithMeta},
    },
};

#[derive(Debug, Clone, Copy)]
pub enum PValue {
    Unit,
    Numeric(f64),
    Integer(i64),
}

#[derive(Debug, Clone)]
pub enum Value {
    Primitive(PValue),
    String(String),
    Tuple(Vec<Value>),
    //Function value holds return type
    Function(Vec<TypedId>, Box<WithMeta<ast::Expr>>, Type),
    FixPoint(TypedId, Box<WithMeta<ast::Expr>>),
    External(String),
}
impl PValue {
    pub fn get_type(&self) -> Type {
        match self {
            PValue::Unit => unit!(),
            PValue::Numeric(_) => numeric!(),
            PValue::Integer(_) => integer!(),
        }
    }
}
impl Value {
    pub fn get_type(&self) -> Type {
        match self {
            Value::Primitive(p) => p.get_type(),
            Value::String(_) => string_t!(),
            Value::Tuple(v) => Type::Tuple(v.iter().map(|t| t.get_type()).collect()),
            Value::Function(a, _e, r_type) => Type::Function(
                a.iter()
                    .map(|TypedId { ty, id: _ }| ty.clone().expect("function argument untyped"))
                    .collect(),
                r_type.clone().into(),
                None,
            ),
            Value::FixPoint(TypedId { ty, id: _ }, _) => ty.clone().unwrap_or(unit!()),
            //todo!
            Value::External(_id) => Type::Unknown,
        }
    }
}

pub struct Context {
    pub env: Environment<Value>,
    ///
    pub history: (u64, Vec<PValue>),
}
impl Context {
    pub fn new() -> Self {
        Self {
            env: Environment::new(),
            history: (0, vec![]),
        }
    }
}

const EXTERN_ENV: [&str; 25] = [
    "add", "sub", "mult", "div", "mod", "eq", "ne", "le", "lt", "ge", "gt", "atan2", "sin", "cos",
    "not", "round", "floor", "ceil", "atan", "sqrt", "abs", "min", "max", "pow", "log",
];

fn lookup_extern_env(name: &str) -> Option<&str> {
    let filtered = EXTERN_ENV
        .into_iter()
        .filter(|n| *n == name)
        .collect::<Vec<_>>();
    filtered.get(0).map(|s| *s)
}

fn eval_literal(e: ast::Literal) -> Value {
    match e {
        ast::Literal::String(s) => Value::String(s),
        ast::Literal::Int(i) => Value::Primitive(PValue::Integer(i)),
        ast::Literal::Float(f) => Value::Primitive(PValue::Numeric(f.parse().unwrap())),
        ast::Literal::SelfLit => {
            panic!("self literal should not be shown in evaluation stage.")
        }
        ast::Literal::Now => {
            panic!("now literal should not be shown in evaluation stage.")
        }
    }
}

fn eval_condition<'a>(
    e: Box<WithMeta<ast::Expr>>,
    ctx: &mut Context,
) -> Result<bool, CompileError> {
    let c_v = eval_ast(e.clone(), ctx)?;

    match c_v.clone() {
        Value::Primitive(PValue::Numeric(f)) => Ok(f > 0.0),
        Value::Primitive(PValue::Integer(i)) => Ok(i > 0),
        _ => {
            let WithMeta(_, span) = *e;
            Err(CompileError(
                ErrorKind::TypeMismatch(numeric!(), c_v.get_type()),
                span,
            ))
        }
    }
}

fn getcell<'a,'ctx:'a>(ctx: &'ctx mut Context) -> &'a mut PValue {
    let history_count = &mut ctx.history.0;
    let history = &mut ctx.history.1;
    let index = *history_count as usize;
    if  history.len() < index{
        history.get_mut(index).unwrap()
    }else{
        history.push(PValue::Numeric(0.0));
        history.get_mut(index).unwrap()
    }
}

fn eval_with_new_env(
    e_meta: Box<WithMeta<ast::Expr>>,
    ctx: &mut Context,
    names: &mut Vec<(String, Value)>,
) -> Result<Value, CompileError> {
    ctx.env.extend();
    ctx.env.add_bind(names);
    let res = eval_ast(e_meta, ctx);
    ctx.env.to_outer();
    res
}

pub fn eval_extern(n: &String, argv: &Vec<Value>, span: Span) -> Result<Value, CompileError> {
    use builtin_fn::{eval_float1, eval_float2, eval_int1, eval_int2};
    let pv = match argv.len() {
        1 => {
            let v = argv.get(0).unwrap();
            match v {
                Value::Primitive(PValue::Numeric(a1)) => {
                    Ok(PValue::Numeric(eval_float1(&n, *a1).unwrap()))
                }
                Value::Primitive(PValue::Integer(a1)) => {
                    Ok(PValue::Integer(eval_int1(&n, *a1).unwrap()))
                }
                _ => Err(CompileError(
                    ErrorKind::TypeMismatch(numeric!(), v.get_type()),
                    span,
                )),
            }
        }
        2 => {
            let v1 = argv.get(0).unwrap();
            let v2 = argv.get(1).unwrap();
            match (v1, v2) {
                (Value::Primitive(PValue::Numeric(a1)), Value::Primitive(PValue::Numeric(a2))) => {
                    Ok(PValue::Numeric(eval_float2(&n, *a1, *a2).unwrap()))
                }
                (Value::Primitive(PValue::Integer(a1)), Value::Primitive(PValue::Integer(a2))) => {
                    Ok(PValue::Integer(eval_int2(&n, *a1, *a2).unwrap()))
                }
                _ => Err(CompileError(
                    ErrorKind::TypeMismatch(v1.get_type(), v2.get_type()),
                    span,
                )),
            }
        }
        _ => Err(CompileError(ErrorKind::NotApplicable, span)),
    }?;
    Ok(Value::Primitive(pv))
}

pub fn eval_ast(
    e_meta: Box<WithMeta<ast::Expr>>,
    ctx: &mut Context,
) -> Result<Value, CompileError> {
    let env = &mut ctx.env;
    let WithMeta(e, span) = *e_meta;
    match e {
        ast::Expr::Literal(l) => Ok(eval_literal(l)),
        ast::Expr::Var(v, _time) => env
            .lookup(&v)
            .map(|v| v.clone())
            .or(lookup_extern_env(&v).map(|n| Value::External(n.to_string())))
            .ok_or(CompileError(ErrorKind::VariableNotFound(v), span.clone())),
        ast::Expr::Block(b) => b.map_or(Ok(Value::Primitive(PValue::Unit)), |body| {
            eval_ast(body, ctx)
        }),
        ast::Expr::Tuple(v) => {
            let res = v
                .iter()
                //todo: collect multiple errors
                .map(|e| eval_ast(e.clone().into(), ctx).unwrap())
                .collect();
            Ok(Value::Tuple(res))
        }
        ast::Expr::Proj(t, i) => {
            let v = eval_ast(t.clone(), ctx)?;
            let WithMeta(_, span) = *t;
            match v {
                Value::Tuple(t) => t.get(i as usize).map(|v| v.clone()).ok_or(CompileError(
                    ErrorKind::IndexOutOfRange(t.len() as u16, i as u16),
                    span,
                )),
                _ => Err(CompileError(
                    ErrorKind::IndexForNonTuple(v.get_type()),
                    span,
                )),
            }
        }
        ast::Expr::Apply(f, args) => {
            let argv: Vec<_> = args
                .iter()
                .map(|e| eval_ast(e.clone().into(), ctx))
                .try_collect()?;
            let func = eval_ast(f.clone(), ctx)?;
            let res = match func.clone() {
                Value::Function(params, b, _r_t) => {
                    let mut argvec: Vec<_> = argv
                        .iter()
                        .zip(params.iter())
                        .map(|(v, TypedId { ty: _, id })| (id.clone(), v.clone()))
                        .collect();
                    eval_with_new_env(b, ctx, &mut argvec)
                }
                Value::FixPoint(TypedId { id, ty: _ }, e) => {
                    eval_with_new_env(e, ctx, &mut vec![(id, func)])
                }
                Value::External(n) => {
                    //todo: appropreate error type
                    eval_extern(&n, &argv, span)
                }
                _ => {
                    let WithMeta(_, span) = *f;

                    Err(CompileError(ErrorKind::NotApplicable, span))
                }
            };
            res
        }
        ast::Expr::Lambda(a, e) => Ok(Value::Function(
            a.iter().map(|WithMeta(tid, _s)| tid.clone()).collect(),
            e.clone(),
            Type::Primitive(PType::Unit), //todo! infer type
        )),
        ast::Expr::Feed(a, e) => {
            let cellv = *getcell(ctx);
            let res = eval_with_new_env(e, ctx, &mut vec![(a, Value::Primitive(cellv))])?;
            let pres = match res {
                Value::Primitive(p) => Ok(p),
                _ => Err(CompileError(ErrorKind::NonPrimitiveInFeed, span)),
            }?;
            *getcell(ctx) = pres;
            ctx.history.0 += 1;
            Ok(res)
        }
        ast::Expr::Let(TypedId { id, ty: _t }, e, then) => {
            let e_v = eval_ast(e, ctx)?;
            match then {
                Some(t) => eval_with_new_env(t, ctx, &mut vec![(id, e_v)]),
                None => Ok(Value::Primitive(PValue::Unit)),
            }
        }
        ast::Expr::LetRec(tid, e, then) => {
            let res_rec = eval_with_new_env(
                e.clone(),
                ctx,
                &mut vec![(tid.clone().id, Value::FixPoint(tid.clone(), e))],
            )?;
            then.map(|t| eval_with_new_env(t, ctx, &mut vec![(tid.id, res_rec)]))
                .unwrap_or(Ok(Value::Primitive(PValue::Unit)))
        }
        ast::Expr::LetTuple(_, _, _) => todo!(),
        ast::Expr::If(cond, then, o_else) => {
            if eval_condition(cond, ctx)? {
                eval_ast(then, ctx)
            } else {
                o_else
                    .map(|e_else| eval_ast(e_else, ctx))
                    .unwrap_or(Ok(Value::Primitive(PValue::Unit)))
            }
        }
        ast::Expr::Bracket(_) => todo!(),
        ast::Expr::Escape(_) => todo!(),
        ast::Expr::Error => panic!("Some Error happend in previous stages"),
    }
}
