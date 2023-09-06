use crate::{
    ast,
    runtime::builtin_fn,
    types::{Type, TypedId},
    utils::{
        environment::Environment,
        error::ReportableError,
        metadata::{Span, WithMeta},
    },
};

#[derive(Debug, Clone)]
pub enum Value {
    Unit,
    Numeric(f64),
    Integer(i64),
    String(String),
    Tuple(Vec<Value>),
    //Function value holds return type
    Function(Vec<TypedId>, Box<WithMeta<ast::Expr>>, Type),
    FixPoint(TypedId, Box<WithMeta<ast::Expr>>),
    External(String),
}

impl Value {
    pub fn get_type(&self) -> Type {
        match self {
            Value::Unit => Type::Unit,
            Value::Numeric(_) => Type::Numeric,
            Value::Integer(_) => Type::Int,
            Value::String(_) => Type::String,
            Value::Tuple(v) => Type::Tuple(v.iter().map(|t| t.get_type()).collect()),
            Value::Function(a, _e, r_type) => Type::Function(
                a.iter()
                    .map(|TypedId { ty, id: _ }| ty.clone().expect("function argument untyped"))
                    .collect(),
                r_type.clone().into(),
                None,
            ),
            Value::FixPoint(TypedId { ty, id: _ }, _) => ty.clone().unwrap_or(Type::Unit),
            //todo!
            Value::External(_id) => Type::Unknown,
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

#[derive(Debug)]
pub enum Error {
    VariableNotFound(Span),
    // Expected/Actual
    TypeMisMatch(Type, Type, Span),
    // Actual Type
    TypeNotTuple(Type, Span),
    // Range, Accessed Index
    TupleIndexOutOfRange(usize, usize, Span),
    NotApplicable(Span),
}
impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::VariableNotFound(_) => {
                write!(f, "Variable Not Found.")
            }
            Error::TypeMisMatch(expect, actual, _) => {
                write!(
                    f,
                    "Type Mismatch, expected {}, but the actual was {}.",
                    expect.to_string(),
                    actual.to_string()
                )
            }
            Error::TypeNotTuple(t, _) => {
                write!(f, "Index access for non tuple-type {}.", t.to_string())
            }
            Error::TupleIndexOutOfRange(r, a, _) => {
                write!(
                    f,
                    "Tuple index out of range, number of elements are {} but accessed with {}.",
                    r, a
                )
            }
            Error::NotApplicable(_) => {
                write!(f, "Application to non-function type value.")
            }
        }
    }
}
impl std::error::Error for Error {}

impl ReportableError for Error {
    fn get_span(&self) -> std::ops::Range<usize> {
        match self {
            Error::VariableNotFound(s) => s.clone(),
            Error::TypeMisMatch(_, _, s) => s.clone(),
            Error::TypeNotTuple(_, s) => s.clone(),
            Error::TupleIndexOutOfRange(_, _, s) => s.clone(),
            Error::NotApplicable(s) => s.clone(),
        }
    }
}

fn eval_literal(e: ast::Literal) -> Value {
    match e {
        ast::Literal::String(s) => Value::String(s),
        ast::Literal::Int(i) => Value::Integer(i),
        ast::Literal::Float(f) => Value::Numeric(f.parse().unwrap()),
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
    env: &mut Environment<Value>,
) -> Result<bool, Error> {
    let c_v = eval_ast(e.clone(), env)?;

    match c_v.clone() {
        Value::Numeric(f) => Ok(f > 0.0),
        Value::Integer(i) => Ok(i > 0),
        _ => {
            let WithMeta(_, span) = *e;
            Err(Error::TypeMisMatch(Type::Numeric, c_v.get_type(), span))
        }
    }
}

fn eval_with_new_env(
    e_meta: Box<WithMeta<ast::Expr>>,
    env: &mut Environment<Value>,
    names: &mut Vec<(String, Value)>,
) -> Result<Value, Error> {
    // let len_origin = env.len();
    env.extend();
    env.add_bind(names);
    let res = eval_ast(e_meta, env);
    env.to_outer();
    res
}

pub fn eval_extern(n: &String, argv: &Vec<Value>, span: Span) -> Result<Value, Error> {
    use builtin_fn::{eval_float1, eval_float2, eval_int1, eval_int2};
    match argv.len() {
        1 => {
            let v = argv.get(0).unwrap();
            match v {
                Value::Numeric(a1) => Ok(Value::Numeric(eval_float1(&n, *a1).unwrap())),
                Value::Integer(a1) => Ok(Value::Integer(eval_int1(&n, *a1).unwrap())),
                _ => Err(Error::TypeMisMatch(Type::Numeric, v.get_type(), span)),
            }
        }
        2 => {
            let v1 = argv.get(0).unwrap();
            let v2 = argv.get(1).unwrap();
            match (v1, v2) {
                (Value::Numeric(a1), Value::Numeric(a2)) => {
                    Ok(Value::Numeric(eval_float2(&n, *a1, *a2).unwrap()))
                }
                (Value::Integer(a1), Value::Integer(a2)) => {
                    Ok(Value::Integer(eval_int2(&n, *a1, *a2).unwrap()))
                }
                _ => Err(Error::TypeMisMatch(v1.get_type(), v2.get_type(), span)),
            }
        }
        _ => Err(Error::NotApplicable(span)),
    }
}

pub fn eval_ast(
    e_meta: Box<WithMeta<ast::Expr>>,
    env: &mut Environment<Value>,
) -> Result<Value, Error> {
    let WithMeta(e, span) = *e_meta;
    match e {
        ast::Expr::Literal(l) => Ok(eval_literal(l)),
        ast::Expr::Var(v, _time) => env
            .lookup(&v)
            .map(|v| v.clone())
            .or(lookup_extern_env(&v).map(|n| Value::External(n.to_string())))
            .ok_or(Error::VariableNotFound(span.clone())),
        ast::Expr::Block(b) => b.map_or(Ok(Value::Unit), |body| eval_ast(body, env)),
        ast::Expr::Tuple(v) => {
            let res = v
                .iter()
                //todo: collect multiple errors
                .map(|e| eval_ast(e.clone().into(), env).unwrap())
                .collect();
            Ok(Value::Tuple(res))
        }
        ast::Expr::Proj(t, i) => {
            let v = eval_ast(t.clone(), env)?;
            let WithMeta(_, span) = *t;
            match v {
                Value::Tuple(t) => t
                    .get(i as usize)
                    .map(|v| v.clone())
                    .ok_or(Error::TupleIndexOutOfRange(t.len(), i as usize, span)),
                _ => Err(Error::TypeNotTuple(v.get_type(), span)),
            }
        }
        ast::Expr::Apply(f, args) => {
            let argv: Vec<_> = args
                .iter()
                .map(|e| eval_ast(e.clone().into(), env))
                .try_collect()?;
            let func = eval_ast(f.clone(), env)?;
            let res = match func.clone() {
                Value::Function(params, b, _r_t) => {
                    let mut argvec: Vec<_> = argv
                        .iter()
                        .zip(params.iter())
                        .map(|(v, TypedId { ty: _, id })| (id.clone(), v.clone()))
                        .collect();
                    eval_with_new_env(b, env, &mut argvec)
                }
                Value::FixPoint(TypedId { id, ty: _ }, e) => {
                    eval_with_new_env(e, env, &mut vec![(id, func)])
                }
                Value::External(n) => {
                    //todo: appropreate error type
                    eval_extern(&n, &argv, span)
                }
                _ => {
                    let WithMeta(_, span) = *f;

                    Err(Error::NotApplicable(span))
                }
            };
            res
        }
        ast::Expr::Lambda(a, e) => Ok(Value::Function(
            a.iter().map(|WithMeta(tid, _s)| tid.clone()).collect(),
            e.clone(),
            Type::Unit, //todo! infer type
        )),
        ast::Expr::Feed(_a, _e) => {
            todo!()
        }
        ast::Expr::Let(TypedId { id, ty: _t }, e, then) => {
            let e_v = eval_ast(e, env)?;
            match then {
                Some(t) => eval_with_new_env(t, env, &mut vec![(id, e_v)]),
                None => Ok(Value::Unit),
            }
        }
        ast::Expr::LetRec(tid, e, then) => {
            let res_rec = eval_with_new_env(
                e.clone(),
                env,
                &mut vec![(tid.clone().id, Value::FixPoint(tid.clone(), e))],
            )?;
            then.map(|t| eval_with_new_env(t, env, &mut vec![(tid.id, res_rec)]))
                .unwrap_or(Ok(Value::Unit))
        }
        ast::Expr::LetTuple(_, _, _) => todo!(),
        ast::Expr::If(cond, then, o_else) => {
            if eval_condition(cond, env)? {
                eval_ast(then, env)
            } else {
                o_else
                    .map(|e_else| eval_ast(e_else, env))
                    .unwrap_or(Ok(Value::Unit))
            }
        }
        ast::Expr::Bracket(_) => todo!(),
        ast::Expr::Escape(_) => todo!(),
        ast::Expr::Error => panic!("Some Error happend in previous stages"),
    }
}
