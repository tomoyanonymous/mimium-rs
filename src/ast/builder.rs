pub use crate::ast::{Expr, Literal};

#[macro_export]
macro_rules! dummy_span {
    () => {
        0..0
    };
}

#[macro_export]
macro_rules! number {
    ($n:literal) => {
        WithMeta(Expr::Literal(Literal::Float($n.to_string())), 0..0)
    };
}

#[macro_export]
macro_rules! string {
    ($n:expr) => {
        WithMeta(Expr::Literal(Literal::String($n)), 0..0)
    };
}
#[macro_export]
macro_rules! var {
    ($n:literal) => {
        WithMeta(Expr::Var($n.to_string(), None), 0..0)
    };
}

#[macro_export]
macro_rules! app {
    ($a:expr,$b:expr) => {
        WithMeta(Expr::Apply(Box::new($a), vec![$b]), 0..0)
    };
}

#[macro_export]
macro_rules! lambda {
    ($arg:expr,$body:expr) => {
        WithMeta(Expr::Lambda($arg, None, $body.into()), 0..0)
    };
}

#[macro_export]
macro_rules! let_ {
    ($id:literal,$body:expr,$then:expr) => {
        WithMeta(
            Expr::Let(
                TypedId {
                    ty: None,
                    id: $id.to_string(),
                },
                Box::new($body),
                $then,
            ),
            0..0,
        )
    };
}

#[macro_export]
macro_rules! letrec {
    ($id:literal,$body:expr,$then:expr) => {
        WithMeta(
            Expr::LetRec(
                TypedId {
                    ty: None,
                    id: $id.to_string(),
                },
                Box::new($body),
                $then,
            ),
            0..0,
        )
    };
}

#[macro_export]
macro_rules! ifexpr {
    ($cond:expr,$then:expr,$else_:expr) => {
        WithMeta(
            Expr::If($cond.into(), $then.into(), Some($else_.into())),
            0..0,
        )
    };
}
