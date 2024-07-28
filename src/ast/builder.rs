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
        WithMeta(Expr::Apply(Box::new($a), $b), 0..0)
    };
}

#[macro_export]
macro_rules! lambda_args {
    ($args:expr) => {
        //expect vec![id]
        $args
            .iter()
            .map(|a| {
                WithMeta(
                    TypedId {
                        ty: None,
                        id: String::from(*a),
                    },
                    0..0,
                )
            })
            .collect::<Vec<_>>()
    };
}

#[macro_export]
macro_rules! lambda {
    ($args:expr,$body:expr) => {
        WithMeta(
            Expr::Lambda(
                $args
                    .iter()
                    .map(|a: &&'static str| {
                        WithMeta(
                            crate::types::TypedId {
                                ty: None,
                                id: String::from(*a),
                            },
                            0..0,
                        )
                    })
                    .collect::<Vec<_>>(),
                None,
                $body.into(),
            ),
            0..0,
        )
    };
}

#[macro_export]
macro_rules! let_ {
    ($id:literal,$body:expr,$then:expr) => {
        WithMeta(
            Expr::Let(
                crate::types::TypedId {
                    ty: None,
                    id: $id.to_string(),
                },
                Box::new($body),
                Some(Box::new($then)),
            ),
            0..0,
        )
    };
    ($id:literal,$body:expr) => {
        WithMeta(
            Expr::Let(
                crate::types::TypedId {
                    ty: None,
                    id: $id.to_string(),
                },
                Box::new($body),
                None,
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
macro_rules! assign {
    ($lhs:literal,$rhs:expr) => {
        WithMeta(Expr::Assign(String::from($lhs), Box::new($rhs)), 0..0)
    };
}
#[macro_export]
macro_rules! then {
    ($first:expr,$second:expr) => {
        WithMeta(Expr::Then(Box::new($first), Box::new($second)), 0..0)
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
