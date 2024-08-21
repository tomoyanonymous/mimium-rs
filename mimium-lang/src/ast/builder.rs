pub use crate::ast::{Expr, Literal};

use super::Symbol;

pub fn str_to_symbol<T: ToString>(x: T) -> Symbol {
    use crate::ast::ToSymbol;
    x.to_string().to_symbol()
}

#[macro_export]
macro_rules! dummy_span {
    () => {
        0..0
    };
}

#[macro_export]
macro_rules! number {
    ($n:literal) => {
        Expr::Literal(Literal::Float($n.to_string())).into_id(0..0)
    };
}

#[macro_export]
macro_rules! string {
    ($n:expr) => {
        Expr::Literal(Literal::String($n)).into_id(0..0)
    };
}
#[macro_export]
macro_rules! var {
    ($n:literal) => {
        Expr::Var($crate::ast::builder::str_to_symbol($n), None).into_id(0..0)
    };
}

#[macro_export]
macro_rules! app {
    ($a:expr,$b:expr) => {
        Expr::Apply($a, $b).into_id(0..0)
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
                        id: $crate::ast::builder::str_to_symbol(a),
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
        Expr::Lambda(
            $args
                .iter()
                .map(|a: &&'static str| {
                    WithMeta(
                        $crate::pattern::TypedId {
                            ty: None,
                            id: $crate::ast::builder::str_to_symbol(a),
                        },
                        0..0,
                    )
                })
                .collect::<Vec<_>>(),
            None,
            $body.into_id(),
        )
        .into_id(0..0)
    };
}

#[macro_export]
macro_rules! let_ {
    ($id:literal,$body:expr,$then:expr) => {
        Expr::Let(
            WithMeta(
                $crate::pattern::TypedPattern {
                    ty: None,
                    pat: $crate::pattern::Pattern::Single($crate::ast::builder::str_to_symbol($id)),
                },
                0..0,
            ),
            $body,
            Some($then),
        )
        .into_id(0..0)
    };
    ($id:literal,$body:expr) => {
        Expr::Let(
            WithMeta(
                $crate::pattern::TypedPattern {
                    ty: None,
                    pat: $crate::pattern::Pattern::Single($crate::ast::builder::str_to_symbol($id)),
                },
                0..0,
            ),
            Box::new($body),
            None,
        )
        .into_id(0..0)
    };
}

#[macro_export]
macro_rules! letrec {
    ($id:literal,$body:expr,$then:expr) => {
        Expr::LetRec(
            TypedId {
                ty: None,
                id: $crate::ast::builder::str_to_symbol($id),
            },
            $body,
            $then,
        )
        .into_id(0..0)
    };
}

#[macro_export]
macro_rules! assign {
    ($lhs:literal,$rhs:expr) => {
        Expr::Assign($crate::ast::builder::str_to_symbol($lhs), Box::new($rhs)).into_id(0..0)
    };
}
#[macro_export]
macro_rules! then {
    ($first:expr,$second:expr) => {
        Expr::Then(Box::new($first), Box::new($second)).into_id(0..0)
    };
}

#[macro_export]
macro_rules! ifexpr {
    ($cond:expr,$then:expr,$else_:expr) => {
        WithMeta(Expr::If($cond, $then, Some($else_)), 0..0)
    };
}
