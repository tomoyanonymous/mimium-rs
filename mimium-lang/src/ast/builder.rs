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
        WithMeta(
            Expr::Var($crate::ast::builder::str_to_symbol($n), None),
            0..0,
        )
    };
}

#[macro_export]
macro_rules! app {
    ($a:expr,$b:expr) => {
        WithMeta(Expr::Apply($a.0.into_id_without_span(), $b), 0..0)
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
        WithMeta(
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
                WithMeta(
                    $crate::pattern::TypedPattern {
                        ty: None,
                        pat: $crate::pattern::Pattern::Single($crate::ast::builder::str_to_symbol(
                            $id,
                        )),
                    },
                    0..0,
                ),
                $body,
                Some($then),
            ),
            0..0,
        )
    };
    ($id:literal,$body:expr) => {
        WithMeta(
            Expr::Let(
                WithMeta(
                    $crate::pattern::TypedPattern {
                        ty: None,
                        pat: $crate::pattern::Pattern::Single($crate::ast::builder::str_to_symbol(
                            $id,
                        )),
                    },
                    0..0,
                ),
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
                    id: $crate::ast::builder::str_to_symbol($id),
                },
                $body,
                $then,
            ),
            0..0,
        )
    };
}

#[macro_export]
macro_rules! assign {
    ($lhs:literal,$rhs:expr) => {
        WithMeta(
            Expr::Assign($crate::ast::builder::str_to_symbol($lhs), Box::new($rhs)),
            0..0,
        )
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
