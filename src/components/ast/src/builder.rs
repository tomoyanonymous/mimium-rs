// use super::expr::*;

#[macro_export]
macro_rules! dummy_span {
    () => {
        0..0
    };
}

#[macro_export]
macro_rules! number {
    ($n:expr) => {
        WithMeta(Expr::Literal(Literal::Float($n)),dummy_span!)
    };
}

#[macro_export]
macro_rules! string {
    ($n:expr) => {
        WithMeta(Expr::Literal(Literal::String($n)),dummy_span!)
    };
}

#[macro_export]
macro_rules! app {
    ($a:expr,$b:expr) => {
        WithMeta(Expr::App(Box::new($a),Box::new($b)),dummy_span!)
    };
}