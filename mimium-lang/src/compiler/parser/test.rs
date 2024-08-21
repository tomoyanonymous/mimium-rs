use super::*;
use crate::pattern::TypedId;
use crate::utils;
use std::path::PathBuf;

macro_rules! test_string {
    ($src:literal, $ans:expr) => {
        let srcstr = $src.to_string();
        match parse(&srcstr) {
            Ok(ast) => {
                assert_eq!(ast, $ans);
            }
            Err(errs) => {
                utils::error::report(&srcstr, PathBuf::new(), &errs);
                panic!();
            }
        }
    };
}

#[test]
fn test_let() {
    let ans = WithMeta(
        Expr::Let(
            WithMeta(
                TypedPattern {
                    pat: Pattern::Single("goge".to_symbol()),
                    ty: None,
                },
                4..8,
            ),
            Box::new(WithMeta(Expr::Literal(Literal::Int(36)), 11..13)),
            Some(Box::new(WithMeta(
                Expr::Var("goge".to_symbol(), None),
                15..19,
            ))),
        ),
        0..19,
    );
    test_string!("let goge = 36\n goge", ans);
}
#[test]
fn test_lettuple() {
    let ans = WithMeta(
        Expr::Let(
            WithMeta(
                TypedPattern {
                    pat: Pattern::Tuple(vec![
                        Pattern::Single("a".to_symbol()),
                        Pattern::Single("b".to_symbol()),
                    ]),
                    ty: None,
                },
                4..9,
            ),
            Box::new(WithMeta(
                Expr::Tuple(vec![
                    WithMeta(Expr::Literal(Literal::Int(36)), 13..15),
                    WithMeta(Expr::Literal(Literal::Int(89)), 16..18),
                ]),
                12..19,
            )),
            Some(Box::new(WithMeta(
                Expr::Var("hoge".to_symbol(), None),
                21..25,
            ))),
        ),
        0..25,
    );
    test_string!("let (a,b) = (36,89)\n hoge", ans);
}
#[test]
fn test_if() {
    let ans = WithMeta(
        Expr::If(
            WithMeta(Expr::Literal(Literal::Int(100)), 4..7).into(),
            WithMeta(Expr::Var("hoge".to_symbol(), None).into(), 9..13).into(),
            Some(WithMeta(Expr::Var("fuga".to_symbol(), None).into(), 19..23).into()),
        ),
        0..23,
    );
    test_string!("if (100) hoge else fuga", ans);
}
#[test]
fn test_if_noelse() {
    let ans = WithMeta(
        Expr::If(
            WithMeta(Expr::Literal(Literal::Int(100)), 4..7).into(),
            WithMeta(Expr::Var("hoge".to_symbol(), None).into(), 9..13).into(),
            None,
        ),
        0..13,
    );
    test_string!("if (100) hoge ", ans);
}

#[test]
fn test_int() {
    let ans = WithMeta(Expr::Literal(Literal::Int(3466)), 0..4);
    test_string!("3466", ans);
}
#[test]
fn test_string() {
    let ans = WithMeta(Expr::Literal(Literal::String("teststr".to_string())), 0..9);
    test_string!("\"teststr\"", ans);
}
#[test]
fn test_block() {
    let ans = WithMeta(
        Expr::Block(Some(
            Expr::Let(
                WithMeta(
                    TypedPattern {
                        pat: Pattern::Single("hoge".to_symbol()),
                        ty: None,
                    },
                    5..9,
                ),
                Box::new(WithMeta(Expr::Literal(Literal::Int(100)), 12..15)),
                Some(Box::new(WithMeta(
                    Expr::Var("hoge".to_symbol(), None),
                    16..20,
                ))),
            )
            .into_id(1..20),
        )),
        0..21,
    );
    test_string!(
        "{let hoge = 100
hoge}",
        ans
    );
}
#[test]
fn test_add() {
    let ans = WithMeta(
        Expr::Apply(
            Expr::Var("add".to_symbol(), None).into_id(6..7),
            vec![
                WithMeta(Expr::Literal(Literal::Float("3466.0".to_string())), 0..6),
                WithMeta(Expr::Literal(Literal::Float("2000.0".to_string())), 7..13),
            ],
        ),
        0..13,
    );
    test_string!("3466.0+2000.0", ans);
}
#[test]
fn test_var() {
    let ans = WithMeta(Expr::Var("hoge".to_symbol(), None), 0..4);
    test_string!("hoge", ans);
}
#[test]
fn test_apply() {
    let ans = WithMeta(
        Expr::Apply(
            Expr::Var("myfun".to_symbol(), None).into_id(0..5),
            vec![WithMeta(Expr::Var("callee".to_symbol(), None), 6..12)],
        ),
        0..13,
    );
    test_string!("myfun(callee)", ans);
}
#[test]
fn test_applynested() {
    let ans = WithMeta(
        Expr::Apply(
            Expr::Var("myfun".to_symbol(), None).into_id(0..5),
            vec![WithMeta(
                Expr::Apply(
                    Expr::Var("myfun2".to_symbol(), None).into_id(6..12),
                    vec![WithMeta(Expr::Var("callee".to_symbol(), None), 13..19)],
                ),
                6..20,
            )],
        ),
        0..21,
    );
    test_string!("myfun(myfun2(callee))", ans);
}
#[test]
fn test_macroexpand() {
    let ans = WithMeta(
        Expr::Escape(Box::new(WithMeta(
            Expr::Apply(
                Expr::Var("myfun".to_symbol(), None).into_id(0..6),
                vec![WithMeta(Expr::Var("callee".to_symbol(), None), 7..13)],
            ),
            0..14,
        ))),
        0..14,
    );
    test_string!("myfun!(callee)", ans);
}
#[test]
fn test_fndef() {
    let ans = WithMeta(
        Expr::LetRec(
            TypedId {
                ty: Some(Type::Function(
                    vec![Type::Unknown, Type::Unknown],
                    Box::new(Type::Unknown),
                    None,
                )),
                id: "hoge".to_symbol(),
            },
            Box::new(WithMeta(
                Expr::Lambda(
                    vec![
                        WithMeta(
                            TypedId {
                                ty: None,
                                id: "input".to_symbol(),
                            },
                            8..13,
                        ),
                        WithMeta(
                            TypedId {
                                ty: None,
                                id: "gue".to_symbol(),
                            },
                            14..17,
                        ),
                    ],
                    None,
                    Expr::Var("input".to_symbol(), None).into_id(21..26),
                ),
                0..28,
            )),
            None,
        ),
        0..28,
    );
    test_string!("fn hoge(input,gue){\n input\n}", ans);
}
#[test]
fn test_macrodef() {
    let ans = WithMeta(
        Expr::LetRec(
            TypedId {
                ty: None,
                id: "hoge".to_symbol(),
            },
            Box::new(WithMeta(
                Expr::Lambda(
                    vec![
                        WithMeta(
                            TypedId {
                                ty: None,
                                id: "input".to_symbol(),
                            },
                            11..16,
                        ),
                        WithMeta(
                            TypedId {
                                ty: None,
                                id: "gue".to_symbol(),
                            },
                            17..20,
                        ),
                    ],
                    None,
                    Expr::Bracket(Box::new(WithMeta(
                        Expr::Var("input".to_symbol(), None),
                        24..29,
                    )))
                    .into_id(24..29),
                ),
                0..31,
            )),
            None,
        ),
        0..31,
    );
    test_string!("macro hoge(input,gue){\n input\n}", ans);
}

#[test]
fn test_tuple() {
    let tuple_items = vec![
        WithMeta(Expr::Literal(Literal::Float("1.0".to_string())), 1..4),
        WithMeta(Expr::Literal(Literal::Float("2.0".to_string())), 6..9),
    ];

    let ans = WithMeta(Expr::Tuple(tuple_items.clone()), 0..10);
    test_string!("(1.0, 2.0)", ans);

    // with trailing comma
    let ans = WithMeta(Expr::Tuple(tuple_items.clone()), 0..12);
    test_string!("(1.0, 2.0, )", ans);

    // trailing comma is mandatory for a single-element tuple
    let ans = WithMeta(Expr::Tuple(vec![tuple_items[0].clone()]), 0..7);
    test_string!("(1.0, )", ans);

    // This is not a tuple
    let ans = tuple_items[0].clone();
    test_string!("(1.0)", ans);
}

#[test]
#[should_panic]
fn test_fail() {
    let src = "let 100 == hoge\n fuga";
    match parse(&src.to_string()) {
        Err(errs) => {
            panic!("{}", utils::error::dump_to_string(&errs))
        }
        _ => {}
    };
}
