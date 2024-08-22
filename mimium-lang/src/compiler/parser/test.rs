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
    let ans = Expr::Let(
        WithMeta(
            TypedPattern {
                pat: Pattern::Single("goge".to_symbol()),
                ty: None,
            },
            4..8,
        ),
        Expr::Literal(Literal::Int(36)).into_id(11..13),
        Some(Expr::Var("goge".to_symbol(), None).into_id(15..19)),
    )
    .into_id(0..19);
    test_string!("let goge = 36\n goge", ans);
}
#[test]
fn test_lettuple() {
    let ans = Expr::Let(
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
        Expr::Tuple(vec![
            Expr::Literal(Literal::Int(36)).into_id(13..15),
            Expr::Literal(Literal::Int(89)).into_id(16..18),
        ])
        .into_id(12..19),
        Some(Expr::Var("hoge".to_symbol(), None).into_id(21..25)),
    )
    .into_id(0..25);
    test_string!("let (a,b) = (36,89)\n hoge", ans);
}
#[test]
fn test_if() {
    let ans = Expr::If(
        Expr::Literal(Literal::Int(100)).into_id(4..7),
        Expr::Var("hoge".to_symbol(), None).into_id(9..13),
        Some(Expr::Var("fuga".to_symbol(), None).into_id(19..23)),
    )
    .into_id(0..23);
    test_string!("if (100) hoge else fuga", ans);
}
#[test]
fn test_if_noelse() {
    let ans = Expr::If(
        Expr::Literal(Literal::Int(100)).into_id(4..7),
        Expr::Var("hoge".to_symbol(), None).into_id(9..13),
        None,
    )
    .into_id(0..13);
    test_string!("if (100) hoge ", ans);
}

#[test]
fn test_int() {
    let ans = Expr::Literal(Literal::Int(3466)).into_id(0..4);
    test_string!("3466", ans);
}
#[test]
fn test_string() {
    let ans = Expr::Literal(Literal::String("teststr".to_string())).into_id(0..9);
    test_string!("\"teststr\"", ans);
}
#[test]
fn test_block() {
    let ans = Expr::Block(Some(
        Expr::Let(
            WithMeta(
                TypedPattern {
                    pat: Pattern::Single("hoge".to_symbol()),
                    ty: None,
                },
                5..9,
            ),
            Expr::Literal(Literal::Int(100)).into_id(12..15),
            Some(Expr::Var("hoge".to_symbol(), None).into_id(16..20)),
        )
        .into_id(1..20),
    ))
    .into_id(0..21);
    test_string!(
        "{let hoge = 100
hoge}",
        ans
    );
}
#[test]
fn test_add() {
    let ans = Expr::Apply(
        Expr::Var("add".to_symbol(), None).into_id(6..7),
        vec![
            Expr::Literal(Literal::Float("3466.0".to_string())).into_id(0..6),
            Expr::Literal(Literal::Float("2000.0".to_string())).into_id(7..13),
        ],
    )
    .into_id(0..13);
    test_string!("3466.0+2000.0", ans);
}
#[test]
fn test_var() {
    let ans = Expr::Var("hoge".to_symbol(), None).into_id(0..4);
    test_string!("hoge", ans);
}
#[test]
fn test_apply() {
    let ans = Expr::Apply(
        Expr::Var("myfun".to_symbol(), None).into_id(0..5),
        vec![Expr::Var("callee".to_symbol(), None).into_id(6..12)],
    )
    .into_id(0..13);
    test_string!("myfun(callee)", ans);
}
#[test]
fn test_applynested() {
    let ans = Expr::Apply(
        Expr::Var("myfun".to_symbol(), None).into_id(0..5),
        vec![Expr::Apply(
            Expr::Var("myfun2".to_symbol(), None).into_id(6..12),
            vec![Expr::Var("callee".to_symbol(), None).into_id(13..19)],
        )
        .into_id(6..20)],
    )
    .into_id(0..21);
    test_string!("myfun(myfun2(callee))", ans);
}
#[test]
fn test_macroexpand() {
    let ans = Expr::Escape(
        Expr::Apply(
            Expr::Var("myfun".to_symbol(), None).into_id(0..6),
            vec![Expr::Var("callee".to_symbol(), None).into_id(7..13)],
        )
        .into_id(0..14),
    )
    .into_id(0..14);
    test_string!("myfun!(callee)", ans);
}
#[test]
fn test_fndef() {
    let ans = Expr::LetRec(
        TypedId {
            ty: Some(Type::Function(
                vec![Type::Unknown, Type::Unknown],
                Type::Unknown.into_id_without_span(),
                None,
            )),
            id: "hoge".to_symbol(),
        },
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
        )
        .into_id(0..28),
        None,
    )
    .into_id(0..28);
    test_string!("fn hoge(input,gue){\n input\n}", ans);
}
#[test]
fn test_macrodef() {
    let ans = Expr::LetRec(
        TypedId {
            ty: None,
            id: "hoge".to_symbol(),
        },
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
            Expr::Bracket(Expr::Var("input".to_symbol(), None).into_id(24..29)).into_id(24..29),
        )
        .into_id(0..31),
        None,
    )
    .into_id(0..31);
    test_string!("macro hoge(input,gue){\n input\n}", ans);
}

#[test]
fn test_tuple() {
    let tuple_items = vec![
        Expr::Literal(Literal::Float("1.0".to_string())).into_id(1..4),
        Expr::Literal(Literal::Float("2.0".to_string())).into_id(6..9),
    ];

    let ans = Expr::Tuple(tuple_items.clone()).into_id(0..10);
    test_string!("(1.0, 2.0)", ans);

    // with trailing comma
    let ans = Expr::Tuple(tuple_items.clone()).into_id(0..12);
    test_string!("(1.0, 2.0, )", ans);

    // trailing comma is mandatory for a single-element tuple
    let ans = Expr::Tuple(vec![tuple_items[0].clone()]).into_id(0..7);
    test_string!("(1.0, )", ans);

    // This is not a tuple
    let ans = tuple_items[0];
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
