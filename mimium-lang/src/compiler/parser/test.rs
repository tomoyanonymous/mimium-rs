use super::*;
use crate::pattern::TypedId;
use crate::utils;
use std::path::PathBuf;

macro_rules! test_string {
    ($src:literal, $ans:expr) => {
        let srcstr = $src.to_string();
        match parse(&srcstr, None) {
            Ok(ast) => {
                assert!(
                    ast.to_expr() == $ans.to_expr(),
                    "res:{:?}\nans:{:?}",
                    ast,
                    $ans
                );
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
        TypedPattern {
            pat: Pattern::Single("goge".to_symbol()),
            ty: Type::Unknown.into_id_with_span(4..8),
        },
        Expr::Literal(Literal::Int(36)).into_id(11..13),
        Some(Expr::Var("goge".to_symbol()).into_id(15..19)),
    )
    .into_id(0..19);
    test_string!("let goge = 36\n goge", ans);
}
#[test]
fn test_lettuple() {
    let ans = Expr::Let(
        TypedPattern {
            pat: Pattern::Tuple(vec![
                Pattern::Single("a".to_symbol()),
                Pattern::Single("b".to_symbol()),
            ]),
            ty: Type::Unknown.into_id_with_span(4..9),
        },
        Expr::Tuple(vec![
            Expr::Literal(Literal::Int(36)).into_id(13..15),
            Expr::Literal(Literal::Int(89)).into_id(16..18),
        ])
        .into_id(12..19),
        Some(Expr::Var("hoge".to_symbol()).into_id(21..25)),
    )
    .into_id(0..25);
    test_string!("let (a,b) = (36,89)\n hoge", ans);
}
#[test]
fn test_if() {
    let ans = Expr::If(
        Expr::Literal(Literal::Int(100)).into_id(4..7),
        Expr::Var("hoge".to_symbol()).into_id(9..13),
        Some(Expr::Var("fuga".to_symbol()).into_id(19..23)),
    )
    .into_id(0..23);
    test_string!("if (100) hoge else fuga", ans);
}
#[test]
fn test_if_noelse() {
    let ans = Expr::If(
        Expr::Literal(Literal::Int(100)).into_id(4..7),
        Expr::Var("hoge".to_symbol()).into_id(9..13),
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
            TypedPattern {
                pat: Pattern::Single("hoge".to_symbol()),
                ty: Type::Unknown.into_id_with_span(5..9),
            },
            Expr::Literal(Literal::Int(100)).into_id(12..15),
            Some(Expr::Var("hoge".to_symbol()).into_id(16..20)),
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
        Expr::Var("add".to_symbol()).into_id(6..7),
        vec![
            Expr::Literal(Literal::Float("3466.0".to_string())).into_id(0..6),
            Expr::Literal(Literal::Float("2000.0".to_string())).into_id(7..13),
        ],
    )
    .into_id(0..13);
    test_string!("3466.0+2000.0", ans);
}
#[test]
fn test_at() {
    let ans1 = Expr::Apply(
        Expr::Var("_mimium_schedule_at".to_symbol()).into_id(3..4),
        vec![
            Expr::Literal(Literal::Float("1.0".to_string())).into_id(4..7),
            Expr::Var("foo".to_symbol()).into_id(0..3),
        ],
    )
    .into_id(0..7);
    test_string!("foo@1.0", ans1);

    let time = Expr::Apply(
        Expr::Var("exp".to_symbol()).into_id(7..8),
        vec![
            Expr::Literal(Literal::Float("1.0".to_string())).into_id(4..7),
            Expr::Literal(Literal::Float("2.0".to_string())).into_id(8..11),
        ],
    )
    .into_id(4..11);
    let ans2 = Expr::Apply(
        Expr::Var("_mimium_schedule_at".to_symbol()).into_id(3..4),
        vec![time, Expr::Var("foo".to_symbol()).into_id(0..3)],
    )
    .into_id(0..11);
    test_string!("foo@1.0^2.0", ans2);
}
#[test]
fn test_var() {
    let ans = Expr::Var("hoge".to_symbol()).into_id(0..4);
    test_string!("hoge", ans);
}
#[test]
fn test_apply() {
    let ans = Expr::Apply(
        Expr::Var("myfun".to_symbol()).into_id(0..5),
        vec![Expr::Var("callee".to_symbol()).into_id(6..12)],
    )
    .into_id(0..13);
    test_string!("myfun(callee)", ans);
}

#[test]
fn test_assign1() {
    let ans = Expr::Then(
        Expr::Assign(
            Expr::Var("hoge".to_symbol()).into_id(0..4),
            Expr::Var("fuga".to_symbol()).into_id(7..11),
        )
        .into_id(0..11),
        None,
    )
    .into_id(0..11);
    test_string!("hoge = fuga", ans);
}
#[test]
fn test_assign2() {
    let ans = Expr::Then(
        Expr::Assign(
            Expr::Var("hoge".to_symbol()).into_id(0..4),
            Expr::Var("fuga".to_symbol()).into_id(7..11),
        )
        .into_id(0..11),
        Some(Expr::Literal(Literal::Float("100.0".to_string())).into_id(13..18)),
    )
    .into_id(0..18);
    test_string!("hoge = fuga\n 100.0", ans);
}
#[test]
fn test_applynested() {
    let ans = Expr::Apply(
        Expr::Var("myfun".to_symbol()).into_id(0..5),
        vec![Expr::Apply(
            Expr::Var("myfun2".to_symbol()).into_id(6..12),
            vec![Expr::Var("callee".to_symbol()).into_id(13..19)],
        )
        .into_id(6..20)],
    )
    .into_id(0..20);
    test_string!("myfun(myfun2(callee))", ans);
}
#[test]
fn test_macroexpand() {
    let ans = Expr::Escape(
        Expr::Apply(
            Expr::Var("myfun".to_symbol()).into_id(0..6),
            vec![Expr::Var("callee".to_symbol()).into_id(7..13)],
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
            ty: Type::Function(
                vec![
                    Type::Unknown.into_id_with_span(0..28),
                    Type::Unknown.into_id_with_span(0..28),
                ],
                Type::Unknown.into_id_with_span(0..28),
                None,
            )
            .into_id_with_span(0..28),

            id: "hoge".to_symbol(),
        },
        Expr::Lambda(
            vec![
                TypedId {
                    id: "input".to_symbol(),
                    ty: Type::Unknown.into_id_with_span(8..13),
                },
                TypedId {
                    id: "gue".to_symbol(),
                    ty: Type::Unknown.into_id_with_span(14..17),
                },
            ],
            None,
            Expr::Var("input".to_symbol()).into_id(21..26),
        )
        .into_id(0..28),
        None,
    )
    .into_id(0..28);
    test_string!("fn hoge(input,gue){\n input\n}", ans);
}
#[test]
fn global_fnmultiple() {
    let ans = Expr::LetRec(
        TypedId {
            id: "hoge".to_symbol(),
            ty: Type::Function(
                vec![
                    Type::Unknown.into_id_with_span(0..28),
                    Type::Unknown.into_id_with_span(0..28),
                ],
                Type::Unknown.into_id_with_span(0..28),
                None,
            )
            .into_id_with_span(0..28),
        },
        Expr::Lambda(
            vec![
                TypedId {
                    id: "input".to_symbol(),
                    ty: Type::Unknown.into_id_with_span(8..13),
                },
                TypedId {
                    id: "gue".to_symbol(),
                    ty: Type::Unknown.into_id_with_span(14..17),
                },
            ],
            None,
            Expr::Var("input".to_symbol()).into_id(21..26),
        )
        .into_id(0..28),
        Some(
            Expr::LetRec(
                TypedId {
                    id: "hoge".to_symbol(),
                    ty: Type::Function(
                        vec![
                            Type::Unknown.into_id_with_span(29..57),
                            Type::Unknown.into_id_with_span(29..57),
                        ],
                        Type::Unknown.into_id_with_span(29..57),
                        None,
                    )
                    .into_id_with_span(29..57),
                },
                Expr::Lambda(
                    vec![
                        TypedId {
                            id: "input".to_symbol(),
                            ty: Type::Unknown.into_id_with_span(37..42),
                        },
                        TypedId {
                            id: "gue".to_symbol(),
                            ty: Type::Unknown.into_id_with_span(43..46),
                        },
                    ],
                    None,
                    Expr::Var("input".to_symbol()).into_id(50..55),
                )
                .into_id(29..57),
                None,
            )
            .into_id(29..57),
        ),
    )
    .into_id(0..57);
    test_string!(
        "fn hoge(input,gue){\n input\n}\nfn hoge(input,gue){\n input\n}",
        ans
    );
}

#[test]
fn test_macrodef() {
    let ans = Expr::LetRec(
        TypedId {
            id: "hoge".to_symbol(),
            ty: Type::Unknown.into_id_with_span(6..10),
        },
        Expr::Lambda(
            vec![
                TypedId {
                    id: "input".to_symbol(),
                    ty: Type::Unknown.into_id_with_span(11..16),
                },
                TypedId {
                    id: "gue".to_symbol(),
                    ty: Type::Unknown.into_id_with_span(17..20),
                },
            ],
            None,
            Expr::Bracket(Expr::Var("input".to_symbol()).into_id(24..29)).into_id(0..31),
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
    let ans = Expr::Tuple(vec![tuple_items[0]]).into_id(0..7);
    test_string!("(1.0, )", ans);

    // This is not a tuple
    let ans = tuple_items[0];
    test_string!("(1.0)", ans);
}

#[test]
fn test_stmt_without_return() {
    let ans = Expr::LetRec(
        TypedId {
            id: "test".to_symbol(),
            ty: Type::Function(
                vec![Type::Unknown.into_id_with_span(0..56)],
                Type::Unknown.into_id_with_span(0..56),
                None,
            )
            .into_id_with_span(0..56),
        },
        Expr::Lambda(
            vec![TypedId {
                id: "input".to_symbol(),
                ty: Type::Unknown.into_id_with_span(8..13),
            }],
            None,
            Expr::Let(
                TypedPattern {
                    pat: Pattern::Single("v".to_symbol()),
                    ty: Type::Unknown.into_id_with_span(24..25),
                },
                Expr::Apply(
                    Expr::Var("add".to_symbol()).into_id(33..34),
                    vec![
                        Expr::Var("input".to_symbol()).into_id(28..33),
                        Expr::Literal(Literal::Int(1)).into_id(34..35),
                    ],
                )
                .into_id(28..35),
                Some(
                    Expr::Then(
                        Expr::Apply(
                            Expr::Var("print".to_symbol()).into_id(40..45),
                            vec![Expr::Var("v".to_symbol()).into_id(46..47)],
                        )
                        .into_id(40..48),
                        Some(Expr::Var("v".to_symbol()).into_id(53..54)),
                    )
                    .into_id(40..54),
                ),
            )
            .into_id(20..54),
        )
        .into_id(0..56),
        None,
    )
    .into_id(0..56);
    test_string!(
        r"fn test(input){
    let v = input+1
    print(v)
    v
}",
        ans
    );
}

#[test]
#[should_panic]
fn test_fail() {
    let src = "let 100 == hoge\n fuga";
    match parse(&src.to_string(), None) {
        Err(errs) => {
            panic!("{}", utils::error::dump_to_string(&errs))
        }
        _ => {}
    };
}

#[test]
fn test_err_builtin_redefine() {
    let src = r"fn div(){
    0.0
}
100.0";
    let res = &parse(&src.to_string(), None).expect_err("should be error");
    assert_eq!(res.len(), 1);

    let err_ans: Box<dyn ReportableError> = Box::new(error::ParseError::<Token>(
        Simple::custom(3..6, "Builtin functions cannot be re-defined.").with_label("function decl"),
    ));
    assert_eq!(res[0].to_string(), err_ans.to_string())
}
