#[cfg(test)]
mod tests {

    use ast::expr::*;
    use mimium_parser::*;
    use std::path::PathBuf;
    use utils::metadata::*;

    macro_rules! test_string {
        ($src:literal, $ans:expr) => {
            let srcstr = $src.to_string();
            match parse(srcstr.clone()) {
                Ok(ast) => {
                    assert_eq!(ast, $ans);
                }
                Err(errs) => {
                    utils::error::report(&srcstr, PathBuf::new(), errs);
                    panic!();
                }
            }
        };
    }

    #[test]
    pub fn test_let() {
        let ans = WithMeta(
            Expr::Let(
                TypedId {
                    id: "goge".to_string(),
                    ty: None,
                },
                Box::new(WithMeta(Expr::Literal(Literal::Int(36)), 11..13)),
                Some(Box::new(WithMeta(
                    Expr::Var("goge".to_string(), None),
                    15..19,
                ))),
            ),
            0..19,
        );
        test_string!("let goge = 36\n goge", ans);
    }
    #[test]
    pub fn test_int() {
        let ans = WithMeta(Expr::Literal(Literal::Int(3466)), 0..4);
        test_string!("3466", ans);
    }
    #[test]
    pub fn test_string() {
        let ans = WithMeta(Expr::Literal(Literal::String("teststr".to_string())), 0..9);
        test_string!("\"teststr\"", ans);
    }
    #[test]
    pub fn test_block() {
        let ans = WithMeta(
            Expr::Block(Some(Box::new(WithMeta(
                Expr::Let(
                    TypedId {
                        ty: None,
                        id: "hoge".to_string(),
                    },
                    Box::new(WithMeta(Expr::Literal(Literal::Int(100)), 12..15)),
                    Some(Box::new(WithMeta(
                        Expr::Var("hoge".to_string(), None),
                        18..22,
                    ))),
                ),
                1..22,
            )))),
            0..23,
        );
        test_string!("{let hoge = 100 \n hoge}", ans);
    }
    #[test]
    pub fn test_add() {
        let ans = WithMeta(
            Expr::Apply(
                Box::new(WithMeta(Expr::Var("add".to_string(), None), 6..7)),
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
    pub fn test_var() {
        let ans = WithMeta(Expr::Var("hoge".to_string(), None), 0..4);
        test_string!("hoge", ans);
    }
    #[test]
    pub fn test_apply() {
        let ans = WithMeta(
            Expr::Apply(
                Box::new(WithMeta(Expr::Var("myfun".to_string(), None), 0..5)),
                vec![WithMeta(Expr::Var("callee".to_string(), None), 6..12)],
            ),
            0..13,
        );
        test_string!("myfun(callee)", ans);
    }
    #[test]
    pub fn test_applynested() {
        let ans = WithMeta(
            Expr::Apply(
                Box::new(WithMeta(Expr::Var("myfun".to_string(), None), 0..5)),
                vec![WithMeta(
                    Expr::Apply(
                        Box::new(WithMeta(Expr::Var("myfun2".to_string(), None), 6..12)),
                        vec![WithMeta(Expr::Var("callee".to_string(), None), 13..19)],
                    ),
                    6..20,
                )],
            ),
            0..21,
        );
        test_string!("myfun(myfun2(callee))", ans);
    }
    #[test]
    pub fn test_macroexpand() {
        let ans = WithMeta(
            Expr::Escape(Box::new(WithMeta(
                Expr::Apply(
                    Box::new(WithMeta(Expr::Var("myfun".to_string(), None), 0..6)),
                    vec![WithMeta(Expr::Var("callee".to_string(), None), 7..13)],
                ),
                0..14,
            ))),
            0..14,
        );
        test_string!("myfun!(callee)", ans);
    }
    #[test]
    pub fn test_fndef() {
        let ans = WithMeta(
            Expr::LetRec(
                TypedId {
                    ty: None,
                    id: "hoge".to_string(),
                },
                Box::new(WithMeta(
                    Expr::Lambda(
                        vec![
                            WithMeta(
                                TypedId {
                                    ty: None,
                                    id: "input".to_string(),
                                },
                                8..13,
                            ),
                            WithMeta(
                                TypedId {
                                    ty: None,
                                    id: "gue".to_string(),
                                },
                                14..17,
                            ),
                        ],
                        Box::new(WithMeta(Expr::Var("input".to_string(), None), 21..26)),
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
    pub fn test_macrodef() {
        let ans = WithMeta(
            Expr::LetRec(
                TypedId {
                    ty: None,
                    id: "hoge".to_string(),
                },
                Box::new(WithMeta(
                    Expr::Lambda(
                        vec![
                            WithMeta(
                                TypedId {
                                    ty: None,
                                    id: "input".to_string(),
                                },
                                11..16,
                            ),
                            WithMeta(
                                TypedId {
                                    ty: None,
                                    id: "gue".to_string(),
                                },
                                17..20,
                            ),
                        ],
                        Box::new(WithMeta(
                            Expr::Bracket(Box::new(WithMeta(
                                Expr::Var("input".to_string(), None),
                                24..29,
                            ))),
                            24..29,
                        )),
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
    #[should_panic]
    pub fn test_fail() {
        let src = "let 100 == hoge\n fuga";
        match parse(src.clone().to_string()) {
            Err(errs) => {
                utils::error::report(&src.to_string(), PathBuf::new(), errs);
                panic!()
            }
            _ => {}
        };
    }
}
