use parse::{
    expression::{Block, CallExp, Expression, Infix, Statement, Value},
    function::Function,
    gen_ast, AbstractSyntaxTree, Ident, Position, Type,
};

#[test]
fn simple() {
    let structure = AbstractSyntaxTree {
        globals: [(
            Ident {
                inner: "main".to_owned(),
            },
            Function {
                args: vec![],
                ret_type: Type {
                    name: Ident {
                        inner: "void".to_owned(),
                    },
                },
                body: Block {
                    stmts: vec![],
                    expr: None,
                },
                span: parse::Span {
                    start: Position {
                        byte: 0,
                        col: 1,
                        line: 1,
                    },
                    end: Position {
                        byte: 12,
                        line: 1,
                        col: 13,
                    },
                },
            },
        )]
        .into_iter()
        .collect(),
    };
    eq_or_panic(
        gen_ast(include_str!("../test_inputs/simple.min.rs")).unwrap(),
        structure,
    )
}

#[test]
fn r#if() {
    let structure = AbstractSyntaxTree {
        globals: [(
            Ident {
                inner: "main".to_owned(),
            },
            Function {
                args: vec![],
                ret_type: Type {
                    name: Ident {
                        inner: "void".to_owned(),
                    },
                },
                body: Block {
                    stmts: vec![],
                    expr: Some(Expression::IfExp {
                        cond: Box::new(Expression::Val(Value::Bool(true))),
                        block: Box::new(Block {
                            stmts: vec![],
                            expr: None,
                        }),
                        else_block: None,
                    }),
                },
                span: parse::Span {
                    start: Position {
                        byte: 0,
                        col: 1,
                        line: 1,
                    },
                    end: Position {
                        byte: 28,
                        line: 3,
                        col: 2,
                    },
                },
            },
        )]
        .into_iter()
        .collect(),
    };
    eq_or_panic(
        gen_ast(include_str!("../test_inputs/if.min.rs")).unwrap(),
        structure,
    )
}

/*
fn main() {
    let a = 38;
}
*/

#[test]
fn r#let() {
    let structure = AbstractSyntaxTree {
        globals: [(
            Ident {
                inner: "main".to_owned(),
            },
            Function {
                args: vec![],
                ret_type: Type {
                    name: Ident {
                        inner: "void".to_owned(),
                    },
                },
                body: Block {
                    stmts: vec![Statement::Let {
                        ident: Ident {
                            inner: "a".to_owned(),
                        },
                        r#mut: false,
                        r#type: None,
                        expr: Expression::Val(Value::Int("38".to_owned())),
                    }],
                    expr: None,
                },
                span: parse::Span {
                    start: Position {
                        byte: 0,
                        col: 1,
                        line: 1,
                    },
                    end: Position {
                        byte: 29,
                        line: 3,
                        col: 2,
                    },
                },
            },
        )]
        .into_iter()
        .collect(),
    };
    eq_or_panic(
        gen_ast(include_str!("../test_inputs/let.min.rs")).unwrap(),
        structure,
    )
}

/*
fn main() {
    let a = 42;
    let b = 69;
    if (a + b) < maths(a, b) {
        return 42;
    }
}

fn maths(a: i32, b: i32) -> i32 {
    let mut c = a * b;
    c = c + 1;
    c
}
*/

#[test]
fn nested_exps() {
    let expected = AbstractSyntaxTree {
        globals: [
            (
                Ident {
                    inner: "main".to_owned(),
                },
                Function {
                    args: vec![],
                    ret_type: Type {
                        name: Ident {
                            inner: "void".to_owned(),
                        },
                    },
                    body: Block {
                        stmts: vec![
                            Statement::Let {
                                ident: Ident {
                                    inner: "a".to_owned(),
                                },
                                r#mut: false,
                                r#type: None,
                                expr: Expression::Val(Value::Int("42".to_owned())),
                            },
                            Statement::Let {
                                ident: Ident {
                                    inner: "b".to_owned(),
                                },
                                r#mut: false,
                                r#type: None,
                                expr: Expression::Val(Value::Int("69".to_owned())),
                            },
                        ],
                        expr: Some(Expression::IfExp {
                            cond: Box::new(Expression::BinExp(
                                Box::new(Expression::BinExp(
                                    Box::new(Expression::Ident(Ident {
                                        inner: "a".to_owned(),
                                    })),
                                    Infix::Add,
                                    Box::new(Expression::Ident(Ident {
                                        inner: "b".to_owned(),
                                    })),
                                )),
                                Infix::Lt,
                                Box::new(Expression::CallExp(CallExp {
                                    ident: Ident {
                                        inner: "maths".to_owned(),
                                    },
                                    args: vec![
                                        Expression::Ident(Ident {
                                            inner: "a".to_owned(),
                                        }),
                                        Expression::Ident(Ident {
                                            inner: "b".to_owned(),
                                        }),
                                    ],
                                })),
                            )),
                            block: Box::new(Block {
                                stmts: vec![Statement::Expression(Expression::Return(Some(
                                    Box::new(Expression::Val(Value::Int("42".to_owned()))),
                                )))],
                                expr: None,
                            }),
                            else_block: None,
                        }),
                    },
                    span: parse::Span {
                        start: Position {
                            byte: 0,
                            col: 1,
                            line: 1,
                        },
                        end: Position {
                            byte: 28,
                            line: 3,
                            col: 2,
                        },
                    },
                },
            ),
            (
                Ident {
                    inner: "maths".to_owned(),
                },
                Function {
                    args: vec![
                        (
                            Ident {
                                inner: "a".to_owned(),
                            },
                            Type {
                                name: Ident {
                                    inner: "i32".to_owned(),
                                },
                            },
                        ),
                        (
                            Ident {
                                inner: "b".to_owned(),
                            },
                            Type {
                                name: Ident {
                                    inner: "i32".to_owned(),
                                },
                            },
                        ),
                    ],
                    ret_type: Type {
                        name: Ident {
                            inner: "i32".to_owned(),
                        },
                    },
                    body: Block {
                        stmts: vec![
                            Statement::Let {
                                ident: Ident {
                                    inner: "c".to_owned(),
                                },
                                r#mut: true,
                                r#type: None,
                                expr: Expression::BinExp(
                                    Box::new(Expression::Ident(Ident {
                                        inner: "a".to_string(),
                                    })),
                                    Infix::Mul,
                                    Box::new(Expression::Ident(Ident {
                                        inner: "b".to_owned(),
                                    })),
                                ),
                            },
                            Statement::Expression(Expression::BinExp(
                                Box::new(Expression::Ident(Ident {
                                    inner: "c".to_owned(),
                                })),
                                Infix::Assign,
                                Box::new(Expression::BinExp(
                                    Box::new(Expression::Ident(Ident {
                                        inner: "c".to_owned(),
                                    })),
                                    Infix::Add,
                                    Box::new(Expression::Val(Value::Int("1".to_owned()))),
                                )),
                            )),
                        ],
                        expr: Some(Expression::Ident(Ident {
                            inner: "c".to_owned(),
                        })),
                    },
                    span: parse::Span {
                        start: Position {
                            byte: 0,
                            line: 0,
                            col: 0,
                        },
                        end: Position {
                            byte: 0,
                            line: 0,
                            col: 0,
                        },
                    },
                },
            ),
        ]
        .into_iter()
        .collect(),
    };
    let parsed = gen_ast(include_str!("../test_inputs/nested_exps.min.rs")).unwrap();
    eq_or_panic(parsed, expected);
}

fn eq_or_panic(parsed: AbstractSyntaxTree, expected: AbstractSyntaxTree) {
    if parsed != expected {
        panic!("parsed had unexpected value:\n{parsed}\nexpected:\n{expected}")
    }
}

#[test]
fn r#loop() {
    let structure = AbstractSyntaxTree {
        globals: [(
            Ident {
                inner: "main".to_owned(),
            },
            Function {
                args: vec![],
                ret_type: Type {
                    name: Ident {
                        inner: "void".to_owned(),
                    },
                },
                body: Block {
                    stmts: vec![
                        Statement::Let {
                            ident: Ident {
                                inner: "i".to_owned(),
                            },
                            r#mut: true,
                            r#type: None,
                            expr: Expression::Val(Value::Int("0".to_owned())),
                        },
                        Statement::Let {
                            ident: Ident {
                                inner: "a".to_owned(),
                            },
                            r#mut: false,
                            r#type: None,
                            expr: Expression::Loop(Box::new(Block {
                                stmts: vec![Statement::Expression(Expression::IfExp {
                                    cond: Box::new(Expression::BinExp(
                                        Box::new(Expression::Ident(Ident {
                                            inner: "i".to_owned(),
                                        })),
                                        Infix::Geq,
                                        Box::new(Expression::Val(Value::Int("10".to_owned()))),
                                    )),
                                    block: Box::new(Block {
                                        stmts: vec![Statement::Expression(Expression::Break(
                                            Some(Box::new(Expression::Val(Value::Int(
                                                "42".to_owned(),
                                            )))),
                                        ))],
                                        expr: None,
                                    }),
                                    else_block: None,
                                })],
                                expr: Some(Expression::BinExp(
                                    Box::new(Expression::Ident(Ident {
                                        inner: "i".to_owned(),
                                    })),
                                    Infix::Assign,
                                    Box::new(Expression::BinExp(
                                        Box::new(Expression::Ident(Ident {
                                            inner: "i".to_owned(),
                                        })),
                                        Infix::Add,
                                        Box::new(Expression::Val(Value::Int("1".to_owned()))),
                                    )),
                                )),
                            })),
                        },
                    ],
                    expr: None,
                },
                span: parse::Span {
                    start: Position {
                        byte: 0,
                        col: 0,
                        line: 0,
                    },
                    end: Position {
                        byte: 0,
                        line: 0,
                        col: 0,
                    },
                },
            },
        )]
        .into_iter()
        .collect(),
    };
    eq_or_panic(
        gen_ast(include_str!("../test_inputs/loop.min.rs")).unwrap(),
        structure,
    )
}
