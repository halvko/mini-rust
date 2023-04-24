use parse::{
    expression::{Block, Expression, Statement, Value},
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
    assert_eq!(
        gen_ast(include_str!("../test_inputs/simple.min.rs")).unwrap(),
        structure
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
                        cond: Box::new(Expression::Ident(Ident {
                            inner: "true".to_owned(),
                        })),
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
    assert_eq!(
        gen_ast(include_str!("../test_inputs/if.min.rs")).unwrap(),
        structure
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
                        ident: "a".to_owned(),
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
    assert_eq!(
        gen_ast(include_str!("../test_inputs/let.min.rs")).unwrap(),
        structure
    )
}
