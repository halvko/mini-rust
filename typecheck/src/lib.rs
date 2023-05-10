use std::collections::HashMap;

#[derive(Hash)]
enum Type {
    Simple(u32),
}

//fn type_check(ast: AbstractSyntaxTree) -> TypedAST {}

struct TypedAST {
    types: HashMap<u32, Type>,
    ast: Vec<Function>,
}

struct Program(Vec<Function>);

struct Function {
    id: u32,
    binders: Vec<Binder>,
    ret_type: Type,
    body: Block,
}

struct Block {
    stmts: Vec<Statement>,
    expr: Box<Expression>,
    r#type: Type,
}

enum Statement {
    Let {
        id: u32,
        r#type: Type,
        expr: Expression,
    },
    Expr(Expression),
}

struct Expression {
    r#type: Type,
    kind: ExpressionKind,
}

enum ExpressionKind {
    BinExp {
        lhs: Box<Expression>,
        op: Infix,
        rhs: Box<Expression>,
    },
    LoopExp(Block),
    IfExp {
        cond: Box<Expression>,
        block: Block,
        else_block: Option<Block>,
    },
    Var {
        id: Id,
    },
    Val(Value),
}

enum Infix {
    Add
}

enum Value {
    USize(usize),
    ISize(isize),
}

type Id = u32;

struct Binder {
    id: u32,
    r#type: u32,
}

struct Node {
    id: Nodes,
    start: usize,
    end: usize,
    inner: Vec<Node>,
}
