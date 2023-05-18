use std::fmt::{Display, Write};

use immutable_map::TreeMap;
use parse::{AbstractSyntaxTree, Ident};
use symbol_table::Symbol;

type Type = Symbol;
type SymbolTable = symbol_table::SymbolTable<String>;

struct SpecialSymbols {
    bool: Symbol,
    void: Symbol,
    isize: Symbol,
    usize: Symbol,
    main: Symbol,
    r#true: Symbol,
    r#false: Symbol,
}

impl SpecialSymbols {
    fn new(symbols: &mut SymbolTable) -> Self {
        Self {
            bool: symbols.symbol("bool".to_owned()),
            void: symbols.symbol("void".to_owned()),
            isize: symbols.symbol("isize".to_owned()),
            usize: symbols.symbol("usize".to_owned()),
            main: symbols.symbol("main".to_owned()),
            r#true: symbols.symbol("true".to_owned()),
            r#false: symbols.symbol("false".to_owned()),
        }
    }
}

pub fn type_check(mut ast: AbstractSyntaxTree) -> TypedAST {
    let mut symbols = SymbolTable::new();
    let ss = SpecialSymbols::new(&mut symbols);
    let var_ctxt = TreeMap::new()
        .insert(ss.r#true, ss.bool)
        .insert(ss.r#false, ss.bool);
    if ast.globals.len() != 1 {
        panic!("only main function will be analyzed (and functions are not otherwise supported)")
    }
    let main = ast
        .globals
        .remove(&Ident {
            inner: "main".to_owned(),
        })
        .unwrap();
    if main.args.len() > 0 {
        panic!("too many arguments to main")
    }
    if main.ret_type
        != (parse::Type {
            name: parse::Ident {
                inner: "void".to_owned(),
            },
        })
    {
        panic!("main should not return anything")
    }
    let main_body = tc_block(main.body, &mut symbols, &ss, var_ctxt, &mut Loop::No);
    if main_body.r#type != ss.void {
        panic!(
            "from main body, expected void got {}",
            symbols.original(main_body.r#type)
        )
    }
    TypedAST {
        symbols,
        ast: vec![Function {
            name: ss.main,
            binders: Vec::new(),
            ret_type: ss.void,
            body: main_body,
        }],
    }
}

#[derive(Clone, Copy)]
enum Loop {
    No,
    Yes { r#type: Option<Symbol> },
}

fn tc_block(
    block: parse::expression::Block,
    symbols: &mut SymbolTable,
    ss: &SpecialSymbols,
    mut var_ctxt: TreeMap<Symbol, Symbol>,
    r#loop: &mut Loop,
) -> Block {
    let mut typed_stmts = Vec::new();
    for stmt in block.stmts {
        let (stmt, updated_ctxt) = tc_stmt(stmt, symbols, ss, var_ctxt, r#loop);
        var_ctxt = updated_ctxt;
        typed_stmts.push(stmt);
    }
    let expr = block
        .expr
        .map(|expr| Box::new(tc_expr(expr, symbols, ss, var_ctxt, r#loop)));

    let r#type = expr.as_ref().map(|e| e.r#type).unwrap_or(ss.void);
    Block {
        stmts: typed_stmts,
        expr,
        r#type,
    }
}

fn tc_stmt(
    stmt: parse::expression::Statement,
    symbols: &mut SymbolTable,
    ss: &SpecialSymbols,
    var_ctxt: TreeMap<Symbol, Symbol>,
    r#loop: &mut Loop,
) -> (Statement, TreeMap<Symbol, Symbol>) {
    match stmt {
        parse::expression::Statement::Let {
            ident,
            r#mut: _,
            r#type,
            expr,
        } => {
            let ident = symbols.symbol(ident.inner);
            let expr = tc_expr(expr, symbols, ss, var_ctxt.clone(), r#loop);
            let r#type = r#type
                .map(|t| symbols.symbol(t.name.inner))
                .unwrap_or(expr.r#type);
            if r#type != expr.r#type {
                panic!(
                    "Expected {}, found {}",
                    symbols.original(r#type),
                    symbols.original(expr.r#type)
                )
            }
            (
                Statement::Let {
                    ident,
                    r#type,
                    expr,
                },
                var_ctxt.insert(ident, r#type),
            )
        }
        parse::expression::Statement::Expression(e) => (
            Statement::Expr(tc_expr(e, symbols, ss, var_ctxt.clone(), r#loop)),
            var_ctxt,
        ),
    }
}

fn tc_expr(
    exp: parse::expression::Expression,
    symbols: &mut SymbolTable,
    ss: &SpecialSymbols,
    var_ctxt: TreeMap<Symbol, Symbol>,
    r#loop: &mut Loop,
) -> Expression {
    match exp {
        parse::expression::Expression::IfExp {
            cond,
            block,
            else_block,
        } => {
            let cond = tc_expr(*cond, symbols, ss, var_ctxt.clone(), r#loop);
            if cond.r#type != ss.bool {
                panic!("Expected bool, found {}", symbols.original(cond.r#type))
            }
            let block = tc_block(*block, symbols, ss, var_ctxt.clone(), r#loop);
            let else_block = else_block.map(|b| tc_block(*b, symbols, ss, var_ctxt, r#loop));
            if let Some(else_block) = &else_block {
                if block.r#type != else_block.r#type {
                    panic!("if {{...}} else {{...}} expression has mismatching types: if -> {}, else -> {}", symbols.original(block.r#type), symbols.original(else_block.r#type))
                }
            } else {
                if block.r#type != ss.void {
                    panic!(
                        "if {{...}} with no else returns {}, expected void",
                        symbols.original(block.r#type)
                    )
                }
            }

            Expression {
                r#type: block.r#type,
                kind: ExpressionKind::IfExp {
                    cond: Box::new(cond),
                    block,
                    else_block,
                },
            }
        }
        parse::expression::Expression::BlockExp(block) => {
            let block = tc_block(*block, symbols, ss, var_ctxt, r#loop);
            Expression {
                r#type: block.r#type,
                kind: ExpressionKind::Block(block),
            }
        }
        parse::expression::Expression::Ident(ident) => {
            let ident = symbols.symbol(ident.inner);
            Expression {
                r#type: var_ctxt[&ident],
                kind: ExpressionKind::Var { ident },
            }
        }
        parse::expression::Expression::Val(v) => match v {
            parse::expression::Value::Int(s) => {
                let (val, r#type) = (|v: &str| {
                    if let Some(u) = v.strip_suffix("usize") {
                        return (Value::USize(u.parse().unwrap()), ss.usize);
                    }
                    if let Some(i) = v.strip_suffix("isize") {
                        return (Value::ISize(i.parse().unwrap()), ss.isize);
                    }
                    (Value::ISize(v.parse().unwrap()), ss.isize)
                })(&s);

                Expression {
                    r#type,
                    kind: ExpressionKind::Val(val),
                }
            }
            parse::expression::Value::Bool(b) => Expression {
                r#type: ss.bool,
                kind: ExpressionKind::Val(Value::Bool(b)),
            },
        },
        parse::expression::Expression::BinExp(lhs, ifx, rhs) => {
            let lhs = tc_expr(*lhs, symbols, ss, var_ctxt.clone(), r#loop);
            let rhs = tc_expr(*rhs, symbols, ss, var_ctxt.clone(), r#loop);
            if lhs.r#type != rhs.r#type {
                panic!("Trying to do operation between different types")
            }
            let (infix, r#type) = match ifx {
                parse::expression::Infix::Add => {
                    if lhs.r#type != ss.isize && lhs.r#type != ss.usize {
                        panic!("Trying to add someting which is not a number")
                    }
                    (Infix::Add, lhs.r#type)
                }
                parse::expression::Infix::Eq => (Infix::Eq, ss.bool),
                parse::expression::Infix::Assign => {
                    if let ExpressionKind::Var { .. } = lhs.kind {
                        (Infix::Assign, ss.void)
                    } else {
                        panic!("trying to assign to general expression")
                    }
                }
                _ => todo!("Properly fix operators -.-"),
            };
            Expression {
                r#type: r#type,
                kind: ExpressionKind::BinExp {
                    lhs: Box::new(lhs),
                    op: infix,
                    rhs: Box::new(rhs),
                },
            }
        }
        parse::expression::Expression::Loop(block) => {
            let mut r#loop = Loop::Yes { r#type: None };
            let b = tc_block(*block, symbols, ss, var_ctxt, &mut r#loop);
            let Loop::Yes{r#type} = r#loop else {panic!("ICE 0: Loop is not inside loop?")};
            Expression {
                r#type: r#type.unwrap_or(ss.void),
                kind: ExpressionKind::LoopExp(b),
            }
        }
        parse::expression::Expression::Break(e) => {
            let e = e.map(|e| tc_expr(*e, symbols, ss, var_ctxt, r#loop));
            let Loop::Yes { r#type } = r#loop else { panic!("Use of break outside loop")};
            let e_type = e.as_ref().map(|b| b.r#type).unwrap_or(ss.void);
            if let Some(t) = r#type {
                if *t != e_type {
                    panic!("Incompatible break statements in loop, first had type {}, second had type {}", symbols.original(*t), symbols.original(e_type))
                }
            } else {
                *r#type = Some(e_type)
            }
            Expression {
                r#type: ss.void,
                kind: ExpressionKind::Break(e.map(|e| Box::new(e))),
            }
        }
        parse::expression::Expression::CallExp(_) => todo!("functions"),
        parse::expression::Expression::Return(_) => todo!("functions"),
    }
}

#[derive(Debug)]
pub struct TypedAST {
    pub symbols: SymbolTable,
    pub ast: Vec<Function>,
}

#[derive(Debug)]
pub struct Program(Vec<Function>);

#[derive(Debug)]
pub struct Function {
    pub name: Symbol,
    pub binders: Vec<Binder>,
    pub ret_type: Type,
    pub body: Block,
}

#[derive(Debug)]
pub struct Block {
    pub stmts: Vec<Statement>,
    pub expr: Option<Box<Expression>>,
    pub r#type: Type,
}

#[derive(Debug)]
pub enum Statement {
    Let {
        ident: Symbol,
        r#type: Type,
        expr: Expression,
    },
    Expr(Expression),
}

#[derive(Debug)]
pub struct Expression {
    pub r#type: Type,
    pub kind: ExpressionKind,
}

#[derive(Debug)]
pub enum ExpressionKind {
    BinExp {
        lhs: Box<Expression>,
        op: Infix,
        rhs: Box<Expression>,
    },
    LoopExp(Block),
    Break(Option<Box<Expression>>),
    IfExp {
        cond: Box<Expression>,
        block: Block,
        else_block: Option<Block>,
    },
    Block(Block),
    Var {
        ident: Symbol,
    },
    Val(Value),
}

#[derive(Debug)]
pub enum Infix {
    Add,
    Eq,
    Assign,
}

impl Display for Infix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Add => f.write_char('+'),
            Self::Assign => f.write_char('='),
            Self::Eq => f.write_str("=="),
        }
    }
}

#[derive(Debug)]
pub enum Value {
    USize(usize),
    ISize(isize),
    Bool(bool),
}
impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::USize(v) => v.fmt(f),
            Self::ISize(v) => v.fmt(f),
            Self::Bool(b) => b.fmt(f),
        }
    }
}

#[derive(Debug)]
pub struct Binder {
    pub id: Symbol,
    pub r#type: Symbol,
}

mod print {
    use std::fmt::{Display, Formatter, Write};

    use symbol_table::SymbolTable;

    use crate::{Block, Expression, Function, Statement, TypedAST};

    impl Display for TypedAST {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let TypedAST { symbols, ast } = self;
            for fun in ast {
                fmt_fun(f, fun, &symbols)?;
                f.write_char('\n')?;
            }
            Ok(())
        }
    }

    fn fmt_fun(f: &mut Formatter, fun: &Function, s: &SymbolTable<String>) -> std::fmt::Result {
        f.write_str("fn ")?;
        f.write_str(s.original(fun.name))?;
        f.write_char('(')?;
        if !fun.binders.is_empty() {
            todo!()
        }
        f.write_str(") -> ")?;
        f.write_str(s.original(fun.ret_type))?;
        f.write_char(' ')?;
        fmt_block(f, &fun.body, s)
    }
    fn fmt_block(f: &mut Formatter, block: &Block, s: &SymbolTable<String>) -> std::fmt::Result {
        let Block {
            stmts,
            expr,
            r#type,
        } = block;
        f.write_str("{ ")?;
        for st in stmts {
            fmt_stmt(f, st, s)?;
            f.write_str("; ")?;
        }
        if let Some(expr) = expr {
            fmt_expr(f, expr, s)?;
            f.write_char(' ')?;
        }
        f.write_str("}: ")?;
        f.write_str(s.original(*r#type))
    }
    fn fmt_stmt(f: &mut Formatter, stmt: &Statement, s: &SymbolTable<String>) -> std::fmt::Result {
        match stmt {
            Statement::Let {
                ident,
                r#type,
                expr,
            } => {
                write!(f, "let {}: {} = ", s.original(*ident), s.original(*r#type))?;
                fmt_expr(f, expr, s)
            }
            Statement::Expr(expr) => fmt_expr(f, expr, s),
        }
    }
    fn fmt_expr(f: &mut Formatter, expr: &Expression, s: &SymbolTable<String>) -> std::fmt::Result {
        let Expression { r#type, kind } = expr;
        match kind {
            crate::ExpressionKind::BinExp { lhs, op, rhs } => {
                f.write_str("( ")?;
                fmt_expr(f, &lhs, s)?;
                write!(f, " {} ", op)?;
                fmt_expr(f, &rhs, s)?;
                f.write_str(" )")?;
            }
            crate::ExpressionKind::LoopExp(b) => {
                f.write_str("loop ")?;
                fmt_block(f, b, s)?;
            }
            crate::ExpressionKind::Break(e) => {
                f.write_str("break")?;
                if let Some(e) = e {
                    f.write_char(' ')?;
                    fmt_expr(f, e, s)?;
                }
            }
            crate::ExpressionKind::IfExp {
                cond,
                block,
                else_block,
            } => {
                f.write_str("if ")?;
                fmt_expr(f, &cond, s)?;
                f.write_char(' ')?;
                fmt_block(f, block, s)?;
                if let Some(else_block) = else_block {
                    f.write_str(" else ")?;
                    fmt_block(f, else_block, s)?;
                }
            }
            crate::ExpressionKind::Block(b) => {
                fmt_block(f, b, s)?;
            }
            crate::ExpressionKind::Var { ident } => {
                f.write_str(s.original(*ident))?;
            }
            crate::ExpressionKind::Val(val) => {
                write!(f, "{val}")?;
            }
        }
        write!(f, ": {}", s.original(*r#type))
    }
}
