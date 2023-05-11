use std::hint::black_box;

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
    let main_body = tc_block(main.body, &mut symbols, &ss, var_ctxt);
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

fn tc_block(
    block: parse::expression::Block,
    symbols: &mut SymbolTable,
    ss: &SpecialSymbols,
    mut var_ctxt: TreeMap<Symbol, Symbol>,
) -> Block {
    let mut typed_stmts = Vec::new();
    for stmt in block.stmts {
        let (stmt, updated_ctxt) = tc_stmt(stmt, symbols, ss, var_ctxt);
        var_ctxt = updated_ctxt;
        typed_stmts.push(stmt);
    }
    let expr = block
        .expr
        .map(|expr| Box::new(tc_expr(expr, symbols, ss, var_ctxt)));

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
) -> (Statement, TreeMap<Symbol, Symbol>) {
    match stmt {
        parse::expression::Statement::Let {
            ident,
            r#mut: _,
            r#type,
            expr,
        } => {
            let ident = symbols.symbol(ident.inner);
            let expr = tc_expr(expr, symbols, ss, var_ctxt.clone());
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
            Statement::Expr(tc_expr(e, symbols, ss, var_ctxt.clone())),
            var_ctxt,
        ),
    }
}

fn tc_expr(
    exp: parse::expression::Expression,
    symbols: &mut SymbolTable,
    ss: &SpecialSymbols,
    var_ctxt: TreeMap<Symbol, Symbol>,
) -> Expression {
    match exp {
        parse::expression::Expression::IfExp {
            cond,
            block,
            else_block,
        } => {
            let cond = tc_expr(*cond, symbols, ss, var_ctxt.clone());
            if cond.r#type != ss.bool {
                panic!("Expected bool, found {}", symbols.original(cond.r#type))
            }
            let block = tc_block(*block, symbols, ss, var_ctxt.clone());
            let else_block = else_block.map(|b| tc_block(*b, symbols, ss, var_ctxt));
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
            let block = tc_block(*block, symbols, ss, var_ctxt);
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
        parse::expression::Expression::Val(parse::expression::Value::Int(s)) => {
            let (val, r#type) = (|v: &str| {
                if let Some(u) = v.strip_suffix("usize") {
                    return (Value::USize(u.parse().unwrap()), ss.usize);
                }
                if let Some(i) = v.strip_suffix("isize") {
                    return (Value::ISize(i.parse().unwrap()), ss.isize);
                }
                panic!("Expected int to end with type");
            })(&s);



            todo!()
        }
        parse::expression::Expression::BinExp(_, _, _) => todo!(),
        parse::expression::Expression::Loop(_) => todo!(),
        parse::expression::Expression::Break(_) => todo!(),
        parse::expression::Expression::Return(_) => todo!(),
        _ => todo!(),
    }
}

pub struct TypedAST {
    pub symbols: SymbolTable,
    pub ast: Vec<Function>,
}

pub struct Program(Vec<Function>);

pub struct Function {
    pub name: Symbol,
    pub binders: Vec<Binder>,
    pub ret_type: Type,
    pub body: Block,
}

pub struct Block {
    pub stmts: Vec<Statement>,
    pub expr: Option<Box<Expression>>,
    pub r#type: Type,
}

pub enum Statement {
    Let {
        ident: Symbol,
        r#type: Type,
        expr: Expression,
    },
    Expr(Expression),
}

pub struct Expression {
    pub r#type: Type,
    pub kind: ExpressionKind,
}

pub enum ExpressionKind {
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
    Block(Block),
    Var {
        ident: Symbol,
    },
    Val(Value),
}

pub enum Infix {
    Add,
}

pub enum Value {
    USize(usize),
    ISize(isize),
}

pub struct Binder {
    pub id: Symbol,
    pub r#type: Symbol,
}
