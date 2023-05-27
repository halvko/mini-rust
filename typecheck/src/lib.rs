use std::{
    fmt::{Display, Write},
    hash::Hash,
};

use immutable_map::TreeMap;
use parse::{AbstractSyntaxTree, Ident};
use symbol_table::{STDisplay, Symbol};

pub fn buildins(st: &mut SymbolTable, ss: &SpecialSymbols) -> VarCtxt {
    VarCtxt::new()
        .insert(
            st.symbol("print".to_owned()),
            Type::Function {
                args: vec![ss.isize.clone()],
                ret: Box::new(ss.void.clone()),
            },
        )
        .insert(
            st.symbol("sleep".to_owned()),
            Type::Function {
                args: vec![ss.usize.clone()],
                ret: Box::new(ss.void.clone()),
            },
        )
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Type {
    Simple(Symbol),
    Function { args: Vec<Type>, ret: Box<Type> },
}
impl PartialEq<Symbol> for Type {
    fn eq(&self, other: &Symbol) -> bool {
        let Self::Simple(s) = self else { return false };
        s == other
    }
}

impl STDisplay for Type {
    fn fmt<SymbolTableType: Eq + Clone + Hash + std::fmt::Display>(
        &self,
        f: &mut std::fmt::Formatter,
        st: &symbol_table::SymbolTable<SymbolTableType>,
    ) -> std::fmt::Result {
        match self {
            Type::Simple(s) => st.original(*s).fmt(f),
            Type::Function { args, ret } => {
                f.write_str("fn(")?;
                let mut args = args.iter();
                if let Some(arg) = args.next() {
                    arg.fmt(f, st)?;
                }
                for arg in args {
                    f.write_str(", ")?;
                    arg.fmt(f, st)?;
                }
                f.write_str(") -> ")?;
                ret.fmt(f, st)
            }
        }
    }
}

type SymbolTable = symbol_table::SymbolTable<String>;

type VarCtxt = TreeMap<Symbol, Type>;

#[derive(Debug)]
pub struct SpecialSymbols {
    bool: Type,
    void: Type,
    isize: Type,
    usize: Type,
    main: Symbol,
}

impl SpecialSymbols {
    fn new(symbols: &mut SymbolTable) -> Self {
        Self {
            bool: Type::Simple(symbols.symbol("bool".to_owned())),
            void: Type::Simple(symbols.symbol("void".to_owned())),
            isize: Type::Simple(symbols.symbol("isize".to_owned())),
            usize: Type::Simple(symbols.symbol("usize".to_owned())),
            main: symbols.symbol("main".to_owned()),
        }
    }
}

pub fn type_check(mut ast: AbstractSyntaxTree) -> TypedAST {
    let mut st = SymbolTable::new();
    let ss = SpecialSymbols::new(&mut st);
    let var_ctxt = buildins(&mut st, &ss);
    if ast.globals.len() != 1 {
        panic!("only main function will be analyzed (and functions are not otherwise supported)")
    }
    let main = ast
        .globals
        .remove(&Ident {
            inner: "main".to_owned(),
        })
        .unwrap();
    if !main.args.is_empty() {
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
    let main_body = tc_block(main.body, &mut st, &ss, var_ctxt, &mut Loop::No);
    if main_body.r#type != ss.void.clone() {
        panic!(
            "from main body, expected void got {}",
            st.displayable(&main_body.r#type)
        )
    }
    TypedAST {
        st,
        ast: vec![Function {
            name: ss.main,
            binders: Vec::new(),
            ret_type: ss.void.clone(),
            body: main_body,
        }],
        ss,
    }
}

#[derive(Clone)]
enum Loop {
    No,
    Yes { r#type: Option<Type> },
}

fn tc_block(
    block: parse::expression::Block,
    st: &mut SymbolTable,
    ss: &SpecialSymbols,
    mut var_ctxt: VarCtxt,
    r#loop: &mut Loop,
) -> Block {
    let mut typed_stmts = Vec::new();
    for stmt in block.stmts {
        let (stmt, updated_ctxt) = tc_stmt(stmt, st, ss, var_ctxt, r#loop);
        var_ctxt = updated_ctxt;
        typed_stmts.push(stmt);
    }
    let expr = block
        .expr
        .map(|expr| Box::new(tc_expr(expr, st, ss, var_ctxt, r#loop)));

    let r#type = expr
        .as_ref()
        .map(|e| e.r#type.clone())
        .unwrap_or(ss.void.clone());
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
    var_ctxt: VarCtxt,
    r#loop: &mut Loop,
) -> (Statement, VarCtxt) {
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
                .map(|t| Type::Simple(symbols.symbol(t.name.inner)))
                .unwrap_or(expr.r#type.clone());
            if r#type != expr.r#type {
                panic!(
                    "Expected {}, found {}",
                    symbols.displayable(&r#type),
                    symbols.displayable(&expr.r#type)
                )
            }
            (
                Statement::Let {
                    ident,
                    r#type: r#type.clone(),
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
    st: &mut SymbolTable,
    ss: &SpecialSymbols,
    var_ctxt: VarCtxt,
    r#loop: &mut Loop,
) -> Expression {
    match exp {
        parse::expression::Expression::IfExp {
            cond,
            block,
            else_block,
        } => {
            let cond = tc_expr(*cond, st, ss, var_ctxt.clone(), r#loop);
            if cond.r#type != ss.bool {
                panic!("Expected bool, found {}", st.displayable(&cond.r#type))
            }
            let block = tc_block(*block, st, ss, var_ctxt.clone(), r#loop);
            let else_block = else_block.map(|b| tc_block(*b, st, ss, var_ctxt, r#loop));
            if let Some(else_block) = &else_block {
                if block.r#type != else_block.r#type {
                    panic!("if {{...}} else {{...}} expression has mismatching types: if -> {}, else -> {}", st.displayable(&block.r#type), st.displayable(&else_block.r#type))
                }
            } else if block.r#type != ss.void {
                panic!(
                    "if {{...}} with no else returns {}, expected void",
                    st.displayable(&block.r#type)
                )
            }

            Expression {
                r#type: block.r#type.clone(),
                kind: ExpressionKind::IfExp {
                    cond: Box::new(cond),
                    block,
                    else_block,
                },
            }
        }
        parse::expression::Expression::BlockExp(block) => {
            let block = tc_block(*block, st, ss, var_ctxt, r#loop);
            Expression {
                r#type: block.r#type.clone(),
                kind: ExpressionKind::Block(block),
            }
        }
        parse::expression::Expression::Ident(ident) => {
            let ident = st.symbol(ident.inner);
            let Some(r#type) = var_ctxt.get(&ident) else {
                panic!("use of undeclared variable {}", st.original(ident))
            };
            Expression {
                r#type: r#type.clone(),
                kind: ExpressionKind::Var { ident },
            }
        }
        parse::expression::Expression::Val(v) => match v {
            parse::expression::Value::Int(s) => {
                let (val, r#type) = (|v: &str| {
                    if let Some(u) = v.strip_suffix("usize") {
                        return (Value::USize(u.parse().unwrap()), ss.usize.clone());
                    }
                    if let Some(i) = v.strip_suffix("isize") {
                        return (Value::ISize(i.parse().unwrap()), ss.isize.clone());
                    }
                    (Value::ISize(v.parse().unwrap()), ss.isize.clone())
                })(&s);

                Expression {
                    r#type,
                    kind: ExpressionKind::Val(val),
                }
            }
            parse::expression::Value::Bool(b) => Expression {
                r#type: ss.bool.clone(),
                kind: ExpressionKind::Val(Value::Bool(b)),
            },
        },
        parse::expression::Expression::BinExp(lhs, ifx, rhs) => {
            let lhs = tc_expr(*lhs, st, ss, var_ctxt.clone(), r#loop);
            let rhs = tc_expr(*rhs, st, ss, var_ctxt, r#loop);
            if lhs.r#type != rhs.r#type {
                panic!("Trying to do operation between different types")
            }
            let (infix, r#type) = match ifx {
                parse::expression::Infix::Add => {
                    if lhs.r#type != ss.isize && lhs.r#type != ss.usize {
                        panic!("Trying to add someting which is not a number")
                    }
                    (Infix::Add, lhs.r#type.clone())
                }
                parse::expression::Infix::Eq => (Infix::Eq, ss.bool.clone()),
                parse::expression::Infix::Assign => {
                    if let ExpressionKind::Var { .. } = lhs.kind {
                        (Infix::Assign, ss.void.clone())
                    } else {
                        panic!("trying to assign to general expression")
                    }
                }
                _ => todo!("Properly fix operators -.-"),
            };
            Expression {
                r#type,
                kind: ExpressionKind::BinExp {
                    lhs: Box::new(lhs),
                    op: infix,
                    rhs: Box::new(rhs),
                },
            }
        }
        parse::expression::Expression::Loop(block) => {
            let mut r#loop = Loop::Yes { r#type: None };
            let b = tc_block(*block, st, ss, var_ctxt, &mut r#loop);
            let Loop::Yes{r#type} = r#loop else {panic!("ICE 0: Loop is not inside loop?")};
            Expression {
                r#type: r#type.unwrap_or(ss.void.clone()),
                kind: ExpressionKind::LoopExp(b),
            }
        }
        parse::expression::Expression::Break(e) => {
            let e = e.map(|e| tc_expr(*e, st, ss, var_ctxt, r#loop));
            let Loop::Yes { r#type } = r#loop else { panic!("Use of break outside loop")};
            let e_type = e
                .as_ref()
                .map(|b| b.r#type.clone())
                .unwrap_or(ss.void.clone());
            if let Some(t) = r#type {
                if *t != e_type {
                    panic!("Incompatible break statements in loop, first had type {}, second had type {}", st.displayable(t), st.displayable(&e_type))
                }
            } else {
                *r#type = Some(e_type)
            }
            Expression {
                r#type: ss.void.clone(),
                kind: ExpressionKind::Break(e.map(Box::new)),
            }
        }
        parse::expression::Expression::CallExp(c) => {
            let expr = tc_expr(*c.expr, st, ss, var_ctxt.clone(), r#loop);
            let call_args: Vec<_> = c
                .args
                .into_iter()
                .map(|e| tc_expr(e, st, ss, var_ctxt.clone(), r#loop))
                .collect();
            let Type::Function { args, ret } = expr.r#type.clone() else {
                panic!("trying to call non-function expression {}", st.displayable(&expr))
            };
            if call_args.len() != args.len() {
                panic!(
                    "trying to call function with {} arguments, expected {} arguments",
                    call_args.len(),
                    args.len()
                )
            }
            for (i, (arg_type, expected_type)) in call_args
                .iter()
                .map(|a| a.r#type.clone())
                .zip(args.iter())
                .enumerate()
            {
                if arg_type != *expected_type {
                    panic!(
                        "argument {i} was parameterised with {}, but expected something of type {}",
                        st.displayable(&call_args[i]),
                        st.displayable(expected_type)
                    )
                }
            }
            Expression {
                r#type: *ret,
                kind: ExpressionKind::Call {
                    fn_expr: Box::new(expr),
                    params: call_args,
                },
            }
        }
        parse::expression::Expression::Return(_) => todo!("functions"),
    }
}

#[derive(Debug)]
pub struct TypedAST {
    pub st: SymbolTable,
    pub ast: Vec<Function>,
    pub ss: SpecialSymbols,
}

impl Display for TypedAST {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for fun in self.ast.iter() {
            writeln!(f, "{}", self.st.displayable(fun))?;
        }
        Ok(())
    }
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

impl STDisplay for Function {
    fn fmt<SymbolTableType: Eq + Clone + Hash + std::fmt::Display>(
        &self,
        f: &mut std::fmt::Formatter,
        st: &symbol_table::SymbolTable<SymbolTableType>,
    ) -> std::fmt::Result {
        write!(f, "fn {}(", st.original(self.name))?;
        let mut binders = self.binders.iter();
        if let Some(binder) = binders.next() {
            st.displayable(binder).fmt(f)?;
        }
        for binder in binders {
            write!(f, ", {}", st.displayable(binder))?;
        }
        write!(
            f,
            ") -> {} {}",
            st.displayable(&self.ret_type),
            st.displayable(&self.body)
        )
    }
}

#[derive(Debug)]
pub struct Block {
    pub stmts: Vec<Statement>,
    pub expr: Option<Box<Expression>>,
    pub r#type: Type,
}

impl STDisplay for Block {
    fn fmt<SymbolTableType: Eq + Clone + Hash + std::fmt::Display>(
        &self,
        f: &mut std::fmt::Formatter,
        st: &symbol_table::SymbolTable<SymbolTableType>,
    ) -> std::fmt::Result {
        let Block {
            stmts,
            expr,
            r#type,
        } = self;
        write!(f, "{{ ")?;
        for stmt in stmts {
            write!(f, "{}; ", st.displayable(stmt))?;
        }
        if let Some(expr) = expr {
            write!(f, "{} ", st.displayable(expr.as_ref()))?;
        }
        write!(f, "}}: {}", st.displayable(r#type))
    }
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

impl STDisplay for Statement {
    fn fmt<SymbolTableType: Eq + Clone + Hash + std::fmt::Display>(
        &self,
        f: &mut std::fmt::Formatter,
        st: &symbol_table::SymbolTable<SymbolTableType>,
    ) -> std::fmt::Result {
        match self {
            Statement::Let {
                ident,
                r#type,
                expr,
            } => {
                write!(
                    f,
                    "let {}: {} = {}",
                    st.original(*ident),
                    st.displayable(r#type),
                    st.displayable(expr)
                )
            }
            Statement::Expr(expr) => st.displayable(expr).fmt(f),
        }
    }
}

#[derive(Debug)]
pub struct Expression {
    pub r#type: Type,
    pub kind: ExpressionKind,
}

impl STDisplay for Expression {
    fn fmt<SymbolTableType: Eq + Clone + Hash + std::fmt::Display>(
        &self,
        f: &mut std::fmt::Formatter,
        st: &symbol_table::SymbolTable<SymbolTableType>,
    ) -> std::fmt::Result {
        let Expression { r#type, kind } = self;
        write!(f, "{}: {}", st.displayable(kind), st.displayable(r#type))
    }
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
    Call {
        fn_expr: Box<Expression>,
        params: Vec<Expression>,
    },
    Val(Value),
}

impl STDisplay for ExpressionKind {
    fn fmt<SymbolTableType: Eq + Clone + Hash + std::fmt::Display>(
        &self,
        f: &mut std::fmt::Formatter,
        st: &symbol_table::SymbolTable<SymbolTableType>,
    ) -> std::fmt::Result {
        match self {
            ExpressionKind::BinExp { lhs, op, rhs } => {
                write!(
                    f,
                    "({} {} {})",
                    st.displayable(lhs.as_ref()),
                    op,
                    st.displayable(rhs.as_ref())
                )
            }
            ExpressionKind::LoopExp(b) => {
                write!(f, "loop {}", st.displayable(b))
            }
            ExpressionKind::Break(e) => {
                f.write_str("break")?;
                if let Some(e) = e {
                    write!(f, " {}", st.displayable(e.as_ref()))?;
                }
                Ok(())
            }
            ExpressionKind::IfExp {
                cond,
                block,
                else_block,
            } => {
                write!(
                    f,
                    "if {} {}",
                    st.displayable(cond.as_ref()),
                    st.displayable(block)
                )?;
                if let Some(else_block) = else_block {
                    write!(f, " else {}", st.displayable(else_block))?;
                }
                Ok(())
            }
            ExpressionKind::Block(b) => st.displayable(b).fmt(f),
            ExpressionKind::Var { ident } => st.original(*ident).fmt(f),
            ExpressionKind::Val(val) => val.fmt(f),
            ExpressionKind::Call {
                fn_expr: expression,
                params,
            } => {
                write!(f, "{}(", st.displayable(expression.as_ref()))?;
                let mut params = params.iter();
                if let Some(param) = params.next() {
                    st.displayable(param).fmt(f)?;
                }
                for param in params {
                    write!(f, ", {}", st.displayable(param))?;
                }
                write!(f, ")")
            }
        }
    }
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
    pub r#type: Type,
}

impl STDisplay for Binder {
    fn fmt<SymbolTableType: Eq + Clone + Hash + std::fmt::Display>(
        &self,
        f: &mut std::fmt::Formatter,
        st: &symbol_table::SymbolTable<SymbolTableType>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{}: {}",
            st.original(self.id),
            st.displayable(&self.r#type)
        )
    }
}
