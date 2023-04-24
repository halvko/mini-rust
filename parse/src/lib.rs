use std::{
    collections::HashMap,
    fmt::{Display, Write},
};

use pest::{iterators::Pair, Parser};
use pest_derive::Parser;

const KEYWORDS: [&str; 3] = ["fn", "let", "mut"];

#[derive(Parser)]
#[grammar = "./grammar.pest"]
struct Grammar;

#[derive(Debug, PartialEq, Eq)]
pub struct AbstractSyntaxTree {
    pub globals: HashMap<Ident, function::Function>,
}

impl std::fmt::Display for AbstractSyntaxTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut inner: Vec<_> = self.globals.iter().collect();
        inner.sort_by_key(|(ident, _)| &ident.inner);
        let mut inner = inner.into_iter();
        if let Some((ident, func)) = inner.next() {
            write!(f, "fn {ident}{func}")?;
        }
        for (ident, func) in inner {
            write!(f, "\nfn {ident}{func}")?;
        }
        Ok(())
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Debug)]
pub struct Ident {
    pub inner: String,
}

impl std::fmt::Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.inner)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

impl<'a> From<pest::Span<'a>> for Span {
    fn from(value: pest::Span<'a>) -> Self {
        Self {
            start: value.start_pos().into(),
            end: value.end_pos().into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Position {
    pub byte: usize,
    pub line: usize,
    pub col: usize,
}

impl<'a> From<pest::Position<'a>> for Position {
    fn from(value: pest::Position<'a>) -> Self {
        Self {
            byte: value.pos(),
            line: value.line_col().0,
            col: value.line_col().1,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type {
    pub name: Ident,
}

impl Type {
    pub fn as_str(&self) -> &str {
        &self.name.inner
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.name.fmt(f)
    }
}

#[derive(Debug)]
pub enum ASTGenError {
    GlobalNameCollision(GlobalNameCollision),
}

#[derive(Debug)]
pub struct GlobalNameCollision {
    pub path: Ident,
    pub locs: (Position, Position),
}

// FUTURE: return (AbstractSyntaxTree, Vec<AstGenError>) containing a best effort construction of a AST and the errors encountered
pub fn gen_ast(input: &str) -> Result<AbstractSyntaxTree, ASTGenError> {
    let mut parsed = Grammar::parse(Rule::program, input).unwrap();

    // Extract the actual program (the program rule just requires that there is nothing but valid
    // syntax in the file, the actual program is in the inner rule)
    let parsed = parsed
        .next()
        .unwrap()
        .into_inner()
        .next()
        .unwrap()
        .into_inner();

    let mut globals = HashMap::new();
    for r in parsed {
        let (path, function) = function::parse(r);

        if let Some(previous) = globals.insert(path.clone(), function).map(|f| f.span.start) {
            // A global of that path already existed, return error
            let new = globals
                .remove(&path)
                .unwrap() //Just inserted path, so newer crashes
                .span
                .start;
            return Err(ASTGenError::GlobalNameCollision(GlobalNameCollision {
                path,
                locs: (previous, new),
            }));
        }
    }
    Ok(AbstractSyntaxTree { globals })
}

fn parse_type(r#type: Pair<Rule>) -> Type {
    debug_assert_eq!(r#type.as_rule(), Rule::r#type);
    Type {
        name: parse_ident(r#type.into_inner().next().unwrap()),
    }
}

fn parse_ident(ident: Pair<Rule>) -> Ident {
    debug_assert_eq!(ident.as_rule(), Rule::ident);
    if KEYWORDS.contains(&ident.as_str()) {
        panic!(
            "Use of keyword \"{}\" as identifier, on line {}",
            ident.as_str(),
            ident.as_span().start_pos().line_col().0
        );
    }
    Ident {
        inner: ident.as_str().to_owned(),
    }
}

pub mod expression {
    use super::*;
    use pest::iterators::Pair;

    #[derive(Debug, PartialEq, Eq, Clone)]
    pub struct Block {
        pub stmts: Vec<Statement>,
        pub expr: Option<Expression>,
    }

    impl Display for Block {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.write_char('{')?;
            for s in &self.stmts {
                write!(f, " {s}; ")?
            }
            if let Some(e) = &self.expr {
                write!(f, "{e} ")?
            }
            f.write_char('}')
        }
    }

    #[derive(Debug, PartialEq, Eq, Clone)]
    pub enum Statement {
        Let {
            ident: Ident,
            r#mut: bool,
            r#type: Option<Type>,
            expr: Expression,
        },
        Expression(Expression),
    }

    impl Display for Statement {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Self::Let {
                    ident,
                    r#mut,
                    r#type,
                    expr,
                } => {
                    f.write_str("let ")?;
                    if *r#mut {
                        f.write_str("mut ")?
                    }
                    ident.fmt(f)?;
                    if let Some(t) = r#type {
                        write!(f, ": {t} ")?
                    }
                    write!(f, " = {expr}")
                }
                Self::Expression(e) => e.fmt(f),
            }
        }
    }

    // FUTURE: Use arena references rather than boxes
    #[derive(Debug, PartialEq, Eq, Clone)]
    pub enum Expression {
        IfExp {
            cond: Box<Expression>,
            block: Box<Block>,
            else_block: Option<Box<Block>>,
        },
        BlockExp(Box<Block>),
        Ident(Ident),
        Val(Value),
        BinExp(Box<Expression>, Infix, Box<Expression>),
        CallExp(CallExp),
        Loop(Box<Block>),
        Break(Option<Box<Expression>>),
        Return(Option<Box<Expression>>),
    }

    impl Display for Expression {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Expression::IfExp {
                    cond,
                    block,
                    else_block,
                } => {
                    write!(f, "if {cond} {block}")?;
                    if let Some(else_block) = else_block {
                        write!(f, " else {else_block}")?
                    }
                    Ok(())
                }
                Expression::BlockExp(b) => b.fmt(f),
                Expression::Ident(i) => i.fmt(f),
                Expression::Val(v) => v.fmt(f),
                Expression::BinExp(lhs, op, rhs) => write!(f, "({lhs}) {op} ({rhs})"),
                Expression::CallExp(c) => c.fmt(f),
                Self::Loop(b) => write!(f, "loop {b}"),
                Self::Break(b) => {
                    f.write_str("break")?;
                    if let Some(b) = b {
                        write!(f, " {b}")?
                    }
                    Ok(())
                }
                Expression::Return(r) => {
                    f.write_str("return")?;
                    if let Some(r) = r {
                        write!(f, " {r}")?
                    }
                    Ok(())
                }
            }
        }
    }

    #[derive(Debug, PartialEq, Eq, Clone)]
    pub struct CallExp {
        pub ident: Ident,
        pub args: Vec<Expression>,
    }

    impl Display for CallExp {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            self.ident.fmt(f)?;
            f.write_char('(')?;
            let mut args = self.args.iter();
            if let Some(first_arg) = args.next() {
                first_arg.fmt(f)?
            }
            for arg in args {
                write!(f, ", {arg}")?
            }
            f.write_char(')')
        }
    }

    #[derive(Debug, PartialEq, Eq, Clone)]
    pub enum Value {
        Int(String),
    }

    impl Display for Value {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Self::Int(s) => f.write_str(s),
            }
        }
    }

    #[derive(Debug, PartialEq, Eq, Clone, Copy)]
    pub enum Infix {
        Add,
        Sub,
        Mul,
        Div,
        Mod,
        Eq,
        Lt,
        Gt,
        Leq,
        Geq,
        Neq,
        Assign,
    }

    impl Display for Infix {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Infix::Add => f.write_char('+'),
                Infix::Sub => f.write_char('-'),
                Infix::Mul => f.write_char('*'),
                Infix::Div => f.write_char('/'),
                Infix::Mod => f.write_char('%'),
                Infix::Eq => f.write_str("=="),
                Infix::Lt => f.write_char('<'),
                Infix::Gt => f.write_char('>'),
                Infix::Leq => f.write_str("<="),
                Infix::Geq => f.write_str(">="),
                Infix::Neq => f.write_str("!="),
                Infix::Assign => f.write_char('='),
            }
        }
    }

    pub fn parse_block(block: Pair<Rule>) -> Block {
        debug_assert_eq!(block.as_rule(), Rule::block);
        let mut block = block.into_inner();
        let stmts = parse_stmts(block.next().unwrap());
        let expr = block.next().map(parse_expr);
        Block { stmts, expr }
    }

    fn parse_stmts(stmts: Pair<Rule>) -> Vec<Statement> {
        debug_assert_eq!(stmts.as_rule(), Rule::stmts);
        stmts.into_inner().map(parse_stmt).collect()
    }

    fn parse_let_stmt(let_stmt: Pair<Rule>) -> Statement {
        debug_assert_eq!(let_stmt.as_rule(), Rule::let_stmt);
        let mut let_stmt = let_stmt.into_inner();
        let r#mut = !let_stmt.next().unwrap().as_str().is_empty();
        let ident = parse_ident(let_stmt.next().unwrap());
        let r#type = parse_let_type(let_stmt.next().unwrap());
        let expr = parse_expr(let_stmt.next().unwrap());
        Statement::Let {
            ident,
            r#mut,
            r#type,
            expr,
        }
    }

    fn parse_let_type(let_type: Pair<Rule>) -> Option<Type> {
        debug_assert_eq!(let_type.as_rule(), Rule::let_type);
        let_type.into_inner().next().map(parse_type)
    }

    fn parse_stmt(stmt: Pair<Rule>) -> Statement {
        debug_assert_eq!(stmt.as_rule(), Rule::stmt);
        let stmt = stmt.into_inner().next().unwrap();
        match stmt.as_rule() {
            Rule::let_stmt => parse_let_stmt(stmt),
            Rule::expr => Statement::Expression(parse_expr(stmt)),
            _ => unreachable!(),
        }
    }

    fn parse_expr(expr: Pair<Rule>) -> Expression {
        debug_assert_eq!(expr.as_rule(), Rule::expr);
        let mut expr = expr.into_inner();
        // TODO: Find way to parse prefixes to an expr (fx. !b)
        let lhs = parse_s_expr(expr.next().unwrap());
        let Some(infix) = expr.next() else {
            return lhs
        };
        let infix = parse_infix(infix);
        let rhs = parse_s_expr(expr.next().unwrap());
        Expression::BinExp(Box::new(lhs), infix, Box::new(rhs))
    }

    fn parse_infix(infix: Pair<Rule>) -> Infix {
        debug_assert_eq!(infix.as_rule(), Rule::infix);
        match infix.into_inner().next().unwrap().as_rule() {
            Rule::add => Infix::Add,
            Rule::sub => Infix::Sub,
            Rule::mul => Infix::Mul,
            Rule::div => Infix::Div,
            Rule::r#mod => Infix::Mod,
            Rule::eq => Infix::Eq,
            Rule::neq => Infix::Neq,
            Rule::lt => Infix::Lt,
            Rule::gt => Infix::Gt,
            Rule::lte => Infix::Leq,
            Rule::gte => Infix::Geq,
            Rule::assign => Infix::Assign,
            _ => unreachable!(),
        }
    }

    fn parse_s_expr(s_expr: Pair<Rule>) -> Expression {
        debug_assert_eq!(s_expr.as_rule(), Rule::s_expr);
        let s_expr = s_expr.into_inner().next().unwrap();
        match s_expr.as_rule() {
            Rule::r#if => parse_if(s_expr),
            Rule::expr => parse_expr(s_expr),
            Rule::block => Expression::BlockExp(Box::new(parse_block(s_expr))),
            Rule::ident => Expression::Ident(parse_ident(s_expr)),
            Rule::value => Expression::Val(parse_value(s_expr)),
            Rule::call => Expression::CallExp(parse_call(s_expr)),
            Rule::r#loop => Expression::Loop(parse_loop(s_expr)),
            Rule::r#break => Expression::Break(parse_break(s_expr)),
            Rule::ret => Expression::Return(parse_ret(s_expr)),
            _ => unreachable!(),
        }
    }

    fn parse_call(call: Pair<Rule>) -> CallExp {
        debug_assert_eq!(call.as_rule(), Rule::call);
        let mut call = call.into_inner();
        let ident = parse_ident(call.next().unwrap());
        let args = parse_arg_list(call.next().unwrap());
        CallExp { ident, args }
    }

    fn parse_arg_list(arg_list: Pair<Rule>) -> Vec<Expression> {
        debug_assert_eq!(arg_list.as_rule(), Rule::arg_list);
        arg_list.into_inner().map(parse_expr).collect()
    }

    fn parse_loop(r#loop: Pair<Rule>) -> Box<Block> {
        debug_assert_eq!(r#loop.as_rule(), Rule::r#loop);
        Box::new(parse_block(r#loop.into_inner().next().unwrap()))
    }

    fn parse_break(r#break: Pair<Rule>) -> Option<Box<Expression>> {
        debug_assert_eq!(r#break.as_rule(), Rule::r#break);
        r#break.into_inner().next().map(parse_expr).map(Box::new)
    }

    fn parse_ret(ret: Pair<Rule>) -> Option<Box<Expression>> {
        debug_assert_eq!(ret.as_rule(), Rule::ret);
        ret.into_inner().next().map(parse_expr).map(Box::new)
    }

    fn parse_if(r#if: Pair<Rule>) -> Expression {
        debug_assert_eq!(r#if.as_rule(), Rule::r#if);
        let mut if_expr = r#if.into_inner();
        let cond = Box::new(parse_expr(if_expr.next().unwrap()));
        let block = Box::new(parse_block(if_expr.next().unwrap()));
        let r#else = parse_else(if_expr.next().unwrap()).map(Box::new);
        Expression::IfExp {
            cond,
            block,
            else_block: r#else,
        }
    }

    fn parse_else(r#else: Pair<Rule>) -> Option<Block> {
        debug_assert_eq!(r#else.as_rule(), Rule::r#else);
        r#else.into_inner().next().map(parse_block)
    }

    fn parse_value(value: Pair<Rule>) -> Value {
        debug_assert_eq!(value.as_rule(), Rule::value);
        let value = value.into_inner().next().unwrap();
        match value.as_rule() {
            Rule::int => Value::Int(value.as_str().to_owned()),
            _ => unreachable!(),
        }
    }
}

pub mod function {
    use std::fmt::Display;

    use super::*;
    use expression::Block;
    use pest::iterators::Pair;

    #[derive(Debug, Clone)]
    pub struct Function {
        pub args: Vec<(Ident, Type)>,
        pub ret_type: Type,
        pub body: Block,

        pub span: Span,
    }

    impl Display for Function {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.write_char('(')?;
            let mut args = self.args.iter();
            if let Some((ident, r#type)) = args.next() {
                write!(f, "{ident}: {type}")?;
            }
            for (ident, r#type) in args {
                write!(f, ", {ident}: {type}")?;
            }
            let ret = &self.ret_type;
            let block = &self.body;
            write!(f, ") -> {ret} {block}")
        }
    }

    impl PartialEq for Function {
        fn eq(&self, other: &Self) -> bool {
            self.args == other.args && self.ret_type == other.ret_type && self.body == other.body
        }
    }

    impl Eq for Function {}

    pub fn parse(function: Pair<Rule>) -> (Ident, Function) {
        debug_assert_eq!(function.as_rule(), Rule::r#fn);
        let span: Span = function.as_span().into();
        let mut tokens = function.into_inner();
        let path = Ident {
            inner: tokens.next().unwrap().as_str().to_owned(),
        };
        let args = parse_fn_args(tokens.next().unwrap());
        let ret_type = parse_fn_ret_type(tokens.next().unwrap());
        let body = expression::parse_block(tokens.next().unwrap());
        (
            path,
            Function {
                args,
                ret_type,
                body,
                span,
            },
        )
    }

    fn parse_fn_args(params: Pair<Rule>) -> Vec<(Ident, Type)> {
        debug_assert_eq!(params.as_rule(), Rule::param_list);
        params.into_inner().map(parse_param).collect()
    }

    fn parse_param(param: Pair<Rule>) -> (Ident, Type) {
        debug_assert_eq!(param.as_rule(), Rule::param);
        let mut param = param.into_inner();
        let name = parse_ident(param.next().unwrap());
        let r#type = parse_type(param.next().unwrap());
        (name, r#type)
    }

    fn parse_fn_ret_type(ret_type: Pair<Rule>) -> Type {
        debug_assert_eq!(ret_type.as_rule(), Rule::fn_ret);
        let mut ret_type = ret_type.into_inner();
        if let Some(r#type) = ret_type.next() {
            parse_type(r#type)
        } else {
            // FUTURE: Add support for (empty) product type
            Type {
                name: Ident {
                    inner: "void".to_owned(),
                },
            }
        }
    }
}
