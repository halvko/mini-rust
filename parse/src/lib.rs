use std::collections::HashMap;

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

    #[derive(Debug, PartialEq, Eq)]
    pub struct Block {
        pub stmts: Vec<Statement>,
        pub expr: Option<Expression>,
    }

    #[derive(Debug, PartialEq, Eq)]
    pub enum Statement {
        Let {
            ident: String,
            r#mut: bool,
            r#type: Option<Type>,
            expr: Expression,
        },
        Expression(Expression),
    }

    // FUTURE: Use arena references rather than boxes
    #[derive(Debug, PartialEq, Eq)]
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
    }

    #[derive(Debug, PartialEq, Eq)]
    pub enum Value {
        Int(String),
    }

    #[derive(Debug, PartialEq, Eq)]
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

    pub fn parse_block(block: Pair<Rule>) -> Block {
        debug_assert_eq!(block.as_rule(), Rule::block);
        let mut block = block.into_inner();
        let stmts = parse_stmts(block.next().unwrap());
        let expr = block.next().map(parse_expr);
        Block { stmts, expr }
    }

    fn parse_stmts(stmts: Pair<Rule>) -> Vec<Statement> {
        debug_assert_eq!(stmts.as_rule(), Rule::stmts);
        let mut ret = Vec::new();
        for statement in stmts.into_inner() {
            ret.push(parse_stmt(statement))
        }
        ret
    }

    fn parse_let_stmt(let_stmt: Pair<Rule>) -> Statement {
        debug_assert_eq!(let_stmt.as_rule(), Rule::let_stmt);
        let mut let_stmt = let_stmt.into_inner();
        let r#mut = !let_stmt.next().unwrap().as_str().is_empty();
        let ident = let_stmt.next().unwrap().as_str().to_owned();
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
            _ => unreachable!(),
        }
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
    use super::*;
    use expression::Block;
    use pest::iterators::Pair;

    #[derive(Debug, PartialEq, Eq)]
    pub struct Function {
        pub args: Vec<(Ident, Type)>,
        pub ret_type: Type,
        pub body: Block,
        pub span: Span,
    }

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

    fn parse_fn_args(args: Pair<Rule>) -> Vec<(Ident, Type)> {
        debug_assert_eq!(args.as_rule(), Rule::arg_list);
        let mut ret = Vec::new();
        for arg in args.into_inner() {
            ret.push(parse_arg(arg));
        }
        ret
    }

    fn parse_arg(arg: Pair<Rule>) -> (Ident, Type) {
        debug_assert_eq!(arg.as_rule(), Rule::arg);
        let mut arg = arg.into_inner();
        let name = parse_ident(arg.next().unwrap());
        let r#type = parse_type(arg.next().unwrap());
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
