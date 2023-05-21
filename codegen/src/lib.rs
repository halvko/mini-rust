use std::io;

use symbol_table::{Symbol, SymbolTable};
use typecheck::{Block, Expression, ExpressionKind, Function, Statement, Type, TypedAST};

mod llvm;

fn gen_buildin(
    buildin: (Symbol, &Type),
    st: &SymbolTable<String>,
    o: &mut impl io::Write,
) -> anyhow::Result<()> {
    let Type::Function { args, ret } = buildin.1 else {
        panic!("ICE: non_function buildin")
    };
    write!(
        o,
        "declare {} @{}(",
        type_conv(ret, st),
        st.original(buildin.0)
    )?;
    let mut args = args.iter();
    if let Some(arg) = args.next() {
        write!(o, "{}", type_conv(arg, st))?;
    }
    for arg in args {
        write!(o, ", {}", type_conv(arg, st))?;
    }
    writeln!(o, ")")?;
    Ok(())
}

fn gen_mm_interface(st: &SymbolTable<String>, o: &mut impl io::Write) -> anyhow::Result<()> {
    writeln!(o, "declare void @store_i64(ptr, i64, i64*)")?;
    writeln!(o, "declare void @load_i64(ptr, i64*)")?;
    writeln!(o, "declare void @store_i1(ptr, i1, i1*)")?;
    writeln!(o, "declare void @load_i1(ptr, i1*)")?;
    Ok(())
}

fn type_conv(r#type: &Type, st: &SymbolTable<String>) -> &'static str {
    match r#type {
        Type::Simple(t) => match st.original(*t).as_str() {
            "void" => "void",
            "bool" => "i1",
            "isize" | "usize" => "i64",
            _ => panic!("unknown type"),
        },
        Type::Function { .. } => todo!("Function types"),
    }
}

pub fn gen_ir(ast: TypedAST, out: &mut impl io::Write) -> anyhow::Result<()> {
    let TypedAST { mut st, ast, ss } = ast;
    let mut tmp = Tmp::default();
    writeln!(out, r#"target triple = "x86_64-pc-linux-gnu""#)?;
    for f in ast {
        gen_fn(f, &st, &mut tmp, out)?;
    }

    for (name, r#type) in typecheck::buildins(&mut st, &ss).iter() {
        gen_buildin((*name, r#type), &st, out)?;
    }
    gen_mm_interface(&st, out)?;
    Ok(())
}

#[derive(Default)]
struct Tmp {
    reg: usize,
    r#loop: usize,
}

impl Tmp {
    fn next_reg(&mut self) -> String {
        let ret = format!("%tmp_{}", self.reg);
        self.reg += 1;
        ret
    }
    fn next_label(&mut self) -> String {
        let ret = format!("L{}", self.r#loop);
        self.r#loop += 1;
        ret
    }
}

fn gen_fn(
    f: Function,
    st: &SymbolTable<String>,
    t: &mut Tmp,
    o: &mut impl io::Write,
) -> anyhow::Result<()> {
    let Function {
        name,
        binders,
        ret_type,
        body: Block { stmts, expr, .. },
    } = f;

    if binders.len() > 0 {
        todo!("impl function arguments")
    }
    let name = st.original(name);
    let name = if name.as_str() == "main" {
        "mr_main"
    } else {
        name.as_str()
    };
    writeln!(
        o,
        "define {} @{} (ptr %mm) {{",
        type_conv(&ret_type, st),
        name
    )?;
    for stmt in stmts {
        gen_stmt(stmt, st, t, o, &mut None)?;
    }
    if let Some(e) = expr {
        let ret = gen_expr(*e, st, t, o, &mut None)?;
        writeln!(o, "ret {ret}")?;
    } else {
        writeln!(o, "ret void")?;
    }
    writeln!(o, "}}")?;
    Ok(())
}

fn gen_stmt(
    stmt: Statement,
    st: &SymbolTable<String>,
    t: &mut Tmp,
    o: &mut impl io::Write,
    l: &mut Option<Loop>,
) -> anyhow::Result<()> {
    match stmt {
        Statement::Let {
            ident,
            r#type,
            expr,
        } => {
            let ident: String = format!("%{}", st.original(ident));
            writeln!(o, "{ident} = alloca {}", type_conv(&r#type, st))?;
            let expr = gen_expr(expr, st, t, o, l)?;
            llvm::store(type_conv(&r#type, st), &expr, &ident, o)?;
            Ok(())
        }
        Statement::Expr(e) => {
            gen_expr(e, st, t, o, l)?;
            Ok(())
        }
    }
}

fn gen_expr(
    expr: Expression,
    st: &SymbolTable<String>,
    t: &mut Tmp,
    o: &mut impl io::Write,
    l: &mut Option<Loop>,
) -> anyhow::Result<String> {
    let Expression { r#type, kind } = expr;
    let r#type = type_conv(&r#type, st);
    match kind {
        typecheck::ExpressionKind::BinExp { lhs, op, rhs } => {
            if let typecheck::Infix::Assign = op {
                let (lhs, lhs_type) = {
                    let Expression {  r#type: var_type, kind: ExpressionKind::Var { ident } } = *lhs else {
                        panic!("ICE: Assign to non-varibale expression typechecked")
                    };
                    (ident, var_type)
                };
                let lhs = st.original(lhs);
                let rhs = gen_expr(*rhs, st, t, o, l)?;
                llvm::store(type_conv(&lhs_type, st), &rhs, &lhs, o)?;
                Ok("0".to_owned())
            } else {
                let r#type = type_conv(&lhs.r#type, st);
                let lhs = gen_expr(*lhs, st, t, o, l)?;
                let rhs = gen_expr(*rhs, st, t, o, l)?;

                match op {
                    typecheck::Infix::Add => {
                        let tmp = t.next_reg();
                        writeln!(o, "{tmp} = add {type} {lhs}, {rhs}",)?;
                        Ok(tmp)
                    }
                    typecheck::Infix::Eq => {
                        let tmp = t.next_reg();
                        writeln!(o, "{tmp} = icmp eq {type} {lhs}, {rhs}",)?;
                        Ok(tmp)
                    }
                    typecheck::Infix::Assign => unreachable!(),
                }
            }
        }
        typecheck::ExpressionKind::LoopExp(block) => {
            let cont = t.next_label();
            let brk = t.next_label();
            let break_reg = t.next_reg();
            if r#type != "void" {
                writeln!(o, "{break_reg} = alloca {}", r#type)?;
            }
            writeln!(o, "br label %{cont}")?;
            writeln!(o, "\n{cont}:")?;
            let l = Loop {
                cont_label: cont,
                break_label: brk,
                break_reg,
            };
            let l = &mut Some(l);
            gen_block(block, st, t, o, l)?;
            let Some(Loop {
                cont_label: cont,
                break_label: brk,
                break_reg,
                ..
            }) = l else { panic!("ICE") };
            writeln!(o, "br label %{cont}")?;
            writeln!(o, "\n{brk}:")?;
            let ret = t.next_reg();
            if r#type != "void" {
                llvm::load(r#type, &ret, &break_reg, o)?;
                Ok(ret)
            } else {
                Ok("0".to_owned())
            }
        }
        typecheck::ExpressionKind::Break(e) => {
            if let Some(e) = e {
                let ty = type_conv(&e.r#type, st);
                let e_reg = gen_expr(*e, st, t, o, l)?;
                let Some(l) = l else {
                    panic!("ICE");
                };
                llvm::store(ty, &e_reg, &l.break_reg, o)?;
            };
            let Some(l) = l else {
                panic!("ICE");
            };
            writeln!(o, "br label %{}", l.break_label)?;
            Ok("0".to_owned())
        }
        typecheck::ExpressionKind::IfExp {
            cond,
            block,
            else_block,
        } => {
            let if_label = t.next_label();
            let else_label = t.next_label();
            let finally_label = t.next_label();
            let ret_alloc = t.next_reg();
            let cond = gen_expr(*cond, st, t, o, l)?;
            if r#type != "void" {
                writeln!(o, "{ret_alloc} = alloca {type}")?;
            }
            if else_block.is_some() {
                writeln!(o, "br i1 {cond}, label %{if_label}, label %{else_label}\n")?;
            } else {
                writeln!(
                    o,
                    "br i1 {cond}, label %{if_label}, label %{finally_label}\n"
                )?;
            }
            writeln!(o, "{if_label}:")?;
            let if_ret = gen_block(block, st, t, o, l)?;
            if r#type != "void" {
                llvm::store(r#type, &if_ret, &ret_alloc, o)?;
            }
            writeln!(o, "br label %{finally_label}")?;

            if let Some(else_block) = else_block {
                writeln!(o, "\n{else_label}:")?;
                let else_ret = gen_block(else_block, st, t, o, l)?;
                if r#type != "void" {
                    llvm::store(r#type, &else_ret, &ret_alloc, o)?;
                }
                writeln!(o, "br label %{finally_label}")?;
            }

            writeln!(o, "\n{finally_label}:")?;

            if r#type != "void" {
                let ret = t.next_reg();
                llvm::load(r#type, &ret, &ret_alloc, o)?;
                Ok(ret)
            } else {
                Ok("0".to_owned())
            }
        }
        typecheck::ExpressionKind::Block(b) => gen_block(b, st, t, o, l),
        typecheck::ExpressionKind::Var { ident } => {
            let ret = t.next_reg();
            llvm::load(r#type, &ret, st.original(ident), o)?;
            Ok(ret)
        }
        typecheck::ExpressionKind::Val(v) => match v {
            typecheck::Value::USize(v) => Ok(v.to_string()),
            typecheck::Value::ISize(v) => Ok(v.to_string()),
            typecheck::Value::Bool(b) => match b {
                true => Ok("1".to_owned()),
                false => Ok("0".to_owned()),
            },
        },
        typecheck::ExpressionKind::Call { fn_expr, params } => {
            let mut computed_params = Vec::new();
            for param in params {
                computed_params.push((param.r#type.clone(), gen_expr(param, st, t, o, l)?));
            }
            let typecheck::Expression { kind, .. } = *fn_expr;
            let typecheck::ExpressionKind::Var { ident } = kind else {
                todo!("Computed function calls")
            };
            let reg = if r#type != "void" {
                let reg = t.next_reg();
                write!(o, "{reg} = ")?;
                reg
            } else {
                "0".to_owned()
            };
            write!(o, "call {} @{}(", r#type, st.original(ident))?;
            let mut params = computed_params.iter();
            if let Some((r#type, param)) = params.next() {
                write!(o, "{} {}", type_conv(r#type, st), param)?;
            }
            for (r#type, param) in params {
                write!(o, ", {} {}", type_conv(r#type, st), param)?;
            }
            writeln!(o, ")")?;
            Ok(reg)
        }
    }
}

struct Loop {
    cont_label: String,
    break_label: String,
    break_reg: String,
}

fn gen_block(
    b: Block,
    s: &SymbolTable<String>,
    t: &mut Tmp,
    o: &mut impl io::Write,
    l: &mut Option<Loop>,
) -> anyhow::Result<String> {
    let Block { stmts, expr, .. } = b;
    for stmt in stmts {
        gen_stmt(stmt, s, t, o, l)?;
    }
    if let Some(expr) = expr {
        gen_expr(*expr, s, t, o, l)
    } else {
        Ok("0".to_owned())
    }
}
