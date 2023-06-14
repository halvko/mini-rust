use std::io;

use llvm::{CowReg, Label, Reg};
use symbol_table::{Symbol, SymbolTable};
use typecheck::{Block, Expression, ExpressionKind, Function, Statement, Type, TypedAST};

mod llvm;

#[derive(Clone, Copy)]
pub struct Options<'a> {
    pub memory_indirection: bool,
    pub target: &'a str,
}

pub fn gen_ir(ast: TypedAST, out: &mut impl io::Write, options: Options) -> anyhow::Result<()> {
    let TypedAST { mut st, ast, ss } = ast;
    let mut tmp = Tmp::default();

    let gen = llvm::LLVM::from(&options);

    gen.initialize_target(out)?;

    for f in ast {
        gen_fn(f, &st, &mut tmp, out, &gen)?;
    }

    for (name, r#type) in typecheck::buildins(&mut st, &ss).iter() {
        gen_buildin((*name, r#type), &st, out)?;
    }
    gen_mm_interface(&st, out)?;
    Ok(())
}

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
        "declare {} @{}(ptr",
        type_conv(ret, st),
        st.original(buildin.0)
    )?;
    for arg in args.iter() {
        write!(o, ", {}", type_conv(arg, st))?;
    }
    writeln!(o, ")")?;
    Ok(())
}

fn gen_mm_interface(_st: &SymbolTable<String>, o: &mut impl io::Write) -> anyhow::Result<()> {
    writeln!(o, "declare void @store_i64(ptr, i64, i64*)")?;
    writeln!(o, "declare i64 @load_i64(ptr, i64*)")?;
    writeln!(o, "declare void @store_i1(ptr, i1, i1*)")?;
    writeln!(o, "declare i1 @load_i1(ptr, i1*)")?;
    Ok(())
}

fn type_conv(r#type: &Type, st: &SymbolTable<String>) -> llvm::CowType<'static> {
    match r#type {
        Type::Simple(t) => match st.original(*t).as_str() {
            "void" => llvm::Type::from_static("void").into(),
            "bool" => llvm::Type::from_static("i1").into(),
            "isize" | "usize" => llvm::Type::from_static("i64").into(),
            _ => panic!("unknown type"),
        },
        Type::Function { .. } => todo!("Function types"),
    }
}

#[derive(Default)]
struct Tmp {
    reg: usize,
    r#loop: usize,
}

impl Tmp {
    fn next_reg(&mut self) -> llvm::Reg {
        let reg = format!("%tmp_{}", self.reg);
        self.reg += 1;
        llvm::Reg::from_reg_string(reg)
    }
    fn next_label(&mut self) -> llvm::Label {
        let label = format!("L{}", self.r#loop);
        self.r#loop += 1;
        llvm::Label::from_name(label)
    }
}

fn gen_fn(
    f: Function,
    st: &SymbolTable<String>,
    t: &mut Tmp,
    o: &mut impl io::Write,
    gen: &llvm::LLVM,
) -> anyhow::Result<()> {
    let Function {
        name,
        binders,
        ret_type,
        body: Block { stmts, expr, .. },
    } = f;

    if !binders.is_empty() {
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
        gen_stmt(stmt, gen, st, t, o, &mut None)?;
    }
    if let Some(e) = expr {
        let ret = gen_expr(*e, gen, st, t, o, &mut None)?;
        writeln!(o, "ret {ret}")?;
    } else {
        writeln!(o, "ret void")?;
    }
    writeln!(o, "}}")?;
    Ok(())
}

fn gen_stmt(
    stmt: Statement,
    gen: &llvm::LLVM,
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
            let ident = llvm::Reg::from_plain(st.original(ident));
            writeln!(o, "{ident} = alloca {}", type_conv(&r#type, st))?;
            let expr = gen_expr(expr, gen, st, t, o, l)?;
            gen.store(type_conv(&r#type, st), expr.as_ref(), ident.as_ref(), o)?;
            Ok(())
        }
        Statement::Expr(e) => {
            gen_expr(e, gen, st, t, o, l)?;
            Ok(())
        }
    }
}

fn gen_expr(
    expr: Expression,
    gen: &llvm::LLVM,
    st: &SymbolTable<String>,
    t: &mut Tmp,
    o: &mut impl io::Write,
    l: &mut Option<Loop>,
) -> anyhow::Result<llvm::CowReg<'static>> {
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
                let rhs = gen_expr(*rhs, gen, st, t, o, l)?;
                gen.store(
                    type_conv(&lhs_type, st),
                    rhs.as_ref(),
                    llvm::Reg::from_plain(lhs).as_ref(),
                    o,
                )?;
                Ok(llvm::Reg::zero().into())
            } else {
                let r#type = type_conv(&lhs.r#type, st);
                let lhs = gen_expr(*lhs, gen, st, t, o, l)?;
                let rhs = gen_expr(*rhs, gen, st, t, o, l)?;

                match op {
                    typecheck::Infix::Add => {
                        let tmp = t.next_reg();
                        writeln!(o, "{tmp} = add {type} {lhs}, {rhs}",)?;
                        Ok(tmp.into())
                    }
                    typecheck::Infix::Eq => {
                        let tmp = t.next_reg();
                        writeln!(o, "{tmp} = icmp eq {type} {lhs}, {rhs}",)?;
                        Ok(tmp.into())
                    }
                    typecheck::Infix::Gt => {
                        let tmp = t.next_reg();
                        writeln!(
                            o,
                            "{tmp} = icmp {} {type} {lhs}, {rhs}",
                            if r#type.is_unsigned() { "ugt" } else { "sgt" }
                        )?;
                        Ok(tmp.into())
                    }
                    typecheck::Infix::Assign => unreachable!(),
                }
            }
        }
        typecheck::ExpressionKind::LoopExp(block) => {
            let cont = t.next_label();
            let brk = t.next_label();
            let break_reg = t.next_reg();
            if !r#type.is_void() {
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
            gen_block(block, gen, st, t, o, l)?;
            let Some(Loop {
                cont_label: cont,
                break_label: brk,
                break_reg,
                ..
            }) = l else { panic!("ICE") };
            writeln!(o, "br label %{cont}")?;
            writeln!(o, "\n{brk}:")?;
            let ret = t.next_reg();
            if !r#type.is_void() {
                gen.load(r#type, ret.as_ref(), break_reg.as_ref(), o)?;
                Ok(ret.into())
            } else {
                Ok(Reg::zero().into())
            }
        }
        typecheck::ExpressionKind::Break(e) => {
            if let Some(e) = e {
                let ty = type_conv(&e.r#type, st);
                let e_reg = gen_expr(*e, gen, st, t, o, l)?;
                let Some(l) = l else {
                    panic!("ICE");
                };
                gen.store(ty, e_reg, l.break_reg.as_ref(), o)?;
            };
            let Some(l) = l else {
                panic!("ICE");
            };
            writeln!(o, "br label %{}", l.break_label)?;
            Ok(Reg::zero().into())
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
            let cond = gen_expr(*cond, gen, st, t, o, l)?;
            if !r#type.is_void() {
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
            let if_ret = gen_block(block, gen, st, t, o, l)?;
            if !r#type.is_void() {
                gen.store(r#type.as_ref(), if_ret, ret_alloc.as_ref(), o)?;
            }
            writeln!(o, "br label %{finally_label}")?;

            if let Some(else_block) = else_block {
                writeln!(o, "\n{else_label}:")?;
                let else_ret = gen_block(else_block, gen, st, t, o, l)?;
                if !r#type.is_void() {
                    gen.store(r#type.as_ref(), else_ret, ret_alloc.as_ref(), o)?;
                }
                writeln!(o, "br label %{finally_label}")?;
            }

            writeln!(o, "\n{finally_label}:")?;

            if !r#type.is_void() {
                let ret = t.next_reg();
                gen.load(r#type, ret.as_ref(), ret_alloc, o)?;
                Ok(ret.into())
            } else {
                Ok(Reg::zero().into())
            }
        }
        typecheck::ExpressionKind::Block(b) => gen_block(b, gen, st, t, o, l),
        typecheck::ExpressionKind::Var { ident } => {
            let ret = t.next_reg();
            gen.load(r#type, ret.as_ref(), Reg::from_plain(st.original(ident)), o)?;
            Ok(ret.into())
        }
        typecheck::ExpressionKind::Val(v) => Ok(Reg::from_value(v)),
        typecheck::ExpressionKind::Call { fn_expr, params } => {
            let mut computed_params = Vec::new();
            for param in params {
                computed_params.push((param.r#type.clone(), gen_expr(param, gen, st, t, o, l)?));
            }
            let typecheck::Expression { kind, .. } = *fn_expr;
            let typecheck::ExpressionKind::Var { ident } = kind else {
                todo!("Computed function calls")
            };
            let reg: CowReg = if !r#type.is_void() {
                let reg = t.next_reg();
                write!(o, "{reg} = ")?;
                reg.into()
            } else {
                Reg::zero().into()
            };
            write!(o, "call {} @{}(ptr %mm", r#type, st.original(ident))?;
            for (r#type, param) in computed_params.iter() {
                write!(o, ", {} {}", type_conv(r#type, st), param)?;
            }
            writeln!(o, ")")?;
            Ok(reg)
        }
    }
}

struct Loop {
    cont_label: Label,
    break_label: Label,
    break_reg: Reg,
}

fn gen_block(
    b: Block,
    gen: &llvm::LLVM,
    s: &SymbolTable<String>,
    t: &mut Tmp,
    o: &mut impl io::Write,
    l: &mut Option<Loop>,
) -> anyhow::Result<CowReg<'static>> {
    let Block { stmts, expr, .. } = b;
    for stmt in stmts {
        gen_stmt(stmt, gen, s, t, o, l)?;
    }
    if let Some(expr) = expr {
        gen_expr(*expr, gen, s, t, o, l)
    } else {
        Ok(Reg::zero().into())
    }
}
