#![allow(unused)]

use std::{cell::Cell, ops};

use crate::check::Ctx;
use crate::parse::*;

// what about something like Cow?
struct Assembly(Vec<String>);

impl Assembly {
    fn new() -> Assembly {
        Assembly(Vec::new())
    }

    fn from(s: String) -> Assembly {
        let mut asm = Self::new();
        asm += s;
        asm
    }

    fn push<S: ops::Deref<Target = str>>(&mut self, command: S) {
        self.0.push(command.to_string());
    }

    fn assemble(&self) -> String {
        self.0.join("\n") + "\n"
    }
}

impl ops::Deref for Assembly {
    type Target = Vec<String>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

// Implement `+=` for Assembly
impl ops::AddAssign<Assembly> for Assembly {
    fn add_assign(&mut self, command: Assembly) {
        self.0.extend(command.0);
    }
}

impl<S: ops::Deref<Target = str>> ops::AddAssign<S> for Assembly {
    fn add_assign(&mut self, command: S) {
        self.push(command);
    }
}

// impl fmt::Display for Assembly {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         write!(f, "{}", self.assemble())
//     }
// }

/// generate a unique label
fn gen_unique_label() -> String {
    // use std::sync::atomic::{AtomicU64, Ordering};
    // static COUNTER: AtomicU64 = AtomicU64::new(0);
    // let id = COUNTER.fetch_add(1, Ordering::Relaxed);

    thread_local! {
        static COUNTER: Cell<u64> = const { Cell::new(0) };
    }

    let id = COUNTER.with(|counter| {
        let current = counter.get();
        counter.set(current + 1);
        current
    });

    format!(".L{id}")
}

/// generate assembly for
/// - `BinOpKind::Add`
/// - `BinOpKind::Sub`
/// - `BinOpKind::Mul`
/// - `BinOpKind::Div`
/// - `BinOpKind::Mod`
fn gen_exp_binop_aritmethic(ctx: &Ctx, kind: &BinOpKind, op1: &Expr, op2: &Expr) -> Assembly {
    let mut output = Assembly::new();

    #[rustfmt::skip]
    let (op1, op2, istr, div, remainder) = match kind {
        BinOpKind::Add => (op1, op2, "add" , false, false),
        BinOpKind::Sub => (op2, op1, "sub" , false, false),
        BinOpKind::Mul => (op1, op2, "imul", false, false),
        BinOpKind::Div => (op2, op1, "idiv", true , false),
        BinOpKind::Mod => (op2, op1, "idiv", true , true ),
        _ => unreachable!(),
    };

    output += gen_exp(ctx, op1);
    output += "push %rax";
    output += gen_exp(ctx, op2);

    if div {
        output += "cqo";
        output += "pop %rcx";
        output += format!("{istr} %rcx");
    } else {
        output += "pop %rcx";
        output += format!("{istr} %rcx, %rax");
    }

    if remainder {
        output += "mov %rdx, %rax";
    }

    output
}

/// generate assembly for
/// - `BinOpKind::LShift`
/// - `BinOpKind::RShift`
fn gen_exp_binop_bitshift(ctx: &Ctx, kind: &BinOpKind, op1: &Expr, op2: &Expr) -> Assembly {
    let mut output = Assembly::new();

    let instr = match kind {
        BinOpKind::LShift => "sal",
        BinOpKind::RShift => "sar",
        _ => unreachable!(),
    };

    output += gen_exp(ctx, op2);
    output += "push %rax";
    output += gen_exp(ctx, op1);
    output += "pop %rcx";
    output += format!("{instr} %cl, %rax");

    output
}

/// generate assembly for
/// - `BinOpKind::Eq`
/// - `BinOpKind::Neq`
/// - `BinOpKind::Lt`
/// - `BinOpKind::Leq`
/// - `BinOpKind::Gt`
/// - `BinOpKind::Geq`
fn gen_exp_binop_relational(ctx: &Ctx, kind: &BinOpKind, op1: &Expr, op2: &Expr) -> Assembly {
    let mut output = Assembly::new();

    let set_instr = match kind {
        BinOpKind::Eq => "sete",
        BinOpKind::Neq => "setne",
        BinOpKind::Lt => "setl",
        BinOpKind::Leq => "setle",
        BinOpKind::Gt => "setg",
        BinOpKind::Geq => "setge",
        _ => unreachable!(),
    };

    output += gen_exp(ctx, op1);
    output += "push %rax";
    output += gen_exp(ctx, op2);
    output += "pop %rcx";
    output += "cmp %rax, %rcx";
    output += "mov $0, %rax";
    output += format!("{set_instr} %al");

    output
}

/// generate assembly for
/// - `BinOpKind::BitOr`
/// - `BinOpKind::BitXor`
/// - `BinOpKind::BitAnd`
fn gen_exp_binop_bitwise(ctx: &Ctx, kind: &BinOpKind, op1: &Expr, op2: &Expr) -> Assembly {
    let mut output = Assembly::new();

    let instr = match kind {
        BinOpKind::BitOr => "or",
        BinOpKind::BitXor => "xor",
        BinOpKind::BitAnd => "and",
        _ => unreachable!(),
    };

    output += gen_exp(ctx, op1);
    output += "push %rax";
    output += gen_exp(ctx, op2);
    output += "pop %rcx";
    output += format!("{instr} %rcx, %rax");

    output
}

/// generate assembly for
/// - `BinOpKind::LogAnd`
/// - `BinOpKind::LogOr`
fn gen_exp_binop_logical(ctx: &Ctx, kind: &BinOpKind, op1: &Expr, op2: &Expr) -> Assembly {
    let mut output = Assembly::new();
    let op2_label = gen_unique_label();
    let end_label = gen_unique_label();

    let different_part = match kind {
        BinOpKind::LogAnd => Assembly::from(format!("jne {op2_label}")),
        BinOpKind::LogOr => {
            let mut s = Assembly::from(format!("je {op2_label}"));
            s += "mov $1, %rax";
            s
        }
        _ => unreachable!(),
    };

    // op1
    output += gen_exp(ctx, op1);
    output += "cmp $0, %rax";
    output += different_part;
    output += format!("jmp {end_label}");

    // op2
    output += format!("{op2_label}:");
    output += gen_exp(ctx, op2);
    output += "cmp $0, %rax";
    output += "mov $0, %rax";
    output += "setne %al";
    output += format!("{end_label}:");

    output
}

fn gen_exp(ctx: &Ctx, expr: &Expr) -> Assembly {
    let mut output = Assembly::new();

    match expr {
        Expr::Const(tok) => output += format!("mov ${}, %rax", tok.value.parse::<i32>().unwrap()),
        Expr::UnOp(_, kind, op) => match kind {
            UnOpKind::Negative => {
                output += gen_exp(ctx, op);
                output += "neg %rax";
            }
            UnOpKind::BitNot => {
                output += gen_exp(ctx, op);
                output += "not %rax";
            }
            UnOpKind::LogNot => {
                output += gen_exp(ctx, op);
                output += "cmp $0, %rax";
                output += "mov $0, %rax";
                output += "sete %al";
            }
            _ => todo!(),
        },
        Expr::BinOp(_, kind, op1, op2) => match kind {
            BinOpKind::Add | BinOpKind::Sub | BinOpKind::Mul | BinOpKind::Div | BinOpKind::Mod => {
                output += gen_exp_binop_aritmethic(ctx, kind, op1, op2);
            }
            BinOpKind::LShift | BinOpKind::RShift => {
                output += gen_exp_binop_bitshift(ctx, kind, op1, op2);
            }
            BinOpKind::Eq
            | BinOpKind::Neq
            | BinOpKind::Lt
            | BinOpKind::Leq
            | BinOpKind::Gt
            | BinOpKind::Geq => {
                output += gen_exp_binop_relational(ctx, kind, op1, op2);
            }
            BinOpKind::BitOr | BinOpKind::BitXor | BinOpKind::BitAnd => {
                output += gen_exp_binop_bitwise(ctx, kind, op1, op2);
            }
            BinOpKind::LogAnd | BinOpKind::LogOr => {
                output += gen_exp_binop_logical(ctx, kind, op1, op2);
            }
            _ => todo!(),
        },
        Expr::Assign(tok, _, var, value) => {
            output += gen_exp(ctx, value);
            let Expr::Var(var) = var.as_ref() else {
                panic!("Expected variable")
            };
            let var = ctx.get_var(var.value).unwrap();
            output += format!("mov %rax, -{}(%rbp)", var.offset());
        }
        Expr::Var(var) => {
            let var = ctx.get_var(var.value).unwrap();
            output += format!("mov -{}(%rbp), %rax", var.offset());
        }
        Expr::TerOp(_, _, cond, trueb, falseb) => {
            todo!()
        }
    }

    output
}

fn gen_stmt(ctx: &Ctx, stmt: &Stmt, ret_lable: &str) -> Assembly {
    let mut output = Assembly::new();

    match stmt {
        Stmt::Ret(expr) => {
            output += gen_exp(ctx, expr);
            output += format!("jmp {ret_lable}");
        }
        Stmt::Expr(expr) => {
            output += gen_exp(ctx, expr);
        }
        Stmt::If{cond, true_b, false_b} => {
            todo!()
        }
    }

    output
}

fn gen_decl(ctx: &Ctx, decl: &Decl) -> Assembly {
    let mut output = Assembly::new();

    match &decl.value {
        Some(expr) => {
            output += gen_exp(ctx, expr);
            output += "push %rax";
        }
        _ => {
            output += "push $0";
        }
    }

    output
}

fn gen_block_item(ctx: &Ctx, block_item: &BlockItem, ret_lable: &str) -> Assembly {
    match block_item {
        BlockItem::Decl(decl) => gen_decl(ctx, decl),
        BlockItem::Stmt(stmt) => gen_stmt(ctx, stmt, ret_lable),
    }
}

fn gen_fn_defs(ctx: &Ctx, ast: &Ast) -> Assembly {
    let mut output = Assembly::new();

    for defs in &ast.defs {
        let FnDef { name, body } = defs;
        output += format!(".globl {}", name.value);
        output += format!("{}:", name.value);
        output += "push %rbp";
        output += "mov %rsp, %rbp";

        let ret_label = gen_unique_label();

        body
            .iter()
            .for_each(|block_item| output += gen_block_item(ctx, block_item, &ret_label));

        if !matches!(body.last(), Some(BlockItem::Stmt(Stmt::Ret(_)))) {
            output += "mov $0, %rax";
        }

        output += format!("{ret_label}:");
        output += "mov %rbp, %rsp";
        output += "pop %rbp";
        output += "ret";
    }

    output
}

/// TODO: extract the 64bit vs 32bit logic/operators out
pub fn codegen(ctx: &Ctx, ast: &Ast) -> Option<String> {
    let asm = gen_fn_defs(ctx, ast);
    let asm = asm.assemble();

    #[cfg(debug_assertions)]
    println!("\n# ASSEMBLY\n```asm\n{asm}\n```\n");

    Some(asm)
}
