#![allow(unused)]

use std::{cell::Cell, ops};

use crate::check::*;
use crate::parse::{BinOpKind, UnOpKind};

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
fn gen_exp_binop_aritmethic(kind: &BinOpKind, op1: &Exp, op2: &Exp) -> Assembly {
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

    output += gen_exp(op1);
    output += "push %rax";
    output += gen_exp(op2);

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
fn gen_exp_binop_bitshift(kind: &BinOpKind, op1: &Exp, op2: &Exp) -> Assembly {
    let mut output = Assembly::new();

    let instr = match kind {
        BinOpKind::LShift => "sal",
        BinOpKind::RShift => "sar",
        _ => unreachable!(),
    };

    output += gen_exp(op2);
    output += "push %rax";
    output += gen_exp(op1);
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
fn gen_exp_binop_relational(kind: &BinOpKind, op1: &Exp, op2: &Exp) -> Assembly {
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

    output += gen_exp(op1);
    output += "push %rax";
    output += gen_exp(op2);
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
fn gen_exp_binop_bitwise(kind: &BinOpKind, op1: &Exp, op2: &Exp) -> Assembly {
    let mut output = Assembly::new();

    let instr = match kind {
        BinOpKind::BitOr => "or",
        BinOpKind::BitXor => "xor",
        BinOpKind::BitAnd => "and",
        _ => unreachable!(),
    };

    output += gen_exp(op1);
    output += "push %rax";
    output += gen_exp(op2);
    output += "pop %rcx";
    output += format!("{instr} %rcx, %rax");

    output
}

/// generate assembly for
/// - `BinOpKind::LogAnd`
/// - `BinOpKind::LogOr`
fn gen_exp_binop_logical(kind: &BinOpKind, op1: &Exp, op2: &Exp) -> Assembly {
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
    output += gen_exp(op1);
    output += "cmp $0, %rax";
    output += different_part;
    output += format!("jmp {end_label}");

    // op2
    output += format!("{op2_label}:");
    output += gen_exp(op2);
    output += "cmp $0, %rax";
    output += "mov $0, %rax";
    output += "setne %al";
    output += format!("{end_label}:");

    output
}

fn gen_exp(exp: &Exp) -> Assembly {
    let mut output = Assembly::new();

    match exp {
        Exp::Literal(Literal::I32(integer)) => output += format!("mov ${integer}, %rax"),
        Exp::UnOp(kind, op) => match kind {
            UnOpKind::Negative => {
                output += gen_exp(op);
                output += "neg %rax";
            }
            UnOpKind::BitNot => {
                output += gen_exp(op);
                output += "not %rax";
            }
            UnOpKind::LogNot => {
                output += gen_exp(op);
                output += "cmp $0, %rax";
                output += "mov $0, %rax";
                output += "sete %al";
            }
            _ => todo!(),
        },
        Exp::BinOp(kind, op1, op2) => match kind {
            BinOpKind::Add | BinOpKind::Sub | BinOpKind::Mul | BinOpKind::Div | BinOpKind::Mod => {
                output += gen_exp_binop_aritmethic(kind, op1, op2);
            }
            BinOpKind::LShift | BinOpKind::RShift => {
                output += gen_exp_binop_bitshift(kind, op1, op2);
            }
            BinOpKind::Eq
            | BinOpKind::Neq
            | BinOpKind::Lt
            | BinOpKind::Leq
            | BinOpKind::Gt
            | BinOpKind::Geq => {
                output += gen_exp_binop_relational(kind, op1, op2);
            }
            BinOpKind::BitOr | BinOpKind::BitXor | BinOpKind::BitAnd => {
                output += gen_exp_binop_bitwise(kind, op1, op2);
            }
            BinOpKind::LogAnd | BinOpKind::LogOr => {
                output += gen_exp_binop_logical(kind, op1, op2);
            }
            _ => todo!(),
        },
        Exp::Assignment(var, value) => {
            output += gen_exp(value);
            output += format!("mov %rax, -{}(%rbp)", var.offset());
        }
        Exp::Var(var) => {
            output += format!("mov -{}(%rbp), %rax", var.offset());
        }
        Exp::TerOp(cond, trueb, falseb) => {
            todo!()
        }
    }

    output
}

fn gen_stmt(stmt: &Stmt, ret_lable: &str) -> Assembly {
    let mut output = Assembly::new();

    match stmt {
        Stmt::Return(exp) => {
            output += gen_exp(exp);
            output += format!("jmp {ret_lable}");
        }
        Stmt::Exp(exp) => {
            output += gen_exp(exp);
        }
        Stmt::If(exp, ifb, elseb) => {
            todo!()
        }
    }

    output
}

fn gen_decl(decl: &Decl) -> Assembly {
    let mut output = Assembly::new();

    match &decl.1 {
        Some(exp) => {
            output += gen_exp(exp);
            output += "push %rax";
        }
        _ => {
            output += "push $0";
        }
    }

    output
}

fn gen_block_item(block_item: &BlockItem, ret_lable: &str) -> Assembly {
    match block_item {
        BlockItem::Delc(decl) => gen_decl(decl),
        BlockItem::Stmt(stmt) => gen_stmt(stmt, ret_lable),
    }
}

fn generate_assembly(ir: IR) -> Assembly {
    let mut output = Assembly::new();

    for node in ir.into_iter() {
        match node {
            IRNode::Func(name, block_items) => {
                output += format!(".globl {name}");
                output += format!("{name}:");
                output += "push %rbp";
                output += "mov %rsp, %rbp";

                let ret_label = gen_unique_label();

                block_items
                    .iter()
                    .for_each(|block_item| output += gen_block_item(block_item, &ret_label));

                if !matches!(block_items.last(), Some(BlockItem::Stmt(Stmt::Return(_)))) {
                    output += "mov $0, %rax";
                }

                output += format!("{ret_label}:");
                output += "mov %rbp, %rsp";
                output += "pop %rbp";
                output += "ret";
            }
        }
    }

    output
}

/// TODO: extract the 64bit vs 32bit logic/operators out
pub fn codegen(ir: IR) -> Option<String> {
    let asm = generate_assembly(ir);
    let asm = asm.assemble();

    #[cfg(debug_assertions)]
    println!("\n# ASSEMBLY\n```asm\n{asm}\n```\n");

    Some(asm)
}
