use std::cell::Cell;

use crate::parse::*;

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

    format!("_l{id}")
}

/// generate assembly for
/// - `BinOpKind::Addition`
/// - `BinOpKind::Subtraction`
/// - `BinOpKind::Multiplication`
/// - `BinOpKind::Division`
fn gen_binop_aritmethic(ast: &Ast, node: &AstNode, kind: &BinOpKind) -> String {
    let mut output = String::new();

    #[rustfmt::skip]
    let (child1, child2, istr, div) = match kind {
        BinOpKind::Addition =>       (0, 1, "add" , false),
        BinOpKind::Subtraction =>    (1, 0, "sub" , false),
        BinOpKind::Multiplication => (0, 1, "imul", false),
        BinOpKind::Division =>       (1, 0, "idiv", true ),
        _ => unreachable!(),
    };

    output += &generate(ast, ast.get(node.children[child1]));
    output += "push %rax\n";
    output += &generate(ast, ast.get(node.children[child2]));

    if div {
        output += "cqo\n";
        output += "pop %rcx\n";
        output += &format!("{istr} %rcx\n");
    } else {
        output += "pop %rcx\n";
        output += &format!("{istr} %rcx, %rax\n");
    }

    output
}

/// generate assembly for
/// - `BinOpKind::LogicalEq`
/// - `BinOpKind::LogicalNotEq`
/// - `BinOpKind::Lt`
/// - `BinOpKind::LtEq`
/// - `BinOpKind::Gt`
/// - `BinOpKind::GtEq`
fn gen_binop_compare(ast: &Ast, node: &AstNode, kind: &BinOpKind) -> String {
    let mut output = String::new();

    let set_instr = match kind {
        BinOpKind::LogicalEq => "sete",
        BinOpKind::LogicalNotEq => "setne",
        BinOpKind::Lt => "setl",
        BinOpKind::LtEq => "setle",
        BinOpKind::Gt => "setg",
        BinOpKind::GtEq => "setge",
        _ => unreachable!(),
    };

    output += &generate(ast, ast.get(node.children[0]));
    output += "push %rax\n";
    output += &generate(ast, ast.get(node.children[1]));
    output += "pop %rcx\n";
    output += "cmp %rax, %rcx\n";
    output += "mov $0, %rax\n";
    output += &format!("{set_instr} %al\n");

    output
}

/// generate assembly for
/// - `BinOpKind::LogicalAnd`
/// - `BinOpKind::LogicalOr`
fn gen_binop_logical(ast: &Ast, node: &AstNode, kind: &BinOpKind) -> String {
    let mut output = String::new();
    let op2_label = gen_unique_label();
    let end_label = gen_unique_label();

    let different_part = match kind {
        BinOpKind::LogicalAnd => {
            format!("jne {op2_label}\n")
        }
        BinOpKind::LogicalOr => {
            let mut s = format!("je {op2_label}\n");
            s += "mov $1, %rax\n";
            s
        }
        _ => unreachable!(),
    };

    // op1
    output += &generate(ast, ast.get(node.children[0]));
    output += "cmp $0, %rax\n";
    output += &different_part;
    output += &format!("jmp {end_label}\n");

    // op2
    output += &format!("{op2_label}:\n");
    output += &generate(ast, ast.get(node.children[1]));
    output += "cmp $0, %rax\n";
    output += "mov $0, %rax\n";
    output += "setne %al\n";
    output += &format!("{end_label}:\n");

    output
}

/// TODO --- extract the 64bit vs 32bit logic/operators out
/// TODO --- don't add newlines by hand
fn generate(ast: &Ast, node: &AstNode) -> String {
    let mut output = String::new();

    match &node.data {
        AstData::Prog(_) => {
            node.children.iter().for_each(|k| output += &generate(ast, ast.get(*k)))
        }
        AstData::Func(Function { name }) => {
            output += &format!(".globl {name}\n");
            output += &format!("{name}:\n");
            node.children.iter().for_each(|k| output += &generate(ast, ast.get(*k)))
        }
        AstData::Stmt(_) => {
            output += &generate(ast, ast.get(node.children[0]));
            output += "ret\n"
        }
        AstData::Exp(Expression::Literal(Literal::Integer(integer))) => {
            output = format!("mov ${integer}, %rax\n")
        }
        AstData::Exp(Expression::UnOp(kind)) => match kind {
            UnOpKind::Negation => {
                output += &generate(ast, ast.get(node.children[0]));
                output += "neg %rax\n";
            }
            UnOpKind::BitwiseNot => {
                output += &generate(ast, ast.get(node.children[0]));
                output += "not %rax\n";
            }
            UnOpKind::LogicalNot => {
                output += &generate(ast, ast.get(node.children[0]));
                output += "cmp $0, %rax\n";
                output += "mov $0, %rax\n";
                output += "sete %al\n";
            }
        },
        AstData::Exp(Expression::BinOp(kind)) => match kind {
            BinOpKind::Addition
            | BinOpKind::Subtraction
            | BinOpKind::Multiplication
            | BinOpKind::Division => {
                output += &gen_binop_aritmethic(ast, node, kind);
            }
            BinOpKind::LogicalEq
            | BinOpKind::LogicalNotEq
            | BinOpKind::Lt
            | BinOpKind::LtEq
            | BinOpKind::Gt
            | BinOpKind::GtEq => {
                output += &gen_binop_compare(ast, node, kind);
            }
            BinOpKind::LogicalAnd | BinOpKind::LogicalOr => {
                output += &gen_binop_logical(ast, node, kind);
            }
        },
    }

    output
}

pub fn codegen(input: Ast) -> Option<String> {
    let asm = generate(&input, input.get_root());

    #[cfg(debug_assertions)]
    println!("\n// ASM //\n{asm}\n");

    Some(asm)
}
