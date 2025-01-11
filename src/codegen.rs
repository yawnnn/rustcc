use std::{cell::Cell, ops, fmt};

use crate::parse::*;

struct Assembly(String);

impl Assembly {
    fn new() -> Assembly {
        Assembly(String::new())
    }

    fn from(s: String) -> Assembly {
        let mut asm = Self::new();
        asm += s;
        asm
    }

    fn push<S: ops::Deref<Target = str>>(&mut self, command: S) {
        let command = &command;
        self.0.reserve(command.len() + 1);
        self.0.push_str(command);
        self.0.push('\n');
    }
}

impl ops::Deref for Assembly {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<S: ops::Deref<Target = str>> ops::AddAssign<S> for Assembly {
    fn add_assign(&mut self, command: S) {
        self.push(command);
    }
}

impl fmt::Display for Assembly {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

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
/// - `BinOpKind::Modulo`
fn gen_binop_aritmethic(ast: &Ast, kind: &BinOpKind, op1: AstKey, op2: AstKey) -> Assembly {
    let mut output = Assembly::new();

    #[rustfmt::skip]
    let (op1, op2, istr, div, remainder) = match kind {
        BinOpKind::Addition =>       (op1, op2, "add" , false, false),
        BinOpKind::Subtraction =>    (op2, op1, "sub" , false, false),
        BinOpKind::Multiplication => (op1, op2, "imul", false, false),
        BinOpKind::Division =>       (op2, op1, "idiv", true , false),
        BinOpKind::Modulo =>         (op2, op1, "idiv", true , true ),
        _ => unreachable!(),
    };

    output += generate(ast, ast.get(op1));
    output += "push %rax";
    output += generate(ast, ast.get(op2));

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
/// - `BinOpKind::LogicalEq`
/// - `BinOpKind::LogicalNotEq`
/// - `BinOpKind::Lt`
/// - `BinOpKind::LtEq`
/// - `BinOpKind::Gt`
/// - `BinOpKind::GtEq`
fn gen_binop_compare(ast: &Ast, kind: &BinOpKind, op1: AstKey, op2: AstKey) -> Assembly {
    let mut output = Assembly::new();

    let set_instr = match kind {
        BinOpKind::LogicalEq => "sete",
        BinOpKind::LogicalNotEq => "setne",
        BinOpKind::Lt => "setl",
        BinOpKind::LtEq => "setle",
        BinOpKind::Gt => "setg",
        BinOpKind::GtEq => "setge",
        _ => unreachable!(),
    };

    output += generate(ast, ast.get(op1));
    output += "push %rax";
    output += generate(ast, ast.get(op2));
    output += "pop %rcx";
    output += "cmp %rax, %rcx";
    output += "mov $0, %rax";
    output += format!("{set_instr} %al");

    output
}

/// generate assembly for
/// - `BinOpKind::LogicalAnd`
/// - `BinOpKind::LogicalOr`
fn gen_binop_logical(ast: &Ast, kind: &BinOpKind, op1: AstKey, op2: AstKey) -> Assembly {
    let mut output = Assembly::new();
    let op2_label = gen_unique_label();
    let end_label = gen_unique_label();

    let different_part = match kind {
        BinOpKind::LogicalAnd => {
            Assembly::from(format!("jne {op2_label}"))
        }
        BinOpKind::LogicalOr => {
            let mut s = Assembly::from(format!("je {op2_label}"));
            s += "mov $1, %rax";
            s
        }
        _ => unreachable!(),
    };

    // op1
    output += generate(ast, ast.get(op1));
    output += "cmp $0, %rax";
    output += different_part;
    output += format!("jmp {end_label}");

    // op2
    output += format!("{op2_label}:");
    output += generate(ast, ast.get(op2));
    output += "cmp $0, %rax";
    output += "mov $0, %rax";
    output += "setne %al";
    output += format!("{end_label}:");

    output
}

/// generate assembly for
/// - `BinOpKind::BitwiseOr`
/// - `BinOpKind::BitwiseXor`
/// - `BinOpKind::BitwiseAnd`
fn gen_binop_bitwise(ast: &Ast, kind: &BinOpKind, op1: AstKey, op2: AstKey) -> Assembly {
    let mut output = Assembly::new();

    let instr = match kind {
        BinOpKind::BitwiseOr => "or",
        BinOpKind::BitwiseXor => "xor",
        BinOpKind::BitwiseAnd => "and",
        _ => unreachable!(),
    };

    output += generate(ast, ast.get(op1));
    output += "push %rax";
    output += generate(ast, ast.get(op2));
    output += "pop %rcx";
    output += format!("{instr} %rcx, %rax");

    output
}

/// generate assembly for
/// - `BinOpKind::BitShiftLeft`
/// - `BinOpKind::BitShiftRight`
fn gen_binop_bitshift(ast: &Ast, kind: &BinOpKind, op1: AstKey, op2: AstKey) -> Assembly {
    let mut output = Assembly::new();

    let instr = match kind {
        BinOpKind::BitShiftLeft => "sal",
        BinOpKind::BitShiftRight => "sar",
        _ => unreachable!(),
    };

    output += generate(ast, ast.get(op2));
    output += "push %rax";
    output += generate(ast, ast.get(op1));
    output += "pop %rcx";
    output += format!("{instr} %cl, %rax");

    output
}

fn generate(ast: &Ast, node: &AstNode) -> Assembly {
    let mut output = Assembly::new();

    match &node.data {
        AstData::Prog(main) => {
            output += generate(ast, ast.get(*main))
        }
        AstData::Func{ name, statements } => {
            output += format!(".globl {name}");
            output += format!("{name}:");
            statements.iter().for_each(|k| output += generate(ast, ast.get(*k)))
        }
        AstData::Stmt(Statement::Return(exp)) => {
            output += generate(ast, ast.get(*exp));
            output += "ret"
        }
        AstData::Exp(Expression::Literal(Literal::Integer(integer))) => {
            output += format!("mov ${integer}, %rax")
        }
        AstData::Exp(Expression::UnOp(kind, op)) => match kind {
            UnOpKind::Negation => {
                output += generate(ast, ast.get(*op));
                output += "neg %rax";
            }
            UnOpKind::BitwiseNot => {
                output += generate(ast, ast.get(*op));
                output += "not %rax";
            }
            UnOpKind::LogicalNot => {
                output += generate(ast, ast.get(*op));
                output += "cmp $0, %rax";
                output += "mov $0, %rax";
                output += "sete %al";
            }
        },
        AstData::Exp(Expression::BinOp(kind, op1, op2)) => match kind {
            BinOpKind::Addition
            | BinOpKind::Subtraction
            | BinOpKind::Multiplication
            | BinOpKind::Division
            | BinOpKind::Modulo => {
                output += gen_binop_aritmethic(ast, kind, *op1, *op2);
            }
            BinOpKind::LogicalEq
            | BinOpKind::LogicalNotEq
            | BinOpKind::Lt
            | BinOpKind::LtEq
            | BinOpKind::Gt
            | BinOpKind::GtEq => {
                output += gen_binop_compare(ast, kind, *op1, *op2);
            }
            BinOpKind::LogicalAnd | BinOpKind::LogicalOr => {
                output += gen_binop_logical(ast, kind, *op1, *op2);
            },
            BinOpKind::BitwiseOr
            | BinOpKind::BitwiseXor
            | BinOpKind::BitwiseAnd => {
                output += gen_binop_bitwise(ast, kind, *op1, *op2);
            },
            BinOpKind::BitShiftLeft
            | BinOpKind::BitShiftRight => {
                output += gen_binop_bitshift(ast, kind, *op1, *op2);
            },
        },
    }

    output
}

/// TODO: extract the 64bit vs 32bit logic/operators out
pub fn codegen(input: Ast) -> Option<String> {
    let asm = generate(&input, input.get_root());

    #[cfg(debug_assertions)]
    println!("\n// ASM //\n{asm}\n");

    Some(asm.0)
}
