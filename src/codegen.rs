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
/// - `BinOpKind::ArithAdd`
/// - `BinOpKind::ArithSub`
/// - `BinOpKind::ArithMul`
/// - `BinOpKind::ArithDiv`
/// - `BinOpKind::ArithMod`
fn gen_binop_aritmethic(ast: &Ast, kind: &BinOpKind, op1: AstKey, op2: AstKey) -> Assembly {
    let mut output = Assembly::new();

    #[rustfmt::skip]
    let (op1, op2, istr, div, remainder) = match kind {
        BinOpKind::ArithAdd =>       (op1, op2, "add" , false, false),
        BinOpKind::ArithSub =>    (op2, op1, "sub" , false, false),
        BinOpKind::ArithMul => (op1, op2, "imul", false, false),
        BinOpKind::ArithDiv =>       (op2, op1, "idiv", true , false),
        BinOpKind::ArithMod =>         (op2, op1, "idiv", true , true ),
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
/// - `BinOpKind::BitShLeft`
/// - `BinOpKind::BitShRight`
fn gen_binop_bitshift(ast: &Ast, kind: &BinOpKind, op1: AstKey, op2: AstKey) -> Assembly {
    let mut output = Assembly::new();

    let instr = match kind {
        BinOpKind::BitShLeft => "sal",
        BinOpKind::BitShRight => "sar",
        _ => unreachable!(),
    };

    output += generate(ast, ast.get(op2));
    output += "push %rax";
    output += generate(ast, ast.get(op1));
    output += "pop %rcx";
    output += format!("{instr} %cl, %rax");

    output
}

/// generate assembly for
/// - `BinOpKind::RelatEq`
/// - `BinOpKind::RelatNotEq`
/// - `BinOpKind::RelatLt`
/// - `BinOpKind::RelatLeq`
/// - `BinOpKind::RelatGt`
/// - `BinOpKind::RelatGeq`
fn gen_binop_relational(ast: &Ast, kind: &BinOpKind, op1: AstKey, op2: AstKey) -> Assembly {
    let mut output = Assembly::new();

    let set_instr = match kind {
        BinOpKind::RelatEq => "sete",
        BinOpKind::RelatNotEq => "setne",
        BinOpKind::RelatLt => "setl",
        BinOpKind::RelatLeq => "setle",
        BinOpKind::RelatGt => "setg",
        BinOpKind::RelatGeq => "setge",
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
/// - `BinOpKind::BitWsOr`
/// - `BinOpKind::BitWsXor`
/// - `BinOpKind::BitWsAnd`
fn gen_binop_bitwise(ast: &Ast, kind: &BinOpKind, op1: AstKey, op2: AstKey) -> Assembly {
    let mut output = Assembly::new();

    let instr = match kind {
        BinOpKind::BitWsOr => "or",
        BinOpKind::BitWsXor => "xor",
        BinOpKind::BitWsAnd => "and",
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
/// - `BinOpKind::LogicAnd`
/// - `BinOpKind::LogicOr`
fn gen_binop_logical(ast: &Ast, kind: &BinOpKind, op1: AstKey, op2: AstKey) -> Assembly {
    let mut output = Assembly::new();
    let op2_label = gen_unique_label();
    let end_label = gen_unique_label();

    let different_part = match kind {
        BinOpKind::LogicAnd => {
            Assembly::from(format!("jne {op2_label}"))
        }
        BinOpKind::LogicOr => {
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

fn generate(ast: &Ast, data: &AstData) -> Assembly {
    let mut output = Assembly::new();

    match data {
        AstData::Prog(main) => {
            output += generate(ast, ast.get(*main))
        }
        AstData::Func{ name, statements } => {
            let ident = name.value;
            output += format!(".globl {ident}");
            output += format!("{ident}:");
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
            UnOpKind::ArithNeg => {
                output += generate(ast, ast.get(*op));
                output += "neg %rax";
            }
            UnOpKind::BitWsNot => {
                output += generate(ast, ast.get(*op));
                output += "not %rax";
            }
            UnOpKind::LogicNot => {
                output += generate(ast, ast.get(*op));
                output += "cmp $0, %rax";
                output += "mov $0, %rax";
                output += "sete %al";
            }
        },
        AstData::Exp(Expression::BinOp(kind, op1, op2)) => match kind {
            BinOpKind::ArithAdd
            | BinOpKind::ArithSub
            | BinOpKind::ArithMul
            | BinOpKind::ArithDiv
            | BinOpKind::ArithMod => {
                output += gen_binop_aritmethic(ast, kind, *op1, *op2);
            }
            BinOpKind::BitShLeft
            | BinOpKind::BitShRight => {
                output += gen_binop_bitshift(ast, kind, *op1, *op2);
            },
            BinOpKind::RelatEq
            | BinOpKind::RelatNotEq
            | BinOpKind::RelatLt
            | BinOpKind::RelatLeq
            | BinOpKind::RelatGt
            | BinOpKind::RelatGeq => {
                output += gen_binop_relational(ast, kind, *op1, *op2);
            }
            BinOpKind::BitWsOr
            | BinOpKind::BitWsXor
            | BinOpKind::BitWsAnd => {
                output += gen_binop_bitwise(ast, kind, *op1, *op2);
            },
            BinOpKind::LogicAnd | BinOpKind::LogicOr => {
                output += gen_binop_logical(ast, kind, *op1, *op2);
            },
        },
        AstData::Stmt(_) | AstData::Exp(_) => todo!(),
    }

    output
}

/// TODO: extract the 64bit vs 32bit logic/operators out
/// TODO: can i reuse the AST traversal here ? i can't rn bc i can't tell when a sub expression ends and put "tail" instructions
pub fn codegen(input: Ast) -> Option<String> {
    let asm = generate(&input, input.get_root());

    #[cfg(debug_assertions)]
    println!("\n// ASM //\n{asm}\n");

    Some(asm.0)
}
