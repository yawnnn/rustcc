use crate::parse::*;

fn gen_logical_cmp(ast: &Ast, node: &AstNode, kind: &BinOpKind) -> String {
    let mut output = String::new();

    output += &generate(ast, ast.get(node.children[0]));
    output += "push %rax\n";
    output += &generate(ast, ast.get(node.children[1]));
    output += "pop %rcx\n";
    output += "cmpl %rax, %rcx\n";
    output += "mov $0, %rax\n";

    let set_instr = match kind {
        BinOpKind::LogicalEq => "sete",
        BinOpKind::LogicalNotEq => "setne",
        BinOpKind::Lt => "setl",
        BinOpKind::LtEq => "setle",
        BinOpKind::Gt => "setg",
        BinOpKind::GtEq => "setge",
        _ => unreachable!(),
    };

    output += &format!("{set_instr} %al");

    output
}

/// TODO --- extract the 64bit vs 32bit logic/operators out
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
            BinOpKind::Addition => {
                output += &generate(ast, ast.get(node.children[0]));
                output += "push %rax\n";
                output += &generate(ast, ast.get(node.children[1]));
                output += "pop %rcx\n";
                output += "add %rcx, %rax\n";
            }
            BinOpKind::Subtraction => {
                output += &generate(ast, ast.get(node.children[1]));
                output += "push %rax\n";
                output += &generate(ast, ast.get(node.children[0]));
                output += "pop %rcx\n";
                output += "sub %rcx, %rax\n";
            }
            BinOpKind::Multiplication => {
                output += &generate(ast, ast.get(node.children[0]));
                output += "push %rax\n";
                output += &generate(ast, ast.get(node.children[1]));
                output += "pop %rcx\n";
                output += "imul %rcx, %rax\n";
            }
            BinOpKind::Division => {
                output += &generate(ast, ast.get(node.children[1]));
                output += "push %rax\n";
                output += &generate(ast, ast.get(node.children[0]));
                output += "cqo\n";
                output += "pop %rcx\n";
                output += "idiv %rcx\n";
            }
            BinOpKind::LogicalEq
            | BinOpKind::LogicalNotEq
            | BinOpKind::Lt
            | BinOpKind::LtEq
            | BinOpKind::Gt
            | BinOpKind::GtEq => output += &gen_logical_cmp(ast, node, kind),
            _ => todo!(),
        },
        _ => todo!(),
    }

    output
}

pub fn codegen(input: Ast) -> Option<String> {
    let asm = generate(&input, input.get_root());

    #[cfg(debug_assertions)]
    println!("\n### ASM ###\n{asm}\n");

    Some(asm)
}
