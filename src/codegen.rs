use crate::parse::*;

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
            },
            BinOpKind::Subtraction => {
                output += &generate(ast, ast.get(node.children[1]));
                output += "push %rax\n";
                output += &generate(ast, ast.get(node.children[0]));
                output += "pop %rcx\n";
                output += "sub %rcx, %rax\n";
            },
            BinOpKind::Multiplication => {
                output += &generate(ast, ast.get(node.children[0]));
                output += "push %rax\n";
                output += &generate(ast, ast.get(node.children[1]));
                output += "pop %rcx\n";
                output += "imul %rcx, %rax\n";
            },
            BinOpKind::Division => {
                output += &generate(ast, ast.get(node.children[1]));
                output += "push %rax\n";
                output += &generate(ast, ast.get(node.children[0]));
                output += "cdq\n";
                output += "pop %rcx\n";
                output += "idiv %rcx\n";
            },
        }
        _ => todo!(),
    }

    output
}

pub fn codegen(input: Ast) -> Option<String> {
    let asm = generate(&input, input.first());

    #[cfg(debug_assertions)]
    println!("\n### ASM ###\n{asm}\n");

    Some(asm)
}
