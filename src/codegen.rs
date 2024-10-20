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
            output = format!("movl ${integer}, %eax\n")
        }
        AstData::Exp(Expression::UnOp(kind)) => match kind {
            UnOpKind::Negation => {
                output += &generate(ast, ast.get(node.children[0]));
                output += "neg %eax\n";
            }
            UnOpKind::BitwiseNot => {
                output += &generate(ast, ast.get(node.children[0]));
                output += "not %eax\n";
            }
            UnOpKind::LogicalNot => {
                output += &generate(ast, ast.get(node.children[0]));
                output += "cmpl $0, %eax\n";
                output += "movl $0, %eax\n";
                output += "sete %al\n";
            }
        },
        AstData::Exp(Expression::BinOp(kind)) => match kind {
            /*
             * <CODE FOR e1 GOES HERE>
             * push %eax ; save value of e1 on the stack
             * <CODE FOR e2 GOES HERE>
             * pop %ecx ; pop e1 from the stack into ecx
             * addl %ecx, %eax ; add e1 to e2, save results in eax
             * 
             * output += "push %eax";
             * output += "pop %ecx";
             * output += "addl %ecx, %eax";
             */
            BinOpKind::Addition => {
                output += &generate(ast, ast.get(node.children[0]));
                output += "push %eax\n";
                output += &generate(ast, ast.get(node.children[1]));
                output += "pop %ecx\n";
                output += "addl %ecx, %eax\n";
            },
            BinOpKind::Subtraction => {
                output += &generate(ast, ast.get(node.children[1]));
                output += "push %eax\n";
                output += &generate(ast, ast.get(node.children[0]));
                output += "pop %ecx\n";
                output += "subl %ecx, %eax\n";
            },
            BinOpKind::Multiplication => {
                output += &generate(ast, ast.get(node.children[0]));
                output += "push %eax\n";
                output += &generate(ast, ast.get(node.children[1]));
                output += "pop %ecx\n";
                output += "imul %ecx, %eax\n";
            },
            BinOpKind::Division => {
                output += &generate(ast, ast.get(node.children[1]));
                output += "push %eax\n";
                output += &generate(ast, ast.get(node.children[0]));
                output += "cdq\n";
                output += "pop %ecx\n";
                output += "idivl %ecx\n";
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
