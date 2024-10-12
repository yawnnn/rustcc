use crate::parse::*;

fn generate(ast: &Ast, node: &AstNode) -> String {
    let mut output = String::new();

    match &node.data {
        AstData::Prog(_) => node.children.iter().for_each(|k| output += &generate(ast, ast.get(*k))),
        AstData::Func(Function(name)) => {
            output += &format!(".globl {name}\n");
            output += &format!("{name}:\n");
            node.children.iter().for_each(|k| output += &generate(ast, ast.get(*k)))
        }
        AstData::Stmt(_) => {
            output += &generate(ast, ast.get(node.children[0]));
            output += "ret\n"
        }
        AstData::Exp(Expression::Constant(c)) => output = format!("movl ${c}, %eax\n"),
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
        _ => todo!(),
    }

    output
}

pub fn codegen(dbg: bool, input: Ast) -> Option<String> {
    let asm = generate(&input, input.first());

    if dbg {
        println!("asm: \n{asm}\n");
    }

    Some(asm)
}
