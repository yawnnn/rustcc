use crate::parse::*;

fn generate(ast: &Ast, key: AstKey) -> String {
    let mut output = String::new();
    let node = ast.get(key);

    match &node.data {
        AstData::Prog(_) => node.children.iter().for_each(|k| output += &generate(ast, *k)),
        AstData::Func(Function(name)) => {
            output += &format!(".globl {name}\n");
            output += &format!("{name}:\n");
            node.children.iter().for_each(|k| output += &generate(ast, *k))
        }
        AstData::Stmt(_) => {
            output += &generate(ast, node.children[0]);
            output += "ret\n"
        }
        AstData::Exp(Expression::Constant(c)) => output = format!("movl ${c}, %eax\n"),
        AstData::Exp(Expression::UnOp(kind)) => match kind {
            UnOpKind::Minus => {
                output += &generate(ast, node.children[0]);
                output += "neg %eax\n";
            }
            UnOpKind::BitwiseNot => {
                output += &generate(ast, node.children[0]);
                output += "not %eax\n";
            }
            UnOpKind::LogicalNot => {
                output += &generate(ast, node.children[0]);
                output += "cmpl $0, %eax\n";
                output += "movl $0, %eax\n";
                output += "sete %al\n";
            }
        },
    }

    output
}

pub fn codegen(dbg_mode: bool, input: Ast) -> Option<String> {
    let asm = generate(&input, 0);

    if dbg_mode {
        println!("asm: \n{asm}\n");
    }

    Some(asm)
}
