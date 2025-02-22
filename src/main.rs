mod check;
mod codegen;
mod common;
mod compile;
mod lex;
mod parse;

use crate::compile::compile;

use std::env;

fn main() -> Result<(), ()> {
    let args = env::args().skip(1).collect::<Vec<_>>();
    let mut args = args.iter().map(String::as_str).collect::<Vec<_>>();

    let mut exe_name = None;
    let mut dump_ast = false;
    let mut dump_asm = false;

    if matches!(args.first(), Some(&"-o")) && args.len() >= 2 {
        exe_name = args.get(1).map(|s| s.to_string());
        args.remove(0);
        args.remove(0);
    }
    
    if matches!(args.first(), Some(&"-p")) {
        dump_ast = true;
        args.remove(0);
    }

    if matches!(args.first(), Some(&"-s")) {
        dump_asm = true;
        args.remove(0);
    }

    if args.is_empty() || (exe_name.is_none() && !dump_ast && !dump_asm) {
        return Err(());
    }

    compile(exe_name, args[0].to_string(), dump_ast, dump_asm).ok_or(())
}
