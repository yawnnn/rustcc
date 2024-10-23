mod codegen;
mod compile;
mod lex;
mod parse;

use crate::compile::compile;

use std::env;

fn main() -> Result<(), ()> {
    let args = env::args().collect::<Vec<_>>();
    let args = args.iter().map(String::as_str).collect::<Vec<_>>();

    match args.as_slice() {
        [_, "-o", exe_name, files @ ..] => {
            compile(exe_name, &files[0]).ok_or(())
        },
        _ => Err(()),
    }
}
