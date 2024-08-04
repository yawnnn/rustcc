mod codegen;
mod compile;
mod lex;
mod parse;

use crate::compile::compile;

use std::env;

fn main() -> Result<(), ()> {
    let args = env::args().collect::<Vec<_>>();

    match args.len() {
        2 => compile(&args[1]).ok_or(()),
        _ => Err(()),
    }
}
