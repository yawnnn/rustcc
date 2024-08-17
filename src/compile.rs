use crate::codegen::codegen;
use crate::lex::lex;
use crate::parse::parse;

use std::{
    fs::{self, File},
    io::Write,
    path::{Path, PathBuf},
    process,
};

fn write_asm<P: AsRef<Path>>(src_name: P, input: &str) -> Option<PathBuf> {
    let asm_name = src_name.as_ref().with_extension("s");
    let _ = fs::remove_file(&asm_name); // delete if exists
    let mut file = File::create(&asm_name).ok()?;
    write!(file, "{input}").ok()?;

    Some(asm_name)
}

fn link<P: AsRef<Path>>(asm_name: P) -> Option<()> {
    // process::Command::new().args accepts IntoIterator<AsRef<OsStr>>
    // but it doesn't like it when i pass a mix of different types, all individually ok.
    // so i convert them all to_str, since i have "-o"
    let exe_name = asm_name.as_ref().with_extension("");
    let exe_name = exe_name.to_str()?;
    let asm_name = asm_name.as_ref().to_str()?;
    let _ = fs::remove_file(exe_name); // delete if exists
    let process = process::Command::new("gcc")
        .args([asm_name, "-o", exe_name])
        .spawn()
        .ok()?;
    let output = process.wait_with_output().ok()?;
    let stdout = String::from_utf8(output.stdout).ok()?;
    let stderr = String::from_utf8(output.stderr).ok()?;
    let _output = stdout + &stderr;

    //println!("link: \n{output}\n");

    Some(())
}

pub fn compile<P: AsRef<Path>>(src_name: P) -> Option<()> {
    let dbg_mode = false;

    let src = fs::read_to_string(&src_name).unwrap();
    let tokens = lex(dbg_mode, &src);
    let ast = parse(dbg_mode, &src, tokens)?;
    let asm = codegen(dbg_mode, ast)?;
    let asm_name = write_asm(&src_name, &asm)?;
    link(asm_name)
}
