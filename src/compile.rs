use crate::lex::lex;
use crate::parse::parse;
use crate::check::check;
use crate::codegen::codegen;

use std::{
    fs::{self, File},
    io::Write,
    path::Path,
    process,
};

fn write_asm<P: AsRef<Path>>(exe_name: P, input: &str) -> Option<()> {
    let asm_name = exe_name.as_ref().with_extension("s");
    let _ = fs::remove_file(&asm_name); // delete if exists
    let mut file = File::create(&asm_name).ok()?;
    write!(file, "{input}").ok()?;

    Some(())
}

fn link<P: AsRef<Path>>(exe_name: P) -> Option<()> {
    // process::Command::new().args accepts IntoIterator<AsRef<OsStr>>
    // but it doesn't like it when i pass a mix of different types, all individually ok.
    // so i convert them all to_str, since i have "-o"
    let exe_name = exe_name.as_ref();
    let asm_name = exe_name.with_extension("s");
    let exe_name = exe_name.to_str()?;
    let asm_name = asm_name.to_str()?;
    let _ = fs::remove_file(exe_name); // delete if exists
    let process = process::Command::new("gcc")
        .args([asm_name, "-o", exe_name])
        .stdout(process::Stdio::piped())
        .stderr(process::Stdio::piped())
        .spawn()
        .ok()?;
    let output = process.wait_with_output().ok()?;
    let stdout = String::from_utf8(output.stdout).ok()?;
    let stderr = String::from_utf8(output.stderr).ok()?;
    let _output = stdout + &stderr;

    #[cfg(debug_assertions)]
    println!("\n# LINKING\n```\n{_output}\n```\n");

    Some(())
}

pub fn compile<P: AsRef<Path>>(exe_name: Option<P>, src_name: P, dump_ast: bool, dump_asm: bool) -> Option<()> {
    let src = fs::read_to_string(&src_name).unwrap();
    let tokens = lex(&src);
    let ast = parse(&tokens)?;

    if dump_ast {
        println!("{:#?}", ast);
        return Some(());
    }

    let ir = check(&ast);
    let asm = codegen(ir)?;

    if dump_asm {
        println!("{}", asm);
        return Some(())
    }

    let exe_name = exe_name.unwrap();
    write_asm(&exe_name, &asm)?;
    link(&exe_name)
}
