use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::{fs, path};

use clap::Parser;

const BANNER: &str = "====================================================";

#[derive(Parser, Debug, Default)]
#[command(about, long_about = None)]
struct TestArgs {
    #[arg(short, long)]
    stages: Vec<u32>,
    #[arg(short, long)]
    files: Vec<String>,
    #[arg(short, long)]
    debug: bool,
    #[arg(short, long)]
    parse_only: bool,
    #[arg(short, long)]
    keep_exe: bool,
    #[arg(short, long)]
    no_run: bool,
}

fn dbg_file_name(file: &path::Path) {
    let basename = file.file_stem().unwrap().to_str().unwrap();
    println!("{0:.<1$}", basename, BANNER.len());
}

fn dbg_banner_centered(s: &str) {
    println!("{0:=^1$}", s, BANNER.len());
}

fn dbg_stage_summary(stage: u32, succ: u32, fail: u32) {
    dbg_banner_centered(&format!("Stage {stage} Summary"));
    println!("{succ} successes, {fail} failures")
}

fn dbg_final_summary(succ: u32, fail: u32) {
    dbg_banner_centered("FINAL SUMMARY");
    println!("{succ} successes, {fail} failures")
}

fn check(found: Option<(i32, String)>, expected: Option<(i32, String)>) -> bool {
    let succ = found == expected;

    if succ {
        println!("OK");
    } else {
        println!("ERR found: {found:?}, expected: {expected:?}",);
    }

    succ
}

fn exe_extension() -> &'static str {
    if cfg!(windows) {
        ".exe"
    } else {
        ""
    }
}

fn executable_cmd(path: &str) -> Command {
    if cfg!(windows) {
        Command::new(path)
    } else {
        Command::new(format!("./{}", path))
    }
}

fn cargo_profile(debug: bool) -> &'static [&'static str] {
    if debug {
        &["--profile", "dev"]
    } else {
        &["--profile", "release"]
    }
}

fn run_gcc<T: AsRef<str>>(files: &[T], output: &str, _args: &TestArgs) -> (i32, String) {
    let _ = Command::new("gcc")
        .args(["-w", "-o", output])
        .args(files.iter().map(|f| f.as_ref()))
        .stdout(Stdio::null())
        .status()
        .unwrap();

    let out = executable_cmd(output)
        .output()
        .expect("Failed to run gcc output");

    if Path::new(output).exists() {
        fs::remove_file(output).unwrap();
    }

    (
        out.status.code().unwrap_or(-1),
        String::from_utf8(out.stdout).unwrap().trim().to_string(),
    )
}

fn run_rustcc<T: AsRef<str>>(
    files: &[T],
    output: &str,
    asm_path: &str,
    args: &TestArgs,
) -> Option<(i32, String)> {
    let mut cmd = Command::new("cargo");
    cmd.arg("run")
        .args(cargo_profile(args.debug))
        .args(["--quiet", "--", "-o", output])
        .stderr(Stdio::null());
    if args.parse_only {
        cmd.arg("-p");
    }
    if !args.debug {
        cmd.stdout(Stdio::null());
    }
    cmd.args(files.iter().map(|f| f.as_ref()));

    let mut build_retcode = cmd.status().unwrap().code().unwrap();
    if Path::new(output).exists() || Path::new(asm_path).exists() {
        build_retcode = 0;
    }

    let mut result = None;
    if !args.no_run && build_retcode == 0 {
        let output = executable_cmd(output)
            .output()
            .expect("Failed to run output");

        result = Some((
            output.status.code().unwrap_or(-1),
            String::from_utf8_lossy(&output.stdout).trim().to_string(),
        ))
    }

    if Path::new(asm_path).exists() {
        fs::remove_file(asm_path).unwrap();
    }

    if Path::new(output).exists() {
        fs::remove_file(output).unwrap();
    }

    result
}

fn get_all_stages() -> Vec<u32> {
    fs::read_dir("input")
        .unwrap()
        .filter_map(move |entry| {
            let path = entry.ok()?.path();
            path.is_dir().then(|| {
                path.file_name()?
                    .to_str()?
                    .split('_')
                    .last()?
                    .parse::<u32>()
                    .ok()
            })?
        })
        .collect()
}

fn get_stage_files(stage: u32, kind: &str, pattern_all: &str, filters: &[String]) -> Vec<PathBuf> {
    let root = PathBuf::from(format!("input/stage_{stage}")).join(kind);

    let all: Vec<_> = glob::glob(&format!("{}/{pattern_all}", root.to_str().unwrap()))
        .unwrap()
        .filter_map(Result::ok)
        .collect();

    if filters.is_empty() {
        all
    } else {
        // flt is only of the outermost layer
        let dirs = all
            .iter()
            .map(|p| p.parent().unwrap().to_owned())
            .collect::<HashSet<_>>()
            .into_iter()
            .collect::<Vec<_>>();

        filters
            .iter()
            .flat_map(|flt| {
                dirs.iter().flat_map(move |dir| {
                    glob::glob(&format!("{}/{flt}", dir.to_str().unwrap()))
                        .unwrap()
                        .filter_map(Result::ok)
                })
            })
            .collect()
    }
}

fn test_stage(stage: u32, args: &TestArgs) -> (u32, u32) {
    let valid_files = get_stage_files(stage, "valid", "**/*.c", &args.files);
    let invalid_files = get_stage_files(stage, "invalid", "**/*.c", &args.files);
    let multifile_dirs = get_stage_files(stage, "valid_multifile", "*", &args.files);

    //println!("{valid_files:#?}");
    //println!("{invalid_files:#?}");
    //println!("{multifile_dirs:#?}");

    if valid_files.len() + invalid_files.len() == 0 {
        return (0, 0);
    }

    let mut tot_succ = 0;
    let mut tot_fail = 0;

    println!("{BANNER}");
    println!("STAGE {}", stage);

    dbg_banner_centered("Valid Programs");

    for file in &valid_files {
        let file_str = file.to_str().unwrap();
        let out_base = file_str.trim_end_matches(".c");
        let asm_path = format!("{out_base}.s");
        let exe_path = format!("{out_base}{}", exe_extension());

        dbg_file_name(file);
        let gcc = run_gcc(&[file_str], &exe_path, args);
        let rustcc = run_rustcc(&[file_str], &exe_path, &asm_path, args);

        let succ = check(rustcc, Some(gcc));
        tot_succ += succ as u32;
        if !file_str.contains("skip_on_failure") {
            tot_fail += !succ as u32;
        }
    }

    for dir in &multifile_dirs {
        let files: Vec<String> = fs::read_dir(dir)
            .unwrap()
            .filter_map(Result::ok)
            .map(|e| e.path().to_str().unwrap().to_owned())
            .collect();
        let out_name = dir.file_name().unwrap().to_str().unwrap();
        let asm_path = format!("{out_name}.s");
        let exe_path = format!("{out_name}{}", exe_extension());

        dbg_file_name(dir);

        let gcc = run_gcc(&files, &exe_path, args);
        let rustcc = run_rustcc(&files, &exe_path, &asm_path, args);

        let succ = check(rustcc, Some(gcc));
        tot_succ += succ as u32;
        tot_fail += !succ as u32;
    }

    dbg_banner_centered("Invalid Programs");

    for file in &invalid_files {
        let file_str = file.to_str().unwrap();
        let out_base = file_str.trim_end_matches(".c");
        let asm_path = format!("{out_base}.s");
        let exe_path = format!("{out_base}{}", exe_extension());

        dbg_file_name(file);
        let rustcc = run_rustcc(&[file_str], &exe_path, &asm_path, args);

        let succ = check(rustcc, None);
        tot_succ += succ as u32;
        tot_fail += !succ as u32;
    }

    dbg_stage_summary(stage, tot_succ, tot_fail);

    (tot_succ, tot_fail)
}

#[test]
fn test_stages() {
    let args = std::env::args().skip_while(|a| a != "--");

    let mut args = TestArgs::parse_from(args);
    if args.stages.is_empty() {
        args.stages = get_all_stages();
    }

    let mut tot_succ = 0;
    let mut tot_fail = 0;

    for &stage in args.stages.iter() {
        let (succ, fail) = test_stage(stage, &args);
        tot_succ += succ;
        tot_fail += fail;
    }

    dbg_final_summary(tot_succ, tot_fail);
}
