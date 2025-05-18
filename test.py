import os
import subprocess
import glob
import argparse

ROOT_DIR = os.path.dirname(os.path.realpath(__file__))
INPUT_DIR = os.path.join(ROOT_DIR, "input")
BANNER = "=" * 50


def dbg_stage_header(stage: int):
    print(BANNER)
    print(f"STAGE {stage}")


def dbg_test_name(f: str):
    basename = os.path.splitext(os.path.basename(f))[0]
    print(f"{basename:.<{len(BANNER)}}", end="")


def dbg_banner_centered(s: str):
    print(f"{s:=^{len(BANNER)}}")


def dbg_stage_summary(stage: int, succ: int, fail: int):
    dbg_banner_centered(f"Stage {stage} Summary")
    print(f"{succ} successes, {fail} failures")


def dbg_final_summary(succ: int, fail: int):
    dbg_banner_centered("FINAL SUMMARY")
    print(f"{succ} successes, {fail} failures")


def check(found: tuple[int, str], expected: tuple[int, str]):
    succ = found == expected
    if succ:
        print("OK")
    else:
        print(f"ERR found: {found}, expected: {expected}")

    return succ


def get_all_stages():
    for entry in os.listdir(INPUT_DIR):
        entry = os.path.join(INPUT_DIR, entry)
        if not os.path.isfile(entry):
            yield int(entry.split("_")[-1])


def get_exe_extension() -> str:
    return ".exe" if os.name == "nt" else ""


def get_executable_cmd(exe_name: str) -> list[str]:
    return [exe_name] if os.name == "nt" else [f"./{exe_name}"]


def get_profile_cmd(args) -> list[str]:
    if args.debug:
        return "--profile dev".split()
    else:
        return "--profile release".split()


def run_rustcc(
    args,
    files: list[str],
    asm_name: str,
    out_name: str,
    exe_name: str,
) -> tuple[int, tuple[int, str]]:
    build_cmd = ["cargo", "run"] + get_profile_cmd(args) + ["--quiet", "--", "-o", out_name]
    if args.parse_only:
        build_cmd.append("-p")
    build_cmd += files

    if args.debug:
        stdout = None
    else:
        stdout = subprocess.DEVNULL
    build_retcode = subprocess.run(build_cmd, stdout=stdout, stderr=subprocess.DEVNULL).returncode

    # if i created some files, then i force retcode okay
    if os.path.exists(asm_name) or os.path.exists(exe_name):
        build_retcode = 0

    result = None
    if build_retcode == 0 and not args.no_run and not args.parse_only:
        run_cmd = get_executable_cmd(exe_name)
        proc = subprocess.run(run_cmd, stdout=subprocess.PIPE)
        result = (proc.returncode, proc.stdout.decode().strip())

    if os.path.exists(asm_name):
        os.remove(asm_name)

    if os.path.exists(exe_name):
        # run_gcc will run after this, create one with same name and delete it
        if args.keep_exe:
            os.rename(exe_name, os.path.join(ROOT_DIR, os.path.basename(exe_name)))
        else:
            os.remove(exe_name)

    return result


def run_gcc(files: list[str], out_name: str, exe_name: str) -> tuple[int, str]:
    build_cmd = ["gcc", "-w", "-o", out_name] + files
    subprocess.run(build_cmd, stdout=subprocess.DEVNULL)

    run_cmd = get_executable_cmd(exe_name)
    proc = subprocess.run(run_cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    stdout = proc.stdout.decode().strip()
    retcode = proc.returncode

    if os.path.exists(exe_name):
        os.remove(exe_name)

    return (retcode, stdout)


def get_stage_files(stage: int, kind: str, pattern_all: str, filters: list[str]):
    root = os.path.join(INPUT_DIR, f"stage_{stage}", kind)
    files = glob.glob(f"{root}/{pattern_all}", recursive=True)

    if filters:
        dirs = list({os.path.dirname(p) for p in files})

        files = []
        for flt in filters:
            for dir in dirs:
                files.extend(glob.glob(f"{dir}/{flt}", recursive=True))

    return files


def test_stage(
    stage: int,
    args,
) -> tuple[int, int]:
    succ = 0
    fail = 0

    valid_files = get_stage_files(stage, "valid", "**/*.c", args.files)
    invalid_files = get_stage_files(stage, "invalid", "**/*.c", args.files)
    multifile_dirs = get_stage_files(stage, "valid_multifile", "*", args.files)

    if len(valid_files) + len(multifile_dirs) + len(invalid_files) == 0:
        return (0, 0)

    dbg_stage_header(stage)

    dbg_banner_centered("Valid Program")

    for f in valid_files:
        out_name = os.path.splitext(f)[0]
        asm_name = f"{out_name}.s"
        exe_name = f"{out_name}{get_exe_extension()}"
        files = [f]

        dbg_test_name(f)

        rustcc = run_rustcc(args, files, asm_name, out_name, exe_name)
        if not args.no_run:
            gcc = run_gcc(files, out_name, exe_name)
        else:
            gcc = rustcc

        result = check(rustcc, gcc)
        succ += result
        if "skip_on_failure" not in f:
            fail += not result

    # Multi-file programs
    for dir in multifile_dirs:
        out_name = os.path.basename(dir)
        asm_name = exe_name = out_name
        files = glob.glob(f"{dir}/*")

        dbg_test_name(dir)

        rustcc = run_rustcc(args, files, asm_name, out_name, exe_name)
        gcc = run_gcc(files, out_name, exe_name)

        result = check(rustcc, gcc)
        succ += result

    # Invalid programs
    dbg_banner_centered("Invalid Program")

    for f in invalid_files:
        out_name = os.path.splitext(f)[0]
        asm_name = f"{out_name}.s"
        exe_name = f"{out_name}{get_exe_extension()}"
        files = [f]

        dbg_test_name(f)

        rustcc = run_rustcc(args, files, asm_name, out_name, exe_name)

        result = check(rustcc, None)
        succ += result

    dbg_stage_summary(stage, succ, fail)

    return (succ, fail)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="test rustcc")
    parser.add_argument("-s", "--stages", nargs="*", type=int, help="stages")
    parser.add_argument("-f", "--files", nargs="*", help="files")
    parser.add_argument("-d", "--debug", action="store_true", help="debug mode")
    parser.add_argument("-p", "--parse-only", action="store_true", help="only do parsing")
    parser.add_argument("-k", "--keep-exe", action="store_true", help="keep my executable (move it in root dir)")
    parser.add_argument("-n", "--no-run", action="store_true", help="don't run executable")
    args = parser.parse_args()

    if not args.stages:
        args.stages = list(get_all_stages())

    tot_succ = 0
    tot_fail = 0

    for stage in args.stages:
        (succ, fail) = test_stage(stage, args)
        tot_succ += succ
        tot_fail += fail

    dbg_final_summary(tot_succ, tot_fail)
