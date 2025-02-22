import os
import subprocess
import glob
import argparse

MAX_STAGES: int = 10
MAX_PADDING: int = 50


def print_stage_header(stage: int):
    print("====================================================")
    print(f"STAGE {stage}")


def print_test_name(test_name: str):
    padding = MAX_PADDING - len(test_name)
    msg = test_name + "." * padding
    print(msg, end="")


def print_stage_summary(stage: int, successes: int, failures: int):
    print(f"===================Stage {stage} Summary=================")
    print(f"{successes} successes, {failures} failures")


def print_total_summary(total_successes: int, total_failures: int):
    print("===================TOTAL SUMMARY====================")
    print(f"{total_successes} successes, {total_failures} failures")


def print_success():
    print("OK")


def print_failure(correct_result: tuple[int, str], my_result: tuple[int, str]):
    print(f"FAIL => correct: {correct_result}, mine: {my_result}")


def get_exe_extension() -> str:
    return ".exe" if os.name == "nt" else ""


def get_executable_command(exe_name: str) -> list[str]:
    return [exe_name] if os.name == "nt" else [f"./{exe_name}"]


def get_build_mode(debug_mode: bool) -> str:
    if debug_mode:
        return "--profile dev"
    else:
        return "--profile release"


def run_correct(files: list[str], out_name: str, exe_name: str) -> tuple[int, str]:
    build_cmd = ["gcc", "-w", "-o", out_name] + files
    subprocess.run(build_cmd, stdout=subprocess.DEVNULL)

    run_cmd = get_executable_command(exe_name)
    proc = subprocess.run(run_cmd, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
    stdout = proc.stdout.decode().strip()
    retcode = proc.returncode

    if os.path.exists(exe_name):
        os.remove(exe_name)

    return (stdout, retcode)


def run_mine(
    debug_mode: bool,
    run_exe: bool,
    keep_exe: bool,
    files: list[str],
    asm_name: str,
    out_name: str,
    exe_name: str,
) -> tuple[int, tuple[int, str]]:
    build_cmd = f"cargo run {get_build_mode(debug_mode)} --".split() + ["-o", out_name] + files

    if debug_mode:
        build_retcode = subprocess.run(build_cmd).returncode
    else:
        build_retcode = subprocess.run(build_cmd, stderr=subprocess.DEVNULL).returncode

    # if i created some files, then i force retcode okay
    if os.path.exists(asm_name) or os.path.exists(exe_name):
        build_retcode = 0

    if build_retcode == 0 and run_exe:
        run_cmd = get_executable_command(exe_name)
        proc = subprocess.run(run_cmd, stdout=subprocess.PIPE)
        stdout = proc.stdout.decode().strip()
        retcode = proc.returncode
    else:
        stdout = None
        retcode = None

    if os.path.exists(asm_name):
        os.remove(asm_name)

    if os.path.exists(exe_name):
        # run_correct will run after this, create one with same name and delete it
        if keep_exe:
            os.rename(exe_name, os.path.join(os.path.dirname(os.path.realpath(__file__)), os.path.basename(exe_name)))
        else:
            os.remove(exe_name)

    return (build_retcode, (stdout, retcode))


def expand_into_possible_files(files: list[str], valid_dir: str, multifile_dir: str, invalid_dir: str) -> list[str]:
    possible = []

    if not files:
        files = []

    for file in files:
        if "*" in file:
            possible += glob.glob(os.path.join(valid_dir, file))
            possible += glob.glob(os.path.join(invalid_dir, file))
            possible += glob.glob(os.path.join(multifile_dir, file))
        else:
            possible.append(os.path.join(valid_dir, f"{file}.c"))
            possible.append(os.path.join(invalid_dir, f"{file}.c"))
            possible.append(os.path.join(multifile_dir, file))

    return possible


def get_stage_files(
    possible_files: list[str], valid_dir: str, multifile_dir: str, invalid_dir: str
) -> tuple[list[str], list[str], list[str]]:
    valid_prog_pattern = os.path.join(valid_dir, "**", "*.c")
    valid_files = glob.glob(valid_prog_pattern, recursive=True)

    multi_file_pattern = os.path.join(multifile_dir, "*")
    multifile_dirs = glob.glob(multi_file_pattern)

    invalid_prog_pattern = os.path.join(invalid_dir, "**", "*.c")
    invalid_files = glob.glob(invalid_prog_pattern, recursive=True)

    def filter_possible(files: list[str]) -> list[str]:
        return list(filter(lambda f: f in possible_files, files))

    if len(possible_files):
        valid_files = filter_possible(valid_files)
        multifile_dirs = filter_possible(multifile_dirs)
        invalid_files = filter_possible(invalid_files)

    return (valid_files, multifile_dirs, invalid_files)


def test_stage(stage: int, debug_mode: bool, run_exe: bool, keep_exe: bool, input_files: list[str]) -> tuple[int]:
    successes = 0
    failures = 0

    stage_dir = os.path.join("input", f"stage_{stage}")
    valid_dir = os.path.join(stage_dir, "valid")
    multifile_dir = os.path.join(stage_dir, "valid_multifile")
    invalid_dir = os.path.join(stage_dir, "invalid")

    # I don't know which one it is, so i add all possibilites, and then filter the existing files with this
    input_files = expand_into_possible_files(input_files, valid_dir, multifile_dir, invalid_dir)

    valid_files, multifile_dirs, invalid_files = get_stage_files(input_files, valid_dir, multifile_dir, invalid_dir)

    # If i don't have anything, i quit
    if not len(valid_files + multifile_dirs + invalid_files):
        return (successes, failures)

    print_stage_header(stage)

    # Valid programs
    if len(valid_files) or len(multifile_dirs):
        print("===================Valid Programs===================")

    for prog in valid_files:
        out_name = os.path.splitext(prog)[0]
        asm_name = f"{out_name}.s"
        exe_name = f"{out_name}{get_exe_extension()}"
        test_name = os.path.basename(out_name)
        files = [prog]

        print_test_name(test_name)

        (_, my_result) = run_mine(debug_mode, run_exe, keep_exe, files, asm_name, out_name, exe_name)

        if run_exe:
            correct_result = run_correct(files, out_name, exe_name)
        else:
            correct_result = my_result

        if correct_result == my_result:
            successes += 1
            print_success()
        else:
            if "skip_on_failure" not in prog:
                failures += 1
            print_failure(correct_result, my_result)

    # Multi-file programs
    for dir in multifile_dirs:
        out_name = os.path.basename(dir)
        asm_name = exe_name = test_name = out_name
        files = glob.glob(f"{dir}/*")

        print_test_name(test_name)

        correct_result = run_correct(files, out_name, exe_name)
        (_, my_result) = run_mine(debug_mode, run_exe, keep_exe, files, asm_name, out_name, exe_name)

        if correct_result == my_result:
            successes += 1
            print_success()
        else:
            failures += 1
            print_failure(correct_result, my_result)

    # Invalid programs
    if len(invalid_files):
        print("===================Invalid Programs=================")

    for prog in invalid_files:
        out_name = os.path.splitext(prog)[0]
        asm_name = f"{out_name}.s"
        exe_name = f"{out_name}{get_exe_extension()}"
        test_name = os.path.basename(out_name)
        files = [prog]

        print_test_name(test_name)

        (build_retcode, my_result) = run_mine(debug_mode, run_exe, keep_exe, files, asm_name, out_name, exe_name)

        # Build failure is success
        if build_retcode != 0:
            successes += 1
            print_success()
        else:
            failures += 1
            print_failure((None, None), (build_retcode, None))

    print_stage_summary(stage, successes, failures)

    return (successes, failures)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="test rustcc")
    parser.add_argument("-d", "--debug", action="store_true", help="debug mode")
    parser.add_argument("-s", "--stages", nargs="*", help="stages")
    parser.add_argument("-f", "--files", nargs="*", help="files")
    parser.add_argument("-n", "--no-run", action="store_true", help="don't run my executable")
    parser.add_argument("-k", "--keep-exe", action="store_true", help="keep my executable")
    args = parser.parse_args()

    total_successes = 0
    total_failures = 0

    build_cc_cmd = f"cargo build {get_build_mode(args.debug)}".split()

    proc = subprocess.run(build_cc_cmd, stderr=subprocess.PIPE)
    if proc.returncode:
        print(proc.stderr.decode().strip())
        exit()

    if args.stages and len(args.stages):
        stages = [int(stage) for stage in args.stages]
    else:
        stages = list(range(1, MAX_STAGES + 1))

    for stage in stages:
        (successes, failures) = test_stage(stage, args.debug, not args.no_run, args.keep_exe, args.files)
        total_successes += successes
        total_failures += failures

    print_total_summary(total_successes, total_failures)
