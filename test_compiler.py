import os
import subprocess
import glob
import argparse

MAX_STAGES: int = 10
MAX_PADDING: int = 50
DEVNULL = subprocess.DEVNULL


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
    print("FAIL")
    print(correct_result, my_result)


def run_correct(files: list[str], exe_name: str) -> tuple[int, str]:
    build_cmd = ["gcc", "-w", "-o", exe_name] + files
    subprocess.run(build_cmd, stdout=DEVNULL, stderr=DEVNULL)

    run_cmd = [exe_name]
    try:
        stdout = subprocess.check_output(run_cmd, stderr=DEVNULL).decode().strip()
        retcode = 0
    except subprocess.CalledProcessError as e:
        stdout = ""
        retcode = e.returncode

    if os.path.exists(exe_name):
        os.remove(exe_name)

    return (stdout, retcode)


def run_mine(
    build_cmd: list[str], files: list[str], asm_name: str, exe_name: str
) -> tuple[int, tuple[int, str]]:
    build_cmd = build_cmd + ["-o", exe_name] + files
    build_retcode = subprocess.run(build_cmd, stdout=DEVNULL, stderr=DEVNULL).returncode

    # if i created some files, then i force retcode okay
    if os.path.exists(asm_name) or os.path.exists(exe_name):
        build_retcode = 0

    stdout = ""
    retcode = 0

    if build_retcode == 0:
        run_cmd = [exe_name]
        try:
            stdout = subprocess.check_output(run_cmd, stderr=DEVNULL).decode().strip()
        except subprocess.CalledProcessError as e:
            retcode = e.returncode

    if os.path.exists(asm_name):
        os.remove(asm_name)
    if os.path.exists(exe_name):
        os.remove(exe_name)

    return (build_retcode, (stdout, retcode))


def test_stage(stage: int, build_cmd: list[str]) -> tuple[int]:
    successes = 0
    failures = 0

    print_stage_header(stage)

    print("===================Valid Programs===================")

    # Valid programs
    src_pattern = os.path.join(".", "input", f"stage_{stage}", "valid", "**", "*.c")

    for prog in glob.glob(src_pattern, recursive=True):
        base_name = os.path.splitext(prog)[0]
        asm_name = f"{base_name}.s"
        exe_name = f"{base_name}.exe"
        test_name = os.path.basename(base_name)
        files = [prog]

        print_test_name(test_name)

        correct_result = run_correct(files, exe_name)
        (_, my_result) = run_mine(build_cmd, files, asm_name, exe_name)

        if correct_result == my_result:
            successes += 1
            print_success()
        else:
            failures += 1
            print_failure(correct_result, my_result)

    # Multi-file programs
    src_pattern = os.path.join("input", f"stage_{stage}", "valid_multifile", "*")

    for dir in glob.glob(src_pattern):
        test_name = os.path.basename(base_name)
        asm_name = exe_name = test_name
        files = glob.glob(f"{dir}/*")

        print_test_name(test_name)

        correct_result = run_correct(files)
        (_, my_result) = run_mine(build_cmd, files, asm_name, exe_name)

        if correct_result == my_result:
            successes += 1
            print_success()
        else:
            failures += 1
            print_failure(correct_result, my_result)

    print("===================Invalid Programs=================")

    # Invalid programs
    src_pattern = os.path.join("input", f"stage_{stage}", "invalid", "**", "*.c")

    for prog in glob.glob(src_pattern, recursive=True):
        base = os.path.splitext(prog)[0]
        asm_name = f"{base}.s"
        exe_name = f"{base}.exe"
        test_name = os.path.basename(base)
        files = [prog]

        print_test_name(test_name)

        (build_retcode, my_result) = run_mine(build_cmd, files, asm_name, exe_name)

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
    parser.add_argument("-s", "--stage", nargs="*", help="stages")
    parser.add_argument("-f", "--file", nargs="*", help="files")
    args = parser.parse_args()

    total_successes = 0
    total_failures = 0

    if args.debug:
        build_mode = "--debug"
    else:
        build_mode = "--release"

    build_compiler_cmd = f"cargo build {build_mode}".split()

    try:
        proc = subprocess.run(build_compiler_cmd)
    except Exception:
        pass

    if len(args.stage):
        stages = [int(stage) for stage in args.stage]
    else:
        stages = list(range(MAX_STAGES))

    build_cmd = f"cargo run {build_mode} -- ".split()

    for stage in stages:
        (successes, failures) = test_stage(stage, build_cmd)
        total_successes += successes
        total_failures += failures

    print_total_summary(total_successes, total_failures)
