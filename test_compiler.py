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
    print("FAIL")
    print(correct_result, my_result)


def run_correct(files: list[str], exe_name: str) -> tuple[int, str]:
    build_cmd = ["gcc", "-w", "-o", exe_name] + files
    subprocess.run(build_cmd, stdout=subprocess.DEVNULL)

    run_cmd = [exe_name]
    try:
        stdout = (
            subprocess.check_output(run_cmd, stderr=subprocess.DEVNULL).decode().strip()
        )
        retcode = 0
    except subprocess.CalledProcessError as e:
        stdout = ""
        retcode = e.returncode

    if os.path.exists(exe_name):
        os.remove(exe_name)

    return (stdout, retcode)


def run_mine(
    build_mode: str, files: list[str], asm_name: str, exe_name: str
) -> tuple[int, tuple[int, str]]:
    build_cmd = f"cargo run {build_mode} --".split() + ["-o", exe_name] + files
    # print("\n{}\n".format(" ".join(build_cmd)))
    build_retcode = subprocess.run(build_cmd, stderr=subprocess.DEVNULL).returncode

    # if i created some files, then i force retcode okay
    if os.path.exists(asm_name) or os.path.exists(exe_name):
        build_retcode = 0

    stdout = ""
    retcode = 0

    if build_retcode == 0:
        run_cmd = [exe_name]
        try:
            stdout = subprocess.check_output(run_cmd).decode().strip()
        except subprocess.CalledProcessError as e:
            retcode = e.returncode

    if os.path.exists(asm_name):
        os.remove(asm_name)
    if os.path.exists(exe_name):
        os.remove(exe_name)

    return (build_retcode, (stdout, retcode))


def expand_into_possible_files(
    files: list[str], valid_dir: str, multifile_dir: str, invalid_dir: str
) -> list[str]:
    if files:
        tmp = []
        for file in files:
            valid_file = os.path.join(valid_dir, f"{file}.c")
            invalid_file = os.path.join(invalid_dir, f"{file}.c")
            multifile_dir = os.path.join(multifile_dir, file)
            tmp += [valid_file, invalid_file, multifile_dir]
        return tmp
    return []


def get_stage_files(
    possible_files: list[str], valid_dir: str, multifile_dir: str, invalid_dir: str
) -> tuple[list[str], list[str], list[str]]:
    def callback(f):
        return f in possible_files

    valid_prog_pattern = os.path.join(valid_dir, "**", "*.c")
    valid_files = glob.glob(valid_prog_pattern, recursive=True)

    multi_file_pattern = os.path.join(multifile_dir, "*")
    multifile_dirs = glob.glob(multi_file_pattern)

    invalid_prog_pattern = os.path.join(invalid_dir, "**", "*.c")
    invalid_files = glob.glob(invalid_prog_pattern, recursive=True)

    if len(possible_files):
        valid_files = list(filter(callback, valid_files))
        multifile_dirs = list(filter(callback, multifile_dirs))
        invalid_files = list(filter(callback, invalid_files))

    return (valid_files, multifile_dirs, invalid_files)


def test_stage(stage: int, build_mode: str, input_files: list[str]) -> tuple[int]:
    successes = 0
    failures = 0

    stage_dir = os.path.join("input", f"stage_{stage}")
    valid_dir = os.path.join(stage_dir, "valid")
    multifile_dir = os.path.join(stage_dir, "valid_multifile")
    invalid_dir = os.path.join(stage_dir, "invalid")

    # I don't know which one it is, so i add all possibilites, and then filter the existing files with this
    input_files = expand_into_possible_files(
        input_files, valid_dir, multifile_dir, invalid_dir
    )

    valid_files, multifile_dirs, invalid_files = get_stage_files(
        input_files, valid_dir, multifile_dir, invalid_dir
    )

    # If i don't have anything, i quit
    if not len(valid_files + multifile_dirs + invalid_files):
        return (successes, failures)

    print_stage_header(stage)

    # Valid programs
    if len(valid_files) or len(multifile_dirs):
        print("===================Valid Programs===================")

    for prog in valid_files:
        base_name = os.path.splitext(prog)[0]
        asm_name = f"{base_name}.s"
        exe_name = f"{base_name}.exe"
        test_name = os.path.basename(base_name)
        files = [prog]

        print_test_name(test_name)

        correct_result = run_correct(files, exe_name)
        (_, my_result) = run_mine(build_mode, files, asm_name, exe_name)

        if correct_result == my_result:
            successes += 1
            print_success()
        else:
            failures += 1
            print_failure(correct_result, my_result)

    # Multi-file programs
    for dir in multifile_dirs:
        test_name = os.path.basename(base_name)
        asm_name = exe_name = test_name
        files = glob.glob(f"{dir}/*")

        print_test_name(test_name)

        correct_result = run_correct(files)
        (_, my_result) = run_mine(build_mode, files, asm_name, exe_name)

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
        base = os.path.splitext(prog)[0]
        asm_name = f"{base}.s"
        exe_name = f"{base}.exe"
        test_name = os.path.basename(base)
        files = [prog]

        print_test_name(test_name)

        (build_retcode, my_result) = run_mine(build_mode, files, asm_name, exe_name)

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
    args = parser.parse_args()

    total_successes = 0
    total_failures = 0

    if args.debug:
        build_mode = "--profile dev"
    else:
        build_mode = "--profile release"

    build_compiler_cmd = f"cargo build {build_mode}".split()

    try:
        proc = subprocess.run(build_compiler_cmd, stderr=subprocess.DEVNULL)
    except Exception:
        pass

    if args.stages and len(args.stages):
        stages = [int(stage) for stage in args.stages]
    else:
        stages = list(range(1, MAX_STAGES + 1))

    for stage in stages:
        (successes, failures) = test_stage(stage, build_mode, args.files)
        total_successes += successes
        total_failures += failures

    print_total_summary(total_successes, total_failures)
