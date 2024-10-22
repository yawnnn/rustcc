import os
import subprocess
import glob

PADDING_DOTS = "." * 60
PAD_LENGTH = 50
CMP_DEBUG = "./target/debug/rustcc.exe"
CMP_RELEASE = "./target/release/rustcc.exe"
SUCCESS_TOTAL = 0
FAILURE_TOTAL = 0


def print_test_name(test_name):
    print(f"{test_name}", end="")
    padding = PAD_LENGTH - len(test_name)
    print(f"{PADDING_DOTS[:padding]}", end="")


def test_success():
    global success
    print("OK")
    success += 1


def test_failure(gcc_exit_code, rustcc_exit_code, gcc_stdout, rustcc_out):
    global fail
    print("FAIL")
    print(gcc_exit_code, rustcc_exit_code, gcc_stdout, rustcc_out)
    fail += 1


def test_not_implemented():
    print("NOT IMPLEMENTED")


def run_program_rustcc(asm_name, exe_name):
    try:
        stdout = (
            subprocess.check_output([f"./{exe_name}"], stderr=subprocess.DEVNULL)
            .decode()
            .strip()
        )
        exit_code = 0
    except subprocess.CalledProcessError as e:
        stdout = ""
        exit_code = e.returncode

    # Clean up generated files
    if os.path.exists(asm_name):
        os.remove(asm_name)
    if os.path.exists(exe_name):
        os.remove(exe_name)

    return stdout, exit_code


def run_program_gcc():
    try:
        stdout = (
            subprocess.check_output(["./a.exe"], stderr=subprocess.DEVNULL)
            .decode()
            .strip()
        )
        exit_code = 0
    except subprocess.CalledProcessError as e:
        stdout = ""
        exit_code = e.returncode

    # Remove 'a.exe' after execution
    if os.path.exists("a.exe"):
        os.remove("a.exe")

    return stdout, exit_code


def compare_program_results(gcc_exit_code, rustcc_exit_code, gcc_stdout, rustcc_out):
    if gcc_exit_code != rustcc_exit_code or gcc_stdout != rustcc_out:
        test_failure(gcc_exit_code, rustcc_exit_code, gcc_stdout, rustcc_out)
    else:
        test_success()


def test_stage(stage_num, cmp_program):
    global success, fail
    success = 0
    fail = 0

    print("====================================================")
    print(f"STAGE {stage_num}")
    print("===================Valid Programs===================")

    # Valid programs
    for prog in glob.glob(f"./input/stage_{stage_num}/valid/**/*.c", recursive=True):
        base = os.path.splitext(prog)[0]
        asm_name = f"{base}.s"
        exe_name = f"{base}.exe"
        test_name = os.path.basename(base)

        # Compile and run GCC program
        subprocess.run(
            ["gcc", "-w", prog], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL
        )
        gcc_stdout, gcc_exit_code = run_program_gcc()

        print_test_name(test_name)

        # Compile and run our program
        subprocess.run(
            [cmp_program, prog], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL
        )
        rustcc_out, rustcc_exit_code = run_program_rustcc(asm_name, exe_name)

        compare_program_results(gcc_exit_code, rustcc_exit_code, gcc_stdout, rustcc_out)

    # Multi-file programs
    for dir in glob.glob(f"input/stage_{stage_num}/valid_multifile/*"):
        test_name = os.path.basename(dir)

        # Compile and run GCC program
        subprocess.run(
            ["gcc", "-w"] + glob.glob(f"{dir}/*"),
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )
        gcc_stdout, gcc_exit_code = run_program_gcc()

        # Compile and run our program
        subprocess.run(
            [cmp_program, "-o", test_name] + glob.glob(f"{dir}/*"),
            stdout=subprocess.DEVNULL,
        )
        print_test_name(test_name)

        rustcc_out, rustcc_exit_code = run_program_rustcc(test_name, test_name)

        compare_program_results(gcc_exit_code, rustcc_exit_code, gcc_stdout, rustcc_out)

    print("===================Invalid Programs=================")

    # Invalid programs
    for prog in glob.glob(f"input/stage_{stage_num}/invalid/**/*.c", recursive=True):
        base = os.path.splitext(prog)[0]
        asm_name = f"{base}.s"
        exe_name = f"{base}.exe"
        test_name = os.path.basename(base)

        print_test_name(test_name)

        # Compile invalid program (expect failure)
        status = subprocess.run(
            [cmp_program, prog], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL
        ).returncode

        # Ensure no executable or assembly was produced
        if os.path.exists(exe_name) or os.path.exists(asm_name) or status == 0:
            test_failure(None, status, None, None)
            if os.path.exists(exe_name):
                os.remove(exe_name)
            if os.path.exists(asm_name):
                os.remove(asm_name)
        else:
            test_success()

    print(f"===================Stage {stage_num} Summary=================")
    print(f"{success} successes, {fail} failures")
    global SUCCESS_TOTAL, FAILURE_TOTAL
    SUCCESS_TOTAL += success
    FAILURE_TOTAL += fail


def total_summary():
    print("===================TOTAL SUMMARY====================")
    print(f"{SUCCESS_TOTAL} successes, {FAILURE_TOTAL} failures")


if __name__ == "__main__":
    import sys

    cmp_program = CMP_RELEASE
    if len(sys.argv) > 1 and sys.argv[1] == "dbg":
        cmp_program = CMP_DEBUG
        sys.argv.pop(1)

    if len(sys.argv) > 1:
        stages = [int(arg) for arg in sys.argv[1:]]
        for stage in stages:
            test_stage(stage, cmp_program)
        total_summary()
    else:
        # Default number of stages to test
        NUM_STAGES = 10
        for stage in range(1, NUM_STAGES + 1):
            test_stage(stage, cmp_program)
        total_summary()
