"""
setup for the tests of the module string_to_c
"""
import general_utilities as gu

import setup_cpp

from string_to_code import to_c


def get_c_compiler():
    """returns the name of the c compiler"""
    return "gcc"


def _get_source_code_file_extension():
    return "c"


def compile_c_code(in_code, tmp_folder):
    """
    Compiles the C in_code and returns the filename of the output.
    The output file is located in tmp_folder
    """
    source_filename = gu.get_unique_filename(
        tmp_folder, _get_source_code_file_extension()
    )
    executable_filename = gu.get_unique_filename(tmp_folder, "o")
    gu.save_str_to_file(tmp_folder / source_filename, in_code)
    gu.subprocess_run_with_check(
        [
            setup_cpp.get_cppcheck(),
            "--enable=all",
            "--addon=cert",
            "--addon=threadsafety",
            "--addon=y2038",
            "--error-exitcode=1",
            "--force",
            "--inconclusive",
            source_filename,
        ],
        cwd=str(tmp_folder),
        capture_output=True,
    )

    gu.subprocess_run_with_check(
        [
            get_c_compiler(),
            source_filename,
            "-Werror",
            "-Wpedantic",
            "-Wall",
            "-Wextra",
            "-o",
            executable_filename,
        ],
        cwd=str(tmp_folder),
    )
    return executable_filename


def run_c_code(in_code, tmp_folder):
    """
    Compiles and executes the C code in_code.
    Returns the output of the program.
    """
    return setup_cpp.run_executable(
        compile_c_code(in_code, tmp_folder), tmp_folder
    )


def get_test_data():
    """returns test data for the module string_to_c"""
    return gu.Language(
        tool_names=[get_c_compiler(), setup_cpp.get_cppcheck()],
        string_to_code=to_c.proc,
        printer_program_to_code=to_c.proc_printer_program,
        run_code=run_c_code,
        id="c",
        source_code_file_extension=_get_source_code_file_extension(),
    )
