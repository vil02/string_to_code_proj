"""
setup for the tests of the module string_to_nim
"""
import general_utilities as gu

import setup_cpp

from string_to_code import to_nim


def get_nim():
    """returns the name of the nim compiler"""
    return "nim"


def _get_source_code_file_extension():
    return "nim"


def compile_nim_code(in_code, tmp_folder):
    """
    Compiles the nim in_code and returns the filename of the output.
    The output file is located in tmp_folder
    """
    source_filename = gu.get_unique_filename(
        tmp_folder, _get_source_code_file_extension()
    )
    executable_filename = gu.get_unique_filename(tmp_folder, "out")
    gu.save_str_to_file(tmp_folder / source_filename, in_code)

    gu.subprocess_run_with_check(
        [
            get_nim(),
            "compile",
            "--styleCheck:error",
            "--out:" + executable_filename,
            "--verbosity:0",
            source_filename,
        ],
        cwd=str(tmp_folder),
    )
    return executable_filename


def run_nim_code(in_code, tmp_folder):
    """
    Compiles and executes the nim code in_code.
    Returns the output of the program.
    """
    return setup_cpp.run_executable(
        compile_nim_code(in_code, tmp_folder), tmp_folder
    )


def get_test_data():
    """returns test data for the module string_to_python"""
    return gu.Language(
        tool_names=[get_nim()],
        string_to_code=to_nim.proc,
        printer_program_to_code=to_nim.proc_printer_program,
        run_code=run_nim_code,
        id="nim",
        source_code_file_extension=_get_source_code_file_extension(),
    )
