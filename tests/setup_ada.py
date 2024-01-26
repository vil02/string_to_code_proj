"""
setup for the tests of the module string_to_ada
"""

import general_utilities as gu

from string_to_code import to_ada


def get_compiler():
    """returns the name of the Ada compiler"""
    return "gnatmake"


def _get_source_code_file_extension():
    return "adb"


def compile_code(in_code, tmp_folder):
    """
    Compiles the Ada in_code and returns the filename of the output.
    The output file is located in res_folder
    """
    res_folder = tmp_folder / gu.get_unique_foldername(tmp_folder)
    res_folder.mkdir(parents=False, exist_ok=False)
    program_name = to_ada.default_program_name().lower()
    source_filename = program_name + "." + _get_source_code_file_extension()
    gu.save_str_to_file(res_folder / source_filename, in_code)

    gu.subprocess_run_with_check(
        [
            get_compiler(),
            "-we",
            source_filename,
        ],
        cwd=str(res_folder),
    )
    return program_name, res_folder


def run_code(in_code, tmp_folder):
    """
    Compiles and executes the Ada code in_code.
    Returns the output of the program.
    """
    return gu.run_executable(*compile_code(in_code, tmp_folder))


def get_test_data():
    """returns test data for the module string_to_ada"""
    return gu.Language(
        tool_names=[get_compiler()],
        string_to_code=to_ada.proc,
        printer_program_to_code=to_ada.proc_printer_program,
        run_code=run_code,
        id="ada",
        source_code_file_extension=_get_source_code_file_extension(),
    )
