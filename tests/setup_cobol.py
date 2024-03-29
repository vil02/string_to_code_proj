"""
setup for the tests of the module string_to_cobol
"""

import general_utilities as gu

from string_to_code import to_cobol


def get_cobc():
    """returns the name of the cobol compiler/cobc"""
    return "cobc"


def _get_source_code_file_extension():
    return "cbl"


def run_cobol_code(in_code, tmp_folder):
    """
    Runs the cobol code in_code.
    Returns the output of the program.
    """
    source_filename = gu.get_unique_filename(
        tmp_folder, _get_source_code_file_extension()
    )
    executable_name = gu.get_unique_filename(tmp_folder, "out")
    gu.save_str_to_file(tmp_folder / source_filename, in_code)
    return gu.subprocess_run_with_check(
        [
            get_cobc(),
            "-W",
            "-Wall",
            "-Werror",
            "-x",
            "-j",
            "-free",
            f"-o{executable_name}",
            source_filename,
        ],
        cwd=str(tmp_folder),
        capture_output=True,
        text=True,
    )


def get_test_data():
    """returns test data for the module string_to_cobol"""
    return gu.Language(
        tool_names=[get_cobc()],
        string_to_code=to_cobol.proc,
        printer_program_to_code=to_cobol.proc_printer_program,
        run_code=run_cobol_code,
        id="cobol",
        source_code_file_extension=_get_source_code_file_extension(),
    )
