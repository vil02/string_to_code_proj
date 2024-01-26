"""
setup for the tests of the module string_to_ALGOL 68
"""

import general_utilities as gu

from string_to_code import to_algol68


def get_algol68_interpreter():
    """returns the name of the ALGOL 68 interpreter"""
    return "a68g"


def _get_source_code_file_extension():
    return "alg"


def run_algol68_code(in_code, tmp_folder):
    """
    Runs the ALGOL 68 code in_code.
    Returns the output of the program.
    """
    source_filename = gu.get_unique_filename(
        tmp_folder, _get_source_code_file_extension()
    )
    gu.save_str_to_file(tmp_folder / source_filename, in_code)

    res = gu.subprocess_run_with_check(
        [get_algol68_interpreter(), "--pedantic", "--strict", source_filename],
        cwd=str(tmp_folder),
        capture_output=True,
        text=True,
    )
    return res


def get_test_data():
    """returns test data for the module string_to_ALGOL 68"""
    return gu.Language(
        tool_names=[get_algol68_interpreter()],
        string_to_code=to_algol68.proc,
        printer_program_to_code=to_algol68.proc_printer_program,
        run_code=run_algol68_code,
        id="algol68",
        source_code_file_extension=_get_source_code_file_extension(),
    )
