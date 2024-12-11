"""
setup for the tests of the module string_to_ballerina
"""

import general_utilities as gu

from string_to_code import to_ballerina


def get_bal():
    """returns the name of the ballerina interpreter"""
    return "bal"


def _get_source_code_file_extension():
    return "bal"


def run_code(in_code, tmp_folder):
    """
    Runs the ballerina code in_code.
    Returns the output of the program.
    """
    source_filename = gu.get_unique_filename(
        tmp_folder, _get_source_code_file_extension()
    )
    gu.save_str_to_file(tmp_folder / source_filename, in_code)

    res = gu.subprocess_run_with_check(
        [get_bal(), "run", "--offline", source_filename],
        cwd=str(tmp_folder),
        capture_output=True,
        text=True,
    )
    return res


def get_test_data():
    """returns test data for the module string_to_ballerina"""
    return gu.Language(
        tool_names=[get_bal()],
        string_to_code=to_ballerina.proc,
        printer_program_to_code=to_ballerina.proc_printer_program,
        run_code=run_code,
        id="ballerina",
        source_code_file_extension=_get_source_code_file_extension(),
    )
