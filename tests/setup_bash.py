"""
setup for the tests of the module string_to_bash
"""
import general_utilities as gu

from string_to_code import to_bash


def get_bash():
    """returns the name of the bash interpreter"""
    return "bash"


def get_shellcheck():
    """returns shellcheck"""
    return "shellcheck"


def _get_source_code_file_extension():
    return "sh"


def run_bash_code(in_code, tmp_folder):
    """
    Runs the bash code in_code.
    Returns the output of the program.
    """
    source_filename = gu.get_unique_filename(
        tmp_folder, _get_source_code_file_extension()
    )
    gu.save_str_to_file(tmp_folder / source_filename, in_code)
    gu.subprocess_run_with_check(
        [get_shellcheck(), "--enable=all", source_filename],
        cwd=str(tmp_folder),
    )
    res = gu.subprocess_run_with_check(
        [get_bash(), source_filename],
        cwd=str(tmp_folder),
        capture_output=True,
        text=True,
    )
    return res


def get_test_data():
    """returns test data for the module string_to_bash"""
    return gu.Language(
        tool_names=[get_bash(), get_shellcheck()],
        string_to_code=to_bash.proc,
        printer_program_to_code=to_bash.proc_printer_program,
        run_code=run_bash_code,
        id="bash",
        source_code_file_extension=_get_source_code_file_extension(),
    )
