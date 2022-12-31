"""
setup for the tests of the module string_to_lisp
"""
import general_utilities as gu

from string_to_code import to_lisp


def get_lisp_interpreter():
    """returns the name of the lisp interpreter"""
    return "clisp"


def _get_source_code_file_extension():
    return "lsp"


def run_lisp_code(in_code, tmp_folder):
    """
    Runs the lisp code in_code.
    Returns the output of the program.
    """
    source_filename = gu.get_unique_filename(
        tmp_folder, _get_source_code_file_extension()
    )
    gu.save_str_to_file(tmp_folder / source_filename, in_code)
    res = gu.subprocess_run_with_check(
        [get_lisp_interpreter(), source_filename],
        cwd=str(tmp_folder),
        capture_output=True,
        text=True,
    )
    return res


def get_test_data():
    """returns test data for the module string_to_lisp"""
    return gu.Language(
        tool_names=[get_lisp_interpreter()],
        string_to_code=to_lisp.proc,
        printer_program_to_code=[],
        run_code=run_lisp_code,
        id="lisp",
        source_code_file_extension=_get_source_code_file_extension(),
    )
