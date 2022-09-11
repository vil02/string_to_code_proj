"""
setup for the tests of the module string_to_lisp
"""
import subprocess
import general_utilities as gu

from string_to_code import to_lisp


def get_lisp_interpreter():
    """returns the name of the lisp interpreter"""
    return "clisp"


def run_lisp_code(in_code, tmp_folder):
    """
    Runs the lisp code in_code.
    Returns the output of the program.
    """
    source_filename = gu.get_unique_filename(tmp_folder, "lsp")
    gu.save_str_to_file(tmp_folder / source_filename, in_code)
    res = subprocess.run(
        [get_lisp_interpreter(), source_filename],
        cwd=str(tmp_folder),
        check=True,
        capture_output=True,
        text=True,
    )
    return res


def get_test_data():
    """returns test data for the module string_to_lisp"""
    return gu.Language(
        [get_lisp_interpreter()], to_lisp.proc, run_lisp_code, "lisp"
    )
