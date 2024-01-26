"""
setup for the tests of the module string_to_python
"""

import general_utilities as gu

from string_to_code import to_python3


def get_python_interpreter():
    """returns the name of the python interpreter"""
    return "python3"


def get_pylint():
    """returns the name of pylint"""
    return "pylint"


def get_flake8():
    """returns the name of flake8"""
    return "flake8"


def _get_source_code_file_extension():
    return "py"


def run_python_code(in_code, tmp_folder):
    """
    Runs the python code in_code.
    Returns the output of the program.
    """
    source_filename = gu.get_unique_filename(
        tmp_folder, _get_source_code_file_extension()
    )
    gu.save_str_to_file(tmp_folder / source_filename, in_code)

    gu.subprocess_run_with_check(
        [
            get_pylint(),
            source_filename,
            "--disable=missing-module-docstring,"
            "missing-function-docstring,"
            "too-many-lines",
        ],
        cwd=str(tmp_folder),
        capture_output=True,
        text=True,
    )

    gu.subprocess_run_with_check(
        [get_flake8(), source_filename, "--count"],
        cwd=str(tmp_folder),
        capture_output=True,
        text=True,
    )

    res = gu.subprocess_run_with_check(
        [get_python_interpreter(), source_filename],
        cwd=str(tmp_folder),
        capture_output=True,
        text=True,
    )
    return res


def get_test_data():
    """returns test data for the module string_to_python"""
    return gu.Language(
        tool_names=[get_python_interpreter(), get_pylint(), get_flake8()],
        string_to_code=to_python3.proc,
        printer_program_to_code=to_python3.proc_printer_program,
        run_code=run_python_code,
        id="python3",
        source_code_file_extension=_get_source_code_file_extension(),
    )
