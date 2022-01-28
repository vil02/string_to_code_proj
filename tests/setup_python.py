"""
setup for the tests of the module string_to_python
"""
import subprocess
import general_utilities as gu

import string_to_code.to_python as to_python


def get_python_interpreter():
    """returns the name of the python interpreter"""
    return 'python3'


def get_pylint():
    """returns the name of pylint"""
    return 'pylint'


def run_python_code(in_code, tmp_folder):
    """
    Runs the python code in_code.
    Returns the output of the program.
    """
    source_filename = gu.get_unique_filename(tmp_folder, 'py')
    gu.save_str_to_file(tmp_folder/source_filename, in_code)

    subprocess.run(
        [get_pylint(),
         source_filename,
         '--disable=missing-module-docstring,'
         'missing-function-docstring,'
         'too-many-lines'],
        cwd=str(tmp_folder),
        check=True,
        capture_output=True,
        text=True)

    res = subprocess.run(
        [get_python_interpreter(), source_filename],
        cwd=str(tmp_folder),
        check=True,
        capture_output=True,
        text=True)
    return res


def get_test_data():
    """returns test data for the module string_to_python"""
    return gu.Language(
        [get_python_interpreter(), get_pylint()],
        to_python.proc,
        run_python_code,
        'python')
