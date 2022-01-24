"""
setup for the tests of the module string_to_bash
"""
import subprocess
import general_utilities as gu

import string_to_code.to_bash as to_bash


def get_bash():
    """returns the name of the bash interpreter"""
    return 'bash'


def get_shellcheck():
    """returns shellcheck"""
    return 'shellcheck'


def run_bash_code(in_code, tmp_folder):
    """
    Runs the bash code in_code.
    Returns the output of the program.
    """
    source_filename = gu.get_unique_filename(tmp_folder, 'sh')
    gu.save_str_to_file(tmp_folder/source_filename, in_code)
    subprocess.run(
        [get_shellcheck(), source_filename],
        cwd=str(tmp_folder),
        check=True)
    res = subprocess.run(
        [get_bash(), source_filename],
        cwd=str(tmp_folder),
        check=True,
        capture_output=True,
        text=True)
    return res


def get_test_data():
    """returns test data for the module string_to_bash"""
    return gu.SingleTestParam(
        [get_bash(), get_shellcheck()],
        to_bash.proc,
        run_bash_code,
        'bash')
