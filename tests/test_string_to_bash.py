"""
test of the module string_to_bash
"""
import unittest
import subprocess

import general_utilities as gu
import string_to_bash as stc
import base_string_to_code_test


def get_bash():
    """returns the name of the bash interpreter"""
    return 'bash'


def get_shellcheck():
    """returns shellcheck"""
    return 'shellcheck'


def run_bash_code(in_code):
    """
    Runs the bash code in_code.
    Returns the output of the program.
    """
    source_filename = gu.get_unique_filename('sh')
    gu.save_str_to_file(
        gu.get_tmp_test_folder_path()/source_filename, in_code)
    subprocess.run(
        [get_shellcheck(), source_filename],
        cwd=str(gu.get_tmp_test_folder_path()),
        check=True)
    res = subprocess.run(
        [get_bash(), source_filename],
        cwd=str(gu.get_tmp_test_folder_path()),
        check=True,
        capture_output=True,
        text=True)
    return res


class TestSetup(unittest.TestCase):
    """
    tests verifying if the setup of the system is suitable
    to perform other tests
    """
    def test_interpreter(self):
        gu.check_version(get_bash())

    def test_linter(self):
        gu.check_version(get_shellcheck())


class TestStringToLisp(base_string_to_code_test.BaseStringToCode):
    """
    unit tests for the function string_to_lisp.proc
    """
    __test__ = True

    def str_to_code(_, in_str):
        return stc.proc(in_str)

    def run_code(_, in_code):
        return run_bash_code(in_code)


if __name__ == '__main__':
    unittest.main()
