"""
test of the module string_to_cpp
"""
import unittest
import subprocess

import general_utilities as gu
import string_to_cpp as stc
import base_string_to_code_test


def get_cpp_compiler():
    """returns the name of the c++ compiler"""
    return 'g++'


def compile_cpp_code(in_code):
    """
    Compiles the C++ in_code and returns the filename of the output.
    The output file is located in gu.get_tmp_test_folder_path()
    """
    source_filename = gu.get_unique_filename('cpp')
    executable_filename = gu.get_unique_filename('o')
    gu.save_str_to_file(
        gu.get_tmp_test_folder_path()/source_filename, in_code)
    subprocess.run(
        [get_cpp_compiler(), source_filename,
         '-Werror', '-Wpedantic', '-Wall', '-Wextra',
         '-o', executable_filename],
        cwd=str(gu.get_tmp_test_folder_path()),
        check=True)
    return executable_filename


def run_executable(in_executable_name):
    """
    runs the executable in_executable_name
    in the folder gu.get_tmp_test_folder_path()
    """
    assert (gu.get_tmp_test_folder_path()/in_executable_name).is_file()
    return subprocess.run(
        ['./'+in_executable_name],
        cwd=str(gu.get_tmp_test_folder_path()),
        check=True,
        capture_output=True,
        text=True)


def run_cpp_code(in_code):
    """
    Compiles and executes the C++ code in_code.
    Returns the output of the program.
    """
    return run_executable(compile_cpp_code(in_code))


class TestSetup(unittest.TestCase):
    """
    tests verifying if the setup of the system is suitable
    to perform other tests
    """
    def test_compiler(self):
        subprocess.run(
            [get_cpp_compiler(), '--version'],
            check=True,
            capture_output=True)


class TestStringToCpp(base_string_to_code_test.BaseStringToCode):
    """
    unit tests for the function string_to_cpp.proc
    """
    __test__ = True

    def str_to_code(_, in_str):
        return stc.proc(in_str)

    def run_code(_, in_code):
        return run_cpp_code(in_code)


if __name__ == '__main__':
    unittest.main()
