"""
tests of the string_to_code module
"""

import unittest
import subprocess

import general_utilities as gu
import string_to_code_module.string_to_code as stc


class TestStingToCodeCpp(unittest.TestCase):
    """
    unit tests for the function str_to_cpp
    """

    def setUp(self):
        gu.create_tmp_test_folder()

    def tearDown(self):
        pass
        gu.delete_tmp_test_folder()

    def test_str_to_cpp(self):
        input_str = 'Hello World!'
        result_str = stc.str_to_cpp(input_str)
        file_name = 'hello_world.cpp'
        executable_name = 'hello_world.o'
        gu.save_str_to_file(
            gu.get_tmp_test_folder_path()/file_name, result_str)
        subprocess.run(
            ['g++', file_name,
             '-Werror', '-Wpedantic', '-Wall', '-Wextra',
             '-o', executable_name],
            cwd=str(gu.get_tmp_test_folder_path()),
            check=True)

        executable_output = subprocess.run(
            ['./'+executable_name],
            cwd=str(gu.get_tmp_test_folder_path()),
            check=True,
            capture_output=True,
            text=True)
        self.assertEqual(executable_output.stdout, input_str)
        self.assertEqual(executable_output.stderr, '');

if __name__ == '__main__':
    unittest.main()
