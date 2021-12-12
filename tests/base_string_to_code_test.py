"""
defines a base class for all string_to_code type tests
"""
import unittest
import general_utilities as gu


class BaseStringToCode(unittest.TestCase):
    __test__ = False

    def setUp(self):
        gu.create_tmp_test_folder()

    def tearDown(self):
        gu.delete_tmp_test_folder()

    def check_output(test_obj, in_ex_output, in_target_str):
        """
        does all of the checks of the program output against the expected
        result
        """
        test_obj.assertEqual(in_ex_output.stdout, in_target_str)
        test_obj.assertEqual(in_ex_output.stderr, '')

    def test_string_to_code(self):
        """
        basic test of the string_to_code type function
        """
        def proc_single(in_str):
            source_code = self.str_to_code(in_str)
            executable_output = self.run_code(source_code)
            self.check_output(executable_output, in_str)
        gu.check_all(proc_single)

    def test_string_to_code_iteration(self):
        """
        tests the iterations of the string_to_code function
        """
        def proc_single(in_str):
            string_list = [in_str]
            max_iteration = 2
            for _ in range(max_iteration):
                string_list.append(self.str_to_code(string_list[-1]))

            for _ in range(max_iteration, 0, -1):
                run_code_fun = self.run_code
                self.check_output(
                    run_code_fun(string_list[_]), string_list[_-1])
        gu.check_all(proc_single)
