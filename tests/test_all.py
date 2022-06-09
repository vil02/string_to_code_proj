"""tests for all of the to_*.py modules"""

import itertools
import pytest
import general_utilities as gu


def check_output(in_ex_output, in_target_str):
    """
    does all of the checks of the program output against the expected
    result
    """
    if len(in_ex_output.stdout)-1 == len(in_target_str):
        # Some of the interpreters add newline symbol at the end of the output.
        assert in_ex_output.stdout[-1] == '\n'
        assert in_target_str[-1] != '\n'
        assert in_ex_output.stdout[:-1] == in_target_str
    else:
        assert in_ex_output.stdout == in_target_str
    assert not in_ex_output.stderr


def test_tool(tool_name):
    """checks if a program named tool_name is available in the system"""
    gu.check_version(tool_name)


@pytest.fixture(name='example_string', params=[
    'Hello World!',
    'a',
    'ab',
    '\n',
    ' ',
    ';'
    '\\',
    '\n'.join(['Line 1', 'Line 2']),
    '',
    '#',
    'aaa\tb\nccc\t#',
    '\'',
    '\"',
    '~',
    '{',
    '`'])
def fixture_example_string(request):
    """fixture returing an example string to test"""
    yield request.param


def test_string_to_code(tmp_path, function_pair, example_string):
    """basic test of the string_to_code type function"""
    source_code = function_pair.string_to_code(example_string)
    executable_output = function_pair.run_code(source_code, tmp_path)
    check_output(executable_output, example_string)


def test_string_to_code_iteration(
        tmp_path, function_pair, example_string, iteration_size):
    """tests iterations of single string_to_code function"""
    cur_string = example_string
    for _ in range(iteration_size):
        cur_code = function_pair.string_to_code(cur_string)
        check_output(function_pair.run_code(cur_code, tmp_path), cur_string)
        cur_string = cur_code


def test_string_to_code_composition(
        tmp_path, example_string, composition_chain):
    """tests compositions of different string_to_code functions"""
    cur_string = example_string
    for string_to_code, run_code in composition_chain:
        cur_code = string_to_code(cur_string)
        check_output(run_code(cur_code, tmp_path), cur_string)
        cur_string = cur_code


def test_code_with_custom_function_names(function_pair):
    """checks if generated code contains custom function names"""
    function_names = (f'custom_fun_{_}' for _ in itertools.count(100))
    code_str = function_pair.string_to_code(
        'some_example string!', function_names)
    assert 'custom_fun_100' in code_str
