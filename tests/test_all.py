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
    gu.check_version(tool_name)


@pytest.fixture(params=[
    'Hello World!',
    'a',
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
def example_string(request):
    yield request.param


def test_string_to_code(tmp_path, fun_pair, example_string):
    """
    basic test of the string_to_code type function
    """
    str_to_code_fun, run_code_fun = fun_pair
    source_code = str_to_code_fun(example_string)
    executable_output = run_code_fun(source_code, tmp_path)
    check_output(executable_output, example_string)
#
#
# def test_string_to_code_iteration(
#         tmp_path, fun_pair, example_string, iteration_size):
#     """
#     tests the iterations of the string_to_code function
#     """
#     str_to_code_fun, run_code_fun = fun_pair
#     cur_string = example_string
#     for _ in range(iteration_size):
#         cur_code = str_to_code_fun(cur_string)
#         check_output(run_code_fun(cur_code, tmp_path), cur_string)
#         cur_string = cur_code
#
#
# @pytest.mark.parametrize(
#     'composition_chain',
#     itertools.product(all_language_data.get_all_function_pairs(), repeat=2),
#     ids=('-'.join(_) for _ in itertools.product(all_language_data.get_all_ids(), repeat=2)))
# def test_string_to_code_composition(
#         tmp_path, example_string, composition_chain):
#     cur_string = example_string
#     for string_to_code, run_code in composition_chain:
#         cur_code = string_to_code(cur_string)
#         check_output(run_code(cur_code, tmp_path), cur_string)
#         cur_string = cur_code
