"""tests for all of the to_*.py modules"""

import pytest
import general_utilities as gu


def test_tool(tool_name):
    """checks if a program named tool_name is available in the system"""
    gu.check_version(tool_name)


@pytest.fixture(
    name="example_string",
    params=[
        "Hello World!",
        "\n",
        " ",
        ";",
        "\\",
        "\n".join(["Line 1", "Line 2"]),
        "",
        "#",
        "aaa\tb\nccc\t#",
        "'",
        '"',
        "~",
        "{",
        "`",
    ],
)
def fixture_example_string(request):
    """fixture returing an example string to test"""
    yield request.param


@pytest.fixture(
    name="example_string_for_long_tests",
    params=[
        "!@#$%^&*()_+[]{\t}|\\/?~`'\"aB\n12.-=",
        "other example<>",
    ],
)
def fixture_example_string_for_long_tests(request):
    """
    fixture returing an example string for long tests like
    test_string_to_code_iteration or test_string_to_code_composition
    """
    yield request.param


def test_string_to_code(tmp_path, function_pair, example_string):
    """basic test of the string_to_code type function"""
    source_code = function_pair.string_to_code(example_string)
    executable_output = function_pair.run_code(source_code, tmp_path)
    gu.check_output(executable_output, example_string)


def test_string_to_code_iteration(
    tmp_path, function_pair, example_string_for_long_tests, iteration_size
):
    """tests iterations of single string_to_code function"""
    cur_string = example_string_for_long_tests
    for _ in range(iteration_size):
        cur_code = function_pair.string_to_code(cur_string)
        gu.check_output(function_pair.run_code(cur_code, tmp_path), cur_string)
        cur_string = cur_code


def test_string_to_code_composition(
    tmp_path, example_string_for_long_tests, composition_chain
):
    """tests compositions of different string_to_code functions"""
    cur_string = example_string_for_long_tests
    for string_to_code, run_code in composition_chain:
        cur_code = string_to_code(cur_string)
        gu.check_output(run_code(cur_code, tmp_path), cur_string)
        cur_string = cur_code


def test_code_with_custom_function_names(function_pair):
    """checks if generated code contains custom function names"""
    code_str = function_pair.string_to_code(
        "some_example string!",
        function_id_to_name=lambda fun_id: "custom_fun_" + str(fun_id + 100),
    )
    assert "custom_fun_100" in code_str
