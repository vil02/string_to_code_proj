import pytest

import all_language_data

_ALL_TEST_DATA = all_language_data.get_all_test_data()


def pytest_addoption(parser):
    parser.addoption(
        "--iteration-size",
        action="store",
        default="2",
        help="Maximal number of iterations in test_string_to_code_iteration")
    parser.addoption(
        "--skip",
        action="append",
        default=[],
        help="list of languages to skip")


def get_tool_list(languages_to_skip):
    res = []
    for cur_langauge_data in _ALL_TEST_DATA:
        cur_language = cur_langauge_data.id
        is_skipped = cur_language in languages_to_skip
        for cur_tool in cur_langauge_data.tool_names:
            res.append(pytest.param(
                cur_tool,
                id=f'{cur_language}-{cur_tool}',
                marks=pytest.mark.skipif(
                    is_skipped, reason='Skipped by command line arguments')))
    return res


def check_format(in_code):
    """Checks the format of the code."""
    code_lines = in_code.split('\n')
    assert not code_lines[-1], "Code must end with empty line."
    if len(code_lines) > 1:
        assert code_lines[-2], "Code must end with exactly single empty line."
    for _ in code_lines:
        if _:
            assert _[-1] not in {' ', '\t'}, "No trailing whitespaces."


def add_check_format(in_string_to_code_fun):
    def inner(in_str):
        res_code = in_string_to_code_fun(in_str)
        check_format(res_code)
        return res_code
    return inner


def get_function_pair_list(languages_to_skip):
    def proc_single(in_language):
        return pytest.param(
            (add_check_format(in_language.string_to_code), in_language.run_code),
            id=in_language.id,
            marks=pytest.mark.skipif(
                in_language.id in languages_to_skip,
                reason='Skipped by command line arguments'))

    return [proc_single(_) for _ in _ALL_TEST_DATA]


def pytest_generate_tests(metafunc):
    languages_to_skip = metafunc.config.getoption('skip')
    if 'tool_name' in metafunc.fixturenames:
        metafunc.parametrize(
            'tool_name',
            get_tool_list(languages_to_skip))
    if 'fun_pair' in metafunc.fixturenames:
        metafunc.parametrize(
            'fun_pair',
            get_function_pair_list(languages_to_skip))


@pytest.fixture
def iteration_size(request):
    return int(request.config.getoption("--iteration-size"))
