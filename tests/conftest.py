"""pytest config file"""

import collections
import itertools
import pytest

import all_language_data
import source_code_examples

_ALL_TEST_DATA = all_language_data.get_all_test_data()


def pytest_addoption(parser):
    """specifies command line options"""
    parser.addoption(
        "--iteration_size",
        action="store",
        type=int,
        default=2,
        help="Maximal number of iterations in test_string_to_code_iteration",
    )
    parser.addoption(
        "--composition_chain_size",
        action="store",
        type=int,
        default=2,
        help="Size of the composition_chain in test_string_to_code_composition",
    )
    parser.addoption(
        "--skip",
        action="append",
        default=[],
        choices=all_language_data.get_all_ids(),
        help="list of languages to skip",
    )
    parser.addoption(
        "--select",
        action="append",
        default=[],
        choices=all_language_data.get_all_ids(),
        help="list of languages to run",
    )


_SKIPPED_BY_CMD_ARGS = "Skipped by command line arguments"


def get_tool_list(languages_to_run):
    """returns data for the parametrized fixture 'tool_name'"""
    res = []
    for cur_langauge_data in _ALL_TEST_DATA:
        cur_language = cur_langauge_data.id
        is_skipped = cur_language not in languages_to_run
        for cur_tool in cur_langauge_data.tool_names:
            res.append(
                pytest.param(
                    cur_tool,
                    id=f"{cur_language}-{cur_tool}",
                    marks=pytest.mark.skipif(is_skipped, reason=_SKIPPED_BY_CMD_ARGS),
                )
            )
    return res


def check_format(in_code):
    """Checks the format of the code."""
    code_lines = in_code.split("\n")
    assert not code_lines[-1], "Code must end with empty line."
    if len(code_lines) > 1:
        assert code_lines[-2], "Code must end with exactly single empty line."
    for _ in code_lines:
        if _:
            assert _[-1] not in {" ", "\t"}, "No trailing whitespaces."


FunctionPair = collections.namedtuple("FunctionPair", ["string_to_code", "run_code"])


def extract_function_pair(in_language):
    """
    returns FunctionPair for given in_language.
    The function 'string_to_code' is decorated check_format.
    """

    def _add_check_format(in_string_to_code_fun):
        def _inner(in_str, **kwargs):
            res_code = in_string_to_code_fun(in_str, **kwargs)
            check_format(res_code)
            return res_code

        return _inner

    return FunctionPair(
        _add_check_format(in_language.string_to_code), in_language.run_code
    )


def get_function_pair_list(languages_to_run):
    """returns data for the parametrized fixture 'function_pair'"""

    def proc_single(in_language):
        return pytest.param(
            extract_function_pair(in_language),
            id=in_language.id,
            marks=pytest.mark.skipif(
                in_language.id not in languages_to_run,
                reason=_SKIPPED_BY_CMD_ARGS,
            ),
        )

    return [proc_single(_) for _ in _ALL_TEST_DATA]


def get_composition_chain_list(languages_to_run, in_chain_size):
    """returns data for the parametrized fixture 'composition_chain'"""

    def proc_single(in_chain):
        id_list = [_.id for _ in in_chain]
        return pytest.param(
            [extract_function_pair(_) for _ in in_chain],
            id="-".join(id_list),
            marks=pytest.mark.skipif(
                any(_ not in languages_to_run for _ in id_list),
                reason=_SKIPPED_BY_CMD_ARGS,
            ),
        )

    return [
        proc_single(_) for _ in itertools.product(_ALL_TEST_DATA, repeat=in_chain_size)
    ]


def _get_source_code_list(in_examples, languages_to_run):
    souce_code = collections.namedtuple(
        "souce_code", ["example_id", "language_id", "expected_output"]
    )
    res = []
    for _ in in_examples:
        for cur_lang in _.languages:
            cur_source_code = souce_code(_.id, cur_lang, _.output)
            res.append(
                pytest.param(
                    cur_source_code,
                    id=f"{_.id}-{cur_lang}",
                    marks=pytest.mark.skipif(
                        cur_lang not in languages_to_run,
                        reason=_SKIPPED_BY_CMD_ARGS,
                    ),
                )
            )
    return res


def pytest_generate_tests(metafunc):
    """generates all test data"""
    specified_to_skip = set(metafunc.config.getoption("skip"))
    specified_to_run = set(metafunc.config.getoption("select"))
    if not specified_to_run:
        languages_to_run = set(all_language_data.get_all_ids()) - specified_to_skip
    else:
        assert not specified_to_skip, "Do not mix --skip and --select."
        languages_to_run = specified_to_run
    if "tool_name" in metafunc.fixturenames:
        metafunc.parametrize("tool_name", get_tool_list(languages_to_run))
    if "function_pair" in metafunc.fixturenames:
        metafunc.parametrize("function_pair", get_function_pair_list(languages_to_run))
    if "composition_chain" in metafunc.fixturenames:
        metafunc.parametrize(
            "composition_chain",
            get_composition_chain_list(
                languages_to_run,
                metafunc.config.getoption("composition_chain_size"),
            ),
        )
    if "source_code" in metafunc.fixturenames:
        metafunc.parametrize(
            "source_code",
            _get_source_code_list(
                source_code_examples.get_full_examples(),
                languages_to_run,
            ),
        )
    if "target_language_id" in metafunc.fixturenames:
        metafunc.parametrize(
            "target_language_id",
            all_language_data.get_all_ids(),
        )


@pytest.fixture
def iteration_size(request):
    """fixture returning the 'iteration_size'"""
    return request.config.getoption("--iteration_size")
