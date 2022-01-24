import pytest


def pytest_addoption(parser):
    parser.addoption(
        "--iteration-size",
        action="store",
        default="2",
        help="Maximal number of iterations in test_string_to_code_iteration")


@pytest.fixture
def iteration_size(request):
    return int(request.config.getoption("--iteration-size"))
