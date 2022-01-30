"""Example usage of string_to_code.to_cpp with custom function names"""

import itertools
import setup_examples  # noqa # pylint: disable=unused-import
from string_to_code import to_cpp

EXAMPLE_STR = 'messy code'
FUNCTION_NAMES = (f"some_messy_fun_num_{_}" for _ in itertools.count(100, 100))
CODE = to_cpp.proc(EXAMPLE_STR, FUNCTION_NAMES)

print(f'C++ code below prints \'{EXAMPLE_STR}\':\n{CODE}')
