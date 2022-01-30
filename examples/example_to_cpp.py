"""Example usage of string_to_code.to_cpp"""

import setup_examples  # noqa # pylint: disable=unused-import
from string_to_code import to_cpp

EXAMPLE_STR = 'Hello World!'
CODE = to_cpp.proc(EXAMPLE_STR)

print(f'C++ code below prints \'{EXAMPLE_STR}\':\n{CODE}')
