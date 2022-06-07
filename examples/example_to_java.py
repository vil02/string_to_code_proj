"""Example usage of string_to_code.to_java. Note the custom class name."""

import setup_examples  # noqa # pylint: disable=unused-import
from string_to_code import to_java

EXAMPLE_STR = 'Hello World!'
CODE = to_java.proc(EXAMPLE_STR, class_name='HelloWorldPrinter')

print(f'java code below prints \'{EXAMPLE_STR}\':\n{CODE}')
