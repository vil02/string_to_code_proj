"""
this file illustrates the usage of the general string_to_code module
"""

import setup_examples  # noqa # pylint: disable=unused-import
from string_to_code import string_to_code

EXAMPLE_STR = "Hello, World!"
TARGET_LANGUAGE = "ada"

assert TARGET_LANGUAGE in string_to_code.get_target_languages()  # nosec B101

CODE = string_to_code.proc(TARGET_LANGUAGE, EXAMPLE_STR)

print(f"{TARGET_LANGUAGE} code below prints '{EXAMPLE_STR}':\n{CODE}")
