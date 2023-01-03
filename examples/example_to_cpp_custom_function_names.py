"""Example usage of string_to_code.to_cpp with custom function names"""

import setup_examples  # noqa # pylint: disable=unused-import
from string_to_code import to_cpp

EXAMPLE_STR = "messy code"
CODE = to_cpp.proc(
    EXAMPLE_STR,
    function_id_to_name=lambda _: "some_messy_fun_num_" + str(100 * _ + 100),
)

print(f"C++ code below prints '{EXAMPLE_STR}':\n{CODE}")
