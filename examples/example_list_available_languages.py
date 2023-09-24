"""
this file dsplays all available target languages
"""
import setup_examples  # noqa # pylint: disable=unused-import
from string_to_code import string_to_code

print("Available target languages:")
for _ in sorted(string_to_code.get_target_languages()):
    print(_)
