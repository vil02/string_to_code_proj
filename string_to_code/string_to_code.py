"""
Provides utilities related to:
- querying available target languages,
"""
import pathlib


def _remove_prefix(in_str, in_prefix):
    assert in_str.startswith(in_prefix)
    return in_str[len(in_prefix) :]


_MODULE_PREFIX = "to_"
_PACKAGE_PATH = pathlib.Path(__file__).parent
_LANGUAGE_IDS = frozenset(
    _remove_prefix(_.stem, _MODULE_PREFIX)
    for _ in _PACKAGE_PATH.glob(_MODULE_PREFIX + "*.py")
)


def get_target_languages():
    """
    returns the set of ids of all supported target languages
    """
    return _LANGUAGE_IDS


def is_language_supported(in_language_id):
    """
    checks is language with given id is supported
    """
    return in_language_id in get_target_languages()
