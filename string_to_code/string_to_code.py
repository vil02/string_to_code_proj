"""
Provides utilities related to:
- querying available target languages,
- calling proc-like functions, where target language is specified programmatically
"""
import pathlib
import importlib


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


def _to_module_name(in_id):
    return _MODULE_PREFIX + in_id


_ALL_MODULES_DICT = {
    _: importlib.import_module("." + _to_module_name(_), "string_to_code")
    for _ in get_target_languages()
}


def proc(in_target_language_id, in_str, **kwargs):
    """
    returns a code in in_target_language_id displaying in_str
    """
    return _ALL_MODULES_DICT[in_target_language_id].proc(in_str, **kwargs)
