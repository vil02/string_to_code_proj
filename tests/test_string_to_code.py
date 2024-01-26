"""
tests string_to_code
"""

import pytest

import all_language_data

from string_to_code import string_to_code as stc


def test_get_target_languages():
    """tests get_target_languages"""
    assert stc.get_target_languages() == frozenset(all_language_data.get_all_ids())


def test_is_language_supported_positive(target_language_id):
    """positive test of is_language_supported"""
    assert stc.is_language_supported(target_language_id)


_NON_EXISTENT_LANGUAGE = "turbo_snake"


def test_is_language_supported_negative():
    """negative test of is_language_supported"""
    assert not stc.is_language_supported(_NON_EXISTENT_LANGUAGE)


def test_proc(target_language_id, mocker):
    """tests if general proc calls the correct proc function"""
    input_str = f"{target_language_id} hello!"
    mocked_proc = mocker.patch(f"string_to_code.to_{target_language_id}.proc")
    stc.proc(target_language_id, input_str)
    mocked_proc.assert_called_once_with(input_str)


def test_proc_unknown_language():
    """checks if general proc raises an error for non-existen target language"""
    with pytest.raises(KeyError):
        stc.proc(_NON_EXISTENT_LANGUAGE, "this will not work")
