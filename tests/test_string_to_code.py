"""
tests string_to_code
"""
import all_language_data

from string_to_code import string_to_code as stc


def test_get_target_languages():
    """tests get_target_languages"""
    assert stc.get_target_languages() == frozenset(all_language_data.get_all_ids())


def test_is_language_supported_positive(target_language_id):
    """positive test of is_language_supported"""
    assert stc.is_language_supported(target_language_id)


def test_is_language_supported_negative():
    """negative test of is_language_supported"""
    assert not stc.is_language_supported("turbo_snake")
