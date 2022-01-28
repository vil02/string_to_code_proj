"""
imports all of the setup_*.py modules
"""
import importlib
import pathlib

_ALL_SETUP = [
    importlib.import_module(_.stem)
    for _ in pathlib.Path(__file__).parent.glob('setup_*.py')]


def get_all_test_data():
    """returns test-specific-descrition of each available langage"""
    return [_.get_test_data() for _ in _ALL_SETUP]


def get_all_ids():
    """returns id's of all of available languages"""
    return (_.id for _ in get_all_test_data())
