"""
tests of the class core.get_printer_program
"""

from string_to_code import core


def test_trivial_for_empty_input():
    """program printing an empty string is trivial"""
    printer_program = core.get_printer_program("")
    assert printer_program.initial_call is None
    assert not printer_program.needed_functions


def test_initial_call_is_atom_for_single_char():
    """program printing a single character has single atomic print"""
    example = "a"
    printer_program = core.get_printer_program(example)
    assert isinstance(printer_program.initial_call, core.Atom)
    assert not printer_program.needed_functions
    assert printer_program.initial_call.atom_char == example


def test_non_trivial_for_regular_input():
    """program printing a string longer than 1 is non-trivial"""
    example = "some string"
    printer_program = core.get_printer_program(example)
    assert isinstance(printer_program.initial_call, int)
    assert printer_program.needed_functions
    assert printer_program.initial_call == len(printer_program.needed_functions) - 1
