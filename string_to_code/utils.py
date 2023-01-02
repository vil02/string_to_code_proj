"""
utilities for to_some_language modules
"""

from . import core


def get_call_function_or_atom(in_atom_to_code, in_function_call_str):
    """
    returns the function call_function_or_atom returning a string
    representing a function call or displayng given atom
    """

    def _inner(in_data):
        if isinstance(in_data, core.Atom):
            return in_atom_to_code(in_data)
        assert isinstance(in_data, str)
        return in_function_call_str(in_data)

    return _inner


def get_proc_printer_program_function(
    main_call_to_code, function_to_code, join_to_final
):
    """returns the proc_printer_program function"""

    def _inner(in_printer_program, **kwargs):
        main_call = main_call_to_code(in_printer_program.initial_call)
        function_definitions = (
            in_printer_program.needed_function_definitions_str_list(
                function_to_code
            )
        )
        return join_to_final(main_call, function_definitions, **kwargs)

    return _inner


def get_proc_function(main_call_to_code, function_to_code, join_to_final):
    """returns the proc function"""

    def _inner(in_str, gen_function_names=None, **kwargs):
        if gen_function_names is None:
            gen_function_names = core.gen_function_names()
        printer_program = core.get_printer_program(in_str, gen_function_names)
        return get_proc_printer_program_function(
            main_call_to_code, function_to_code, join_to_final
        )(printer_program, **kwargs)

    return _inner


def get_all_proc_functions(main_call_to_code, function_to_code, join_to_final):
    """returns proc and proc_printer_program functions"""
    return get_proc_printer_program_function(
        main_call_to_code, function_to_code, join_to_final
    ), get_proc_function(main_call_to_code, function_to_code, join_to_final)
