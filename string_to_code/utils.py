"""
utilities for to_some_language modules
"""

from . import core


def get_proc_function(main_call_to_code, function_to_code, join_to_final):
    """returns the proc function"""

    def inner(in_str, gen_function_names=None, **kwargs):
        if gen_function_names is None:
            gen_function_names = core.gen_function_names()
        printer_program = core.get_printer_program(in_str, gen_function_names)
        main_call = main_call_to_code(printer_program.initial_call)
        function_definitions = (
            printer_program.needed_function_definitions_str_list(
                function_to_code
            )
        )
        return join_to_final(main_call, function_definitions, **kwargs)

    return inner
