"""
utilities for to_some_language modules
"""

from . import core


def get_function_name_fun(in_prefix: str = "fun_"):
    """returns a function returing a function name based on id"""

    def _get_function_name(in_function_id: int, **kwargs) -> str:
        return kwargs.get("function_id_to_name", core.get_function_namer(in_prefix))(
            in_function_id
        )

    return _get_function_name


def get_function_call_str_fun(in_get_function_name, in_prefix: str, in_postfix: str):
    """returns a function_call_str-like function"""

    def _function_call_str(in_function_id: int, **kwargs) -> str:
        function_name = in_get_function_name(in_function_id, **kwargs)
        return in_prefix + function_name + in_postfix

    return _function_call_str


def get_call_function_or_atom(in_atom_to_code, in_function_call_str):
    """
    returns the function call_function_or_atom returning a string
    representing a function call or displayng given atom
    """

    def _inner(in_data: core.CalledListEntry, **kwargs) -> str:
        if isinstance(in_data, core.Atom):
            return in_atom_to_code(in_data)
        assert isinstance(in_data, int)
        return in_function_call_str(in_data, **kwargs)

    return _inner


def get_proc_printer_program_function(
    main_call_to_code, function_to_code, join_to_final
):
    """returns the proc_printer_program function"""

    def _inner(in_printer_program, **kwargs):
        main_call = main_call_to_code(in_printer_program.initial_call, **kwargs)
        function_definitions = in_printer_program.needed_function_definitions_str_list(
            function_to_code, **kwargs
        )
        return join_to_final(main_call, function_definitions, **kwargs)

    return _inner


def get_proc_function(main_call_to_code, function_to_code, join_to_final):
    """returns the proc function"""

    def _inner(in_str, **kwargs):
        printer_program = core.get_printer_program(in_str)
        return get_proc_printer_program_function(
            main_call_to_code, function_to_code, join_to_final
        )(printer_program, **kwargs)

    return _inner


def get_all_proc_functions(main_call_to_code, function_to_code, join_to_final):
    """returns proc and proc_printer_program functions"""
    return get_proc_printer_program_function(
        main_call_to_code, function_to_code, join_to_final
    ), get_proc_function(main_call_to_code, function_to_code, join_to_final)


def get_body_to_str(
    in_separator: str,
    in_prefix: str,
    in_call_function_or_atom,
    in_postfix: str,
    in_empty_result: str,
):
    """returns body_to_str-like function"""

    def _body_to_str(in_function: core.SimpleFunction) -> str:
        if in_function.called_list:
            return in_separator.join(
                in_prefix + in_call_function_or_atom(_) + in_postfix
                for _ in in_function.called_list
            )
        return in_empty_result

    return _body_to_str


def get_function_to_code(
    in_get_function_name, in_body_to_str, in_merge_to_full_function
):
    """return function_to_code-like function"""

    def _function_to_code(
        in_function_id: int, in_function: core.SimpleFunction, **kwargs
    ) -> str:
        function_name = in_get_function_name(in_function_id, **kwargs)
        function_body = in_body_to_str(in_function)
        res = in_merge_to_full_function(function_name, function_body)
        assert res[-1] == "\n"
        return res

    return _function_to_code
