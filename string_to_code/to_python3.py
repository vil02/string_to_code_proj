"""
provides string_to_python utilities
"""
from . import core
from . import utils


def _get_function_name(in_function_id, **kwargs):
    return kwargs.get("function_id_to_name", core.get_function_namer("fun_"))(
        in_function_id
    )


def atom_to_code(in_atom):
    """
    returns a string/piece of python code resulting in printing the
    in_atom.atom_char to the standard output
    """
    assert isinstance(in_atom, core.Atom)
    print_arg = ""
    if in_atom.atom_char != "\n":
        special_chars = {r'"': r"\"", r"'": r"\'", "\\": "\\\\", "\t": "\\t"}
        res_char = special_chars.get(in_atom.atom_char, in_atom.atom_char)
        print_arg = f'"{res_char}", end=""'
    return f"print({print_arg})"


def function_call_str(in_function_id, **kwargs):
    """
    returns a string calling a function with name in_function_name in python
    """
    function_name = _get_function_name(in_function_id, **kwargs)
    return f"{function_name}()"


_call_function_or_atom = utils.get_call_function_or_atom(
    atom_to_code, function_call_str
)


def function_to_code(in_function_id, in_function, **kwargs):
    """
    returns a string representing the code of the function definiton in python
    """
    assert isinstance(in_function, core.SimpleFunction)

    function_body = "    pass"
    if in_function.called_list:
        function_body = "\n".join(
            "    " + _call_function_or_atom(_, **kwargs)
            for _ in in_function.called_list
        )

    function_name = _get_function_name(in_function_id, **kwargs)
    return "\n".join([f"def {function_name}():", function_body])


def _main_call_to_code(in_initial_call, **kwargs):
    return (
        _call_function_or_atom(in_initial_call, **kwargs) + "\n"
        if in_initial_call is not None
        else ""
    )


def _join_to_final(main_call, function_definitions):
    res = "\n\n\n".join(function_definitions + [main_call])
    return res


proc_printer_program, proc = utils.get_all_proc_functions(
    _main_call_to_code, function_to_code, _join_to_final
)
