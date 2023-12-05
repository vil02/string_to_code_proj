"""
provides string_to_python utilities
"""
from . import core
from . import utils


_get_function_name = utils.get_function_name_fun()


def atom_to_code(in_atom: core.Atom) -> str:
    """
    returns a string/piece of python code resulting in printing the
    in_atom.atom_char to the standard output
    """
    print_arg = ""
    if in_atom.atom_char != "\n":
        special_chars = {r'"': r"\"", r"'": r"\'", "\\": "\\\\", "\t": "\\t"}
        res_char = special_chars.get(in_atom.atom_char, in_atom.atom_char)
        print_arg = f'"{res_char}", end=""'
    return f"print({print_arg})"


_function_call_str = utils.get_function_call_str_fun(_get_function_name, "", "()")

_call_function_or_atom = utils.get_call_function_or_atom(
    atom_to_code, _function_call_str
)


_body_to_str = utils.get_body_to_str(
    "\n", "    ", _call_function_or_atom, "", "    pass"
)


def _merge_to_full_function(in_function_name, in_function_body):
    return "\n".join([f"def {in_function_name}():", in_function_body]) + "\n"


_function_to_code = utils.get_function_to_code(
    _get_function_name, _body_to_str, _merge_to_full_function
)


def _main_call_to_code(in_initial_call, **kwargs):
    return (
        _call_function_or_atom(in_initial_call, **kwargs) + "\n"
        if in_initial_call is not None
        else ""
    )


def _join_to_final(main_call, function_definitions, **_kwargs):
    res = "\n\n".join(function_definitions + [main_call])
    return res


proc_printer_program, proc = utils.get_all_proc_functions(
    _main_call_to_code, _function_to_code, _join_to_final
)
