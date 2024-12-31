"""
provides string_to_lua utilities
"""

from . import core
from . import utils


def _get_table_name(**kwargs) -> str:
    return kwargs.get("table_name", "main")


def _get_function_name(in_function_id: int, **kwargs) -> str:
    prefix = _get_table_name(**kwargs) + "."
    return prefix + kwargs.get("function_id_to_name", core.get_function_namer("fun_"))(
        in_function_id
    )


def atom_to_code(in_atom: core.Atom) -> str:
    """
    returns a string/piece of lua code resulting in printing the
    in_atom.atom_char to the standard output
    """
    if in_atom.atom_char == "\n":
        return "print()"
    if in_atom.atom_char == '"':
        return "io.write('\"')"
    special_chars = {"\\": "\\\\", "\t": "\\t"}
    res_char = special_chars.get(in_atom.atom_char, in_atom.atom_char)
    write_arg = f'"{res_char}"'
    return f"io.write({write_arg})"


_function_call_str = utils.get_function_call_str_fun(_get_function_name, "", "()")

_call_function_or_atom = utils.get_call_function_or_atom(
    atom_to_code, _function_call_str
)


_body_to_str = utils.get_body_to_str("\n", "\t", _call_function_or_atom, "", "")


def _merge_to_full_function(in_function_name, in_function_body):
    function_body = " "
    if in_function_body:
        function_body = "\n" + in_function_body + "\n"
    return f"function {in_function_name}()" + function_body + "end\n"


_function_to_code = utils.get_function_to_code(
    _get_function_name, _body_to_str, _merge_to_full_function
)


def _main_call_to_code(in_initial_call, **kwargs):
    return (
        _call_function_or_atom(in_initial_call, **kwargs) + "\n"
        if in_initial_call is not None
        else ""
    )


def _join_to_final(main_call, function_definitions, **kwargs):
    res = "\n".join(function_definitions + [main_call])
    if function_definitions:
        res = "local " + _get_table_name(**kwargs) + " = {}\n\n" + res
    return res


proc_printer_program, proc = utils.get_all_proc_functions(
    _main_call_to_code, _function_to_code, _join_to_final
)
