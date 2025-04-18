"""
provides string_to_kotlin utilities
"""

from . import core
from . import utils


_get_function_name = utils.get_function_name_fun("fun")


def _atom_to_code(in_atom: core.Atom) -> str:
    if in_atom.atom_char == "\n":
        return "println()"
    special_chars = {
        '"': '\\"',
        "'": "\\'",
        "\\": "\\\\",
        "\t": "\\t",
        "{": "{",
        "}": "}",
    }
    res_char = special_chars.get(in_atom.atom_char, in_atom.atom_char)
    return f"print('{res_char}')"


_function_call_str = utils.get_function_call_str_fun(_get_function_name, "", "()")


_call_function_or_atom = utils.get_call_function_or_atom(
    _atom_to_code, _function_call_str
)

_EMPTY_BODY = "    // No code needed"

_body_to_str = utils.get_body_to_str(
    "\n", "    ", _call_function_or_atom, "", _EMPTY_BODY
)


def _merge_to_full_function(in_function_name, in_function_body):
    body_str = "\n" + in_function_body + "\n"
    return f"fun {in_function_name}() {{{body_str}}}\n"


_function_to_code = utils.get_function_to_code(
    _get_function_name, _body_to_str, _merge_to_full_function
)


def _main_call_to_code(in_initial_call, **kwargs):
    call_in_main = (
        "\n" + _EMPTY_BODY
        if in_initial_call is None
        else "\n    " + _call_function_or_atom(in_initial_call, **kwargs)
    )
    return "fun main() {" + call_in_main + "\n}\n"


def _join_to_final(main_call, function_definitions, **_kwargs):
    function_definitions_str = (
        "\n".join(function_definitions) if function_definitions else ""
    )
    if function_definitions_str:
        function_definitions_str = function_definitions_str + "\n"
    return function_definitions_str + main_call


proc_printer_program, proc = utils.get_all_proc_functions(
    _main_call_to_code, _function_to_code, _join_to_final
)
