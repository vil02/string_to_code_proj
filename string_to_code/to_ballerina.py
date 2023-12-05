"""
provides string_to_ballereina utilities
"""
from . import core
from . import utils
from . import c_like_utils


_get_function_name = utils.get_function_name_fun("fun_")


def atom_to_code(in_atom: core.Atom) -> str:
    """
    returns a string/piece of ballerina code resulting in printing the
    in_atom.atom_char to the standard output
    """
    if in_atom.atom_char == "\n":
        res = "io:println();"
    else:
        special_chars = {
            "\\": "\\\\",
            '"': '\\"',
        }
        res_char = special_chars.get(in_atom.atom_char, in_atom.atom_char)
        res = f'io:print("{res_char}");'
    return res


function_call_str = c_like_utils.get_function_call_str_fun(_get_function_name)


_call_function_or_atom = utils.get_call_function_or_atom(
    atom_to_code, function_call_str
)


_body_to_str = utils.get_body_to_str("", "    ", _call_function_or_atom, "\n", "")


def _merge_to_full_function(in_function_name, in_function_body):
    return f"function {in_function_name}() {{\n" + in_function_body + "}\n"


_function_to_code = utils.get_function_to_code(
    _get_function_name, _body_to_str, _merge_to_full_function
)


def _main_call_to_code(in_initial_call, **kwargs):
    initial_call_str = (
        "    " + _call_function_or_atom(in_initial_call, **kwargs) + "\n"
        if in_initial_call is not None
        else ""
    )
    return "".join(
        [
            "public function main() {\n",
            initial_call_str,
            "}",
        ]
    )


def _join_to_final(main_call, function_definitions, **_kwargs):
    res = "\n".join(function_definitions + [main_call]) + "\n"
    if function_definitions or "io:" in main_call:
        res = "import ballerina/io;\n\n" + res
    return res


proc_printer_program, proc = utils.get_all_proc_functions(
    _main_call_to_code, _function_to_code, _join_to_final
)
