"""
provides string_to_bash utilities
"""
from . import core
from . import utils


def atom_to_code(in_atom):
    """
    returns a string/piece of bash code resulting in printing the
    in_atom.atom_char to the standard output
    """
    assert isinstance(in_atom, core.Atom)
    special_chars = {
        r'"': r"\"",
        "\\": "\\\\",
        "`": "\\`",
        "\n": "\\n",
        "\t": "\\t",
        "%": "%%",
    }
    res_char = special_chars.get(in_atom.atom_char, in_atom.atom_char)
    return f'printf "{res_char}"'


def function_call_str(in_function_name):
    """
    returns a string calling a function with name in_function_name in bash
    """
    return in_function_name


def _call_function_or_atom(in_data):
    if isinstance(in_data, core.Atom):
        return atom_to_code(in_data)
    assert isinstance(in_data, core.SimpleFunction)
    return function_call_str(in_data.function_name)


def function_to_code(in_function):
    """
    returns a string representing the code of the function definiton in bash
    """
    assert isinstance(in_function, core.SimpleFunction)

    function_body = "    true"
    if in_function.called_list:
        function_body = "\n".join(
            "    " + _call_function_or_atom(_) for _ in in_function.called_list
        )

    return f"{in_function.function_name} ()\n{{\n{function_body}\n}}"


def _main_call_to_code(in_initial_call):
    res = ""
    if in_initial_call is not None:
        res = _call_function_or_atom(in_initial_call)
    return res


def _join_to_final(main_call, function_definitions):
    res = "\n\n\n".join(function_definitions + [main_call])
    if res:
        res = "\n\n" + res
    res += "\n"
    return "#!/usr/bin/env bash\n\nset -euo pipefail" + res


proc_printer_program, proc = utils.get_all_proc_functions(
    _main_call_to_code, function_to_code, _join_to_final
)
