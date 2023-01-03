"""
provides string_to_bash utilities
"""
from . import core
from . import utils


def _get_function_name(in_function_id, **kwargs):
    return kwargs.get("function_id_to_name", core.get_function_namer("fun_"))(
        in_function_id
    )


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


def function_call_str(in_function_id, **kwargs):
    """
    returns a string calling a function with name in_function_name in bash
    """
    return _get_function_name(in_function_id, **kwargs)


_call_function_or_atom = utils.get_call_function_or_atom(
    atom_to_code, function_call_str
)


def function_to_code(in_function_id, in_function, **kwargs):
    """
    returns a string representing the code of the function definiton in bash
    """
    assert isinstance(in_function, core.SimpleFunction)

    function_body = "    true"
    if in_function.called_list:
        function_body = "\n".join(
            "    " + _call_function_or_atom(_, **kwargs)
            for _ in in_function.called_list
        )

    function_name = _get_function_name(in_function_id, **kwargs)
    return f"{function_name} ()\n{{\n{function_body}\n}}"


def _main_call_to_code(in_initial_call, **kwargs):
    res = ""
    if in_initial_call is not None:
        res = _call_function_or_atom(in_initial_call, **kwargs)
    return res


def _join_to_final(main_call, function_definitions, **_kwargs):
    res = "\n\n\n".join(function_definitions + [main_call])
    if res:
        res = "\n\n" + res
    res += "\n"
    return "#!/usr/bin/env bash\n\nset -euo pipefail" + res


proc_printer_program, proc = utils.get_all_proc_functions(
    _main_call_to_code, function_to_code, _join_to_final
)
