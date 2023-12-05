"""
provides string_to_pascal utilities
"""
from . import core
from . import utils
from . import c_like_utils


_get_function_name = utils.get_function_name_fun("p_")


def atom_to_code(in_atom: core.Atom) -> str:
    """
    returns a string/piece of Pascal code resulting in printing the
    in_atom.atom_char to the standard output
    """
    if in_atom.atom_char == "\n":
        res = "WriteLn();"
    else:
        res_char = "''" if in_atom.atom_char == r"'" else in_atom.atom_char
        res = f"write('{res_char}');"
    return res


_function_call_str = c_like_utils.get_function_call_str_fun(_get_function_name)


_call_function_or_atom = utils.get_call_function_or_atom(
    atom_to_code, _function_call_str
)


_body_to_str = utils.get_body_to_str("\n", "  ", _call_function_or_atom, "", "")


def _merge_to_full_function(in_function_name, in_function_body):
    body_str = ""
    if in_function_body:
        assert in_function_body[-1] == ";"
        body_str = in_function_body[:-1] + "\n"
    return "".join(
        [
            f"procedure {in_function_name}();\n",
            "begin\n",
            body_str,
            "end;\n",
        ]
    )


_function_to_code = utils.get_function_to_code(
    _get_function_name, _body_to_str, _merge_to_full_function
)


def _main_call_to_code(in_initial_call, **kwargs):
    return (
        ""
        if in_initial_call is None
        else "\n  " + _call_function_or_atom(in_initial_call, **kwargs)[:-1]
    )


def _join_to_final(main_call, function_definitions, **_kwargs):
    function_definitions_str = (
        "\n" + "\n\n".join(function_definitions) + "\n\n"
        if function_definitions
        else "\n"
    )
    return (
        "program Main;\n" + function_definitions_str + "begin" + main_call + "\nend.\n"
    )


proc_printer_program, proc = utils.get_all_proc_functions(
    _main_call_to_code, _function_to_code, _join_to_final
)
