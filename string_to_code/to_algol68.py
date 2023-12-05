"""
provides string_to_algol68 utilities
"""
from . import core
from . import utils


_get_function_name = utils.get_function_name_fun("p")


def atom_to_code(in_atom: core.Atom) -> str:
    """
    returns a string/piece of ALGOL 68 code resulting in printing the
    in_atom.atom_char to the standard output
    """
    special_char_dict = {"\n": "newline", '"': '""""'}
    res_char = special_char_dict.get(in_atom.atom_char, f'"{in_atom.atom_char}"')
    return f"print({res_char});"


_function_call_str = utils.get_function_call_str_fun(_get_function_name, "", ";")

_call_function_or_atom = utils.get_call_function_or_atom(
    atom_to_code, _function_call_str
)


_body_to_str = utils.get_body_to_str("\n", "  ", _call_function_or_atom, "", "")


def _merge_to_full_function(in_function_name, in_function_body):
    body_str = ""
    if in_function_body:
        assert in_function_body[-1] == ";"
        body_str = "\n" + in_function_body[:-1] + "\n"
    return f"PROC {in_function_name} = VOID :({body_str});\n"


_function_to_code = utils.get_function_to_code(
    _get_function_name, _body_to_str, _merge_to_full_function
)


def _main_call_to_code(in_initial_call, **kwargs):
    if in_initial_call is None:
        return 'print("")'
    res = _call_function_or_atom(in_initial_call, **kwargs)
    assert res[-1] == ";"
    return res[:-1]


def _join_to_final(main_call, function_definitions, **_kwargs):
    assert main_call[-1] != "\n"
    return "\n\n".join(function_definitions + [main_call + "\n"])


proc_printer_program, proc = utils.get_all_proc_functions(
    _main_call_to_code, _function_to_code, _join_to_final
)
