"""
provides string_to_algol68 utilities
"""
from . import core
from . import utils


def atom_to_code(in_atom):
    """
    returns a string/piece of ALGOL 68 code resulting in printing the
    in_atom.atom_char to the standard output
    """
    assert isinstance(in_atom, core.Atom)
    special_char_dict = {"\n": "newline", '"': '""""'}
    res_char = special_char_dict.get(
        in_atom.atom_char, f'"{in_atom.atom_char}"'
    )
    return f"print({res_char});"


def function_call_str(in_function_name):
    """
    returns a string calling a function with name in_function_name in ALGOL 68
    """
    return f"{in_function_name};"


def _call_function_or_atom(in_data):
    if isinstance(in_data, core.Atom):
        return atom_to_code(in_data)
    assert isinstance(in_data, core.SimpleFunction)
    return function_call_str(in_data.function_name)


def function_to_code(in_function):
    """
    returns a string representing the code of the function/procedure
    definiton in ALGOL 68
    """
    assert isinstance(in_function, core.SimpleFunction)

    function_body = "\n".join(
        "  " + _call_function_or_atom(_) for _ in in_function.called_list
    )
    if function_body:
        assert function_body[-1] == ";"
        function_body = "\n" + function_body[:-1] + "\n"

    return f"PROC {in_function.function_name} = VOID :({function_body});"


def _main_call_to_code(in_initial_call):
    if in_initial_call is None:
        return 'print("")'
    res = _call_function_or_atom(in_initial_call)
    assert res[-1] == ";"
    return res[:-1]


def _join_to_final(main_call, function_definitions):
    assert main_call[-1] != "\n"
    return "\n\n\n".join(function_definitions + [main_call + "\n"])


proc_printer_program, proc = utils.get_all_proc_functions(
    _main_call_to_code, function_to_code, _join_to_final
)
