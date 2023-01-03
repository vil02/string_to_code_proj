"""
provides string_to_algol68 utilities
"""
from . import core
from . import utils


def _get_function_name(in_function_id, **kwargs):
    return kwargs.get("function_id_to_name", core.get_function_namer("p"))(
        in_function_id
    )


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


def function_call_str(in_function_id, **kwargs):
    """
    returns a string calling a function with name in_function_name in ALGOL 68
    """
    function_name = _get_function_name(in_function_id, **kwargs)
    return f"{function_name};"


_call_function_or_atom = utils.get_call_function_or_atom(
    atom_to_code, function_call_str
)


def function_to_code(in_function_id, in_function, **kwargs):
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

    function_name = _get_function_name(in_function_id, **kwargs)
    return f"PROC {function_name} = VOID :({function_body});"


def _main_call_to_code(in_initial_call, **kwargs):
    if in_initial_call is None:
        return 'print("")'
    res = _call_function_or_atom(in_initial_call, **kwargs)
    assert res[-1] == ";"
    return res[:-1]


def _join_to_final(main_call, function_definitions, **_kwargs):
    assert main_call[-1] != "\n"
    return "\n\n\n".join(function_definitions + [main_call + "\n"])


proc_printer_program, proc = utils.get_all_proc_functions(
    _main_call_to_code, function_to_code, _join_to_final
)
