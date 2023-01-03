"""
provides string_to_pascal utilities
"""
from . import core
from . import utils


def _get_function_name(in_function_id, **kwargs):
    return kwargs.get("function_id_to_name", core.get_function_namer("p_"))(
        in_function_id
    )


def atom_to_code(in_atom):
    """
    returns a string/piece of Pascal code resulting in printing the
    in_atom.atom_char to the standard output
    """
    assert isinstance(in_atom, core.Atom)
    if in_atom.atom_char == "\n":
        res = "WriteLn();"
    else:
        res_char = "''" if in_atom.atom_char == r"'" else in_atom.atom_char
        res = f"write('{res_char}');"
    return res


def function_call_str(in_function_id, **kwargs):
    """
    returns a string calling a function with name in_function_name in Pascal
    """
    function_name = _get_function_name(in_function_id, **kwargs)
    return f"{function_name}();"


_call_function_or_atom = utils.get_call_function_or_atom(
    atom_to_code, function_call_str
)


def function_to_code(in_function_id, in_function, **kwargs):
    """
    returns a string representing the code of the function/procedure
    definiton in Pascal
    """
    assert isinstance(in_function, core.SimpleFunction)

    function_body = "\n".join(
        "  " + _call_function_or_atom(_, **kwargs)
        for _ in in_function.called_list
    )
    if function_body:
        assert function_body[-1] == ";"
        function_body = function_body[:-1] + "\n"

    function_name = _get_function_name(in_function_id, **kwargs)
    return "".join(
        [
            f"procedure {function_name}();\n",
            "begin\n",
            function_body,
            "end;",
        ]
    )


def _main_call_to_code(in_initial_call, **kwargs):
    return (
        ""
        if in_initial_call is None
        else "\n  " + _call_function_or_atom(in_initial_call, **kwargs)[:-1]
    )


def _join_to_final(main_call, function_definitions, **_kwargs):
    function_definitions_str = (
        "\n" + "\n\n\n".join(function_definitions) + "\n\n"
        if function_definitions
        else ""
    )
    return (
        "program Main;\n"
        + function_definitions_str
        + "\nbegin"
        + main_call
        + "\nend.\n"
    )


proc_printer_program, proc = utils.get_all_proc_functions(
    _main_call_to_code, function_to_code, _join_to_final
)
