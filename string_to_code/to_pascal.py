"""
provides string_to_pascal utilities
"""
from . import core
from . import utils


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


def function_call_str(in_function_name):
    """
    returns a string calling a function with name in_function_name in Pascal
    """
    return f"{in_function_name}();"


_call_function_or_atom = utils.get_call_function_or_atom(
    atom_to_code, function_call_str
)


def function_to_code(in_function):
    """
    returns a string representing the code of the function/procedure
    definiton in Pascal
    """
    assert isinstance(in_function, core.SimpleFunction)

    function_body = "\n".join(
        "  " + _call_function_or_atom(_) for _ in in_function.called_list
    )
    if function_body:
        assert function_body[-1] == ";"
        function_body = function_body[:-1] + "\n"

    return "".join(
        [
            f"procedure {in_function.function_name}();\n",
            "begin\n",
            function_body,
            "end;",
        ]
    )


def _main_call_to_code(in_initial_call):
    return (
        ""
        if in_initial_call is None
        else "\n  " + _call_function_or_atom(in_initial_call)[:-1]
    )


def _join_to_final(main_call, function_definitions):
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
