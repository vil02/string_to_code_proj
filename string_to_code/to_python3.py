"""
provides string_to_python utilities
"""
from . import core
from . import utils


def atom_to_code(in_atom):
    """
    returns a string/piece of python code resulting in printing the
    in_atom.atom_char to the standard output
    """
    assert isinstance(in_atom, core.Atom)
    print_arg = ""
    if in_atom.atom_char != "\n":
        special_chars = {r'"': r"\"", r"'": r"\'", "\\": "\\\\", "\t": "\\t"}
        res_char = special_chars.get(in_atom.atom_char, in_atom.atom_char)
        print_arg = f'"{res_char}", end=""'
    return f"print({print_arg})"


def function_call_str(in_function_name):
    """
    returns a string calling a function with name in_function_name in python
    """
    return f"{in_function_name}()"


_call_function_or_atom = utils.get_call_function_or_atom(
    atom_to_code, function_call_str
)


def function_to_code(in_function):
    """
    returns a string representing the code of the function definiton in python
    """
    assert isinstance(in_function, core.SimpleFunction)

    function_body = "    pass"
    if in_function.called_list:
        function_body = "\n".join(
            "    " + _call_function_or_atom(_) for _ in in_function.called_list
        )

    return "\n".join([f"def {in_function.function_name}():", function_body])


def _main_call_to_code(in_initial_call):
    return (
        _call_function_or_atom(in_initial_call) + "\n"
        if in_initial_call is not None
        else ""
    )


def _join_to_final(main_call, function_definitions):
    res = "\n\n\n".join(function_definitions + [main_call])
    return res


proc_printer_program, proc = utils.get_all_proc_functions(
    _main_call_to_code, function_to_code, _join_to_final
)
