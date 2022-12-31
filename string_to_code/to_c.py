"""
provides string_to_c utilities
"""
from . import core
from . import utils


def atom_to_code(in_atom):
    """
    returns a string/piece of C code resulting in printing the
    in_atom.atom_char to the standard output
    """
    assert isinstance(in_atom, core.Atom)
    special_chars = {
        r'"': r"\"",
        r"'": r"\'",
        "\\": "\\\\",
        "\n": "\\n",
        "\t": "\\t",
    }
    res_char = special_chars.get(in_atom.atom_char, in_atom.atom_char)
    return f"putchar('{res_char}');"


def function_call_str(in_function_name):
    """returns a string calling a function with name in_function_name in C"""
    return f"{in_function_name}();"


def _proc_single_body_line(in_line_data):
    if isinstance(in_line_data, core.Atom):
        res = atom_to_code(in_line_data)
    else:
        res = function_call_str(in_line_data.function_name)
    return "    " + res


def function_to_code(in_function):
    """
    returns a string representing the code of the function definiton in C
    """
    assert isinstance(in_function, core.SimpleFunction)

    function_body = "\n".join(
        _proc_single_body_line(_) for _ in in_function.called_list
    )

    if function_body:
        function_body = "\n" + function_body + "\n"

    res = "\n".join(
        [
            f"void {in_function.function_name}()",
            "{" + function_body + "}",
        ]
    )
    return res


def _main_call_to_code(in_initial_call):
    initial_call_str = (
        _proc_single_body_line(in_initial_call) + "\n    "
        if in_initial_call is not None
        else "    "
    )
    return f"""int main()
{{
{initial_call_str}return 0;
}}"""


def _join_to_final(main_call, function_definitions):
    res = "\n\n".join(function_definitions + [main_call]) + "\n"
    if function_definitions or "putchar" in main_call:
        res = "#include <stdio.h>\n\n" + res
    return res


proc_printer_program, proc = utils.get_all_proc_functions(
    _main_call_to_code, function_to_code, _join_to_final
)
