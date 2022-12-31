"""
provides string_to_cpp utilities
"""
from . import core
from . import utils


def atom_to_code(in_atom):
    """
    returns a string/piece of C++ code resulting in printing the
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
    return f"std::putchar('{res_char}');"


def function_call_str(in_function_name):
    """returns a string calling a function with name in_function_name in C++"""
    return f"{in_function_name}();"


_call_function_or_atom = utils.get_call_function_or_atom(
    atom_to_code, function_call_str
)


def function_to_code(in_function):
    """
    returns a string representing the code of the function definiton in C++
    """
    assert isinstance(in_function, core.SimpleFunction)

    function_body = "\n".join(
        "    " + _call_function_or_atom(_) for _ in in_function.called_list
    )

    if function_body:
        function_body = "\n" + function_body + "\n"

    res = "\n".join(
        [
            f"inline void {in_function.function_name}()",
            "{" + function_body + "}",
        ]
    )
    return res


def _main_call_to_code(in_initial_call):
    initial_call_str = (
        "    " + _call_function_or_atom(in_initial_call) + "\n    "
        if in_initial_call is not None
        else "    "
    )
    return f"""int main()
{{
{initial_call_str}return 0;
}}"""


def _join_to_final(main_call, function_definitions):
    res = "\n\n".join(function_definitions + [main_call]) + "\n"
    if function_definitions or "std::putchar" in main_call:
        res = "#include <cstdio>\n\n" + res
    return res


proc_printer_program, proc = utils.get_all_proc_functions(
    _main_call_to_code, function_to_code, _join_to_final
)
