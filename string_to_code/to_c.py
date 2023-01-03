"""
provides string_to_c utilities
"""
from . import core
from . import utils


def _get_function_name(in_function_id, **kwargs):
    return kwargs.get("function_id_to_name", core.get_function_namer("fun_"))(
        in_function_id
    )


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


def function_call_str(in_function_id, **kwargs):
    """returns a string calling a function with name in_function_name in C"""
    function_name = _get_function_name(in_function_id, **kwargs)
    return f"{function_name}();"


_call_function_or_atom = utils.get_call_function_or_atom(
    atom_to_code, function_call_str
)


def function_to_code(in_function_id, in_function, **kwargs):
    """
    returns a string representing the code of the function definiton in C
    """
    assert isinstance(in_function, core.SimpleFunction)

    function_body = "\n".join(
        "    " + _call_function_or_atom(_, **kwargs)
        for _ in in_function.called_list
    )

    if function_body:
        function_body = "\n" + function_body + "\n"

    function_name = _get_function_name(in_function_id, **kwargs)
    res = "\n".join(
        [
            f"void {function_name}()",
            "{" + function_body + "}",
        ]
    )
    return res


def _main_call_to_code(in_initial_call, **kwargs):
    initial_call_str = (
        "    " + _call_function_or_atom(in_initial_call, **kwargs) + "\n    "
        if in_initial_call is not None
        else "    "
    )
    return f"""int main()
{{
{initial_call_str}return 0;
}}"""


def _join_to_final(main_call, function_definitions, **_kwargs):
    res = "\n\n".join(function_definitions + [main_call]) + "\n"
    if function_definitions or "putchar" in main_call:
        res = "#include <stdio.h>\n\n" + res
    return res


proc_printer_program, proc = utils.get_all_proc_functions(
    _main_call_to_code, function_to_code, _join_to_final
)
