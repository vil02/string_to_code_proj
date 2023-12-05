"""
utilities fror C-like languages
"""

from . import core
from . import utils


def escape_special_char(in_atom: core.Atom) -> str:
    """
    if in_char is a special_character function returns its escape sequences
    otherwise it returns in_char
    """
    special_chars = {
        r'"': r"\"",
        r"'": r"\'",
        "\\": "\\\\",
        "\n": "\\n",
        "\t": "\\t",
    }
    return special_chars.get(in_atom.atom_char, in_atom.atom_char)


def get_atom_to_code(in_printer_function_name, in_escape_special_char_fun):
    """returns the atom_to_code type function"""

    def _inner(in_atom: core.Atom) -> str:
        res_char = in_escape_special_char_fun(in_atom)
        return f"{in_printer_function_name}('{res_char}');"

    return _inner


def get_function_call_str_fun(get_function_name):
    """returns a function returning a string calling a function with given id"""
    return utils.get_function_call_str_fun(get_function_name, "", "();")


def get_body_to_str(in_call_function_or_atom):
    """returns body_to_str-like function for c-like languages"""
    return utils.get_body_to_str("\n", "    ", in_call_function_or_atom, "", "")


def get_merge_to_full_function(in_function_prefix):
    """returns merge_to_full_function-like function for c-like languages"""

    def _merge_to_full_function(in_function_name, in_function_body):
        body_str = "\n" + in_function_body + "\n" if in_function_body else ""
        return "\n".join(
            [
                f"{in_function_prefix}{in_function_name}()",
                "{" + body_str + "}\n",
            ]
        )

    return _merge_to_full_function


def get_main_call_fun(in_call_function_or_atom):
    """returns function returing code of main C or C++ function"""

    def _main_call(in_initial_call, **kwargs):
        initial_call_str = (
            "    " + in_call_function_or_atom(in_initial_call, **kwargs) + "\n    "
            if in_initial_call is not None
            else "    "
        )
        return f"""int main()
{{
{initial_call_str}return 0;
}}"""

    return _main_call
