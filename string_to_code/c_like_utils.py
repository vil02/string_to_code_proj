"""
utilities fror C-like languages
"""

from . import core


def escape_special_char(in_atom):
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

    def _inner(in_atom):
        assert isinstance(in_atom, core.Atom)
        res_char = in_escape_special_char_fun(in_atom)
        return f"{in_printer_function_name}('{res_char}');"

    return _inner


def get_function_call_str_fun(get_function_name):
    """returns a function returning a string calling a function with given id"""

    def _function_call_str(in_function_id, **kwargs):
        function_name = get_function_name(in_function_id, **kwargs)
        return f"{function_name}();"

    return _function_call_str


def get_function_to_code(
    in_function_prefix, in_call_function_or_atom, in_get_function_name
):
    """returns a function_to_code for given prefix"""

    def _function_to_code(in_function_id, in_function, **kwargs):
        assert isinstance(in_function, core.SimpleFunction)

        function_body = "\n".join(
            "    " + in_call_function_or_atom(_, **kwargs)
            for _ in in_function.called_list
        )

        if function_body:
            function_body = "\n" + function_body + "\n"

        function_name = in_get_function_name(in_function_id, **kwargs)
        res = "\n".join(
            [
                f"{in_function_prefix}{function_name}()",
                "{" + function_body + "}",
            ]
        )
        return res

    return _function_to_code


def get_main_call_fun(in_call_function_or_atom):
    """returns function returing code of main C or C++ function"""

    def _main_call(in_initial_call, **kwargs):
        initial_call_str = (
            "    "
            + in_call_function_or_atom(in_initial_call, **kwargs)
            + "\n    "
            if in_initial_call is not None
            else "    "
        )
        return f"""int main()
{{
{initial_call_str}return 0;
}}"""

    return _main_call
