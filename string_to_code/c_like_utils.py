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
