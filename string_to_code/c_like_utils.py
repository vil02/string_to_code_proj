"""
utilities fror C-like languages
"""

from . import core


def escape_special_char(in_atom):
    """
    if in_char is a special_character function returns its escape sequences
    otherwise it returns in_char
    """
    assert isinstance(in_atom, core.Atom)
    special_chars = {
        r'"': r"\"",
        r"'": r"\'",
        "\\": "\\\\",
        "\n": "\\n",
        "\t": "\\t",
    }
    return special_chars.get(in_atom.atom_char, in_atom.atom_char)
