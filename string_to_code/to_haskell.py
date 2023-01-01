"""
provides string_to_haskell utilities
"""
from . import core
from . import utils


def atom_to_code(in_atom):
    """
    returns a string/piece of bash code resulting in printing the
    in_atom.atom_char to the standard output
    """
    assert isinstance(in_atom, core.Atom)
    special_chars = {r'"': r"\"", "\\": "\\\\", "\n": "\\n", "\t": "\\t"}
    res_char = special_chars.get(in_atom.atom_char, in_atom.atom_char)
    return f'"{res_char}"'


def function_call_str(in_function_name):
    """
    returns a string calling a function with name in_function_name in haskell
    """
    return in_function_name


_call_function_or_atom = utils.get_call_function_or_atom(
    atom_to_code, function_call_str
)


def function_to_code(in_function):
    """
    returns a string representing the code of the function definiton in haskell
    """
    assert isinstance(in_function, core.SimpleFunction)

    function_body = '""'
    if in_function.called_list:
        function_body = " ++ ".join(
            _call_function_or_atom(_) for _ in in_function.called_list
        )

    return "\n".join(
        [
            f"{in_function.function_name} :: String",
            f"{in_function.function_name} = {function_body}",
        ]
    )


def _main_call_to_code(in_initial_call):
    if in_initial_call is not None:
        return _call_function_or_atom(in_initial_call)
    return '""'


def _join_to_final(main_call, function_definitions):
    function_definitions_str = ""
    if function_definitions:
        function_definitions_str = "\n\n" + "\n\n".join(function_definitions)

    res = "main :: IO ()\nmain = putStr "
    res += f"{main_call}{function_definitions_str}\n"

    import_list = ["IO", "putStr"]
    if function_definitions:
        assert "String" in res
        import_list.append("String")
        assert "++" in res
        import_list.append("(++)")
    res = f'import Prelude ({", ".join(import_list)})\n' + res

    return res


proc_printer_program, proc = utils.get_all_proc_functions(
    _main_call_to_code, function_to_code, _join_to_final
)
