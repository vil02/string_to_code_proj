"""
provides string_to_haskell utilities
"""
from . import core


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


def function_to_code(in_function):
    """
    returns a string representing the code of the function definiton in haskell
    """
    assert isinstance(in_function, core.SimpleFunction)

    def proc_single_body_line(in_line_data):
        if isinstance(in_line_data, core.Atom):
            res = atom_to_code(in_line_data)
        else:
            res = function_call_str(in_line_data.function_name)
        return res

    function_body = '""'
    if in_function.called_list:
        function_body = " ++ ".join(
            proc_single_body_line(_) for _ in in_function.called_list
        )

    return "\n".join(
        [
            f"{in_function.function_name} :: String",
            f"{in_function.function_name} = {function_body}",
        ]
    )


def proc(in_str, gen_function_names=None):
    """
    returns a haskell code printing in_str to the standard output
    """
    if gen_function_names is None:
        gen_function_names = core.gen_function_names()

    res = "main :: IO ()\nmain = putStr "

    if in_str:
        printer_program = core.get_printer_program(in_str, gen_function_names)
        function_definitions = printer_program.needed_function_definitions_str(
            function_to_code, "\n\n"
        )
        if function_definitions:
            function_definitions = "\n\n" + function_definitions
        initial_call_str = printer_program.initial_call_str(
            atom_to_code, function_call_str
        )
        res += f"{initial_call_str}{function_definitions}"
    else:
        res += '""'
    res += "\n"
    import_list = ["IO", "putStr"]
    if len(in_str) >= 2:
        import_list.append("String")
        assert import_list[-1] in res
    if len(in_str) >= 2:
        import_list.append("(++)")
        assert "++" in res
    res = f'import Prelude ({", ".join(import_list)})\n' + res

    return res
