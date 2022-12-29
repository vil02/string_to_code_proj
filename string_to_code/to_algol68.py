"""
provides string_to_algol68 utilities
"""
from . import core


def atom_to_code(in_atom):
    """
    returns a string/piece of ALGOL 68 code resulting in printing the
    in_atom.atom_char to the standard output
    """
    assert isinstance(in_atom, core.Atom)
    special_char_dict = {"\n": "newline", '"': '""""'}
    res_char = special_char_dict.get(
        in_atom.atom_char, f'"{in_atom.atom_char}"'
    )
    return f"print({res_char});"


def function_call_str(in_function_name):
    """
    returns a string calling a function with name in_function_name in ALGOL 68
    """
    return f"{in_function_name};"


def function_to_code(in_function):
    """
    returns a string representing the code of the function/procedure
    definiton in ALGOL 68
    """
    assert isinstance(in_function, core.SimpleFunction)

    def proc_single_body_line(in_line_data):
        if isinstance(in_line_data, core.Atom):
            res = atom_to_code(in_line_data)
        else:
            res = function_call_str(in_line_data.function_name)
        return "  " + res

    function_body = "\n".join(
        proc_single_body_line(_) for _ in in_function.called_list
    )
    if function_body[-1] == ";":
        function_body = function_body[:-1]

    return "\n".join(
        [f"PROC {in_function.function_name} = VOID :(", function_body, ");"]
    )


def proc(in_str, gen_function_names=None):
    """
    returns an ALGOL 68 code printing in_str to the standard output
    """
    if gen_function_names is None:
        gen_function_names = core.gen_function_names("p")

    res = 'print("")\n'
    if in_str:
        printer_program = core.get_printer_program(in_str, gen_function_names)
        function_definitions = printer_program.needed_function_definitions_str(
            function_to_code, "\n\n\n"
        )
        if function_definitions:
            function_definitions = function_definitions + "\n\n"
        main_call_str = printer_program.initial_call_str(
            atom_to_code, function_call_str
        )
        main_call_str = "\n" + main_call_str[0:-1] + "\n"

        res = function_definitions + main_call_str
    return res
