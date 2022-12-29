"""
provides string_to_pascal utilities
"""
from . import core


def atom_to_code(in_atom):
    """
    returns a string/piece of Pascal code resulting in printing the
    in_atom.atom_char to the standard output
    """
    assert isinstance(in_atom, core.Atom)
    if in_atom.atom_char == "\n":
        res = "WriteLn();"
    else:
        res_char = "''" if in_atom.atom_char == r"'" else in_atom.atom_char
        res = f"write('{res_char}');"
    return res


def function_call_str(in_function_name):
    """
    returns a string calling a function with name in_function_name in Pascal
    """
    return f"{in_function_name}();"


def function_to_code(in_function):
    """
    returns a string representing the code of the function/procedure
    definiton in Pascal
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
        [
            f"procedure {in_function.function_name}();",
            "begin",
            function_body,
            "end;",
        ]
    )


def proc(in_str, gen_function_names=None):
    """
    returns a Pascal code printing in_str to the standard output
    """
    if gen_function_names is None:
        gen_function_names = core.gen_function_names("P")

    printer_program = core.get_printer_program(in_str, gen_function_names)
    function_definitions = printer_program.needed_function_definitions_str(
        function_to_code, "\n\n\n"
    )
    if function_definitions:
        function_definitions = "\n" + function_definitions + "\n\n"
    main_call_str = printer_program.initial_call_str(
        atom_to_code, function_call_str
    )
    if main_call_str:
        main_call_str = "\n  " + main_call_str[0:-1]

    res = (
        "program Main;\n"
        + function_definitions
        + "\nbegin"
        + main_call_str
        + "\nend.\n"
    )
    return res
