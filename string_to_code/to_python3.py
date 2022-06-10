"""
provides string_to_python utilities
"""
from . import core


def atom_to_code(in_atom):
    """
    returns a string/piece of python code resulting in printing the
    in_atom.atom_char to the standard output
    """
    assert isinstance(in_atom, core.Atom)
    print_arg = ''
    if in_atom.atom_char != '\n':
        special_chars = {
            r'"': r'\"',
            r"'": r'\'',
            '\\': '\\\\',
            '\t': '\\t'}
        res_char = special_chars.get(in_atom.atom_char, in_atom.atom_char)
        print_arg = f"\'{res_char}\', end=\'\'"
    return f'print({print_arg})'


def function_call_str(in_function_name):
    """
    returns a string calling a function with name in_function_name in python
    """
    return f'{in_function_name}()'


def function_to_code(in_function):
    """
    returns a string representing the code of the function definiton in python
    """
    assert isinstance(in_function, core.SimpleFunction)

    def proc_single_body_line(in_line_data):
        if isinstance(in_line_data, core.Atom):
            res = atom_to_code(in_line_data)
        else:
            res = function_call_str(in_line_data.function_name)
        return '    '+res
    function_body = '    pass'
    if in_function.called_list:
        function_body = '\n'.join(
            proc_single_body_line(_) for _ in in_function.called_list)

    return '\n'.join([
        f'def {in_function.function_name}():',
        function_body])


def proc(in_str, gen_function_names=None):
    """
    returns a python code printing in_str to the standard output
    """
    if gen_function_names is None:
        gen_function_names = core.gen_function_names()
    res = ''
    if in_str:
        printer_program = core.PrinterProgram(in_str, gen_function_names)
        function_definitions = printer_program.needed_function_definitions_str(
            function_to_code, '\n\n\n')
        if function_definitions:
            function_definitions = function_definitions+'\n\n\n'
        initial_call_str = printer_program.initial_call_str(
            atom_to_code, function_call_str)

        res = function_definitions+initial_call_str+'\n'
    return res
