"""
provides string_to_java utilities
"""
from . import core


def atom_to_code(in_atom):
    """
    returns a string/piece of java code resulting in printing the
    in_atom.atom_char to the standard output
    """
    assert isinstance(in_atom, core.Atom)
    special_chars = {
        r'"': r'\"',
        r"'": r'\'',
        '\\': '\\\\',
        '\n': '\\n',
        '\t': '\\t'}
    res_char = special_chars.get(in_atom.atom_char, in_atom.atom_char)
    return f'System.out.print(\'{res_char}\');'


def function_call_str(in_function_name):
    """returns a string calling a function with name in_function_name in java"""
    return f'{in_function_name}();'


def function_to_code(in_function):
    """
    returns a string representing the code of the function definiton in java
    """
    assert isinstance(in_function, core.SimpleFunction)

    def proc_single_body_line(in_line_data):
        if isinstance(in_line_data, core.Atom):
            res = atom_to_code(in_line_data)
        else:
            res = function_call_str(in_line_data.function_name)
        return '        '+res
    function_body = \
        '\n'.join(proc_single_body_line(_) for _ in in_function.called_list)

    return '\n'.join([
        f'    static void {in_function.function_name}() {{',
        function_body,
        '    }'])


def default_class_name():
    """returns the default class name used by the proc function"""
    return 'Printer'


def proc(in_str, gen_function_names=None, **kwargs):
    """
    returns a java code printing in_str to the standard output
    """
    if gen_function_names is None:
        gen_function_names = core.gen_function_names()

    printer_program = core.PrinterProgram(in_str, gen_function_names)
    function_defitions = printer_program.needed_function_definitions_str(
        function_to_code, '\n\n')

    if function_defitions:
        function_defitions = '\n\n'+function_defitions
    call_in_main_str = printer_program.initial_call_str(
        atom_to_code, function_call_str)
    if call_in_main_str:
        call_in_main_str = '        '+call_in_main_str+'\n'

    res = ''.join(
        [f'public class {kwargs.get("class_name", default_class_name())} {{\n',
         '    public static void main(String[] args) {\n',
         call_in_main_str,
         '    }',
         function_defitions,
         '\n}\n'])
    return res
