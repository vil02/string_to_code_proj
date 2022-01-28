"""
provides string_to_lisp utilities
"""
from . import core


def atom_to_code(in_atom):
    """
    returns a string/piece of lisp code resulting in printing the
    in_atom.atom_char to the standard output
    """
    assert isinstance(in_atom, core.Atom)

    def proc_char(in_char):
        special_char_dict = {
            '\n': '#\\Newline',
            '\t': '#\\Tab',
            ' ': '#\\Space'}
        return special_char_dict.get(in_char, '#\\'+in_char)
    return f'(format T "~c" {proc_char(in_atom.atom_char)})'


def function_call_str(in_function_name):
    """returns a string clling a function with name in_function_name in lisp"""
    return f'({in_function_name})'


def function_to_code(in_function):
    """
    returns a string representing the code of the function definiton in lisp
    """
    assert isinstance(in_function, core.SimpleFunction)

    def proc_single_body_line(in_line_data):
        if isinstance(in_line_data, core.Atom):
            res = atom_to_code(in_line_data)
        else:
            res = function_call_str(in_line_data.function_name)
        return '  '+res
    function_body = \
        '\n'.join(proc_single_body_line(_) for _ in in_function.called_list)

    return '\n'.join([
        f'(defun {in_function.function_name} ()',
        function_body+')'])


def proc(in_str):
    """
    returns a lisp code printing in_str to the standard output
    """
    res = ''
    if in_str:
        printer_function = core.PrinterFunction(
            in_str, core.gen_function_names())
        function_list = '\n\n'.join(
            function_to_code(_) for _ in printer_function.function_stack)
        if isinstance(printer_function.initial_call, core.Atom):
            call_in_main_str = atom_to_code(printer_function.initial_call)
        else:
            call_in_main_str = function_call_str(
                printer_function.initial_call.function_name)

        res = '\n\n'.join(
            [function_list,
             call_in_main_str])+'\n'
    return res
