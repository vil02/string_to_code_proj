"""
provides string_to_cobol utilities
"""
from . import core


def atom_to_code(in_atom):
    """
    returns a string/piece of cobol code resulting in printing the
    in_atom.atom_char to the standard output
    """
    def proc_char(in_char):
        if in_char == "\'":
            res = "\"\'\'\""
        elif in_char == "\"":
            res = "\'\"\'"
        else:
            res = '\''+in_char+'\''
        return res
    assert isinstance(in_atom, core.Atom)
    if in_atom.atom_char == '\n':
        res = "DISPLAY LOW-VALUE."
    else:
        res = f"DISPLAY {proc_char(in_atom.atom_char)} WITH NO ADVANCING."
    return res


def function_call_str(in_function_name):
    """
    returns a string calling a function with name in_function_name in cobol
    """
    return f'CALL \'{in_function_name}\'.'


def function_to_code(in_function):
    """
    returns a string representing the code of the function definiton in cobol
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

    return '    IDENTIFICATION DIVISION.\n' \
        f'    PROGRAM-ID. {in_function.function_name} IS COMMON.\n' \
        '    ENVIRONMENT DIVISION.\n' \
        '    PROCEDURE DIVISION.\n' \
        f'{function_body}\n' \
        f'    END PROGRAM {in_function.function_name}.\n'


def proc(in_str, gen_function_names=None):
    """
    returns a cobol code printing in_str to the standard output
    """
    if gen_function_names is None:
        gen_function_names = core.gen_function_names('PROG')

    printer_program = core.PrinterProgram(in_str, gen_function_names)
    function_list = '\n'.join(
        function_to_code(_) for _ in printer_program.function_stack)
    call_in_main_str = ''
    if isinstance(printer_program.initial_call, core.Atom):
        call_in_main_str = \
            '    '+atom_to_code(printer_program.initial_call)+'\n'
    elif printer_program.function_stack:
        call_in_main_str = '    '+function_call_str(
            printer_program.initial_call.function_name)+'\n'

    return 'IDENTIFICATION DIVISION.\n' \
        'PROGRAM-ID. MAIN.\n' \
        'ENVIRONMENT DIVISION.\n' \
        'PROCEDURE DIVISION.\n' \
        f'{call_in_main_str}' \
        f'{function_list}' \
        'END PROGRAM MAIN.\n'
