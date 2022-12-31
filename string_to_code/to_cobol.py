"""
provides string_to_cobol utilities
"""
from . import core
from . import utils


def atom_to_code(in_atom):
    """
    returns a string/piece of cobol code resulting in printing the
    in_atom.atom_char to the standard output
    """

    def proc_char(in_char):
        special_char_dict = {
            "'": '"\'"',
            '"': "'\"'",
            "\t": "x'09'",
            "\n": "x'0A'",
        }
        return special_char_dict.get(in_char, "'" + in_char + "'")

    assert isinstance(in_atom, core.Atom)
    res = (
        f"DISPLAY {proc_char(in_atom.atom_char)} "
        "WITH NO ADVANCING END-DISPLAY."
    )
    return res


def function_call_str(in_function_name):
    """
    returns a string calling a function with name in_function_name in cobol
    """
    return f"CALL '{in_function_name}' END-CALL."


def _call_function_or_atom(in_data):
    if isinstance(in_data, core.Atom):
        return atom_to_code(in_data)
    assert isinstance(in_data, core.SimpleFunction)
    return function_call_str(in_data.function_name)


def function_to_code(in_function):
    """
    returns a string representing the code of the function definiton in cobol
    """
    assert isinstance(in_function, core.SimpleFunction)

    function_body = ""
    if in_function.called_list:
        function_body = (
            "\n".join(
                "        " + _call_function_or_atom(_)
                for _ in in_function.called_list
            )
            + "\n"
        )

    return (
        "    IDENTIFICATION DIVISION.\n"
        f"    PROGRAM-ID. {in_function.function_name} IS COMMON.\n"
        "    ENVIRONMENT DIVISION.\n"
        "    PROCEDURE DIVISION.\n"
        f"{function_body}"
        f"    END PROGRAM {in_function.function_name}."
    )


def _main_call_to_code(in_initial_call):
    res = ""
    if in_initial_call is not None:
        res = "    " + _call_function_or_atom(in_initial_call)
    return res


def _join_to_final(main_call, function_definitions):
    code_str = "\n\n".join([main_call] + function_definitions)
    if code_str:
        code_str += "\n"
    return (
        "IDENTIFICATION DIVISION.\n"
        "PROGRAM-ID. MAIN.\n"
        "ENVIRONMENT DIVISION.\n"
        "PROCEDURE DIVISION.\n"
        f"{code_str}"
        "END PROGRAM MAIN.\n"
    )


proc_printer_program, proc = utils.get_all_proc_functions(
    _main_call_to_code, function_to_code, _join_to_final
)
