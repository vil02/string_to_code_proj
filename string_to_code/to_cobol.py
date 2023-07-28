"""
provides string_to_cobol utilities
"""
from . import core
from . import utils


_get_function_name = utils.get_function_name_fun("P_")


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
    res = f"DISPLAY {proc_char(in_atom.atom_char)} " "WITH NO ADVANCING END-DISPLAY."
    return res


def function_call_str(in_function_id, **kwargs):
    """
    returns a string calling a function with name in_function_name in cobol
    """
    function_name = _get_function_name(in_function_id, **kwargs)
    return f"CALL '{function_name}' END-CALL."


_call_function_or_atom = utils.get_call_function_or_atom(
    atom_to_code, function_call_str
)


def function_to_code(in_function_id, in_function, **kwargs):
    """
    returns a string representing the code of the function definiton in cobol
    """
    assert isinstance(in_function, core.SimpleFunction)

    function_body = ""
    if in_function.called_list:
        function_body = (
            "\n".join(
                "        " + _call_function_or_atom(_) for _ in in_function.called_list
            )
            + "\n"
        )
    function_name = _get_function_name(in_function_id, **kwargs)
    return (
        "    IDENTIFICATION DIVISION.\n"
        f"    PROGRAM-ID. {function_name} IS COMMON.\n"
        "    ENVIRONMENT DIVISION.\n"
        "    PROCEDURE DIVISION.\n"
        f"{function_body}"
        f"    END PROGRAM {function_name}."
    )


def _main_call_to_code(in_initial_call, **kwargs):
    res = ""
    if in_initial_call is not None:
        res = "    " + _call_function_or_atom(in_initial_call, **kwargs)
    return res


def _join_to_final(main_call, function_definitions, **_kwargs):
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
