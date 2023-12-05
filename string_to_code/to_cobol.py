"""
provides string_to_cobol utilities
"""
from . import core
from . import utils


_get_function_name = utils.get_function_name_fun("P_")


def atom_to_code(in_atom: core.Atom) -> str:
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

    res = f"DISPLAY {proc_char(in_atom.atom_char)} " "WITH NO ADVANCING END-DISPLAY."
    return res


_function_call_str = utils.get_function_call_str_fun(
    _get_function_name, "CALL '", "' END-CALL."
)

_call_function_or_atom = utils.get_call_function_or_atom(
    atom_to_code, _function_call_str
)


_body_to_str = utils.get_body_to_str("\n", "        ", _call_function_or_atom, "", "")


def _merge_to_full_function(in_function_name, in_function_body):
    body_str = ""
    if in_function_body:
        body_str = in_function_body + "\n"
    return (
        "    IDENTIFICATION DIVISION.\n"
        f"    PROGRAM-ID. {in_function_name} IS COMMON.\n"
        "    ENVIRONMENT DIVISION.\n"
        "    PROCEDURE DIVISION.\n"
        f"{body_str}"
        f"    END PROGRAM {in_function_name}.\n"
    )


_function_to_code = utils.get_function_to_code(
    _get_function_name, _body_to_str, _merge_to_full_function
)


def _main_call_to_code(in_initial_call, **kwargs):
    res = ""
    if in_initial_call is not None:
        res = "    " + _call_function_or_atom(in_initial_call, **kwargs) + "\n"
    return res


def _join_to_final(main_call, function_definitions, **_kwargs):
    code_str = "\n".join([main_call] + function_definitions)
    return (
        "IDENTIFICATION DIVISION.\n"
        "PROGRAM-ID. MAIN.\n"
        "ENVIRONMENT DIVISION.\n"
        "PROCEDURE DIVISION.\n"
        f"{code_str}"
        "END PROGRAM MAIN.\n"
    )


proc_printer_program, proc = utils.get_all_proc_functions(
    _main_call_to_code, _function_to_code, _join_to_final
)
