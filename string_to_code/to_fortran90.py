"""
provides string_to_fortran90 utilities
"""

from . import core
from . import utils


_get_function_name = utils.get_function_name_fun("fun_")


def _atom_to_code(in_atom: core.Atom) -> str:
    if in_atom.atom_char == "\n":
        return "write (*, *)"
    special_chars = {
        '"': "'\"'",
        "\t": "char(9)",
    }
    return 'write (*, "(A)", advance="no") ' + special_chars.get(
        in_atom.atom_char, f'"{in_atom.atom_char}"'
    )


_function_call_str = utils.get_function_call_str_fun(_get_function_name, "call ", "()")

_call_function_or_atom = utils.get_call_function_or_atom(
    _atom_to_code, _function_call_str
)


_body_to_str = utils.get_body_to_str("\n", "        ", _call_function_or_atom, "", "")


def _merge_to_full_function(in_function_name: str, in_function_body: str) -> str:
    body_str = in_function_body + "\n" if in_function_body else ""
    return f"""    subroutine {in_function_name}()
{body_str}    end subroutine {in_function_name}
"""


_function_to_code = utils.get_function_to_code(
    _get_function_name, _body_to_str, _merge_to_full_function
)


def _main_call_to_code(in_initial_call: core.InitialCall, **kwargs) -> str:
    if in_initial_call is None:
        return ""
    return _call_function_or_atom(in_initial_call, **kwargs)


def _join_to_final(main_call: str, function_definitions: list[str], **_kwargs) -> str:
    main_call_str = "\n    " + main_call if main_call else ""
    contains_str = (
        "\ncontains\n" + "\n".join(function_definitions)
        if function_definitions
        else "\n"
    )
    return f"""program main
    implicit none{main_call_str}{contains_str}end program main
"""


proc_printer_program, proc = utils.get_all_proc_functions(
    _main_call_to_code, _function_to_code, _join_to_final
)
