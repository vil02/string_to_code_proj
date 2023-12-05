"""
provides string_to_lisp utilities
"""
from . import core
from . import utils


_get_function_name = utils.get_function_name_fun()


def atom_to_code(in_atom: core.Atom) -> str:
    """
    returns a string/piece of lisp code resulting in printing the
    in_atom.atom_char to the standard output
    """

    def proc_char(in_char):
        special_char_dict = {
            "\n": "#\\Newline",
            "\t": "#\\Tab",
            " ": "#\\Space",
        }
        return special_char_dict.get(in_char, "#\\" + in_char)

    return f'(format T "~c" {proc_char(in_atom.atom_char)})'


_function_call_str = utils.get_function_call_str_fun(_get_function_name, "(", ")")

_call_function_or_atom = utils.get_call_function_or_atom(
    atom_to_code, _function_call_str
)

_body_to_str = utils.get_body_to_str("", "\n  ", _call_function_or_atom, "", "")


def _merge_to_full_function(in_function_name, in_function_body):
    return f"(defun {in_function_name} (){in_function_body})\n"


_function_to_code = utils.get_function_to_code(
    _get_function_name, _body_to_str, _merge_to_full_function
)


def _main_call_to_code(in_initial_call, **kwargs):
    return (
        ""
        if in_initial_call is None
        else _call_function_or_atom(in_initial_call, **kwargs)
    )


def _join_to_final(main_call, function_definitions, **_kwargs):
    main_call_with_ending = main_call + "\n" if main_call else ""
    return "\n".join(function_definitions + [main_call_with_ending])


proc_printer_program, proc = utils.get_all_proc_functions(
    _main_call_to_code, _function_to_code, _join_to_final
)
