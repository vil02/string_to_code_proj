"""
provides string_to_lisp utilities
"""
from . import core
from . import utils


def _get_function_name(in_function_id, **kwargs):
    return kwargs.get("function_id_to_name", core.get_function_namer("fun_"))(
        in_function_id
    )


def atom_to_code(in_atom):
    """
    returns a string/piece of lisp code resulting in printing the
    in_atom.atom_char to the standard output
    """
    assert isinstance(in_atom, core.Atom)

    def proc_char(in_char):
        special_char_dict = {
            "\n": "#\\Newline",
            "\t": "#\\Tab",
            " ": "#\\Space",
        }
        return special_char_dict.get(in_char, "#\\" + in_char)

    return f'(format T "~c" {proc_char(in_atom.atom_char)})'


def function_call_str(in_function_id, **kwargs):
    """returns a string clling a function with name in_function_name in lisp"""
    function_name = _get_function_name(in_function_id, **kwargs)
    return f"({function_name})"


_call_function_or_atom = utils.get_call_function_or_atom(
    atom_to_code, function_call_str
)


def function_to_code(in_function_id, in_function, **kwargs):
    """
    returns a string representing the code of the function definiton in lisp
    """
    assert isinstance(in_function, core.SimpleFunction)

    function_body = "\n".join(
        "  " + _call_function_or_atom(_) for _ in in_function.called_list
    )
    if function_body:
        function_body = "\n" + function_body

    function_name = _get_function_name(in_function_id, **kwargs)
    return "".join([f"(defun {function_name} ()", function_body + ")"])


def _main_call_to_code(in_initial_call, **kwargs):
    return (
        ""
        if in_initial_call is None
        else _call_function_or_atom(in_initial_call, **kwargs)
    )


def _join_to_final(main_call, function_definitions, **_kwargs):
    main_call_with_ending = main_call + "\n" if main_call else ""
    return "\n\n".join(function_definitions + [main_call_with_ending])


proc_printer_program, proc = utils.get_all_proc_functions(
    _main_call_to_code, function_to_code, _join_to_final
)
