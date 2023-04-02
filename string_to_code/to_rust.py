"""
provides string_to_rust utilities
"""
from . import core
from . import utils
from . import c_like_utils


def _get_function_name(in_function_id, **kwargs):
    return kwargs.get("function_id_to_name", core.get_function_namer("fun_"))(
        in_function_id
    )


def atom_to_code(in_atom):
    """
    returns a string/piece of Rust code resulting in printing the
    in_atom.atom_char to the standard output
    """
    assert isinstance(in_atom, core.Atom)
    if in_atom.atom_char == "\n":
        return "println!();"
    special_chars = {
        '"': '\\"',
        "\\": "\\\\",
        "\t": "\\t",
        "{": "{{",
        "}": "}}",
    }
    res_char = special_chars.get(in_atom.atom_char, in_atom.atom_char)
    return f'print!("{res_char}");'


function_call_str = c_like_utils.get_function_call_str_fun(_get_function_name)


_call_function_or_atom = utils.get_call_function_or_atom(
    atom_to_code, function_call_str
)


def function_to_code(in_function_id, in_function, **kwargs):
    """
    returns a string representing the code of the function definiton in Rust
    """
    assert isinstance(in_function, core.SimpleFunction)

    function_body = "\n".join(
        "    " + _call_function_or_atom(_, **kwargs)
        for _ in in_function.called_list
    )
    if function_body:
        function_body = "\n" + function_body + "\n"

    function_name = _get_function_name(in_function_id, **kwargs)
    return f"fn {function_name}() {{{function_body}}}"


def _main_call_to_code(in_initial_call, **kwargs):
    call_in_main = (
        ""
        if in_initial_call is None
        else "\n    " + _call_function_or_atom(in_initial_call, **kwargs)
    )
    return "fn main() {" + call_in_main + "\n}\n"


def _join_to_final(main_call, function_definitions, **_kwargs):
    function_definitions_str = (
        "\n\n".join(function_definitions) if function_definitions else ""
    )
    if function_definitions_str:
        function_definitions_str = function_definitions_str + "\n\n"
    return function_definitions_str + main_call


proc_printer_program, proc = utils.get_all_proc_functions(
    _main_call_to_code, function_to_code, _join_to_final
)
