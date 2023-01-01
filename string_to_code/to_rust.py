"""
provides string_to_rust utilities
"""
from . import core
from . import utils


def atom_to_code(in_atom):
    """
    returns a string/piece of Rust code resulting in printing the
    in_atom.atom_char to the standard output
    """
    assert isinstance(in_atom, core.Atom)
    special_chars = {
        '"': '\\"',
        "\\": "\\\\",
        "\n": "\\n",
        "\t": "\\t",
        "{": "{{",
        "}": "}}",
    }
    res_char = special_chars.get(in_atom.atom_char, in_atom.atom_char)
    return f'print!("{res_char}");'


def function_call_str(in_function_name):
    """
    returns a string calling a function with name in_function_name in Rust
    """
    return f"{in_function_name}();"


_call_function_or_atom = utils.get_call_function_or_atom(
    atom_to_code, function_call_str
)


def function_to_code(in_function):
    """
    returns a string representing the code of the function definiton in Rust
    """
    assert isinstance(in_function, core.SimpleFunction)

    function_body = "\n".join(
        "    " + _call_function_or_atom(_) for _ in in_function.called_list
    )
    if function_body:
        function_body = "\n" + function_body + "\n"

    return f"fn {in_function.function_name}() {{{function_body}}}"


def _main_call_to_code(in_initial_call):
    call_in_main = (
        ""
        if in_initial_call is None
        else "\n    " + _call_function_or_atom(in_initial_call)
    )
    return "fn main() {" + call_in_main + "\n}\n"


def _join_to_final(main_call, function_definitions):
    function_definitions_str = (
        "\n\n".join(function_definitions) if function_definitions else ""
    )
    if function_definitions_str:
        function_definitions_str = function_definitions_str + "\n\n"
    return function_definitions_str + main_call


proc_printer_program, proc = utils.get_all_proc_functions(
    _main_call_to_code, function_to_code, _join_to_final
)
