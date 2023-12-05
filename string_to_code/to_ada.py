"""
provides string_to_ada utilities
"""
from . import utils
from . import core


_get_function_name = utils.get_function_name_fun("Proc_")

_NULL_CALL = "null;"


def atom_to_code(in_atom: core.Atom) -> str:
    """
    returns a string/piece of Ada code resulting in printing the
    in_atom.atom_char to the standard output
    """
    if in_atom.atom_char == "\n":
        return "Ada.Text_IO.New_Line;"
    return f"Ada.Text_IO.Put(Character'Val({ord(in_atom.atom_char)}));"


_function_call_str = utils.get_function_call_str_fun(_get_function_name, "", ";")


_call_function_or_atom = utils.get_call_function_or_atom(
    atom_to_code, _function_call_str
)


_body_to_str = utils.get_body_to_str(
    "\n", "      ", _call_function_or_atom, "", "      " + _NULL_CALL
)


def _merge_to_full_function(in_function_name, in_function_body):
    return "\n".join(
        [
            f"   procedure {in_function_name} is",
            "   begin",
            in_function_body,
            f"   end {in_function_name};\n",
        ]
    )


_function_to_code = utils.get_function_to_code(
    _get_function_name, _body_to_str, _merge_to_full_function
)


def default_program_name():
    """returns the default program name used by the proc function"""
    return "Main"


_MAIN_NULL_CALL = "   " + _NULL_CALL + "\n"


def _main_call_to_code(in_initial_call, **kwargs):
    if in_initial_call:
        return "   " + _call_function_or_atom(in_initial_call, **kwargs) + "\n"
    return _MAIN_NULL_CALL


def _join_to_final(main_call, function_definitions, **kwargs):
    function_definitions_str = "\n".join(function_definitions)
    if function_definitions_str:
        function_definitions_str += "\n"
    program_name = kwargs.get("program_name", default_program_name())
    res = "".join(
        [
            f"procedure {program_name} is\n",
            function_definitions_str,
            "begin\n",
            main_call,
            f"end {program_name};\n",
        ]
    )
    if function_definitions or main_call != _MAIN_NULL_CALL:
        assert "Ada.Text_IO." in res
        res = "with Ada.Text_IO;\n\n" + res
    return res


proc_printer_program, proc = utils.get_all_proc_functions(
    _main_call_to_code, _function_to_code, _join_to_final
)
