"""
provides string_to_java utilities
"""
from . import utils
from . import c_like_utils


_get_function_name = utils.get_function_name_fun("fun")


atom_to_code = c_like_utils.get_atom_to_code(
    "System.out.print", c_like_utils.escape_special_char
)


function_call_str = c_like_utils.get_function_call_str_fun(_get_function_name)


_call_function_or_atom = utils.get_call_function_or_atom(
    atom_to_code, function_call_str
)


_body_to_str = utils.get_body_to_str("", "        ", _call_function_or_atom, "\n", "")


def _merge_to_full_function(in_function_name, in_function_body):
    return f"    static void {in_function_name}() {{\n" + in_function_body + "    }\n"


_function_to_code = utils.get_function_to_code(
    _get_function_name, _body_to_str, _merge_to_full_function
)


def default_class_name():
    """returns the default class name used by the proc function"""
    return "Printer"


def _main_call_to_code(in_initial_call, **kwargs):
    initial_call_str = (
        "        " + _call_function_or_atom(in_initial_call, **kwargs) + "\n    "
        if in_initial_call is not None
        else "    "
    )
    return "".join(
        [
            "    public static void main(String[] args) {\n",
            initial_call_str,
            "}",
        ]
    )


def _join_to_final(main_call, function_definitions, **kwargs):
    function_definitions_str = "\n"
    if function_definitions:
        function_definitions_str = "\n\n" + "\n".join(function_definitions)
    class_name = kwargs.get("class_name", default_class_name())
    res = "".join(
        [
            f"public class {class_name} {{\n",
            main_call,
            function_definitions_str,
            "}\n",
        ]
    )
    return res


proc_printer_program, proc = utils.get_all_proc_functions(
    _main_call_to_code, _function_to_code, _join_to_final
)
