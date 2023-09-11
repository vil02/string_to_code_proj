"""
provides string_to_cpp utilities
"""
from . import utils
from . import c_like_utils


_get_function_name = utils.get_function_name_fun()


_atom_to_code = c_like_utils.get_atom_to_code(
    "std::putchar", c_like_utils.escape_special_char
)


_call_function_or_atom = utils.get_call_function_or_atom(
    _atom_to_code, c_like_utils.get_function_call_str_fun(_get_function_name)
)

_function_to_code = utils.get_function_to_code(
    _get_function_name,
    c_like_utils.get_body_to_str(_call_function_or_atom),
    c_like_utils.get_merge_to_full_function("inline void "),
)

_main_call_to_code = c_like_utils.get_main_call_fun(_call_function_or_atom)


def _join_to_final(main_call, function_definitions, **_kwargs):
    res = "\n".join(function_definitions + [main_call]) + "\n"
    if function_definitions or "std::putchar" in main_call:
        res = "#include <cstdio>\n\n" + res
    return res


proc_printer_program, proc = utils.get_all_proc_functions(
    _main_call_to_code, _function_to_code, _join_to_final
)
