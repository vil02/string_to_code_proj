"""
contains the function string_to_cpp
"""
import core


def atom_to_cpp(in_atom):
    assert isinstance(in_atom, core.Atom)
    return f'std::putchar(\'{in_atom.atom_char}\');'


def call_function_in_cpp(in_function_name):
    """returns a string clling a function with name in_function_name in C++"""
    return f'{in_function_name}();'


def function_to_cpp(in_function):
    assert isinstance(in_function, core.SimpleFunction)

    def proc_single_body_line(in_line_data):
        if isinstance(in_line_data, core.Atom):
            res = atom_to_cpp(in_line_data)
        else:
            res = call_function_in_cpp(in_line_data.function_name)
        return '    '+res
    function_body = \
        '\n'.join(proc_single_body_line(_) for _ in in_function.called_list)

    return '\n'.join([
        f'inline void {in_function.function_name}()',
        '{',
        function_body,
        '}'])


def str_to_cpp(in_str):
    initial_fun, function_stack = core.str_to_function_stack(
        in_str, core.gen_function_names())
    function_list = '\n\n'.join(function_to_cpp(_) for _ in function_stack)
    if isinstance(initial_fun, core.Atom):
        assert not function_stack
        call_in_main_str = atom_to_cpp(initial_fun)
    else:
        assert isinstance(initial_fun, core.SimpleFunction)
        call_in_main_str = call_function_in_cpp(initial_fun.function_name)

    main_str = '\n'.join(
        ['int main()',
         '{',
         f'    {call_in_main_str}',
         '    return 0;',
         '}',
         ''])
    res = '\n\n'.join(
        ['#include <cstdio>',
         function_list,
         main_str])
    res = res.replace("\'\n\'", "\'\\n\'")
    res = res.replace("\'\t\'", "\'\\t\'")
    res = res.replace("\'\'\'", "\'\\\'\'")
    res = res.replace("(\'\\')", "(\'\\\\')")
    return res
