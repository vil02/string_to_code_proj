"""
core functions of the string_to_code module
"""
import random


class Atom:
    def __init__(self, in_atom_char):
        self._atom_char = in_atom_char

    @property
    def atom_char(self):
        return self._atom_char


class SimpleFunction:
    def __init__(self, in_function_name, in_called_list):
        self._function_name = in_function_name
        self._called_list = in_called_list

    @property
    def function_name(self):
        return self._function_name

    @property
    def called_list(self):
        return self._called_list


def str_pieces(in_str, in_pieces_len):
    assert all(_ > 0 for _ in in_pieces_len)
    assert sum(in_pieces_len) == len(in_str)
    cur_str = in_str
    res = []
    for _ in in_pieces_len:
        res.append(cur_str[:_])
        cur_str = cur_str[_:]
    assert ''.join(res) == in_str
    return res


def random_pieces_len(in_total_len):
    cur_num = in_total_len
    res = []
    while cur_num > 0:
        tmp_len = random.randint(1, max(cur_num, 1))
        res.append(tmp_len)
        cur_num -= tmp_len
    random.shuffle(res)
    assert sum(res) == in_total_len
    return res


def random_split(in_str):
    return str_pieces(in_str, random_pieces_len(len(in_str)))


def gen_function_names(in_name_prefix='f_'):
    cur_function_num = 0
    while True:
        yield f"{in_name_prefix}{cur_function_num}"
        cur_function_num += 1


def str_to_function_stack(in_str, function_names):
    function_stack = []
    known_codes = {}

    def generate_code(in_str):
        if in_str in known_codes:
            res = known_codes[in_str]
        elif len(in_str) == 1:
            res = Atom(in_str)
        else:
            cur_function_name = next(function_names)
            str_split = random_split(in_str)
            if len(in_str) > 1:
                while len(str_split) == 1:
                    str_split = random_split(in_str)
            needed_functions = [generate_code(_) for _ in str_split]
            res = SimpleFunction(cur_function_name, needed_functions)
            function_stack.append(res)
            known_codes[in_str] = res
        return res
    code = None
    if in_str:
        code = generate_code(in_str)
    return code, function_stack
