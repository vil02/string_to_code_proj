"""
core functions of the string_to_code module
"""
import collections
import random

Atom = collections.namedtuple('Atom', 'atom_char')


class SimpleFunction(collections.namedtuple(
        'SimpleFunction', ['function_name', 'called_list'])):
    """
    represents a function with no arguments and no return value calling
    other functions of such type or displaying single characters
    """


def str_pieces(in_str, in_pieces_len):
    """returns in_str splited into pieces of lengths as in in_pieces_len"""
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
    """returns a list of positive numbers wuch their sum being in_total_len"""
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
    """randomply splits in_str"""
    return str_pieces(in_str, random_pieces_len(len(in_str)))


def gen_function_names(in_name_prefix='f_'):
    """yields 'f_0', 'f_1', 'f_2', ..."""
    cur_function_num = 0
    while True:
        yield f"{in_name_prefix}{cur_function_num}"
        cur_function_num += 1


class PrinterProgram:
    def __init__(self, in_str, function_names):
        """returns a SimpleFunction object which evaluates to in_str."""
        self._function_stack = []
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
                self._function_stack.append(res)
                known_codes[in_str] = res
            return res
        self._initial_call = None
        if in_str:
            self._initial_call = generate_code(in_str)
        self._check_data()

    def _check_data(self):
        if self._function_stack:
            assert isinstance(self._initial_call, SimpleFunction)
        else:
            assert isinstance(self._initial_call, Atom) \
                or self._initial_call is None

    @property
    def initial_call(self):
        return self._initial_call

    @property
    def function_stack(self):
        return self._function_stack
