"""
core functions of the string_to_code module
"""
import collections
import random
import functools

Atom = collections.namedtuple("Atom", "atom_char")


class SimpleFunction(
    collections.namedtuple("SimpleFunction", ["function_name", "called_list"])
):
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
    assert "".join(res) == in_str
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


def _interesting_random_split(in_str):
    assert len(in_str) > 1
    res = random_split(in_str)
    while len(res) == 1:
        res = random_split(in_str)
    return res


def gen_function_names(in_name_prefix="f_"):
    """yields 'f_0', 'f_1', 'f_2', ..."""
    cur_function_num = 0
    while True:
        yield f"{in_name_prefix}{cur_function_num}"
        cur_function_num += 1


def _prepare_printer_program(in_str, function_names):
    needed_functions = []

    @functools.lru_cache(maxsize=None)
    def _generate_code(in_str):
        if len(in_str) == 1:
            return Atom(in_str)

        cur_function_name = next(function_names)
        str_split = _interesting_random_split(in_str)
        res = SimpleFunction(
            cur_function_name, [_generate_code(_) for _ in str_split]
        )
        needed_functions.append(res)
        return res

    initial_call = _generate_code(in_str) if in_str else None
    return initial_call, needed_functions


class PrinterProgram:
    """
    Represents a program printing a given string.
    It consists only of SimpleFunctions and Atoms.
    """

    def __init__(self, initial_call, needed_functions):
        self._initial_call = initial_call
        self._needed_functions = needed_functions

        self._check_data()

    def _check_data(self):
        if self._needed_functions:
            assert isinstance(self._initial_call, SimpleFunction)
        else:
            assert (
                isinstance(self._initial_call, Atom)
                or self._initial_call is None
            )


    def needed_function_definitions_str_list(self, in_function_to_str):
        """
        returns a list of string representations of the definition of
        the needed functions
        """
        return [in_function_to_str(_) for _ in self.needed_functions]


    @property
    def initial_call(self):
        """returns the 'entry point' of the program"""
        return self._initial_call

    @property
    def needed_functions(self):
        """returns the list of all needed functions"""
        return self._needed_functions


def get_printer_program(in_str, function_names):
    """returns a PrinterProgram object diplaying in_str"""
    return PrinterProgram(*_prepare_printer_program(in_str, function_names))
