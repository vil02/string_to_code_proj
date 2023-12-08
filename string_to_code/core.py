"""
core functions of the string_to_code module
"""
import typing
import collections.abc
import random
import functools

Strings: typing.TypeAlias = typing.List[str]


class Atom(typing.NamedTuple):
    """represents a single character to be printed"""

    atom_char: str


CalledListEntry: typing.TypeAlias = int | Atom
InitialCall: typing.TypeAlias = CalledListEntry | None


class SimpleFunction(typing.NamedTuple):
    """
    represents a function with no arguments and no return value calling
    other functions of such type or displaying single characters
    """

    called_list: typing.List[CalledListEntry]


SimpleFunctions: typing.TypeAlias = typing.List[SimpleFunction]


def str_pieces(in_str: str, in_pieces_len: typing.List[int]) -> Strings:
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


def random_pieces_len(in_total_len: int) -> typing.List[int]:
    """returns a list of positive numbers with their sum being in_total_len"""
    cur_num = in_total_len
    res = []
    while cur_num > 0:
        tmp_len = random.randint(1, max(cur_num, 1))
        res.append(tmp_len)
        cur_num -= tmp_len
    random.shuffle(res)
    assert sum(res) == in_total_len
    return res


def random_split(in_str: str) -> Strings:
    """randomply splits in_str"""
    return str_pieces(in_str, random_pieces_len(len(in_str)))


def _interesting_random_split(in_str: str) -> Strings:
    assert len(in_str) > 1
    res = random_split(in_str)
    while len(res) == 1:
        res = random_split(in_str)
    return res


def get_function_namer(
    in_name_prefix: str = "f_",
) -> collections.abc.Callable[[int], str]:
    """returns a defult function namer"""

    def _inner(in_id: int) -> str:
        return in_name_prefix + str(in_id)

    return _inner


def _prepare_printer_program(
    in_str: str,
) -> typing.Tuple[InitialCall, SimpleFunctions]:
    needed_functions: SimpleFunctions = []

    @functools.lru_cache(maxsize=None)
    def _generate_code(in_str: str) -> CalledListEntry:
        if len(in_str) == 1:
            return Atom(in_str)

        str_split = _interesting_random_split(in_str)
        res = SimpleFunction([_generate_code(_) for _ in str_split])
        needed_functions.append(res)
        return len(needed_functions) - 1

    initial_call = _generate_code(in_str) if in_str else None
    return initial_call, needed_functions


class PrinterProgram:
    """
    Represents a program printing a given string.
    It consists only of SimpleFunctions and Atoms.
    """

    def __init__(self, initial_call: InitialCall, needed_functions: SimpleFunctions):
        self._initial_call = initial_call
        self._needed_functions = needed_functions

        self._check_data()

    def _check_data(self) -> None:
        if self.initial_call is not None and not isinstance(self.initial_call, Atom):
            assert self.needed_functions
        if self.needed_functions:
            assert isinstance(self.initial_call, int) and self.initial_call + 1 == len(
                self.needed_functions
            )
        for fun_id, fun in enumerate(self.needed_functions):
            assert all(_ < fun_id for _ in fun.called_list if not isinstance(_, Atom))

    def needed_function_definitions_str_list(
        self, in_function_to_str, **kwargs
    ) -> Strings:
        """
        returns a list of string representations of the definition of
        the needed functions
        """
        return [
            in_function_to_str(id, fun, **kwargs)
            for id, fun in enumerate(self.needed_functions)
        ]

    @property
    def initial_call(self) -> InitialCall:
        """returns the 'entry point' of the program"""
        return self._initial_call

    @property
    def needed_functions(self) -> SimpleFunctions:
        """returns the list of all needed functions"""
        return self._needed_functions


def get_printer_program(in_str: str) -> PrinterProgram:
    """returns a PrinterProgram object diplaying in_str"""
    return PrinterProgram(*_prepare_printer_program(in_str))
