"""utilities to read  available source code  examples"""

import pathlib
import collections
import all_language_data
from string_to_code import core

ProcPrinterProgramExample = collections.namedtuple(
    "ProcPrinterProgramExample",
    ["id", "printer_program", "output"],
)

_function_used_twice = core.PrinterProgram(
    1,
    [
        core.SimpleFunction([core.Atom("{"), core.Atom("%")]),
        core.SimpleFunction(
            [0, core.Atom("\n"), 0],
        ),
    ],
)

_PRINTER_PROGRAMS = [
    ProcPrinterProgramExample("trivial", core.PrinterProgram(None, []), ""),
    ProcPrinterProgramExample(
        "one_empty_function",
        core.PrinterProgram(
            1,
            [
                core.SimpleFunction([]),
                core.SimpleFunction([0, core.Atom("C")]),
            ],
        ),
        "C",
    ),
    ProcPrinterProgramExample(
        "single_atom", core.PrinterProgram(core.Atom("X"), []), "X"
    ),
    ProcPrinterProgramExample(
        "function_used_twice", _function_used_twice, "{%\n{%"
    ),
]


def _get_examples_dir():
    return pathlib.Path(__file__).parent / "example_data"


def _get_single_language_examples_dir(in_language_id):
    return _get_examples_dir() / in_language_id


def _get_all_langueges_with_examples():
    return [_.name for _ in _get_examples_dir().iterdir() if _.is_dir()]


def _get_language_file_extension(in_language_id):
    return all_language_data.get_all_languages_as_dict()[
        in_language_id
    ].source_code_file_extension


def _get_all_examples_ids_for_given_language(in_language_id):
    example_dir = _get_single_language_examples_dir(in_language_id)
    file_extension = _get_language_file_extension(in_language_id)
    return [_.stem for _ in example_dir.glob("*." + file_extension)]


def _get_all_example_and_language_ids():
    res = {}
    for cur_language_id in _get_all_langueges_with_examples():
        for cur_example_id in _get_all_examples_ids_for_given_language(
            cur_language_id
        ):
            if cur_example_id not in res:
                res[cur_example_id] = []
            res[cur_example_id].append(cur_language_id)
    return res


def _prepare_full_examples(in_printer_program_examples):
    full_proc_printer_program_example = collections.namedtuple(
        "full_proc_printer_program_example",
        ["id", "printer_program", "output", "languages"],
    )
    res = []
    all_example_and_language_ids = _get_all_example_and_language_ids()
    for _ in in_printer_program_examples:
        res.append(
            full_proc_printer_program_example(
                id=_.id,
                printer_program=_.printer_program,
                output=_.output,
                languages=all_example_and_language_ids[_.id],
            )
        )
    return res


def get_example_source_code_path(in_example_id, in_language_id):
    """returns the path of the example source code"""
    file_name = (
        in_example_id + "." + _get_language_file_extension(in_language_id)
    )
    return _get_single_language_examples_dir(in_language_id) / file_name


def read_example_source_code(in_example_id, in_language_id):
    """reads given source code into a string"""
    example_path = get_example_source_code_path(in_example_id, in_language_id)
    with open(example_path, "r", encoding="utf-8") as file:
        return file.read()


def get_full_examples():
    """returns all of the available examples"""
    return _prepare_full_examples(_PRINTER_PROGRAMS)
