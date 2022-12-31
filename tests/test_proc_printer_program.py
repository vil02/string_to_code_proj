"""
tests of the functions proc_printer_program
"""

import pytest
import all_language_data
import source_code_examples as sce
import general_utilities as gu


def _flattern_for_test_proc_printer_program(in_examples):
    res = []
    for _ in in_examples:
        for cur_lang in _.languages:
            res.append(
                pytest.param(
                    _.id, _.printer_program, cur_lang, id=f"{_.id}-{cur_lang}"
                )
            )
    return res


@pytest.mark.parametrize(
    "example_id, printer_program, language_id",
    _flattern_for_test_proc_printer_program(sce.get_full_examples()),
)
def test_proc_printer_program(example_id, printer_program, language_id):
    """
    checks if the output of given proc_printer_program
    matches the source code from the example
    """
    expected = sce.read_example_source_code(example_id, language_id)
    actual = all_language_data.get_all_languages_as_dict()[
        language_id
    ].printer_program_to_code(printer_program)
    assert actual == expected


def test_example_data(tmp_path, source_code):
    """
    checks if the source code from the example data evaluates to the expected
    output
    """
    source_code_str = sce.read_example_source_code(
        source_code.example_id, source_code.language_id
    )
    run_code = all_language_data.get_all_languages_as_dict()[
        source_code.language_id
    ].run_code
    gu.check_output(
        run_code(source_code_str, tmp_path), source_code.expected_output
    )
