"""
setup for the tests of the module string_to_pascal
"""
import general_utilities as gu

from string_to_code import to_pascal


def get_pascal_compiler():
    """returns the name of the Pascal compiler"""
    return "fpc"


def _get_source_code_file_extension():
    return "pas"


def run_pascal_code(in_code, tmp_folder):
    """
    Runs the Pascal code in_code.
    Returns the output of the program.
    """
    source_filename = gu.get_unique_filename(
        tmp_folder, _get_source_code_file_extension()
    )
    executable_name = gu.get_unique_filename(tmp_folder, "out")
    gu.save_str_to_file(tmp_folder / source_filename, in_code)
    gu.subprocess_run_with_check(
        [
            get_pascal_compiler(),
            "-Sew",
            "-Sen",
            "-Seh",
            f"-o{executable_name}",
            source_filename,
        ],
        cwd=str(tmp_folder),
        capture_output=True,
        text=True,
    )

    return gu.subprocess_run_with_check(
        ["./" + executable_name],
        cwd=str(tmp_folder),
        capture_output=True,
        text=True,
    )


def get_test_data():
    """returns test data for the module string_to_pascal"""
    return gu.Language(
        tool_names=[],  # fpc does not support --version
        string_to_code=to_pascal.proc,
        printer_program_to_code=to_pascal.proc_printer_program,
        run_code=run_pascal_code,
        id="pascal",
        source_code_file_extension=_get_source_code_file_extension(),
    )
