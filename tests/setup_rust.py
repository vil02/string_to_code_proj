"""
setup for the tests of the module string_to_code.to_rust
"""

import general_utilities as gu

from string_to_code import to_rust


def get_rustc():
    """returns the name of the rust compiler"""
    return "rustc"


def _get_source_code_file_extension():
    return "rs"


def run_rust_code(in_code, tmp_folder):
    """
    Runs the rust code in_code.
    Returns the output of the program.
    """
    source_filename = gu.get_unique_filename(
        tmp_folder, _get_source_code_file_extension()
    )
    executable_name = gu.get_unique_filename(tmp_folder, "out")
    gu.save_str_to_file(tmp_folder / source_filename, in_code)
    gu.subprocess_run_with_check(
        [
            get_rustc(),
            "--deny",
            "warnings",
            f"-o{executable_name}",
            source_filename,
        ],
        cwd=str(tmp_folder),
    )
    return gu.run_executable(executable_name, tmp_folder)


def get_test_data():
    """returns test data for the module string_to_code.to_rust"""
    return gu.Language(
        tool_names=[get_rustc()],
        string_to_code=to_rust.proc,
        printer_program_to_code=to_rust.proc_printer_program,
        run_code=run_rust_code,
        id="rust",
        source_code_file_extension=_get_source_code_file_extension(),
    )
