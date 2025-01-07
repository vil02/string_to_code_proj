"""
setup for the tests of the module string_to_fortran90
"""

import general_utilities as gu

from string_to_code import to_fortran90

_GFOTRAN = "gfortran"

_FILE_EXTENSION = "f90"


def _compile_code(in_code, tmp_folder):
    source_filename = gu.get_unique_filename(tmp_folder, _FILE_EXTENSION)
    gu.save_str_to_file(tmp_folder / source_filename, in_code)
    executable_filename = gu.get_unique_filename(tmp_folder, "o")

    gu.subprocess_run_with_check(
        [
            _GFOTRAN,
            "-Wall",
            "-Wextra",
            "-Wpedantic",
            "-Waliasing",
            "-Wconversion-extra",
            "-Wimplicit-interface",
            "-Wimplicit-procedure",
            "-Wsurprising",
            "-Werror",
            source_filename,
            "-o",
            executable_filename,
        ],
        cwd=str(tmp_folder),
    )
    return executable_filename


def _run_code(in_code, tmp_folder):
    return gu.run_executable(_compile_code(in_code, tmp_folder), tmp_folder)


def get_test_data():
    """returns test data for the module string_to_fortran90"""
    return gu.Language(
        tool_names=[_GFOTRAN],
        string_to_code=to_fortran90.proc,
        printer_program_to_code=to_fortran90.proc_printer_program,
        run_code=_run_code,
        id="fortran90",
        source_code_file_extension=_FILE_EXTENSION,
    )
