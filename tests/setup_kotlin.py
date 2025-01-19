"""
setup for the tests of the module string_to_kotlin
"""

import general_utilities as gu

from string_to_code import to_kotlin

_KOTLINC = "kotlinc"

_FILE_EXTENSION = "kt"
_JAVA = "java"


def _compile_code(in_code: str, tmp_folder):
    source_filename = gu.get_unique_filename(tmp_folder, _FILE_EXTENSION)
    gu.save_str_to_file(tmp_folder / source_filename, in_code)
    jar_filename = gu.get_unique_filename(tmp_folder, "jar")

    gu.subprocess_run_with_check(
        [
            _KOTLINC,
            "-Werror",
            "-progressive",
            source_filename,
            "-d",
            jar_filename,
        ],
        cwd=str(tmp_folder),
    )
    return jar_filename


def _run_jar(jar_filename: str, tmp_folder):
    return gu.subprocess_run_with_check(
        [_JAVA, "-jar", jar_filename],
        cwd=str(tmp_folder),
        capture_output=True,
        text=True,
    )


def _run_code(in_code: str, tmp_folder):
    return _run_jar(_compile_code(in_code, tmp_folder), tmp_folder)


def get_test_data():
    """returns test data for the module string_to_kotlin"""
    return gu.Language(
        tool_names=[_KOTLINC, _JAVA],
        string_to_code=to_kotlin.proc,
        printer_program_to_code=to_kotlin.proc_printer_program,
        run_code=_run_code,
        id="kotlin",
        source_code_file_extension=_FILE_EXTENSION,
    )
