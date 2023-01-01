"""
setup for the tests of the module string_to_java
"""
import general_utilities as gu

from string_to_code import to_java


def get_java_compiler():
    """returns the name of the java compiler"""
    return "javac"


def get_java():
    """returns the name of java"""
    return "java"


def _get_source_code_file_extension():
    return "java"


def run_java_code(in_code, tmp_folder):
    """
    Compiles and runs the java in_code.
    Returns the output of the program.
    """
    cur_directory = tmp_folder / gu.get_unique_foldername(tmp_folder)
    cur_directory.mkdir(parents=False, exist_ok=False)
    class_name = to_java.default_class_name()
    source_filename = class_name + "." + _get_source_code_file_extension()
    gu.save_str_to_file(cur_directory / source_filename, in_code)

    gu.subprocess_run_with_check(
        [get_java_compiler(), source_filename, "-Xlint:all", "-Werror"],
        cwd=str(cur_directory),
    )

    return gu.subprocess_run_with_check(
        [get_java(), class_name],
        cwd=str(cur_directory),
        capture_output=True,
        text=True,
    )


def get_test_data():
    """returns test data for the module string_to_java"""
    return gu.Language(
        tool_names=[get_java_compiler(), get_java()],
        string_to_code=to_java.proc,
        printer_program_to_code=to_java.proc_printer_program,
        run_code=run_java_code,
        id="java",
        source_code_file_extension=_get_source_code_file_extension(),
    )
