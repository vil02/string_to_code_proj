"""
setup for the tests of the module string_to_haskell
"""
import general_utilities as gu

from string_to_code import to_haskell


def get_haskell_compiler():
    """returns the name of the haskell compiler"""
    return "ghc"


def _get_source_code_file_extension():
    return "hs"


def compile_haskell_code(in_code, tmp_folder):
    """
    Compiles the haslekk in_code and returns the filename of the output.
    The output file is located in tmp_folder
    """
    source_filename = gu.get_unique_filename(
        tmp_folder, _get_source_code_file_extension()
    )
    executable_filename = gu.get_unique_filename(tmp_folder, "out")
    gu.save_str_to_file(tmp_folder / source_filename, in_code)
    gu.subprocess_run_with_check(
        [
            get_haskell_compiler(),
            source_filename,
            "-Werror",
            "-Wall",
            "-Wextra",
            "-W",
            "-o",
            executable_filename,
        ],
        cwd=str(tmp_folder),
    )
    return executable_filename


def run_haskell_code(in_code, tmp_folder):
    """
    Compiles and executes the haskell code in_code.
    Returns the output of the program.
    """
    return gu.run_executable(
        compile_haskell_code(in_code, tmp_folder), tmp_folder
    )


def get_test_data():
    """returns test data for the module string_to_haskell"""
    return gu.Language(
        tool_names=[get_haskell_compiler()],
        string_to_code=to_haskell.proc,
        printer_program_to_code=to_haskell.proc_printer_program,
        run_code=run_haskell_code,
        id="haskell",
        source_code_file_extension=_get_source_code_file_extension(),
    )
