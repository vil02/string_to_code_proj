"""
setup for the tests of the module string_to_cobol
"""
import subprocess
import general_utilities as gu

from string_to_code import to_cobol


def get_cobc():
    """returns the name of the cobol compiler/cobc"""
    return 'cobc'


def run_cobol_code(in_code, tmp_folder):
    """
    Runs the cobol code in_code.
    Returns the output of the program.
    """
    source_filename = gu.get_unique_filename(tmp_folder, 'clb')
    executable_name = gu.get_unique_filename(tmp_folder, 'out')
    gu.save_str_to_file(tmp_folder/source_filename, in_code)
    return subprocess.run(
        [get_cobc(),
         '-W',
         '-Wall',
         '-Werror',
         '-x',
         '-j',
         '-free',
         f'-o{executable_name}',
         source_filename],
        cwd=str(tmp_folder),
        check=True,
        capture_output=True,
        text=True)


def get_test_data():
    """returns test data for the module string_to_cobol"""
    return gu.Language(
        [get_cobc()],
        to_cobol.proc,
        run_cobol_code,
        'cobol')
