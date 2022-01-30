"""
setup for the tests of the module string_to_cobol
"""
import subprocess
import general_utilities as gu

from string_to_code import to_cobol


def get_cobc():
    """returns the name of the lisp interpreter"""
    return 'cobc'


def run_cobol_code(in_code, tmp_folder):
    """
    Runs the cobol code in_code.
    Returns the output of the program.
    """
    source_filename = gu.get_unique_filename(tmp_folder, 'clb')
    executable_name = gu.get_unique_filename(tmp_folder, 'out')
    gu.save_str_to_file(tmp_folder/source_filename, in_code)
    subprocess.run(
        [get_cobc(),
         '-W',
         '-Werror',
         '-x',
         '-free',
         f'-o{executable_name}',
         source_filename],
        cwd=str(tmp_folder),
        check=True,
        capture_output=True,
        text=True)
    res = subprocess.run(
        ['./'+executable_name],
        cwd=str(tmp_folder),
        check=True,
        capture_output=True,
        text=True)
    res.stdout = res.stdout.replace('\x00', '')
    return res


def get_test_data():
    """returns test data for the module string_to_cobol"""
    return gu.Language(
        [get_cobc()],
        to_cobol.proc,
        run_cobol_code,
        'cobol')
