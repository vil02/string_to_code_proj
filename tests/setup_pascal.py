"""
setup for the tests of the module string_to_pascal
"""
import subprocess
import general_utilities as gu

from string_to_code import to_pascal


def get_pascal_compiler():
    """returns the name of the Pascal compiler"""
    return 'fpc'


def run_pascal_code(in_code, tmp_folder):
    """
    Runs the Pascal code in_code.
    Returns the output of the program.
    """
    source_filename = gu.get_unique_filename(tmp_folder, 'pas')
    executable_name = gu.get_unique_filename(tmp_folder, 'out')
    gu.save_str_to_file(tmp_folder/source_filename, in_code)
    subprocess.run(
        [get_pascal_compiler(),
         '-Sew',
         '-Sen',
         '-Seh',
         f'-o{executable_name}',
         source_filename],
        cwd=str(tmp_folder),
        check=True,
        capture_output=True,
        text=True)

    return subprocess.run(
        ['./'+executable_name],
        cwd=str(tmp_folder),
        check=True,
        capture_output=True,
        text=True)


def get_test_data():
    """returns test data for the module string_to_pascal"""
    return gu.Language(
        [],  # fpc does not support --version
        to_pascal.proc,
        run_pascal_code,
        'pascal')
