"""
setup for the tests of the module string_to_code.to_rust
"""
import subprocess
import general_utilities as gu

from string_to_code import to_rust


def get_rustc():
    """returns the name of the rust compiler"""
    return 'rustc'


def run_rust_code(in_code, tmp_folder):
    """
    Runs the rust code in_code.
    Returns the output of the program.
    """
    source_filename = gu.get_unique_filename(tmp_folder, 'rs')
    executable_name = gu.get_unique_filename(tmp_folder, 'out')
    gu.save_str_to_file(tmp_folder/source_filename, in_code)
    subprocess.run(
        [get_rustc(),
         '--deny', 'warnings',
         f'-o{executable_name}',
         source_filename],
        cwd=str(tmp_folder),
        check=True)
    return subprocess.run(
        ['./'+executable_name],
        cwd=str(tmp_folder),
        check=True,
        capture_output=True,
        text=True)


def get_test_data():
    """returns test data for the module string_to_code.to_rust"""
    return gu.Language(
        [get_rustc()],
        to_rust.proc,
        run_rust_code,
        'rust')
