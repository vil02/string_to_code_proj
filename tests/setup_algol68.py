"""
setup for the tests of the module string_to_ALGOL 68
"""
import subprocess
import general_utilities as gu

from string_to_code import to_algol68


def get_algol68_interpreter():
    """returns the name of the ALGOL 68 interpreter"""
    return 'a68g'


def run_algol68_code(in_code, tmp_folder):
    """
    Runs the ALGOL 68 code in_code.
    Returns the output of the program.
    """
    source_filename = gu.get_unique_filename(tmp_folder, 'alg')
    gu.save_str_to_file(tmp_folder/source_filename, in_code)

    res = subprocess.run(
        [get_algol68_interpreter(),
         '--pedantic',
         '--strict',
         source_filename],
        cwd=str(tmp_folder),
        check=True,
        capture_output=True,
        text=True)
    return res


def get_test_data():
    """returns test data for the module string_to_ALGOL 68"""
    return gu.Language(
        [get_algol68_interpreter()],
        to_algol68.proc,
        run_algol68_code,
        'algol68')
