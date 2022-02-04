"""
setup for the tests of the module string_to_cpp
"""
import subprocess
import general_utilities as gu

from string_to_code import to_cpp


def get_cpp_compiler():
    """returns the name of the c++ compiler"""
    return 'g++'


def get_cppcheck():
    """returns cppcheck"""
    return 'cppcheck'


def compile_cpp_code(in_code, tmp_folder):
    """
    Compiles the C++ in_code and returns the filename of the output.
    The output file is located in tmp_folder
    """
    source_filename = gu.get_unique_filename(tmp_folder, 'cpp')
    executable_filename = gu.get_unique_filename(tmp_folder, 'o')
    gu.save_str_to_file(tmp_folder/source_filename, in_code)
    subprocess.run(
        [get_cppcheck(),
         '--enable=all',
         '--addon=cert',
         '--addon=threadsafety',
         '--addon=y2038',
         '--error-exitcode=1',
         '--force',
         '--inconclusive',
         source_filename],
        cwd=str(tmp_folder),
        check=True,
        capture_output=True)

    subprocess.run(
        [get_cpp_compiler(), source_filename,
         '-Werror', '-Wpedantic', '-Wall', '-Wextra',
         '-o', executable_filename],
        cwd=str(tmp_folder),
        check=True)
    return executable_filename


def run_executable(in_executable_name, tmp_folder):
    """
    runs the executable in_executable_name
    in the folder tmp_folder
    """
    assert (tmp_folder/in_executable_name).is_file()
    return subprocess.run(
        ['./'+in_executable_name],
        cwd=str(tmp_folder),
        check=True,
        capture_output=True,
        text=True)


def run_cpp_code(in_code, tmp_folder):
    """
    Compiles and executes the C++ code in_code.
    Returns the output of the program.
    """
    return run_executable(compile_cpp_code(in_code, tmp_folder), tmp_folder)


def get_test_data():
    """returns test data for the module string_to_cpp"""
    return gu.Language(
        [get_cpp_compiler(), get_cppcheck()],
        to_cpp.proc,
        run_cpp_code,
        'cpp')
