"""
general utilities for tests
"""
import pathlib
import shutil
import sys


def project_folder():
    """returns the path of the main folder of this project"""
    this_file_path = pathlib.Path(__file__)
    for _ in this_file_path.parents:
        if _.name == 'string_to_code_proj':
            res = _
            break
    else:
        raise RuntimeError('Wrong folder structure')
    return res.resolve()


sys.path.insert(0, str(project_folder()/'string_to_code_module'))


def get_tmp_test_folder_path():
    """returns the path of the folder for tmp data"""
    return project_folder()/'tmp_test_folder'


def create_tmp_test_folder():
    """creates the folder for the tmp data for tests and returns its path"""
    res = get_tmp_test_folder_path()
    res.mkdir(parents=True)
    return res


def delete_tmp_test_folder():
    """deletes folder for the tmp data for tests"""
    shutil.rmtree(get_tmp_test_folder_path())


def save_str_to_file(in_file_path, in_str):
    """writes in_str into the file in_file_path"""
    with open(in_file_path, mode='x', encoding='utf-8') as output_file:
        output_file.write(in_str)


def get_unique_filename(in_file_extension):
    """
    returns a file name with in_file_extension,
    which does not exist in the folder get_tmp_test_folder_path()
    """
    def gen_names():
        cur_try_num = 0
        while True:
            yield f'tmp_file_{cur_try_num}.'+in_file_extension
            cur_try_num += 1
    for _ in gen_names():
        if not (get_tmp_test_folder_path()/_).exists():
            return _


def get_test_string_list():
    """returns a list of strings for testing"""
    return [
        'Hello World!',
        'a',
        '\n',
        ' ',
        ';'
        '\\',
        '\n'.join(['Line 1', 'Line 2']),
        '']


def check_all(in_proc_single_fun):
    """
    performs all checks defined by in_proc_single_fun
    on all of the elements of gu.get_test_string_list()
    """
    for _ in get_test_string_list():
        in_proc_single_fun(_)
