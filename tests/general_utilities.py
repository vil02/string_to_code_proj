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


sys.path.insert(0, str(project_folder()))


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
