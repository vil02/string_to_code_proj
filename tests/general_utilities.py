"""
general utilities for tests
"""

import collections
import subprocess  # nosec B404
import pathlib
import typing

Language = collections.namedtuple(
    "Language",
    [
        "tool_names",
        "string_to_code",
        "printer_program_to_code",
        "run_code",
        "id",
        "source_code_file_extension",
    ],
)


def save_str_to_file(in_file_path: pathlib.Path, in_str: str) -> None:
    """writes in_str into the file in_file_path"""
    with open(in_file_path, mode="x", encoding="utf-8") as output_file:
        output_file.write(in_str)


def _find_not_existing(
    in_tmp_folder: pathlib.Path,
    names_generator: typing.Callable[[], typing.Generator[str, None, None]],
) -> str:
    assert in_tmp_folder.exists()
    res = next(_ for _ in names_generator() if not (in_tmp_folder / _).exists())
    assert isinstance(res, str)
    return res


def get_unique_foldername(in_tmp_folder: pathlib.Path) -> str:
    """
    returns a directory name,
    which does not exist in the folder in_tmp_folder
    """

    def gen_names() -> typing.Generator[str, None, None]:
        cur_try_num = 0
        while True:
            yield f"tmp_dir_{cur_try_num}"
            cur_try_num += 1

    return _find_not_existing(in_tmp_folder, gen_names)


def get_unique_filename(in_tmp_folder: pathlib.Path, in_file_extension: str) -> str:
    """
    returns a file name with in_file_extension,
    which does not exist in the folder in_tmp_folder
    """

    def gen_names() -> typing.Generator[str, None, None]:
        cur_try_num = 0
        while True:
            yield f"tmp_file_{cur_try_num}." + in_file_extension
            cur_try_num += 1

    return _find_not_existing(in_tmp_folder, gen_names)


def check_tool(in_program_name: str) -> None:
    """
    Checks if given tool is available
    """
    subprocess_run_with_check(["which", in_program_name], capture_output=True)


def subprocess_run_with_check(*args, **kwargs):
    """
    Simple wrapper of the subprocess.run function.
    """
    return subprocess.run(*args, **kwargs, check=True)  # nosec B603


def run_executable(in_executable_name: str, tmp_folder: pathlib.Path):
    """
    runs the executable in_executable_name
    in the folder tmp_folder
    """
    assert (tmp_folder / in_executable_name).is_file()
    return subprocess_run_with_check(
        ["./" + in_executable_name],
        cwd=str(tmp_folder),
        capture_output=True,
        text=True,
    )


def check_output(in_ex_output, in_target_str: str) -> None:
    """
    does all of the checks of the program output against the expected
    result
    """
    if len(in_ex_output.stdout) - 1 == len(in_target_str):
        # Some of the interpreters add newline symbol at the end of the output.
        assert in_target_str[-1] != "\n"
        assert in_ex_output.stdout == in_target_str + "\n"
    else:
        assert in_ex_output.stdout == in_target_str
