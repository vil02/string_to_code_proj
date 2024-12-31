"""
setup for the tests of the module string_to_lua
"""

import general_utilities as gu

from string_to_code import to_lua


def get_lua():
    """returns the name of the lua interpreter"""
    return "lua"


def get_luacheck():
    """returns the name of luacheck"""
    return "luacheck"


def _get_source_code_file_extension():
    return "lua"


def run_lua_code(in_code, tmp_folder):
    """
    Runs the lua code in_code.
    Returns the output of the program.
    """
    source_filename = gu.get_unique_filename(
        tmp_folder, _get_source_code_file_extension()
    )
    gu.save_str_to_file(tmp_folder / source_filename, in_code)

    gu.subprocess_run_with_check(
        [
            get_luacheck(),
            source_filename,
        ],
        cwd=str(tmp_folder),
        capture_output=True,
        text=True,
    )

    res = gu.subprocess_run_with_check(
        [get_lua(), source_filename],
        cwd=str(tmp_folder),
        capture_output=True,
        text=True,
    )
    return res


def get_test_data():
    """returns test data for the module string_to_lua"""
    return gu.Language(
        tool_names=[get_lua(), get_luacheck()],
        string_to_code=to_lua.proc,
        printer_program_to_code=to_lua.proc_printer_program,
        run_code=run_lua_code,
        id="lua",
        source_code_file_extension=_get_source_code_file_extension(),
    )
