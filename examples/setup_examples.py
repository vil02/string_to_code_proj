"inserts project's rooth path to sys.path"

import pathlib
import sys

_PROJ_PATH_STR = str(pathlib.Path(__file__).parents[1])
assert str(_PROJ_PATH_STR).endswith("string_to_code_proj")
sys.path.insert(0, _PROJ_PATH_STR)
