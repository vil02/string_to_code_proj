"""checks if the environment is setup properly to perform all of the other tests"""

import general_utilities as gu


def test_tool(tool_name):
    """checks if a program named tool_name is available in the system"""
    gu.check_tool(tool_name)
