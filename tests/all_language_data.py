import setup_bash
import setup_cpp
import setup_python
import setup_lisp


def get_all_test_data():
    return [_.get_test_data()
            for _ in [setup_cpp, setup_bash, setup_python, setup_lisp]]


def get_all_ids():
    return (_.id for _ in get_all_test_data())
