# About

Basic functionality of [`string_to_code`](./string_to_code) is to generate a piece of _messy_ code printing a given string.
For example
```python
def f_2():
    print('l', end='')
    print('o', end='')


def f_1():
    print('H', end='')
    print('e', end='')
    print('l', end='')
    f_2()
    print(' ', end='')


def f_5():
    print('W', end='')
    print('o', end='')


def f_6():
    print('r', end='')
    print('l', end='')


def f_4():
    f_5()
    f_6()


def f_3():
    f_4()
    print('d', end='')
    print('!', end='')


def f_0():
    f_1()
    f_3()


f_0()
```
is a `python` program generated with [`string_to_code.to_python3`](./string_to_code/to_python3.py) [displaying](https://www.online-python.com/gsGkE4myQa) `Hello World!`.


In order to generate a code in `your_favourite_language` just call the function `string_to_code.to_your_favourite_language.proc` with the string which you want to display. [`examples`](./examples) show some basic usage.

# Adding a new language

If you think that [`string_to_code`](./string_to_code) is missing some language, feel free to add it - it is simple.
If you want to add support of let's say `turbo_snake`, you will need to add three files:
- `to_turbo_snake.py` into [`string_to_code`](./string_to_code). This module should contain a function `proc` taking two arguments:
  - `in_str` being the string which the generated code should display,
  - `gen_function_names=None` being a generator which yields the function names in generated code.

  Most likely you will apply [`core.py`](./string_to_code/core.py) module. Have a look at the existing `proc` functions.
- `setup_turbo_snake.py` into [`tests`](./tests). This file is used for tests. It should contain a function `get_test_data()` returning a `Language` object having four fields:
  - `tool_names`: a list of program names (compilers, interpreters, linters etc.) used for executing and analysing the generated code,
  - `string_to_code` being the function `string_to_code.to_turbo_snake.proc`,
  - `run_code`: the function, which executes the generated code and returns the _standard output_,
  - `id`: the _id_ of the language, most likely `turbo_snake`.
- `setup_for_turbo_snake.sh` into [`system_setup_scripts`](./system_setup_scripts). This script is used by the workflows to install programs needed to execute corresponding tests - cf. step `Install language dependencies` in [`python_test.yml`](./.github/workflows/python_test.yml) and [`sonarcloud_check.yml`](./.github/workflows/sonarcloud_check.yml), and the the script [`install_all.sh`](./system_setup_scripts/install_all.sh).

# Running tests locally

In order to run all of the tests just run `pytest`.
If you do not want to run the tests for all of the languages (because it takes too long or you do not want to install all of the requerd _tools_) you can skip some of them by using the `--skip` option.
For example, in order to skip [`bash`](./string_to_code/to_bash.py), [`C++`](./string_to_code/to_cpp.py) and [`Lisp`](./string_to_code/to_lisp.py) run the command
```pytest --skip=bash --skip=cpp --skip=lisp```.
You can also specify the languages which you want to test by using `--run` option.
For example to run tests for [`C++`](./string_to_code/to_cpp.py) and [`COBOL`](./string_to_code/to_cobol.py) exclusively run
```pytest --run=cpp --run=cobol```.

[![python_test](https://github.com/vil02/string_to_code_proj/actions/workflows/python_test.yml/badge.svg)](https://github.com/vil02/string_to_code_proj/actions/workflows/python_test.yml)
[![Quality Gate Status](https://sonarcloud.io/api/project_badges/measure?project=vil02_string_to_code_proj&metric=alert_status)](https://sonarcloud.io/summary/new_code?id=vil02_string_to_code_proj)
