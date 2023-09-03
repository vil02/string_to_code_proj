# About string_to_code

[![python_test](https://github.com/vil02/string_to_code_proj/actions/workflows/python_test.yml/badge.svg)](https://github.com/vil02/string_to_code_proj/actions/workflows/python_test.yml)
[![Quality Gate Status](https://sonarcloud.io/api/project_badges/measure?project=vil02_string_to_code_proj&metric=alert_status)](https://sonarcloud.io/summary/new_code?id=vil02_string_to_code_proj)
[![codecov](https://codecov.io/gh/vil02/string_to_code_proj/branch/master/graph/badge.svg?token=EZN5LIK387)](https://codecov.io/gh/vil02/string_to_code_proj)
[![CodeFactor](https://www.codefactor.io/repository/github/vil02/string_to_code_proj/badge)](https://www.codefactor.io/repository/github/vil02/string_to_code_proj)
[![Codacy Badge](https://app.codacy.com/project/badge/Grade/db4940f574964617abc44d57ee4e7c9b)](https://app.codacy.com/gh/vil02/string_to_code_proj/dashboard?utm_source=gh&utm_medium=referral&utm_content=&utm_campaign=Badge_grade)
[![Open in Gitpod](https://img.shields.io/badge/Gitpod-Ready--to--Code-blue?logo=gitpod)](https://gitpod.io/#https://github.com/vil02/string_to_code_proj)

Basic functionality of [`string_to_code`](./string_to_code) is to generate
a piece of _messy_ code printing a given string.
For example

```python
def f_1():
    print('H', end='')
    print('e', end='')


def f_2():
    print('l', end='')
    print('l', end='')


def f_5():
    print(',', end='')
    print(' ', end='')


def f_4():
    print('o', end='')
    f_5()
    print('W', end='')


def f_6():
    print('o', end='')
    print('r', end='')
    print('l', end='')


def f_3():
    f_4()
    f_6()


def f_7():
    print('d', end='')
    print('!', end='')


def f_0():
    f_1()
    f_2()
    f_3()
    f_7()


f_0()
```

is a `python` program generated with
[`string_to_code.to_python3`](./string_to_code/to_python3.py)
[displaying](https://www.online-python.com/jgzNiCAvxR) `Hello, World!`.

In order to generate a code in `your_favourite_language` just call the function `string_to_code.to_your_favourite_language.proc` with the string which you want to display. [`examples`](./examples) show some basic usage.

## Getting started

The project is setup using [poetry](https://python-poetry.org/).

In odred to create a _develompent enviroment_, after cloning this repository, run the command:
```shell
poetry install --with dev
```
Have a look at [**Running tests locally** section](#running-tests-locally).

If you just want to see the examples in action, it is enough to clone this repository and run the command:
```shell
poetry install
```
Afterwards you can execute the commands like
```shell
poetry run python3 examples/example_to_cpp.py
```

## Adding a new language

If you think that [`string_to_code`](./string_to_code) is missing some
language, feel free to add it - it is simple.
If you want to add support of let's say `turbo_snake`,
there are few things which you need to do:

- create `to_turbo_snake.py` in [`string_to_code`](./string_to_code).
  This module should define two function `proc` and `proc_printer_program`.
  The `proc` function returns a `turbo_snake` program displaying the input string.
  The `proc_printer_program` returns a `turbo_snake` representation of the given [`core.PrinterProgram`](string_to_code/core.py) object.

  Have a look at the existing modules.

- create `setup_turbo_snake.py` in [`tests`](./tests). This file is used for tests. It should contain a function `get_test_data()` returning a `Language` object having fields like:
  - `tool_names`: a list of program names (compilers, interpreters, linters etc.) used for executing and analysing the generated code,
  - `string_to_code` being the function `string_to_code.to_turbo_snake.proc`,
  - `printer_program_to_code` being the function `string_to_code.to_turbo_snake.proc_printer_program`,
  - `run_code`: the function, which executes the generated code and returns the _standard output_,
  - `id`: the _id_ of the language, most likely `turbo_snake`,
  - `source_code_file_extension`: in case of `turbo_snake` something like `ts`.

- create `setup_for_turbo_snake.sh` or `setup_no_sudo_for_turbo_snake.sh` in [`system_setup_scripts`](./system_setup_scripts).
  These scripts are used by the workflows to install programs needed to execute
  corresponding tests - cf. step `Install language dependencies`
  in [`python_test.yml`](./.github/workflows/python_test.yml) and
  [`generate_and_upload_coverage_data.yml`](./.github/workflows/generate_and_upload_coverage_data.yml),
  and [`.gitpod.dockerfile`](.gitpod.dockerfile).

- create and populate the `turbo_snake` directory in [`tests/example_data`](tests/example_data).
  These files should contain the `turbo_snake` sourcecode of some basic programs.
  Please have a look at the already existing examples.

## Running tests locally

[`tests`](tests/) use [`pytest`](https://docs.pytest.org/).
If you do not want to run the tests for all of the languages
(because it takes too long
or you do not want to install all of the required _tools_)
you can skip some of them by using the `--skip` option.
For example, in order to skip
[`bash`](./string_to_code/to_bash.py), [`C++`](./string_to_code/to_cpp.py) and
[`Lisp`](./string_to_code/to_lisp.py) run the command
```shell
poetry pytest --skip=bash --skip=cpp --skip=lisp
```

You can also specify the languages which you want to test by using
`--select` option.
For example to run tests for [`C++`](./string_to_code/to_cpp.py) and
[`COBOL`](./string_to_code/to_cobol.py) exclusively run
```shell
poetry pytest --select=cpp --select=cobol
```

## Existing development environment

This project is setup to be used in [gitpod](https://www.gitpod.io/).
[![Open in Gitpod](https://gitpod.io/button/open-in-gitpod.svg)](https://gitpod.io/#https://github.com/vil02/string_to_code_proj)
