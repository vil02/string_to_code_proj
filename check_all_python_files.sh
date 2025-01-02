#!/usr/bin/env bash

set -euo pipefail

poetry run ruff check .
poetry run mypy .
poetry run bandit -c bandit.yml -r .

find . -name "*.py" -not -path "./tests/example_data/python3/*" -exec ./check_python_file.sh {} +
