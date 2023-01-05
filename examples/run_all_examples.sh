#!/usr/bin/env bash

set -euo pipefail

find . -name "example_*.py" -exec ./run_python_file.sh {} +
