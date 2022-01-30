#!/usr/bin/env bash

set -euo pipefail

while IFS="" read -r cur_script
do
    python3 "$cur_script"
done < <(find . -name "example_*.py")
