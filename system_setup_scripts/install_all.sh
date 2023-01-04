#!/usr/bin/env bash

set -euo pipefail

while IFS="" read -r cur_script
do
    ./"${cur_script}"
done < <(find . -name "setup_for_*.sh")
