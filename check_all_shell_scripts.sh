#!/usr/bin/env bash

set -euo pipefail

declare -i exit_code=0

while IFS="" read -r cur_script
do
  printf "Checking \"%s\"\n" "${cur_script}"
    if ! shellcheck "$cur_script"; then
      exit_code=1
    fi
    if ! grep -q "set -euo pipefail" "$cur_script"; then
      printf "add \"set -euo pipefail\"\n"
      exit_code=1
    fi
done < <(find . -name "*.sh")

exit $exit_code
