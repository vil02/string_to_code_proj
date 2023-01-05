#!/usr/bin/env bash

set -euo pipefail

declare -i exit_code=0

for cur_file in "$@"
do
    if ! poetry run python3 "${cur_file}" ; then
        exit_code=1
    fi
done

if [[ ${exit_code} -eq 0 ]] ; then
   printf "\nNo errors found!\n"
fi

exit "${exit_code}"
