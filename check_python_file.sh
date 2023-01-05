#!/usr/bin/env bash

set -euo pipefail

function check_single()
{
    declare -r in_path="$1"
    declare -i result_val=0
    printf "Checking \"%s\"\n" "${in_path}"
    printf "Checking with pylint:\n"
    if ! poetry run pylint "${in_path}" ; then
        result_val=1
    fi

    printf "Checking with flake8:\n"
    if ! poetry run flake8 "${in_path}" --count --max-line-length=80 --show-source ; then
        result_val=1
    fi
    printf "...done\n\n\n"
    return "${result_val}"
}


declare -i exit_code=0

for cur_file in "$@"
do
    if ! check_single "${cur_file}" ; then
        exit_code=1
    fi
done

if [[ ${exit_code} -eq 0 ]] ; then
   printf "\nNo errors found!\n"
fi

exit "${exit_code}"
