#!/usr/bin/env bash

set -euo pipefail

function check_single()
{
    declare -r in_path="$1"
    declare -i result_val=0
    printf "Checking \"%s\"\n" "${in_path}"
    printf "Checking with pylint:\n"
    if ! pylint "$in_path" ; then
        result_val=1
    fi

    printf "Checking with flake8:\n"
    if ! flake8 "$in_path" --count --max-line-length=80 --show-source ; then
        result_val=1
    fi
    printf "...done\n\n\n"
    return $result_val
}


declare -i exit_code=0

while IFS="" read -r cur_script
do
    if ! check_single "$cur_script" ; then
        exit_code=1
    fi
done < <(find . -name "*.py")

exit $exit_code
