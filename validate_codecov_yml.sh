#!/usr/bin/env bash

set -euo pipefail

status=$(curl -s -X POST --data-binary @codecov.yml https://codecov.io/validate)
printf "%s\n" "${status}"
if [[ $status == *Error* ]];
then
    exit 1
fi
