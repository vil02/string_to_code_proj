#!/usr/bin/env bash

set -euo pipefail

tmp_file="tmp_ballerina.deb"
readonly tmp_file

if [[ -e "${tmp_file}" ]]
then
    printf "The file \`%s\` already exists.\n" "${tmp_file}"
    exit 1
fi

curl https://dist.ballerina.io/downloads/2201.10.3/ballerina-2201.10.3-swan-lake-linux-x64.deb -sSf -o "${tmp_file}"
apt-get install ./"${tmp_file}"
rm "${tmp_file}"
