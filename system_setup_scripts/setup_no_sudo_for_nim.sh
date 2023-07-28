#!/usr/bin/env bash

set -euo pipefail

tmp_script_name="tmp_install_nim.sh"
readonly tmp_script_name

if [[ -e "${tmp_script_name}" ]]
then
    printf "The file \`%s\` already exists.\n" "${tmp_script_name}"
    exit 1
fi

curl https://nim-lang.org/choosenim/init.sh -sSf -o "${tmp_script_name}"
chmod +x "${tmp_script_name}"
./"${tmp_script_name}" -y
rm "${tmp_script_name}"

echo "export PATH=${HOME}/.nimble/bin:\$PATH" >> ~/.profile
if [[ -n "${GITHUB_PATH}" ]]
then
    echo "${HOME}/.nimble/bin" >> "${GITHUB_PATH}"
fi
