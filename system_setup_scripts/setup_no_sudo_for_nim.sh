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

username=$(whoami)
readonly username
echo "export PATH=/home/${username}/.nimble/bin:\$PATH" >> ~/.bashrc
