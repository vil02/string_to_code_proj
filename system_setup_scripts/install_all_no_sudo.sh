#!/usr/bin/env bash

set -euo pipefail


find . -name "setup_no_sudo_for_*.sh" -exec ./{} \;
