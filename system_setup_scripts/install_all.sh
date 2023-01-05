#!/usr/bin/env bash

set -euo pipefail


find . -name "setup_for_*.sh" -exec ./{} \;
