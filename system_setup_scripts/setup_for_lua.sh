#!/usr/bin/env bash

set -euo pipefail

apt-get install -y --no-install-recommends luarocks
luarocks install luacheck
