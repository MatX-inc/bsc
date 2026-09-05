#!/bin/sh
set -eu
p0_spike_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
exec "${P0_PYTHON:-python3}" "$p0_spike_dir/runner.py" "$@"
