#!/bin/sh
# M0 spike battery: run every fixture on the given verilator binary.
#   sh run.sh [verilator-binary]
# For a post-5.046 verilator built from source, set VERILATOR_ROOT.
set -e
VLT=${1:-verilator}
cd "$(dirname "$0")"
for f in counter shadow argrdy liar xing violator xprobe params fatal; do
  python3 driver.py "$VLT" "$f" "out/run-$f" || exit 1
done
echo "M0 battery: all fixtures PASS on $($VLT --version)"
