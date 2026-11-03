#!/bin/bash
L=$HOME/.elan/toolchains/leanprover--lean4---v4.35.0/bin/lean
cd "$(dirname "$0")"
for f in TC_512_32 TC_2564_32 TC_2564_256 TC_10240_256 TC_40960_1024; do
  echo "== $f $(uptime)"
  LEAN_PATH=../../build /usr/bin/time -f "WALL %e USER %U MAXRSS_KB %M" prlimit --as=25769803776 timeout 1200 $L -j1 -DElab.async=false $f.lean 2>&1 | grep -v "^warning\|linter" | grep "type checking\|WALL\|axioms\|error\|took"
done
