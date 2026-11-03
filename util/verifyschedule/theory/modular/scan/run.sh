#!/bin/bash
# run.sh FILE [timeout] : check FILE against build/, capped; prints wall and peak RSS
cd "$(dirname "$0")"
L=$HOME/.elan/toolchains/leanprover--lean4---v4.35.0/bin/lean
T=${2:-600}
LEAN_PATH=build /usr/bin/time -f "WALL %e USER %U MAXRSS_KB %M" prlimit --as=25769803776 timeout $T $L -j1 -DElab.async=false "$1" 2>&1
