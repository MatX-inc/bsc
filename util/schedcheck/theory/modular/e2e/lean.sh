#!/bin/bash
# lean.sh DIR FILE [timeout] [olean-out]: check FILE with LEAN_PATH=DIR, capped; prints wall, user, peak RSS
L=$HOME/.elan/toolchains/leanprover--lean4---v4.35.0/bin/lean
D=$1; F=$2; T=${3:-1200}; O=$4
if [ -n "$O" ]; then OPT="-o $O.olean -i $O.ilean"; else OPT=""; fi
LEAN_PATH=$D /usr/bin/time -f "WALL %e USER %U MAXRSS_KB %M" prlimit --as=25769803776 timeout $T $L -j1 -DElab.async=false $OPT "$F" 2>&1
