#!/bin/bash
# lean.sh FILE [OUT] [TIMEOUT]: capped Lean run against build_lc; prints wall, user, peak RSS
L=$HOME/.elan/toolchains/leanprover--lean4---v4.35.0/bin/lean
cd "$(dirname "$0")/.."
F=$1; O=$2; T=${3:-1200}
OPT=""; [ -n "$O" ] && OPT="-o build_lc/$O.olean -i build_lc/$O.ilean"
echo "== $F $(date +%T) load $(cut -d' ' -f1 /proc/loadavg) avail $(free -g | awk '/Mem/{print $7}')G"
LEAN_PATH=build_lc /usr/bin/time -f "WALL %e USER %U SYS %S MAXRSS_KB %M" prlimit --as=25769803776 timeout $T \
  $L -j1 -DElab.async=false $OPT "$F" 2>&1 | grep -v "^warning\|^  \|linter\|^$\|^Note\|^Hint"
