#!/bin/bash
# run.sh FILE...: each port probe once (capped), logs/<name>.log with section marks and totals.
cd "$(dirname "$0")"
mkdir -p logs
L=$HOME/.elan/toolchains/leanprover--lean4---v4.35.0/bin/lean
for f in "$@"; do
  n=$(basename $f .lean)
  while [ "$(free -g | awk '/Mem:/{print $7}')" -lt 30 ]; do sleep 20; done
  echo "== $n $(date +%T) load $(cut -d' ' -f1-3 /proc/loadavg) avail $(free -g | awk '/Mem:/{print $7}')G" > logs/$n.log
  ( LEAN_PATH=../../build /usr/bin/time -f "WALL %e USER %U SYS %S MAXRSS_KB %M" prlimit --as=25769803776 timeout 1800 \
      $L -j1 -DElab.async=false $f ) 2>&1 | grep "@@\|WALL\|axioms\|error" >> logs/$n.log
  awk '/@@/{t=$3; if (p) printf "  %s %.2f s %.0f Mhb\n", $2, (t-p)/1000, ($4-ph)/1e6; p=t; ph=$4}' logs/$n.log >> logs/$n.log
done
