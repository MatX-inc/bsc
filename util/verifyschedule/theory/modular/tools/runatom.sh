#!/bin/bash
# Check the atomized files one at a time (prlimit 24 GB, timeout), logging result, wall time and peak RSS.
L=$HOME/.elan/toolchains/leanprover--lean4---v4.35.0/bin/lean
D=$(cd "$(dirname "$0")/.." && pwd)/atom
for d in "$@"; do
  /usr/bin/time -f "%e s %M KB" -o $D/$d.time prlimit --as=25769803776 timeout 1500 $L -j1 -DElab.async=false $D/$d.lean > $D/$d.out 2>&1
  echo "$d rc=$? $(cat $D/$d.time | tail -1)" >> $D/results.txt
done
echo done >> $D/results.txt
