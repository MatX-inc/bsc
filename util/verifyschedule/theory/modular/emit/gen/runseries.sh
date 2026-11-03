#!/bin/bash
# usage: runseries.sh m...   one Lean process at a time; appends to out/series.txt
cd "$(dirname "$0")/.."
L=$HOME/.elan/toolchains/leanprover--lean4---v4.35.0/bin/lean
for m in "$@"; do
  python3 -I gen/mkemit.py $m out/Em$m.lean
  while [ "$(free -g | awk '/Mem:/{print $7}')" -lt 30 ]; do sleep 30; done
  LEAN_PATH=build /usr/bin/time -f "%e %U %M" -o out/Em$m.time prlimit --as=25769803776 timeout 3000 \
    $L -j1 -DElab.async=false -o out/Em$m.olean out/Em$m.lean > out/Em$m.out 2>&1
  rc=$?
  ax=$(grep -c "depends on axioms: \[propext, Classical.choice, Quot.sound\]" out/Em$m.out)
  echo "m=$m children=$((2*m+3)) go_sites=$((2*m+1)) rc=$rc std_axioms=$ax lines=$(wc -l < out/Em$m.lean) bytes=$(wc -c < out/Em$m.lean) olean=$(stat -c %s out/Em$m.olean 2>/dev/null) time(wall,user,KB)=$(tail -1 out/Em$m.time)" >> out/series.txt
done
