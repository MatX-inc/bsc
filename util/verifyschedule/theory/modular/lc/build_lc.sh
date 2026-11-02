#!/bin/bash
# build_lc.sh: the last-caller files in dependency order against build_lc/, one Lean process at a time,
# logged to lc/build_lc.log (wall, user, peak RSS, #print axioms).
cd "$(dirname "$0")/.."
LOG=lc/build_lc.log
: > $LOG
for f in Modular:Modular EmitLemmas:EmitLemmas Scan:Scan LastCaller:LastCaller LastCallerEx: \
         e2e/lc/MkUrgencyBlocking: e2e/lc/MkBufferControlLC: e2e/MkBufferControl: e2e/lc/MkMCRLC: lc/probe/LcProbe64: ; do
  src=${f%%:*}.lean; out=${f##*:}
  while [ $(free -g | awk '/Mem/{print $7}') -lt 30 ]; do sleep 20; done
  ./lc/lean.sh $src "$out" 1800 2>&1 | grep -v 'deprecated' >> $LOG
done
echo "== done $(date +%T)" >> $LOG
