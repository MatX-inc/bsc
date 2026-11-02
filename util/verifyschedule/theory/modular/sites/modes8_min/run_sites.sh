#!/bin/bash
# run_sites.sh (round 8, soundness minors): the site tool (sites/tools, new) against the round-7 copy
# (base_tools), four modes on the 8 designs with site conditions, and the default corpus.
cd "$(dirname "$0")/.."
W=modes8_min
D8="mkBufferControl mkPrediction mkDeblockFilter mkMCR mkAssertedSchedules sysFloatTest sysConflictFreeOKLarge sysArithPipe"
for which in new base; do
  T=tools; [ $which = base ] && T=$W/base_tools
  for mode in default noagg nosched both; do
    o=$W/out_${which}_$mode; mkdir -p $o
    SITES_DATA=modes/$mode/data python3 -I $T/sitecond.py $o $(for d in $D8; do echo modes/$mode/$d; done) > $o/run.txt 2>&1
    echo "$which $mode done"
  done
  o=$W/out_${which}_corpus; mkdir -p $o
  SITES_DATA=data python3 -I $T/sitecond.py $o $(cat out/list.txt) > $o/run.txt 2>&1
  echo "$which corpus done"
done
