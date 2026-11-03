#!/bin/bash
# run_sites.sh: the site-condition tool on each mode's compiled designs
cd "$(dirname "$0")/.."
for mode in default noagg nosched both; do
  mkdir -p modes/out_$mode
  SITES_DATA=modes/$mode/data python3 -I tools/sitecond.py modes/out_$mode modes/$mode/mkBufferControl modes/$mode/mkPrediction modes/$mode/mkDeblockFilter modes/$mode/mkMCR modes/$mode/mkAssertedSchedules modes/$mode/sysFloatTest modes/$mode/sysConflictFreeOKLarge modes/$mode/sysArithPipe > modes/out_$mode/run.txt 2>&1
  echo "$mode done"
done
