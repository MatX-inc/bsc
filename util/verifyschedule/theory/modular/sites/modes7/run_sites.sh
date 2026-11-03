#!/bin/bash
# run_sites.sh (round 7): the site tool with the readiness rule sequenced by expression structure.
cd "$(dirname "$0")/.."
for mode in default noagg nosched both; do
  mkdir -p modes7/out_$mode
  SITES_RDY_TRACE=1 SITES_DATA=modes/$mode/data python3 -I tools/sitecond.py modes7/out_$mode modes/$mode/mkBufferControl modes/$mode/mkPrediction modes/$mode/mkDeblockFilter modes/$mode/mkMCR modes/$mode/mkAssertedSchedules modes/$mode/sysFloatTest modes/$mode/sysConflictFreeOKLarge modes/$mode/sysArithPipe > modes7/out_$mode/run.txt 2>&1
  echo "$mode done"
done
mkdir -p modes7/out_corpus
python3 -I tools/sitecond.py modes7/out_corpus $(cat out/list.txt) > modes7/out_corpus/run.txt 2>&1
echo "corpus done"
