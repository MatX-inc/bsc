#!/bin/bash
# regen.sh IN: the files of README.md's table from schedcheck, into this directory, logged to regen.log.
# IN holds the compiled modules: IN/corpus/<m>/<m>.ba, IN/lcmodes/<mode>/mkUrgencyBlocking/, IN/lcprobe/mkLcProbe64/,
# IN/mtask/mkMethTask/, IN/mejoin/sysMeJoin/.  SCHEDCHECK names the tool (default: the checkout's).
set -u
IN=$(cd "$1" && pwd)
cd "$(dirname "$0")"
REPO=$(cd ../../../../.. && pwd)
B=${SCHEDCHECK:-$REPO/inst/bin/schedcheck}
[ -x "$B" ] || B=$REPO/src/comp/schedcheck
E="prlimit --as=34359738368 timeout 1800 $B"
mkdir -p modes8 lcprobe mtask mejoin
{
$E -o SysArithPipe.lean $IN/corpus/sysArithPipe/sysArithPipe.ba
$E --no-task-agreement -o SysArithPipeNoTasks.lean $IN/corpus/sysArithPipe/sysArithPipe.ba
$E --limit seg-uses=128 --first-rules 60 -o SysArithPipe60.lean $IN/corpus/sysArithPipe/sysArithPipe.ba
$E --limit seg-uses=600 -o SysArithPipeSeg.lean $IN/corpus/sysArithPipe/sysArithPipe.ba
$E -o MkBufferControl.lean $IN/corpus/mkBufferControl/mkBufferControl.ba
$E -o MkUrgencyBlocking.lean $IN/corpus/mkUrgencyBlocking/mkUrgencyBlocking.ba
for m in noagg nosched both; do
  $E -o modes8/MkUrgencyBlocking_$m.lean $IN/lcmodes/$m/mkUrgencyBlocking/mkUrgencyBlocking.ba
done
$E -o MkMCR.lean $IN/corpus/mkMCR/mkMCR.ba
# user assertions in the file: a mutually_exclusive line and a conflict_free check (design.md section 5)
$E -o MkAssertedSchedules.lean $IN/corpus/mkAssertedSchedules/mkAssertedSchedules.ba
( cd lcprobe && $E -o LcProbe64.lean $IN/lcprobe/mkLcProbe64/mkLcProbe64.ba )
# MethTask, checked, and six hardware mutants, which must fail (mutant 3, the rule's task statement
# enabled always, was MethTaskBadEn); Lean from LEAN or PATH
( cd mtask && $E --mutants 6 -o MethTask.lean $IN/mtask/mkMethTask/mkMethTask.ba )
# mejoin: a check line with two rules on its first side, one check per rule of it; the gate finds the
# last-caller port the file has
( cd mejoin && $E -o SysMeJoin.lean $IN/mejoin/sysMeJoin/sysMeJoin.ba 2>&1 | tail -1;
  $E --gate $IN/mejoin/sysMeJoin/sysMeJoin.ba | grep -o '"lc": [^]]*]]' )
} > regen.log 2>&1
echo done >> regen.log
