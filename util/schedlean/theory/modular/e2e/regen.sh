#!/bin/bash
# regen.sh IN: the files of README.md's table from schedlean, into this directory, logged to regen.log.
# IN holds the compiled modules: IN/corpus/<m>/<m>.ba, IN/lcmodes/<mode>/mkUrgencyBlocking/, IN/lcprobe/mkLcProbe64/,
# IN/mtask/mkMethTask/, IN/mejoin/sysMeJoin/.  SCHEDLEAN names the tool (default: the checkout's).
set -u
IN=$(cd "$1" && pwd)
cd "$(dirname "$0")"
REPO=$(cd ../../../../.. && pwd)
B=${SCHEDLEAN:-$REPO/inst/bin/schedlean}
[ -x "$B" ] || B=$REPO/src/comp/schedlean
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
( cd lcprobe && $E -o LcProbe64.lean $IN/lcprobe/mkLcProbe64/mkLcProbe64.ba )
# MethTaskBadEn: the rule's task statement enabled always, which must fail
( cd mtask && $E -o MethTask.lean $IN/mtask/mkMethTask/mkMethTask.ba &&
  sed -e 's/(.leaf (fun V (ops : Ops V) (E : EqC V) (oc : OC V) => WF ops E oc 1 \&\& true))/(.leaf (fun V (ops : Ops V) (E : EqC V) (oc : OC V) => true))/' \
      -e 's/^namespace MethTask$/namespace MethTaskBadEn/' -e 's/^end MethTask$/end MethTaskBadEn/' \
      -e 's/MethTask\.obl/MethTaskBadEn.obl/; s/MethTask\.top/MethTaskBadEn.top/' MethTask.lean > MethTaskBadEn.lean &&
  diff MethTask.lean MethTaskBadEn.lean | grep -c '^>' )
# mejoin: the emitter must stop; the gate still finds the last-caller port
( cd mejoin && $E -o SysMeJoin.lean $IN/mejoin/sysMeJoin/sysMeJoin.ba 2>&1 | tail -1;
  $E --gate $IN/mejoin/sysMeJoin/sysMeJoin.ba | grep -o '"lc": [^]]*]]' )
} > regen.log 2>&1
echo done >> regen.log
