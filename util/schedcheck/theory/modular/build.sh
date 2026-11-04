#!/bin/bash
# build.sh: the Lean library and its checks in dependency order, one Lean process at a time, capped.
# Logs to build/all.log: per file wall, user, peak RSS, rc, and the #print axioms lines.
# The e2e files are written by schedcheck first (e2e/regen.sh; e2e/README.md); the probes by their generators.
cd "$(dirname "$0")"
TB=$HOME/.elan/toolchains/leanprover--lean4---v4.35.0/bin
L=$TB/lean
run() {  # run LEANPATH FILE [OUT]
  local lp=$1 f=$2 o=$3 opt=""
  [ -n "$o" ] && opt="-o $o.olean -i $o.ilean"
  # a file above 2 MB may peak above 4 GB (one file at the 4 MB part bound: 6.6 GiB, cost review r8-1 minor 1):
  # wait for 30 GB available; below 2 MB every file here peaks below 3 GiB, so keep 10 GB free
  local need=10
  [ $(stat -c %s $f) -gt 2000000 ] && need=30
  while [ $(free -g | awk '/Mem/{print $7}') -lt $need ]; do sleep 20; done
  echo "== $f $(date +%T) load $(cut -d' ' -f1 /proc/loadavg) avail $(free -g | awk '/Mem/{print $7}')G" >> build/all.log
  LEAN_PATH=$lp /usr/bin/time -f "WALL %e USER %U SYS %S MAXRSS_KB %M" prlimit --as=25769803776 timeout 1800 \
    $L -j1 -DElab.async=false $opt $f 2>&1 | grep -v "^warning\|^  \|linter\|^$\|^Note\|^Hint\|warning:" >> build/all.log
  echo "rc ${PIPESTATUS[0]}" >> build/all.log
}
mkdir -p build split/build e2e/build
: > build/all.log
run build Modular.lean build/Modular
run build EmitLemmas.lean build/EmitLemmas
run build Scan.lean build/Scan
run build LastCaller.lean build/LastCaller
run build LastCallerEx.lean
run build SiteCond.lean build/SiteCond
run build Example.lean build/Example
run build Scale.lean
for f in ExLib ExChildIface ExChild ExChildRealizes ExTop ExTopRealizes; do
  run build:split/build split/$f.lean split/build/$f
done
run build examples/ScanEx.lean build/ScanEx
run build examples/ScanExLate.lean
run build examples/ScanTasks.lean
run build examples/EqvTest.lean
run build emit/out/Em16.lean
run build scan/bench/B1k.lean
run build scan/esc/Esc16.lean
run build scan/esc7/EscFn512.lean
run build scan/escS/EscS_spread_1024.lean
echo "(negative: EscS_neg_8 must fail, rc 1, at escChk)" >> build/all.log
run build scan/escS/EscS_neg_8.lean
run build scan/chain7/Ch256.lean
run build scan/chain8/Cs1024.lean
run build scan/tasks/TC_2564_32.lean
run build scan/tasks/TC_10240_256.lean
run build e2e/mtask/MethTask.lean
run build e2e/mejoin/SysMeJoin.lean
run build e2e/MkAssertedSchedules.lean
run build e2e/MkUrgencyBlocking.lean
for m in noagg nosched both; do run build e2e/modes8/MkUrgencyBlocking_$m.lean; done
run build e2e/lcprobe/LcProbe64.lean
run build e2e/SysArithPipe60.lean
run build e2e/MkBufferControl.lean e2e/build/MkBufferControl
run build e2e/MkMCR.lean
run build e2e/SysArithPipeNoTasks.lean e2e/build/SysArithPipeNoTasks
run build e2e/SysArithPipe.lean e2e/build/SysArithPipe
run build e2e/SysArithPipeSeg.lean
for fr in "" "--fresh"; do
  for m in SysArithPipe MkBufferControl; do
    echo "== leanchecker $fr $m" >> build/all.log
    PATH=$TB:$PATH LEAN_PATH=build:e2e/build /usr/bin/time -f "WALL %e USER %U SYS %S MAXRSS_KB %M" prlimit --as=25769803776 timeout 1800 \
      $TB/leanchecker $fr $m >> build/all.log 2>&1
    echo "rc $?" >> build/all.log
  done
done
echo "== leanchecker --fresh ExTopRealizes" >> build/all.log
PATH=$TB:$PATH LEAN_PATH=build:split/build /usr/bin/time -f "WALL %e USER %U SYS %S MAXRSS_KB %M" prlimit --as=25769803776 timeout 1800 \
  $TB/leanchecker --fresh ExTopRealizes >> build/all.log 2>&1
echo "rc $?" >> build/all.log
echo "== done $(date +%T)" >> build/all.log
