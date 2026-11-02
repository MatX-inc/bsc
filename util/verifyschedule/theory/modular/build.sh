#!/bin/bash
# build.sh: the whole prototype in dependency order, one Lean process at a time, capped.
# Logs to build/all.log: per file wall, user, peak RSS, and the #print axioms lines.
# The e2e files are emitted by e2e/tools/mke2e.py (commands in e2e/README.md); SysArithPipe60 with
# E2E_SEG_USES=128 (a multi-segment scan).
cd "$(dirname "$0")"
TB=$HOME/.elan/toolchains/leanprover--lean4---v4.35.0/bin
L=$TB/lean
run() {  # run LEANPATH FILE [OUT]
  local lp=$1 f=$2 o=$3 opt=""
  [ -n "$o" ] && opt="-o $o.olean -i $o.ilean"
  echo "== $f $(date +%T) load $(cut -d' ' -f1 /proc/loadavg) avail $(free -g | awk '/Mem/{print $7}')G" >> build/all.log
  LEAN_PATH=$lp /usr/bin/time -f "WALL %e USER %U SYS %S MAXRSS_KB %M" prlimit --as=25769803776 timeout 1800 \
    $L -j1 -DElab.async=false $opt $f 2>&1 | grep -v "^warning\|^  \|linter\|^$\|^Note\|^Hint" >> build/all.log
}
mkdir -p build split/build e2e/build
: > build/all.log
run build Modular.lean build/Modular
run build EmitLemmas.lean build/EmitLemmas
run build Scan.lean build/Scan
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
run build scan/chain7/Ch256.lean
run build scan/chain8/Cs1024.lean
run build scan/tasks/TC_2564_32.lean
run build scan/tasks/TC_10240_256.lean
run build e2e/SysArithPipe60.lean
run build e2e/MkBufferControl.lean e2e/build/MkBufferControl
run build e2e/SysArithPipeNoTasks.lean e2e/build/SysArithPipeNoTasks
run build e2e/SysArithPipe.lean e2e/build/SysArithPipe
for fr in "" "--fresh"; do
  for m in SysArithPipe MkBufferControl; do
    echo "== leanchecker $fr $m" >> build/all.log
    PATH=$TB:$PATH LEAN_PATH=build:e2e/build /usr/bin/time -f "WALL %e USER %U SYS %S MAXRSS_KB %M" prlimit --as=25769803776 timeout 1800 \
      $TB/leanchecker $fr $m >> build/all.log 2>&1
  done
done
echo "== leanchecker --fresh ExTopRealizes" >> build/all.log
PATH=$TB:$PATH LEAN_PATH=build:split/build /usr/bin/time -f "WALL %e USER %U SYS %S MAXRSS_KB %M" prlimit --as=25769803776 timeout 1800 \
  $TB/leanchecker --fresh ExTopRealizes >> build/all.log 2>&1
echo "== done" >> build/all.log
