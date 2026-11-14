#!/bin/bash
# usage: job.sh <variant> <suite> : runs make check (and localcheck when the suite has
# top-level .exp files) in ts-<variant>/testsuite/<suite> with the variant's flags
# passed through the testsuite's own TEST_BSC_OPTIONS (exported to bsc as BSC_OPTIONS)
W=/home/ravi/claude-tmp/claude-1548697660/-home-ravi/6640a2f0-3ba9-46af-8f63-2f02110094cd/scratchpad/isimp/work/keepflags/keep-suites
I=/home/ravi/claude-tmp/claude-1548697660/-home-ravi/6640a2f0-3ba9-46af-8f63-2f02110094cd/scratchpad/isimp/base/inst
v=$1; s=$2
case $v in
  MAIN) F="" ;;
  K) F="-keep-fires -keep-inlined-boundaries" ;;
  KM) F="-keep-fires -keep-inlined-boundaries -keep-method-conds" ;;
  M) F="-keep-method-conds" ;;
  *) echo "bad variant $v"; exit 1 ;;
esac
unset GHCRTS
while :; do a=$(free -g | awk '/^Mem:/{print $7}'); [ "$a" -ge 24 ] && break; sleep 30; done
L=$W/logs/$v; mkdir -p $L
T=$W/ts-$v/testsuite/$s
t0=$(date +%s)
make -C $T check CHECKPREREQUISITES=clean TEST_RELEASE=$I VTEST=1 CTEST=1 SYSTEMCTEST=0 CXXFLAGS=-O0 LDFLAGS=-Wl,-fuse-ld=lld TEST_BSC_OPTIONS="$F" > $L/$s.out 2>&1
mkdir -p $L/rt-$s; for f in $T/*.log $T/*.sum; do [ -f "$f" ] && cp "$f" $L/rt-$s/check.$(basename "$f"); done
if ls $T/*.exp >/dev/null 2>&1; then
  make -C $T localcheck TEST_RELEASE=$I VTEST=1 CTEST=1 SYSTEMCTEST=0 CXXFLAGS=-O0 LDFLAGS=-Wl,-fuse-ld=lld TEST_BSC_OPTIONS="$F" > $L/$s.top.out 2>&1
  for f in $T/*.log $T/*.sum; do [ -f "$f" ] && cp "$f" $L/rt-$s/top.$(basename "$f"); done
fi
echo "$v $s done $(( $(date +%s) - t0 ))s $(date +%T)" >> $W/logs/status
