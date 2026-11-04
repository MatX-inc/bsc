#!/bin/bash
# run.sh: the linear escape check, one pair with W facts (cost review r8-2 M1); capped, one Lean process at a time
cd "$(dirname "$0")"; R=../..
L=$HOME/.elan/toolchains/leanprover--lean4---v4.35.0/bin/lean
one() {  # one SHAPE W
  f=EscS_$1_$2.lean; python3 -I genescS.py $1 $2 $f > /dev/null
  while [ $(free -g | awk '/Mem/{print $7}') -lt 12 ]; do sleep 20; done
  l0=$(cut -d' ' -f1 /proc/loadavg)
  ( cd $R && LEAN_PATH=build /usr/bin/time -f "WALL %e USER %U SYS %S MAXRSS_KB %M" prlimit --as=25769803776 timeout 1800 \
      $L -j1 -DElab.async=false scan/escS/$f ) > log_$1_$2.full 2>&1
  rc=$?
  w=$(grep WALL log_$1_$2.full); kb=$(echo "$w" | sed 's/.*MAXRSS_KB //')
  tc=$(grep -P '^\ttype checking' log_$1_$2.full | tail -1 | sed 's/.*type checking //')
  echo "$1 $2 rc=$rc $w ($(awk "BEGIN{printf \"%.2f\", $kb/1048576}") GiB) kernel(type checking, check part) $tc load $l0-$(cut -d' ' -f1 /proc/loadavg) bytes $(stat -c %s $f) | $(grep 'depends on axioms' log_$1_$2.full)" >> run.txt
}
: > run.txt
one neg 8
for W in 256 512 1024 2048 4096; do one spread $W; done
for W in 1024 4096; do one fan $W; done
echo done >> run.txt
