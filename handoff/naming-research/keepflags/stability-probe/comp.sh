#!/bin/bash
# usage: comp.sh : compiles every generated probe edit (gen/<probe>/<edit>) to Verilog and Bluesim
# under each variant into out/<variant>/<probe>/<edit>/
PR=/home/ravi/claude-tmp/claude-1548697660/-home-ravi/6640a2f0-3ba9-46af-8f63-2f02110094cd/scratchpad/isimp/work/keepflags/keep-suites/probe
I=/home/ravi/claude-tmp/claude-1548697660/-home-ravi/6640a2f0-3ba9-46af-8f63-2f02110094cd/scratchpad/isimp/base/inst
declare -A F=([MAIN]="" [K]="-keep-fires -keep-inlined-boundaries" [KM]="-keep-fires -keep-inlined-boundaries -keep-method-conds" [M]="-keep-method-conds")
unset GHCRTS
for v in MAIN K KM M; do
  for p in Probe Probe2; do
    for e in $(ls $PR/gen/$p); do
      d=$PR/out/$v/$p/$e; mkdir -p $d/v $d/s
      src=$(ls $PR/gen/$p/$e/)
      cp $PR/gen/$p/$e/$src $d/
      ( cd $d && $I/bin/bsc -u -verilog -bdir v -vdir v -info-dir v ${F[$v]} $src > v.log 2>&1; echo "exit=$?" >> v.log
                 $I/bin/bsc -u -sim -bdir s -simdir s -info-dir s ${F[$v]} $src > s.log 2>&1; echo "exit=$?" >> s.log ) &
    done
  done
done
wait
grep -L "exit=0" $PR/out/*/*/*/v.log $PR/out/*/*/*/s.log
