#!/bin/bash
# usage: run.sh ; compiles every probe top under five flag sets, Verilog and Bluesim
unset GHCRTS
W=/home/ravi/claude-tmp/claude-1548697660/-home-ravi/6640a2f0-3ba9-46af-8f63-2f02110094cd/scratchpad/isimp/work/anchor-roles
BSC=/home/ravi/claude-tmp/claude-1548697660/-home-ravi/6640a2f0-3ba9-46af-8f63-2f02110094cd/scratchpad/isimp/base/inst/bin/bsc
declare -A FL=( [def]="" [kf]="-keep-fires" [kib]="-keep-inlined-boundaries" \
                [kf_kib]="-keep-fires -keep-inlined-boundaries" \
                [kf_kib_kmc]="-keep-fires -keep-inlined-boundaries -keep-method-conds" )
TOPS="EnRdy:sysEnRdy Inlined:sysInlined MethFwd:sysMethFwd TwoRules:sysTwoRules Always:sysAlwaysParent Fifo:sysFifo Wires:sysWires"
for f in def kf kib kf_kib kf_kib_kmc; do
  for t in $TOPS; do
    pkg=${t%%:*}; top=${t##*:}
    V=$W/out/$f/v/$pkg; S=$W/out/$f/sim/$pkg
    mkdir -p $V $S
    ( cd $W/probes && $BSC ${FL[$f]} -verilog -u -g $top -bdir $V -vdir $V -info-dir $V -p .:%/Libraries $pkg.bsv ) > $V/log 2>&1 || echo "VFAIL $f $pkg"
    ( cd $W/probes && $BSC ${FL[$f]} -sim -u -g $top -bdir $S -simdir $S -info-dir $S -p .:%/Libraries $pkg.bsv \
      && $BSC ${FL[$f]} -sim -e $top -bdir $S -simdir $S -info-dir $S -p $S:%/Libraries -o $S/$top ) > $S/log 2>&1 || echo "SFAIL $f $pkg"
  done
done
echo done
