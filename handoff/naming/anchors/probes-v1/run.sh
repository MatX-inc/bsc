#!/bin/bash
# usage: run.sh <src-file> <top> <variant-dir-name> [extra bsc flags...]
# compiles <src-file> on MAIN for Verilog and Bluesim into probes/out/<variant>/{v,c}
set -e
P=/home/ravi/claude-tmp/claude-1548697660/-home-ravi/6640a2f0-3ba9-46af-8f63-2f02110094cd/scratchpad/isimp/work/anchors/anchors-inventory/probes
BSC=${BSC:-/home/ravi/claude-tmp/claude-1548697660/-home-ravi/6640a2f0-3ba9-46af-8f63-2f02110094cd/scratchpad/isimp/base/inst/bin/bsc}
srcf=$1; top=$2; var=$3; shift 3
O=$P/out/$var
mkdir -p $O/v $O/c
unset GHCRTS
( cd $O/v && $BSC -u -verilog -bdir . -vdir . -info-dir . -p $P/src:+ \
    -dATS=ats.dump -dastate=astate.dump -daopt=aopt.dump -dexpanded=expanded.dump \
    -g $top "$@" $P/src/$srcf > log 2>&1 ) || { echo "verilog FAIL $var"; tail -20 $O/v/log; }
( cd $O/c && $BSC -u -sim -bdir . -simdir . -info-dir . -p $P/src:+ \
    -dATS=ats.dump -dastate=astate.dump -dsimExpand=simexpand.dump \
    -g $top "$@" $P/src/$srcf > log 2>&1 && $BSC -sim -e $top -simdir . -bdir . -o $top.exe >> log 2>&1 ) || { echo "sim FAIL $var"; tail -20 $O/c/log; }
echo "done $var"
