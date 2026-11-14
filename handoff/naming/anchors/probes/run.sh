#!/bin/bash
# usage: run.sh <src-file> <top> <variant-dir-name> [extra bsc flags...]
# Compiles <src-file> with the MAIN install for Verilog (out/<variant>/v)
# and Bluesim (out/<variant>/c, linked to <top>.exe), with dumps of the
# naming-relevant stages.
set -e
P=/home/ravi/claude-tmp/claude-1548697660/-home-ravi/6640a2f0-3ba9-46af-8f63-2f02110094cd/scratchpad/isimp/work/anchors/anchors-inv2/probes
BSC=${BSC:-/home/ravi/claude-tmp/claude-1548697660/-home-ravi/6640a2f0-3ba9-46af-8f63-2f02110094cd/scratchpad/isimp/base/inst/bin/bsc}
srcf=$1; top=$2; var=$3; shift 3
O=$P/out/$var
mkdir -p $O/v $O/c
unset GHCRTS
VD="-dexpanded=expanded.dump -dinline=inline.dump -dtransform=transform.dump -dATS=ats.dump -dastate=astate.dump -drenameio=renameio.dump -daopt=aopt.dump -dfinalcleanup=final.dump"
( cd $O/v && $BSC -u -verilog -bdir . -vdir . -info-dir . -p $P/src:%/Libraries \
    $VD -g $top "$@" $P/src/$srcf > log 2>&1 ) || { echo "verilog FAIL $var"; tail -20 $O/v/log; }
( cd $O/c && $BSC -u -sim -bdir . -simdir . -info-dir . -p $P/src:%/Libraries \
    -dexpanded=expanded.dump -dATS=ats.dump -g $top "$@" $P/src/$srcf > log 2>&1 && \
  $BSC -sim -e $top -simdir . -bdir . -p .:%/Libraries -dsimPackageOpt=simpkgopt.dump -o $top.exe >> log 2>&1 ) || { echo "sim FAIL $var"; tail -20 $O/c/log; }
echo "done $var"
