#!/bin/sh
# Edge-SSA regression battery: compile each design with the installed
# bsc, run the reference Bluesim executable and the trs AOT artifact
# (bare defaults = the specialized fast compile), and diff stdout +
# exit codes.  BSC=/path/bsc TRS=/path/trs sh run.sh [workdir]
BSC=${BSC:-bsc}
TRS=${TRS:-trs}
SRC=$(cd "$(dirname "$0")" && pwd)
case "$BSC" in
    */*) PATH="$(cd "$(dirname "$BSC")" && pwd):$PATH"; export PATH;;
esac
WK=${1:-$(mktemp -d)}
cd "$WK" || exit 2
fail=0
check() { # name top [cfile]
    name=$1; top=$2; cfile=$3
    cp "$SRC/$name.bsv" .
    [ -n "$cfile" ] && cp "$SRC/$cfile" .
    $BSC -sim -bir -u -g "$top" "$name.bsv" >/dev/null 2>&1 || { echo "FAIL $name (bsc)"; fail=1; return; }
    $BSC -sim -bir -e "$top" -o ref.exe $cfile >/dev/null 2>&1 || { echo "FAIL $name (ref link)"; fail=1; return; }
    ./ref.exe > ref.out 2>&1; refrc=$?
    "$TRS" link "$top.bir" -o art >/dev/null 2>&1 || { echo "FAIL $name (trs link)"; fail=1; return; }
    TRS="$TRS" ./art > got.out 2>&1; gotrc=$?
    if [ "$refrc" != "$gotrc" ]; then echo "FAIL $name (exit $refrc vs $gotrc)"; fail=1; return; fi
    if ! cmp -s ref.out got.out; then echo "FAIL $name (stdout)"; diff ref.out got.out | head -3; fail=1; return; fi
    echo "PASS $name"
}
check EdgeSelfKill sysEdgeSelfKill
check HoistDivTrap sysHoistDivTrap
# sched-cone RegFile warnings: evaluation count (proven: pre-fix
# doubled 2 -> 4) and eager-list order are part of byte parity
check RegFileWarnCone sysRegFileWarnCone
# ActionValue method on a user-module child, inlined; result width
# comes from the result (synthetic AV temps are in no def table)
check AvMethInline sysAvMethInline
# direct-BDPI (task #22): narrow + wide value imports must run
# COMPILED (a fallback-to-interp regression still passes stdout —
# the artifact note is the tell, but byte-parity is the contract)
check BdpiMin sysBdpiMin ops.c
exit $fail
