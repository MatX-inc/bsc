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
# $finish edge completion (compiled paths): rules scheduled after
# the $finish rule still run — state lands, output suppressed.
# Batch stdout gates the suppression half (count's finish-edge line
# must vanish); the state half is peeked by the interactive
# FinishPeek witness (same shape, jit engine)
check FinishEdge sysFinishEdge
# expected-file variant: designs the REFERENCE cannot express — the
# .out.expected is the contract instead of a ref build
check_expected() { # name top
    name=$1; top=$2
    cp "$SRC/$name.bsv" .
    $BSC -sim -bir -u -g "$top" "$name.bsv" >/dev/null 2>&1 || { echo "FAIL $name (bsc)"; fail=1; return; }
    # the .bir exports during the -e link, BEFORE the C++ compile that
    # fails for these designs — ignore the exit code, require the .bir
    $BSC -sim -bir -e "$top" -o ref.exe >/dev/null 2>&1
    [ -f "$top.bir" ] || { echo "FAIL $name (no .bir)"; fail=1; return; }
    "$TRS" link "$top.bir" -o art >/dev/null 2>&1 || { echo "FAIL $name (trs link)"; fail=1; return; }
    TRS="$TRS" ./art > got.out 2>&1
    if ! cmp -s "$SRC/$name.out.expected" got.out; then echo "FAIL $name (stdout)"; diff "$SRC/$name.out.expected" got.out | head -3; fail=1; return; fi
    echo "PASS $name"
}
# BRAM byte enables past lane 63 (128 lanes on 1024-bit data): the
# reference's generated C++ does not compile at these widths
# (bs_wide_data.h operator!= overload miss), so the expected file is
# the contract — top byte AND low byte zeroed by the lane-127|lane-0
# write, everything between stays 0xAA
check_expected BramWideBE sysBramWideBE
exit $fail
