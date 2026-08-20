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
# A RegFile load file is an input to the simulation, not to the build.
# The reference opens one when the model object is constructed, which is
# run time, so link -- both the reference's and ours -- must complete
# with the file absent.  (Verilog differs: $readmemh runs from an initial
# block.  The reference is what we match.)  Contents are then checked the
# usual way: byte parity on the run with the file in place.
check_memload() {
    name=RegFileLoadLink; top=sysRegFileLoadLink
    cp "$SRC/$name.bsv" .
    rm -f "$name.mem"
    $BSC -sim -bir -u -g "$top" "$name.bsv" >/dev/null 2>&1 || { echo "FAIL $name (bsc)"; fail=1; return; }
    $BSC -sim -bir -e "$top" -o ref.exe > reflink.out 2>&1 || { echo "FAIL $name (ref link)"; fail=1; return; }
    "$TRS" link "$top.bir" -o art > link.out 2>&1 || { echo "FAIL $name (trs link)"; fail=1; return; }
    # neither link may so much as name the file (a missing load file is
    # only a diagnostic, so silence -- not exit status -- is the contract)
    if grep -q "$name.mem" reflink.out; then echo "FAIL $name (ref link opened the .mem)"; fail=1; return; fi
    if grep -q "$name.mem" link.out; then echo "FAIL $name (trs link opened the .mem)"; sed -n 1,2p link.out; fail=1; return; fi
    # still absent: both must report it the same way at RUN time, which
    # also proves the greps above would have caught a load if one happened
    ./ref.exe > ref.absent 2>&1; refrc=$?
    TRS="$TRS" ./art > got.absent 2>&1; gotrc=$?
    if ! grep -q "$name.mem" ref.absent; then echo "FAIL $name (reference did not read it at run time either)"; fail=1; return; fi
    if [ "$refrc" != "$gotrc" ]; then echo "FAIL $name (absent: exit $refrc vs $gotrc)"; fail=1; return; fi
    if ! cmp -s ref.absent got.absent; then echo "FAIL $name (absent: stdout)"; diff ref.absent got.absent | head -4; fail=1; return; fi
    cp "$SRC/$name.mem" .
    ./ref.exe > ref.out 2>&1; refrc=$?
    TRS="$TRS" ./art > got.out 2>&1; gotrc=$?
    if [ "$refrc" != "$gotrc" ]; then echo "FAIL $name (exit $refrc vs $gotrc)"; fail=1; return; fi
    if ! cmp -s ref.out got.out; then echo "FAIL $name (stdout)"; diff ref.out got.out | head -3; fail=1; return; fi
    echo "PASS $name"
}
check_memload
# String args must run COMPILED: byte parity alone would pass on an
# interpreted fallback (see BdpiMin), and the point here is that the
# compiler does not bail out on a string.  The model .so beside the
# artifact is the tell.
check_compiled() { # name top [cfile]
    name=$1; top=$2; cfile=$3
    cp "$SRC/$name.bsv" .
    [ -n "$cfile" ] && cp "$SRC/$cfile" .
    rm -f art.so
    $BSC -sim -bir -u -g "$top" "$name.bsv" >/dev/null 2>&1 || { echo "FAIL $name (bsc)"; fail=1; return; }
    $BSC -sim -bir -e "$top" -o ref.exe $cfile >/dev/null 2>&1 || { echo "FAIL $name (ref link)"; fail=1; return; }
    ./ref.exe > ref.out 2>&1; refrc=$?
    "$TRS" link "$top.bir" -o art >/dev/null 2>&1 || { echo "FAIL $name (trs link)"; fail=1; return; }
    [ -f art.so ] || { echo "FAIL $name (fell back to interpreted)"; fail=1; return; }
    TRS="$TRS" ./art > got.out 2>&1; gotrc=$?
    if [ "$refrc" != "$gotrc" ]; then echo "FAIL $name (exit $refrc vs $gotrc)"; fail=1; return; fi
    if ! cmp -s ref.out got.out; then echo "FAIL $name (stdout)"; diff ref.out got.out | head -3; fail=1; return; fi
    echo "PASS $name"
}
# every way a constant string is built (param/literal concats, nesting,
# $display of a concat), across two instances with different parameters:
# compiled bodies are shared per equivalence class, so a baked-in string
# would show up as one instance wearing the other's text
check_compiled StrCatBdpi sysStrCatBdpi slen.c
# a string chosen by a runtime condition: not a per-instance constant —
# on this stack it still compiles (StrDyn marker values select among
# interned ids at runtime), and the output must match the reference
check_compiled StrDynSelect sysStrDynSelect slen.c
exit $fail
