#!/bin/sh
# VCD byte-parity battery: compile each design with the installed bsc,
# run reference Bluesim and trs with the same arguments, and diff the
# VCD output (modulo the $date line) and stdout.
#
#   BSC=/path/to/bsc TRS=/path/to/trs sh run.sh [workdir]
BSC=${BSC:-bsc}
TRS=${TRS:-trs}
SRC=$(cd "$(dirname "$0")" && pwd)
# the reference executable is a script that needs bluetcl on PATH
case "$BSC" in
    */*) PATH="$(cd "$(dirname "$BSC")" && pwd):$PATH"; export PATH;;
esac
WK=${1:-$(mktemp -d)}
cd "$WK" || exit 2

fail=0
vcddiff() { # a b
    diff "$(mktemp -u)" /dev/null >/dev/null 2>&1 # noop for shells without <()
    sed 2d "$1" > .a.$$ && sed 2d "$2" > .b.$$
    diff .a.$$ .b.$$ >/dev/null
    r=$?
    rm -f .a.$$ .b.$$
    return $r
}
check() { # name top args...
    name=$1; top=$2; shift 2
    rm -f test1.vcd test2.vcd ref.test1.vcd ref.test2.vcd ref.vcd mine.vcd ref.out mine.out
    cp "$SRC/$name.bsv" .
    $BSC -sim -bir -u -g "$top" "$name.bsv" >/dev/null 2>&1
    $BSC -sim -bir -e "$top" -o "$top.exe" >/dev/null 2>&1
    ./"$top.exe" -V ref.vcd "$@" > ref.out 2>/dev/null
    for f in test1.vcd test2.vcd; do [ -f "$f" ] && mv "$f" "ref.$f"; done
    $TRS run "$top.bir" -V mine.vcd "$@" > mine.out 2>&1
    ok=1
    diff ref.out mine.out >/dev/null || ok=0
    vcddiff ref.vcd mine.vcd || ok=0
    for f in test1.vcd test2.vcd; do
        if [ -f "ref.$f" ]; then vcddiff "ref.$f" "$f" || ok=0; fi
    done
    if [ "$ok" = 1 ]; then echo "PASS $name"; else echo "FAIL $name"; fail=1; fi
}

check VcdGT sysVcdGT -m 12
check VCDTest1 sysVCDTest1 -m 12
check VCDTest2 sysVCDTest2 -m 60
check SameCanFire sysSameCanFire -m 20
check MClk sysMClk -m 20
check SyncB sysSyncB -m 20
check SyncHR sysSyncHR -m 22
check BramVcd sysBramVcd -m 25
check CDiv sysCDiv -m 25
# $finish edge boundary: the reference DROPS the finish instant's
# buffered changes at shutdown (vcd.cxx flush_changes early-return
# at t==now) — the post-finish state writes must NOT appear
check FinishEdge sysFinishEdge -m 20
exit $fail
