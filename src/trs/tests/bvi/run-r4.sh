#!/bin/sh
# BVI R4 gate battery: the BviPrim runtime, differential against the
# Verilog flow (iverilog PRIMARY oracle, per the design's oracle
# contract).  Each positive fixture builds twice from the same BSV:
#   oracle: bsc -verilog -vsim iverilog  (the netlist semantics)
#   trs:    bsc -sim -trs -e  ->  trs run  (BviPrim shadow protocol)
# and stdout + exit code must agree ($finish noise lines filtered).
# BSC=/path/bsc TRS=/path/trs sh run-r4.sh [workdir]
BSC=${BSC:-bsc}
TRS=${TRS:-trs}
TRSBIR=${TRSBIR:-trs-bir}
case "$TRSBIR" in
    */*) PATH="$(cd "$(dirname "$TRSBIR")" && pwd):$PATH"; export PATH;;
esac
# `bsc -sim -trs -e <top>` is two programs now: bsc elaborates each
# synthesis boundary to a .ba, and trs-bir exports one .bir per .ba.
# The verilate step moved with the split -- it runs in `trs link`, not
# in a bsc link -- so an export failing here is a REFUSAL, which is
# what the negatives below check for.
# Verilation is a BUILD step and `trs run` is load-only, so a battery
# that runs a design has to build its models first.  `bsc -sim -trs -e`
# used to fold this into the export; `trs link` does it now, and
# `trs vlt build` is the same step without producing an artifact.
build_models() { # top
    $TRS vlt build "$1.bir" >/dev/null 2>&1 || return 1
}
export_bir() { # top [trs-bir args...]
    eb_top=$1; shift
    for eb_f in *.ba; do
        [ -e "$eb_f" ] || continue
        eb_m=${eb_f%.ba}
        [ "$eb_m" = "$eb_top" ] && continue
        case $eb_m in sys*) continue;; esac
        $TRSBIR "$eb_f" >/dev/null 2>&1 || continue
    done
    $TRSBIR "$@" "$eb_top.ba"
}
SRC=$(cd "$(dirname "$0")" && pwd)
case "$BSC" in
    */*) PATH="$(cd "$(dirname "$BSC")" && pwd):$PATH"; export PATH;;
esac
case "$TRS" in
    */*) TRSDIR=$(cd "$(dirname "$TRS")" && pwd); TRS=$TRSDIR/$(basename "$TRS")
         PATH="$TRSDIR:$PATH"; export PATH;;
esac
WK=${1:-$(mktemp -d)}
mkdir -p "$WK"; WK=$(cd "$WK" && pwd)
fail=0
export TRS_VLT_CACHE="$WK/cache"

differ() { # name top
    name=$1; top=$2
    d="$WK/$name"; rm -rf "$d"; mkdir -p "$d"; cd "$d" || exit 2
    cp "$SRC/$name.bsv" .
    cp "$SRC"/rtl/*.v .
    $BSC -verilog -u -g "$top" "$name.bsv" >v.out 2>&1 \
        && $BSC -verilog -vsim iverilog -e "$top" -o vref.exe >>v.out 2>&1 || {
        echo "FAIL $name (verilog oracle build)"; head -3 v.out; fail=1; return; }
    timeout 120 ./vref.exe > vref.out 2>&1; vrc=$?
    $BSC -sim -u -g "$top" "$name.bsv" >b.out 2>&1 || {
        echo "FAIL $name (bsc compile)"; head -3 b.out; fail=1; return; }
    export_bir "$top" >link.out 2>&1 && build_models "$top" || {
        echo "FAIL $name (trs link)"; head -5 link.out; fail=1; return; }
    timeout 120 "$TRS" run "$top.bir" > got.out 2>&1; grc=$?
    # iverilog prints a "$finish called at ..." style notice; the
    # bsc-generated main also reports "Verilog $finish" -- neither is
    # design output
    grep -v '\$finish' vref.out > vref.flt
    grep -v '\$finish' got.out > got.flt
    if [ "$vrc" != "$grc" ]; then
        echo "FAIL $name (exit $vrc vs $grc)"; fail=1; return
    fi
    if ! cmp -s vref.flt got.flt; then
        echo "FAIL $name (stdout)"; diff vref.flt got.flt | head -6
        fail=1; return
    fi
    echo "PASS $name"
}

differ PosCounter sysPosCounter
differ PosShadowAction sysPosShadowAction
differ PosShadowFixed sysPosShadowFixed
differ PosArgRdy sysPosArgRdy

exit $fail
