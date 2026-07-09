#!/bin/sh
# Interactive (bluetcl `sim`) battery: for each design in the
# reference interactive testsuite, build the reference Bluesim model
# AND the trs capi model (`trs link --interactive`), run every
# .cmd script through BOTH via the bluesim.tcl wrapper, and diff the
# outputs byte-for-byte — the reference is the oracle, so this holds
# even where the checked-in .expected files drift.
#
# Matrix mirrored from testsuite/bsc.bluesim/interactive/
# interactive.exp — keep in sync (22 sim_output assertions).
#
#   BSC=/path/bsc TRS=/path/trs TRS_CAPI_LIB=/path/libtrs_capi.a \
#       sh run.sh [workdir]
#
# `expected-status` entries: the wrapper's exit code must ALSO match.
BSC=${BSC:-bsc}
TRS=${TRS:-trs}
SRC=$(cd "$(dirname "$0")" && pwd)
TSRC=$(cd "$SRC/../../../../testsuite/bsc.bluesim/interactive" && pwd)
case "$BSC" in
    */*) PATH="$(cd "$(dirname "$BSC")" && pwd):$PATH"; export PATH;;
esac
WK=${1:-$(mktemp -d)}
mkdir -p "$WK" || exit 2
cd "$WK" || exit 2
# capability tiers (docs/TCL-CAPI.md): def/symbol peeks need the
# INTERP engine's recording; async.cmd's fixed wall needs the JIT
# engine's speed and touches no symbols — set per test below
export TRS_CAPI_ENGINES=interp
cp "$TSRC"/*.bsv "$TSRC"/*.bs "$TSRC"/*.cmd . 2>/dev/null

fail=0
build() { # src top extra-link-flags...
    src=$1; top=$2; shift 2
    $BSC -sim -bir -u -g "$top" "$src" > "$top.bsc.log" 2>&1 \
        || { echo "FAIL $top (bsc compile)"; fail=1; return 1; }
    $BSC -sim -bir "$@" -e "$top" -o "ref_$top" >> "$top.bsc.log" 2>&1 \
        || { echo "FAIL $top (bsc link)"; fail=1; return 1; }
    "$TRS" link "$top.bir" --interactive -o "b3_$top" \
        > "$top.b3.log" 2>&1 \
        || { echo "FAIL $top (trs link --interactive)"; fail=1; return 1; }
}
check() { # top cmd [expected-status]
    top=$1; cmd=$2; want=${3:-0}
    timeout 120 ./"ref_$top" -f "$cmd" > "ref_${top}_${cmd%.cmd}.out" 2>&1
    ref_rc=$?
    timeout 120 ./"b3_$top" -f "$cmd" > "b3_${top}_${cmd%.cmd}.out" 2>&1
    b3_rc=$?
    [ "$ref_rc" = "$want" ] \
        || echo "WARN $top $cmd: reference exit $ref_rc (expected $want)"
    if [ "$ref_rc" != "$b3_rc" ]; then
        echo "FAIL $top $cmd (exit $ref_rc vs $b3_rc)"; fail=1; return
    fi
    if ! cmp -s "ref_${top}_${cmd%.cmd}.out" "b3_${top}_${cmd%.cmd}.out"; then
        echo "FAIL $top $cmd (output)"
        diff "ref_${top}_${cmd%.cmd}.out" "b3_${top}_${cmd%.cmd}.out" | head -5
        fail=1; return
    fi
    echo "PASS $top $cmd"
}

if build tiny.bsv mkTest; then
    check mkTest step.cmd
    check mkTest step2.cmd
    check mkTest sync.cmd
    check mkTest exit.cmd 7
    check mkTest help.cmd
    check mkTest error.cmd 1
fi
if build MCDTest.bsv mkMCDTest; then
    check mkMCDTest clock.cmd
fi
if build APeriodicTest.bsv sysAPeriodicTest; then
    check sysAPeriodicTest aperiodic.cmd
    check sysAPeriodicTest debug4.cmd
fi
if build Long.bsv mkLong; then
    export TRS_CAPI_ENGINES=jit
    check mkLong async.cmd
    export TRS_CAPI_ENGINES=interp
fi
if build hier.bsv mkTop -keep-fires; then
    check mkTop hier.cmd
    check mkTop glob.cmd
    check mkTop error2.cmd 1
fi
if build prims.bsv mkPrims; then
    check mkPrims prims.cmd
fi
if build TbGCD.bsv mkTbGCD -keep-fires; then
    check mkTbGCD gcd.cmd
    check mkTbGCD debug.cmd
    check mkTbGCD debug2.cmd
    check mkTbGCD debug3.cmd
    check mkTbGCD debug4.cmd
    check mkTbGCD debug5.cmd
fi
if build TimescaleTest.bs mkTimescaleTest; then
    check mkTimescaleTest timescale.cmd
    check mkTimescaleTest timescale2.cmd
fi
exit $fail
