#!/bin/sh
# Interactive (bluetcl `sim`) battery: for each design in the
# reference interactive testsuite, build the reference Bluesim model
# AND the trs capi model (`trs link --interactive`), run every
# .cmd script through BOTH via the bluesim.tcl wrapper, and diff the
# outputs byte-for-byte — the reference is the oracle, so this holds
# even where the checked-in .expected files drift.
#
# Matrix mirrored from testsuite/bsc.bluesim/interactive/
# interactive.exp — keep in sync (22 sim_output assertions), plus
# local witnesses (FinishPeek, bdpi, oracle, oracleaot,
# finishpeekaot, oracleprims, quietwarn, stopres x2,
# capi_witness, vcdtcl, fsttcl, oraclecnt: 35 total).
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
# local witnesses (not in the reference testsuite) live beside this
# script
cp "$SRC"/*.bsv "$SRC"/*.cmd "$SRC"/*.c . 2>/dev/null

fail=0
build() { # src top [flags and/or C link files]...
    src=$1; top=$2; shift 2
    # bsc grammar: flags before -e, source/link files after -o
    flags=""; cfiles=""
    for a in "$@"; do
        case "$a" in
            *.c|*.cxx|*.cpp|*.o) cfiles="$cfiles $a";;
            *) flags="$flags $a";;
        esac
    done
    $BSC -sim -bir -u -g "$top" "$src" > "$top.bsc.log" 2>&1 \
        || { echo "FAIL $top (bsc compile)"; fail=1; return 1; }
    $BSC -sim -bir $flags -e "$top" -o "ref_$top" $cfiles >> "$top.bsc.log" 2>&1 \
        || { echo "FAIL $top (bsc link)"; fail=1; return 1; }
    "$TRS" link "$top.bir" --interactive -o "trs_$top" \
        > "$top.trs.log" 2>&1 \
        || { echo "FAIL $top (trs link --interactive)"; fail=1; return 1; }
}
check() { # top cmd [expected-status]
    top=$1; cmd=$2; want=${3:-0}
    timeout 120 ./"ref_$top" -f "$cmd" > "ref_${top}_${cmd%.cmd}.out" 2>&1
    ref_rc=$?
    timeout 120 ./"trs_$top" -f "$cmd" > "trs_${top}_${cmd%.cmd}.out" 2>&1
    trs_rc=$?
    [ "$ref_rc" = "$want" ] \
        || echo "WARN $top $cmd: reference exit $ref_rc (expected $want)"
    if [ "$ref_rc" != "$trs_rc" ]; then
        echo "FAIL $top $cmd (exit $ref_rc vs $trs_rc)"; fail=1; return
    fi
    if ! cmp -s "ref_${top}_${cmd%.cmd}.out" "trs_${top}_${cmd%.cmd}.out"; then
        echo "FAIL $top $cmd (output)"
        diff "ref_${top}_${cmd%.cmd}.out" "trs_${top}_${cmd%.cmd}.out" | head -5
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
    # fifo state through the arena mirror, oracle-compared per stop
    export TRS_CAPI_ENGINES=interp,aot
    check mkPrims oracleprims.cmd
    export TRS_CAPI_ENGINES=interp
fi
if build TbGCD.bsv mkTbGCD -keep-fires -dump-formats vcd,fst; then
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
# $finish edge-completion witness (local): rules scheduled after the
# $finish rule on the finish edge still write state — peeked on the
# JIT engine (register peeks are arena-resident on that tier, so
# this witnesses the COMPILED path's post-finish writes; a mid-edge
# abort answers mark=0)
if build FinishPeek.bsv sysFinishPeek; then
    # TRS_JIT_SYNC pins warmth: without it the compiled-path
    # coverage is a wall-clock race (the assertion itself is
    # race-immune, but the witness must deterministically exercise
    # the COMPILED finish edge)
    export TRS_CAPI_ENGINES=jit TRS_JIT_SYNC=1
    check sysFinishPeek finishpeek.cmd
    unset TRS_JIT_SYNC
    export TRS_CAPI_ENGINES=interp
fi
# BDPI-under-Tcl (task #10 packaging): the companion .bdpi.so travels
# with the model (link copies it; bk_init dladdr-loads it).  jit
# engine: also a SHORT session, witnessing the compile-worker join at
# teardown (pre-fix: 5/5 segfault after correct output)
if build BdpiMin.bsv sysBdpiMin ops.c; then
    export TRS_CAPI_ENGINES=jit
    check sysBdpiMin bdpi.cmd
    export TRS_CAPI_ENGINES=interp
fi
# ORACLE mode (task #10): interp primary + QUIET jit secondary,
# lockstep-compared at every stop — the full gcd session must stay
# byte-identical to the single-engine reference
if [ -x ./ref_mkTbGCD ]; then
    export TRS_CAPI_ENGINES=interp,jit
    check mkTbGCD oracle.cmd
    # flagship debug config: interp + aot from the artifact pair
    export TRS_CAPI_ENGINES=interp,aot
    check mkTbGCD oracleaot.cmd
    export TRS_CAPI_ENGINES=interp
fi
# pure AOT engine: artifact-warm compiled bodies from t=0, register
# peeks from the arena, $finish edge completion on the aot tier
if [ -x ./ref_sysFinishPeek ]; then
    export TRS_CAPI_ENGINES=aot
    check sysFinishPeek finishpeekaot.cmd
    export TRS_CAPI_ENGINES=interp
fi
# quiet-engine diagnostics: prim-level guard warnings must appear
# exactly once (the fleet: quiet secondaries duplicated every
# "Enqueuing to a full fifo" line pre-fix)
if build QuietWarn.bsv sysQuietWarn; then
    export TRS_CAPI_ENGINES=interp,jit
    check sysQuietWarn quietwarn.cmd
    export TRS_CAPI_ENGINES=interp
fi
# $stop pauses, $finish terminates (review backlog): run to the
# $stop, peek, RESUME to the $finish, then stepping must refuse —
# byte-identical to the reference through the whole session
if build StopRes.bsv sysStopRes; then
    check sysStopRes stopres.cmd
    export TRS_CAPI_ENGINES=interp,jit
    check sysStopRes stopresoracle.cmd
    export TRS_CAPI_ENGINES=interp
fi
# Counter/CReg oracle state surface: symbol-less in the reference's
# bk tree (`sim ls` parity) but compared via state_children at
# every dual-engine stop
if build CRegCnt.bsv sysCRegCnt; then
    export TRS_CAPI_ENGINES=interp,jit
    check sysCRegCnt oraclecnt.cmd
    export TRS_CAPI_ENGINES=interp
fi
# trs_* namespace (task #10): direct C-API witness — dlopen the
# model without bluetcl, drive the bk_ lifecycle, exercise engine
# queries + the on-demand oracle checkpoint.  Assertions are ours
# (no reference to mirror): compare against a literal expectation
if [ -f ./trs_sysFinishPeek.so ] && cc -o capi_witness capi_witness.c -ldl 2>cc.log; then
    TRS_CAPI_ENGINES=interp,aot ./capi_witness ./trs_sysFinishPeek.so \
        sysFinishPeek > capi_witness.out 2>capi_witness.err
    rc=$?
    printf 'engines 2: interp aot\nkind-oob null\nfinishing at 1000000 mark 0\noracle 0\nfinished 1 status 0\nshutdown ok\n' \
        > capi_witness.want
    if [ "$rc" = 0 ] && cmp -s capi_witness.out capi_witness.want; then
        echo "PASS capi_witness (trs_* namespace)"
    else
        echo "FAIL capi_witness (rc=$rc)"
        diff capi_witness.want capi_witness.out | head -5
        fail=1
    fi
else
    echo "FAIL capi_witness (cc)"; fail=1
fi
# VCD-under-Tcl (task #10): `sim vcd <file>`/off/on -> the three
# bk_* VCD controls, served by the interp engine's writer.  stdout
# AND the VCD bytes must match (modulo the $date line), including
# the yield-boundary flush (bk_shutdown mirrors kernel vcd_reset)
vcdtcldiff() { # ref got
    sed 2d "$1" > .vr.$$ && sed 2d "$2" > .vg.$$
    diff .vr.$$ .vg.$$ > /dev/null; r=$?
    rm -f .vr.$$ .vg.$$; return $r
}
if [ -x ./ref_mkTbGCD ]; then
    rm -f waves.vcd
    timeout 120 ./ref_mkTbGCD -f vcdtcl.cmd > ref_mkTbGCD_vcdtcl.out 2>&1
    ref_rc=$?
    mv waves.vcd ref_waves.vcd 2>/dev/null
    rm -f waves.vcd
    timeout 120 ./trs_mkTbGCD -f vcdtcl.cmd > trs_mkTbGCD_vcdtcl.out 2>&1
    trs_rc=$?
    ok=1
    [ "$ref_rc" = "$trs_rc" ] || ok=0
    cmp -s ref_mkTbGCD_vcdtcl.out trs_mkTbGCD_vcdtcl.out || ok=0
    vcdtcldiff ref_waves.vcd waves.vcd || ok=0
    if [ "$ok" = 1 ]; then echo "PASS mkTbGCD vcdtcl.cmd"
    else echo "FAIL mkTbGCD vcdtcl.cmd"; fail=1; fi
fi
# FST-under-Tcl (fst rebase): `sim fst` through the shared engine;
# stdout AND the FST files must match semantically (fst2vcd +
# fstcmp.py: FST bytes embed timestamps, so no byte compare)
if [ -x ./ref_mkTbGCD ] && command -v fst2vcd > /dev/null 2>&1; then
    rm -f waves.fst
    timeout 120 ./ref_mkTbGCD -f fsttcl.cmd > ref_mkTbGCD_fsttcl.out 2>&1
    ref_rc=$?
    mv waves.fst ref_waves.fst 2>/dev/null
    rm -f waves.fst
    timeout 120 ./trs_mkTbGCD -f fsttcl.cmd > trs_mkTbGCD_fsttcl.out 2>&1
    trs_rc=$?
    ok=1
    [ "$ref_rc" = "$trs_rc" ] || ok=0
    cmp -s ref_mkTbGCD_fsttcl.out trs_mkTbGCD_fsttcl.out || ok=0
    fst2vcd ref_waves.fst > .rf.$$ 2>/dev/null || ok=0
    fst2vcd waves.fst > .bf.$$ 2>/dev/null || ok=0
    python3 "$SRC/../vcd/fstcmp.py" .rf.$$ .bf.$$ > /dev/null || ok=0
    rm -f .rf.$$ .bf.$$
    if [ "$ok" = 1 ]; then echo "PASS mkTbGCD fsttcl.cmd"
    else echo "FAIL mkTbGCD fsttcl.cmd"; fail=1; fi
fi
exit $fail
