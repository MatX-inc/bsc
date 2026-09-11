#!/bin/sh
# BVI R5 gate battery: semantic fixtures + corpus, differential against
# the Verilog flow (iverilog PRIMARY oracle) or the testsuite's stored
# goldens (RAMS, SimpleRealImport), plus the observe-mode witness and
# the forwarded-parameter refusal pin.
# BSC=/path/bsc TRS=/path/trs sh run-r5.sh [workdir]
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
REPO=$(cd "$SRC/../../../.." && pwd)
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

# same-BSV differential: iverilog oracle vs trs (stdout + exit code)
differ() { # name top [extra-files-dir]
    name=$1; top=$2; extras=$3
    d="$WK/$name"; rm -rf "$d"; mkdir -p "$d"; cd "$d" || exit 2
    if [ -n "$extras" ]; then cp "$extras"/* . 2>/dev/null
    else cp "$SRC/$name.bsv" .; cp "$SRC"/rtl/*.v .; fi
    bsv=$(ls *.bsv | head -1)
    [ -f "$name.bsv" ] && bsv=$name.bsv
    $BSC -verilog -u -g "$top" "$bsv" >v.out 2>&1 \
        && $BSC -verilog -vsim iverilog -e "$top" -o vref.exe >>v.out 2>&1 || {
        echo "FAIL $name (verilog oracle build)"; tail -3 v.out; fail=1; return; }
    # RUNARGS (e.g. "+doit +lvl=7") reach both the oracle exe and the
    # trs run, then reset -- the plusargs fixture uses this
    timeout 120 ./vref.exe $RUNARGS > vref.out 2>&1; vrc=$?
    $BSC -sim -u -g "$top" "$bsv" >b.out 2>&1 || {
        echo "FAIL $name (bsc compile)"; head -3 b.out; fail=1; return; }
    export_bir "$top" >link.out 2>&1 && build_models "$top" || {
        echo "FAIL $name (trs link)"; head -5 link.out; fail=1; return; }
    timeout 120 "$TRS" run "$top.bir" $RUNARGS > got.out 2>&1; grc=$?
    RUNARGS=
    grep -v '\$finish' vref.out > vref.flt
    grep -v '\$finish' got.out > got.flt
    if [ -n "$XFILTER" ]; then
        grep -v "$XFILTER" vref.flt > vref.flt2 && mv vref.flt2 vref.flt
        XFILTER=
    fi
    # GFILTER: drop matching lines from the TRS side -- the mirror of
    # XFILTER, for deterministic two-state output the 4-state oracle
    # X-suppresses at startup (sec 4.3)
    if [ -n "$GFILTER" ]; then
        grep -v "$GFILTER" got.flt > got.flt2 && mv got.flt2 got.flt
        GFILTER=
    fi
    if [ "$vrc" != "$grc" ]; then
        echo "FAIL $name (exit $vrc vs $grc)"; fail=1; return
    fi
    if ! cmp -s vref.flt got.flt; then
        echo "FAIL $name (stdout)"; diff vref.flt got.flt | head -6
        fail=1; return
    fi
    echo "PASS $name"
}

# corpus vs a stored golden (for designs whose live iverilog harness
# can't see the model .v, the testsuite golden is the oracle)
golden() { # name top srcdir bsv expected
    name=$1; top=$2; srcdir=$3; bsv=$4; expected=$5
    d="$WK/$name"; rm -rf "$d"; mkdir -p "$d"; cd "$d" || exit 2
    cp "$srcdir"/*.bsv "$srcdir"/*.v . 2>/dev/null
    cp "$srcdir"/*.data . 2>/dev/null
    $BSC -sim -u -g "$top" "$bsv" >b.out 2>&1 || {
        echo "FAIL $name (bsc compile)"; head -3 b.out; fail=1; return; }
    export_bir "$top" >link.out 2>&1 && build_models "$top" || {
        echo "FAIL $name (trs link)"; head -5 link.out; fail=1; return; }
    timeout 120 "$TRS" run "$top.bir" > got.out 2>&1
    grep -v '\$finish' "$srcdir/$expected" > want.flt
    grep -v '\$finish' got.out > got.flt
    if cmp -s want.flt got.flt; then
        echo "PASS $name"
    else
        echo "FAIL $name (vs stored golden)"; diff want.flt got.flt | head -6
        fail=1
    fi
}

differ PosClocks sysPosClocks
# PosGate carries the documented sec 4.3 startup divergence: the
# 4-state oracle fires one PRE-RESET display with x-valued control
# (gated-clock + async-reset warmup), which two-state trs never does.
# Pin: drop the oracle's x-valued startup lines, byte-compare the rest.
XFILTER='=x$' differ PosGate sysPosGate
differ PosRst sysPosRst
differ PosParams sysPosParams
differ PosMix sysPosMix
differ PosTwins sysPosTwins
differ PosTwoRst sysPosTwoRst
differ PosPortlessRst sysPosPortlessRst
differ PosCombPortlessClk sysPosCombPortlessClk
differ PosTime sysPosTime
RUNARGS="+doit +lvl=7" differ PosPlus sysPosPlus
differ PosWrap sysPosWrap
# PosDelay: real intra-cycle delays (#3/#12/#13 NBAs) -> the --timing
# build mode; delayed events fire between edges via vlt_advance
differ PosDelay sysPosDelay
# output resets (v1.2a): a registered stretcher generates a reset that
# a downstream register is reset_by (asserts AND deasserts mid-run)
differ PosRstOut sysPosRstOut
# the census design: library mkResetInverter (combinational output
# reset) + native MakeResetA through one network.  Sec 4.3 startup pin:
# the 4-state oracle X-suppresses the first CounterB display (its guard
# reads !X at the first task instant); two-state trs prints it -- drop
# that one deterministic line from the trs side.
GFILTER='is -1431655766$' differ ResetInv sysResetInv \
        "$REPO/testsuite/bsc.mcd/ClockDividers"
# output clocks (v1.2b): a divide-by-2 clock generated inside the
# import; a downstream register and rule live in the derived domain,
# so edges must fire between the kernel's scheduled slices.  Sec 4.3
# startup pin (mirror of ResetInv): the 4-state oracle's first
# derived-domain task instant absorbs one fire; drop our first
# uninit-value display.
GFILTER='^slow=170$' differ PosClkOut sysPosClkOut
# census pair: output clocks queried only for elaboration-time family
# facts (isAncestor/sameFamily) -- constant-0 model clocks, never tick
differ TransitiveAncestor sysTransitiveAncestor "$REPO/testsuite/bsc.mcd/Misc"
differ TransitiveFamily sysTransitiveFamily "$REPO/testsuite/bsc.mcd/Misc"
differ ParamOrder sysParamOrder "$REPO/testsuite/bsc.verilog/v95"

golden Rams mkTop "$REPO/testsuite/bsc.bsv_examples/RAMS" Test.bsv \
       mkTop.out.expected
golden SimpleReal sysSimpleRealImport \
       "$REPO/testsuite/bsc.verilog/parameters/real" SimpleRealImport.bsv \
       sysSimpleRealImport.out.expected

# forwarded parameters (v1.1 lift): a real parameter crossing a
# synthesis boundary resolves at instantiation and verilates per
# valuation -- byte-compared against the stored golden
golden TwoLevelReal sysTwoLevelReal \
       "$REPO/testsuite/bsc.verilog/parameters/real" TwoLevelReal.bsv \
       sysTwoLevelReal.out.expected

# the lying import: a clean run diverges SILENTLY (that is the threat
# model); TRS_BVI_CHECK=observe produces a sound DYNAMIC_LIE witness
d="$WK/NegLie"; rm -rf "$d"; mkdir -p "$d"; cd "$d"
cp "$SRC/NegLie.bsv" .; cp "$SRC"/rtl/*.v .
$BSC -sim -u -g sysNegLie NegLie.bsv >b.out 2>&1 \
    && export_bir sysNegLie >link.out 2>&1 && build_models sysNegLie || {
    echo "FAIL NegLie (build)"; tail -3 link.out; fail=1; }
if [ -f sysNegLie.bir ]; then
    TRS_BVI_CHECK=observe timeout 120 "$TRS" run sysNegLie.bir \
        >lie.out 2>lie.err
    if grep -q "DYNAMIC_LIE" lie.err && grep -q "PEEK" lie.err; then
        echo "PASS NegLie-witness"
    else
        echo "FAIL NegLie-witness (no attributed witness)"
        head -5 lie.err; fail=1
    fi
fi

# per-fragment compilation across a BVI import: the manifest names the
# model, the fragment builds ALONE byte-identically, and the design
# assembled from that object runs.  Each half of this has silently
# broken once in a neighbouring form -- a shared body baking one
# instance's parameters, and an identity that was really a position --
# so all three are pinned rather than argued.
d="$WK/PosSpecialize"; rm -rf "$d"; mkdir -p "$d"; cd "$d" || exit 2
cp "$SRC/PosSpecialize.bsv" .; cp "$SRC"/rtl/BviCounter.v .
spec_fail() { echo "FAIL PosSpecialize ($1)"; shift; [ $# -gt 0 ] && tail -n 5 "$@"; fail=1; }
TRS_VLT_CACHE="$d/vlt"; export TRS_VLT_CACHE
if ! $BSC -sim -u -g sysPosSpecialize -g mkWrap PosSpecialize.bsv >b.out 2>&1; then
    spec_fail build b.out
elif ! export_bir sysPosSpecialize >e.out 2>&1; then
    spec_fail export e.out
elif ! $TRS link sysPosSpecialize.bir -o sys.exe >l.out 2>&1; then
    spec_fail link l.out
elif ! $TRS specializations sys.exe.bir -o m.json >s.out 2>&1; then
    spec_fail manifest s.out
else
    # the model edge, and the run key it is named by
    key=$(sed -n 's/.*"run_key": "\([0-9a-f]*\)".*/\1/p' m.json | head -1)
    if [ -z "$key" ]; then
        spec_fail "manifest names no model" m.json
    elif [ ! -f "$TRS_VLT_CACHE/vlt/byid/$key" ]; then
        # the run key IS the byid file name: that is what makes a model
        # a build node with a real output path
        spec_fail "run key $key is not a byid file"
    else
        echo "PASS PosSpecialize-manifest"
    fi
    obj=$(sed -n 's/.*"object": "\([^"]*\)".*/\1/p' m.json | head -1)
    binds=$(sed -n 's/.*"params": {"k": "\([^"]*\)".*/\1/p' m.json | head -1)
    mkdir -p indesign
    $TRS compile sys.exe.bir --spec-obj-out indesign -o sys.so >c.out 2>&1
    # alone: its own tree, its own model cache, nothing of the design
    mkdir -p alone/out; cp mkWrap.bir BviCounter.v alone/
    ( cd alone && TRS_VLT_CACHE="$d/alone/vlt" $TRS link mkWrap.bir "+k=$binds" \
        -o w.exe >la.out 2>&1 \
      && TRS_VLT_CACHE="$d/alone/vlt" $TRS compile w.exe.bir --spec-obj-out out \
        >ca.out 2>&1 )
    if [ ! -f "indesign/$obj" ] || [ ! -f "alone/out/$obj" ]; then
        spec_fail "no object $obj (in-design or alone)" c.out alone/ca.out
    elif ! cmp -s "indesign/$obj" "alone/out/$obj"; then
        spec_fail "the fragment built alone differs from the design's $obj"
    else
        echo "PASS PosSpecialize-identical"
    fi
    # and the design assembled from the standalone object still runs
    if [ -f "alone/out/$obj" ]; then
        $TRS compile sys.exe.bir --spec-obj-in alone/out -o sys2.so >c2.out 2>&1
        if ! grep -q "1 of 2 specializations reused" c2.out; then
            spec_fail "the design did not reuse the standalone object" c2.out
        else
            cp sys2.so sys.exe.so
            timeout 120 $TRS run sys.exe.bir >r.out 2>&1
            if grep -q "a=9 b=21" r.out; then
                echo "PASS PosSpecialize-run"
            else
                spec_fail "assembled design ran wrong" r.out
            fi
        fi
    fi
fi

# two reset ports in one fragment: the reset table's ORDER.  Assigned
# from HashMap iteration it differed per map, so instances of one type
# split at random and an EMITTING process numbered the table
# differently from the LOADING one -- a wrong answer, invisible at the
# corpus mean of 1.02 reset ports.  Both halves gated: the signature
# is stable across runs, and the fragment still builds alone
# byte-identically.
d="$WK/PosSpecTwoRst"; rm -rf "$d"; mkdir -p "$d"; cd "$d" || exit 2
cp "$SRC/PosSpecTwoRst.bsv" .; cp "$SRC"/rtl/RstStretch.v .
TRS_VLT_CACHE="$d/vlt"; export TRS_VLT_CACHE
if ! $BSC -sim -u -g sysPosSpecTwoRst -g mkRstWrap PosSpecTwoRst.bsv >b.out 2>&1; then
    echo "FAIL PosSpecTwoRst (build)"; tail -n 5 b.out; fail=1
elif ! export_bir sysPosSpecTwoRst >e.out 2>&1 \
        || ! $TRS link sysPosSpecTwoRst.bir -o sys.exe >l.out 2>&1; then
    echo "FAIL PosSpecTwoRst (export/link)"; tail -n 5 e.out l.out; fail=1
else
    # five plans of one design must agree: the ordinals are a
    # per-instance table index, and nothing else here varies
    n=0
    while [ $n -lt 5 ]; do
        $TRS specializations sys.exe.bir --format text 2>/dev/null \
            | grep mkRstWrap > "sig$n.txt"
        n=$((n + 1))
    done
    if ! cmp -s sig0.txt sig1.txt || ! cmp -s sig0.txt sig2.txt \
            || ! cmp -s sig0.txt sig3.txt || ! cmp -s sig0.txt sig4.txt; then
        echo "FAIL PosSpecTwoRst-stable (the signature varies between runs)"
        sort -u sig0.txt sig1.txt sig2.txt sig3.txt sig4.txt; fail=1
    else
        echo "PASS PosSpecTwoRst-stable"
    fi
    $TRS specializations sys.exe.bir -o m.json >/dev/null 2>&1
    obj=$(sed -n 's/.*"object": "\([^"]*\)".*/\1/p' m.json | head -1)
    binds=$(sed -n 's/.*"params": {"k": "\([^"]*\)".*/\1/p' m.json | head -1)
    mkdir -p indesign alone/out
    $TRS compile sys.exe.bir --spec-obj-out indesign -o sys.so >c.out 2>&1
    cp mkRstWrap.bir RstStretch.v alone/
    ( cd alone && TRS_VLT_CACHE="$d/alone/vlt" $TRS link mkRstWrap.bir "+k=$binds" \
        -o w.exe >la.out 2>&1 \
      && TRS_VLT_CACHE="$d/alone/vlt" $TRS compile w.exe.bir --spec-obj-out out \
        >ca.out 2>&1 )
    if [ ! -f "indesign/$obj" ] || [ ! -f "alone/out/$obj" ]; then
        echo "FAIL PosSpecTwoRst-identical (no object $obj)"
        tail -n 5 c.out alone/ca.out; fail=1
    elif ! cmp -s "indesign/$obj" "alone/out/$obj"; then
        echo "FAIL PosSpecTwoRst-identical ($obj differs built alone)"; fail=1
    else
        echo "PASS PosSpecTwoRst-identical"
    fi
fi

exit $fail
