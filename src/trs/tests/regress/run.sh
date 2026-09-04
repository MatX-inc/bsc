#!/bin/sh
# Edge-SSA regression battery: compile each design with the installed
# bsc, run the reference Bluesim executable and the trs AOT artifact
# (bare defaults = the specialized fast compile), and diff stdout +
# exit codes.  BSC=/path/bsc TRS=/path/trs sh run.sh [workdir]
BSC=${BSC:-bsc}
TRS=${TRS:-trs}
TRSBIR=${TRSBIR:-trs-bir}
SRC=$(cd "$(dirname "$0")" && pwd)
. "$SRC/../../tools/fragments.sh"
case "$BSC" in
    */*) PATH="$(cd "$(dirname "$BSC")" && pwd):$PATH"; export PATH;;
esac
WK=${1:-$(mktemp -d)}
cd "$WK" || exit 2
fail=0

# The reference executable and the trs inputs come from two programs:
# bsc links the Bluesim executable, trs-bir writes the .bir and, for a
# design with BDPI, the companion object the runtime loads beside it.
ref_link() { # top outexe [cfile]
    rl_top=$1; rl_exe=$2; rl_c=$3
    $BSC -sim -e "$rl_top" -o "$rl_exe" $rl_c || return 1
    # every synthesized module of the design, not just the top: bsc
    # writes one .ba each and trs-bir one .bir each, and the link
    # follows the instantiations from the top
    frags_sub "$rl_top"
    $TRSBIR "$rl_top.ba"
}

# BDPI implementations go to the LINK, as they do for bsc: the export
# knows nothing about them.  Callers put $BDPI on every trs link line.
BDPI=""
set_bdpi() { BDPI=""; [ -z "$1" ] || BDPI="--bdpi $1"; }
check() { # name top [cfile]
    name=$1; top=$2; cfile=$3
    cp "$SRC/$name.bsv" .
    [ -n "$cfile" ] && cp "$SRC/$cfile" .
    $BSC -sim -u -g "$top" "$name.bsv" >/dev/null 2>&1 || { echo "FAIL $name (bsc)"; fail=1; return; }
    set_bdpi "$cfile"
    ref_link "$top" ref.exe $cfile >/dev/null 2>&1 || { echo "FAIL $name (ref link)"; fail=1; return; }
    ./ref.exe > ref.out 2>&1; refrc=$?
    "$TRS" link $BDPI "$top.bir" -o art >link.out 2>&1 || { echo "FAIL $name (trs link)"; fail=1; return; }
    # byte parity cannot distinguish engines (that is the oracle
    # contract), so the compiled contract is asserted explicitly: a
    # fallback-to-interp artifact fails the battery
    if grep -q "run interpreted" link.out; then
        echo "FAIL $name (not compiled: $(head -1 link.out))"; fail=1; return
    fi
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
# BRAM byte enables past lane 63 (128 lanes on 1024-bit data), plus
# out-of-bounds puts on both the write and the read side.  The
# reference's generated C++ did not COMPILE at these widths before
# the bs_prim_mod_bram.h is_zero fix (WideData has no operator!=
# against int), so this used to be an expected-file test; now a live
# byte-compare whose out-of-bounds arms exercise the fixed Write/Read
# warning discriminator on both engines.
check BramWideBE sysBramWideBE
# guarded-FIFO warn arms: enq-to-full / deq-from-empty warn and drop
# on both engines; under TRS_RUNCORE=1 they exercise the boot's
# natively restored Fifo servicer (rung 3b)
check FifoWarn sysFifoWarn
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
    $BSC -sim -u -g "$top" "$name.bsv" >/dev/null 2>&1 || { echo "FAIL $name (bsc)"; fail=1; return; }
    ref_link "$top" ref.exe > reflink.out 2>&1 || { echo "FAIL $name (ref link)"; fail=1; return; }
    "$TRS" link $BDPI "$top.bir" -o art > link.out 2>&1 || { echo "FAIL $name (trs link)"; fail=1; return; }
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
    $BSC -sim -u -g "$top" "$name.bsv" >/dev/null 2>&1 || { echo "FAIL $name (bsc)"; fail=1; return; }
    set_bdpi "$cfile"
    ref_link "$top" ref.exe $cfile >/dev/null 2>&1 || { echo "FAIL $name (ref link)"; fail=1; return; }
    ./ref.exe > ref.out 2>&1; refrc=$?
    "$TRS" link $BDPI "$top.bir" -o art >/dev/null 2>&1 || { echo "FAIL $name (trs link)"; fail=1; return; }
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
# dual-port BE BRAM, same-instant same-address writes: collided-write
# out takes disabled lanes from prev, memory resolves last-writer-wins
# in clkA-then-clkB tick order (SimExportIR), read-during-write bypass
check DualBE sysDualBE
# the dual-write collision warning: fires on EQUAL overlapping chunks
# (the reference's chunks_eq quirk), two lines per collision instant,
# byte-positioned between the cycles' $display output
check CollideEq sysCollideEq
# design-armed $dumpvars on a compiled TRACED artifact: the dump must
# byte-match the reference's ($date stripped) — this corner broke
# silently twice (central loop never yielded to the wave engine: empty
# files; inline FIFO enq bypassed the boxed D_IN bookkeeping)
check_vcd() { # name top
    name=$1; top=$2
    set_bdpi ""
    cp "$SRC/$name.bsv" .
    $BSC -sim -u -g "$top" "$name.bsv" >/dev/null 2>&1 || { echo "FAIL $name (bsc)"; fail=1; return; }
    ref_link "$top" ref.exe >/dev/null 2>&1 || { echo "FAIL $name (ref link)"; fail=1; return; }
    rm -f dump.vcd
    ./ref.exe > ref.out 2>&1; refrc=$?
    sed '/^\$date/,/^\$end/d' dump.vcd > ref.vcd 2>/dev/null
    "$TRS" link $BDPI "$top.bir" -o art >/dev/null 2>&1 || { echo "FAIL $name (trs link)"; fail=1; return; }
    rm -f dump.vcd
    TRS="$TRS" ./art > got.out 2>&1; gotrc=$?
    sed '/^\$date/,/^\$end/d' dump.vcd > got.vcd 2>/dev/null
    if [ "$refrc" != "$gotrc" ]; then echo "FAIL $name (exit $refrc vs $gotrc)"; fail=1; return; fi
    if ! cmp -s ref.out got.out; then echo "FAIL $name (stdout)"; diff ref.out got.out | head -3; fail=1; return; fi
    if ! cmp -s ref.vcd got.vcd; then echo "FAIL $name (vcd)"; diff ref.vcd got.vcd | head -3; fail=1; return; fi
    echo "PASS $name"
}
check_vcd FifoVcd sysFifoVcd
# wide (>64-bit) module arguments in compiled bodies: multi-limb
# port_consts (a single-u64 store once folded them to 0/1 and the run
# went silently empty)
check WideArgConst sysWideArgConst
# ---- top-level restriction lifts (trs only; no reference Bluesim
# executable exists for these BY DESIGN — classic Bluesim refuses the
# design class, so stdout gates against stored hand-derived goldens
# and the classic refusal tags are pinned) ----
# Top-level module arguments/parameters: classic link keeps
# EBSimTopLevelArgOrParam (G0099); trs binds +NAME=value at link/run.
# The parameter is WIDE (96 bits) — multi-limb port_consts folding is
# the point — and the design must run COMPILED through both the
# per-run path and the baked artifact.  Missing/unknown/oversized
# bindings each produce their specific loud error.
check_topparam() {
    name=TopParam; top=sysTopParam
    bigv=0x0123456789ABCDEF0FEDCBA9
    cp "$SRC/$name.bsv" .
    $BSC -sim -u -g "$top" "$name.bsv" >/dev/null 2>&1 || { echo "FAIL $name (bsc)"; fail=1; return; }
    if $BSC -sim -e "$top" -o tp_ref.exe >tp_err1.out 2>&1; then
        echo "FAIL $name (classic Bluesim link unexpectedly succeeded)"; fail=1; return
    fi
    grep -q "(G0099)" tp_err1.out || { echo "FAIL $name (expected G0099)"; fail=1; return; }
    # the .bir every check below consumes
    { frags_sub "$top"; $TRSBIR "$top.ba"; } >tp_bir.out 2>&1 || { echo "FAIL $name (trs-bir)"; fail=1; return; }
    # a link with no bindings must fail with the loud missing-binding error
    if TRS="$TRS" "$TRS" link $BDPI "$top.bir" -o tp.exe >tp_err2.out 2>&1; then
        echo "FAIL $name (link without bindings unexpectedly succeeded)"; fail=1; return
    fi
    grep -q "requires bindings for" tp_err2.out || { echo "FAIL $name (expected missing-binding error)"; fail=1; return; }
    [ -f "$top.bir" ] || { echo "FAIL $name (no .bir exported)"; fail=1; return; }
    "$TRS" link $BDPI "$top.bir" +big=1 +inc=1 +typo=9 -o tpbad >tp_err3.out 2>&1 && { echo "FAIL $name (unknown binding accepted)"; fail=1; return; }
    grep -q "unknown top-level binding" tp_err3.out || { echo "FAIL $name (expected unknown-binding error)"; fail=1; return; }
    "$TRS" run "$top.bir" +big=1 +inc=999 >tp_err4.out 2>&1 && { echo "FAIL $name (oversized binding accepted)"; fail=1; return; }
    grep -q "does not fit in the declared width" tp_err4.out || { echo "FAIL $name (expected oversized-binding error)"; fail=1; return; }
    "$TRS" run "$top.bir" +big=$bigv +inc=3 > got.out 2>&1; gotrc=$?
    if [ "$gotrc" != 0 ] || ! cmp -s "$SRC/$name.expected" got.out; then
        echo "FAIL $name (run stdout, rc=$gotrc)"; diff "$SRC/$name.expected" got.out | head -3; fail=1; return
    fi
    "$TRS" link $BDPI "$top.bir" +big=$bigv +inc=3 -o tpart >tplink.out 2>&1 || { echo "FAIL $name (trs link)"; fail=1; return; }
    if grep -q "run interpreted" tplink.out; then
        echo "FAIL $name (not compiled: $(head -1 tplink.out))"; fail=1; return
    fi
    TRS="$TRS" ./tpart > gota.out 2>&1; gotrc=$?
    if [ "$gotrc" != 0 ] || ! cmp -s "$SRC/$name.expected" gota.out; then
        echo "FAIL $name (artifact stdout, rc=$gotrc)"; diff "$SRC/$name.expected" gota.out | head -3; fail=1; return
    fi
    echo "PASS $name"
}
check_topparam
# always_enabled methods on the top interface: classic link keeps
# EBSimEnablePragma (G0062); trs batch mode auto-fires them per cycle
# at their schedule position (tick's state mutation is read by the
# rule BEFORE the methods' Exec cut, so position is observable in the
# values), with setStep's argument constant-bound.  The engine
# contract is COMPILED (pseudo exec sections at the cut anchors);
# `trs run` on the .bir still exercises the interp path above.
check_topae() {
    name=TopAlwaysEn; top=sysTopAlwaysEn
    cp "$SRC/$name.bsv" .
    $BSC -sim -u -g "$top" "$name.bsv" >/dev/null 2>&1 || { echo "FAIL $name (bsc)"; fail=1; return; }
    if $BSC -sim -e "$top" -o ae_ref.exe >ae_err1.out 2>&1; then
        echo "FAIL $name (classic Bluesim link unexpectedly succeeded)"; fail=1; return
    fi
    grep -q "(G0062)" ae_err1.out || { echo "FAIL $name (expected G0062)"; fail=1; return; }
    { frags_sub "$top"; $TRSBIR "$top.ba"; } >ae_bir.out 2>&1 || { echo "FAIL $name (trs-bir)"; fail=1; return; }
    if TRS="$TRS" "$TRS" link $BDPI "$top.bir" -o ae.exe >ae_err2.out 2>&1; then
        echo "FAIL $name (link without bindings unexpectedly succeeded)"; fail=1; return
    fi
    grep -q "requires bindings for" ae_err2.out || { echo "FAIL $name (expected missing-binding error)"; fail=1; return; }
    "$TRS" run "$top.bir" +setStep.v=2 > got.out 2>&1; gotrc=$?
    if [ "$gotrc" != 0 ] || ! cmp -s "$SRC/$name.expected" got.out; then
        echo "FAIL $name (run stdout, rc=$gotrc)"; diff "$SRC/$name.expected" got.out | head -3; fail=1; return
    fi
    # compiled auto-fire: the artifact's edge fns carry the method
    # bodies at their cut anchors, so the link must COMPILE —
    # TRS_REQUIRE_AOT trips (rc 86) if the engine silently falls back
    TRS_REQUIRE_AOT=1 "$TRS" link $BDPI "$top.bir" +setStep.v=2 -o aeart >aelink.out 2>&1 || { echo "FAIL $name (trs link, compiled)"; fail=1; return; }
    TRS="$TRS" ./aeart > gota.out 2>&1; gotrc=$?
    if [ "$gotrc" != 0 ] || ! cmp -s "$SRC/$name.expected" gota.out; then
        echo "FAIL $name (artifact stdout, rc=$gotrc)"; diff "$SRC/$name.expected" gota.out | head -3; fail=1; return
    fi
    echo "PASS $name"
}
check_topae
# NEGATIVE: bindable arguments plus an additional input clock — a
# binding supplies a constant, never a waveform, so the trs link
# refuses loudly (and classic keeps G0099 via the Bit# argument).
#
# The EXPORT takes it: it writes one synthesis boundary, and whether a
# module can be a running design's top is a question about the design.
# The link is what asks.
check_topclk() {
    name=TopClkArg; top=sysTopClkArg
    set_bdpi ""
    cp "$SRC/$name.bsv" .
    $BSC -sim -u -g "$top" "$name.bsv" >/dev/null 2>&1 || { echo "FAIL $name (bsc)"; fail=1; return; }
    if $BSC -sim -e "$top" -o ck_ref.exe >ck_err1.out 2>&1; then
        echo "FAIL $name (classic Bluesim link unexpectedly succeeded)"; fail=1; return
    fi
    grep -q "(G0099)" ck_err1.out || { echo "FAIL $name (expected G0099)"; fail=1; return; }
    frags_sub "$top"
    $TRSBIR "$top.ba" >ck_err2.out 2>&1 || { echo "FAIL $name (trs-bir)"; fail=1; return; }
    if "$TRS" link "$top.bir" -o ck.exe >ck_err3.out 2>&1; then
        echo "FAIL $name (link unexpectedly succeeded)"; fail=1; return
    fi
    grep -q "additional input clocks are not supported" ck_err3.out || { echo "FAIL $name (expected input-clock refusal)"; head -2 ck_err3.out; fail=1; return; }
    echo "PASS $name"
}
check_topclk
# dynamic scheduling (bsc G0096/G0100/G0101/G0116 family): no
# reference Bluesim exe exists by design — the classic C++ backend
# refuses these designs — so stdout diffs against a stored golden
# whose values are hand-derived.  Also gates the two refusals: plain
# -sim errors with the class's tag, and the Bluesim link of a
# errors at link.
check_dyn() { # name top errtag
    name=$1; top=$2; tag=$3
    set_bdpi ""
    cp "$SRC/$name.bsv" .
    if $BSC -sim -u -g "$top" "$name.bsv" >dyn_err1.out 2>&1; then
        echo "FAIL $name (static compile unexpectedly succeeded)"; fail=1; return
    fi
    grep -q "$tag" dyn_err1.out || { echo "FAIL $name (expected $tag)"; fail=1; return; }
    $BSC -sim -sched-dynamic -u -g "$top" "$name.bsv" >/dev/null 2>&1 || { echo "FAIL $name (bsc -sched-dynamic)"; fail=1; return; }
    if $BSC -sim -sched-dynamic -e "$top" -o dyn_ref.exe >dyn_err2.out 2>&1; then
        echo "FAIL $name (classic Bluesim link unexpectedly succeeded)"; fail=1; return
    fi
    grep -q "dynamic scheduling" dyn_err2.out || { echo "FAIL $name (expected dynamic-scheduling refusal)"; fail=1; return; }
    { frags_sub "$top"; $TRSBIR "$top.ba"; } >dyn_bir.out 2>&1 || { echo "FAIL $name (trs-bir)"; fail=1; return; }
    "$TRS" link $BDPI "$top.bir" -o dyn.exe >/dev/null 2>&1 || { echo "FAIL $name (trs link)"; fail=1; return; }
    "$TRS" run "$top.bir" > got.out 2>&1 || { echo "FAIL $name (trs run)"; fail=1; return; }
    if ! cmp -s "$SRC/$name.expected" got.out; then echo "FAIL $name (run stdout)"; diff "$SRC/$name.expected" got.out | head -3; fail=1; return; fi
    # compiled alts: the artifact's edge fns carry the per-edge guard
    # dispatch, so the link must COMPILE — TRS_REQUIRE_AOT trips (rc 86)
    # if the engine silently falls back to interp again
    TRS_REQUIRE_AOT=1 "$TRS" link $BDPI "$top.bir" -o dynart >dynlink.out 2>&1 || { echo "FAIL $name (trs link, compiled)"; fail=1; return; }
    TRS="$TRS" ./dynart > gota.out 2>&1 || { echo "FAIL $name (art run)"; fail=1; return; }
    if ! cmp -s "$SRC/$name.expected" gota.out; then echo "FAIL $name (art stdout)"; diff "$SRC/$name.expected" gota.out | head -3; fail=1; return; fi
    echo "PASS $name"
}
# coincident MakeClock domains + CrossingReg crossed-read semantics:
# destination-domain logic backdates to pre-edge, the after-edge combo
# pass reads post-edge (gate detectors break as steady-0 otherwise)
check MakeClkCross sysMakeClkCross
check DeepTiles sysDeepTiles
# a BDPI import the COMPILER ships (Randomizable's rand32/srand): the
# fragment names it and carries no signature, so the link must find
# rand32.bir among the compiler's own files rather than beside the
# design.  Nothing in the design's directory supplies it.
check LibBdpi sysLibBdpi
check_bdpi_missing() { # name top — task #58: an EXECUTED BDPI import
    # with no partner .c/.so must die LOUDLY naming the import on both
    # trs tiers (the old compiled path called through a NULL global =
    # segfault).  Bluesim cannot even link this shape (undefined C
    # symbol), so there is no reference leg.
    name=$1; top=$2
    set_bdpi ""
    cp "$SRC/$name.bsv" .
    $BSC -sim -u -g "$top" "$name.bsv" >/dev/null 2>&1 || { echo "FAIL $name (bsc)"; fail=1; return; }
    # no reference leg: the import has no implementation, so a Bluesim
    # link would fail on the undefined symbol.  The export does not care
    # -- only the .bir is needed here
    frags_sub "$top"; $TRSBIR "$top.ba" >/dev/null 2>&1
    [ -f "$top.bir" ] || { echo "FAIL $name (no .bir)"; fail=1; return; }
    # the trap may fire during link (the window bake / reset protocol
    # executes early cycles — the field repro died exactly there) or,
    # if link-time execution never reaches the call, at run time.
    # Either way: loud, named, never a segfault.
    "$TRS" link $BDPI "$top.bir" -o bdm >bdm-link.out 2>&1; lrc=$?
    if [ "$lrc" -eq 139 ]; then echo "FAIL $name (segfault at link)"; fail=1; return; fi
    if [ "$lrc" -ne 0 ]; then
        grep -q "BDPI import 'bdpi_mystery'" bdm-link.out || { echo "FAIL $name (link failed without naming the import)"; head -2 bdm-link.out; fail=1; return; }
    else
        TRS="$TRS" ./bdm > bdm.out 2>&1; rc=$?
        if [ "$rc" -eq 139 ]; then echo "FAIL $name (segfault)"; fail=1; return; fi
        if [ "$rc" -eq 0 ]; then echo "FAIL $name (ran clean without the import)"; fail=1; return; fi
        grep -q "BDPI import 'bdpi_mystery'" bdm.out || { echo "FAIL $name (aot trap message missing)"; head -2 bdm.out; fail=1; return; }
    fi
    TRS_NO_JIT=1 "$TRS" run "$top.bir" > bdmi.out 2>&1; irc=$?
    if [ "$irc" -eq 139 ] || [ "$irc" -eq 0 ]; then echo "FAIL $name (interp rc $irc)"; fail=1; return; fi
    grep -qi "bdpi" bdmi.out || { echo "FAIL $name (interp error message missing)"; head -2 bdmi.out; fail=1; return; }
    echo "PASS $name"
}
check_bdpi_missing BdpiMissing sysBdpiMissing
check BdpiDead sysBdpiDead
# repeated BDPI import under CHUNKED AOT (TRS_AOT_ONE_MODULE=0): call
# sites land in several emitted modules; the per-module diagnostic
# string trs_bdpiname_<name> must carry private linkage or the
# cc -shared link dies on duplicate strong definitions (the one-module
# battery cannot see this topology).  Byte parity proves the chunked
# artifact runs compiled.
check_chunked() { # name top cfile
    name=$1; top=$2; cfile=$3
    set_bdpi "$cfile"
    cp "$SRC/$name.bsv" .
    [ -n "$cfile" ] && cp "$SRC/$cfile" .
    $BSC -sim -u -g "$top" "$name.bsv" >/dev/null 2>&1 || { echo "FAIL $name (bsc)"; fail=1; return; }
    ref_link "$top" bc_ref.exe $cfile >/dev/null 2>&1 || { echo "FAIL $name (ref link)"; fail=1; return; }
    ./bc_ref.exe > bc_ref.out 2>&1; refrc=$?
    rm -f bcart.so
    TRS_AOT_ONE_MODULE=0 TRS_JIT_THREADS=4 TRS_REQUIRE_AOT=1 "$TRS" link $BDPI "$top.bir" -o bcart >bc_link.out 2>&1 || { echo "FAIL $name (chunked link)"; tail -2 bc_link.out; fail=1; return; }
    [ -f bcart.so ] || { echo "FAIL $name (no chunked artifact)"; fail=1; return; }
    TRS="$TRS" ./bcart > bc_got.out 2>&1; gotrc=$?
    if [ "$refrc" != "$gotrc" ] || ! cmp -s bc_ref.out bc_got.out; then echo "FAIL $name (parity)"; diff bc_ref.out bc_got.out | head -3; fail=1; return; fi
    echo "PASS $name"
}
check_chunked BdpiChunk sysBdpiChunk ops.c
# the missing-.so trap must survive chunking: calls gated past the
# link's window bake so the chunked link completes, then the run dies
# loudly naming the import — never a segfault
check_chunked_missing() {
    name=BdpiChunkMissing; top=sysBdpiChunkMissing
    set_bdpi ""
    cp "$SRC/$name.bsv" .
    $BSC -sim -u -g "$top" "$name.bsv" >/dev/null 2>&1 || { echo "FAIL $name (bsc)"; fail=1; return; }
    frags_sub "$top"; $TRSBIR "$top.ba" >/dev/null 2>&1
    [ -f "$top.bir" ] || { echo "FAIL $name (no .bir)"; fail=1; return; }
    TRS_AOT_ONE_MODULE=0 TRS_JIT_THREADS=4 "$TRS" link $BDPI "$top.bir" -o bcm >bcm_link.out 2>&1; lrc=$?
    if [ "$lrc" -eq 139 ]; then echo "FAIL $name (segfault at link)"; fail=1; return; fi
    if [ "$lrc" -ne 0 ]; then
        grep -q "BDPI import 'bdpi_mystery'" bcm_link.out || { echo "FAIL $name (link failed without naming the import)"; head -2 bcm_link.out; fail=1; return; }
    else
        TRS="$TRS" ./bcm > bcm.out 2>&1; rc=$?
        if [ "$rc" -eq 139 ]; then echo "FAIL $name (segfault)"; fail=1; return; fi
        if [ "$rc" -eq 0 ]; then echo "FAIL $name (ran clean without the import)"; fail=1; return; fi
        grep -q "BDPI import 'bdpi_mystery'" bcm.out || { echo "FAIL $name (trap message missing)"; head -2 bcm.out; fail=1; return; }
    fi
    echo "PASS $name"
}
check_chunked_missing
check_dyn DynSched sysDynSched G0100
check_dyn DynSchedBoth sysDynSchedBoth G0101
check_dyn DynSchedSelf sysDynSchedSelf G0096
check_dyn DynSchedLoop sysDynSchedLoop G0116
# rung-40 EN liveness (external review): byte parity on ALL THREE
# tiers — interp, hybrid jit, and the aot artifact — for designs
# whose enables exercise the liveness walk; a pruned-but-read EN now
# fails CLOSED on every tier, so parity here also witnesses that no
# trap fires
check_en() { # name top
    name=$1; top=$2
    set_bdpi ""
    cp "$SRC/$name.bsv" .
    $BSC -sim -u -g "$top" "$name.bsv" >/dev/null 2>&1 || { echo "FAIL $name (bsc)"; fail=1; return; }
    ref_link "$top" en_ref.exe >/dev/null 2>&1 || { echo "FAIL $name (ref link)"; fail=1; return; }
    ./en_ref.exe > en_ref.out 2>&1; refrc=$?
    "$TRS" run "$top.bir" > en_i.out 2>&1; irc=$?
    if [ "$irc" != "$refrc" ] || ! cmp -s en_ref.out en_i.out; then echo "FAIL $name (interp)"; diff en_ref.out en_i.out | head -3; fail=1; return; fi
    TRS_JIT=1 "$TRS" run "$top.bir" > en_j.out 2>&1; jrc=$?
    if [ "$jrc" != "$refrc" ] || ! cmp -s en_ref.out en_j.out; then echo "FAIL $name (jit)"; diff en_ref.out en_j.out | head -3; fail=1; return; fi
    TRS_REQUIRE_AOT=1 "$TRS" link $BDPI "$top.bir" -o en_art >en_link.out 2>&1 || { echo "FAIL $name (link)"; fail=1; return; }
    TRS="$TRS" ./en_art > en_a.out 2>&1; arc=$?
    if [ "$arc" != "$refrc" ] || ! cmp -s en_ref.out en_a.out; then echo "FAIL $name (aot)"; diff en_ref.out en_a.out | head -3; fail=1; return; fi
    echo "PASS $name"
}
# the REAL live-EN shape (rule-vs-method conflict: CAN_FIRE_bump reads
# EN_poke) — the battery's only design with a runtime-live enable
check_en EnConflict sysEnConflict
# MethValue path pin: an AV method whose body+result cones read the
# child wire's whas, consumed through Expr::MethValue on every tier
check_en MethValueEn sysMethValueEn
# census pins (the de-circularized ENSUM enumerates the PORT TABLE,
# so pruned-but-read enables are VISIBLE): EnConflict's EN_poke must
# stay live-allocated; MethValueEn's EN_ping is legitimately pruned
# (table-read only — the wire routes through the RWire prim) and the
# census must say so instead of hiding the row
census_pin() { # name top pattern
    name=$1; top=$2; pat=$3
    TRS_LAYOUT_CENSUS="cens_$name.txt" "$TRS" link $BDPI "$top.bir" -o "censart_$name" >/dev/null 2>&1
    grep -Eq "$pat" "cens_$name.txt" || { echo "FAIL $name (census pin: $pat)"; grep "^EN " "cens_$name.txt" | head -3; fail=1; return; }
    echo "PASS $name"
}
census_pin EnConflictCensus sysEnConflict '^EN [0-9]+ [0-9]+ read=1 live=1 stay1=. alloc=1 EN_poke'
census_pin MethValueEnCensus sysMethValueEn '^EN [0-9]+ - read=1 live=0 stay1=. alloc=0 EN_ping'
# dynamic scheduling x EN liveness: DynSched's shape plus a live
# enable (kick conflicts with rule r) inside the alt-carrying
# composition, and an AV peek consumed via MethValue from an alt-
# reordered rule — the alternates walk must keep EN_kick's slot
check_dyn DynSchedEn sysDynSchedEn G0100
# ...and the census must agree on an ALTS design (its live column
# walks alternate cones too): EN_kick live-allocated, EN_put pruned
census_pin DynSchedEnCensus sysDynSchedEn '^EN [0-9]+ [0-9]+ read=1 live=1 stay1=. alloc=1 EN_kick'
# layout-rev compat negatives: the load gate is an exact-equality
# check, so ONE binary witnesses BOTH skew directions — baking REV-1
# plays new-runtime/old-artifact, REV+1 plays old-runtime/new-artifact
# (the rev-26 field hazard: trs_cb_bdpi_missing joined the callback
# ABI without a bump, so an old runtime loaded the new artifact and
# null-called the unfilled trap pointer).  TRS_TEST_LAYOUT_REV is the
# emit-side-only bake override; the check side has none on purpose.
# The mismatched artifact must refuse LOUDLY on stderr, fall back to
# an in-process compile, and still match the reference byte-for-byte
# (never rc 139 — the null-call regression this test pins).
check_revcompat() {
    name=RevCompat; top=sysEdgeSelfKill
    cp "$SRC/EdgeSelfKill.bsv" .
    $BSC -sim -u -g "$top" EdgeSelfKill.bsv >/dev/null 2>&1 || { echo "FAIL $name (bsc)"; fail=1; return; }
    ref_link "$top" rev_ref.exe >/dev/null 2>&1 || { echo "FAIL $name (ref link)"; fail=1; return; }
    ./rev_ref.exe > rev_ref.out 2>&1; refrc=$?
    # control: a matched-rev artifact loads with no refusal note
    "$TRS" link $BDPI "$top.bir" -o revart >revlink.out 2>&1 || { echo "FAIL $name (link)"; fail=1; return; }
    TRS="$TRS" ./revart > revgot.out 2>revgot.err; gotrc=$?
    if grep -q "layout revision" revgot.err; then echo "FAIL $name (control run refused)"; fail=1; return; fi
    if [ "$refrc" != "$gotrc" ] || ! cmp -s rev_ref.out revgot.out; then echo "FAIL $name (control)"; fail=1; return; fi
    # the link's own window bake loads the just-baked mismatched .so
    # and prints the same fallback note into revlink.out — expected;
    # only the RUN's stderr is asserted
    for rev in 26 28; do
        TRS_TEST_LAYOUT_REV=$rev "$TRS" link $BDPI "$top.bir" -o revart >revlink.out 2>&1 || { echo "FAIL $name (link rev $rev)"; fail=1; return; }
        TRS="$TRS" ./revart > revgot.out 2>revgot.err; gotrc=$?
        if [ "$gotrc" = 139 ]; then echo "FAIL $name (rev $rev segfault)"; fail=1; return; fi
        grep -q "layout revision $rev" revgot.err || { echo "FAIL $name (rev $rev: no refusal note)"; sed -n 1,3p revgot.err; fail=1; return; }
        grep -q "compiling in-process instead" revgot.err || { echo "FAIL $name (rev $rev: no fallback note)"; fail=1; return; }
        if [ "$refrc" != "$gotrc" ] || ! cmp -s rev_ref.out revgot.out; then echo "FAIL $name (rev $rev: fallback parity)"; fail=1; return; fi
    done
    echo "PASS $name"
}
check_revcompat
# the demoted size tier is a distinct codegen path (lever 1 replaced
# default<O1> with a pinned O1-minus-MemCpyOpt expansion): force a
# tiny design over the budget and pin the tier trace, the pinned
# string PARSING (a stale string under a future LLVM must fail this
# check, not silently deoptimize), and byte parity
check_demoted() {
    name=DemotedTier; top=sysEdgeSelfKill
    cp "$SRC/EdgeSelfKill.bsv" .
    $BSC -sim -u -g "$top" EdgeSelfKill.bsv >/dev/null 2>&1 || { echo "FAIL $name (bsc)"; fail=1; return; }
    ref_link "$top" dt_ref.exe >/dev/null 2>&1 || { echo "FAIL $name (ref link)"; fail=1; return; }
    ./dt_ref.exe < /dev/null > dt_ref.out 2>&1; refrc=$?
    TRS_JIT_FN_INSN_BUDGET=10 TRS_JIT_TRACE=1 "$TRS" link $BDPI "$top.bir" -o dtart >dt_link.out 2>&1 || { echo "FAIL $name (link)"; fail=1; return; }
    grep -q "demoted size tier" dt_link.out || { echo "FAIL $name (tier did not engage)"; fail=1; return; }
    grep -q "IR pass pipeline rejected" dt_link.out && { echo "FAIL $name (pinned pipeline rejected)"; fail=1; return; }
    TRS="$TRS" ./dtart < /dev/null > dt_got.out 2>&1; gotrc=$?
    if [ "$refrc" != "$gotrc" ] || ! cmp -s dt_ref.out dt_got.out; then echo "FAIL $name (parity)"; diff dt_ref.out dt_got.out | head -3; fail=1; return; fi
    echo "PASS $name"
}
check_demoted
exit $fail
