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
    "$TRS" link "$top.bir" -o art >link.out 2>&1 || { echo "FAIL $name (trs link)"; fail=1; return; }
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
    cp "$SRC/$name.bsv" .
    $BSC -sim -bir -u -g "$top" "$name.bsv" >/dev/null 2>&1 || { echo "FAIL $name (bsc)"; fail=1; return; }
    $BSC -sim -bir -e "$top" -o ref.exe >/dev/null 2>&1 || { echo "FAIL $name (ref link)"; fail=1; return; }
    rm -f dump.vcd
    ./ref.exe > ref.out 2>&1; refrc=$?
    sed '/^\$date/,/^\$end/d' dump.vcd > ref.vcd 2>/dev/null
    "$TRS" link "$top.bir" -o art >/dev/null 2>&1 || { echo "FAIL $name (trs link)"; fail=1; return; }
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
# ---- top-level restriction lifts (-trs only; no reference Bluesim
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
    if $BSC -sim -bir -e "$top" -o tp_ref.exe >tp_err1.out 2>&1; then
        echo "FAIL $name (classic Bluesim link unexpectedly succeeded)"; fail=1; return
    fi
    grep -q "(G0099)" tp_err1.out || { echo "FAIL $name (expected G0099)"; fail=1; return; }
    # bsc's own -trs link supplies no bindings: the trs link inside it
    # must fail with the loud missing-binding error (and still export
    # the .bir, which everything below consumes)
    if TRS="$TRS" $BSC -sim -bir -trs -e "$top" -o tp.exe >tp_err2.out 2>&1; then
        echo "FAIL $name (-trs link without bindings unexpectedly succeeded)"; fail=1; return
    fi
    grep -q "requires bindings for" tp_err2.out || { echo "FAIL $name (expected missing-binding error)"; fail=1; return; }
    [ -f "$top.bir" ] || { echo "FAIL $name (no .bir exported)"; fail=1; return; }
    "$TRS" link "$top.bir" +big=1 +inc=1 +typo=9 -o tpbad >tp_err3.out 2>&1 && { echo "FAIL $name (unknown binding accepted)"; fail=1; return; }
    grep -q "unknown top-level binding" tp_err3.out || { echo "FAIL $name (expected unknown-binding error)"; fail=1; return; }
    "$TRS" run "$top.bir" +big=1 +inc=999 >tp_err4.out 2>&1 && { echo "FAIL $name (oversized binding accepted)"; fail=1; return; }
    grep -q "does not fit in the declared width" tp_err4.out || { echo "FAIL $name (expected oversized-binding error)"; fail=1; return; }
    "$TRS" run "$top.bir" +big=$bigv +inc=3 > got.out 2>&1; gotrc=$?
    if [ "$gotrc" != 0 ] || ! cmp -s "$SRC/$name.expected" got.out; then
        echo "FAIL $name (run stdout, rc=$gotrc)"; diff "$SRC/$name.expected" got.out | head -3; fail=1; return
    fi
    "$TRS" link "$top.bir" +big=$bigv +inc=3 -o tpart >tplink.out 2>&1 || { echo "FAIL $name (trs link)"; fail=1; return; }
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
# values), with setStep's argument constant-bound.  The documented v1
# engine contract is INTERPRETED with the specific decline reason —
# asserted here in both spellings (the link note and the traced why).
check_topae() {
    name=TopAlwaysEn; top=sysTopAlwaysEn
    cp "$SRC/$name.bsv" .
    $BSC -sim -u -g "$top" "$name.bsv" >/dev/null 2>&1 || { echo "FAIL $name (bsc)"; fail=1; return; }
    if $BSC -sim -bir -e "$top" -o ae_ref.exe >ae_err1.out 2>&1; then
        echo "FAIL $name (classic Bluesim link unexpectedly succeeded)"; fail=1; return
    fi
    grep -q "(G0062)" ae_err1.out || { echo "FAIL $name (expected G0062)"; fail=1; return; }
    if TRS="$TRS" $BSC -sim -bir -trs -e "$top" -o ae.exe >ae_err2.out 2>&1; then
        echo "FAIL $name (-trs link without bindings unexpectedly succeeded)"; fail=1; return
    fi
    grep -q "requires bindings for" ae_err2.out || { echo "FAIL $name (expected missing-binding error)"; fail=1; return; }
    "$TRS" run "$top.bir" +setStep.v=2 > got.out 2>&1; gotrc=$?
    if [ "$gotrc" != 0 ] || ! cmp -s "$SRC/$name.expected" got.out; then
        echo "FAIL $name (run stdout, rc=$gotrc)"; diff "$SRC/$name.expected" got.out | head -3; fail=1; return
    fi
    TRS_JIT_TRACE=1 "$TRS" link "$top.bir" +setStep.v=2 -o aeart >aelink.out 2>&1 || { echo "FAIL $name (trs link)"; fail=1; return; }
    grep -q "run interpreted" aelink.out || { echo "FAIL $name (expected interpreted artifact)"; fail=1; return; }
    grep -q "top always_enabled autofire" aelink.out || { echo "FAIL $name (expected the autofire decline reason)"; fail=1; return; }
    TRS="$TRS" ./aeart > gota.out 2>&1; gotrc=$?
    if [ "$gotrc" != 0 ] || ! cmp -s "$SRC/$name.expected" gota.out; then
        echo "FAIL $name (artifact stdout, rc=$gotrc)"; diff "$SRC/$name.expected" gota.out | head -3; fail=1; return
    fi
    echo "PASS $name"
}
check_topae
# NEGATIVE: bindable arguments plus an additional input clock — a
# binding supplies a constant, never a waveform, so the -trs link
# refuses loudly (and classic keeps G0099 via the Bit# argument)
check_topclk() {
    name=TopClkArg; top=sysTopClkArg
    cp "$SRC/$name.bsv" .
    $BSC -sim -u -g "$top" "$name.bsv" >/dev/null 2>&1 || { echo "FAIL $name (bsc)"; fail=1; return; }
    if $BSC -sim -bir -e "$top" -o ck_ref.exe >ck_err1.out 2>&1; then
        echo "FAIL $name (classic Bluesim link unexpectedly succeeded)"; fail=1; return
    fi
    grep -q "(G0099)" ck_err1.out || { echo "FAIL $name (expected G0099)"; fail=1; return; }
    if TRS="$TRS" $BSC -sim -bir -trs -e "$top" -o ck.exe >ck_err2.out 2>&1; then
        echo "FAIL $name (-trs link unexpectedly succeeded)"; fail=1; return
    fi
    grep -q "does not support additional input" ck_err2.out || { echo "FAIL $name (expected input-clock refusal)"; fail=1; return; }
    echo "PASS $name"
}
check_topclk
# dynamic scheduling (bsc G0096/G0100/G0101/G0116 family): no
# reference Bluesim exe exists by design — the classic C++ backend
# refuses these designs — so stdout diffs against a stored golden
# whose values are hand-derived.  Also gates the two refusals: plain
# -sim errors with the class's tag, and -sched-dynamic without -trs
# errors at link.
check_dyn() { # name top errtag
    name=$1; top=$2; tag=$3
    cp "$SRC/$name.bsv" .
    if $BSC -sim -u -g "$top" "$name.bsv" >dyn_err1.out 2>&1; then
        echo "FAIL $name (static compile unexpectedly succeeded)"; fail=1; return
    fi
    grep -q "$tag" dyn_err1.out || { echo "FAIL $name (expected $tag)"; fail=1; return; }
    $BSC -sim -sched-dynamic -u -g "$top" "$name.bsv" >/dev/null 2>&1 || { echo "FAIL $name (bsc -sched-dynamic)"; fail=1; return; }
    if $BSC -sim -sched-dynamic -bir -e "$top" -o dyn_ref.exe >dyn_err2.out 2>&1; then
        echo "FAIL $name (classic Bluesim link unexpectedly succeeded)"; fail=1; return
    fi
    grep -q "trs backend" dyn_err2.out || { echo "FAIL $name (expected trs-backend refusal)"; fail=1; return; }
    $BSC -sim -sched-dynamic -bir -trs -e "$top" -o dyn.exe >/dev/null 2>&1 || { echo "FAIL $name (bsc -trs link)"; fail=1; return; }
    "$TRS" run "$top.bir" > got.out 2>&1 || { echo "FAIL $name (trs run)"; fail=1; return; }
    if ! cmp -s "$SRC/$name.expected" got.out; then echo "FAIL $name (run stdout)"; diff "$SRC/$name.expected" got.out | head -3; fail=1; return; fi
    "$TRS" link "$top.bir" -o dynart >dynlink.out 2>&1 || { echo "FAIL $name (trs link)"; fail=1; return; }
    # the compiled engine does not execute dynamic schedules yet; a
    # compiled artifact here would mean the jit gate silently vanished
    grep -q "run interpreted" dynlink.out || { echo "FAIL $name (expected interpreted artifact)"; fail=1; return; }
    TRS="$TRS" ./dynart > gota.out 2>&1 || { echo "FAIL $name (art run)"; fail=1; return; }
    if ! cmp -s "$SRC/$name.expected" gota.out; then echo "FAIL $name (art stdout)"; diff "$SRC/$name.expected" gota.out | head -3; fail=1; return; fi
    echo "PASS $name"
}
check_dyn DynSched sysDynSched G0100
check_dyn DynSchedBoth sysDynSchedBoth G0101
check_dyn DynSchedSelf sysDynSchedSelf G0096
check_dyn DynSchedLoop sysDynSchedLoop G0116
exit $fail
