## Synthesized design: `verinorun` — sim-keyed skip-not-run interlock

Base = minimal-extension (ambient bracket, whole-tail worker wrap, per-check UNSUPPORTED parity, house-style veribug keying). Grafts: (1) sim-name validation with loud perror, from declarative-guard — a typo'd sim name must not silently disarm a guard whose failure mode is an infinite hang; (2) missing-file safety net in compare_file, from sim-capability — makes raw sites work with ZERO conversions and turns a forgotten conversion into UNSUPPORTED instead of an orphan FAIL; (3) both per-file DejaGnu hooks (bsc_init AND bsc_finish) for leak-proofing (verified available and unused: /usr/share/dejagnu/runtest.exp:1615,1652; no bsc_init/bsc_finish in the harness today); (4) sim-capability's PR-validation checklist and CI UNSUPPORTED-count gate. Capability tags are deliberately NOT adopted: premature for the in-triage inout case and heavier than R6 wants; note in the PR that the sim lists can migrate to a capability table later without touching the hook points.

### 1. Core procs — testsuite/config/unix.exp, immediately above check_verilog_output (~line 3139)

```tcl
# ---------------
# Sim-keyed "cannot run" interlock, in the style of the 'veribug' argument.
#
# 'verinorun SIMS REASON' marks the verilog link/sim/compare checks that
# follow as un-runnable under any simulator in SIMS: those checks are
# reported as UNSUPPORTED (carrying REASON) instead of being attempted.
# The BSC compile to Verilog still runs and is still checked.  Under
# simulators not in SIMS, behavior is unchanged.  'verinorun_end'
# restores normal behavior; the state also auto-clears around each .exp
# file (bsc_init/bsc_finish).  REASON is required: it is the audit trail.

# Simulator names the harness recognizes (see the clean_*_output dispatch
# in verilog.tcl).  verinorun rejects anything else: a typo'd sim name
# would silently disarm the guard and hang the suite.
set verinorun_known_sims {iverilog verilator cvc vcs vcsi ncverilog modelsim questa}

set verinorun_sims   {}
set verinorun_reason ""

proc verinorun { sims reason } {
    global verinorun_sims verinorun_reason verinorun_known_sims
    if { [llength $sims] == 0 } {
        perror "verinorun: a non-empty simulator list is required"
        return
    }
    if { [string compare $reason ""] == 0 } {
        perror "verinorun: a non-empty reason string is required"
        return
    }
    foreach s $sims {
        if { [lsearch -exact $verinorun_known_sims $s] == -1 } {
            perror "verinorun: unknown simulator `$s'"
            return
        }
    }
    set verinorun_sims $sims
    set verinorun_reason $reason
}

proc verinorun_end {} {
    global verinorun_sims verinorun_reason
    set verinorun_sims   {}
    set verinorun_reason ""
}

# true iff the current verilog simulator is marked cannot-run
proc verinorun_active {} {
    global verinorun_sims verilog_compiler
    if { ! [info exists verilog_compiler] } { return 0 }
    return [expr {[lsearch -exact $verinorun_sims $verilog_compiler] != -1}]
}

# report one suppressed check
proc verinorun_unsupported { what } {
    global verinorun_reason verilog_compiler
    incr_stat "verinorun"
    unsupported "$what: not run under $verilog_compiler: $verinorun_reason"
}
```

Near bsc_initialize (~unix.exp:3687), the leak-proofing hooks (DejaGnu calls ${tool}_init before and ${tool}_finish after sourcing every .exp, tool=bsc; ${tool}_finish runs even when the .exp aborted on a Tcl error, so a span can never leak past its file; within a file, DejaGnu's per-file catch bounds any mid-span error to that file):

```tcl
proc bsc_init { test_file_name } { verinorun_end }
proc bsc_finish {}               { verinorun_end }
```

### 2. Hook points

(a) verilog.tcl self-guards — same 4-line stanza inside the existing `if {$vtest == 1}` and BEFORE incr_stat, in: link_verilog_pass (line 4), sim_verilog (136), sim_verilog_vcd (152), sim_verilog_status (167), and (graft from declarative-guard's broader coverage, zero cost) link_verilog_fail (30) and link_verilog_no_main_pass (69):

```tcl
proc link_verilog_pass { objects toplevel {options ""}} {
    global vtest
    if {$vtest == 1} {
      if { [verinorun_active] } {
          verinorun_unsupported "`$objects' link to executable `$toplevel'"
          return
      }
      incr_stat "link_verilog_pass"
      ...unchanged...
```

Message text mirrors each proc's PASS wording so a .sum diff between two sims shows clean PASS↔UNSUPPORTED substitutions. NOT hooked: bsc_link_verilog / sim_verilog_int / link_verilog_no_main (internal builders — vtest is likewise only checked in reporting procs), compare_verilog (compares generated .v text: sim-independent, must keep running).

(b) unix.exp check_verilog_output (~3139) — 4-line guard at top:

```tcl
proc check_verilog_output { output expected veribug } {
    global verilog_compiler
    if { [verinorun_active] } {
        verinorun_unsupported "`$output' matches `$expected'"
        return
    }
    ...unchanged...
```

This makes test_syn compose for free (compile runs; link/sim/compare each mark UNSUPPORTED).

(c) unix.exp compare_file (~2352, right after the cd into $subdir) — the sim-capability safety net, so raw sites need no conversion and a forgotten one can never orphan-FAIL:

```tcl
    # verinorun safety net: inside an armed span, a compare of a file the
    # skipped simulator would have produced is UNSUPPORTED, not FAIL.
    # Only fires when the file is missing, so Bluesim/compile-output
    # compares inside the same span still run and still catch real bugs.
    # (Raw sites should still 'erase' sim outputs up front, per house
    # convention, so a stale .out cannot be compared instead.)
    if { ! [file exists $filename] && [verinorun_active] } {
        verinorun_unsupported "`$filename' output comparison"
        cd $here
        return 0
    }
```

Known residual (accepted, documented in the comment): inside an armed span a *genuinely* missing non-sim output is reported UNSUPPORTED rather than FAIL; the producing check (sim_output / link_objects_pass) still FAILs, so nothing is fully masked. Preferred style for raw verilog-output compares remains `check_verilog_output OUT EXPECTED ""` (drop-in for compare_file when veribug is empty — verified at unix.exp:3139) because it gives the mirrored per-check message; the net is the backstop, not the idiom.

(d) unix.exp test_c_veri_worker_int (~2976) — the single choke point for idioms (a) and (c). Guard AFTER compile_verilog_pass (the -elab compile must run: it produces the .ba files the doC branch links, and actually_doV stays 1 so compile_object_pass stays skipped — byte-identical C-half). The ENTIRE remaining tail (link, sim/sim_verilog_int, move, sort, check_verilog_output, and the whole check_vcd block, current lines 2977-3017) goes in the else — this whole-tail wrap is what makes the veribug∧verinorun overlap safe, because the veribug path's unchecked sim_verilog_int calls can never fire while armed:

```tcl
        compile_verilog_pass $top.$extension $sysmod $gen_options

        if { [verinorun_active] } {
            # Cannot be linked or simulated under this simulator: one
            # UNSUPPORTED per check that would have been reported.  The
            # .v compile above still ran (sim-independent, and with
            # -elab it feeds the Bluesim link below).  The C half is
            # untouched.
            verinorun_unsupported "`[join $vmods]' link to executable `$sysmod'"
            verinorun_unsupported "Verilog simulation `$sysmod' executes"
            verinorun_unsupported "`$sysmod.v.out' matches `$expected'"
            if { $check_vcd == 1 } {
                verinorun_unsupported "Verilog simulation `$sysmod' executes with VCD dump"
            }
        } else {
            link_verilog_pass [join $vmods] $sysmod $link_options
            ...existing lines 2977-3017 verbatim, re-indented (git diff -w shows the true ~12-line size)...
        }
```

Marker parity: exactly one UNSUPPORTED per check reported today (link, sim, compare, +vcd when check_vcd==1), so per-sim totals reconcile line-for-line.

### 3. Call-site syntax

Idiom (a) — bundle (battery.exp:43, srandom case; doC=1 doV=1):

```tcl
# The BDPI import `srandom' collides with the SystemVerilog built-in of
# the same name under DPI linkage (implied for verilator); Verilator
# 5.048 hits an internal error at link.  Upstream test content — the
# import name must not be renamed.
verinorun verilator "BDPI import `srandom' collides with the SV built-in under DPI; Verilator 5.048 internal error at link"
test_c_veri_bsv_multi_options TestActionValues mkTestActionValues "$actionvalue_abins const_narrow.ba const_wide.ba common.c actionvalues.c values.c" {} {} {} $iverilog_bug 1 1 {} {+Hi}
verinorun_end
```

Under verilator: .v compile PASSes, 4 UNSUPPORTED, full Bluesim half runs untouched. Under iverilog/cvc/vcs: byte-identical, including the $iverilog_bug veribug behavior. A contiguous run of INFINITELOOP-class bundles with one reason shares one bracket (it is a span, not a one-shot) — this is what keeps the ~60-site conversion cheap:

```tcl
verinorun verilator "derived-clock generators need event-driven timing (--timing); spin under the harness's --no-timing (verilator's fatal INFINITELOOP lint is the enforcement)"
test_veri_only_bsv ClockMux        "sysClockMux.v.out.expected"
test_veri_only_bsv ClockSelect     "sysClockSelect.v.out.expected"
test_veri_only_bsv UngatedClockMux "sysUngatedClockMux.v.out.expected"
verinorun_end
```

Idiom (b) — raw sequence (b535 shape). No rewrite of any existing call is required (the compare_file net covers a bare compare); preferred form uses check_verilog_output for the mirrored message:

```tcl
compile_verilog_pass ClockDiv.bsv sysClockDiv        ;# runs: sim-independent

verinorun verilator "derived-clock generator spins under --no-timing; INFINITELOOP lint fires at verilate time"
link_verilog_pass sysClockDiv.v sysClockDiv          ;# UNSUPPORTED
sim_verilog sysClockDiv                               ;# UNSUPPORTED
check_verilog_output sysClockDiv.out sysClockDiv.out.expected ""  ;# UNSUPPORTED (or a legacy bare
                                                      ;# 'compare_file sysClockDiv.out' -> UNSUPPORTED via the net)
copy sysClockDiv.out sysClockDiv.v.out               ;# tolerant no-op (copy/move/erase test file exists first)
verinorun_end
```

Multi-config .exp — inout.exp (same test_inout_simulation battery run twice under different BSC_OPTIONS, lines 5/69/71/86; only the -no-inline-inout-connect run is broken). The ambient bracket expresses "same 12 calls, skip in run 1 only" — impossible for any argument-threading design:

```tcl
set ::env(BSC_OPTIONS) "-no-inline-inout-connect $old_bsc_options"
# InoutConnect/tran-primitive designs fail to link under verilator
# (triage ongoing; if it lands as veribug-XFAIL, delete these two lines
# — the mechanisms are orthogonal).  The .v compiles are still checked.
verinorun verilator "InoutConnect (tran primitive) designs fail to link under verilator with -no-inline-inout-connect"
test_inout_simulation
verinorun_end

set ::env(BSC_OPTIONS) $old_bsc_options
# *.v.out / *.diff-out rename loops: glob_pattern uses -nocomplain, so
# under verilator both foreach loops iterate empty lists — no failures.

test_inout_simulation        ;# default config: unmarked, runs everywhere
```

### 4. UNSUPPORTED reporting and audit

Each suppressed check yields one standard DejaGnu line in the per-directory testrun.sum:

```
UNSUPPORTED: `sysClockMux.v' link to executable `sysClockMux': not run under verilator: derived-clock generator spins under --no-timing; INFINITELOOP lint fires at verilate time
```

- .sum tail gains "# of unsupported tests"; PASS/FAIL never silently shrink — 1:1 marker parity with today's checks in the bundle/preferred-raw cases.
- Static audit: `grep -rn '^verinorun ' testsuite --include='*.exp'` → every site, sim list, mandatory reason. Runtime audit: `grep -h '^UNSUPPORTED:' **/testrun.sum`. `incr_stat "verinorun"` surfaces the total in the Test Distribution Summary.
- Suite-monitor poll loops should tally `^UNSUPPORTED:` alongside `^PASS:`/`^FAIL:`; the verilator CI gate should additionally assert the expected UNSUPPORTED count so a change that silently widens skipping is caught (graft from sim-capability).

### 5. Edge cases (carried through)

- vcd variant: worker emits the vcd marker only when check_vcd==1; dump.vcd never exists; erase/move are missing-file no-ops (unix.exp:627-695).
- veribug overlap: verinorun dominates — the whole tail incl. the unchecked sim_verilog_int and the XFAIL compare is skipped; sims only in veribug behave exactly as today.
- setup_xfail hygiene: unsupported → record_test → `set xfail_flag 0` (framework.exp), so an armed xfail from *_pass_bug is consumed by the UNSUPPORTED, never leaking.
- vtest==0 / doV==0: every guard sits inside the existing vtest/actually_doV conditions — no spurious markers; verinorun_active also tolerates unset verilog_compiler.
- R4: two per-process globals, reset by bsc_init/bsc_finish around every .exp; no external state files; parallel per-directory runtests independent.
- R5: no .vlt files, no lint flags touched; verilator's fatal INFINITELOOP remains the loud enforcement for every unguarded (and any newly written) spinning design.

### 6. Diff footprint

- unix.exp: core procs + known-sims list (~50 lines), check_verilog_output guard (4), compare_file net (7), worker tail wrap (~12 + whitespace), bsc_init/bsc_finish (2×3).
- verilog.tcl: 6 × 4-line stanzas.
- .exp sites: 2 additive lines per span; no existing invocation rewritten (bare compare_file conversions optional, covered by the net).

### 7. Validation checklist for the PR (from sim-capability)

1. battery dir under BSC_VERILOG_SIM=verilator: compile PASS + 4 UNSUPPORTED + full Bluesim PASSes for mkTestActionValues; under iverilog: .sum byte-identical to pre-change.
2. One MCD dir under verilator: zero INFINITELOOP FAILs, matching UNSUPPORTED count; under iverilog: byte-identical.
3. A raw-sequence dir: UNSUPPORTED for link/sim/compare, no orphan FAIL from compare_file/copy — test both the check_verilog_output form and a deliberately unconverted bare compare_file (net path).
4. Negative: `verinorun verilaotr ...` and empty reason → ERROR/UNRESOLVED, never a silent no-op; full-suite .sum diff under iverilog must be empty.

Key paths: /home/ravi/bluespec/matx/bsc/testsuite/config/unix.exp (hooks ~2976, ~2352, ~3139, ~3687), /home/ravi/bluespec/matx/bsc/testsuite/config/verilog.tcl (procs at 4, 30, 69, 136, 152, 167), /home/ravi/bluespec/matx/bsc/testsuite/bsc.codegen/foreign/battery/battery.exp:43, /home/ravi/bluespec/matx/bsc/testsuite/bsc.verilog/inout/inout.exp (lines 5/69/71/86).