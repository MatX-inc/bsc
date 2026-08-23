# Handoff: two Bluesim-parity bugs from the matx corpus

First run of trs against the MatX monorepo's rtl testbenches (2026-08-22/23,
matx branch `claude/winning-monorepo-sd8zgt`, `tools/trs_bringup/`) found two
byte-parity divergences. Both reproduce identically under the interpreter and
the AOT artifact, so they are BIR/semantics-level, not codegen. Everything
below was isolated to minimal fixtures; the corpus tests that exposed them
are named for the reseal. A third, non-parity finding (a missing-BDPI
segfault in the link path) is at the end.

## Bug 1: don't-care module arguments concretize to 0, not the AA pattern

A `_` (don't-care) passed as a module instantiation argument reads back as
zeros under trs where Bluesim (and trs's own inline-expression path) produce
the 0xAA unspecified pattern.

Minimal fixture (stdlib only):

```bluespec
package MxParityDw where

mkMxParityDw :: Module Empty
mkMxParityDw = module
  w :: Wire (Bit 32) <- mkDWire _
  done :: Reg Bool <- mkReg False
  rules
    "probe": when not done ==> do
      $display "dwire_default=%h" w
      done := True
    "fin": when done ==> $finish 0
```

Observed: Bluesim `dwire_default=aaaaaaaa`; trs interp and artifact both
`dwire_default=00000000`.

Control probes that MATCH (both engines 0xAA), narrowing the bug to the
argument path:
- inline don't-care expression: `let x :: Bit 32 = _` displays `aaaaaaaa`
  on both engines;
- uninitialized RegFile holes (`mkRegFile` read-before-write) display
  `aaaaaaaa` on both engines.

Analysis: the runtime has the pattern (`Value::undet`,
trs-interp/src/value.rs:111, 0xAAAA... fill matching bsc's default
`-unspecified-to A`), and expression-level undets clearly reach it. The
instance-argument lowering does not: SimExportIR's instance-arg encoding has
no Undet arm (grep turns up none), so a don't-care argument most likely
leaves the exporter as a literal 0 and the undet-ness is unrecoverable
downstream. Fix belongs wherever instance args are encoded/decoded, routing
undet args to the same Value::undet the expression path uses.

Corpus witness: `rtl/tile/shared/test/SharedRegfileTest` — its `SWire` is a
DWire whose default carries a don't-care data plane; the test's expected-side
display prints `dat=[0xaaaaaaaa]` under Bluesim vs `dat=[0x00000000]` under
trs (test still passes both — the valid bit gates the comparison — but byte
parity is broken). Expect this class wherever `mkDWire _` / don't-care
defaults are displayed.

## Bug 2: coincident edges across two MakeClock domains sample post-edge

When two software-driven clock domains (`mkUngatedClock` + `setClockValue`,
MakeClock family) receive edges from the same testbench action in the same
instant, a register in domain B whose D-side reads a register in domain A
observes A's POST-edge value under trs; Bluesim observes the pre-edge value
(normal register semantics across simultaneous edges).

Control probe that MATCHES: one software-driven domain sampled from the
free-running kernel/default domain is correct on both engines (toggle,
derived 0-stage async reset, gate readback all byte-identical). The
divergence needs BOTH sides software-driven and coincidently edged.

Reproducing fixture (uses matx lib helpers; runnable via the matx branch —
`tools/trs_bringup/README.md` has the three-command flow — or translate to
a house .bsv regress fixture):

```bluespec
package MxParityClk where

import Clocks
import Rules
import RegExtra
import ClocksExtra

mkMxParityClk :: Module Empty
mkMxParityClk = module
  (cc, _cr) :: (Clock, Reset) <- exposeCurrentClkRst

  clkIfc :: MakeClockIfc Bool <- mkUngatedClock False
  let sclk :: Clock = clkIfc.new_clk
  srst :: Reset <- mkAsyncResetFromCR 0 sclk

  let mkTog :: Module (ReadOnly Bool)
      mkTog = module
        t :: Reg Bool <- mkRegA False
        rules
          "tog": when True ==> t := not t
        interface ReadOnly
          _read = t
  tog :: ReadOnly Bool <- withClkRst (sclk, srst) mkTog

  rstAsserted_ <- withClkRst (sclk, srst) isResetAssertedDirect
  rstAsserted :: ReadOnly Bool <- mkNullCrossingWire cc rstAsserted_
  toggleV :: ReadOnly Bool <- mkNullCrossingWire cc tog._read

  -- The divergent ingredient: the gate reader (a cross-domain toggle
  -- sampler, mkClkGate2Bool from matx ClocksExtra) lives in a SECOND
  -- software-driven domain, edged by the same testbench action.
  clkIfcU :: MakeClockIfc Bool <- mkUngatedClock False
  let uclk :: Clock = clkIfcU.new_clk
  urst :: Reset <- mkAsyncReset 3 _cr uclk
  gateBool_ :: Bool <- withClkRst (uclk, urst) $ mkClkGate2Bool sclk
  gateBool :: ReadOnly Bool <- mkNullCrossingWire cc gateBool_

  cyc :: Reg (UInt 8) <- mkReg 0
  rules
    "drive": when True ==> do
      cyc := cyc + 1
      clkIfc.setClockValue ((pack cyc)[0:0] == 1)
      clkIfcU.setClockValue ((pack cyc)[0:0] == 1)
      $display "cyc %0d clk=%b rstAsserted=%b toggle=%b gateBool=%b"
        cyc clkIfc.getClockValue rstAsserted._read toggleV._read gateBool._read
      doIf (cyc == 12) ($finish 0)
```

Observed: identical through cyc 7 (while uclk's 3-stage reset holds); from
cyc 8 Bluesim reads `gateBool=1` steadily, trs reads `gateBool=0` steadily —
i.e. mkClkGate2Bool's `toggleDly` (uclk domain) equals `toggle` (sclk
domain) every instant under trs, so the gate detector reports "disabled".

Analysis: both domains' pending levels are applied somewhere in the same
timeslice; the uclk-domain register's D-side read of the sclk-domain toggle
must see the pre-edge value. Native prims survive same-kernel-instant edges
via timestamp shadows (cf. SyncVar's backdating, trs-interp/src/prim.rs
~:2999); the MakeClock-to-MakeClock coincident case appears to bypass that —
suspect the per-timeslice ordering of derived-domain edge processing (one
domain's edge commits before the other's samplers read). A regress fixture
in the BviClocks style (coincident posedges + crossing reg, but for
MakeClock domains) would pin the semantics permanently.

Corpus witness: `rtl/lib/test/ClockGatingTest` — passes Bluesim, fails trs
(`Expected dut = False to be True`), rc 0 vs 1.

## Robustness: a missing BDPI .so segfaults the JIT/AOT link path

Not a parity bug — a crash-versus-designed-error gap from the same sweep.
A design that *calls* an imported BDPI function whose partner `.so` is
absent behaves differently per engine path:

- `trs run <bir>` (interp): the designed loud panic — `BDPI function
  "bdpi_mufu_result" called but no .bdpi.so was found next to the .bir
  (link with the user's C files)` (trs-interp/src/lib.rs:2222).
- `trs link <bir>`: SIGSEGV (exit 139) once the JIT comes on and the
  measurement/verification run first executes the BDPI call site — the
  compiled body appears to go through the unresolved function pointer
  without the interp path's guard. The `.so`, arena, and birsnap are
  already written when it dies; the `.cexe` never lands.

Corpus witnesses: `PbuAlu0Fp{Exp2,Exp2Add1,AbsExp2Add1,Cos}Test` — all
call `bdpi_mufu_result` on an early cycle; the same BIR crashes the
trs/33 and trs/36 linkers identically (a 512MB stack does not help).
Contrast: the cosim tests die in the *guarded* path (lib.rs:2183 panic
during link measurement), and a design that merely imports BDPI without
calling it is fine (MxSimdAluBench passes byte-exact; its mufu import is
dead code there).

Expected fix shape: route compiled BDPI call sites through the same
missing-.so check the interp uses — or fail `trs link` cleanly when a
*reachable* BDPI import has no `.so` — so both paths fail with the
designed panic.

## Reseal expectations

Both fixtures byte-exact on interp and artifact; `ClockGatingTest` and
`SharedRegfileTest` byte-exact through the matx sweep
(`tools/trs_bringup/bin/mxsweep.sh` on the matx branch); the stack's own
battery and 1003/0 census unchanged. The matx corpus's other 136 passing
testbenches are the regression guard for the fixes.
