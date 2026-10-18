# Concurrent assertions from BH

Report on MatX-inc/bsc#236: SVA assertions written in Bluespec Haskell,
emitted as native SystemVerilog `assert property` for simulation and
formal, and executed by Bluesim. Two branches, each a draft PR:

- `yehowshua/sva-phase1` (#237): library over the existing `$SVA` string
  transport, plus the backend fixes formal needs.
- `yehowshua/sva-phase2`: first-class SVA primitives in the compiler,
  stacked on phase 1.

## Using it

```haskell
import SVAssert

mkArb = module
  req :: Reg (Bit 2) <- mkReg 0
  gnt :: Reg (Bit 2) <- mkReg 0
  ...
  svaAssert "grant_onehot"     (gnt /= 3)
  svaAssert "req0_granted"     ((req[0:0] == 1) |=> (gnt == 1))
  svaAssert "req1_within_two"  ((req == 2) |-> delayRange 1 2 True (gnt == 2))
  svaCover  "both_requesting"  (req == 3)
```

A property is an ordinary elaboration-time value: `Bool`s (or `Reg Bool`s)
are sequences, `##`, `delay n`, `delayRange lo hi`, `delayUnbound lo`,
`rep`, `repRange`, `repUnbound`, `sOr`, `sIntersect`, `firstMatch` build
sequences, `|->`, `|=>`, `pNot`, `pAnd`, `pOr` build properties.
`svaAssert`, `svaAssume`, `svaCover` and `svaAssertWith` (pass/fail
actions, Bluesim only) take a label and a property.

- `bsc -verilog` emits, per assertion, outside `translate_off`:

      `ifndef BSV_NO_SVA
      req0_granted: assert property (@(posedge CLK) disable iff (RST_N == `BSV_RESET_VALUE)
          (((req[0:0] == 1'd1)) ##1 ((1'd1))) |-> ((gnt == 2'd1)));
      `endif

  The rule's firing condition, if not constant, becomes the antecedent.
  Define `BSV_NO_SVA` for flows that only read Verilog-2001.
- `bsc -sim` lowers the same value to the FSM checkers in `SVA.bsv`;
  a failure prints `[SVA] assertion "req0_granted" failed`.
- `bsc -verilog -sva-immediate` is for formal tools without SVA support
  (open-source yosys): a boolean property prints as
  `always @(posedge CLK) if (!(RST_N == ...)) label: assert (expr);`,
  and a temporal one is checked by the FSM checkers in the design, with
  the assertion reduced to the flag they drive (a violation shows one
  cycle late).
- BSV's own `assert property` syntax with `-passthrough-assertions` now
  builds the same primitives; pass/fail actions are ignored there (P0226).

## What the phases changed

Phase 1 (compiler: `TCheck`, `IInlineFmt`, `AVerilogUtil`, `Verilog`;
library: `SVAssert.bs`, `SVA.bsv`):

- `$SVA` accepts a `Fmt`, which the library renders the property into, and
  `IInlineFmt` splices its pieces into the one call rather than splitting
  it like `$display`.
- The Verilog backend adds `disable iff` from the rule's resets, folds the
  firing condition in, replaces `translate_off` with the macro guard and
  prints the pieces structurally.
- Two bugs in the FSM checkers that lost failures: a finished `or` branch
  reported `SeqPending` forever, and a property copy was reused while its
  sequence was still mid-attempt from the previous use (`mkSeqOr`,
  `mkPropImplies`, `mkAssertAlways`). The BSV frontend's own `[*m:n]`
  desugaring still over-reports in Bluesim; that is separate and untouched.

Phase 2 (compiler: `Prim`, `Prelude.bs`, `IExpand`, `IExpandUtils`,
`AConv`, `ACheck`, `ASVA` (new), `AVerilogUtil`, `Verilog`, `GenABin`,
`MakeSymTab`, `Flags`, `Error`, `CVParserAssertion`):

- Abstract Prelude types `SvaSeq`/`SvaProp` and thirteen `primSva*`
  constructors. A property travels through ISyntax and ASyntax as a tree of
  suspended prims over packed booleans and `Bit 32` bounds; the typed
  foreign tasks `$SVA`, `$SVAssume`, `$SVACover` take `String -> SvaProp`.
- The Verilog AST gains `VSeq`/`VProp`, and `VAssert` carries kind, label,
  clock, `disable iff` and the tree. Native and immediate printers.
- `ASVA.checkSVA` runs after `AConv` for both backends: label is a Verilog
  identifier (G0134), unique in the module (G0135), boolean-only under
  `-sva-immediate` (G0136), and `$SVA` never reaches Bluesim (S0100).
- The `.bo` format tag is bumped (`PrimOp` is encoded by `fromEnum`).

## Verification

- `testsuite/bsc.assertions/svassert`: 54 checks on the phase 2 branch
  (Verilog shape for assert/assume/cover, immediate mode, the four
  diagnostics, Bluesim pass/fail/cover output, BSV pass-through).
  `bsc.assertions/properties` Bluesim checks still pass; its Verilog-sim
  checks need iverilog, which the build machine lacks.
- Verilator 5.048 `--assert`: lints both emission styles; on the counter
  design with one dropped `ack` it reports `ack_follows_req` and
  `ack_within_3` at the expected cycles.
- SymbiYosys (yosys 0.70, z3) on `examples/SVAssert/Arb.bs` compiled with
  `-sva-immediate`: `mode bmc`, depth 12, PASS; the broken arbiter in
  `ArbBad.bs` FAILs `req0_granted_next` at step 5 with a VCD counterexample.
  `mode prove` passes the base case but k-induction fails on the FSM
  checkers' unreachable states, so bounded checking is what the immediate
  style gives today. `examples/SVAssert/run_sby.sh` reproduces both.
- Not verified: a commercial formal tool on the native output.

## Easy, hard, and open

Easy: the native printer, the library combinators, `disable iff` and the
guard, assume/cover, the BSV pass-through on the primitives (its
`SVA_CORE` tree maps one to one), and the diagnostics.

Hard: the elaborator. A dynamic `Bool` is not a normal form (it is `PrimChr`
over bits), so property leaves are packed `Bit 1`; the prims had to be
registered as strict (`conAp'`), as walkable (`walkNF`, `realPrimOp`) and
their types as primitive (`isPrimType`), and `ACheck`, `MakeSymTab` and
`AConv` each had a whitelist to extend. None of it is deep, but each gate
is found by compiling. The FSM checker bugs took longest to find, because
a lost failure is silent.

Open:

- A text interface for DV users (`svaText "label" "req |-> ##1 ack" ...`)
  needs a small SVA parser in BH over a signal binding list; BSV users
  already have the native syntax.
- k-induction for the immediate style needs the checkers to be inductive
  or an abstraction; alternatively a yosys build with Verific reads the
  native output directly.
- `svaAssertWith` actions run only in Bluesim; a Verilog simulator reports
  the assertion itself.
- Unbounded repetition (`[*n:$]`) in Bluesim consumes a checker copy
  permanently, as in the BSV frontend.
