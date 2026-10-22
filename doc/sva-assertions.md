# Concurrent assertions from BH

SVA assertions written in Bluespec Haskell and emitted as native
SystemVerilog `assert property` for simulators and formal tools (Jasper is
the target). MatX-inc/bsc#236.

## Using it

```haskell
import SVAssert

mkArb = module
  ...
  svaAssert "grant_onehot"      (gnt /= 3)
  svaAssert "req0_granted_next" ((req[0:0] == 1) |=> (gnt == 1))
  svaAssert "req1_granted_soon" ((req == 2) |-> cycleDelayRange 1 2 True (gnt == 2))
  svaCover  "both_requesting"   (req == 3)
```

A property is an elaboration-time value. `Bool`s (or `Reg Bool`s) are the
atoms; the combinators in `SVAssert.bs` follow the operator names of IEEE
1800 clause 16 (`cycleDelay`, `consecutiveRep`, `gotoRep`, `throughout`,
`within`, `firstMatch`, `overlappingImplication` as `|->`, `negation`,
`nexttime`, `until`, `strong`, ...), with short aliases for the common ones
(`delay`, `rep`, `pNot`, ...). `svaAssert`, `svaAssume` and `svaCover` take
a label and a property. `guarantee`/`require` with a `Role` write a link's
rules once and let the module under verification decide which are assumed
and which are checked.

`bsc -verilog` emits, per statement, outside `translate_off`:

    `ifndef BSV_NO_SVA
    req0_granted_next: assert property (@(posedge CLK) disable iff (RST_N == `BSV_RESET_VALUE)
        (((req[0:0] == 1'd1)) |=> ((gnt == 2'd1))));
    `endif

The clock and reset are the rule's, so the module's defaults. A rule's
firing condition, when not constant, becomes the antecedent. Define
`BSV_NO_SVA` for flows that only read Verilog-2001. Under `-sim` the
statements are no-ops; Bluesim does not check SVA. `-sva-immediate` prints
a *boolean* property as `always @(posedge CLK) if (!(reset)) label: assert
(expr);` for tools without SVA support; a temporal property is an error
(G0136) in that mode.

BSV's own `assert property` syntax with `-passthrough-assertions` builds
the same tree; its pass/fail actions are ignored there (P0226).

## How it works

The compiler knows nothing about SVA operators. The Prelude has an
abstract type `SvaNode` and three constructors: `primSvaBits` (a design
signal), `primSvaInt` (a constant) and `primSvaNodeN` (a text template with
`%0`, `%1`, ... for its N children). `SVAssert.bs` defines every operator
as one template, e.g.

```haskell
cycleDelayRange m n a b = Seq (primSvaNode4 "(%0) ##[%1:%2] (%3)" (unS a) (int m) (int n) (unS b))
```

`Seq` and `Prop` are distinct types, so a composition the LRM forbids (a
property inside `##`, a sequence as the operand of goto repetition) is a
type error. Every template parenthesizes its operands, so the printed text
never depends on SVA precedence. The tree travels through elaboration and
ASyntax as a strict `APrim` tree of abstract type (`IExpand`, `AConv`,
`ACheck`, `MakeSymTab` admit it), the typed foreign tasks `$SVA`,
`$SVAssume`, `$SVACover` carry it to the rule, and `AVerilogUtil` turns it
into `VSva`, which `Verilog.hs` prints by substitution. `ASVA.checkSVA` runs
after `AConv`: label is a Verilog identifier (G0134), unique in the module
(G0135), boolean under `-sva-immediate` (G0136), and `$SVA` never reaches
Bluesim (S0100). Adding an operator is a line in `SVAssert.bs`.

## Verification

- `testsuite/bsc.assertions/svassert`: the printed form of 30 statements
  covering every operator family, the four diagnostics, and the BSV
  pass-through.
- Every statement's output parses under slang (pyslang 11, IEEE 1800-2023)
  with no errors.
- Verilator 5.048 `--assert` lints the output except for unbounded
  repetition and `intersect`, which it does not implement; on the arbiter
  it catches the injected failures of `|->`, `|=>`, `##n` and `##[m:n]`.
- Not verified: Jasper itself; the `examples/SVAssert/Arb.bs` output is the
  test case for whoever has the seat.

## Not here

The FSM lowering of properties to Bluespec checkers (`SVA.bsv`, and the
earlier #237/#238 experiments) is not used: Bluesim does not check SVA.
Running assertions in emulation would want it back, at which point the
checkers need a token-automaton rewrite and a differential test against a
simulator's native engine. Not provided: the `case` property, `$sampled` and the global-clocking
`$past_gclk` family, `restrict`/`expect`, sequence local variables and
match items, multiclock (`clockedBy`/`disableIff`), and a text front end
for pasting SV properties.
