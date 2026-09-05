# Proposed compiler witnesses (unexecuted)

`DynamicConditionalCalls.bsv` is a deterministic initialization of the
scheduling shape in pinned BSC `SchedCondsDynamicSchedule.bsv`, with public
state observation and bounded termination. The added sample method/rules
have not yet been checked for preserving that scheduling diagnostic.
Compiler acceptance and exact output are not claimed. If minimization is
needed, preserve the two unconditional parent rules and their conditional
opposite child calls; compare the original seed as a compile-only control.

Use a compiler built from `a9462e0e28102f07a89b3f3c478b33c806d62a3e`.
The source checkout alone is not a compiler-version assertion. Run each
backend in a different newly created temporary directory; retain every
compiler, link, run stdout/stderr and return code. With `BSC` pointing to
that compiler, the intended primary-reference commands are:

```sh
timeout 120 "$BSC" -verilog -sched-conditions -u -g sysDynamicConditionalCalls DynamicConditionalCalls.bsv
timeout 120 "$BSC" -verilog -vsim iverilog -e sysDynamicConditionalCalls -o reference.exe
timeout 30 ./reference.exe
```

An independent simulator means Icarus here, not the eventual Verilator
module executor. Capture its version. Preserve raw output: do not remove
`$finish` effects, sort effect lines, filter startup observations, or create
an expected trace from the toy Python model. With the deterministic
register initialization, initial reset/clock scheduling still needs to be
recorded. The primary assertion is accepted Verilog with a dynamic
scheduling diagnostic plus settled per-edge state observations.

As a separate **coverage discriminator**, record the result of:

```sh
timeout 120 "$BSC" -sim -sched-conditions -sched-dynamic -u -g sysDynamicConditionalCalls DynamicConditionalCalls.bsv
```

The expected coverage distinction is source-inferred: both parent
CAN_FIREs are true, so `mkDynSched`'s `areRulesDisjoint` condition cannot
encode the dynamic pair. A simulator failure is not a P0 expected rejection;
it identifies a required case not supplied by this legacy export path.
This is not a request to extend the target beyond BSC's Verilog backend.

The pinned original's `sched-conditions.exp` explicitly records Verilog
compile success with G0100 under `-sched-conditions`. Its flag-off control
expects compile success with G0010 and no G0100. Run those original checks
alongside this new observation fixture; they distinguish compiler scheduling
mode from an accidental change caused by fixture instrumentation.

The baseline RTL run and the new hierarchical engine must never exchange
global schedule graphs, merged BIR, or ordering-derived inhibitors.
Further obligations include guarded pair/self cases, method callers,
wire-dependent conditions and independent instances; they remain explicit
inventory items, not inferred passes from this one witness.
