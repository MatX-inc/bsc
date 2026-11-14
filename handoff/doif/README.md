# doIf equal-arm shortcut: handoff (2026-11-14)

This branch is a handoff, not a PR. It is based on MatX-inc/bsc `main`
(99677cb33). The first commit is the prototype fix.

## The bug on main

In `IExpand.doIf`, an `if` whose condition is dynamic has a shortcut that
fires when the evaluated then arm equals the else expression:

    if thn' == els then return (P pthn thn')

That returns the then arm's implicit condition alone. It drops the else arm's
condition, and it does not combine the two per branch (`pIf`). As a result, a
rule's guard depends on whether the two arms happen to evaluate to the same
value:

    v = (r[0] == 1) ? when(c, 8'd5) : 8'd5   -- guard: c
    v = (r[0] == 1) ? when(c, 8'd5) : 8'd6   -- guard: !r[0] || c

This is the same with and without `-aggressive-conditions`. See
`probes/IfSame.bsv`, and main's output in `main-out/sysIfSame.v`, lines 90-92.

The result guards more than it should, never less, because the then arm's
condition is applied unconditionally. When both arms carry a condition but
differ as expressions, the general path is taken and the result is right
(`probes/IfSame2.bsv`, which gives `r[0] ? c : d`).

The first shortcut, `if thn == els then eval1 thn`, for arms that are equal
before evaluation, also returns only one arm's condition. Whether it should
combine the conditions too is still to decide.

## The prototype

The first commit evaluates the else arm in the `thn' == els` case and returns
`pIf ecnd pthn pels`.

A survey of whole-ActionValue conditions ran the full suites on prototypes
that include this fix. The fix on its own changes no existing test. It also
removed the failures (sal and lambda_calculus MergeIf, `mcd_Rand`) that a
prototype which gives `()` values a condition hit through this shortcut.

## Not done

- A testsuite test from `IfSame` (Verilog and Bluesim, with and without
  `-aggressive-conditions`) that fails on main.
- A decision on the first shortcut (`thn == els` before evaluation).
- Check whether `pIf` follows `-aggressive-conditions`. The survey found that
  `getBuriedPreds` always uses `pIf`.
- Full suites, the PR text (not mirror-safe: guards change in designs that hit
  the shortcut), and a review.

The survey's full write-up stays on ravi-pvm:
`/home/ravi/claude-tmp/claude-1548697660/-home-ravi/6640a2f0-3ba9-46af-8f63-2f02110094cd/scratchpad/isimp/work/avwhole/answer.md`.
The probe outputs for each prototype are in `avwhole/verify-0/probe/r/` there.
