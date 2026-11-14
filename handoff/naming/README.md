# Naming quality: handoff (2026-11-14)

This branch is a handoff, not a PR. It is based on MatX-inc/bsc `main`
(99677cb33). The first commit is the uncommitted work tree of an agent that
was stopped mid-run (variant "v"), committed as it was, apart from one change:
the `.bo`/`.ba` tags are `20261114-17` rather than `20261114-1`, because PR
#354 uses `-1`. It builds (rc 0), but its suites were never run and its test
has no `.exp` yet.

## Decisions so far (Ravi)

- Rank of names for a heap cell or signal, best first:
  1. names derived from design anchors: state elements; rule names
     (`CAN_FIRE_RL_r`, `WILL_FIRE_RL_r`); `state_element$port` names; method
     ports (arguments, results, ready, enable); names produced under
     `-keep-method-conds`;
  2. user binders;
  3. other inferred names;
  4. binders written in a bsc library (`x__h`, `y__h`, `b__h`, ...): "lower
     priority than everything else but bad names";
  5. bad ids (`IdP_bad_name`, pDef's unnamed cells, which AExpand may inline).
- The naming-quality PR does the minimum: rank library binders below the
  other names, and nothing more. Everything else here is context for later work:
  - anchor names ranked above user binders;
  - no tie-break between anchors: keep both. For example, keep separate EN and
    RDY signals even when their expressions are the same;
  - keep the best name, and record the other names a signal had, with their
    positions. Keep that record minimal: lexical provenance and
    cross-references are separate work;
  - the rule implies `-keep-fires` and `-keep-inlined-boundaries` should be on
    by default.
- This PR and the library point-reduction PR are separate. That PR edits
  libraries and is stacked on the evaluator PR; this one leaves the libraries
  unchanged. Whichever lands second must re-measure names.
- Each PR sets its own unique `.bo`/`.ba` tag (assembly resolves collisions)
  and says that it is not mirror-safe. This one renames netlist signals.

## Mechanism

`IExpandUtils.improveCellName` already drops a cell name whose binder has a
library position, but it never fires. `FixupDefs.get` (`updateIExprPosition`
on each ICDef reference) and `ISimplify.iSimpAp'` (`mapIExprPosition`) restamp
an inlined library binder with the use site's position first.

The snapshot in the first commit:
- `ISyntaxXRef.moveBinder` marks a binder the user did not write with
  `IdPNonUserBinder` when it moves it to a use site. The property is
  serialised, hence the tag bumps.
- `Id.isUserWrittenId`, and `idQuality`, which ranks user-written names above
  other names within keep and non-bad Ids.
- `evalHeap` prefixes a struct cell's name to its fields only when the user
  wrote it.
- `testsuite/bsc.names/library_names/LibraryNames.bs` is a witness design:
  - a user name beats a library name;
  - a library name is kept when nothing else names the value;
  - fold keeps its tree.

  It has no `.exp` yet.

`patches/variant-w.diff` is the agent's earlier variant, which uses a
different rule for naming struct fields. `variant-w-instrumented.diff` adds the
naming measurement instrumentation (`patches/naming-measurement-instrumentation.diff`,
`scripts/naming.py`) to it.

## Results so far

Round 2, `patches/round2-drop-library-names.diff`, drops library names
outright:
- Signals named after library parameters in the testsuite go from 561
  (Verilog) and 643 (Bluesim) to 0 and 0. Library locals go from 106/142 to 0.
  Regs500 goes from 498 to 0, and a production MatX build from 74 to 0.
- Cells that lose their name also lose their `IdP_keep` boundary. On
  one module of a production MatX build, AOpt goes from 2.2 s to 23.6 s. A Vector fold over a Bit
  operator flattens into a chain (Genus builds identical gates from the tree
  and the chain).

Round 2, `patches/round2-keep-boundary.diff`, keeps the boundary without the
name:
- Fold trees are kept; that module costs +0.25%, and the whole production build +0.08%.
- Two bsc.mcd LevelFifo Bluesim tests (`sysGFIFOLevelTest`,
  `sysGFIFOCountTest`) swap the order of a FIFO warning. This is unexplained.

The rank approach in this branch keeps a library name, and with it the
boundary, when nothing better applies. It should therefore avoid both costs,
but this has not been measured.

A partial suite run of variant w (not v) gave unexpected failures in
bsc.bugs 2, misc 6, names 2, scheduler 27 and verilog 2. Every other suite
passes. All of these failures are expected outputs that contain names:
- schedule dumps (AVArgUse, SharedPortsFIFO, ...);
- the lambda_calculus and sal exports (sysMethod_Split, sysTb, sysExtract);
- regexps on Verilog names (`triple__f1_2`, `s$put_1_...`);
- bsc.names MethodActionValue and TaskValue.

None of them has been triaged.

## Not done

- Run the suites on v.
- Triage every expected-file change: a regold with a reason, or a bug.
- Write the witness test's `.exp` for Verilog and Bluesim.
- Measure the fixtures and the production build: allocation, MUT, AOpt time of the slow module,
  all netlists classified.
- Explain the LevelFifo ordering if it appears.
- Review.

## Stopped research (context for later work)

- `anchors/`: probes for an inventory of every naming source by provenance,
  and what `-keep-method-conds` names. It stopped before reporting.
- `anchor-roles/`: probes where two anchor roles coincide (EN equal to RDY,
  `CAN_FIRE` equal to `WILL_FIRE`, `WILL_FIRE` equal to `inst$EN` equal to
  `r$EN`, ...), compiled with and without the keep flags. It stopped before
  reporting.
- `keepflags/`: scripts that measure `-keep-fires -keep-inlined-boundaries`
  (and `-keep-method-conds`) on the suites. They
  stopped before results.

Full outputs stay on ravi-pvm under
`/home/ravi/claude-tmp/claude-1548697660/-home-ravi/6640a2f0-3ba9-46af-8f63-2f02110094cd/scratchpad/isimp/work/`
in the directories `ppr/ppr3b-names`, `ppr/ppr2-icn`, `anchors`,
`anchor-roles` and `keepflags`.
