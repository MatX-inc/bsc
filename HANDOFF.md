# Handoff: ATF cache → materialized rewrite rules

This branch is the PR branch (`claude/itype-rnf-type-var-cache-sddtz9`)
plus this document.  This file is a working note for continuing the
work locally — it is not meant to merge.

## Current state

Three commits on top of upstream `B-Lang-org/bsc` main (`941eecfe`):

1. `5347d1fe` — `src/comp/ATFRules.hs` (pure ground evaluator over
   instance equations) + consumer rewiring: `fullTypeNormalizer`
   reduces armed applications via the evaluator (ground miss =
   `internalError`, never a solver call); `IATFCache` threading
   removed from `iExpand`/`genModule`/`runG`; `mergeATFCache` growth
   in `cExprToIExpr` deleted; `-trace-atf-cache[-miss]` replaced by
   `-trace-atf-rules`; cache pin tests converted to rules pin tests
   (`ATFElabReduce`, `ATFElabImportDef/Use`).
2. `b57cecf6` — cache production pipeline removed end to end:
   `recordATFResult` / `sat`'s `recordATFs`, `CATFCache` /
   `tiATFCache`, the `ipkg_atf_cache` field of `IPackage` (now a
   4-field constructor), `iConvPackage`'s conversion, `GenBin`
   serialization.  `.bo` format change; invalidated by version hash.
3. `a40e5170` — `iConvT` = `expandSyn` + `atfReduceInType` (pure);
   the `expandSynN` (`runTI`) round trip survives only for a type
   still carrying a variable-dependent ATF application, believed
   unreachable (typechecking already normalized it) — flagged for an
   assertion.

PR staged on the fork (integration has read-only access to
B-Lang-org): https://github.com/nanavati/bsc/pull/12
Open the upstream PR from:
https://github.com/B-Lang-org/bsc/compare/main...nanavati:bsc:claude/itype-rnf-type-var-cache-sddtz9

Validation performed: `-Wall`-clean build of bsc + bluetcl; full clean
world rebuild (all libraries) after each commit stage; testsuite
`bsc.typechecker/typeclasses` 107/107, `bsc.lib/Prelude` 79/79
(includes TupleSize), `bsc.binary` pass.  Wider testsuite dirs have
NOT been run — CI or a broad local run is the next validation step.

## Design record (why the code is shaped this way)

The cache captured *point observations* of instance equations at
whatever types `sat` happened to discharge them.  Enumerated at the
single writer (`recordATFs`, called only on coherent full instance
discharges): open-keyed captures (polymorphic and rigid-variable
discharges) were never readable — every consumer (`fullTypeNormalizer`
here, via `canNorm`) is ground-key gated — and ground captures are
re-derivable from the instances.  Incoherent discharges were already
never recorded.  So the cache's entire reachable content was a memo of
the instance equations, and the equations become the semantics.

Core invariant: **the judgment boundary is the persistence boundary.**
Phases whose outputs persist (IConv / ISimplify / IExpand) may only
*evaluate*: ground reduction over a fixed equation set — matching is
decisive, contexts resolve recursively at ground types.  Judgment
(scope-relative reduction, incoherent commitment, improvement) is the
typechecker's monopoly and is consumed at generalization into pinned
signature results; it never leaks into anything keyed by a type.
`ISyntaxCheck` may use judgment because its verdicts are ephemeral —
currently it doesn't need to (parity holds ground-only).

Scope-relative ("parametric") reduction is mechanically implemented in
the typechecker by `bound_tyvars` acting as rigid atoms in
`mgu`/`matchTop` (see `sat` in `TCMisc.hs`): `TupleSize (a,b) = 2`
inside a polymorphic scope is a *coherent* decisive match because the
pair instance cannot match a rigid `b`.  It is a fact about the
binder, not the type — the nested-pair encoding erases arity, so arity
is a property of (type, quantification scope).  Such facts escape only
as pinned fundep outputs in signatures, which is why they must never
be stored keyed by type.  For a full write-up of the semantics
(extensional vs parametric, closed families, apartness, the sealing
requirement, the monotone rule-log framing) see the session log —
key conclusions:

- `TupleSize`-style families are closed families in disguise: ordered
  overlapping equations, sealed in one package.  Ground reduction over
  a sealed set is deterministic; the "incoherent" label applies only
  to eager commitment against *meta* variables.
- The durable declaration-time check (NOT yet implemented): an
  ATF-carrying class and the fundep-helper cone its instances
  reference must be **sealed** (all equations in the home package), or
  non-overlapping-and-open.  Registration order: rules are complete
  before anything can arm (arming requires substitution, which happens
  at/after the proviso-discharge site, which is a loaded package).
- The rule table is a monotone log of complete families, unlike a
  symbol table (a scoped view).  Per-`.bo` "local rules" are just the
  package's own instance declarations — nothing new is serialized.

## ATFRules.hs implementation notes

- IType-native on purpose: matching binds shared subtrees by
  reference, so reduction cost is O(rule), not O(argument) — required
  for the interning work stacked on top (a CType round trip would
  unroll shared DAGs; an earlier CType draft was discarded for exactly
  this).
- Instance commitment: computes *the unique most-specific matching
  instance* (pairwise generalization test via one-way matching with
  subject variables as atoms) rather than mirroring `genInsts` trie
  order.  Equivalence argument: on ground arguments could-unify equals
  does-match, so `reducePred`'s first-match in most-specific-first
  order IS the unique most-specific match; ambiguity (incomparable
  co-matches) means an unsealed/incoherent family and returns Nothing
  → loud `internalError` at the caller.  Note trie-mode `getInsts` is
  NOT globally specificity-sorted (only trie leaves are; see
  `MakeSymTab.getCls` / `sortLeaf`), which is why the evaluator does
  not depend on list order at all.
- Context solving (`solveCtx`): fixpoint over instance-context preds;
  a pred is solvable when some fundep's inputs are ground; solving
  binds output-pattern variables (handles `TupleSize'` helper-class
  recursion and `Bits a sa` contexts whose `sa` feeds the result).
  Preds that never become solvable are ignored iff they bind nothing
  the result needs (dictionary obligations are not re-verified — same
  stance as the old cache-hit path).
- Known restrictions: structural head matching only (no numeric
  back-solving à la mgu's deferred equalities — no current family
  needs it); partially applied ATF constructors are inert; fuel guard
  (4096) turns non-terminating families into Nothing → internalError.

## Follow-ups (roughly in order)

1. Run the wider testsuite / upstream CI on the PR.
2. Assertion that `iConvT`'s `expandSynN` fallback is unreachable
   (trace it, run the suite, then make it an internalError).
3. Sealing check at class/instance registration for ATF-carrying
   classes + their helper cone; error on out-of-home-package
   instances.  Also exempt ATF classes from the global
   allow-incoherent flag (the `fromMaybe ai (allowIncoherent c)` gate
   in `sat` would otherwise let a meta-variable commitment carry an
   ATF projection into a signature with residuals).
4. Generalization-time assertion: no ATF application whose pred was
   discharged parametrically survives unconsumed into a generalized
   type.
5. Optional perf: memo keyed by intern unique once interning lands;
   per-family head-constructor index if profiling shows linear scans.
6. Rebase the interning stack (`nanavati/itype-ftv-cache`,
   `itype-interning`, `claude/itype-rnf-perf-test-wa0355`) onto this;
   the endgame (ATF folding in `mkITAp` via a process-global rule
   table, same posture as the intern table) deletes
   `fullTypeNormalizer` and the ATF bit entirely.

## Environment / build notes (for a fresh machine)

- GHC 9.4.7 + alex + happy work.  Debian/Ubuntu packages:
  `ghc alex happy gperf flex bison tcl-dev pkg-config autoconf
  libghc-regex-compat-dev libghc-syb-dev libghc-old-time-dev
  libghc-split-dev`; plus `cabal v1-install strict-concurrency`
  (not packaged); `git submodule update --init --recursive` (yices).
- Build: `make install-src GHCJOBS=4` at repo root.  Compiler only:
  `make -C src/comp bsc bluetcl GHCJOBS=4`.
  Beware piped exit codes: `make ... | tail` hides failures — capture
  `$?` explicitly.
- Testsuite: needs `dejagnu time iverilog` and a generated
  `en_US.UTF-8` locale (`locale-gen en_US.UTF-8`; DejaGnu fails
  cryptically at config.guess without it; missing `/usr/bin/time`
  makes every test fail with empty output).  Targeted run:
  `cd testsuite/<dir> && make localcheck`.
- `.bo` format changed in commit 2: always rebuild libraries clean
  (`make -C src/Libraries clean`) when switching across the commit
  boundary.

## Key code waypoints

- `src/comp/ATFRules.hs` — the evaluator (this PR's centerpiece).
- `src/comp/TCMisc.hs` — `sat` (discharge paths, `bound_tyvars` as
  rigid atoms, the incoherent gate), `reducePred`/`byInst`/`matchTop`
  (candidate walk + could-unify incoherence marking), `expTFun`
  (the funnel: every TF app becomes a class pred), `expandSynN`
  (the old miss path: `runTI` per query).
- `src/comp/MakeSymTab.hs` — instance trie construction, `cmpQInsts`
  specificity, `sortLeaf` topo-sort, `assocTypes` extraction.
- `src/comp/IExpandUtils.hs` — `fullTypeNormalizer` (armed = fully
  applied + ground via `canNorm`; dormant otherwise).
- `testsuite/bsc.typechecker/typeclasses/` — the ATF battery and the
  new pins.
