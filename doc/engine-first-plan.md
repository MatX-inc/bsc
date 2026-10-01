# bsc orchestration and rebuild: implementation plan

Status: **Revision 3.1** — 2026-10-01. Plan of record; concise by design.
Revision 3.1 records the component structure as built (sixteen packages,
not four sublibraries), the P1 delivery, and the follow-ups P1 surfaced;
the phases are unchanged. Revision 3 superseded Revision 2 (the externally drafted plan document reviewed in the
2026-09-30 adversarial round). This document carries decisions, phases,
exit criteria, and the acceptance matrix. Rationale, measured economics,
and requirement derivations live in `doc/testsuite-after-shake.md` (v1.3),
`doc/RFC-bsc-artifact-graph.md` (v0.24), and the project knowledge-base
records (measured CI economics; the adversarial review trail).

## 1. Fixed decisions

- **Baseline: the B0 manifest.** `bsc.cabal` + `cabal.project` on
  matx-inc/bsc branch `release-devel-B0` (tip `9306c345`). Cabalization has
  already landed there: one `bsc` package (v2026.1) with a single
  ~251-module library, executables as thin clients under `src/comp/app`,
  `-O2` parity with the make build, `SetupHooks.hs`, three testsuite
  stanzas. The semantic port-property flow (`getIOPropsA`) and `-elab-only`
  are merged.
- **Engine: Shake**, used directly as the initial batch coordinator
  (scheduler, discovered dependencies, equality cutoff). Domain keys,
  artifact schemas, and result envelopes stay independent of its database
  format. No second generic engine is built and no cheap future core swap
  is promised; reopen conditions are as recorded in the review round.
- **Component structure: one cabal package per component**, sixteen at
  Revision 2.3 of `doc/recabalization-brief.md` (its Appendix A is the
  partition of record; §2 below is the original four-way sketch and its
  mapping). cabal-install builds a Hooks package as one unit and admits no
  sublibraries in it, so the fingerprint unit is the cabal package.
- **Legacy option: resolved.** `-semantic-ports-comment`
  (`src/comp/Flags.hs:173`, off by default) selects only the source of the
  Verilog "Ports:" comment. `.bo`/`.ba` port properties are unconditionally
  semantic (`src/comp/app/bsc.hs:1011-1023`). There is no conditional
  legacy `.bo` producer; Revision 2's legacy contingency and its P0
  investigation task are closed.
- **The library build is `.bo`-only.** The Base Makefiles invoke bsc with
  no backend flag; `.bo` is version-free and hash-chains its import
  closure. The `.ba` compatibility migration therefore gates
  backend-artifact reuse (P4), not the library win (P1–P3).
- **Staged release.** Library value ships twice before any backend reuse:
  P1+P2 (graph parallelism plus clean-output restore under a conservative
  whole-compiler key), then P3 (`.bo`-level selective reuse across compiler
  edits, with no artifact-format work).
- **Run everything, always.** The whole testsuite is requested at every
  milestone; pruning only by validated content identity; no path-based or
  predicted test selection. Periodic uncached sweeps are cache
  verification, not scoping, and bypass every cache layer.

## 2. The four sublibraries (sketch) and the packages as built

The four-way sketch below was the Revision 3 design; the carve was done as
packages (the brief's Revision 2 to 2.3): `bsc-core` is core plus the
leaves it shed (`bsc-stp`, `bsc-yices`, `htcl`, `waveforms`); the sketch's
`bsc-semantic` is `bsc-parse`, `bsc-typecheck`, `bsc-sat`, `bsc-elab`,
`bsc-schedule` with the codecs `bsc-bo` and `bsc-ba` as their own
components; `bsc-aopt` is the optimizer both backends run; `bsc-verilog`
and `bsc-bluesim` are as sketched; `bsc-bluetcl` holds the bluetcl glue.
The carve rule and the fingerprint definition below are unchanged.

| Sublibrary | Products | Owns |
| --- | --- | --- |
| `bsc-core` | none (supporting) | shared IR (CSyntax/ISyntax/ASyntax), binary codecs, `Flags`, positions and diagnostics, neutral utilities |
| `bsc-semantic` | `.bo` + `.ba` (coupled) | parse, imports, typecheck, elaboration (`iExpand`), scheduling, semantic port properties (`getIOPropsA`), wrapper finalization, `.bo`/`.ba` serialization |
| `bsc-verilog` | `.v` and associated files | `.ba` reading for emission, Verilog lowering, optimization, naming, rendering, requested filters, netlist port measurement (`getIOProps`) |
| `bsc-bluesim` | `.cxx` + `.h` (co-products) | hierarchy loading, simulation lowering, Bluesim scheduling, BDPI declarations, rendering |

Executables remain thin clients in `src/comp/app`, depending on all four.
`bsc-core` may split further later; start with one. `VIOProps.hs`
splits: `getIOPropsA` and shared helpers into `bsc-semantic`/`bsc-core`;
`getIOProps` into `bsc-verilog`.

**Fingerprints.** Generated per component at build time from the cabal
component graph:

```text
F(component) = H( fingerprint schema + generator identity,
                  the component's actual source/generated/CPP inputs,
                  its resolved per-component recipe + toolchain,
                  F(each dependency component) )
```

Never a git revision or a GHC ABI hash. `bsc-core` has a fingerprint
that enters each producer's `F` recursively but keys no action of its own.
The registry of fingerprints is generated during the build and embedded in
that exact compiler; producer modules do not import the registry (the
driver/adapter imports both). `BuildVersion` presentation stays out of
`F`; version-affecting observations are explicit inputs or outputs. Action
and product identity follow the RFC: `action = H(kind, F(producer),
effective options, validated input observations)`; `product = H(schema,
content)`. Downstream consumes product identities, so equal regenerated
content cuts off.

**The carve rule (P3 exit criterion).** Producer stage orchestration moves
out of `app/bsc.hs` into the components: the `getIOPropsA` call site and
wrapper finalization into `bsc-semantic`; `genModuleVerilog` /
`genModuleC` sequencing into their producers. What remains in the
executable is argument parsing and dispatch, thin enough that its
fingerprint is genuinely an execution-protocol fingerprint. Measure: a
driver-only edit invalidates no producer fingerprint.

## 3. Phases

Numbering is fresh: P0 absorbs Revision 2's P1 (Cabal landed at B0);
P1/P2 split Revision 2's P2; P3–P5 correspond to Revision 2's P3–P5.

### P0 — Pin and measure

**Deliver:** the pinned baseline (`release-devel-B0` @ `9306c345`) as the
working base; a reproducible benchmark command; an output/option
inventory.

- Measure separately: compiler build, library build, native/runtime build,
  install. Record wall, CPU, peak memory, GHC recompilation counts,
  package compilations, critical path.
- Scenarios: cold; unchanged incremental; clean outputs with retained
  cache; compiler leaf edit; frontend edit; evaluator/scheduler edit;
  Verilog-only edit; Bluesim-only edit; library source edit.
- Residual packaging checks (Revision 2's P1, mostly landed at B0): which
  auxiliary tools and vendor C/C++ builds remain make-built;
  object-directory hygiene; install parity. The make build is preserved as
  the comparison oracle.

**Exit:** another developer reproduces the baseline; every later saving
has a named stage and counter.

### P1 — The library as one graph

**Deliver:** a Shake coordinator scheduling Base1/Base2/Base3 as one
package graph, fed by a machine-readable discovery interface from bsc
itself (post-preprocessing imports, effective options, search-path
resolution including negative probes).

- One non-recursive compiler worker per package action; the coordinator
  owns scheduling. Staged outputs published atomically; a unique producer
  per output; concurrency bounded by CPU and measured memory.
- Preserve Prelude bootstrap, `Contexts.defines`, bloogle, `tconcheck`,
  and install behavior.

**Exit:** one-worker and N-worker builds produce equivalent installations;
a newly shadowing search-path file changes resolution correctly; no output
races.

### P2 — Persistent restore under a conservative key

**Deliver:** a persistent action/result store outside `build/` and
`inst/`, keyed by the whole-compiler execution identity (executable,
loaded dependencies, configuration) until selective fingerprints exist.

- Each result stores the observed input certificate, output inventory,
  required attributes, diagnostics, and termination status. Atomic
  publication; a missing or corrupt blob is a safe miss. Explain mode
  reports the first invalidating dependency.
- Recursive cache-bypass wiring is present from the start: verification
  and performance/staleness populations bypass every layer.

**Exit:** a warm rebuild into empty outputs restores every eligible
library result with no compiler work; corruption, interruption,
concurrency, and relocation fail safe. **P1+P2 is the first shippable
library win.**

### P3 — The carve and selective fingerprints

**Deliver:** the component restructuring (done, as packages: brief
Revisions 2 to 2.3), the build-time fingerprint generator, and the
embedded registry. P1's `--compiler-key closure` is the hand-written
preview of what the generator produces.

- Move stage orchestration per the carve rule; split `VIOProps.hs`;
  isolate `BuildVersion` from `F`.
- `.bo`-level selective reuse across compiler edits ships here, with no
  artifact-format work.

**Exit** (an invalidation report demonstrates all of): a Verilog-only edit
moves only `F(bsc-verilog)`; a Bluesim-only edit only `F(bsc-bluesim)`;
evaluator/typechecker edits move `F(bsc-semantic)`; a `bsc-core` edit
moves all dependents; a driver-only edit moves no producer fingerprint; an
unchanged regenerated artifact stops downstream invalidation. The whole
suite passes.

### P4 — Backend reuse and the `.ba` envelope

**Deliver:** replayable `.v` and `.cxx`/`.h` producer actions (via the
`-elab-only` path), native compile/link actions with their own toolchain
identity, and the `.ba` compatibility policy.

- The blocker: `.ba` embeds `ab_version` (which carries the compiler git
  hash) and the loader hard-rejects mismatches
  (`src/comp/ABinUtil.hs:511-516`). With per-component fingerprints, a
  Verilog-only edit either selects a cached old-version `.ba` that the new
  loader rejects, or re-runs the semantic action and breaks cutoff on
  trivially different bytes. Resolution: an envelope/payload split (digest
  the canonical payload; rematerialize and validate the envelope on
  restore), or a documented conservative cross-version miss with repriced
  savings. Never silently weaken the loader check; exact-version rejection
  stays until the replacement is implemented and tested (the N-to-N+1
  scenario, plus rejection of genuinely incompatible input).

**Exit:** a Verilog-only edit reuses semantic products across builds; a
native toolchain change rebuilds native outputs without rerunning
generators; incompatible artifacts are rejected identically with and
without cache hits.

### P5 — Behind `bsc -u`; the testsuite as second consumer

**Deliver:** the proven coordinator behind `bsc -u` (then direct entry
points), with the old recursive walker retired after parity.

- The un-migrated testsuite consumes the engine: zero `.exp` edits plus
  bypass wiring for never-memoize populations; uncached audits bypass
  every layer with isolated outputs.
- The whole suite runs at every behavioral milestone against the uncached
  oracle.

**Exit:** ordinary development uses the new path; suite outcomes match the
uncached comparison; the invocation-cache wrapper contingency is formally
closed, or invoked per its recorded trigger.

## 4. Acceptance matrix

| Experiment | Required result |
| --- | --- |
| Unchanged incremental build | No Haskell code generation or library compilation |
| Empty outputs with valid local cache | Eligible library products restored correctly |
| Backend-emission-only edit | Only that backend's producer fingerprint moves; unrelated library identities stable |
| Typechecker or evaluator edit | Affected producers run even with unchanged input files |
| Driver-only edit (argument parsing, help text) | No producer fingerprint invalidated (post-P3) |
| Recomputed output is identical | Downstream content consumers stay cached |
| Library leaf or Prelude edit | Validated dependency propagation only; output equality stops propagation |
| `-semantic-ports-comment` toggle | Only the `.v` comment observation changes; `.bo`/`.ba` identities stable |
| GHC, solver, build-option or shared-library change | Compiler and execution fingerprints invalidate conservatively |
| Search path, include, environment or foreign input change | Recorded observations detect the change before reuse |
| Different checkout path | Correct paths and diagnostics under the declared relocation policy |
| Parallel execution or interrupted publication | Complete deterministic products; no partial result accepted |
| Incompatible cached artifact | Same rejection or safe recomputation as the uncached policy |
| Whole testsuite | All checks requested; resource/staleness cases bypass nested caches |

The legacy rows of Revision 2 collapse to the single
`-semantic-ports-comment` row (the option is comment-only); the
driver-only row is new.

## 5. Adopted review requirements (normative)

From the 2026-09-30 adversarial round; full statements in RFC v0.24
§3/§12 and the knowledge-base review record.

1. Non-cacheability propagates downward: verdict reuse and recursive
   action reuse are independently controlled; test intent travels ahead of
   the invocation; demonstrated with a seeded resource regression and a
   warmed staleness test.
2. A versioned observable-result envelope: the merged stdout/stderr
   transcript contract, side files and effects, destination preconditions,
   termination classification, atomic publication.
3. Manifests carry negative resolutions; a pre-execution hit validates the
   full dependency certificate; foreign-tool boundaries are traced or the
   action is ineligible.
4. `.ba` envelope/payload discipline as in P4.
5. Shake integration cautions: resources cannot enclose dependency
   requests; concurrent coordinators need separate materialization roots
   or exclusive locking; the worker manifest must match the requested
   fingerprint; stable logical requests observe the current producer
   identity on restart; diagnostics keep their own observable projection.
6. Economics stay labeled modeled versus measured; audit cadence is a
   priced parameter.

## 6. References

- Baseline: matx-inc/bsc `release-devel-B0` @ `9306c345` (`bsc.cabal`,
  `cabal.project`).
- `doc/testsuite-after-shake.md` v1.3 — sequencing, measured economics,
  the wrapper contingency and its trigger.
- `doc/RFC-bsc-artifact-graph.md` v0.24 — identity layer, engine
  requirements, manifest and envelope contracts.
- Knowledge-base records: measured CI economics; the adversarial review
  trail (Revisions 1–2 and their corrections); this plan's full-text
  mirror.
- Evidence anchors: `src/comp/Flags.hs:173`;
  `src/comp/app/bsc.hs:1011-1023, 1261-1266, 1626-1628`;
  `src/comp/ABinUtil.hs:511-516`; `src/Libraries/Base*/Makefile`.

## P1 delivered: the libraries as one Shake graph (2026-10-01)

Package `engine/` (bsc-engine, a Hooks package like the components, so it
gets Warmup and the project's determinism settings; depends on shake
0.19.9, which builds with GHC 9.14.1 from Hackage with no overrides).
`bsc-engine libraries [targets]` reproduces src/Libraries' Makefiles:
one discovery per library directory (bscdeps on each root: packages with
their resolution, imports, includes, foreign imports and the probe paths
that lost), one non-recursive bsc worker per package depending on exactly
the .bo files of its imports, the Prelude bootstrap (-no-use-prelude for
Prelude.bs and PreludeBSV.bsv), the Base3 roots and their flags, the
Contexts.defines copy, tconcheck, bo2bloogle and the install of
everything in BUILDDIR. Every output has one producer: a package defined
in two directories fails the build where `-u` would silently recompile it
from the later source into the shared directory; a binary resolution no
earlier directory produces (a stale build directory) fails likewise.

Exit criteria, measured with the make-built compiler and tools:

| criterion | result |
| --- | --- |
| one-worker and N-worker builds produce equivalent installations | identical, 131 of 131 files and the bloogle text; N workers 24 s, one worker 46 s |
| no output races | unique-producer check; the two-directory case (HList in Base3-Misc and Base3-Contexts) fails with the two names |
| a newly shadowing search-path file changes resolution | the probe paths are tracked (doesFileExist), so the plan re-runs; a same-directory .bsv beside a .bs is reported by `plan`; a cross-directory shadow is the two-producer failure above |
| unchanged rebuild | 0.2 s, no bsc or bscdeps run |
| one library source edit | only that package and its dependents recompile (Counter.bs: 1 package) |
| against the make build (same compiler) | Base1 and Base2 byte-identical; the Base3 packages differ, see the finding below |

Findings:

- **.bo bytes depend on batching.** make compiles Base3 with `bsc -u` in
  one process; the engine compiles each package in its own process, as
  make itself does for Base1 and Base2 (which match byte for byte). The
  32 Base3 files differ only in typeclass-dictionary names (`_tcdictN`)
  and the hashes over them. The names are positional: `newDict` and
  `newVar` draw from the typechecker's per-package counter `tsNextTyVar`
  (TIMonad.hs; reset to 1000 by `runTI`), which type variables,
  temporaries and dictionaries share, so a dictionary's name is the
  number of fresh things minted before it in that package's typecheck.
  Under `-u` the package is typechecked against in-memory imports rather
  than their .bo files and the consumption differs, non-uniformly (CBus's
  23 names shift by 0 to 13, SquareRoot's by -3000 to +1894,
  ModuleContext's not at all), so .bo bytes depend on how the package was
  reached. Per-package compilation is the deterministic choice and the
  one the engine uses; naming dictionaries from the definition they
  belong to (and their position within it) instead of a package-wide
  counter is the typechecker change that would make a .bo's bytes for one
  definition independent of its neighbours and of batching (content
  cutoff and an in-process worker both need it).
- **A selective compiler key works at P1.** `--compiler-key closure` keys
  the library actions on the object files of the .bo producer's closure
  (bsc-core, bsc-stp, bsc-yices, bsc-sat, bsc-bo, bsc-ba, bsc-parse,
  bsc-typecheck) plus the driver's own, read from the dist-newstyle tree
  the compiler was built in, instead of the bsc binary. With object
  determinism those objects are byte-identical across a backend edit:
  a backend-only edit (VVerilogDollar.hs) costs a 3.2 s compiler rebuild and a 0.20 s library check with no package recompiled; a typechecker edit whose object is unchanged (an unused binding dropped at -O2) also recompiles nothing; a real typechecker change (an error-message string) re-keys all 128 packages (19 s); after a revert the compiler objects are byte-identical again and a change-based run still recompiles once, which is what P2's content-addressed store removes. This previews P3; it is written down rather than
  generated from the component graph, which is why it is not the default.
- **Two orchestrators in one tree corrupt it.** A `cabal build` left
  running by a dying session overlapped a later one and rewrote objects
  under it (rename races on .dyn_o.tmp, a core object with a hash no clean
  build produces). The engine behaved correctly (a core object changed, so
  everything re-keyed) but the experiment had to be redone on a clean
  tree. Plan requirement 5 (concurrent coordinators need separate
  materialization roots or exclusive locking) applies to the compiler
  build as much as to the engine.

What P1 does not do: TAGS (btags), the `depends` targets (bluetcl
makedepend; discovery replaces them), and anything about designs or
`bsc -u` (P5).

Follow-ups P1 surfaced, in the plan's terms: atomic publication of .bo
outputs (bsc writes them in place; the engine must stage and rename:
requirement 2); bloogle's output off the install prefix until install;
BuildVersion out of the closure key (P3); definition-local dictionary
naming (cutoff); the .bo import hash chain hashes the import's bytes, so
any byte change in an import re-keys every dependent under a content-keyed
system, while cross-package inlining (ISimplify) means the right boundary
is the exported interface plus the unfoldings exposed for inlining, not
the interface alone (a later design item); `-u` with several roots. Two
acceptance rows adopted from the 2026-10-01 engine-selection review:
restart after deleting the engine database restores from artifacts; an
interrupted publication exposes no partial result as complete.
