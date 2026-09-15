# Line-12 reorg — the trs/FST re-cut of claude/trs-fst-rebased

Date: 2026-08-05.  Source: matx `claude/trs-fst-rebased` @ ad8a4d06
(487 commits over upstream main 941eecfe; see REBASE-LOG.md at that
tip for the rebase itself).  This log records how the reorg re-cut
that branch into the Line-12 single-topic PR series, and disposes of
every commit that did not travel.

## The series

| branch | base | commits | PR |
|---|---|---|---|
| single/integer-verilog-params | upstream-main | 1 | #104 |
| single/bluetcl-wiretypemap | upstream-main | 6 | #105 |
| single/eqptrs-array-cells | upstream-main | 1 | #106 |
| single/fundep-improvements | upstream-main | 16 | #107 |
| trs/1-bir | upstream-main | 15 | #108 |
| trs/2-engine | trs/1 | 9 | #109 |
| trs/3-backend | trs/2 | 2 | #110 |
| bluesim/fst | verilator/4-dump-formats | 5 | #111 |
| trs/4-fst | trs/3 + join(bluesim/fst) | join+3 | #112 |
| trs/5-sweep | trs/4 | 5 (incl. this log) | #113 |

## Base decisions

- **trs/1 on plain upstream-main**, not staged-flow/6: the exporter
  compiles there (tupleElemRange/argInputPorts/cvtActions/
  mkAVMethTmpId/isOkId/ssys_instmap all present upstream); verified by
  the head gate.  Smaller is better.
- **bluesim/fst on verilator/4-dump-formats** (the FST work's own
  handoff records its rebase onto the -dump-formats PR).
- **trs/4 is the stack join**: bluesim/fst merges into the trs stack;
  its PR diff shows the merged line (review the join resolution only).

## Tags (.ba/.bo)

Format-changing commits and their tags, binder counts re-verified
mechanically (count == Flags record fields) at each affected head:

- trs/1 "Wire BIR export into bsc" — +genBir, 135 binders,
  `bsc-ba-20260805-4` (the original commit's own value).
- trs/3 "Add the -trs backend flag" — +genTrs, 136 binders,
  `bsc-ba-20260805-5` (the source branch's final value).
- trs/4 join — unions the verilator line's dumpFormats binder
  (its side carried `bsc-ba-20260715-1`): 137 binders, fresh
  `bsc-ba-20260805-6`.
- **No .bo bump**: the only .bo format changes on the source branch
  (ICForeign fTyVarNames from named-instance-params; the PrimOp enum
  additions from pack/unpack coercions) belong to content this series
  does not carry, so GenBin stays at its base tag.  The source
  branch's `bsc-bo-20260805-1` retires with it.
- The source branch's `bsc-ba-20260805-1..-3` belonged to the
  staged-flow-era format marches (dropped as carve duplicates); the
  branch's final `-5` denotes a 138-field format that no longer
  exists — the rebased branch is superseded by this series, so no
  live collision.  Next free in the 20260805 series: -7.

## Dropped-content ledger

**A. Carve duplicates (live in the named MatX PR lines; dropped here):**
- staged-flow (#48-#53): -sim-codegen-only (dfc5c5797, b2780367a),
  -block-codegen rename + submodule form + byte-identity docs/tests
  (095edf15c, b3302c890, d2b282670, ddce40fe6, 2fa7c8511, 9adf0a621,
  561f8ea77, cd7a1ef7f, 72137f5e3), -c codegen (ebcae03f8, ab0569a93,
  ae2dc3d64, c74ab93d6-era duplicates 8d0611536-adjacent), .ba-by-default
  (2384db279 + its 265-file regold), unusable-.ba-stale (4ad88ec68),
  dead Verilog-in-.ba removal (fa063edc2), link-time regen (e0af945a6,
  57f01fa24, a87eea94f, 216ea91f5-dupe), -elab-only-era Depend/flag
  hunks, rc3 staged-flow goldens (a1d61931e: S0100, "Elaborated module
  file created"), and the staged-flow-coupled testsuite tendrils the
  guard batches carried (gen_mode.exp, vlink_regen.exp,
  block_codegen.exp, the NullCrossing/NoClock/inout .ba-reuse blocks,
  bsc_gen_modules/check_block_codegen_modules procs).
- verilator line (#41-#44): -system-verilog-output (98=135394...
  8cfeb942b-dupe), SV-safe identifiers + keyword table + G0129-G0133
  (5e16eb616, fc8b171f5, c8090976a, b44bfbeb6, b77197a6c, 3e6b3ac58,
  ea8fe255d), polymorphic BDPI DPI (60e70960d, ed90d77f9), -dump-formats
  (42210a952, 3c492cc24, af72b18d0, ecb44dcc6, ffcc4e9e2) — all arrive
  via the verilator/4 base of bluesim/fst.
- dicts line (#78/#925 base): the LiftDicts pass and its wake —
  1c1ab1e71, 795fff074, cc943a456, de6155c9d, 37a1aabd8, 22123c6fc
  (TypeCheck half), ca0e8d81d, 6b0c04fb1, 7eb76023f, 89ceb0920,
  7f5a67bea, 65308ab43, ae0e4d5f1, 7041bd3ac, 9bfa5aecd, 91b1cb0ba,
  d36708d1b, d21ec8464, 025a725a6, e91d3040b, 51de166f28-adjacent
  T0158 consolidation, DFliftdicts/DFisimpdicts dumps, coherent-dict
  map threading in bsc.hs/FixupDefs, testsuite regolds 668745009,
  f8c2532a4, aa38046ac, 7f838d6db.
- input-port-tuples v2 (#23's territory, needs its own port per the
  landscape): 42c5104db..dc948fd4a cluster (multi-output ports, ATuple
  refactor, PrimPair struct, SplitPorts instances/noinline, unified
  _PORT_ naming) and its testsuite regolds (507252c7f, f48eca84e,
  cdcb5c72b, 0f2c0d83f-part).

**B. Already upstream (echo residue; picking nets to zero):**
- eqPtrs foldl'/SCC.tsort rework pair (459f392db + 5baf52f1a).
- IRefT deepseq/position, heap-cell forcing era (1e3966d9e, a00faf2e0,
  3eb01f8ae, c287a666c-adjacent) — upstream squashes carry the final
  forms.
- ATF-cache tail echoes (4d48d9a74, f226a20ff residues), ITransform
  if-then-undef trio (85..., ff9f3d226, cd5ae2d83), tiExpl #890 cluster
  testsuite echoes (81b13ab42, 7577627a8, fb08e420a, 4f08f24fc,
  f3d1ca860), b302 canonical-name regolds (409d2bc41, 0c9c47b70).

**C. Parked (real content, no home in this series — awaits its own
carve or ruling):**
- pack/unpack coercion primitives (1875e4137, 783a2d88b, 2eb29c1a6,
  c1bced330 + bits_coercion tests + goldens b2ad916bf, 5f8845dc9,
  c42913b50, a02a7ca1d): changes the .bo format (PrimOp enum); has
  its own branch history (pack-unpack-prims-rc8).
- named instance parameters / -v95 removal (0d21679d3, 548342e0d) +
  ANoInlineFun [(String,Integer)] + ICForeign fTyVarNames (.bo format):
  the landscape routes this to its own single ("deliberate Verilog
  semantics").
- default_clock/default_reset argument attributes (6adcc2313,
  38235745f + SimExpand tendrils): own branch exists
  (claude/default-clock-reset-attrs-3d0s0z).
- BH syntax batch: unary negation (f4288d763), unbased unsized
  literals (b694ca9ba), '0 patterns (9db175c68); deriving-via
  (3655a7b86, e7160e26e, 0543848c8, b390d5a8c, 014fe773b).
- TOP_CXXFLAGS Bluesim model-file override (d017a5728): own PR branch
  exists (pr/bluesim-top-cxxflags).
- Verilator CI workflows (9110f3cb1, 15b7f1893, 4756f52c9): verilator
  line's concern (see verilator-ci-handoff).
- bluetcl fst_correlation twin (04b5926d9): needs #105 AND #111;
  travels with neither; add at integration.
- rc3 foreign-function error renumber T0162/T0163 (51b056eb1):
  belongs with the foreign-numeric-contexts line (own branch exists).
- inout one-port-per-net rendering (18d9966d8, fcc707a5b) if not
  already upstream via 8458d046 — netted to near-zero on this base.

**D. MatX-local (never upstream-intent):**
- bo2bloogle (2af56cd13, c69c38609, 972548aef) + Bloogle library
  target; matx-release/prerelease workflows (39e1df05b); Claude Code
  SessionStart hook (2db8a66bc); util/bluetcl-scripts cleanup
  (f10227754 — judged matx-local here).
- Committed .bir/.bdpi.so run artifacts and their later removals
  (1adc685e9, 23d3a4494's deletions, f51f52919's deletions, dump.txt,
  sysGatedClock_OneMod.bir): never committed in this series.

**E. Dead residue (dropped):**
- ForeignFunctions getForeignFunctions/DPIInst export additions — no
  consumer in the final tree.
- rc3 repair commits (5fe0cafac, f32dfcefd, bfba24f91, 2e8912ba1,
  1f3c74d95, e6074dcc5, 199=b411b634a tag commit, f132ec932
  convergence): folded into the owning re-cut commits or vacuous here.
- Committed-conflict-marker artifacts (bsc.hs import union in the
  coverage-warning pick; SolvedBinds orderBinds) — repaired in their
  introducing commits.

## The pair-audit verdicts

- **fundep TCMisc/TCheck pair vs #81**: independent, NOT subsumed —
  #81 is substitution-sharing performance; the pair is instance-
  selection semantics.  Cut as single/fundep-improvements (#107) with
  its whole coherent line (the pair cannot stand without the
  commitment/reporting machinery).  File overlap with #81 exists
  (TCMisc/TCheck/TIMonad/Pred); second lander rebases through
  mechanical conflicts.
- **eqPtrs**: upstream took the tie-order fix and (as a squash) the
  foldl'/SCC.tsort rework; the surviving remainder (array cells +
  linear collection + invariants comments) is single/eqptrs-array-cells
  (#106), byte-identical to the source branch's eqPtrs region.

## Verification summary

- `make -j16 GHCJOBS=8 install-src` exit 0 at every branch head
  (PR-opening gate).
- Byte-parity pins vs the source branch: SimExportIR.hs (trs/1),
  the ten fundep-owned typechecker files (#107), the six wave-tool
  sources (#111), eqPtrs region (#106), src/trs entire tree at
  trs/4+ (minus the trs/5 tools until trs/5).
- trs/5 tip additionally: cargo build + cargo test (jit), trs
  regress + vcd ladders, typeclasses + options localchecks — results
  recorded in the trs/5 PR.
