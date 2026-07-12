# -c-phase / Bazel arc: measured dossier + staged plan (2026-07-11)

Provenance: 8-agent design workflow (4 fact-finders incl. live
measurement, 3 independent designers, adversarial judge).  Winning
skeleton FRAG/PLAN with grafts from STRATA (opening sequence, fallback
semantics) and Split-Spine (noinline A/B, plan-card stability rule,
TYPE_ABI_REV).  Raw agent outputs: session workflow wf_25c7dd90-654.

## The constraint (Ravi, 2026-07-11; sharpened 2026-07-12)

BOTH flows optimal.  (1) One-shot flow (bsc + trs direct; upstream
stays make-based) is first-class and must not regress — .birsnap and
in-link AOT emission stay legitimate there.  (2) Our downstream flow runs
entirely under Bazel: caching/incrementality/recompilation belong to
BAZEL, not the tools — no tool-internal persistent caches.  Extra
flags to expose finer-grained steps are fine and expected (that is
where -c came from).  Steps must be hermetic actions with declared
inputs/outputs and DETERMINISTIC outputs.  Do the right LOCAL work per
step; don't defer to link what can be done earlier.

SHARPENED (2026-07-12): NO Bazel rules in this repo — the deliverable
is FLAGS that make bsc behave in the most Bazel-friendly way.  BSC
ORCHESTRATES TRS, always: even under Bazel the actions are
`bsc -trs -c` per package and `bsc -trs -e` for the link (bsc shells
to trs, as trsLink already does).  The doctrine is therefore a
-c/-e SPLIT RULE: ALL module-specific work goes behind -c (.bo/.ba,
BIR fragment, per-module-type trs codegen via a bsc-invoked trs
subcommand — gated-variant objects, closure-keyed); ONLY truly
link-level work lives in -e (hierarchy/instance map, cross-module
schedule merge, compositions + .bir splice, edge-fn codegen, final
specialize+link, artifact packaging).  -trs-export-only is DROPPED
(no build-system seam between export and link exists; the only seam
is -c/-e).  trs subcommand CLIs are internal bsc<->trs plumbing;
bsc's flag surface is the contract, and every output-affecting trs
env knob must become a flag bsc can pass explicitly.  BAZEL DOES NOT
KNOW TRS EXISTS beyond shipping the binary in the toolchain
filegroup: actions invoke bsc only.  To make even that configuration-
free, trsLink's tool lookup becomes $TRS (explicit override) -> trs
NEXT TO THE RUNNING BSC EXECUTABLE -> PATH (the filegroup and
inst/bin both keep them adjacent), so no -trs-path flag is needed.

## Measured facts (grid v3 N=8 split Tile/Grid8; medians of 3; quiet box)

- Recompile loop after LEAF edit, trs path: bsc recompile 1.554s +
  .bir export 0.161s + trs full relink 0.651s = 2.37s.  bsc's own
  compile phase is ~66% of the loop and scales worst (frontend 2.2s at
  N=8 -> 511.7s at N=32 per PERF-BASELINE).
- BIR export is CHEAP: 0.165s grid8 / 0.042s counter (isolated via
  TRS=/bin/false bsc -trs -e).
- trs link breakdown (grid8, 0.76s wall): LLVM ir-passes ~0.40s (52%),
  backend emit ~0.18s (24%), residual decode/plan/init ~0.16s.  At
  N=32: 27.4s = ir 15.9 + backend 10.0.  trs link today has ZERO
  incrementality (temp objects deleted; .so gated on whole-file
  bir_hash).
- Reference object reuse (the mechanism being imitated) is nearly
  worthless on leaf edits: timestamp-keyed (SimFileUtils.hs:60-74) with
  child->parent invalidation (:76-102) -> 3/3 objects rebuilt; best
  case (top-only edit) saves ~25%.  trs's FULL relink (0.84s incl.
  export) already beats the reference's best incremental relink
  (2.74s) 3.3x at N=8.
- Parity held on every measurement pair.

## Architecture facts (file:line-verified)

- The three-stage flow already exists: elaborate (-g, .ba) -> codegen
  (-c) -> link (-e); per-module byte-identity of -c output is a
  maintained invariant (check_block_codegen_modules;
  DEVELOP.md:126-166).  The per-module SCHEDULE is complete at compile
  time, inside the .ba (aSchedule at bsc.hs:864-886; ABin.hs:42-64).
- The trs backend does nothing at -c today; -trs = Bluesim backend +
  genABin+genBir+genTrs (FlagsDecode.hs:1702-1709), consulted only at
  link (bsc.hs:1346, 1648, 1662).
- BIR modules array is instantiation-independent BY DESIGN; BIR.md:26-28
  explicitly reserves a per-module export for -c-style point codegen.
  encModule (SimExportIR.hs:647-745) reads only the module's own
  SimPackage.  content_hash is a zeroed P0 TODO (SimExportIR.hs:733).
- Link-only BIR residue: top, instance_map, compositions (design
  schedule per clock/edge via cross-module schedule merging,
  SimExpand.hs:932-1099,1398+), foreign_funcs; symMap is whole-design-
  computed today but module-intrinsic per the blockCodegen machinery
  (SimCOpt.hs:126-142).
- trs codegen unit is the WHOLE DESIGN in one LLVM module; per-TYPE
  sub-units already exist (outlined exec bodies, exec ABI
  lower.rs:3473-3495, base+token rebasing :1704-1721; helpers
  hlp_<sig>_<def>).  Blockers for true per-type precompilation:
  schedule-derived inputs (always_fire, eager union, inhibitors),
  instance-derived symbols (jit.rs:2531), baked absolute now/reset
  slots (lower.rs:1719-1720 fallthrough).
- LOAD-BEARING CORRECTION (judge-verified): parent exec lowering
  INLINES CHILD METHOD BODIES (lower.rs:2405-2427 value cones,
  :3797+ ActionValue, depth cap 32).  Any per-type cache key is the
  fragment CLOSURE, so leaf edits invalidate the ancestor chain's
  objects.  Win classes: sibling edits, top/spine edits, cross-design
  sharing, CI/remote no-op builds — NOT the edited leaf's ancestors.
  (Interface-only cutoff would require calling child methods by symbol
  instead of inlining — a codegen-shape change with run-perf
  consequences; NOT assumed by this plan.)
- Nondeterminism sites (block Bazel caching AND cause the memq
  2.4-9.9s O3 tail): jit.rs:2744-2745 en_slots HashMap flat_map;
  jit.rs:1805 consumers HashMap iteration (hoist order via :1881).

## Staged plan (gate between 2 and 3)

INCREMENT 0 — deterministic IR emission.  AS BUILT (2026-07-11) — the
judge's original spec (just sort the two HashMap iterations) was
WRONG in an instructive way: sorting the consumers iteration
regressed memq's link 0.79s -> 17.5s.  A 5-agent fleet falsified the
first theory (SimplifyCFG order-pathology) and found the real
mechanism: emission CONTENT was order-dependent, not just order —
bsc lowers chained folds (countOnes-style d_k = If(bit_k, d_{k+1}+1,
d_{k+1})) with the dep in both arms; lazy_mux_fn save/restores f.ssa
around arms, so a dep not already in edge.shared re-expands in BOTH
arms of every bit-test diamond = 2^k-1 copies (memq: k=16, 47MB IR,
197k blocks).  The old ~13% bimodal tail was random orders losing
the dep-before-user race; O3 time simply tracks emitted size (no
LLVM knob exists — all tested; the hot path
TryToSimplifyUncondBranchFromEmptyBlock has no cl::opt gate).
THE FIX (jit.rs): (a) en_slots sort (innocent, kept); (b) pinned
corder (first-consumer-section major, sorted per-section defs — the
order itself is arbitrary); (c) Kahn TOPO SORT of each section's
hoist prelude, deps before users, pinned-order tie-break — sharing
becomes order-independent in OUTCOME: every hoisted def finds its
deps in edge.shared.  Cone.defs already carried the transitive
closure (no analyzer change).  RESULT: memq 6,242 IR lines / 15
mt-triples — SMALLER than the best-ever random draw (8.8K/197);
grid8 byte-identical size to pre-fix (15,251 — size-neutral where
sharing already worked); byte-identical IR + .so across every run
on both designs.  GATES: determinism + IR-size DONE; ladders green
on v1 (re-run on final pending); wall-time + isolated diffsweep
pending an idle box.  Standing witness: testsuite/bsc.trs/
determinism (a proper DejaGnu testsuite dir, auto-discovered by
fullparallel; CountShare = the chained-fold shape, + HoistDivTrap;
3-link byte-compare of pre-O3 IR + artifact .so, golden stdout
compare; honors $TRS, UNSUPPORTED when trs is absent so upstream
runs are unaffected; verified to FAIL against the pre-fix binary).
It gates the INSTALLED trs — refresh inst/bin after trs changes.  LEARNINGS FOLDED INTO LATER INCREMENTS: (i) emitted-
IR size is the load-immune link-side fence metric (predicts O3
cost; validated across 20 specimens) — add per-design size to the
fence, and I1c's lap should record size per function group; (ii)
single-consumer chained cones (ps.len()<2, never hoisted) remain a
latent 2^k class — pre-existing, now fence-catchable; emitter-side
arm-sharing fix queued, gate-triggered; (iii) the I2 gate inputs
must be RE-MEASURED post-fix (corpus link times changed); (iv) I3's
content_hash inherits the lesson: bsc export byte-determinism is a
measured property — audit (freeze, N-emit, byte-diff, incl.
sandbox-relocated paths) is a hard I3 prerequisite, promoted into
I1's deliverables.  No bsc changes in I0 itself.

INCREMENT 1 — hermeticity flags + measurements at EXISTING
granularity.  (a) SUPERSEDED by the sharpened constraint:
-trs-export-only DROPPED; the bsc-side work is the -c/-e split
(post-rebase, NEXT-UP task list).
(b) DONE 2026-07-12: `trs link` grew the hermeticity flag surface —
--cc (TRS_CC, reaches both cc -shared sites via cc_tool()),
--edge-ssa/--aot-one-module/--jit-split/--jit-opt/--jit-pipeline/
--jit-threads/--outline/--outline-factor/--capi-lib/--no-fusion/
--jit-novec — flags win by writing the env spelling pre-planning
(single-threaded); bsc passes these through, build systems key on
argv.  Verified: flag==env .so bytes identical; no-flag baseline
byte-stable; --cc /bin/false fails the link (override reaches the
tool).  TMPDIR was ALREADY honored (std::env::temp_dir(),
jit.rs:888/958 — the plan item was stale).
(c) DONE 2026-07-12: TRS_JIT_TIME now prints `lowering <dur>` (t
from function entry) + `ir census <group>=<insn>insn/<bb>bb` per
function group (edge_/exec_/hlp_/sched_/other) + `ir top` (5 largest
fns) BEFORE t0, so ir-passes stays unpolluted.  First data point:
memq = ONE edge fn, 5,067 insns, lowering 5.2ms vs ir-passes 393ms —
the I3/I4 ceiling instrument is live.
(d) DROPPED (was: Bazel rules) — no rules in this repo; see the
sharpened constraint: Bazel wraps bsc invocations only.
(e) experiments that decide later increments — MEASURED 2026-07-12
(grid8 split + memq, installed bsc):
  1. SAME-DIR DETERMINISM: PASS — .bo/.ba byte-stable across
     repeated in-place recompiles with -no-show-timestamps
     -no-show-version (both flags LOAD-BEARING: raw runs differ).
     No run-to-run nondeterminism to fix.
  2. PATH SENSITIVITY: FAIL — absolute source paths embedded in the
     .bo header (offset ~21), in .ba after the version string, and
     as resolved dep paths in parent .bo dep records.  FIX EXISTS:
     PR #1040 (-remap-path-prefix FROM=TO, repeatable; Ravi,
     2026-07-12) remaps at serialization time — Positions at the
     BinData share chokepoint (canonical stream), abmi_path/
     abmi_src_name, and the .ba's stored Flags copy (bdir/search/
     output paths normalized; remapPathPrefix itself cleared).
     Pinned guarantees (testsuite/bsc.options/remap-path):
     byte-identical .bo/.ba across build dirs with
     -remap-path-prefix "$PWD=." incl. -u and -g; in-place
     recompiles byte-identical; NO-FLAG bytes unchanged from the
     previous compiler.  Its audit extends ours: the .bo cascade is
     importee positions on imported Ids + ipkg_depends content
     hash; the serialized Flags record also stores paths; NO
     timestamps exist in either format.  Arrives with the new
     release rebase (.ba tag advances for the new Flags field).
     Bazel action flag set: -no-show-timestamps -no-show-version
     -remap-path-prefix "$PWD=." (+ a toolchain-prefix mapping if
     the toolchain lives in the sandbox) + explicit -bdir/-simdir/
     -info-dir output placement.  (.bo format tag embeds the BSC
     BUILD DATE; .ba embeds the version string — toolchain
     identity, acceptable.)
  3. CUTOFF: FAIL, and DEEPER than a dep stamp — a one-constant
     LEAF BODY edit (same byte length) wholesale-renames and
     reorders the PARENT's own defs in sysGrid8.ba (12.8k dumpba
     diff lines; the __h<N>/__d<N> suffixes are elaboration heap
     positions, and the parent's evaluator heap counter is
     perturbed by walking the child's ISyntax even across the
     synthesize boundary).  Parent .bo additionally embeds
     impHashes = content hash over the import's ENTIRE .bo
     (bi_sig, bo_sig, IPackage ISyntax — BinUtil.hs:184,
     decodeWithHash), so parent .bo bytes change under any child
     edit even where naming would survive.  FIX CLASSES (bsc-side,
     post-rebase, deep): (a) boundary-stub elaboration — the
     parent's evaluator consumes only a child interface stub
     across synthesize boundaries (true separate elaboration);
     and/or (b) position-independent canonical def naming
     (upstream main's Bluesim-codegen canonicalization series is
     the adjacent precedent); plus (c) interface-scoped impHashes
     (hash bi_sig/bo_sig only) so .bo stamps stop propagating body
     edits.  Until then, frontend early-cutoff under Bazel is
     limited to skipping actions whose inputs are unchanged — body
     edits invalidate the whole ancestor chain.
Guidance stands: generators should emit one package per module
type — the one-file spine defeats both cutoff and parallelism.

INCREMENT 2 — decision-gate measurements + noinline A/B (days).
Add noinline to outlined exec bodies in the CURRENT pipeline (none
exists today), diffsweep + perf fences to lock the call-based form as
semantic baseline; with the I1 lap, measure the edge-vs-per-type O3
split on a >10-type real design.  GATE: proceed to 3-4 only if
(per-type share) x (edit-profile hit rate under closure keying)
clears >30% of relink wall reusable on top/sibling edits at target
design size.  Otherwise STOP — I0-2 already banked the tail fix,
remote caching, cutoff, and hermeticity.

INCREMENT 3 — bsc per-module fragments + splice (flag-gated,
~1-2 weeks).  -bir-frag (NOT implied by -trs; one-shot compile stays
byte-for-byte untouched) rides the -c/DCodeGen machinery: per module
simExpandABin + simPackageOpt + encModule + blockCodegen-mode symMap;
M.bfrag = {FRAG_LAYOUT_REV, bscVersionStr, BIR_VERSION, exhaustive
options descriptor (learn from Bluesim's 4-dimension under-keying),
canonical Module CBOR, symMap tier, content_hash, subtree_hash}.
content_hash fills the SimExportIR.hs:733 TODO (requires canonical
CBOR: sorted maps, fixed widths); subtree_hash = H(content_hash ||
sorted child subtree_hashes).  writeBirFile splice mode at -e: Module
bytes verbatim from identity-matched fragments + link residue.
GOLDEN TEST: spliced .bir byte-equals monolithic .bir (mirrors
check_block_codegen_modules).  Interp oracle input unchanged by
construction.

INCREMENT 4 — trs per-type pipeline (multi-week; lands dark behind
flags).  (a) type-keyed symbols exec_<subtree_sig>_<rule_idx>
(replacing instance-derived, jit.rs:2531); (b) now/reset slots as
extern globals defined in meta.o (trs_cb_* convention,
lower.rs:827-836) + assertion the absolute fallthrough
(lower.rs:1719-1720) is never taken in per-type bodies; TYPE_ABI_REV
from day one; (c) trs plan: per-type plan cards (always_fire, eager
union, outline set + replication k, inhibitor ARITY not slots, helper
specs, symbol manifest, PLAN_REV) + design plan (edge sections,
sorted EN list, hoists, absolute bases, proto/token tables);
INVARIANT with test: cards contain ZERO absolute slot numbers (one
module's slot-count change never dirties another type's card);
(d) trs modc --frag M.bfrag --frag-closure <children> --plan card:
per-type LLVM module (gated-variant exec bodies — WF read retained so
plan bits are perf-only, never correctness), own O3, PIC .o with
embedded manifest; (e) trs link --objs: verify manifests; ANY
mismatch -> full in-link emission fallback (the fallback IS today's
certified path — a stale .o can never silently link); lower only
edge fns + uncovered scheds + meta.o at real O3 (the edge contains
inlined rule bodies), cc -shared; (f) Bazel per-type actions keyed on
fragment closure + card; (g) certification: diffsweep with --objs +
perf A/B before any default flip.  One-shot trs link stays the
monolithic pipeline — optimal single-invocation composition.

INCREMENT 5 — roadmap composition.  Loop-rolled spine shrinks exactly
the always-rebuilt edge module (N=32 residual: 1024 sched sections +
call sites), driving the non-cacheable floor toward O(changed types).
content_hash/subtree_hash + plan cards are the identity
infrastructure the type-keyed startup-analysis rung needs.
Pools/lanes unaffected (new parallelism is cross-process via Bazel,
respecting no-threads-before-event-loop).  .birsnap remains a
run-startup decode sidecar in both flows, never a build cache.

## Judge dissent (carried forward verbatim in substance)

Measured data only GUARANTEES payoff for Increments 0-2.  The
per-type object program (3-4) optimizes a stage measured at 0.58s of
2.37s at N=8; its ceiling is unmeasured until the I1 lap exists; and
closure keying shrinks the win class to sibling/top/cross-design/CI
edits.  If effort is constrained, ship 0-2 and re-decide at the gate;
do not start Increment 3 first.
