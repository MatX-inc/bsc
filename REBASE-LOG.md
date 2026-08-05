# trs-fst rebase onto upstream main (941eecfe) — conflict log

Date: 2026-08-05. Rebase of personal/claude/trs-fst (be5065ca, 585 commits
since merge-base d2f996c0) onto origin/main (941eecfe, 71 commits ahead of
the merge-base). Executed per src/trs/docs/REBASE-PLAYBOOK.md plus the
delta analysis for the 33 upstream commits that landed after the playbook's
38-commit study (ATF-cache series, idQuality b28e4d8b, strict-State
3e5681b1/83979ec2, SpeedyString b78215d3, perf-creg tests).

Segments (~100 commits each, `git rebase --onto <base> <prev-end> <end>`):

| Seg | Range (old SHAs) | Commits |
|-----|------------------|---------|
| S1  | d2f996c0..09fe4255 | 100 |
| S2  | 09fe4255..b5b0bd30 | 100 |
| S3  | b5b0bd30..148b6852 | 100 |
| S4  | 148b6852..919ca083 | 100 |
| S5  | 919ca083..eb226b44 | 100 |
| S6  | eb226b44..be5065ca | 85 |

## Conflict inventory

(recorded per segment as the rebase advances)

### Segment 1 (d2f996c0..09fe4255, 100 commits -> 62 kept, 38 dropped as already-applied)

The segment is dominated by the port-splitting/incoherence "echo cascade":
our original commit series replaying under upstream's squashes of the same
work (parts 2/3/5 = ddcd1d3f/4075656b/b3091d51, incoherence = 5c5ebb12,
evaluator-leak series = e7a72ee7..bf6c9580, IRefT position = 7e173394,
-cross-info removal = 2a0f45d4, tuple-mux = 366d2186, TupleSize proviso =
578123c4, mcd snapshots = ba3bd1b6/3a68da0a).

Policy for echo conflicts: resolve to the NEW-BASE side ("ours" = upstream's
squashed FINAL form of our own work; our tip 0-diffs against it for shared
files). This never regresses content and preserves all upstream-new work by
construction; empty commits were dropped by the rebase. Verified against a
`git merge-tree origin/main be5065ca` reference (tree 86b385b1) at the end.

Manual (non-mechanical) resolutions in this segment:

1. 760d7925 "Add SplitVector utility" — playbook item 5: ADOPTED upstream's
   Base1/SplitVector.bs packaging; reverted our merge of its content into
   SplitPorts.bs (restored SplitPorts.bs from base); took upstream's
   SplitVector{Ops,Ports}.bs test files. 43e85004 (DefaultValue derive)
   resolved the same way (upstream already derives it).
2. e5f98c6a Id.hs IdProp: union — upstream's IdPIncoherent line kept, our
   IdPCAF added.
3. 4def90ee TypeCheck.hs: import union (ours adds fst3 + commented trace
   import) on top of upstream's CATFCache-threading signature.
4. a0047516 bsc.hs (LiftDicts pass vs ATF cache): kept upstream's
   combinedATFCache = mergeCATFCaches ...; changed iConvPackage input from
   mod' to our mod_lifted (LiftDicts output). Import union (LiftDicts +
   upstream's IATFCache/mergeIATFCaches).
5. 3ca827e6 FixupDefs.hs (drop redundant dicts vs ATF cache): merged our
   dropDict/ds'' filtering into upstream's 5-field IPackage (own_atf_cache)
   shape; kept upstream's own-cache-only comment and own_atf_cache in the
   result. FlagsDecode traceflags: union (trace-drop-dict, trace-eq-witnesses
   added alongside upstream's trace-atf-cache*).

Note: ANoInlineFun named-params (playbook item 4) needed NO action —
upstream's part-3 squash already carries our ([[(String,Integer)]],
[(String,Integer)]) port-list shape; verified identical at both tips.

### Segment 2 (09fe4255..b5b0bd30, 100 commits -> 56 kept)

More echo clusters, all resolved keep-ours (upstream squash = our final form):
ByteString deserialization (c1348cb6 = our 5d9402a9/81224839/9122e861/
704b35ee), PrimStringSplit (cd1fa927), the ENTIRE ATF-cache series
(f33c4a37/1e2d76e8/941eecfe... = our e861242d/1f78e006/f6b18635 etc. — the
ATF cache is our own line's work squashed upstream, so the task's "re-thread
fundeps around upstream ATF machinery" largely materialized as echo
resolution), ISyntaxCheck tracing (992172a2/62a297b4), strictness series
(b78215d3/3e5681b1/83979ec2/bfc3e0af/2e6fc2dc/1571b0d6/77c3153d/fb2579fe/
2444d7f1/33d90289), idQuality (b28e4d8b = our eb363356 — delta item 10 is an
ECHO: upstream's idQuality mechanism IS our commit; nothing to re-thread),
VMItems perf (24acc7fd), dumpba/.ba testsuite checks + perf-creg tests
(41f567db/79cc9aea/f08860a0/af1a7425/481a39c2), eqPtrs tie order (7bea3a31),
tiExpl #890 fix + PR #916 review follow-ons (6be62d63 = our 827bd7ec
cluster), FloatToFixed (5c2ab740), inout-plainly (8458d046), canonicalization
five (4eda94e0/1839fe82/2fb5260f/c04e746b/f974ce22; SimMakeCBlocks/SimCOpt
sortOn hunks resolved to f974ce22 final forms via keep-ours per playbook
item 9).

Manual resolutions:

6. efe6666d fixupDefs signature: our M.Map IType Id first arg merged with
   upstream's 5-field IPackage (own_atf_cache) pattern.
7. d84b1cee ITransform blank-line-only conflict: took theirs (our next
   commit 1a33dfb9 adds the blank line).
8. cf16fe71 golden files (undet/opt/derived_bits/log2_loop): keep-ours —
   upstream 36bff1fa already carries our final expectations.
9. 3b70431b/6556d98d/0d7e4993 AVerilog SV-identifier work (ours-genuine):
   merged our G0129-G0133 legalization pipeline on top of the base's
   renameInoutPorts wrapping (VProgram (map renameInoutPorts mods) ...);
   rerere replayed the adaptation for the two follow-on commits (verified
   each time).
10. 9bec28d0/089392c6 filter tests: kept deletions of basicinout.pl and
    mkImpArgConnect*.v.expected (absent at BOTH tips).
11. b5b0bd30 -sim-codegen-only: THE GenABin Bin Flags recount (playbook
    item 1). Verified arithmetic: origin/main = 134 record fields = 134
    binders (a_000..a_133); our replays renamed systemVerilogTasks ->
    systemVerilogOutput and dropped v95 (record 133), this commit adds
    simCodegenOnly -> 134. Kept HEAD's 134-binder chunked GenABin
    (positional serialization; count re-verified == record fields
    programmatically). Format shift (rename+drop+add at equal count) is
    covered by the single .bo/.ba tag bump at top of stack.

ExpSizeOf/ATF-synonym note: our PR #1028 line (67717f89/ed6faad4/816c1d52,
checkNoTypeFunInHead synonym expansion) is NOT upstream — applied as ours.

### Build checkpoint 1 (after the Flags/GenABin cluster)

First run FAILED with exactly three "Multiple declarations" errors — the
signature echo-replay artifact: our original hunks applied cleanly at a
different file position than upstream's squashed copy, duplicating
byte-identical blocks (ILift.mergeLiftArg, GenBin.headerBS,
ASyntaxUtil.argInputPorts).  Fixed by boundary commit "rebase: drop
declarations duplicated by echo replay" (deduped all three; each pair
verified byte-identical before deletion).  Rebuild: exit 0.

### Segment 3 (b5b0bd30..148b6852, 100 commits -> 81 kept)

Ours-genuine majority (-block-codegen/-c codegen mode, -dump-formats,
coercion primitives, bluetcl wiretypemap/WireAnalysis, bo2bloogle, fundep/
solved-dictionary-pool line, Integer module parameters, CI). Echoes resolved
keep-ours: MPEG4 trio (nets to upstream 6f871dc7 state; dir identical at
tips), parallel-make race (c9b29b25), PHONY decls (4d10ba9b), PrimBNot
(136f3386), b1490 caps (08beeb08), BRAM0Test/InputClocksSameDomain/
relax_method_urgency/b925-XFAIL testsuite echoes, bluetcl util scripts
(66214307), GHC heap scale (6d2f3514), reflect/TypeOf (16218b50), instance-
trie cluster (71226f07 = our abc0e006/ca3c4385/9fc80f0c/c9d39d9e; the
StuckATF/order.exp AA files keep-ours'd, later commits carry our evolution).

Manual resolutions:

12. d7c209ff (-c codegen mode): GenABin Bin Flags recount to 135 binders
    (record grew to 135 with codegenNames); took theirs' a_134 layout;
    count==fields asserted programmatically.
13. The fundep/TCMisc-TCheck cluster (d43f3016..c146340b incl. e3f482b0,
    024ff0e5) applied with NO conflicts: upstream's ATF-cache + incoherence
    machinery in TCMisc/TCheck matches our line's own (echoed) forms, so
    our fundep commits' contexts matched exactly.
14. 148b6852 (rc3 canonical tag commit): resolved the .bo/.ba tag conflict
    to FRESH values bsc-ba-20260805-1 / bsc-bo-20260805-1 (upstream is at
    ba-20260712-1/bo-20260714-1; 20260804 series burned by MatX assembly).
    Later per-format bumps in the stack map 20260710-k -> 20260805-k.

### Segment 4, rc3 repair cluster (2c3c271e..fe29ccf7)

The S3-boundary build failed with 11 errors (VDPI 3-vs-5 arity,
VAParameter 3-vs-4, iparams Either drift, vCommentTaskName/ips'/es' out of
scope) — all of which are our own history's KNOWN intermediate breakage:
the rc3 assembly had the same states, healed by our own repair commits
2c3c271e ("first-compile fixes") and 970cd369 ("repair sat-block splice;
VDPI/VAParameter/iparams arity drift").  The build checkpoint was therefore
taken after the repair cluster instead of at the raw segment boundary.

15. 970cd369 TCMisc.hs sat-block conflict: took THEIRS (the repair).  The
    rebase reproduced the same wrong splice in the incoherent-match arm
    that the rc3 assembly had; after taking the repair, TCMisc.hs is
    byte-identical to the branch tip (diff = 0 lines) — strong evidence the
    fundep/ATF hotspot resolved exactly to the intended final state.
16. 1207d247 (4-field IRefT in pushBNot') dropped by rebase: contents
    already upstream (patch-id match with b28e4d8b-era IExpand state).

### Segment 4 remainder (fe29ccf7..919ca083, 95 commits)

17. b4dae9a7/7668e470/80a87a7b/55c47279 .ba tag marches: each of our
    line's format-bump commits conflicted only on the header tag string;
    mapped mechanically bsc-ba-20260710-k -> bsc-ba-20260805-k (k=2..5).
    A fix-tags helper auto-resolved these; final .ba tag at head:
    bsc-ba-20260805-5, .bo tag: bsc-bo-20260805-1 (matching our line's
    bump cadence, fresh vs upstream's 20260712/20260714 and the burned
    20260804 MatX-assembly series).
18. Everything else in S4 (FST/libfst, .ba-by-default, -c Verilog
    codegen, TRS scaffold + BIR exporter) applied clean.

### Segments 5 and 6 (919ca083..eb226b44..be5065ca, 185 commits)

ZERO conflicts — pure src/trs (Rust) and trs-side testsuite work.

### Post-rebase convergence audit

Reference: git merge-tree --write-tree origin/main be5065ca =
86b385b1 (conflict list = 40 files).  Final tree vs reference differs
only on the deliberately-resolved conflict files.  Files differing from
the OLD TIP (be5065ca): 26 total — all either (a) upstream adoptions we
wanted (SplitVector packaging, GHC.Exts IsList direction [playbook item
7], ISyntaxSubst 8.8-compat CPP block, upstream's newly-added ATF
cache-hit pins ATFCacheHit*.bs + typeclasses.exp block, upstream doc/CI
wording, util/scripts/basicinout.pl, RSchedule cleanup), (b) intended
tag values, or (c) three echo residues cleaned by the convergence
commit (APaths dead num_outputs, FixupDefs blank line, primtcons.exp
duplicated registrations).

Playbook 0-diff pins: VModInfo.hs, AState.hs = 0 diff vs origin/main;
APaths.hs = 0 diff after convergence commit.  GenABin Bin Flags final:
138 binders + chunk9 = 138 record fields, byte-identical to the old tip
except the tag (playbook item 1 satisfied).  TCMisc.hs byte-identical
to old tip; TCheck.hs/SolvedBinds.hs carry our fundep line threaded
through upstream's (echoed) ATF cache exactly as at the old tip.

idQuality (b28e4d8b, delta item 10): RESOLVED AS ECHO — upstream's
commit is a squash of our own eb363356; there was no competing
mechanism to re-thread.  The __h naming work and upstream's idQuality
are the same code at both tips.

History hygiene: REBASE-LOG.md had been accidentally staged into the
first replayed commit by the resolution driver; the stack was rebuilt
from an amended first commit (git rebase --onto <amended> <first>) so
no rebased commit ships the log; it returns as the final docs commit.
PR #1040 (-remap-path-prefix) remains OPEN upstream — deliberately NOT
included (playbook item 2: take-last fires when it merges).

## Gates (rebased head f132ec93)

Builds: full `make install-src` exit 0 at the head (and at the two mid-stack
checkpoints); `cargo build` + `cargo test` clean; release `--features jit`
build clean.

Parity-audit localchecks (targets 6be62d63 tiExpl, 71226f07 instance-trie,
b28e4d8b idQuality — all of which resolved as echoes of our own commits):
- bsc.typechecker/typeclasses: 122 expected passes, 0 unexpected
- bsc.evaluator/opt:            94 expected passes, 0 unexpected
- bsc.bluetcl/commands:         72 expected passes, 0 unexpected
- bsc.options:                  73 expected passes, 0 unexpected
  (no print-flags-raw regold needed; canonicalization five caused zero
  golden churn, as the playbook predicted)

Full testsuite (fullparallel, iverilog + SystemC + CXXFLAGS=-O0):
PASS=23484 FAIL=4 XFAIL=134 XPASS=0.  The 4 FAILs were all
bsc.trs/determinism ".so identical across links" — DISPOSITIONED as a
build-environment artifact, not a rebase regression: `make install-src`
was run without LLVM_SYS_181_PREFIX, so src/trs/Makefile installed the
interpreter-only trs (no `jit` feature) and `trs link` emits no .so at
all (cmp failed on absent files; the artifacts run interpreted and their
golden outputs PASSed).  With the jit-enabled trs installed the directory
re-runs 4/4 PASS in isolation.  Effective tally: 23488/0/134/0.

trs ladders (frozen jit binary): tests/regress 6/6 PASS,
tests/vcd 11/11 PASS (including the FST twin).

trs diffsweep (frozen flow, --aot, quiet machine): see below.
Sweep tally: 1008 PASS / 0 DIFF, perf fence CLEAN (987 designs);
classes: COMPILE_FAIL 30, NO_SOURCE 20, NOT_SUPPORTED 14, LINK_FAIL 3
(sum 1075).  vs the HANDOFF equilibrium (28/20/16/3) two designs moved
from NOT_SUPPORTED to COMPILE_FAIL — the coherence specimens
(sysInstance_Coherent / sysInstance_Default) now fail compilation by
DESIGN under the adopted upstream T0158-as-error semantics; zero
byte-parity DIFFs, so this is classification drift from the intended
test-semantics change, not simulator drift.

## Summary

585 commits replayed onto origin/main 941eecfe -> 486-commit stack
(99 dropped as already-applied upstream squashes of our own work) +
this docs commit.  Both hotspot clusters (GenABin Bin Flags, TCMisc/
TCheck fundeps-vs-ATF) resolved to byte-parity with the old tip.  All
build checkpoints exit 0; parity localchecks clean; full suite
effectively 23488 PASS / 0 unexplained FAIL / 134 XFAIL / 0 XPASS;
trs sweep 1008/0 with clean perf fence.
