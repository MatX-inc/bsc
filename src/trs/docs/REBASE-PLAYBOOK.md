# Release-rebase playbook (upstream sync-read, 2026-07-12)

Fleet-verified study of origin/main vs claude/trs-fst.  Corrections
to the standing HANDOFF item-5 picture, the conflict map, and the
sequencing rules.  (Full agent evidence: session task output; the
load-bearing facts are reproduced here.)

## The picture is far more convergent than assumed

- TRUE MERGE-BASE: d2f996c0 (not f4927f1e — that range is 4 doc-only
  commits already in our line).  Upstream is 38 commits ahead.
- ~30 of the 38 are SQUASHES OF OUR OWN WORK already in HEAD:
  c1348cb6 ByteString deserialization (= our 5d9402a9/81224839/
  9122e861/704b35ee stack, byte-identical readABinFile signatures);
  5c5ebb12 incoherence diagnostics (no instance-selection change);
  port-splitting parts 2/3 (ddcd1d3f/4075656b) incl. the __h->__d
  naming shift — VModInfo.hs/AState.hs/APaths.hs are 0-diff vs
  origin/main tip; the 5 Bluesim-codegen canonicalization commits;
  9 testsuite commits.  These resolve as already-applied.
- Only TWO commits are semantically new to us: 6be62d63 (tiExpl
  type-function expansion — can shift dictionary binding order and
  hence __h/__d numbering; newly accepts programs) and 71226f07
  (instance-trie overlap corners).  These are the narrow post-rebase
  parity-audit targets.

## Conflict map (by severity)

1. GenABin `Bin Flags` binder list: ours = 138 binders + chunk9;
   upstream = 135 and its 38 commits touch Flags/FlagsDecode ZERO
   times — the conflict is purely our +6/-2/1-renamed fields
   replaying.  Mechanical recount+rechunk; keep OUR layout.
2. PR #1040 (-remap-path-prefix): OPEN, not in the 38.  Inserts
   remapPathPrefix mid-record (135->136, renumbers everything after)
   and changes genABinFile arity (threads Position->Position through
   BinData share).  TAKE LAST, as a single top-of-stack cherry-pick
   after it merges; NEVER rebase under it (would multiply the same
   conflict across every flag commit).  Plumb the remapper through
   our extra genABinFile call sites (-bir, -trs, .ba-by-default).
3. TCMisc.hs/TCheck.hs: 1515-line divergence — our fundep commits
   (e3f482b0, 024ff0e5) vs upstream 5c5ebb12 + 6be62d63 in the same
   tiExpl neighborhood.  The manual-resolution hotspot.
4. ANoInlineFun named params [(String,Integer)] vs upstream
   positional [Integer] (ASyntax/AConv/AVerilogUtil): deliberate
   local semantics (named Verilog instance params, Fork.v) —
   RE-APPLY, do not drop.
5. SplitVector packaging: upstream ships Base1/SplitVector.bs; we
   merged its content into SplitPorts.bs — ADOPT upstream packaging,
   revert our merge (kills part-5 test import churn).
6. bluetcl.hs 254-line drift (our wiretypemap/crash fixes vs their
   part-2 results-list format).
7. BinData IsList import direction: take upstream's GHC.Exts (8.8
   compat) unless old-GHC support is dropped.
8. .bo/.ba header tags: bump ONCE at top of stack (rc3 discipline);
   the forced full recompile moots cross-rebase hash stability.

Sequencing: rebase --onto origin/main d2f996c0 while Flags surfaces
are quiet; resolve SimMakeCBlocks/SimCOpt hunks to f974ce22's FINAL
sortOn forms; #1040 last; then the narrow 6be62d63/71226f07 parity
audit; canonicalization five expect ZERO golden churn.

## Two immediately-actionable discoveries (pre-rebase safe)

1. encExpr BACKLOG IS UNBLOCKED AND EASY: the port model (ATuple/
   ATupleSel/ATTuple, vf_outputs) is ALREADY in our tree.  The
   HANDOFF repro "(s.getBar TUPLE_...)[3]" is ATupleSel hitting the
   encExpr internalError fallthrough (SimExportIR.hs:1216).  Fix:
   encExpr cases ATuple -> BIR Concat (first element MSB) and
   ATupleSel -> Extract [aSize t + sizeAfter - 1 : sizeAfter] (idx
   1-BASED; sizeAfter = sum of widths strictly after idx), plus
   aTypeWidth (ATTuple ts) = sum widths (~:1111) — arithmetic
   verbatim from SimCCBlock.hs:1072-1085 (the reference performs
   this identical wide-bit lowering, so export-time lowering is
   byte-parity-correct by construction).  No BIR_VERSION bump.
   Converts the 14 EXPORT_FAILs (12 splitports + sysFloatTest +
   sysTestMesa): sweep ~994 -> ~1008 PASS, strengthening the
   post-rebase parity baseline.  trs-only file = zero rebase risk.
   Upstream "part 4" (first-class Bluesim tuples) does not exist;
   keep the ("port", encW32 0) P0 TODO as the seed for later.
2. BOOMERANG AUDIT (before the -c/-e split ships): our own
   SimExportIR.hs S.toList sites (395, 398, 500, 760, 877) — if any
   set is AId-keyed, .bir bytes inherit run-dependent
   interned-FString Ord (SpeedyString counter), the same class
   upstream's canonicalization fixed in C++ — and batch-vs-separate
   compilation (-c/-e) is exactly the interning-order shift that
   exposes it.  Canonicalize with getIdString-keyed sorts at
   serialization.

## For the boundary contract

The canonicalization five are codegen-only (downstream of .ba):
they neither fix nor conflict with the __h<N> naming mechanism —
that fix stays ours, at NAME-ASSIGNMENT time in the evaluator
(IExpand), using 2fb5260f's rank/unrank surrogate-key pattern as
the template and getIdString (not getIdBaseString — stable-sort
residual on equal base names) as keys.  Port-splitting is
net-positive: port names are interface-derived, and part 2 moved
method-output resolution into AConv, shrinking the __h surface.
No external upstream collaborator exists — the determinism agenda
upstream IS this line's own work; #1040 is the alignment vector.
