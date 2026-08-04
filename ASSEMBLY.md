# integration-carve assembly

Recomposition of the carved PR lines into one cohesive MatX build, on
top of `matx/upstream-main` (941eecfe).  Ravi's ruling: **stable-verilog
stays DEFAULT OFF** — the sv line is merged only through `sv/4-feature`
(#66), not `stable-verilog-7`/#61 (no default flip, no
check_verilog_regen infrastructure).

Setup: worktree from matx/upstream-main; `src/vendor/yices/v2.6/yices2`
copied from wt-sv7 (untracked vendor payload the build needs).

Invariant: after EVERY merge, `make -j16 GHCJOBS=8 install-src` exited 0
before the next merge.  Any post-merge compile fix is folded into that
merge commit (see its message).

## Merge order and conflict resolutions

1. `matx/sv/4-feature` (#66) — clean.

2. `matx/verilator/6-link-tools` (#46)
   - Flags.hs / FlagsDecode.hs / print-flags-raw golden: flag-field
     union; tail order `semanticPortsComment, checkOnly, stableVerilog`
     (sv/4-feature had left checkOnly's comment without the field;
     verilator's `checkOnly` slots exactly there).
   - GenABin.hs (Bin Flags): kept the sv-side 10-chunk a_000..a_137
     layout; added `a_checkOnly` between a_136/a_137.  IMPORTANT: the
     textual auto-merge had placed verilator's `a_dumpFormats` after
     a_021, but the merged record (with sv's `blockCodegen` at position
     6) makes that position `doICheck`; moved `a_dumpFormats` after
     a_022 to match dumpFormats' constructor position (24).
   - AVerilogUtil.hs: VConvtOpts union (`vco_stable` +
     `vco_ffmap`/`vco_def_widths`); adopted verilator's
     `systemVerilogTasks` → `systemVerilogOutput` rename everywhere.
   - Depend.hs: union (sv's elabOnly conditional + verilator's DPI
     wrapper-tracking comment).

3. `matx/single/syb-gating` (#77) — clean (AVerilog.hs only).

4. `matx/atf/5-nf-at-construction` (#88) — GenABin .ba tag only.

5. `matx/iexpr/2-cmp-hash` (#101) — THE planned hard conflict,
   IType.hs: atf/4a..5 rewrote interning to deep-NF-at-construction
   (itRnfSeen retired, `rnf t = t \`seq\` ()`); iexpr widened the
   interned `ITForAll_`/`ITAp_` constructors with a strict UNPACKed
   `Word64` content-hash field filled at intern time.  Resolution keeps
   atf/5's structure and threads the hash through:
   `ITAp_ n (tyHashAp f a) fvs f a` / `ITForAll_ n (tyHashForAll ti t)
   fvs ti k t` inside `assertConstructedNF`, keeping the deepseqs (the
   hash is a strict unpacked word, forced by construction — consistent
   with the NF invariant; nothing in iexpr references itRnfSeen).  Also
   widened the atf-only pattern sites the textual merge could not see:
   assertConstructedNF case arms, sameITypeNode, itHasTFun, itHasATF.
   FlagsDecode traceflags auto-merged as an alphabetical union.

6. `matx/ctype/7-instrumentation` (#99)
   - ATFRules.hs imports: union (body uses atf/5's `itHasATF` skip AND
     ctype's `atfRedMemo` per-unique memo).
   - bluetcl goldens (browseinst2, hierarchy/Example ×2 formats):
     provisional, regenerated later (see final regold commit).

7. `matx/dicts/5-content-named` (#97) — the dicts line is based on
   dicts/0 (#925, the in-IPackage ATF cache) + ctype/1; the assembly
   already contains atf/1's retirement of that cache (66c1b6bf).  Keep
   the retirement, keep the dicts payload:
   - FixupDefs.hs: dicts' DictRedirects dedup on the 4-field IPackage.
   - IConv.hs: ctype's iConvT' canon memo + iConvTStats kept; dicts'
     `liftedDefs` parameter adopted; CATFCache parameter dropped.
   - IExpandUtils.hs: atfrules fullTypeNormalizer kept (no cache).
   - ISyntax/IType/ISyntaxSubst: took HEAD (dicts' copies were
     byte-identical cherry-picks of atf/1..3; its IATFCache/IExpr(..)
     exports superseded by the iexpr pattern-synonym architecture).
   - TIMonad.hs: dicts' GroundDictState pooling kept minus tsATFCache.
   - TypeCheck.hs: dicts' pooled tiDefns/cTypeCheck (extraTaken ids,
     lifted defs at front) minus atfCache threading.
   - bsc.hs: import union; iConvPackage takes lifted_defs mod_lifted.
   - GenABin.hs: slots `a_liftDicts`/`a_liftGroundDicts` at constructor
     positions 49/50 (after a_046).
   - Fixes folded in: two leftover 5-arity IPackage patterns in
     FixupDefs (mkDictRedirects).

8. `matx/tc/2-apsubc` (#82)
   - Subst.hs: tc's change-tracking apSubM/apSubC adopted; ctype's
     `isCanonType` O(1) short-circuit re-threaded as the first equation
     of both walks (`Nothing` / `Unchanged`).  The mechanisms are
     independent: apSubC is gated by -apsubc (default False).
   - Pred.hs: export/import unions (dictBaseName/hashType alongside
     expandSynPred; Changed alongside the dicts hash imports).
   - GenABin.hs: slot `a_useApSubC` at constructor position 122 (after
     a_117, chunk7).
   - Fixes folded in: TCMisc groundPoolHit let-destructures VPred (a
     pattern synonym on the tc line — failable do-bind needs MonadFail);
     LiftDicts imports apSub (moved out of the Types class).

9. `matx/ser/5-thin-ba` (#87) — the biggest format merge; ser is based
   on upstream-main which still had the in-IPackage ATF cache.
   - bsc.hs compilePackage: reconstructed at function granularity —
     ser re-indented the pipeline tail into `if checkOnly … then (emit
     .bc; stop) else do`, so the textual merge paired unrelated code.
     Took ser's function (check-only early exit, dumpUsage /
     warnUnusedImports helpers, optHash'd binmods) and re-applied every
     assembled delta inside the else branch (verilator deferred DPI
     wrappers, dicts liftdicts/dictBuckets/dictRedirects/isimpdicts and
     wrapper lifting, atf 4-field IPackage, no ATF-cache plumbing).
   - TypeCheck/TIMonad/CtxRed: ser's per-declaration usage tracking
     (tsUsedDecls/tiUsedDecls/recordDeclUse) threaded through the dicts
     pooling structure; recordATFResult/CATFCache stay retired;
     runTIWithGroundPool's TIResult gained tiUsedDecls.
   - BinUtil.hs: ser's .bc-preferring doImport + header pill byte, on
     the 4-field IPackage.
   - FixupDefs.hs: DictRedirects kept; ser's `Maybe String` hash type
     adopted in all ipkgs lists.
   - GenABin.hs: `a_importHashes`/`a_baDebugInfo` appended after a_137.
   - GenBin.hs: ser's thin-.bo (sigHash + pill byte, .bc reader/writer)
     all auto-merged; tags kept at the pre-unification values.
   - FlagsDecode.hs -check-only guards: ser's source-compile guard
     (ECheckOnlyConflict "-sim/-verilog") kept ALONGSIDE the elab-only
     guards; ser's link-time guard NOT taken — the verilator line's
     -check-only at link (validate + manifest) is a feature that needs
     a backend there.  The flag description covers both roles and is
     Visible.  Added the `importHashes` entry ser had omitted from
     showFlagsRaw.
   - Error.hs: union (EElabOnly* + ECheckOnlyConflict).

10. Integration singles, in order, each built clean:
    - `intg/conflict-method-check` — AAddScheduleDefs import union.
    - `intg/task-result-live` — clean.
    - `intg/typecheck-perf` — bsc.hs orderGens: ported the branch's
      set-backed membership rewrite onto the assembled (4-field,
      re-indented) copy.
    - `intg/coherence-split` — clean.
    - `intg/letrec-s0066` — clean.
    - `intg/foreign-block-sets` — AVerilogUtil vForeignBlock: composed
      the branch's Set-intersection base list with sv's -stable-verilog
      text-canonical sort on top; `intersect`/`(\\)` imports dropped
      (unused → -Werror).

11. `single/schedcond-cost` (clean), `single/symtab-memo` (clean),
    `single/prof-costcentres` — SpeedyString header: kept iexpr's sHash
    export alongside the -fno-prof-auto pragma.

12. `claude/bluespec-scheduling-complexity-nsvfs9` (#47) — GenABin Bin
    Flags predates the assembly's slots; kept the assembled shape and
    added the branch's new `schedTransposed` flag as a slot at
    constructor position 99 (after a_094, chunk6).
    `fix/signal-naming-suffix-regex` (#60) — clean (its name-only
    golden drift is upstream of this branch's suite gate).
    PR #40 `dupctx-untracked` — taken as a CHERRY-PICK of the tip
    commit only (merging the branch would drag in unrelated
    MatX-internal history); effectively adds the .gitignore line
    (dupctx.c is not tracked on this base).
    PR #23 `input-ports-tuples-v2` — **SKIPPED**: merge attempt
    conflicted non-trivially (12 files, including ASyntax.hs,
    ISyntaxUtil.hs, ACleanup.hs and AVerilogUtil.hs against the
    sv-touched emitter).  Flagged for a dedicated port.

## Flags / Bin Flags (GenABin) final shape

The merged Flags record has 146 fields.  The positional a_N slots keep
the sv-line numbering (a_000..a_137) with named slots inserted at the
exact constructor positions of each line's fields, in order of assembly
arrival:

| slot | record position | field |
|------|-----------------|-------|
| a_dumpFormats        | 24  | dumpFormats (verilator)        |
| a_liftDicts          | 49  | liftDicts (dicts)              |
| a_liftGroundDicts    | 50  | liftGroundDicts (dicts)        |
| a_schedTransposed    | 99  | schedTransposed (#47)          |
| a_useApSubC          | 122 | useApSubC (tc)                 |
| a_checkOnly          | 141 | checkOnly (verilator+ser)      |
| a_importHashes       | 145 | importHashes (ser)             |
| a_baDebugInfo        | 146 | baDebugInfo (ser)              |

(positions 138-140/142-144 are warnUndetPred/semanticPortsComment/.../
stableVerilog per the record tail; a_136=semanticPortsComment,
a_137=stableVerilog.)

## Serialization tags (unified in one commit after all merges)

- .ba → `bsc-ba-20260804-8`
- .bo → `bsc-bo-20260804-5`
- .bc → `bsc-bc-20260804-4`

These are the next free per the campaign ledger and retire the
deliberate ser-vs-dicts tag collisions.

## Golden regenerations (from the assembled compiler; never hand-merged)

- bluetcl: commands/browseinst{,2} and hierarchy/{Design,Example}
  (both -bh and plain) — ctype positions + iexpr naming both apply.
- bsc.options/bsc.print-flags-raw.out.expected — regenerated with the
  BLUESPECDIR macro substituted back (3 occurrences preserved).
- bsc.options/bsc.help.out.expected — hand-edited the single changed
  -check-only line; never regenerated via m4 (m4 eats backticks).

## Sanity checks

- Contamination grep over the full diff vs matx/upstream-main:
  no TypeShareFlags / noITypeWalkMemos / remapP / -hack-ground-ctype.
  (hack-no-iexpr-* and hack-no-itype-ftv-cache traceflags are
  legitimate and present.)
- `bsc -help-hidden` lists -apsubc and -lift-ground-dicts;
  -print-flags-raw shows `useApSubC = False`, `liftGroundDicts =
  False`, `checkOnly = False`, `stableVerilog = False` (default OFF,
  per the ruling).
- Localchecks (inst/bin on PATH), all 0 unexpected failures:
  typechecker/typeclasses 107, bluetcl/commands 60, bluetcl/hierarchy
  8, evaluator/opt 84, bsc.options 71.
