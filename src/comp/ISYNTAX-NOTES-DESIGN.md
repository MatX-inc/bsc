# Phase-indexed IExpr annotations ("notes") — design sketch

Status: PROPOSED (design only; nothing in this document is implemented).
Evidence base: the IExpr metadata work on this branch (free-variable
caches, content hashes) and its Toooba CI measurements, summarized at
the end.

## Problem

IExpr now carries two kinds of cached metadata, each consumed in a
different compiler phase:

  - free-variable / free-type-variable sets: consumed DURING IExpand
    (substitution pruning in ISyntaxSubst); dead weight afterward.
  - content hashes: consumed AFTER IExpand (hash-first cmpE in
    ITransform's CSE/context maps, ISplitIf, predicate sets at module
    exit); dead weight during elaboration.

Both are lazy fields today, and each phase pays a residual tax for the
other phase's metadata:

  - post-IExpand passes allocate fv-set thunks on every rebuilt node
    and hyper forces them (cheap empties, but ~+10s GC on a Toooba
    compile — the measured lazy-vs-strict tradeoff);
  - IExpand allocates hash thunks per interior node: +21 GB on a
    Toooba compile, measured to be the thunk OBJECTS themselves, not
    forced computation — the delta was unchanged by removing the
    rnf-time forcing.  No forcing schedule can remove a field's
    allocation; only a representation in which elaboration-phase nodes
    have no hash slot can, which is this proposal.

## How to read flag-based A/B results against this proposal

Each feature's chicken-flag cell measures its NET effect: the win in
its consuming phase minus the structural tax it pays in the other
phase.  Both wins are demonstrated (substitution pruning: -17% CPU on
conflict_free_large; hash-first comparison: -18% transform on Toooba
itself), and both taxes are structural.  This proposal deletes the
taxes while keeping both wins simultaneously, so flag-cell results are
LOWER BOUNDS on its value: a feature measuring a wash under its flag
may still be a clear win phase-indexed.  Decision data should
therefore include the per-phase breakdown (-v), not just totals, so
the win and tax components are visible separately.  From Toooba data
alone the estimate is: hash transform win (-8..13s) + fv tax refund
(~+10s GC) + hash tax refund (+21 GB alloc, a few seconds of GC/MUT),
i.e. order of -3..4% total, before counting designs that sit in the
substitution-pruning corner.

## Non-solutions (and why): runtime phase signals miscompile

Any runtime phase signal — a global "now hashing" flag flipped between
passes, or a tagged union annotation chosen at construction — decides
the annotation when a constructor application is EVALUATED, not where
it appears in the source.  IExpr construction thunks demonstrably
cross phase boundaries: the IConv env knot and FixupDefs def bodies
are forced during elaboration, in the middle of eSubstBatch.  Under a
flipped flag those late-forced nodes would be born with empty
free-variable sets, substitution pruning would skip subtrees it must
rewrite, and modules would silently miscompile.  A hybrid
"inspect-the-children" scheme instead forces child WHNF at every
construction, which re-materializes speculative structure lazy
evaluation would discard (measured: the reverted known-empty fast
path, +4.9 GB allocation and a lost cflarge win).

## Proposal

Index IExpr by a note type in addition to the heap-payload type:

    data IExpr n a
            = ILam_ (BindNote n) Id IType (IExpr n a)
            | IAps_ (ApsNote n) (IExpr n a) [IType] [IExpr n a]
            | IVar Id
            | ILAM_ (BindNote n) Id IKind (IExpr n a)
            | ICon_ (ConNote n) Id (IConInfo n a)
            | IRefT IType !Int (S.Set Position) a

Notes live on exactly the four interior constructors.  IVar and IRefT
stay bare: their identity is their content (interned name; heap-cell
number), and both comparison and hashing treat them as O(1) leaves.

The note contents are per-constructor, not uniform:

    class ExprNote n where
      type BindNote n    -- ILam/ILAM
      type ApsNote  n    -- IAps
      type ConNote  n    -- ICon
      noteLam :: Id -> IType -> IExpr n a -> BindNote n
      noteLAM :: Id -> IKind -> IExpr n a -> BindNote n
      noteAps :: IExpr n a -> [IType] -> [IExpr n a] -> ApsNote n
      noteCon :: Id -> IConInfo n a -> ConNote n
      -- comparison hook: hash-first for HashNote, structural otherwise
      noteCmp :: IExpr n a -> IExpr n a -> Ordering -> Ordering

with three instances:

  - FV: BindNote/ApsNote = (VarSet, VarSet); ConNote = VarSet (ftv
    only — ICon's fv is a one-level derivation; the termination
    argument requires never caching through IConInfo payloads:
    iConDef/iValDef/IStateVar are knot-tied).
  - HashNote: all notes = Hash (content hash; exactly the
    cmpE/cmpC-observed content, rank-first comparison as today).
  - (): all notes = ().  This is the by-type successor of BOTH chicken
    flags (-hack-no-iexpr-fv-cache, -hack-no-iexpr-hash): historical
    behavior, no metadata cost, selected statically.

The pattern synonyms keep the six historical constructor names, now
with an ExprNote constraint on the builders.  All passes that never
touch metadata stay polymorphic in n (the majority).  Phase-specific
code gets phase-enforcing types:

    eSubst  :: Id -> IExpr FV a -> IExpr FV a -> IExpr FV a
    iTransform :: ... IModule HashNote a ...

"Substitution ends at IExpand" and "vanishes after IExpand" become
compile errors instead of comments.

## Conversion point

IExpand's output already passes through the unheap/walkNF rebuild that
converts heap references into expression trees.  That walk is the
re-annotation point: it rebuilds every surviving node exactly once, so
computing HashNote eagerly bottom-up there is strictly cheaper than
today's thunk-then-force-at-first-comparison, and no FV metadata
survives into the IModule.  IPackage (.bo) stays on the FV side;
GenBin continues not to serialize notes (recomputed on read via the
builders).

## Container types

IModule, IDef, IPackage, IConInfo, IRules, IEFace, IClock, IReset,
IInout, Pred, PTerm, PExpr and the heap types inherit the n index
mechanically.

## Cost estimate

~707 IExpr-mentioning lines across 31 files plus ~246 container-type
mentions (measured on this branch).  Mostly mechanical (add a type
parameter; constrain builders); one-to-two weeks with the existing
validation gates (smoke, bsc.evaluator, AES byte-identity vs baseline
with the () instance, Toooba A/B).  Regold-neutral relative to the
current branch state: pre-expand code has no Ord IExpr consumers (fv
ordering never leaks), and post-expand ordering is hash-first exactly
as already landed.

## Evidence base (Toooba CI configuration, RV64ACDFIMSU bluesim)

  - fv caches + substitution pruning: eSubstBatch walks pruned;
    allocation -13% on substitution-heavy designs; ~+2% total-CPU tax
    on Toooba-shaped workloads, dominated by metadata upkeep in phases
    that never consult it (the motivation above).
  - lazy vs strict vs hybrid metadata fields: lazy+rnf-forced won;
    strict re-materializes spines; child-inspecting hybrids force
    speculative structure (all measured, hybrid reverted).
  - content hashing, rank-first: transform -18%, MUT -2.4%, wall
    -1.5%; expanded neutral; hash never consulted for leaf or
    cross-constructor comparisons.
  - not forcing the hash at hyper points: semantically the right
    schedule (compute at first post-IExpand comparison), but measured
    allocation-neutral — proving the residual elaboration-time tax is
    thunk allocation, i.e. structural, removable only by this
    proposal's phase-indexed representation.
  - predicate interning probe: 99.9% of pConj calls trivial, ~4K
    distinct predicates, no rework to cache — predicate-set redesigns
    are NOT part of this proposal's motivation.
