# Round 4: the all-orders theorem

A separately synthesized module exports, for each pair of its methods, CF, SB,
SBR or C. A parent fires some of its rules in a cycle. Each parent rule calls
some methods, and the calls of all the firing rules form one order. The
contract: every SB/SBR pair among the called methods is called in its order,
no parent rule calls both methods of an SBR pair, and no two C methods are
both called.

Round 4 proves the following for every such cycle. Some order of all the
module's steps runs the calls in the parent's order and keeps each parent
rule's calls together, with no module rule between them. Its
one-at-a-time run agrees with the hardware. `schedule_sound` (bsc's order σ)
is the witness everything reduces to.

The proof has two parts:

- **Generic, proved once.** `Generic.lean`, about 2,080 lines, core Lean only,
  no `sorry`. Its main theorems depend only on `propext`, `Classical.choice`
  and `Quot.sound`.
- **Per design, linear in size.** A footprint table, a few per-step and
  per-method tables, one lemma per step, one lemma per exception group and
  per never-both-fire method group, and a kernel-decided scan of the
  footprint table, cut into checkpointed segments so that no single kernel
  check grows with the design.

`Example.lean` is a toy design in the emitted style. Its value method getR
has two read copies, so a parent may read it twice in a cycle. The headline
theorem names the methods (`inductive Method`), and every table it mentions
is written by method name. The file applies the theorem to 20 concrete ways
of calling the methods:

- twelve orders with one call per parent rule, among them all six orders of
  the three CF methods;
- five that group the calls into parent rules;
- three that read getR twice, from two parent rules or from one.

It also shows the scan failing when an SB annotation is missing, when its
exclusion group is removed, when its exception pair is removed, when a path
through rules is annotated SB where SBR is needed, and when a checkpoint is
wrong. The scan is decided in two checkpointed segments, and also in one
piece.

Files (all in this directory):

| file | what |
|---|---|
| `Generic.lean` | (A), (B), (C), the parent contract, the checkpointed scan, linear helper lemmas |
| `Example.lean` | the toy design (imports `Generic`; build commands in its header) |
| `scan2.py` | Python mirror of the scan, for the corpus counts in §7 |
| `tools/mkchunk.py`, `chk3/run.sh` | checkpointed-scan timing instances (with a Python mirror that writes the checkpoints) and their driver |
| `chk3/` | this round's timing instances and logs (`*.log`: wall, user, peak RSS, cumulative kernel time) |
| `tools/mkinst2.py`, `tools/mkper2.py`, `tools/timeit.sh`, `chk2/` | the previous round's timing instances and logs |
| `prev_round/`, `prev_round3/` | the files of the two previous rounds, for comparison |
| `analyze.py`, `estimate.py`, `idlefp.py`, `table.py`, `data/` | previous-round measurement scripts and round-3 outputs |

## 1. The headline theorem

This is how it reads in `Example.lean`. An emitted file differs only in its
data.

```lean
theorem all_orders (i : Inputs) (h : Hyps i) (P : List (List Method))
    (hP : Gen.ParentOrder Ann.hasEnable Ann.sb Ann.sbr Ann.excl Ann.unv (Ann.en i) (Ann.calls P)) :
    ∃ π : List Nat, π.Perm (List.range Run.n) ∧ Gen.Extends Scan.meth Run.n (Ann.calls P) π ∧
      Gen.Contiguous Scan.meth (Ann.calls P) π ∧
      Agree i (Run.toState (Run.run i π Run.init)) (stepScheduled i) = true
```

- **`P`** holds one list per parent rule that fires. Each list gives that
  rule's calls, by method name, in order, and the rules come in their order
  too.
  - `Method` is an inductive type with one constructor per method. An
    argument-less value method `v` with `K` read copies has the constructor
    `v (j : Fin K)`: the `j`-th read of `v` in the cycle.
  - `Ann.calls P` is the numbering the proof uses (`Method.toNat`), and its
    flattening is the order of all calls.
- **The tables the statement mentions are written by name.** These are
  `Ann.sbM`, `Ann.sbrM`, `Ann.exclM` and `Ann.unvM` (by `Method`), and
  `Ann.enM i`, which reads `| .setR => i.EN_setR | ...`, the enable port
  itself.
  - A script or a reader can check them against bsc's schedule output and the
    `.ba` file without knowing the numbering.
  - The numbered versions the proof uses (`Ann.sb := Ann.byNum Ann.sbM`, ...)
    are derived from them by definition, not emitted separately.
  - `Scan.meth_ok` (decided) checks the rest of the numbering:
    - steps labelled with a method are exactly its steps;
    - `Method.ofNat` inverts `Method.toNat`;
    - no method is annotated with itself.
- **`Gen.ParentOrder`** is the parent's side of the contract. It quantifies
  over sets of calls, groupings and orders without enumerating them. Its
  fields are:
  - `nodup`: no method, and no read copy, is called twice in a cycle. A
    parent reading `v` several times calls `v 0`, `v 1`, ... in turn; see §4.
  - `enabled`: every method that has an enable port (`hasEnable`, from the
    port list) and is enabled this cycle is called.
  - `sb`: no call comes after a call of a method exported SB or SBR before
    it.
  - `sbr`: no parent rule calls both methods of a pair exported SBR. Only
    pairs of distinct methods count (`p ≠ q`), so an exported self-annotation
    can never make a method uncallable. `Scan.meth_ok` also checks that none
    is emitted.
  - `c`: no two methods of a pair exported C are both called.
  - A CF pair may come in either order, from one parent rule or from two.
- **`Ann.unv`** lists exported CF pairs the generator could not verify. It is
  empty in every design except bus_2m_2s (§5). The contract treats them as
  SBR.
  - Because it is an argument of the headline statement, any narrowing is
    visible there.
  - The generator also prints a warning per pair, naming the pair, the path
    and the passable method or rule on it.
  - For bus_2m_2s the headline therefore does **not** verify 16 of bsc's 65
    CF claims. This is tracked in §8.
- **The conclusion** gives a permutation `π` of the steps with three
  properties:
  - `Gen.Extends`: π runs the steps of the called methods in the order of
    the calls.
  - `Gen.Contiguous`: between two steps of methods one parent rule calls, π
    has only steps of methods that rule calls.
  - π's one-at-a-time run agrees with the hardware. That includes the result
    of every read copy, each its own module output compared with the
    hardware's port.
- **The proof** gets `π` from `Gen.all_orders_parent`, which shows that π's
  final state equals σ's. `schedule_sound` then gives `Agree`. `Agree` begins
  with `!illegal_access && !guard_violated`, as in the emitted files, so π
  inherits legality from σ.
- **Hypotheses:** none beyond the existing `Hyps`. The exclusion facts used
  are bsc's disjointness chains (`Hw.disjoint_N`), the user
  `mutually_exclusive` facts and the `ready_` facts.

## 2. The generic theory (`Generic.lean`)

The model is round 3's `Run.Eff`. A step reads `R`, sets `W` to `U(reads)`,
and OR-sets the flags `O` when `C(reads)`. The theory is generic over a value
type with a Boolean reading (`Gen.FlagVal`). Commutation is asked only from
states satisfying an invariant `Inv` that every step keeps: "every field holds
a value of its sort".

- **(A) `Gen.serial`.** Two orders of the same distinct steps end in the same
  state if every pair they run in opposite orders commutes (`Gen.Comm`, an
  abstract relation, so semantic lemmas plug in).
- **(B) `Gen.blocks_serial`.** This takes a block certificate (`BlockOK`):
  per field, along σ, a write alone, a run of reads, a run of flag-sets, or a
  run of steps of one exception group.
  - Any order that keeps each field's block indices in order among the steps
    that fire (`Valid`) ends in σ's state.
  - Steps that do not fire commute with everything (`IdleComm`). Steps of one
    group commute (`TagComm`).
  - Both hypotheses are now asked only for steps below `n`, so a design never
    has to cover step numbers past its last step.
- **(C) `Gen.all_orders` / `Gen.all_orders_parent`.** These take `D.Ok`:
  every step passes the scan's check (`stepOk`), reached by the methods the
  scan finds reaching it.
  - `scan_ok` gives `D.Ok` from one decided `D.scan = true`.
  - `ok_of_chain` gives it from a chain of checkpointed segments, which is
    what a design emits (see **Checkpoints** below).
  - The scan walks σ once.
  - **Uses.** Each step's fields come from `ul`, a linear three-way merge of
    its sorted `R`, `W` and `O` into `(field, bits)`. The scan checks that the
    result is strictly sorted (`sortedK`) and reads the kind of use off the
    bits in constant time (`useOfBits`). This replaces the previous round's
    `useOf` calls, which scanned the step's whole footprint for every field
    and made the cost Σ|fp|² (the review's finding).
    - `useOfBits_ul` and `bitsAt_sorted` prove once that the merged entry
      gives `useOf`'s answer.
    - Unsorted lists only make the scan fail, so sortedness is not trusted.
  - **Reach, in two kinds.** Per field, the scan keeps the current block and
    two bit sets in `Nat`s: the methods reaching the earlier blocks, and those
    reaching the current block. Each set holds two kinds of bit:
    - bit `a` means method `a` reaches the field directly;
    - bit `M + a` means it reaches the field through a passable step (a rule,
      or a step of a method that is not enable-gated).

    A step passes on its own method's bit directly. A passable step also
    passes on everything reaching it, as through-bits (`Design.lift`).
  - **The check (`stepOk`).** At a step of method `b`:
    - every direct reacher must be in SB ∪ SBR ∪ C ∪ nba ∪ unv of `b`;
    - every through reacher must be in SBR ∪ C ∪ nba ∪ unv of `b`;
    - `b` itself must not reach the step, in either kind.

    `Gen.allowedOf` builds the allowed bits from the annotation lists.
  - **The order π (`Design.order`).** Steps are sorted by
    `(key1, key2, σ position)`:
    - a step of a called method gets `key1 = 2·(index of its parent rule) + 1`
      and `key2` = the position of its call within the rule;
    - every other step goes right after the last parent rule with a firing
      call that reaches it: `key1 = 2·(that index + 1)`, which is even, so
      never inside a rule.

    A rule reached by a call therefore sits after that call's whole parent
    rule, which gives `Contiguous`. `key_mono` shows π is `Valid`:
    - a through-reacher must be in an earlier parent rule (`Fits.ordT`),
      which is what SBR/C/nba/unv guarantee;
    - a direct reacher must only be called earlier (`Fits.ordD`).
  - **Checkpoints (`segFrom`, `segChk`, `ChainFrom`, `ok_of_chain`).**
    - **What is emitted.** The generator cuts σ into segments of about
      `K = max(2000, F)` uses. After each segment it writes the scan state as
      a literal: `Tr.ofList d [(field, ⟨last, idx, ty, prev, cur⟩), ...]`,
      one entry per field used so far.
    - **What is checked.** Each segment is its own theorem,
      `D.segChk a m cpA cpB = true := by decide +kernel`: from `cpA`, the
      `m` steps from `a` pass and end exactly in `cpB` (compared with
      `Tr.beq`). The last segment is `D.scanFrom (List.range' a (D.n - a))
      cpA = true`.
    - **How the chain closes.** `ok_of_chain` chains the segments by
      induction (`segFrom_ok`: a segment run from `stAt a` ends in
      `stAt (a + m)`).
    - **Why literals suffice.** A tree built by `Tr.ofList` from the fields
      used so far has the same shape and values as the scan's tree, whatever
      the order of entries, so the literal equals the scan's state exactly.
      A wrong literal only makes a check fail (`Example`'s
      `Scan.bad_checkpoint`).
    - **Size.** Emitted size is `O(uses + (uses / K) · F) = O(uses)`.
    - **Memory.** Each kernel check's working set is bounded by its segment,
      since the kernel's caches live per declaration.
  - **Tables the kernel reads are data (`Gen.Tab`).** These are the
    footprints, step → method, gated, tag and allowed. `Tab` is a search
    tree with a structurally recursive `get`.
    - **The problem with code.** A table written as code,
      `def fp (k) := if k < m then .. else ..`, makes the kernel copy the
      code's whole spine (`n` nodes) at every lookup when it substitutes
      `k`. Over a scan that is `O(n²)`, and it was the main source of the
      superlinear time the review measured (§7).
    - **With data.** A lookup in `Tab` walks `O(log n)` nodes and copies
      nothing.
  - **Cost model (corrected).** Each (step, field) use costs `O(log F)` tree
    lookup and update, plus OR/AND on bit sets of `2M` bits (`⌈2M/64⌉`
    words), plus `O(log n)` table lookups per step.
    - Per checkpoint, building and comparing the written state costs
      `O(F log F)`.
    - The total is `O(uses × (log F + log n + ⌈M/32⌉))`.
    - The `M` factor is a deliberate exception to "linear". It is a word
      count, invisible in the measurements up to `M = 200` (99 and 198
      methods cost the same, in the review's runs and in §7). The
      output-sensitive alternative, reach sets only over methods that have
      non-CF partners, is noted in §8.
- **Linear helper lemmas, for the per-design side:**
  - `range_split`, `range_one`, `range_all`: a fact about every step from one
    lemma per step, combined by a balanced tree (O(n) nodes, no `match` on
    numerals).
  - `tagComm_of_groups`: `TagComm` from one decided membership fact per step
    (`tag x ≠ 0 → tag x < G ∧ x ∈ mem (tag x)`) and one `GroupComm` per group.
  - `groupComm_of_chain` (with `chainOK` and `chainOK_excl`): an exclusion
    group from its chain-form disjointness fact, where each member is disjoint
    from the OR of the earlier ones. This is exactly the shape of
    `Hw.disjoint_N`.
  - `groupComm_pair`: a semantic pair from its `Comm` lemma.
  - `nba_of_groups`: never-both-fire between methods, from method groups
    (`ng b`, `gmeth g`, `gsteps g`) with one chain-form disjointness fact per
    group. The facts are linear in the methods; there is never one per pair.
  - `comm_of_excl`, `comm_of_id`, `comm_of_sim`, `comm_of_idle_outputs`,
    `Eff.comm_on`, `Eff.apply_eq_self`, `Eff.keeps_sorts`.
  - `ParentOrder.of_check`: a decidable check for concrete groupings (with
    `respects` and `groupsOK`).

`#print axioms` (from `chk3/ax.lean`):

```
'Gen.serial' depends on axioms: [propext, Quot.sound]
'Gen.blocks_serial' depends on axioms: [propext, Classical.choice, Quot.sound]
'Gen.Design.blockOK' depends on axioms: [propext, Classical.choice, Quot.sound]
'Gen.Design.key_mono' depends on axioms: [propext, Classical.choice, Quot.sound]
'Gen.all_orders' depends on axioms: [propext, Classical.choice, Quot.sound]
'Gen.all_orders_parent' depends on axioms: [propext, Classical.choice, Quot.sound]
'Gen.Design.ok_of_chain' depends on axioms: [propext, Quot.sound]
'Gen.Design.scan_ok' depends on axioms: [propext, Quot.sound]
'Gen.nba_of_groups' depends on axioms: [propext, Quot.sound]
'Gen.tagComm_of_groups' does not depend on any axioms
'Gen.groupComm_of_chain' depends on axioms: [propext]
'Gen.useOfBits_ul' depends on axioms: [propext, Quot.sound]
```

`Example.lean` (printed at its end):

- `Ex.Scan.ok` (checkpointed) depends on `[propext, Quot.sound]`;
  `Ex.Scan.bad_annotation` and `Ex.Scan.sb_not_enough` on `[propext]`.
- `Ex.all_orders` and `Ex.every_order` depend on `[propext, Classical.choice,
  Quot.sound]` plus the three per-call `bv_decide` axioms (from
  `schedule_sound` and the semantic pair), as every emitted file does today.

## 3. What the generator emits per design

Emit the `Run`/`Scan` sections only when the design exports at least one CF
pair, the same trigger as round 3's `withOrders`. With only SB/SBR/C pairs, π
is σ and the headline theorem is `schedule_sound` with `P` unused.

| item | size | proof |
|---|---|---|
| `Run.fp.<step>` with **sorted, duplicate-free** `R`, `W`, `O` | the footprint table (already emitted) | none |
| `Run.fpT : Gen.Tab FP` (footprints by step, as **data**), `Run.fpOf k := Run.fpT.get k` | n leaves | per-step `rfl` equations `Run.fp_k` |
| `Run.U`/`Run.C` with `Run.rd` indices renumbered to the sorted `R` | as today | none |
| one read-copy step per extra read of each argument-less value method (§4), with its own result field and `Agree` conjunct | (K − 1) × that method's footprint | none |
| `inductive Method` (one constructor per method; `v (j : Fin K)` for read copies), `Method.toNat`, `Method.ofNat` (a `Tab`), `Ann.M` | M | `Scan.meth_ok` checks `ofNat ∘ toNat = id` |
| `Scan.meth` (step → method + 1), `Scan.stepsOf` (method → steps, from `dMethodSteps`), both `Tab` | n, n | `Scan.meth_ok`, one linear `decide +kernel` |
| `Scan.tag`, `Scan.mem` (exception groups, members in chain order), `Scan.groups` | n, Σ group sizes | membership: one linear `decide +kernel` |
| `Ann.hasEnableM`, `Ann.enM i` by `Method` (`| .m => i.EN_m`, literally the port) | M | none |
| `Design.gated` (proof-internal) | M | `hge : gated ⊆ hasEnable`, by `rfl`/decide |
| `Ann.sbM b`, `Ann.sbrM b` by `Method` | the exported SB / SBR pairs | none; `Scan.meth_ok` checks there are no self-pairs |
| `Ann.exclM b` | twice the exported C pairs | none |
| `Ann.ng` (method → never-both-fire group), `Ann.gmeth g`, `Ann.gsteps g` (members and their steps, in chain order) | M, M, n | `Gen.nba_of_groups`: one decided membership fact per method and per step, one chain lemma per group (from `Hw.disjoint_N`, as `groupComm_of_chain`) |
| `Ann.unvM b` | normally empty; see §5 | none (it narrows the contract, visibly) |
| numbered views `Ann.sb := Ann.byNum Ann.sbM`, ... | 1 line each | definitional |
| `Run.fire` | `WILL_FIRE` for steps of gated methods and steps in exclusion groups; `true` for every other step | none |
| `Run.forallSteps` | one balanced `range_split` tree, n leaves | reused by every per-step fact |
| table equations `Run.fpOf k = Run.fp.<s>` (and `uOf`, `cOf`, `fire`) | 4 per step that needs an idle lemma | `rfl` (an O(log n) walk down the if-tree) |
| `Run.keeps` | `fun rs => rfl` per step through the tree, plus one `decide +kernel` over `List.all` for the flag sorts | rfl / kernel |
| `Run.idle_k` | **only** for steps of gated methods and steps in exclusion groups | `Gen.Eff.apply_eq_self`, `simp` with the step's table equations; for state-independent ready outputs, `comm_of_idle_outputs` with round 3's owner table |
| `Run.hen` | one tiny lemma per step through the tree | `simp` |
| one `GroupComm` per exclusion group | groups used | `groupComm_of_chain` + `simp` with the chain fact |
| one `Comm` lemma per semantic pair | pairs found | `Gen.Eff.comm_on` + `bv_decide`, then `groupComm_pair` |
| `Scan.D`; checkpoints `Scan.cpJ := Tr.ofList d [...]` every `K = max(2000, F)` uses; `Scan.segJ` per segment; `Scan.ok : Scan.D.Ok := ok_of_chain [...] ⟨seg0, ...⟩` | O(uses) in total | kernel, one bounded check per segment |
| `all_orders` | about 15 lines | `Gen.all_orders_parent` + `schedule_sound` |

Everything is linear in the footprint table, the step count, the method count
and the exported annotations. No item enumerates step pairs or method pairs.

**Table shapes (required, so that no lookup costs O(table)):**

- **Tables the kernel reads during the scan** are `Gen.Tab` data: `fp`,
  `meth`, `gated`, `tag`, `allowed`, and `Method.ofNat`.
- **Tables keyed by method** are `match` on `Method`. Each lookup is one
  `casesOn` step.
- **Per-group tables** (`Scan.mem`, `Ann.gmeth`, `Ann.gsteps`) are `Tab` too.
- **Group dispatch** (`tagComm_of_groups`'s `GroupComm` per group, and
  `nba_of_groups`'s chain per group) goes through one `range_split` tree over
  the group numbers, as `Example`'s `Run.tag_comm` does. A `G`-way `omega`
  case split is not used.
- **`if k < m then ..` code over a bound variable** is used only for tables
  the kernel never evaluates repeatedly (`Run.uOf`, `Run.cOf`, `Run.fire`).
  Its spine is copied at each unfolding.

- **The per-step pattern above was measured** (`tools/mkper2.py`, n rules
  with round-3 if-tree tables, idle lemmas with table equations, `range_split`
  dispatch): n = 100 / 200 / 400 took 4.5 / 9.2 / 18.1 s user. Example's old
  pattern (an n-arm `match` with a `k + n` catch-all, `simp` unfolding whole
  tables, `decide` over all step pairs for `htag`) took 9.5 / 26.2 / 81.1 s in
  the review's measurement.
- **Table facts use `decide +kernel`.** Elaborator `decide` is slow even when
  linear. A `∀ k < n` fact goes through `List.all` over `List.range n`, which
  is cheaper for the kernel than `Nat.decidableBallLT`: 6.4 s against 21 s at
  n = 400 in `chk2/P_400`.
- **Data definitions the kernel alone reads** (`Run.fpOf` trees, `Scan.*`,
  `allowed`) can be marked `noncomputable`, so the code generator does not
  compile them. Without `maxHeartbeats 0`, a 4096-leaf `def fp` hits the LCNF
  heartbeat limit. Note that `decide +kernel` does not need compiled code.

**Integration checklist (round 3 → round 4):**

1. `Run.Val`: add `instance : Gen.FlagVal Run.Val := ⟨toBool, .vBool, fun _ => rfl⟩`.
2. `Run.Eff` becomes `Gen.Eff Run.Val`, `Run.St` becomes `Gen.St Run.Val`,
   `Run.step` becomes `Gen.stepOf (Run.eff i)` and `Run.run` becomes `Gen.run`.
   `Run.Eff.comm` is `Gen.Eff.comm`.
3. `Run.Comm` (for every state) gives `Gen.Comm Run.Inv` directly.
4. Keep `Run.link` / `Run.final` / `seg_run` to connect σ's `Gen.run` to
   `stepSequential`.
5. New tables and lemmas: `Run.fsort`, `Run.Inv`, `Run.init_ok`, `Run.fire`,
   `Run.keeps`, `Run.idle_k`, `Run.hen`, `Scan.*`, `Ann.*`.
6. Emit `R`, `W`, `O` sorted and duplicate-free (a duplicate read makes
   `sortedK` and so the scan fail). Renumber `Run.rd` **and** the index lists
   of `sbView` (round 3's `stepEffDefs`: `let v := i.view (Run.rd rs j)`). An
   alternative is to keep round 3's `R` order for `Run.rd`, emit a separate
   sorted footprint for the scan, and prove the two have the same members.
7. Delete round 3's `CF.methods`, `CF.stepsOf`, `CF.orders`, `CF.Order`,
   `CF.iv`, `CF.ivOf`, `CF.checks`, `Run.free`, `Run.blocks`,
   `Run.moves_range`, the per-order `Run.seg`/`Run.run` plumbing,
   `exported_CF`, `ordersOf`'s swapped orders and `orderBlock`. Keep
   `CF.unpack` only if the tables still need it.

## 4. How each kind of step is classified (resolved questions)

Line numbers are for `src/comp/app/verifyschedule.hs` as of 2026-11-01 23:53
(read only; round 3 is editing it, so function names are given too).

- **Rules: firing is a cycle constant, the hardware WILL_FIRE.**
  - `ident` (2008): `side == Seq, "WILL_FIRE_" ... -> atHardware <$> defTerm
    Seq name` (2013).
  - `analyse` takes `fire0 <- ident Seq aTBool (stFire st)` (2410).
  - In `buildSeq` every effect is gated: `fa c = tAnd [fireT, c]` (3050),
    calls under `fa` (3063-3065), flags likewise (3059).
  - A false guard is a sticky `guard_violated` flag (3053).
  - A step that does not fire changes nothing but its unconditional outputs
    (3052, `EUncond`).

  So "never both fire" facts hold in every state. Rules need `Run.fire` and
  idle lemmas only when they are in an exclusion group; otherwise
  `fire := true` (the review's point).
- **Action methods with an enable** (`dEnables`; `enableOf` at 2507).
  WILL_FIRE implies EN: `WILL_FIRE_incA i := i.EN_incA`, and `WILL_FIRE_put_T
  i := CAN_FIRE_put_T i && i.EN_put` for a split method. One of two cases
  applies:
  - **Gated** (paths do not pass through it): the step's outputs when idle
    do not depend on the state (`RDY := CAN_FIRE_m i`), so an idle step
    commutes with everything.
  - **Passable**: the step's outputs read the step's view even when idle. In
    bus_2m_2s these are the action-value results and the ready signals that
    read same-cycle wires (`data/bus_2m_2s/out.lean:1310`). For such a
    method, `fire := true`; it cannot take part in `nba`.
- **always_enabled action methods** have no enable port (`enableOf` gives
  `TLit True`, 2507). They are not in `hasEnable`, so the contract does not
  force them into `P`. They are passable.
- **Value methods** are read-only steps, passable, `fire := true`. They may be
  in `P` (where the parent reads them) or not.
  - **Repeated reads (read copies).** A value method without arguments may be
    read by several parent rules in one cycle, as a register's `_read` is.
    bsc allows this because such a method is CF with itself. Whether a parent
    does so is a property of the parent and cannot be seen from the module.
    - **What the generator emits.** For each such method (`mtKind ==
      ValueMethod`, `ptArgs` empty), `K` read copies (`K = 2` by default, an
      option). Each copy is its own step, placed right after the method's
      step in σ. It reads the same fields, sets its own result field, and has
      the same flags.
    - **Annotations.** Each copy is its own method (`Method.v j`), with the
      original's annotations against every other method, and CF with the
      other copies.
    - **The contract.** A parent reading `v` `r ≤ K` times calls `v 0`, ...,
      `v (r-1)` in its order.
    - **Why this is sound.** Each copy's result is a module output that
      `Agree` compares with the hardware's port. So the theorem says every
      read, at its own place in π, returns the hardware value. A firing step
      that changed a field `v` reads between two reads would make some copy
      disagree. This is exactly the guarantee the review asked for, and it is
      checked rather than assumed.
    - **Why the scan still passes.** A copy joins its original's read blocks
      (adjacent readers), reaches and is reached by the same methods, and
      conflicts with nothing else.
    - **Cost.** `(K − 1)` × the method's footprint in uses, and `(K − 1)`
      extra method bits per such method. Both are linear.
    - **The narrowing that remains.** A parent reading one method more than
      `K` times in a cycle is not covered. The headline docstring says so,
      and `K` is visible in `Method`. Removing the bound would need a generic
      theorem that the scan's verdict for one copy holds for any number of
      copies (§8).
    - **No claim about the corpus.** The previous round said repeated reads
      do "not occur in the corpus". That claim is withdrawn: the corpus holds
      only modules, not their parents.
    - `Example.lean`: getR has two copies (`Method.getR 0/1`), and
      `every_order` includes three orders that read it twice.
  - **Unread value methods on a path.** Paths pass through a value method the
    parent does not read, and the theorem keeps its output equal to σ's. For
    a → v → b, bsc can correctly export a CF b (its path search does not pass
    through methods, `ASchedule.hs` 3082), yet the scan fails. This is a
    known source of false failures. §7 ("methods passed directly") counts
    none in the corpus. That is a count of the modules' own paths, which is
    a property of the module, unlike repeated reads.
- **Methods bsc split into several steps** (`extractStep` 1003,
  `dMethodSteps` / `stepsMap` 734). All of a method's steps share the method's
  key, so they sit together inside its parent rule, ties in σ's order. The
  scan rejects a path from one step of a method to another. bsc splits by
  disjoint conditions, so two conflicting steps of one split method form an
  exclusion group.
- **SB and SBR are now different.** bsc changes CF or SB to SBR when a rule
  lies on a path between the methods (`changeCFOrSBToSBR`, `ASchedule.hs`
  2956, via `hasRuleMap`). That rule makes the pair uncallable from one atomic
  parent rule. The model reflects this:
  - `P` groups calls by parent rule and `ParentOrder.sbr` forbids SBR pairs
    within a group;
  - the scan requires a path through a rule (or another passable step) to be
    SBR (or C/nba/unv);
  - the conclusion states contiguity.

  `Example.lean`'s `Scan.sb_not_enough` and `Scan.sbr_ok` show the
  difference. Corpus: no design gains a failure from this (§7).
- **ME is never exported** (`sME = []`, `ASchedule.hs` 2982).
- **Named tables, and what is checked.**
  - **Readable by name.** The headline's tables are written by `Method`
    name, with `Ann.enM` naming the `EN_` port, so a script can compare them
    with bsc's schedule output and the `.ba` file (§1).
  - **What `Scan.meth_ok` adds.** It checks that the numbering is consistent:
    step labels against the method-step table, `ofNat` against `toNat`, and
    no self-annotations.
  - **What it cannot catch.** The review notes this check is internal to the
    generator: both of its sides come from the generator. It cannot catch,
    say, a rule step labelled as a value method. What it does achieve is that
    every numbered table is either derived by definition from a named one or
    checked against one, so the only trusted table is the step labelling, as
    trusted as the rest of the emitted file (`Run.fp`, `Run.U`, ...).
  - **An external check.** A script compares `Scan.meth`/`Method` with the
    `.ba` file's method list and rule list.

## 5. Exceptions, and bus_2m_2s

An exception is a pair of steps that share a field in conflicting ways yet
commute. There are three kinds:

- **`nba` (never both fire, between methods).** This applies only when both
  methods are gated, because `fire` is WILL_FIRE only for gated steps.
  - It is stated **per group, never per pair**. Each method has a group
    (`Ann.ng`), each group lists its methods (`Ann.gmeth`, which is also each
    member's `nba` list) and their steps in chain order (`Ann.gsteps`).
  - `Gen.nba_of_groups` derives the pairwise fact from one chain-form
    disjointness lemma per group, as `groupComm_of_chain` does for tags. The
    groups are bsc's `Hw.disjoint_N` chains restricted to gated methods.
  - Size: M + n table entries and one lemma per group. With k pairwise
    disjoint gated methods that all write one field, this is one group of k,
    not k(k−1)/2 pair lemmas (the review's state-machine example). The previous round counted bus_2m_2s's (ms0, ms1)
  `_bms_request_put` pair as `nba`. That was wrong: both are passable, since
  their RDY reads the view when idle.
- **Exclusion groups (`Scan.tag`).** Cliques of steps that never both fire
  (rules, steps of one split method, gated methods), taken from bsc's
  disjointness the way `disjointChains` groups them (2770). The proof is
  `groupComm_of_chain` with the chain fact; the members' `fire` must be
  WILL_FIRE.
- **Semantic pairs (`Scan.tag`, two members).** Steps that conflict but
  commute, for example Example's clearA/clearB. The proof is
  `Gen.Eff.comm_on` + `bv_decide`, then `groupComm_pair`.

**How the generator finds them.** It runs the same scan in Haskell. On a
failure it walks back to the offending path and, for each edge, tries in
order:

1. **A shared bsc exclusion group.** If both ends are in one
   `Hw.disjoint_N` chain, the chain's steps become one exclusion tag (for
   step-level edges), or the methods' group joins `Ann.ng` (for gated
   methods).
2. **A `bv_decide` commutation** for a remaining step pair: a two-member
   semantic tag.

Groups come first, so the number of `nba` entries is bounded by the methods
in bsc's emitted disjointness chains (each method in at most one group), and
the number of semantic lemmas by the failing edges. The generator then
re-runs the scan; the work is proportional to the failures. Any pair still
failing goes into `Ann.unvM`, with a warning that names the pair, its path
and the rule or passable method on the path (the reason it counts as
"through"). It is never rewritten into `Ann.sbM`.

**bus_2m_2s, corrected** (`scan2.py`, mirroring the proved scan: 5 gated
methods, `nba` only between gated ones). 16 exported CF pairs fail, not 15:

- **1 direct pair:** (ms0, ms1)`_bms_request_put`. The two steps both read and
  flag field 4 and both write field 5. They are passable, so `nba` does not
  apply. A two-member semantic tag should clear it: the `Comm` lemma
  case-splits on which method fires (`disjoint_ms0_ms1`, `ready_` facts,
  WILL_FIRE = EN), and an idle step sets only its RDY from fields the other
  does not write. This is not yet proved.
- **15 through pairs:** {bus_request ×2, response_put ×3} → {request_get ×3}.
  Each path runs through a passable method. They need the idle-footprint
  refinement: a passable step that does not fire acts as an effect reading
  only its view and writing only its outputs.
  - The refinement must also require `indep(idle x, full y)` and
    `indep(full x, idle y)` whenever never-both-fire is used. The previous
    round's `idlefp.py` "0 pairs" figure skipped that check and is withdrawn.
  - The refinement is not proved in `Generic.lean`.
- **Status:** the headline is emitted with these 16 pairs in `Ann.unv`.
  `chk2/R_bus_2m_2s.lean` decides `scan = true` with them in 0.4 s user;
  `chk2/V_bus_nounv.lean` decides `scan = false` without them.
- **With every enabled method gated** (unsound here), 0 pairs fail. That is
  the previous round's `mkcheck.py` setting.

## 6. The pairs round 2/3 left unrealized (`dUnrealized`)

There are no swaps any more, so the notion goes away. Every exported CF pair
is covered by the one theorem, or the scan fails, names the pair and its
path, and the pair lands in `Ann.unv`. bus_2m_2s's unrealized pairs are among
the 16 of §5.

## 7. Measurements

**Corpus check under the proved theory** (`python3 scan2.py data/*`). "Fail"
counts failing method pairs: direct, then through a rule or passable method.
"Methods passed directly" re-runs with passable method steps passing reach on
as direct (bsc's view), to count false failures from value methods in
between.

| design | M | gated | SB | SBR | CF | fail (direct / through) | methods passed directly | all EN gated (unsound) |
|---|---|---|---|---|---|---|---|---|
| bus_2m_2s | 14 | 5 | 0 | 26 | 65 | **16 (1 / 15)** | 16 | 0 |
| mkAes | 3 | 2 | 2 | 0 | 1 | 0 | 0 | 0 |
| mkAssertedSchedules | 3 | 3 | 0 | 0 | 3 | 0 | 0 | 0 |
| mkBufferControl | 9 | 9 | 0 | 0 | 36 | 0 | 0 | 0 |
| mkDeblockFilter | 6 | 6 | 0 | 0 | 15 | 0 | 0 | 0 |
| mkDutLogic | 71 | 0 | 70 | 0 | 2415 | 0 | 0 | 0 |
| mkLeanNames | 3 | 1 | 1 | 1 | 1 | 0 | 0 | 0 |
| mkLookupQ | 2 | 2 | 0 | 0 | 1 | 0 | 0 | 0 |
| mkMCR | 7 | 1 | 0 | 0 | 21 | 0 | 0 | 0 |
| mkMethodRelations | 5 | 3 | 3 | 4 | 3 | 0 | 0 | 0 |
| mkPrediction | 9 | 9 | 0 | 0 | 36 | 0 | 0 | 0 |
| mkPump | 2 | 1 | 0 | 0 | 1 | 0 | 0 | 0 |
| mkSplitMethod | 3 | 2 | 1 | 0 | 2 | 0 | 0 | 0 |
| mkValueAfterAction | 3 | 1 | 0 | 1 | 2 | 0 | 0 | 0 |
| sysManyMeths | 200 | 0 | 0 | 0 | 19900 | 0 | 0 | 0 |

What the table shows:

- The SB/SBR distinction costs nothing on the corpus: every path through a
  rule between two methods is exported SBR.
- No false failures come from value methods in between.
- bus_2m_2s is the only design that needs `unv`.

**Footprint tables** (previous round, unchanged; * = no methods, table
re-derived by `estimate.py`):

| design | steps | fields | incidences | accesses | blocks | conflicting step pairs | CF-exported methods in conflicting pairs |
|---|---|---|---|---|---|---|---|
| sysManyMeths | 200 | 400 | 400 | 400 | 400 | 0 | 0 |
| sysFloatTest* | 790 | 226 | 8651 | 6216 | 2698 | 87737 | 0 |
| sysBRAM0Test* | 776 | 1580 | 8016 | 6276 | 3528 | 66053 | 0 |
| sysArithPipe* | 227 | 258 | 2537 | 1929 | 874 | 10615 | 0 |
| sysTrafficBRAM* | 235 | 2024 | 5021 | 3388 | 2852 | 897 | 0 |
| mkCregInlineBlowup* | 4096 | 8194 | 24576 | 16384 | 8194 | 0 | 0 |
| mkAes | 19 | 197 | 1210 | 879 | 534 | 123 | 2 |
| mkDutLogic | 71 | 76 | 273 | 271 | 78 | 70 | 70 |
| bus_2m_2s | 15 | 43 | 171 | 155 | 67 | 39 | 39 |
| mkMethodRelations | 6 | 15 | 35 | 29 | 21 | 8 | 7 |
| mkSplitMethod | 5 | 14 | 28 | 23 | 16 | 3 | 3 |
| mkPump | 3 | 9 | 18 | 14 | 11 | 2 | 2 |

Exceptions needed: bus_2m_2s 16 (in `unv`; §5), every other design 0.

**Lean times** (`/usr/bin/time`, `lean -j1`, `nice`, load average 14–47):

| what | wall | user | peak RSS |
|---|---|---|---|
| `Generic.lean` | 4.1–6.3 s | 3.4–5.4 s | 0.65 GB |
| `Example.lean` (against `build/Generic.olean`) | 2.5–7.9 s | 2.1–3.6 s | 0.77 GB |

**Kernel time of the scan at scale (this round, `chk3/`).**

- **The instances.** The sysFloatTest footprint table, replicated 1/2/4/8
  times, with every (8 × rep)-th step a method (M = 99) and all pairs
  allowed. This is the review's setup (`tools/mkchunk.py est
  data/sysFloatTest EVERY REP CHUNK`).
- **How they were run.** One `lean -j1` process at a time, under `nice` and
  `timeout`.
- **What the kernel column shows.** Total kernel time over all segments,
  taken from the profiler's cumulative `type checking took` lines.

| uses | fields | tables | scan | segments | kernel | wall | peak RSS |
|---|---|---|---|---|---|---|---|
| 6,216 | 226 | if-code | one piece (review, u1m99) | 1 | 14 s | 16.9 s | 2.0 GB |
| 12,432 | 452 | if-code | one piece (review, u2m99) | 1 | 27.9 s | 32.8 s | 4.3 GB |
| 24,864 | 904 | if-code | one piece (review) | 1 | 85 s | — | 10.9 GB |
| 6,216 | 226 | if-code | checkpointed | 4 | 9.3 s | 13.0 s | 1.14 GB |
| 12,432 | 452 | if-code | checkpointed | 7 | 22 s | 28.1 s | 1.47 GB |
| 24,864 | 904 | if-code | checkpointed | 13 | 68 s | 84.3 s | 2.18 GB |
| 49,728 | 1,808 | if-code | checkpointed | 25 | 234 s | 278 s | 3.79 GB |
| 24,864 | 904 | **`Tab`** | one piece | 1 | 49.9 s | 62.7 s | 5.9 GB |
| 6,216 | 226 | **`Tab`** | checkpointed | 4 | 9.0 s | 11.9 s | 1.05 GB |
| 12,432 | 452 | **`Tab`** | checkpointed | 7 | 23.1 s | 29.0 s | 1.26 GB |
| 24,864 | 904 | **`Tab`** | checkpointed | 13 | 49.4 s | 63.5 s | 1.76 GB |
| 49,728 | 1,808 | **`Tab`** | checkpointed | 25 | 120 s | 158 s | 3.09 GB |

What the timings show (the review's cost finding, re-measured):

- **The previous cost model was wrong, as the review said.** The
  superlinear time came mainly from tables written as `if` code over `k`.
  The kernel copies such code's spine (`n` nodes) at every lookup, which
  makes the scan `O(n²)`. With `Tab` data, the 24,864-use scan in one piece
  drops from 85 s to 49.9 s.
- **Memory came from one unbounded kernel check, also as the review
  said.** Checkpointing bounds each check: the 24,864-use case falls from
  10.9 GB (one piece, if-code) and 5.9 GB (one piece, `Tab`) to 1.76 GB.
- **With both fixes, growth is close to linear but not exactly.**
  - Kernel time per 1,000 uses is 1.45 / 1.86 / 1.99 / 2.41 s from 6k to
    50k uses. Time per segment of 2,000 uses goes 2.9 → 3.3 → 3.9 → 4.9 s
    (mean).
  - Within the 50k file, segments drift from about 4 s to about 6 s with
    position. This was not traced further. It is not the bit-set width
    (fixed at 2M = 198 bits), and each checkpoint is an independent literal.
  - Peak RSS grows with the file. It holds the elaborated environment (the
    50k file is 2.4 MB, with 25 written states of 1,808 fields each), not a
    kernel working set: the largest segment check is the same size in every
    file.
- **Checkpoint size.** With a fixed 2,000-use segment, the written states
  grow as (uses / 2000) × F. That is quadratic in this replicated synthetic
  instance, where F grows with the uses. The generator therefore uses
  `K = max(2000, F)`, which keeps the written states within the uses.
- **Real designs with methods are small.** mkAes has 879 uses and
  sysManyMeths 400. Designs without CF pairs emit no scan at all. The
  largest corpus table (mkCregInlineBlowup, 16,384 uses, no methods) would
  be about 9 segments, by the rows above about 30 s and under 1.5 GB.

The previous round's one-piece measurements on real tables, which still
apply to small designs (`chk2/*.log`; whole file, including about 0.4 s to
load `Generic`):

| instance | uses | methods | widest step | type checking | wall | peak RSS |
|---|---|---|---|---|---|---|
| mkMethodRelations (real) | 29 | 5 | 6 | 0.03 s | 0.5 s | 0.49 GB |
| bus_2m_2s (real, 16 unv) | 155 | 14 | 20 | 0.22 s | 0.7 s | 0.52 GB |
| mkAes (real) | 879 | 3 | 148 | 0.45 s | 1.2 s | 0.56 GB |
| mkDutLogic (real) | 271 | 71 | 5 | 0.51 s | 1.1 s | 0.56 GB |
| sysManyMeths (real) | 400 | 200 | 2 | 1.1 s | 2.0 s | 0.64 GB |
| sysConflictFreeOKLarge table, each step a method (wide case) | 9753 | 5 | 3077 | 23.1 s | 28.5 s | 4.1 GB |

**Per-step lemmas** (`chk2/P_*.log`, §3): n = 100 / 200 / 400 took 4.5 / 9.2 /
18.1 s user and 0.85 / 1.04 / 1.46 GB peak.

## 8. Open issues

1. **bus_2m_2s: 16 of 65 exported CF pairs are narrowed to SBR (`Ann.unv`).**
   This is a coverage gap in the headline for that design, and for any
   design whose method-to-method paths pass through value, action-value or
   always_enabled methods. Two pieces of work would close it:
   - the idle-footprint refinement, with its `indep(idle, full)` side
     conditions, for the 15 through pairs;
   - the (ms0, ms1) `_bms_request_put` semantic tag (§5), for the direct
     pair.

   Neither is proved yet. The generator's per-pair warning names the
   passable method on each path.
2. **Read copies bound repeated reads at `K` per cycle.** Unbounded reads
   would need a generic theorem: if the scan passes with one copy, the
   certificate (blocks and keys) extends to any number of adjacent copies.
   The proof would rebuild `key_mono` over an extended step set.
3. **Unread value methods on a path** cause false scan failures (§4). There
   are none in the corpus's module paths.
4. **The `M` factor in the scan** (bit sets `2M` wide) is a deliberate
   exception to linearity. It is not visible up to M = 200. An
   output-sensitive variant would give reach bits only to methods with a
   non-CF partner, or to methods in a path's endpoint set.
5. **Within-file drift of segment time** (about 4 s → 6 s over 25 segments
   at 50k uses), not traced.
6. **The exception search (§5) and the read copies** are specified, not
   implemented in the generator.
7. **`Scan.meth_ok` is internal to the generator.** The external check
   against the `.ba` file is a script (§4), not part of the Lean file.
