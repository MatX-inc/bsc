/-
Generic.lean: the all-orders theory, once for every design.

A design's cycle is a list of steps, each an effect on a State given by field
numbers (`Gen.Eff`, as `Run.Eff` in the emitted files).  The schedule's order
is `List.range n`.  This file proves, with no design in sight:

  (A) `Gen.serial`: two orders of the same steps end in the same state if
      every pair of steps they order differently commutes;
  (B) `Gen.blocks_serial`: a block certificate (per field, the schedule's
      accesses in blocks: a write alone, a run of reads, a run of flag-sets,
      or a run of steps of one exception group) makes every order that
      keeps each field's block indices in order end in the schedule's state;
  (C) `Gen.all_orders`: one decidable scan (`Gen.Design.scan`) of the
      footprint table, linear in its size, finds the blocks and, for every
      method, the methods that reach it through conflicts, directly or
      through a rule; if every such pair is one the parent must order anyway
      (and, through a rule, may not call from one parent rule), then every
      way the parent may call the methods (one list of calls per parent
      rule) extends to a full order of the steps that keeps each parent
      rule's calls together and ends in the schedule's state.
      `Gen.all_orders_parent` states the parent's side as the exported
      annotations (`Gen.ParentOrder`: SB, SBR, C).

Commutation is asked only from states satisfying an invariant every step
keeps (`Inv`, e.g. "every field holds a value of its sort"): a step that does
not fire rewrites a flag `v` as `ofBool (toBool v)`, which is `v` only when
`v` is a Boolean.  The last section has the lemmas a design proves its
side with: steps that do not fire, exclusion groups (from chain-form
disjointness facts), semantic pairs, sorts, and a linear per-step dispatch.

Only core Lean.
-/
set_option autoImplicit false
set_option linter.unusedVariables false
set_option linter.unusedSimpArgs false
set_option linter.deprecated false
open scoped List

namespace Gen

/-! ## Values, states and effects -/

/-- A type of field values with a Boolean reading and a way to store a
    Boolean (a flag).  In an emitted file: `Run.Val`, `toBool`, `.vBool`. -/
class FlagVal (V : Type) where
  toBool : V → Bool
  ofBool : Bool → V
  toBool_ofBool : ∀ b, toBool (ofBool b) = b

/-- A State as a function of the fields' numbers. -/
abbrev St (V : Type) := Nat → V

/-- What a step does: the fields it reads (`R`), the fields it sets (`W`) to
    values computed from them (`U`), and the flags (`O`) it sets when
    conditions computed from them hold (`C`). -/
structure Eff (V : Type) where
  R : List Nat
  W : List Nat
  O : List Nat
  U : List V → List V
  C : List V → List Bool

/-- The fields a step reads, sets and flags, as a footprint. -/
abbrev FP := List Nat × List Nat × List Nat

def Eff.fp {V : Type} (e : Eff V) : FP := (e.R, e.W, e.O)

variable {V : Type} [FlagVal V]

/-- Field `k` once a step ran that read `rs`, given its value `old` before. -/
def Eff.val (e : Eff V) (rs : List V) (old : V) (k : Nat) : V :=
  let v1 := match (e.W.zip (e.U rs)).lookup k with
    | some v => v
    | none => old
  match (e.O.zip (e.C rs)).lookup k with
    | some c => FlagVal.ofBool (FlagVal.toBool v1 || c)
    | none => v1

def Eff.apply (e : Eff V) (s : St V) : St V := fun k => e.val (e.R.map s) (s k) k

theorem lookup_zip_none {α : Type} (l : List Nat) (vs : List α) (k : Nat) (h : l.contains k = false) :
    (l.zip vs).lookup k = none := by
  induction l generalizing vs with
  | nil => rfl
  | cons a l ih =>
    cases vs with
    | nil => rfl
    | cons v vs =>
      simp only [List.contains_cons, Bool.or_eq_false_iff] at h
      simp only [List.zip_cons_cons, List.lookup_cons]
      rw [h.1]; exact ih vs h.2

theorem lookup_zip_mem {α : Type} (l : List Nat) (vs : List α) (k : Nat) (v : α)
    (h : (l.zip vs).lookup k = some v) : l.contains k = true := by
  cases hc : l.contains k
  · rw [lookup_zip_none l vs k hc] at h; cases h
  · rfl

theorem Eff.apply_other (e : Eff V) (s : St V) (k : Nat) (hw : e.W.contains k = false)
    (ho : e.O.contains k = false) : e.apply s k = s k := by
  simp only [Eff.apply, Eff.val, lookup_zip_none _ _ k hw, lookup_zip_none _ _ k ho]

theorem not_contains_of {l l' : List Nat} {r : Nat} (hr : r ∈ l)
    (h : ∀ w ∈ l', l.contains w = false) : l'.contains r = false := by
  cases e : l'.contains r
  · rfl
  · have := h r (List.contains_iff_mem.mp e)
    rw [List.contains_iff_mem.mpr hr] at this; cases this

/-- Neither step reads or sets a field the other sets or flags, but both may flag one. -/
def indep (x y : FP) : Bool :=
  x.2.1.all (fun w => !y.1.contains w && !y.2.1.contains w && !y.2.2.contains w) &&
  y.2.1.all (fun w => !x.1.contains w && !x.2.1.contains w && !x.2.2.contains w) &&
  x.2.2.all (fun o => !y.1.contains o) && y.2.2.all (fun o => !x.1.contains o)

/-- Independent steps commute: flags only accumulate. -/
theorem Eff.comm (x y : Eff V) (h : indep x.fp y.fp = true) (s : St V) :
    x.apply (y.apply s) = y.apply (x.apply s) := by
  simp only [indep, Eff.fp, Bool.and_eq_true, List.all_eq_true, Bool.not_eq_true'] at h
  obtain ⟨⟨⟨hxw, hyw⟩, hxo⟩, hyo⟩ := h
  have rx : x.R.map (y.apply s) = x.R.map s := List.map_congr_left (fun r hr =>
    Eff.apply_other y s r (not_contains_of hr (fun w hw => (hyw w hw).1.1))
      (not_contains_of hr (fun w hw => hyo w hw)))
  have ry : y.R.map (x.apply s) = y.R.map s := List.map_congr_left (fun r hr =>
    Eff.apply_other x s r (not_contains_of hr (fun w hw => (hxw w hw).1.1))
      (not_contains_of hr (fun w hw => hxo w hw)))
  funext k
  show x.val (x.R.map (y.apply s)) (y.apply s k) k = y.val (y.R.map (x.apply s)) (x.apply s k) k
  rw [rx, ry]
  show x.val (x.R.map s) (y.val (y.R.map s) (s k) k) k = y.val (y.R.map s) (x.val (x.R.map s) (s k) k) k
  simp only [Eff.val]
  cases hLx : (x.W.zip (x.U (x.R.map s))).lookup k with
  | some v =>
    have e := hxw k (List.contains_iff_mem.mp (lookup_zip_mem _ _ _ _ hLx))
    simp only [lookup_zip_none _ _ k e.1.2, lookup_zip_none _ _ k e.2]
  | none =>
    cases hLy : (y.W.zip (y.U (y.R.map s))).lookup k with
    | some w =>
      have e := hyw k (List.contains_iff_mem.mp (lookup_zip_mem _ _ _ _ hLy))
      simp only [lookup_zip_none _ _ k e.1.2, lookup_zip_none _ _ k e.2]
    | none =>
      cases hOx : (x.O.zip (x.C (x.R.map s))).lookup k <;>
      cases hOy : (y.O.zip (y.C (y.R.map s))).lookup k <;>
      simp only [FlagVal.toBool_ofBool, Bool.or_right_comm]

/-! ## (A) Serializability

Two orders of the same steps end in the same state when every pair they run
in opposite orders commutes.  (Conflict serializability: the first step of
the new order commutes with every step the old order runs before it, so it
moves to the front; the rest follows by induction.) -/

/-- Run steps one after the other. -/
def run {α β : Type} (step : α → β → β) : List α → β → β
  | [], s => s
  | k :: l, s => run step l (step k s)

/-- Steps `x` and `y` commute, from every state satisfying `Inv` (an invariant
    every step keeps, e.g. that each field holds a value of its sort). -/
def Comm {α β : Type} (Inv : β → Prop) (step : α → β → β) (x y : α) : Prop :=
  ∀ s, Inv s → step y (step x s) = step x (step y s)

theorem Comm.symm {α β : Type} {Inv : β → Prop} {step : α → β → β} {x y : α}
    (h : Comm Inv step x y) : Comm Inv step y x :=
  fun s hs => (h s hs).symm

/-- Every step keeps `Inv`. -/
def Keeps {α β : Type} (Inv : β → Prop) (step : α → β → β) : Prop := ∀ k s, Inv s → Inv (step k s)

/-- A step that commutes with every step of a list can run before the list. -/
theorem run_move {α β : Type} (Inv : β → Prop) (step : α → β → β) (hk : Keeps Inv step) (a : α)
    (l₁ l₂ : List α) (h : ∀ z ∈ l₁, Comm Inv step z a) :
    ∀ s, Inv s → run step (l₁ ++ a :: l₂) s = run step (a :: (l₁ ++ l₂)) s := by
  induction l₁ with
  | nil => intro s _; rfl
  | cons z l ih =>
    intro s hs
    show run step (l ++ a :: l₂) (step z s) = run step (l ++ l₂) (step z (step a s))
    rw [ih (fun w hw => h w (List.mem_cons_of_mem _ hw)) (step z s) (hk z s hs)]
    show run step (l ++ l₂) (step a (step z s)) = _
    rw [h z (List.mem_cons_self ..) s hs]

/-- **(A)** Two orders `π` and `σ` of the same distinct steps end in the same
    state if every two steps they run in opposite orders commute. -/
theorem serial {α β : Type} (Inv : β → Prop) (step : α → β → β) (hk : Keeps Inv step) :
    ∀ (π σ : List α), σ.Nodup → π.Perm σ →
      (∀ x y, [x, y] <+ σ → [y, x] <+ π → Comm Inv step x y) →
      ∀ s, Inv s → run step π s = run step σ s
  | [], σ, _, hp, _, s, _ => by rw [List.Perm.nil_eq hp]
  | a :: π', σ, hnd, hp, hc, s, hs => by
    have ha : a ∈ σ := hp.subset (List.mem_cons_self ..)
    obtain ⟨l₁, l₂, rfl⟩ := List.append_of_mem ha
    have hna : a ∉ l₁ := by
      intro h
      have := (List.nodup_append.mp hnd).2.2 a h a (List.mem_cons_self ..)
      exact this rfl
    have hz : ∀ z ∈ l₁, Comm Inv step z a := by
      intro z hz
      apply hc z a
      · have h1 : [z] <+ l₁ := List.singleton_sublist.mpr hz
        have h2 : [a] <+ a :: l₂ := List.singleton_sublist.mpr (List.mem_cons_self ..)
        exact List.Sublist.append h1 h2
      · have hzπ : z ∈ π' := by
          have : z ∈ a :: π' := hp.symm.subset (List.mem_append_left _ hz)
          rcases List.mem_cons.mp this with e | e
          · exact absurd (e ▸ hz) hna
          · exact e
        exact List.cons_sublist_cons.mpr (List.singleton_sublist.mpr hzπ)
    rw [run_move Inv step hk a l₁ l₂ hz s hs]
    show run step π' (step a s) = run step (l₁ ++ l₂) (step a s)
    have hp' : π'.Perm (l₁ ++ l₂) :=
      (List.Perm.cons_inv (hp.trans List.perm_middle))
    have hsub : l₁ ++ l₂ <+ l₁ ++ a :: l₂ :=
      List.Sublist.append (List.Sublist.refl _) (List.sublist_cons_self _ _)
    exact serial Inv step hk π' (l₁ ++ l₂) (hnd.sublist hsub) hp'
      (fun x y h1 h2 => hc x y (h1.trans hsub) (h2.trans (List.sublist_cons_self _ _))) _ (hk a s hs)

/-! ## (B) Blocks

For each field, the schedule's accesses to it, in its order, fall into
blocks: a step that sets the field (or reads and flags it) alone, a run of
steps that only read it, a run of steps that only flag it, or a run of steps
of one exception group.  Two uses in one block commute; uses in different
blocks are kept in order by any order that keeps each field's block indices
in order.  Steps that do not fire this cycle commute with every step and are
left out. -/

/-- How a step uses a field: it only reads it, only flags it, or otherwise
    (sets it, or both reads and flags it). -/
inductive Use where
  | rd
  | fl
  | wr
  deriving DecidableEq

/-- How the step with footprint `x` uses field `f`, if at all. -/
def useOf (x : FP) (f : Nat) : Option Use :=
  if x.2.1.contains f || (x.1.contains f && x.2.2.contains f) then some .wr
  else if x.1.contains f then some .rd
  else if x.2.2.contains f then some .fl
  else none

/-- Two uses that commute: two reads, or two flag-sets. -/
def Use.compat : Use → Use → Bool
  | .rd, .rd => true
  | .fl, .fl => true
  | _, _ => false

theorem Use.compat_comm (u v : Use) : u.compat v = v.compat u := by
  cases u <;> cases v <;> rfl

theorem useOf_W (x : FP) (f : Nat) (h : x.2.1.contains f = true) : useOf x f = some .wr := by
  simp only [useOf, h, Bool.true_or, ↓reduceIte]

theorem useOf_R (x : FP) (f : Nat) (h : x.1.contains f = true) :
    useOf x f = some .wr ∨ useOf x f = some .rd := by
  cases hw : x.2.1.contains f <;> cases ho : x.2.2.contains f <;>
    simp only [useOf, h, hw, ho, Bool.true_or, Bool.false_or, Bool.true_and, Bool.false_and,
      ↓reduceIte, Bool.false_eq_true] <;>
    first | exact Or.inl rfl | exact Or.inr rfl | trivial | simp

theorem useOf_O (x : FP) (f : Nat) (h : x.2.2.contains f = true) :
    useOf x f = some .wr ∨ useOf x f = some .fl := by
  cases hw : x.2.1.contains f <;> cases hr : x.1.contains f <;>
    simp only [useOf, h, hw, hr, Bool.true_or, Bool.false_or, Bool.true_and, Bool.false_and,
      Bool.and_true, ↓reduceIte, Bool.false_eq_true] <;>
    first | exact Or.inl rfl | exact Or.inr rfl | trivial | simp

theorem useOf_some (x : FP) (f : Nat) (h : (x.1.contains f || x.2.1.contains f || x.2.2.contains f) = true) :
    ∃ u, useOf x f = some u := by
  cases hw : x.2.1.contains f <;> cases hr : x.1.contains f <;> cases ho : x.2.2.contains f <;>
    simp only [hw, hr, ho, Bool.or_false, Bool.false_or, Bool.or_true, Bool.false_eq_true] at h <;>
    simp only [useOf, hw, hr, ho, Bool.true_or, Bool.false_or, Bool.true_and, Bool.false_and,
      Bool.and_true, Bool.and_false, ↓reduceIte, Bool.false_eq_true] <;>
    exact ⟨_, rfl⟩

/-- Steps whose uses of every field they share commute are independent. -/
theorem indep_of_compat (x y : FP)
    (h : ∀ f u v, useOf x f = some u → useOf y f = some v → u.compat v = true) : indep x y = true := by
  have bad : ∀ f u v, useOf x f = some u → useOf y f = some v → u.compat v = false → False :=
    fun f u v hu hv hc => by rw [h f u v hu hv] at hc; cases hc
  have bad' : ∀ f u v, useOf y f = some u → useOf x f = some v → u.compat v = false → False :=
    fun f u v hu hv hc => bad f v u hv hu (by rw [Use.compat_comm]; exact hc)
  -- a field one sets, the other does not use
  have wr_free : ∀ (a b : FP) (hb : ∀ f u v, useOf a f = some u → useOf b f = some v → u.compat v = false → False),
      ∀ w ∈ a.2.1, (!b.1.contains w && !b.2.1.contains w && !b.2.2.contains w) = true := by
    intro a b hb w hw
    have ha := useOf_W a w (List.contains_iff_mem.mpr hw)
    cases e : (b.1.contains w || b.2.1.contains w || b.2.2.contains w)
    · simp only [Bool.or_eq_false_iff] at e
      rw [e.1.1, e.1.2, e.2]; rfl
    · obtain ⟨v, hv⟩ := useOf_some b w e
      exact (hb w .wr v ha hv (by cases v <;> rfl)).elim
  have fl_free : ∀ (a b : FP) (hb : ∀ f u v, useOf a f = some u → useOf b f = some v → u.compat v = false → False),
      ∀ o ∈ a.2.2, (!b.1.contains o) = true := by
    intro a b hb o ho
    cases e : b.1.contains o
    · rfl
    · rcases useOf_O a o (List.contains_iff_mem.mpr ho) with h1 | h1 <;>
      rcases useOf_R b o e with h2 | h2 <;>
      exact (hb o _ _ h1 h2 rfl).elim
  unfold indep
  rw [Bool.and_eq_true, Bool.and_eq_true, Bool.and_eq_true]
  exact ⟨⟨⟨List.all_eq_true.mpr (wr_free x y bad), List.all_eq_true.mpr (wr_free y x bad')⟩,
    List.all_eq_true.mpr (fl_free x y bad)⟩, List.all_eq_true.mpr (fl_free y x bad')⟩

/-- A block certificate for the schedule's order of `n` steps: `blk x f` is
    the block of step `x`'s use of field `f`.  Along the schedule's order a
    field's block indices never decrease, and two uses in one block commute:
    two reads, two flag-sets, or two steps of one exception group (`tag`,
    0 for none). -/
def BlockOK (n : Nat) (fp : Nat → FP) (tag : Nat → Nat) (blk : Nat → Nat → Nat) : Prop :=
  ∀ x y f u v, x < y → y < n → useOf (fp x) f = some u → useOf (fp y) f = some v →
    blk x f ≤ blk y f ∧ (blk x f = blk y f → u.compat v = true ∨ (tag x = tag y ∧ tag x ≠ 0))

/-- An order keeps every field's block indices in order, among the steps that
    fire (`fire`). -/
def Valid (fp : Nat → FP) (blk : Nat → Nat → Nat) (fire : Nat → Bool) (π : List Nat) : Prop :=
  ∀ x y f, [x, y] <+ π → fire x = true → fire y = true →
    (useOf (fp x) f).isSome → (useOf (fp y) f).isSome → blk x f ≤ blk y f

theorem range_pair {x y n : Nat} (h : [x, y] <+ List.range n) : x < y ∧ y < n := by
  refine ⟨List.pairwise_iff_forall_sublist.mp List.pairwise_lt_range h, ?_⟩
  exact List.mem_range.mp (h.subset (by simp))

/-- The step function of a list of effects. -/
def stepOf (E : Nat → Eff V) (k : Nat) (s : St V) : St V := (E k).apply s

/-- Steps (below `n`) that do not fire this cycle commute with every step. -/
abbrev IdleComm {α : Type} (Inv : α → Prop) (step : Nat → α → α) (n : Nat) (fire : Nat → Bool) : Prop :=
  ∀ x y, x < n → y < n → fire x = false → Comm Inv step x y

/-- Two steps (below `n`) of one exception group (`tag`, 0 for none) commute. -/
abbrev TagComm {α : Type} (Inv : α → Prop) (step : Nat → α → α) (n : Nat) (tag : Nat → Nat) : Prop :=
  ∀ x y, x < n → y < n → x ≠ y → tag x = tag y → tag x ≠ 0 → Comm Inv step x y

/-- **(B)** With a block certificate, an order that keeps every field's block
    indices in order (among the steps that fire) runs every two steps that
    the schedule runs the other way round only where they commute: one of
    them does not fire, or they are of one exception group, or they use
    every field they share compatibly. -/
theorem blocks_comm (Inv : St V → Prop) (E : Nat → Eff V) (n : Nat) (tag : Nat → Nat)
    (blk : Nat → Nat → Nat) (fire : Nat → Bool) (hok : BlockOK n (fun k => (E k).fp) tag blk)
    (hidle : IdleComm Inv (stepOf E) n fire)
    (htag : TagComm Inv (stepOf E) n tag)
    (π : List Nat) (hv : Valid (fun k => (E k).fp) blk fire π) :
    ∀ x y, [x, y] <+ List.range n → [y, x] <+ π → Comm Inv (stepOf E) x y := by
  intro x y hs hp
  obtain ⟨hxy, hyn⟩ := range_pair hs
  have hxn : x < n := by omega
  cases hfx : fire x
  · exact hidle x y hxn hyn hfx
  cases hfy : fire y
  · exact (hidle y x hyn hxn hfy).symm
  by_cases ht : tag x = tag y ∧ tag x ≠ 0
  · exact htag x y hxn hyn (by omega) ht.1 ht.2
  intro s _
  show (E y).apply ((E x).apply s) = (E x).apply ((E y).apply s)
  apply Eff.comm
  apply indep_of_compat
  intro f u v hu' hv'
  obtain ⟨hle, heq⟩ := hok x y f v u hxy hyn hv' hu'
  have hge : blk y f ≤ blk x f :=
    hv y x f hp hfy hfx (by rw [hu']; rfl) (by rw [hv']; rfl)
  rcases heq (by omega) with hc | hc
  · rw [Use.compat_comm]; exact hc
  · exact absurd hc ht

/-- **(B)**, whole: such an order, if it is an order of all the steps, ends
    in the schedule's state. -/
theorem blocks_serial (Inv : St V → Prop) (E : Nat → Eff V) (hk : Keeps Inv (stepOf E)) (n : Nat)
    (tag : Nat → Nat) (blk : Nat → Nat → Nat)
    (fire : Nat → Bool) (hok : BlockOK n (fun k => (E k).fp) tag blk)
    (hidle : IdleComm Inv (stepOf E) n fire)
    (htag : TagComm Inv (stepOf E) n tag)
    (π : List Nat) (hperm : π.Perm (List.range n)) (hv : Valid (fun k => (E k).fp) blk fire π) :
    ∀ s, Inv s → run (stepOf E) π s = run (stepOf E) (List.range n) s :=
  serial Inv (stepOf E) hk π (List.range n) List.nodup_range hperm
    (blocks_comm Inv E n tag blk fire hok hidle htag π hv)

/-! ## (C) The scan, and the parent's orders

`Design.scan` walks the schedule's steps once.  For each field it keeps (in a
binary tree by field number, `Tr`) the field's current block and, as bit sets
in a `Nat`, the methods reaching its earlier blocks and its current one.  A
step that opens a new block of a field is reached by every method reaching the
field's blocks so far; a step joining the current block, by those reaching the
blocks before it.  Bits come in two kinds: bit `a` (method `a` reaches
directly) and bit `M + a` (it reaches through a rule or another passable
step).  A step passes on its own method directly and, unless it is a step of
an enable-gated method, everything reaching it as reaching through it: a
gated step, when its method is not enabled, does nothing, and when it is, the
parent's order places it.  The check: only the methods `allowed` for a method
reach its steps, in each kind, and not the method itself (so two conflicting
steps of a method bsc split must be in one exception group; bsc splits a
method into steps with disjoint guards).  The work per (step, field) use is
one tree lookup and update and a few operations on `2M`-bit sets. -/

/-- A field's state during the scan. -/
structure FS where
  /-- one more than the last step that used the field (0: none yet) -/
  last : Nat
  /-- the field's current block (0: none yet) -/
  idx : Nat
  /-- the current block's kind: 0 reads, 1 flag-sets, 2 a write, `3 + t` group `t` -/
  ty : Nat
  /-- the methods reaching the field's earlier blocks (bits) -/
  prev : Nat
  /-- the methods reaching its current block (bits) -/
  cur : Nat

def FS.init : FS := ⟨0, 0, 0, 0, 0⟩

/-- Field states by field number below `2 ^ d`, a tree by the number's bits
    (a leaf stands for a whole subtree of one state). -/
inductive Tr where
  | lf (v : FS)
  | nd (l r : Tr)

def Tr.left : Tr → Tr
  | .lf v => .lf v
  | .nd l _ => l

def Tr.right : Tr → Tr
  | .lf v => .lf v
  | .nd _ r => r

def Tr.val : Tr → FS
  | .lf v => v
  | .nd _ _ => FS.init

/-- The state of field `k` (by recursion on the depth, with Boolean tests: the
    form the kernel evaluates fastest). -/
def Tr.get (d : Nat) : Tr → Nat → FS :=
  Nat.rec (motive := fun _ => Tr → Nat → FS) (fun t _ => t.val)
    (fun _ ih t k => bif k % 2 == 0 then ih t.left (k / 2) else ih t.right (k / 2)) d

/-- The tree with field `k`'s state replaced. -/
def Tr.set (d : Nat) : Tr → Nat → FS → Tr :=
  Nat.rec (motive := fun _ => Tr → Nat → FS → Tr) (fun _ _ v => .lf v)
    (fun _ ih t k v => bif k % 2 == 0 then .nd (ih t.left (k / 2) v) t.right
                       else .nd t.left (ih t.right (k / 2) v)) d

/-- Field states as a list of `(field, state)` entries, set one by one into the
    all-initial tree: how a checkpoint of the scan is written down. -/
def Tr.ofList (d : Nat) (l : List (Nat × FS)) : Tr :=
  l.foldl (fun t e => Tr.set d t e.1 e.2) (.lf FS.init)

def FS.beq (a b : FS) : Bool :=
  a.last == b.last && a.idx == b.idx && a.ty == b.ty && a.prev == b.prev && a.cur == b.cur

theorem FS.eq_of_beq {a b : FS} (h : a.beq b = true) : a = b := by
  cases a; cases b
  simp only [FS.beq, Bool.and_eq_true, beq_iff_eq] at h
  obtain ⟨⟨⟨⟨h1, h2⟩, h3⟩, h4⟩, h5⟩ := h
  subst h1 h2 h3 h4 h5; rfl

/-- Equality of trees, as a Boolean the kernel evaluates. -/
def Tr.beq : Tr → Tr → Bool
  | .lf a, .lf b => a.beq b
  | .nd l r, .nd l' r' => l.beq l' && r.beq r'
  | _, _ => false

theorem Tr.eq_of_beq : ∀ {a b : Tr}, a.beq b = true → a = b
  | .lf a, .lf b, h => by rw [FS.eq_of_beq h]
  | .nd l r, .nd l' r', h => by
    simp only [Tr.beq, Bool.and_eq_true] at h
    rw [Tr.eq_of_beq h.1, Tr.eq_of_beq h.2]
  | .lf _, .nd _ _, h => by simp [Tr.beq] at h
  | .nd _ _, .lf _, h => by simp [Tr.beq] at h

theorem Tr.get_succ (d : Nat) (t : Tr) (k : Nat) :
    Tr.get (d + 1) t k = bif k % 2 == 0 then Tr.get d t.left (k / 2) else Tr.get d t.right (k / 2) := rfl

theorem Tr.set_succ (d : Nat) (t : Tr) (k : Nat) (v : FS) :
    Tr.set (d + 1) t k v = bif k % 2 == 0 then .nd (Tr.set d t.left (k / 2) v) t.right
                           else .nd t.left (Tr.set d t.right (k / 2) v) := rfl

theorem Tr.get_lf (v : FS) : ∀ d k, Tr.get d (.lf v) k = v
  | 0, _ => rfl
  | d + 1, k => by
    rw [Tr.get_succ]
    cases k % 2 == 0
    · exact Tr.get_lf v d _
    · exact Tr.get_lf v d _

theorem Tr.get_set : ∀ (d : Nat) (t : Tr) (k k' : Nat) (v : FS), k < 2 ^ d → k' < 2 ^ d →
    Tr.get d (Tr.set d t k v) k' = if k' = k then v else Tr.get d t k'
  | 0, t, k, k', v, hk, hk' => by
    have e1 : k = 0 := by simp at hk; exact hk
    have e2 : k' = 0 := by simp at hk'; exact hk'
    subst e1; subst e2; rfl
  | d + 1, t, k, k', v, hk, hk' => by
    have h2 : k / 2 < 2 ^ d := by rw [Nat.pow_succ] at hk; omega
    have h2' : k' / 2 < 2 ^ d := by rw [Nat.pow_succ] at hk'; omega
    have ih := Tr.get_set d
    rw [Tr.set_succ, Tr.get_succ d t k']
    have odd : ∀ x : Nat, (x % 2 == 0) = false → x % 2 ≠ 0 := fun x h e => by
      rw [beq_iff_eq.mpr e] at h; cases h
    have even : ∀ x : Nat, (x % 2 == 0) = true → x % 2 = 0 := fun x h => beq_iff_eq.mp h
    cases hk2 : k % 2 == 0 <;> cases hk2' : k' % 2 == 0
    · -- both odd
      have p1 := odd k hk2; have p2 := odd k' hk2'
      rw [cond_false, cond_false, Tr.get_succ, hk2', cond_false]
      show Tr.get d (Tr.set d t.right (k / 2) v) (k' / 2) = if k' = k then v else Tr.get d t.right (k' / 2)
      rw [ih t.right (k / 2) (k' / 2) v h2 h2']
      by_cases e' : k' = k
      · rw [if_pos e', if_pos (by rw [e'])]
      · rw [if_neg e', if_neg (by omega)]
    · -- k odd, k' even
      have p1 := odd k hk2; have p2 := even k' hk2'
      rw [cond_false, cond_true, Tr.get_succ, hk2', cond_true]
      show Tr.get d t.left (k' / 2) = if k' = k then v else Tr.get d t.left (k' / 2)
      rw [if_neg (by omega)]
    · -- k even, k' odd
      have p1 := even k hk2; have p2 := odd k' hk2'
      rw [cond_true, cond_false, Tr.get_succ, hk2', cond_false]
      show Tr.get d t.right (k' / 2) = if k' = k then v else Tr.get d t.right (k' / 2)
      rw [if_neg (by omega)]
    · -- both even
      have p1 := even k hk2; have p2 := even k' hk2'
      rw [cond_true, cond_true, Tr.get_succ, hk2', cond_true]
      show Tr.get d (Tr.set d t.left (k / 2) v) (k' / 2) = if k' = k then v else Tr.get d t.left (k' / 2)
      rw [ih t.left (k / 2) (k' / 2) v h2 h2']
      by_cases e' : k' = k
      · rw [if_pos e', if_pos (by rw [e'])]
      · rw [if_neg e', if_neg (by omega)]

/-- A table by number (a step's footprint, a step's method, a method's allowed
    bits), as a search tree.  Emitted tables are data of this type, never
    `if k < m then .. else ..` code over a bound `k`: the kernel looks an entry
    up in O(depth) steps, whereas unfolding code over `k` first copies the
    code's whole spine (the table's size) for every lookup. -/
inductive Tab (α : Type) where
  | leaf (a : α)
  | node (mid : Nat) (l r : Tab α)

/-- The entry for `k`: left of `mid` below it, right otherwise. -/
def Tab.get {α : Type} : Tab α → Nat → α
  | .leaf a, _ => a
  | .node mid l r, k => bif Nat.blt k mid then l.get k else r.get k

/-! ### A step's uses, field by field

The scan visits each field a step uses once, with the kind of use already
known.  A footprint's three lists, each sorted, are merged into one list of
`(field, bits)` (bit 0: read, bit 1: set, bit 2: flagged), in one linear pass
(`ul`).  The scan checks that the merged list is strictly sorted (so each
field appears once); the kind of use is then read off the bits in constant
time (`useOfBits`).  Nothing here needs the lists to be sorted for soundness:
an unsorted footprint only makes the scan fail. -/

/-- Merge `x :: xs` into `ys`, given `rec`, the merge of `xs` (structural
    recursion on `ys`; equal fields combine their bits). -/
def mergeAux (x : Nat × Nat) (xs : List (Nat × Nat)) (rec : List (Nat × Nat) → List (Nat × Nat)) :
    List (Nat × Nat) → List (Nat × Nat)
  | [] => x :: xs
  | y :: ys =>
    bif x.1 < y.1 then x :: rec (y :: ys)
    else bif y.1 < x.1 then y :: mergeAux x xs rec ys
    else (x.1, x.2 ||| y.2) :: rec ys

/-- Merge two lists of `(field, bits)` sorted by field. -/
def mergeB : List (Nat × Nat) → List (Nat × Nat) → List (Nat × Nat)
  | [], ys => ys
  | x :: xs, ys => mergeAux x xs (mergeB xs) ys

/-- A list of fields, each with the bits `c`. -/
def tagged (l : List Nat) (c : Nat) : List (Nat × Nat) := l.map (fun f => (f, c))

/-- A footprint's fields with their kinds of use. -/
def ul (x : FP) : List (Nat × Nat) := mergeB (mergeB (tagged x.1 1) (tagged x.2.1 2)) (tagged x.2.2 4)

/-- The kind of use the bits say. -/
def useOfBits (b : Nat) : Use :=
  bif b.testBit 1 || (b.testBit 0 && b.testBit 2) then .wr else bif b.testBit 0 then .rd else .fl

/-- The list's fields are strictly increasing. -/
def sortedK : List (Nat × Nat) → Bool
  | e₁ :: e₂ :: l => decide (e₁.1 < e₂.1) && sortedK (e₂ :: l)
  | _ => true

/-- The bits of all entries for field `f` (specification only, never computed). -/
def bitsAt : List (Nat × Nat) → Nat → Nat
  | [], _ => 0
  | e :: l, f => (if e.1 = f then e.2 else 0) ||| bitsAt l f

theorem bitsAt_mergeAux (f : Nat) (x : Nat × Nat) (xs : List (Nat × Nat))
    (rec : List (Nat × Nat) → List (Nat × Nat)) (hrec : ∀ zs, bitsAt (rec zs) f = bitsAt xs f ||| bitsAt zs f) :
    ∀ ys, bitsAt (mergeAux x xs rec ys) f = bitsAt (x :: xs) f ||| bitsAt ys f
  | [] => by simp [mergeAux, bitsAt]
  | y :: ys => by
    have ih := bitsAt_mergeAux f x xs rec hrec ys
    unfold mergeAux
    cases h1 : decide (x.1 < y.1)
    · cases h2 : decide (y.1 < x.1)
      · have e : x.1 = y.1 := by simp at h1 h2; omega
        simp only [h1, h2, cond_false, bitsAt, hrec]
        apply Nat.eq_of_testBit_eq; intro i
        by_cases hf : x.1 = f
        · have hf' : y.1 = f := e ▸ hf
          simp [hf, hf', Nat.testBit_or]; cases x.2.testBit i <;> cases y.2.testBit i <;> simp
        · have hf' : ¬ y.1 = f := e ▸ hf
          simp [hf, hf', Nat.testBit_or]
      · simp only [h1, h2, cond_false, cond_true, bitsAt, ih]
        apply Nat.eq_of_testBit_eq; intro i
        simp only [bitsAt, Nat.testBit_or]
        cases (if y.1 = f then y.2 else 0).testBit i <;> simp
    · simp only [h1, cond_true, bitsAt, hrec]
      apply Nat.eq_of_testBit_eq; intro i
      simp only [bitsAt, Nat.testBit_or, Bool.or_assoc]

theorem bitsAt_mergeB (f : Nat) : ∀ xs ys, bitsAt (mergeB xs ys) f = bitsAt xs f ||| bitsAt ys f
  | [], ys => by simp [mergeB, bitsAt]
  | x :: xs, ys => bitsAt_mergeAux f x xs (mergeB xs) (bitsAt_mergeB f xs) ys

theorem bitsAt_tagged (f c : Nat) : ∀ l, bitsAt (tagged l c) f = bif l.contains f then c else 0
  | [] => rfl
  | a :: l => by
    have ih := bitsAt_tagged f c l
    simp only [tagged, List.map_cons, bitsAt] at ih ⊢
    rw [ih]
    by_cases h : a = f
    · subst h
      apply Nat.eq_of_testBit_eq; intro i
      simp only [↓reduceIte, Nat.testBit_or]
      cases l.contains a <;> simp
    · have : (a :: l).contains f = l.contains f := by
        simp only [List.contains_cons]; rw [show (f == a) = false from by simp; omega, Bool.false_or]
      rw [this]; simp [h]

theorem bitsAt_ul (x : FP) (f : Nat) :
    bitsAt (ul x) f = (bif x.1.contains f then 1 else 0) ||| (bif x.2.1.contains f then 2 else 0) |||
      (bif x.2.2.contains f then 4 else 0) := by
  simp only [ul, bitsAt_mergeB, bitsAt_tagged]

/-- The bits of a field a footprint uses give its use, and are not zero. -/
theorem useOfBits_ul {x : FP} {f : Nat} {u : Use} (h : useOf x f = some u) :
    useOfBits (bitsAt (ul x) f) = u ∧ bitsAt (ul x) f ≠ 0 := by
  rw [bitsAt_ul]
  cases hr : x.1.contains f <;> cases hw : x.2.1.contains f <;> cases ho : x.2.2.contains f <;>
    simp only [useOf, hr, hw, ho, Bool.true_or, Bool.false_or, Bool.true_and, Bool.false_and,
      Bool.and_true, Bool.and_false, ↓reduceIte, Bool.false_eq_true, Option.some.injEq,
      reduceCtorEq] at h <;>
    (try subst h) <;> (try cases h) <;> decide

theorem mem_of_bitsAt {L : List (Nat × Nat)} {f : Nat} (h : bitsAt L f ≠ 0) : f ∈ L.map Prod.fst := by
  induction L with
  | nil => exact absurd rfl h
  | cons e l ih =>
    by_cases he : e.1 = f
    · exact List.mem_cons.mpr (Or.inl he.symm)
    · simp only [bitsAt, he, ↓reduceIte, Nat.zero_or] at h
      exact List.mem_cons_of_mem _ (ih h)

theorem bitsAt_none {L : List (Nat × Nat)} {f : Nat} (h : ∀ e ∈ L, e.1 ≠ f) : bitsAt L f = 0 := by
  induction L with
  | nil => rfl
  | cons e l ih =>
    simp only [bitsAt, h e (List.mem_cons_self ..), ↓reduceIte, Nat.zero_or]
    exact ih (fun e' he' => h e' (List.mem_cons_of_mem _ he'))

theorem sortedK_head {e : Nat × Nat} : ∀ {l : List (Nat × Nat)}, sortedK (e :: l) = true → ∀ e' ∈ l, e.1 < e'.1
  | [], _, e', h => by cases h
  | e₂ :: l, hs, e', he' => by
    simp only [sortedK, Bool.and_eq_true, decide_eq_true_eq] at hs
    rcases List.mem_cons.mp he' with rfl | h'
    · exact hs.1
    · exact Nat.lt_trans hs.1 (sortedK_head hs.2 e' h')

theorem sortedK_tail {e : Nat × Nat} : ∀ {l : List (Nat × Nat)}, sortedK (e :: l) = true → sortedK l = true
  | [], _ => rfl
  | e₂ :: l, hs => by simp only [sortedK, Bool.and_eq_true] at hs; exact hs.2

/-- In a strictly sorted list an entry's bits are all the field's bits. -/
theorem bitsAt_sorted : ∀ {L : List (Nat × Nat)}, sortedK L = true → ∀ e ∈ L, bitsAt L e.1 = e.2
  | [], _, e, he => by cases he
  | e₁ :: l, hs, e, he => by
    have hh := sortedK_head hs
    rcases List.mem_cons.mp he with rfl | h'
    · simp only [bitsAt, ↓reduceIte]
      rw [bitsAt_none (fun e' he' => by have := hh e' he'; omega), Nat.or_zero]
    · have hne : e₁.1 ≠ e.1 := by have := hh e h'; omega
      simp only [bitsAt, hne, ↓reduceIte, Nat.zero_or]
      exact bitsAt_sorted (sortedK_tail hs) e h'

theorem foldl_fst {β : Type} (g : β → Nat × Nat → β) (g' : β → Nat → β) :
    ∀ (L : List (Nat × Nat)), (∀ e ∈ L, ∀ acc, g acc e = g' acc e.1) →
      ∀ a, L.foldl g a = (L.map Prod.fst).foldl g' a
  | [], _, a => rfl
  | e :: l, h, a => by
    simp only [List.foldl_cons, List.map_cons]
    rw [h e (List.mem_cons_self ..) a]
    exact foldl_fst g g' l (fun e' he' => h e' (List.mem_cons_of_mem _ he')) _

/-! ### The scan -/

/-- What the scan knows of a design. -/
structure Design where
  /-- the number of steps, `0 .. n - 1` in the schedule's order -/
  n : Nat
  /-- field numbers are below `2 ^ d` -/
  d : Nat
  /-- the number of methods, `0 .. M - 1` -/
  M : Nat
  /-- each step's footprint (each list sorted) -/
  fp : Nat → FP
  /-- each step's method: `m + 1` for method `m`, 0 for a rule -/
  meth : Nat → Nat
  /-- by method: paths are not followed through its steps.  Sound for a method
      whose steps, when they do not fire, commute with every step (`hidle`:
      an action method with an enable whose outputs, when it is not enabled,
      do not depend on the state) and that, when they fire, are in the
      parent's order (`hgated`).  Every other method (value methods,
      action-value methods with results read from the state, always-enabled
      methods) is not gated: paths go through it like through a rule. -/
  gated : Nat → Bool
  /-- each step's exception group, 0 for none -/
  tag : Nat → Nat
  /-- by method `b`, as bits: bit `a` (for `a < M`) if method `a` may reach
      `b`'s steps at all, bit `M + a` if it may reach them through a rule (or
      another step paths pass through).  Built by `allowedOf`. -/
  allowed : Nat → Nat

/-- The fields a footprint mentions (with repeats). -/
def uses (x : FP) : List Nat := x.1 ++ x.2.1 ++ x.2.2

def FS.joins (st : FS) (u : Use) (t : Nat) : Bool :=
  st.idx != 0 && ((t != 0 && st.ty == t + 3) || (st.ty == 0 && u == .rd) || (st.ty == 1 && u == .fl))

def newTy (u : Use) (t : Nat) : Nat :=
  if t != 0 then t + 3 else match u with | .rd => 0 | .fl => 1 | .wr => 2

/-- The methods reaching a use of the field: those reaching the earlier blocks
    if it joins the current block, else those reaching any block so far. -/
def FS.contrib (st : FS) (u : Use) (t : Nat) : Nat :=
  if st.joins u t then st.prev else st.prev ||| st.cur

/-- The field once step `k` used it, passing on `o`. -/
def FS.upd (st : FS) (k : Nat) (u : Use) (t o : Nat) : FS :=
  if st.joins u t then { st with last := k + 1, cur := st.cur ||| o }
  else ⟨k + 1, st.idx + 1, newTy u t, st.prev ||| st.cur, o⟩

namespace Design

variable (D : Design)

/-- Step `k`'s fields with their kinds of use. -/
def ent (k : Nat) : List (Nat × Nat) := ul (D.fp k)

/-- The fields step `k` uses (specification). -/
def fs (k : Nat) : List Nat := (D.ent k).map Prod.fst

/-- How step `k` uses field `f` (specification). -/
def ua (k f : Nat) : Use := useOfBits (bitsAt (D.ent k) f)

/-- The methods reaching step `k`, from the fields' states before it: bit `a`
    if method `a` reaches it directly, bit `M + a` if through a rule. -/
def reachIn (t : Tr) (k : Nat) : Nat :=
  (D.ent k).foldl (fun acc e => acc ||| (t.get D.d e.1).contrib (useOfBits e.2) (D.tag k)) 0

/-- The bit of step `k`'s method (none for a rule). -/
def mbit (k : Nat) : Nat := if D.meth k = 0 then 0 else 2 ^ (D.meth k - 1)

/-- Paths may pass through step `k`: it is not a step of an enable-gated method. -/
def passable (k : Nat) : Bool := D.meth k == 0 || !D.gated (D.meth k - 1)

/-- Every method reaching a step (directly or not) as reaching through it. -/
def lift (r : Nat) : Nat := (r % 2 ^ D.M ||| r >>> D.M) <<< D.M

/-- What step `k`, reached by `r`, passes on: its method (directly), and if paths
    may pass through it, the methods reaching it (through it). -/
def outOf (k r : Nat) : Nat := D.mbit k ||| (if D.passable k then D.lift r else 0)

/-- The fields' states once step `k` ran, passing on `o`. -/
def applyStep (t : Tr) (k o : Nat) : Tr :=
  (D.ent k).foldl (fun t e =>
    if (t.get D.d e.1).last == k + 1 then t
    else t.set D.d e.1 ((t.get D.d e.1).upd k (useOfBits e.2) (D.tag k) o)) t

/-- The check at step `k`, reached by `r`: its fields are sorted and in range,
    its method is below `M`, and if it is a method's step, only methods allowed
    for that method reach it, and not the method itself. -/
def stepOk (k r : Nat) : Bool :=
  sortedK (D.ent k) && (D.ent k).all (·.1 < 2 ^ D.d) && decide (D.meth k ≤ D.M) &&
  (D.meth k == 0 || ((r &&& D.allowed (D.meth k - 1)) == r && !r.testBit (D.meth k - 1) &&
    !r.testBit (D.meth k - 1 + D.M)))

def next (t : Tr) (k : Nat) : Tr := D.applyStep t k (D.outOf k (D.reachIn t k))

def scanFrom : List Nat → Tr → Bool
  | [], _ => true
  | k :: ks, t =>
    let r := D.reachIn t k
    D.stepOk k r && scanFrom ks (D.applyStep t k (D.outOf k r))

/-- **The check**, decided by the kernel: one pass over the schedule's steps. -/
def scan : Bool := D.scanFrom (List.range D.n) (.lf FS.init)

/-- The fields' states before step `p`. -/
def stAt (p : Nat) : Tr := (List.range p).foldl D.next (.lf FS.init)

/-- The methods reaching step `k`. -/
def rby (k : Nat) : Nat := D.reachIn (D.stAt k) k

/-- What step `k` passes on. -/
def out (k : Nat) : Nat := D.outOf k (D.rby k)

/-- The block of field `f` step `x`'s use is in (the certificate the scan finds). -/
def blk (x f : Nat) : Nat := ((D.stAt (x + 1)).get D.d f).idx

theorem ent_bits {k : Nat} (hs : sortedK (D.ent k) = true) : ∀ e ∈ D.ent k, useOfBits e.2 = D.ua k e.1 :=
  fun e he => by unfold ua; rw [bitsAt_sorted hs e he]

theorem reachIn_eq (t : Tr) (k : Nat) (hs : sortedK (D.ent k) = true) :
    D.reachIn t k = (D.fs k).foldl (fun acc f => acc ||| (t.get D.d f).contrib (D.ua k f) (D.tag k)) 0 :=
  foldl_fst _ _ (D.ent k) (fun e he acc => by rw [D.ent_bits hs e he]) 0

theorem applyStep_eq (t : Tr) (k o : Nat) (hs : sortedK (D.ent k) = true) :
    D.applyStep t k o = (D.fs k).foldl (fun t f =>
      if (t.get D.d f).last == k + 1 then t
      else t.set D.d f ((t.get D.d f).upd k (D.ua k f) (D.tag k) o)) t :=
  foldl_fst _ _ (D.ent k) (fun e he acc => by rw [D.ent_bits hs e he]) t

theorem mem_fs {k f : Nat} {u : Use} (h : useOf (D.fp k) f = some u) : f ∈ D.fs k :=
  mem_of_bitsAt (useOfBits_ul h).2

theorem ua_eq {k f : Nat} {u : Use} (h : useOf (D.fp k) f = some u) : D.ua k f = u :=
  (useOfBits_ul h).1

end Design

/-! ### The scan finds a block certificate and the methods reaching each step -/

/-- Bit set inclusion. -/
def Sub (a b : Nat) : Prop := ∀ i, a.testBit i = true → b.testBit i = true

/-- What a block of kind `ty` admits: a read in a block of reads, a flag-set in a
    block of flag-sets, anything in a write's block (which holds one use), any
    use by a step of group `t` in group `t`'s block. -/
def admits (ty : Nat) (u : Use) (t : Nat) : Prop :=
  (ty = 0 ∧ u = .rd) ∨ (ty = 1 ∧ u = .fl) ∨ ty = 2 ∨ (t ≠ 0 ∧ ty = t + 3)

theorem joins_spec {st : FS} {u : Use} {t : Nat} (h : st.joins u t = true) :
    st.idx ≠ 0 ∧ st.ty ≠ 2 ∧ admits st.ty u t := by
  simp only [FS.joins, Bool.and_eq_true, Bool.or_eq_true, bne_iff_ne, ne_eq, beq_iff_eq] at h
  obtain ⟨h1, h2⟩ := h
  refine ⟨h1, ?_, ?_⟩
  · rcases h2 with (⟨_, e⟩ | ⟨e, _⟩) | ⟨e, _⟩ <;> omega
  · unfold admits
    rcases h2 with (⟨ht, e⟩ | ⟨e, hu⟩) | ⟨e, hu⟩
    · exact Or.inr (Or.inr (Or.inr ⟨ht, e⟩))
    · exact Or.inl ⟨e, hu⟩
    · exact Or.inr (Or.inl ⟨e, hu⟩)

theorem newTy_admits (u : Use) (t : Nat) : admits (newTy u t) u t := by
  unfold newTy admits
  by_cases ht : t = 0
  · subst ht; cases u <;> simp
  · simp [ht]

theorem admits_compat {ty t t' : Nat} {u u' : Use} (h : admits ty u t) (h' : admits ty u' t')
    (h2 : ty ≠ 2) : u.compat u' = true ∨ (t = t' ∧ t ≠ 0) := by
  unfold admits at h h'
  rcases h with ⟨e, hu⟩ | ⟨e, hu⟩ | e | ⟨ht, e⟩ <;>
  rcases h' with ⟨e', hu'⟩ | ⟨e', hu'⟩ | e' | ⟨ht', e'⟩ <;>
  first
  | omega
  | (rw [hu, hu']; exact Or.inl rfl)
  | exact Or.inr ⟨by omega, ht⟩

theorem FS.upd_last (st : FS) (k : Nat) (u : Use) (t o : Nat) : (st.upd k u t o).last = k + 1 := by
  unfold FS.upd; split <;> rfl

theorem FS.upd_idx (st : FS) (k : Nat) (u : Use) (t o : Nat) :
    (st.upd k u t o).idx = if st.joins u t = true then st.idx else st.idx + 1 := by
  unfold FS.upd; split <;> rfl

theorem testBit_foldl_or (l : List Nat) (g : Nat → Nat) (init i : Nat) :
    (l.foldl (fun acc f => acc ||| g f) init).testBit i = (init.testBit i || l.any (fun f => (g f).testBit i)) := by
  induction l generalizing init with
  | nil => simp
  | cons f l ih => simp only [List.foldl, ih, Nat.testBit_or, List.any_cons, Bool.or_assoc]

theorem mem_uses_of_useOf {x : FP} {f : Nat} {u : Use} (h : useOf x f = some u) : f ∈ uses x := by
  cases hr : x.1.contains f
  · cases hw : x.2.1.contains f
    · cases ho : x.2.2.contains f
      · simp only [useOf, hr, hw, ho, Bool.false_or, Bool.false_and, ↓reduceIte,
          Bool.false_eq_true] at h
        cases h
      · simp only [uses, List.mem_append]; exact Or.inr (List.contains_iff_mem.mp ho)
    · simp only [uses, List.mem_append]; exact Or.inl (Or.inr (List.contains_iff_mem.mp hw))
  · simp only [uses, List.mem_append]; exact Or.inl (Or.inl (List.contains_iff_mem.mp hr))

theorem foldl_upd (d k : Nat) (U : Nat → FS → FS) (hU : ∀ f st, (U f st).last = k + 1) :
    ∀ (L : List Nat) (t : Tr), (∀ f ∈ L, f < 2 ^ d) → ∀ g, g < 2 ^ d →
      (L.foldl (fun t f => if (t.get d f).last == k + 1 then t else t.set d f (U f (t.get d f))) t).get d g =
        if g ∈ L ∧ (t.get d g).last ≠ k + 1 then U g (t.get d g) else t.get d g
  | [], t, _, g, _ => by simp
  | f :: L, t, hL, g, hg => by
    rw [List.foldl_cons]
    rw [foldl_upd d k U hU L _ (fun f' h' => hL f' (List.mem_cons_of_mem _ h')) g hg]
    have hf := hL f (List.mem_cons_self ..)
    by_cases hl : (t.get d f).last = k + 1
    · have e : (if (t.get d f).last == k + 1 then t else t.set d f (U f (t.get d f))) = t := by
        simp [hl]
      rw [e]
      by_cases hgf : g = f
      · subst hgf; simp [hl]
      · simp [hgf]
    · have e : (if (t.get d f).last == k + 1 then t else t.set d f (U f (t.get d f))) =
          t.set d f (U f (t.get d f)) := by simp [hl]
      rw [e, Tr.get_set d t f g _ hf hg]
      by_cases hgf : g = f
      · subst hgf; simp [hl, hU]
      · simp [hgf]

namespace Design

variable (D : Design)

theorem reachIn_ge (t : Tr) (k f i : Nat) (hs : sortedK (D.ent k) = true) (hf : f ∈ D.fs k)
    (h : ((t.get D.d f).contrib (D.ua k f) (D.tag k)).testBit i = true) :
    (D.reachIn t k).testBit i = true := by
  rw [D.reachIn_eq t k hs]
  rw [testBit_foldl_or (D.fs k) (fun f => (t.get D.d f).contrib (D.ua k f) (D.tag k))]
  simp only [Bool.or_eq_true, List.any_eq_true]
  exact Or.inr ⟨f, hf, h⟩

theorem applyStep_get (t : Tr) (k o g : Nat) (hs : sortedK (D.ent k) = true) (hb : ∀ f ∈ D.fs k, f < 2 ^ D.d)
    (hg : g < 2 ^ D.d) (hl : (t.get D.d g).last ≠ k + 1) :
    (D.applyStep t k o).get D.d g =
      if g ∈ D.fs k then (t.get D.d g).upd k (D.ua k g) (D.tag k) o else t.get D.d g := by
  rw [D.applyStep_eq t k o hs]
  rw [foldl_upd D.d k (fun f st => st.upd k (D.ua k f) (D.tag k) o)
    (fun f st => FS.upd_last ..) _ t hb g hg]
  by_cases h : g ∈ D.fs k <;> simp [h, hl]

theorem stAt_succ (p : Nat) : D.stAt (p + 1) = D.next (D.stAt p) p := by
  unfold stAt; rw [List.range_succ, List.foldl_append]; rfl

/-- What the theorems need of the scan: every step passes its check, reached by
    the methods reaching it.  `scan_ok` gives it from one decided `scan`;
    `ok_of_chain` from a chain of checkpointed segments (each its own kernel
    check, so each check's memory is bounded by its segment). -/
def Ok : Prop := ∀ k, k < D.n → D.stepOk k (D.rby k) = true

theorem scanFrom_ok : ∀ (m a : Nat), D.scanFrom (List.range' a m) (D.stAt a) = true →
    ∀ k, a ≤ k → k < a + m → D.stepOk k (D.rby k) = true
  | 0, a, _, k, h1, h2 => by omega
  | m + 1, a, h, k, h1, h2 => by
    rw [List.range'_succ] at h
    simp only [scanFrom, Bool.and_eq_true] at h
    by_cases e : k = a
    · subst e; exact h.1
    · have h2' := h.2
      have e : D.applyStep (D.stAt a) a (D.outOf a (D.reachIn (D.stAt a) a)) = D.stAt (a + 1) :=
        (D.stAt_succ a).symm
      rw [e] at h2'
      exact scanFrom_ok (m := m) (a := a + 1) h2' k (by omega) (by omega)

theorem scan_ok (h : D.scan = true) : D.Ok := by
  intro k hk
  have h0 : D.stAt 0 = .lf FS.init := rfl
  unfold scan at h
  rw [List.range_eq_range', ← h0] at h
  exact D.scanFrom_ok D.n 0 h k (Nat.zero_le _) (by omega)

/-- The scan over the steps `ks` from the fields' states `t`: the states after
    them if every step passes its check. -/
def segFrom : List Nat → Tr → Option Tr
  | [], t => some t
  | k :: ks, t =>
    let r := D.reachIn t k
    bif D.stepOk k r then segFrom ks (D.applyStep t k (D.outOf k r)) else none

/-- One checkpointed segment: from state `ta` before step `a`, the `m` steps
    from `a` pass and leave the state `tb`. -/
def segChk (a m : Nat) (ta tb : Tr) : Bool :=
  match D.segFrom (List.range' a m) ta with
  | some t => t.beq tb
  | none => false

theorem segFrom_ok : ∀ (m a : Nat) (t : Tr), D.segFrom (List.range' a m) (D.stAt a) = some t →
    t = D.stAt (a + m) ∧ ∀ k, a ≤ k → k < a + m → D.stepOk k (D.rby k) = true
  | 0, a, t, h => by
    simp only [List.range'_zero, segFrom, Option.some.injEq] at h
    exact ⟨h.symm, fun k h1 h2 => by omega⟩
  | m + 1, a, t, h => by
    rw [List.range'_succ] at h
    simp only [segFrom] at h
    cases hk : D.stepOk a (D.reachIn (D.stAt a) a)
    · rw [hk] at h; cases h
    · rw [hk] at h
      have e : D.applyStep (D.stAt a) a (D.outOf a (D.reachIn (D.stAt a) a)) = D.stAt (a + 1) :=
        (D.stAt_succ a).symm
      simp only [cond_true] at h
      rw [e] at h
      obtain ⟨ht, hr⟩ := segFrom_ok m (a + 1) t h
      refine ⟨by rw [ht]; congr 1; omega, fun k h1 h2 => ?_⟩
      by_cases e' : k = a
      · subst e'; exact hk
      · exact hr k (by omega) (by omega)

/-- A chain of checkpointed segments from state `ta` before step `a`: each entry
    `(m, tb)` is a segment of `m` steps ending in `tb`; after the last one, the
    rest of the steps pass. -/
def ChainFrom (a : Nat) (ta : Tr) : List (Nat × Tr) → Prop
  | [] => D.scanFrom (List.range' a (D.n - a)) ta = true
  | (m, tb) :: rest => D.segChk a m ta tb = true ∧ ChainFrom (a + m) tb rest

theorem chainFrom_ok : ∀ (cs : List (Nat × Tr)) (a : Nat), D.ChainFrom a (D.stAt a) cs →
    ∀ k, a ≤ k → k < D.n → D.stepOk k (D.rby k) = true
  | [], a, h, k, h1, h2 => D.scanFrom_ok (D.n - a) a h k h1 (by omega)
  | (m, tb) :: rest, a, h, k, h1, h2 => by
    obtain ⟨hc, hr⟩ := h
    unfold segChk at hc
    split at hc
    · next t ht =>
      have e := Tr.eq_of_beq hc
      obtain ⟨ht', hseg⟩ := D.segFrom_ok m a t ht
      rw [← e, ht'] at hr
      by_cases hk : k < a + m
      · exact hseg k h1 hk
      · exact chainFrom_ok rest (a + m) hr k (by omega) h2
    · cases hc

/-- The scan's check, from a chain of checkpointed segments starting from the
    all-initial state.  Each segment is decided on its own (`segChk`), so the
    kernel's work and memory per check are bounded by the segment, and the
    whole is linear: the steps, plus one written-down state per checkpoint. -/
theorem ok_of_chain (cs : List (Nat × Tr)) (h : D.ChainFrom 0 (.lf FS.init) cs) : D.Ok :=
  fun k hk => D.chainFrom_ok cs 0 h k (Nat.zero_le _) hk

theorem stepOk_sorted {k r : Nat} (h : D.stepOk k r = true) : sortedK (D.ent k) = true := by
  unfold stepOk at h
  simp only [Bool.and_eq_true] at h
  exact h.1.1.1

theorem stepOk_bound {k r : Nat} (h : D.stepOk k r = true) : ∀ f ∈ D.fs k, f < 2 ^ D.d := by
  unfold stepOk at h
  simp only [Bool.and_eq_true] at h
  intro f hf
  obtain ⟨e, he, rfl⟩ := List.mem_map.mp hf
  exact of_decide_eq_true (List.all_eq_true.mp h.1.1.2 e he)

theorem stepOk_methM {k r : Nat} (h : D.stepOk k r = true) : D.meth k ≤ D.M := by
  unfold stepOk at h
  simp only [Bool.and_eq_true] at h
  exact of_decide_eq_true h.1.2

theorem stepOk_meth {k r : Nat} (h : D.stepOk k r = true) (hm : D.meth k ≠ 0) :
    ∀ i, r.testBit i = true → (D.allowed (D.meth k - 1)).testBit i = true ∧ i ≠ D.meth k - 1 ∧
      i ≠ D.meth k - 1 + D.M := by
  unfold stepOk at h
  simp only [Bool.and_eq_true] at h
  have h2 := h.2
  simp only [Bool.or_eq_true, beq_iff_eq, hm, false_or, Bool.and_eq_true, Bool.not_eq_true'] at h2
  obtain ⟨⟨hsub, hnot⟩, hnot'⟩ := h2
  intro i hi
  refine ⟨?_, ?_, ?_⟩
  · have := congrArg (fun x => x.testBit i) hsub
    simp only [Nat.testBit_and, hi, Bool.true_and] at this
    exact this
  · intro e; rw [e, hnot] at hi; cases hi
  · intro e; rw [e, hnot'] at hi; cases hi

/-- What the scan keeps true of field `f` before step `p`, its state `st`. -/
structure InvF (p f : Nat) (st : FS) : Prop where
  last_le : st.last ≤ p
  blk_le : ∀ z, z < p → f ∈ D.fs z → 0 < D.blk z f ∧ D.blk z f ≤ st.idx
  kind_ok : ∀ z, z < p → f ∈ D.fs z → D.blk z f = st.idx → admits st.ty (D.ua z f) (D.tag z)
  single : st.ty = 2 → ∀ z z', z < p → z' < p → f ∈ D.fs z → f ∈ D.fs z' →
    D.blk z f = st.idx → D.blk z' f = st.idx → z = z'
  prev : ∀ z, z < p → f ∈ D.fs z → D.blk z f < st.idx → Sub (D.out z) st.prev
  cur : ∀ z, z < p → f ∈ D.fs z → D.blk z f = st.idx → Sub (D.out z) st.cur

theorem inv_zero (f : Nat) : D.InvF 0 f ((D.stAt 0).get D.d f) where
  last_le := by show (Tr.get D.d (.lf FS.init) f).last ≤ 0; rw [Tr.get_lf]; exact Nat.le_refl _
  blk_le := fun z hz => absurd hz (Nat.not_lt_zero _)
  kind_ok := fun z hz => absurd hz (Nat.not_lt_zero _)
  single := fun _ z _ hz => absurd hz (Nat.not_lt_zero _)
  prev := fun z hz => absurd hz (Nat.not_lt_zero _)
  cur := fun z hz => absurd hz (Nat.not_lt_zero _)

theorem get_next (p f : Nat) (hso : sortedK (D.ent p) = true) (hb : ∀ g ∈ D.fs p, g < 2 ^ D.d) (hf : f < 2 ^ D.d)
    (hl : ((D.stAt p).get D.d f).last ≤ p) :
    (D.stAt (p + 1)).get D.d f =
      if f ∈ D.fs p then ((D.stAt p).get D.d f).upd p (D.ua p f) (D.tag p) (D.out p)
      else (D.stAt p).get D.d f := by
  rw [D.stAt_succ]
  show (D.applyStep (D.stAt p) p (D.out p)).get D.d f = _
  exact D.applyStep_get (D.stAt p) p (D.out p) f hso hb hf (by omega)

theorem inv_step (p f : Nat) (hso : sortedK (D.ent p) = true) (hb : ∀ g ∈ D.fs p, g < 2 ^ D.d) (hf : f < 2 ^ D.d)
    (I : D.InvF p f ((D.stAt p).get D.d f)) : D.InvF (p + 1) f ((D.stAt (p + 1)).get D.d f) := by
  have hget := D.get_next p f hso hb hf I.last_le
  have hblk : D.blk p f = ((D.stAt (p + 1)).get D.d f).idx := rfl
  generalize (D.stAt p).get D.d f = st at I hget
  by_cases hfp : f ∈ D.fs p
  · rw [if_pos hfp] at hget
    rw [hget] at hblk ⊢
    by_cases hj : st.joins (D.ua p f) (D.tag p) = true
    · obtain ⟨hi0, hty2, had⟩ := joins_spec hj
      have e : st.upd p (D.ua p f) (D.tag p) (D.out p) =
          { st with last := p + 1, cur := st.cur ||| D.out p } := by
        unfold FS.upd; rw [if_pos hj]
      rw [e] at hblk ⊢
      change D.blk p f = st.idx at hblk
      constructor
      · show p + 1 ≤ p + 1; exact Nat.le_refl _
      · intro z hz hfz
        show 0 < D.blk z f ∧ D.blk z f ≤ st.idx
        rcases (by omega : z < p ∨ z = p) with hz | hz
        · exact I.blk_le z hz hfz
        · rw [hz, hblk]; exact ⟨Nat.pos_of_ne_zero hi0, Nat.le_refl _⟩
      · intro z hz hfz he
        show admits st.ty _ _
        rcases (by omega : z < p ∨ z = p) with hz | hz
        · exact I.kind_ok z hz hfz he
        · rw [hz]; exact had
      · intro h2; exact absurd h2 hty2
      · intro z hz hfz hlt
        show Sub (D.out z) st.prev
        rcases (by omega : z < p ∨ z = p) with hz | hz
        · exact I.prev z hz hfz hlt
        · rw [hz, hblk] at hlt; exact absurd hlt (Nat.lt_irrefl _)
      · intro z hz hfz he
        show Sub (D.out z) (st.cur ||| D.out p)
        rcases (by omega : z < p ∨ z = p) with hz | hz
        · intro i hi; rw [Nat.testBit_or, I.cur z hz hfz he i hi, Bool.true_or]
        · intro i hi; rw [hz] at hi; rw [Nat.testBit_or, hi, Bool.or_true]
    · have e : st.upd p (D.ua p f) (D.tag p) (D.out p) =
          ⟨p + 1, st.idx + 1, newTy (D.ua p f) (D.tag p), st.prev ||| st.cur, D.out p⟩ := by
        unfold FS.upd; rw [if_neg hj]
      rw [e] at hblk ⊢
      change D.blk p f = st.idx + 1 at hblk
      constructor
      · show p + 1 ≤ p + 1; exact Nat.le_refl _
      · intro z hz hfz
        show 0 < D.blk z f ∧ D.blk z f ≤ st.idx + 1
        rcases (by omega : z < p ∨ z = p) with hz | hz
        · have := I.blk_le z hz hfz; omega
        · rw [hz, hblk]; omega
      · intro z hz hfz he
        change D.blk z f = st.idx + 1 at he
        show admits (newTy _ _) _ _
        rcases (by omega : z < p ∨ z = p) with hz | hz
        · have := I.blk_le z hz hfz; omega
        · rw [hz]; exact newTy_admits _ _
      · intro _ z z' hz hz' hfz hfz' he he'
        change D.blk z f = st.idx + 1 at he
        change D.blk z' f = st.idx + 1 at he'
        rcases (by omega : z < p ∨ z = p) with hz | hz
        · have := I.blk_le z hz hfz; omega
        rcases (by omega : z' < p ∨ z' = p) with hz' | hz'
        · have := I.blk_le z' hz' hfz'; omega
        omega
      · intro z hz hfz hlt
        change D.blk z f < st.idx + 1 at hlt
        show Sub (D.out z) (st.prev ||| st.cur)
        rcases (by omega : z < p ∨ z = p) with hz | hz
        · intro i hi
          rw [Nat.testBit_or]
          rcases Nat.lt_or_eq_of_le (Nat.le_of_lt_succ hlt) with h | h
          · rw [I.prev z hz hfz h i hi, Bool.true_or]
          · rw [I.cur z hz hfz h i hi, Bool.or_true]
        · rw [hz, hblk] at hlt; exact absurd hlt (Nat.lt_irrefl _)
      · intro z hz hfz he
        change D.blk z f = st.idx + 1 at he
        show Sub (D.out z) (D.out p)
        rcases (by omega : z < p ∨ z = p) with hz | hz
        · have := I.blk_le z hz hfz; omega
        · rw [hz]; exact fun i hi => hi
  · rw [if_neg hfp] at hget
    rw [hget]
    have notp : ∀ z, z < p + 1 → f ∈ D.fs z → z < p := by
      intro z hz hfz
      rcases (by omega : z < p ∨ z = p) with hz | hz
      · exact hz
      · rw [hz] at hfz; exact absurd hfz hfp
    exact ⟨by have := I.last_le; omega,
      fun z hz hfz => I.blk_le z (notp z hz hfz) hfz,
      fun z hz hfz => I.kind_ok z (notp z hz hfz) hfz,
      fun h2 z z' hz hz' hfz hfz' => I.single h2 z z' (notp z hz hfz) (notp z' hz' hfz') hfz hfz',
      fun z hz hfz => I.prev z (notp z hz hfz) hfz,
      fun z hz hfz => I.cur z (notp z hz hfz) hfz⟩

theorem inv (hs : D.Ok) : ∀ p, p ≤ D.n → ∀ f, f < 2 ^ D.d → D.InvF p f ((D.stAt p).get D.d f)
  | 0, _, f, _ => D.inv_zero f
  | p + 1, hp, f, hf =>
    D.inv_step p f (D.stepOk_sorted (hs p (by omega))) (D.stepOk_bound (hs p (by omega))) hf (inv hs p (by omega) f hf)

theorem bound (hs : D.Ok) {x f : Nat} (hx : x < D.n) (hf : f ∈ D.fs x) : f < 2 ^ D.d :=
  D.stepOk_bound (hs x hx) f hf

theorem blk_upd (hs : D.Ok) (x f : Nat) (hx : x < D.n) (hf : f ∈ D.fs x) :
    D.blk x f = (((D.stAt x).get D.d f).upd x (D.ua x f) (D.tag x) (D.out x)).idx := by
  have I := D.inv hs x (by omega) f (D.bound hs hx hf)
  show ((D.stAt (x + 1)).get D.d f).idx = _
  rw [D.get_next x f (D.stepOk_sorted (hs x hx)) (D.stepOk_bound (hs x hx)) (D.bound hs hx hf) I.last_le, if_pos hf]

/-- The scan's blocks are a block certificate. -/
theorem blockOK (hs : D.Ok) : BlockOK D.n D.fp D.tag D.blk := by
  intro x y f u v hxy hyn hu hv
  have hfx := D.mem_fs hu
  have hfy := D.mem_fs hv
  have hbd := D.bound hs hyn hfy
  have Iy := D.inv hs y (by omega) f hbd
  have Iy1 := D.inv hs (y + 1) (by omega) f hbd
  have hby := D.blk_upd hs y f hyn hfy
  have hge : ((D.stAt y).get D.d f).idx ≤ D.blk y f := by
    rw [hby, FS.upd_idx]; split <;> omega
  have hle := (Iy.blk_le x hxy hfx).2
  refine ⟨by omega, fun he => ?_⟩
  have hcur : D.blk y f = ((D.stAt (y + 1)).get D.d f).idx := rfl
  have ax := Iy1.kind_ok x (by omega) hfx (by rw [he]; exact hcur)
  have ay := Iy1.kind_ok y (by omega) hfy hcur
  rw [D.ua_eq hu] at ax
  rw [D.ua_eq hv] at ay
  by_cases h2 : ((D.stAt (y + 1)).get D.d f).ty = 2
  · have := Iy1.single h2 x y (by omega) (by omega) hfx hfy (by rw [he]; exact hcur) hcur; omega
  · exact admits_compat ax ay h2

/-- A use in a later block of a field is reached by what the earlier use passes on. -/
theorem closure (hs : D.Ok) (x y f : Nat) (hxy : x < y) (hyn : y < D.n)
    (hfx : f ∈ D.fs x) (hfy : f ∈ D.fs y) (hlt : D.blk x f < D.blk y f) :
    Sub (D.out x) (D.rby y) := by
  have hbd := D.bound hs hyn hfy
  have Iy := D.inv hs y (by omega) f hbd
  have hby := D.blk_upd hs y f hyn hfy
  intro i hi
  apply D.reachIn_ge (D.stAt y) y f i (D.stepOk_sorted (hs y hyn)) hfy
  generalize (D.stAt y).get D.d f = st at Iy hby
  unfold FS.contrib
  rw [FS.upd_idx] at hby
  by_cases hj : st.joins (D.ua y f) (D.tag y) = true
  · rw [if_pos hj] at hby ⊢
    exact Iy.prev x hxy hfx (by omega) i hi
  · rw [if_neg hj] at hby ⊢
    rw [Nat.testBit_or]
    rcases Nat.lt_or_eq_of_le (Iy.blk_le x hxy hfx).2 with h | h
    · rw [Iy.prev x hxy hfx h i hi, Bool.true_or]
    · rw [Iy.cur x hxy hfx h i hi, Bool.or_true]

theorem out_meth (x a : Nat) (h : D.meth x = a + 1) : (D.out x).testBit a = true := by
  unfold out outOf mbit
  rw [Nat.testBit_or, if_neg (by omega), h, Nat.add_sub_cancel, Nat.testBit_two_pow_self, Bool.true_or]

theorem out_pass (x a : Nat) (h : D.passable x = true) (ha : a < D.M)
    (hr : ((D.rby x).testBit a || (D.rby x).testBit (a + D.M)) = true) : (D.out x).testBit (a + D.M) = true := by
  have e : a + D.M - D.M = a := by omega
  unfold out outOf lift
  rw [if_pos h, Nat.testBit_or, Nat.testBit_shiftLeft, e, Nat.testBit_or, Nat.testBit_mod_two_pow,
    Nat.testBit_shiftRight, Nat.add_comm D.M a]
  simp only [ge_iff_le, Nat.le_add_left, decide_true, Bool.true_and, ha]
  rw [hr, Bool.or_true]

end Design

/-! ### The parent's orders

The parent calls methods from its rules.  `P : List (List Nat)` lists, in
order, the calls of each parent rule that fires this cycle (a group); the
flattened list is the order of all calls.  A group runs atomically: no rule
of the module may run between two of its calls.  That is what SBR adds to SB
(bsc exports SBR when a rule lies on a path between the two methods), so the
scan tracks paths through rules separately (`Design.allowed`, bits `M + a`). -/

/-- The position of `a` in a list. -/
def posIn : List Nat → Nat → Nat
  | [], _ => 0
  | c :: l, a => if c = a then 0 else posIn l a + 1

theorem posIn_lt : ∀ (P : List Nat) (a b : Nat), P.Nodup → [a, b] <+ P → posIn P a < posIn P b
  | [], a, b, _, h => by cases List.sublist_nil.mp h
  | c :: l, a, b, hnd, h => by
    have hc : c ∉ l := (List.nodup_cons.mp hnd).1
    rcases List.sublist_cons_iff.mp h with h' | ⟨r, hr, h'⟩
    · have ha : a ∈ l := h'.subset (by simp)
      have hb : b ∈ l := h'.subset (by simp)
      have hca : c ≠ a := fun e => hc (e ▸ ha)
      have hcb : c ≠ b := fun e => hc (e ▸ hb)
      simp only [posIn, if_neg hca, if_neg hcb]
      have := posIn_lt l a b (List.nodup_cons.mp hnd).2 h'
      omega
    · simp only [List.cons.injEq] at hr
      obtain ⟨hac, hr2⟩ := hr
      subst hr2
      have hb : b ∈ l := h'.subset (by simp)
      have hcb : c ≠ b := fun e => hc (e ▸ hb)
      simp only [posIn, if_pos hac.symm, if_neg hcb]
      omega

/-- The largest value of `g` on a list (0 if empty). -/
def maxOf (l : List Nat) (g : Nat → Nat) : Nat := l.foldr (fun a m => max m (g a)) 0

theorem le_maxOf (g : Nat → Nat) : ∀ (l : List Nat) (a : Nat), a ∈ l → g a ≤ maxOf l g
  | [], a, h => by cases h
  | b :: l, a, h => by
    unfold maxOf; rw [List.foldr_cons]
    rcases List.mem_cons.mp h with e | e
    · rw [e]; exact Nat.le_max_right _ _
    · exact Nat.le_trans (le_maxOf g l a e) (Nat.le_max_left _ _)

theorem maxOf_le (g : Nat → Nat) (T : Nat) : ∀ (l : List Nat), (∀ a ∈ l, g a ≤ T) → maxOf l g ≤ T
  | [], _ => Nat.zero_le _
  | b :: l, h => by
    unfold maxOf; rw [List.foldr_cons]
    exact Nat.max_le.mpr ⟨maxOf_le g T l (fun a ha => h a (List.mem_cons_of_mem _ ha)),
      h b (List.mem_cons_self ..)⟩

theorem either_sublist {x y : Nat} : ∀ (l : List Nat), x ∈ l → y ∈ l → x ≠ y → [x, y] <+ l ∨ [y, x] <+ l
  | [], hx, _, _ => by cases hx
  | c :: l, hx, hy, hne => by
    rcases List.mem_cons.mp hx with ex | ex <;> rcases List.mem_cons.mp hy with ey | ey
    · exact absurd (ex.trans ey.symm) hne
    · rw [ex]; exact Or.inl (List.cons_sublist_cons.mpr (List.singleton_sublist.mpr ey))
    · rw [ey]; exact Or.inr (List.cons_sublist_cons.mpr (List.singleton_sublist.mpr ex))
    · rcases either_sublist l ex ey hne with h | h
      · exact Or.inl (h.trans (List.sublist_cons_self _ _))
      · exact Or.inr (h.trans (List.sublist_cons_self _ _))

/-- The index of the group of `P` that calls `a`. -/
def gidOf : List (List Nat) → Nat → Nat
  | [], _ => 0
  | g :: gs, a => if a ∈ g then 0 else gidOf gs a + 1

/-- The position of `a` within its group. -/
def gpos : List (List Nat) → Nat → Nat
  | [], _ => 0
  | g :: gs, a => if a ∈ g then posIn g a else gpos gs a

theorem nodup_groups {g : List Nat} {gs : List (List Nat)} (hnd : (g :: gs).flatten.Nodup) :
    g.Nodup ∧ gs.flatten.Nodup ∧ ∀ a ∈ g, a ∉ gs.flatten := by
  rw [List.flatten_cons] at hnd
  obtain ⟨h1, h2, h3⟩ := List.nodup_append.mp hnd
  exact ⟨h1, h2, fun a ha hb => h3 a ha a hb rfl⟩

/-- Calls in order in the flattened list are in order of group, then position. -/
theorem flat_order : ∀ (P : List (List Nat)) (a b : Nat), P.flatten.Nodup → [a, b] <+ P.flatten →
    gidOf P a < gidOf P b ∨ (gidOf P a = gidOf P b ∧ gpos P a < gpos P b)
  | [], a, b, _, h => by simp at h
  | g :: gs, a, b, hnd, h => by
    obtain ⟨hg, hgs, hdis⟩ := nodup_groups hnd
    rw [List.flatten_cons] at h
    obtain ⟨l₁, l₂, e, h1, h2⟩ := List.sublist_append_iff.mp h
    rcases l₁ with _ | ⟨x1, _ | ⟨x2, _ | ⟨x3, l₁⟩⟩⟩
    · simp only [List.nil_append] at e
      subst e
      have ha : a ∉ g := fun m => hdis a m (h2.subset (by simp))
      have hb : b ∉ g := fun m => hdis b m (h2.subset (by simp))
      simp only [gidOf, gpos, if_neg ha, if_neg hb]
      have := flat_order gs a b hgs h2
      omega
    · simp only [List.cons_append, List.nil_append, List.cons.injEq] at e
      obtain ⟨rfl, rfl⟩ := e
      have ha : a ∈ g := h1.subset (by simp)
      have hb : b ∉ g := fun m => hdis b m (h2.subset (by simp))
      simp only [gidOf, if_pos ha, if_neg hb]
      omega
    · simp only [List.cons_append, List.nil_append, List.cons.injEq] at e
      obtain ⟨rfl, rfl, rfl⟩ := e
      have ha : a ∈ g := h1.subset (by simp)
      have hb : b ∈ g := h1.subset (by simp)
      simp only [gidOf, gpos, if_pos ha, if_pos hb]
      simp only [Nat.lt_irrefl, false_or, true_and]
      exact posIn_lt g a b hg h1
    · simp only [List.cons_append, List.cons.injEq] at e
      obtain ⟨_, _, e⟩ := e
      cases e

theorem group_of_mem {g h : List Nat} {gs : List (List Nat)} {a : Nat} (hnd : (h :: gs).flatten.Nodup)
    (hgP : g ∈ h :: gs) (ha : a ∈ g) (hah : a ∈ h) : g = h := by
  rcases List.mem_cons.mp hgP with e | e
  · exact e
  · exact absurd (List.mem_flatten.mpr ⟨g, e, ha⟩) ((nodup_groups hnd).2.2 a hah)

/-- A call with the group index of a member of group `g` is in `g`. -/
theorem gid_mem : ∀ (P : List (List Nat)) (g : List Nat) (a c : Nat), P.flatten.Nodup → g ∈ P → a ∈ g →
    c ∈ P.flatten → gidOf P c = gidOf P a → c ∈ g
  | [], g, _, _, _, hgP, _, _, _ => by cases hgP
  | h :: gs, g, a, c, hnd, hgP, ha, hc, he => by
    by_cases hah : a ∈ h
    · rw [group_of_mem hnd hgP ha hah]
      simp only [gidOf, if_pos hah] at he
      by_cases hch : c ∈ h
      · exact hch
      · simp only [if_neg hch] at he; omega
    · have hg : g ∈ gs := by
        rcases List.mem_cons.mp hgP with e | e
        · exact absurd (e ▸ ha) hah
        · exact e
      by_cases hch : c ∈ h
      · simp only [gidOf, if_pos hch, if_neg hah] at he; omega
      · simp only [gidOf, if_neg hch, if_neg hah] at he
        rw [List.flatten_cons, List.mem_append] at hc
        exact gid_mem gs g a c (nodup_groups hnd).2.1 hg ha (hc.resolve_left hch) (by omega)

/-- Two calls of one group have its index. -/
theorem gid_eq : ∀ (P : List (List Nat)) (g : List Nat) (a b : Nat), P.flatten.Nodup → g ∈ P → a ∈ g →
    b ∈ g → gidOf P a = gidOf P b
  | [], g, _, _, _, hgP, _, _ => by cases hgP
  | h :: gs, g, a, b, hnd, hgP, ha, hb => by
    by_cases hah : a ∈ h
    · have e := group_of_mem hnd hgP ha hah
      subst e
      simp only [gidOf, if_pos hah, if_pos hb]
    · have hg : g ∈ gs := by
        rcases List.mem_cons.mp hgP with e | e
        · exact absurd (e ▸ ha) hah
        · exact e
      have hbh : b ∉ h := fun m => (nodup_groups hnd).2.2 b m (List.mem_flatten.mpr ⟨g, hg, hb⟩)
      simp only [gidOf, if_neg hah, if_neg hbh]
      rw [gid_eq gs g a b (nodup_groups hnd).2.1 hg ha hb]

/-- `π` runs the steps of the methods the parent calls in the order it calls them. -/
def Extends (meth : Nat → Nat) (n : Nat) (P : List (List Nat)) (π : List Nat) : Prop :=
  ∀ a b x y, [a, b] <+ P.flatten → x < n → y < n → meth x = a + 1 → meth y = b + 1 → [x, y] <+ π

/-- Each parent rule's calls are contiguous in `π`: between two steps of
    methods it calls there are only steps of methods it calls. -/
def Contiguous (meth : Nat → Nat) (P : List (List Nat)) (π : List Nat) : Prop :=
  ∀ g ∈ P, ∀ x z y a b, [x, z, y] <+ π → meth x = a + 1 → meth y = b + 1 → a ∈ g → b ∈ g →
    ∃ c ∈ g, meth z = c + 1

namespace Design

variable (D : Design)

/-- Method `a` has a step that fires this cycle. -/
def active (fire : Nat → Bool) (a : Nat) : Bool :=
  (List.range D.n).any (fun z => D.meth z == a + 1 && fire z)

/-- Method `a` fires this cycle and reaches step `x`, directly or through a rule. -/
def hits (fire : Nat → Bool) (x a : Nat) : Bool :=
  ((D.rby x).testBit a || (D.rby x).testBit (a + D.M)) && D.active fire a

/-- The order the proof builds sorts steps by `(key1, key2, σ position)`.  A step of a
    method the parent calls goes inside that call's group (odd `key1`); any other
    step goes right after the last group with a firing call that reaches it
    (even `key1`), so never inside a group. -/
def key1 (fire : Nat → Bool) (P : List (List Nat)) (x : Nat) : Nat :=
  if D.meth x ≠ 0 ∧ D.meth x - 1 ∈ P.flatten then 2 * gidOf P (D.meth x - 1) + 1
  else 2 * maxOf (P.flatten.filter (D.hits fire x)) (fun a => gidOf P a + 1)

def key2 (P : List (List Nat)) (x : Nat) : Nat :=
  if D.meth x ≠ 0 ∧ D.meth x - 1 ∈ P.flatten then gpos P (D.meth x - 1) else 0

/-- Step `x` sorts no later than step `y`. -/
def Before (fire : Nat → Bool) (P : List (List Nat)) (x y : Nat) : Prop :=
  D.key1 fire P x < D.key1 fire P y ∨ (D.key1 fire P x = D.key1 fire P y ∧
    (D.key2 P x < D.key2 P y ∨ (D.key2 P x = D.key2 P y ∧ x ≤ y)))

instance (fire : Nat → Bool) (P : List (List Nat)) (x y : Nat) : Decidable (D.Before fire P x y) := by
  unfold Before; infer_instance

/-- The order the proof builds. -/
def order (fire : Nat → Bool) (P : List (List Nat)) : List Nat :=
  (List.range D.n).mergeSort (fun x y => decide (D.Before fire P x y))

theorem order_perm (fire : Nat → Bool) (P : List (List Nat)) : (D.order fire P).Perm (List.range D.n) :=
  List.mergeSort_perm _ _

theorem order_sorted (fire : Nat → Bool) (P : List (List Nat)) {x y : Nat} (h : [x, y] <+ D.order fire P) :
    D.Before fire P x y := by
  have hp := List.pairwise_mergeSort (le := fun x y => decide (D.Before fire P x y))
    (fun a b c hab hbc => by simp only [Before, decide_eq_true_eq] at hab hbc ⊢; omega)
    (fun a b => by simp only [Before, Bool.or_eq_true, decide_eq_true_eq]; omega)
    (List.range D.n)
  have := List.pairwise_iff_forall_sublist.mp hp h
  simpa using this

theorem active_lt (hs : D.Ok) (fire : Nat → Bool) {a : Nat} (h : D.active fire a = true) :
    a < D.M ∧ ∃ z, z < D.n ∧ D.meth z = a + 1 ∧ fire z = true := by
  unfold active at h
  obtain ⟨z, hz, hz'⟩ := List.any_eq_true.mp h
  simp only [Bool.and_eq_true, beq_iff_eq] at hz'
  have := D.stepOk_methM (hs z (List.mem_range.mp hz))
  exact ⟨by omega, z, List.mem_range.mp hz, hz'.1, hz'.2⟩

/-- The parent's order, as the proof needs it: every firing step of an enable-gated
    method is called; a method allowed to reach another is called before it; one
    allowed to reach it only through a rule is called in an earlier group. -/
structure Fits (fire : Nat → Bool) (P : List (List Nat)) : Prop where
  nodup : P.flatten.Nodup
  gated : ∀ x, x < D.n → fire x = true → D.meth x ≠ 0 → D.gated (D.meth x - 1) = true →
    D.meth x - 1 ∈ P.flatten
  ordD : ∀ a b x y, x < D.n → y < D.n → (D.allowed b).testBit a = true → a < D.M → D.meth x = a + 1 →
    D.meth y = b + 1 → fire x = true → fire y = true → a ∈ P.flatten → b ∈ P.flatten → a ≠ b →
    [a, b] <+ P.flatten
  ordT : ∀ a b x y, x < D.n → y < D.n → (D.allowed b).testBit (a + D.M) = true → a < D.M →
    D.meth x = a + 1 → D.meth y = b + 1 → fire x = true → fire y = true → a ∈ P.flatten →
    b ∈ P.flatten → a ≠ b → gidOf P a < gidOf P b

theorem key_mono (hs : D.Ok) (fire : Nat → Bool) (P : List (List Nat)) (hF : D.Fits fire P)
    (x y f : Nat) (hxy : x < y) (hyn : y < D.n) (hfx : fire x = true) (hfy : fire y = true)
    (hux : f ∈ D.fs x) (huy : f ∈ D.fs y) (hlt : D.blk x f < D.blk y f) :
    D.Before fire P x y := by
  have hxn : x < D.n := by omega
  have hcl := D.closure hs x y f hxy hyn hux huy hlt
  have hchk := D.stepOk_meth (hs y hyn)
  have hMx := D.stepOk_methM (hs x hxn)
  -- a step outside the parent's calls that fires is not a step of an enable-gated method
  have hpass : ¬(D.meth x ≠ 0 ∧ D.meth x - 1 ∈ P.flatten) → D.passable x = true := by
    intro h
    unfold passable
    by_cases hm : D.meth x = 0
    · simp [hm]
    · cases hg : D.gated (D.meth x - 1)
      · simp
      · exact absurd ⟨hm, hF.gated x hxn hfx hm hg⟩ h
  -- a method hitting `x` reaches `y` through `x`, when `x` is outside the parent's calls
  have hthru : ¬(D.meth x ≠ 0 ∧ D.meth x - 1 ∈ P.flatten) → ∀ a, D.hits fire x a = true →
      (D.rby y).testBit (a + D.M) = true ∧ D.active fire a = true := by
    intro h a ha
    simp only [hits, Bool.and_eq_true] at ha
    exact ⟨hcl _ (D.out_pass x a (hpass h) (D.active_lt hs fire ha.2).1 ha.1), ha.2⟩
  have hactx : D.meth x ≠ 0 → D.active fire (D.meth x - 1) = true := by
    intro hm
    unfold active
    exact List.any_eq_true.mpr ⟨x, List.mem_range.mpr hxn, by simp [hfx]; omega⟩
  unfold Before key1 key2
  by_cases hPx : D.meth x ≠ 0 ∧ D.meth x - 1 ∈ P.flatten <;>
    by_cases hPy : D.meth y ≠ 0 ∧ D.meth y - 1 ∈ P.flatten
  · rw [if_pos hPx, if_pos hPy, if_pos hPx, if_pos hPy]
    have hb := hcl _ (D.out_meth x (D.meth x - 1) (by omega))
    obtain ⟨hal, hne, _⟩ := hchk hPy.1 _ hb
    have := flat_order P _ _ hF.nodup (hF.ordD _ _ x y hxn hyn hal (by omega) (by omega) (by omega)
      hfx hfy hPx.2 hPy.2 hne)
    omega
  · rw [if_pos hPx, if_neg hPy, if_pos hPx, if_neg hPy]
    have hb := hcl _ (D.out_meth x (D.meth x - 1) (by omega))
    have := le_maxOf (fun a => gidOf P a + 1)
      (P.flatten.filter (D.hits fire y)) (D.meth x - 1)
      (List.mem_filter.mpr ⟨hPx.2, by simp [hits, hb, hactx hPx.1]⟩)
    omega
  · rw [if_neg hPx, if_pos hPy, if_neg hPx, if_pos hPy]
    have := maxOf_le (fun a => gidOf P a + 1) (gidOf P (D.meth y - 1))
      (P.flatten.filter (D.hits fire x)) (fun a ha => by
      obtain ⟨haP, hq⟩ := List.mem_filter.mp ha
      obtain ⟨hbit, hact⟩ := hthru hPx a hq
      obtain ⟨hal, _, hne⟩ := hchk hPy.1 _ hbit
      obtain ⟨haM, z, hz, hmz, hfz⟩ := D.active_lt hs fire hact
      have := hF.ordT a (D.meth y - 1) z y hz hyn hal haM hmz (by omega) hfz hfy haP hPy.2 (by omega)
      show gidOf P a + 1 ≤ _
      omega)
    omega
  · rw [if_neg hPx, if_neg hPy, if_neg hPx, if_neg hPy]
    have := maxOf_le (fun a => gidOf P a + 1)
      (maxOf (P.flatten.filter (D.hits fire y)) (fun a => gidOf P a + 1))
      (P.flatten.filter (D.hits fire x)) (fun a ha => by
      obtain ⟨haP, hq⟩ := List.mem_filter.mp ha
      obtain ⟨hbit, hact⟩ := hthru hPx a hq
      exact le_maxOf (fun a => gidOf P a + 1)
        (P.flatten.filter (D.hits fire y)) a (List.mem_filter.mpr ⟨haP, by simp [hits, hbit, hact]⟩))
    omega

end Design

/-- **(C)** If the scan passes, then for every cycle and every way `P` the parent may
    call the methods (`Design.Fits`), there is an order of all the steps that runs
    the calls in `P`'s order, keeps each parent rule's calls together, and ends in
    the schedule's state.

    Of the cycle: `fire` says which steps fire; a step that does not fire commutes
    with every step (`hidle`); steps of one exception group commute (`htag`). -/
theorem all_orders (D : Design) (hs : D.Ok) (E : Nat → Eff V) (hE : ∀ k, (E k).fp = D.fp k)
    (Inv : St V → Prop) (hk : Keeps Inv (stepOf E)) (fire : Nat → Bool)
    (hidle : IdleComm Inv (stepOf E) D.n fire)
    (htag : TagComm Inv (stepOf E) D.n D.tag)
    (P : List (List Nat)) (hF : D.Fits fire P) :
    ∃ π : List Nat, π.Perm (List.range D.n) ∧ Extends D.meth D.n P π ∧ Contiguous D.meth P π ∧
      ∀ s, Inv s → run (stepOf E) π s = run (stepOf E) (List.range D.n) s := by
  have hperm := D.order_perm fire P
  have hmem : ∀ x, x < D.n → x ∈ D.order fire P := fun x hx => hperm.symm.subset (List.mem_range.mpr hx)
  have hlt : ∀ x, x ∈ D.order fire P → x < D.n := fun x hx => List.mem_range.mp (hperm.subset hx)
  have hndπ : (D.order fire P).Nodup := hperm.symm.nodup List.nodup_range
  have hk1 : ∀ x a, D.meth x = a + 1 → a ∈ P.flatten → D.key1 fire P x = 2 * gidOf P a + 1 ∧
      D.key2 P x = gpos P a := by
    intro x a hm ha
    unfold Design.key1 Design.key2
    rw [if_pos ⟨by omega, by rw [hm]; exact ha⟩, if_pos ⟨by omega, by rw [hm]; exact ha⟩, hm,
      Nat.add_sub_cancel]
    simp
  refine ⟨D.order fire P, hperm, ?_, ?_, ?_⟩
  · intro a b x y hab hx hy hmx hmy
    have haP : a ∈ P.flatten := hab.subset (by simp)
    have hbP : b ∈ P.flatten := hab.subset (by simp)
    have ho := flat_order P a b hF.nodup hab
    have hka := hk1 x a hmx haP
    have hkb := hk1 y b hmy hbP
    have hne : x ≠ y := by
      intro e; rw [e] at hmx; rw [hmx] at hmy
      have : a = b := by omega
      subst this
      omega
    rcases either_sublist _ (hmem x hx) (hmem y hy) hne with h | h
    · exact h
    · have := D.order_sorted fire P h
      unfold Design.Before at this
      omega
  · intro g hg x z y a b h hmx hmy ha hb
    have hxz : [x, z] <+ D.order fire P :=
      (List.cons_sublist_cons.mpr (List.cons_sublist_cons.mpr (List.nil_sublist _))).trans h
    have hzy : [z, y] <+ D.order fire P := (List.sublist_cons_self _ _).trans h
    have b1 := D.order_sorted fire P hxz
    have b2 := D.order_sorted fire P hzy
    have haP : a ∈ P.flatten := List.mem_flatten.mpr ⟨g, hg, ha⟩
    have hbP : b ∈ P.flatten := List.mem_flatten.mpr ⟨g, hg, hb⟩
    have hka := hk1 x a hmx haP
    have hkb := hk1 y b hmy hbP
    have hgab := gid_eq P g a b hF.nodup hg ha hb
    unfold Design.Before at b1 b2
    by_cases hPz : D.meth z ≠ 0 ∧ D.meth z - 1 ∈ P.flatten
    · have hkz := hk1 z (D.meth z - 1) (by omega) hPz.2
      refine ⟨D.meth z - 1, gid_mem P g a _ hF.nodup hg ha hPz.2 (by omega), by omega⟩
    · have : D.key1 fire P z = 2 * maxOf (P.flatten.filter (D.hits fire z)) (fun a => gidOf P a + 1) := by
        unfold Design.key1; rw [if_neg hPz]
      omega
  · have e : (fun k => (E k).fp) = D.fp := funext hE
    apply blocks_serial Inv E hk D.n D.tag D.blk fire (by rw [e]; exact D.blockOK hs) hidle htag _ hperm
    intro x y f hxy hfx hfy hsx hsy
    rw [e] at hsx hsy
    obtain ⟨u, hu⟩ := Option.isSome_iff_exists.mp hsx
    obtain ⟨v, hv⟩ := Option.isSome_iff_exists.mp hsy
    have hxn := hlt x (hxy.subset (by simp))
    have hyn := hlt y (hxy.subset (by simp))
    have hne : x ≠ y := by
      have := hndπ.sublist hxy
      simp at this; exact this
    rcases Nat.lt_or_gt_of_ne hne with h | h
    · exact (D.blockOK hs x y f u v h hyn hu hv).1
    · have hle := (D.blockOK hs y x f v u h hxn hv hu).1
      rcases Nat.lt_or_eq_of_le hle with h' | h'
      · have hk := D.key_mono hs fire P hF y x f h hxn hfy hfx (D.mem_fs hv) (D.mem_fs hu) h'
        have := D.order_sorted fire P hxy
        unfold Design.Before at hk this
        omega
      · omega

/-! ## The parent's contract -/

/-- The bit set of a list of numbers. -/
def bits (l : List Nat) : Nat := l.foldl (fun acc a => acc ||| 2 ^ a) 0

theorem mem_of_testBit_bits {l : List Nat} {a : Nat} (h : (bits l).testBit a = true) : a ∈ l := by
  unfold bits at h
  rw [testBit_foldl_or l (fun a => 2 ^ a) 0 a] at h
  simp only [Nat.zero_testBit, Bool.false_or, List.any_eq_true, Nat.testBit_two_pow,
    decide_eq_true_eq] at h
  obtain ⟨x, hx, e⟩ := h
  exact e ▸ hx

/-- A method's allowed bits (`Design.allowed`) from the exported annotations.
    Directly (bit `a`): the methods that must precede it (SB, SBR, `unv`), those
    exported C with it and those never firing with it (`nba`).  Through a rule (bit
    `M + a`): only those that may not be called with it from one parent rule (SBR,
    C, `nba`, `unv`). -/
def allowedOf (M : Nat) (sb sbr excl nba unv : List Nat) : Nat :=
  bits (sb ++ sbr ++ excl ++ nba ++ unv) % 2 ^ M ||| (bits (sbr ++ excl ++ nba ++ unv) % 2 ^ M) <<< M

theorem allowedOf_direct {M : Nat} {sb sbr excl nba unv : List Nat} {a : Nat} (ha : a < M)
    (h : (allowedOf M sb sbr excl nba unv).testBit a = true) : a ∈ sb ++ sbr ++ excl ++ nba ++ unv := by
  unfold allowedOf at h
  rw [Nat.testBit_or, Nat.testBit_shiftLeft, Nat.testBit_mod_two_pow] at h
  simp only [ha, decide_true, Bool.true_and, ge_iff_le, show ¬ M ≤ a from by omega, decide_false,
    Bool.false_and, Bool.or_false] at h
  exact mem_of_testBit_bits h

theorem allowedOf_thru {M : Nat} {sb sbr excl nba unv : List Nat} {a : Nat}
    (h : (allowedOf M sb sbr excl nba unv).testBit (a + M) = true) : a ∈ sbr ++ excl ++ nba ++ unv := by
  unfold allowedOf at h
  rw [Nat.testBit_or, Nat.testBit_shiftLeft, Nat.testBit_mod_two_pow, Nat.testBit_mod_two_pow,
    Nat.add_sub_cancel] at h
  simp only [show ¬ a + M < M from by omega, decide_false, Bool.false_and, Bool.false_or] at h
  simp only [Bool.and_eq_true] at h
  exact mem_of_testBit_bits h.2.2

/-- The parent's side of the contract for one cycle.  `P` lists, in order, the calls
    of each parent rule that fires (one list per rule); flattened, it is the order of
    all calls.  No method is called twice (a design gives each value method
    without arguments a fixed number of read copies, each its own method, so a
    parent reading it several times calls a different copy each time); every
    method that has an enable port (`hasEnable`) and is enabled (`en`) is called;
    no call comes after a call of a method exported SB or SBR before it (`sb q`,
    `sbr q`: the methods that must precede `q`); no parent rule calls both
    methods of a pair exported SBR (a method's annotation with itself is never
    used); no two methods exported C are both called.  A pair exported CF may come in either order, in one
    parent rule or not.

    `unv q` lists exported CF pairs the generator could not verify (normally
    empty): the theorem then assumes the parent treats them as SBR.  It is part of
    the statement so that such a narrowing is visible. -/
structure ParentOrder (hasEnable : Nat → Bool) (sb sbr excl unv : Nat → List Nat) (en : Nat → Bool)
    (P : List (List Nat)) : Prop where
  nodup : P.flatten.Nodup
  enabled : ∀ m, hasEnable m = true → en m = true → m ∈ P.flatten
  sb : ∀ p q, [p, q] <+ P.flatten → q ∉ sb p ∧ q ∉ sbr p ∧ q ∉ unv p
  sbr : ∀ g ∈ P, ∀ p ∈ g, ∀ q ∈ g, p ≠ q → q ∉ sbr p ∧ q ∉ unv p
  c : ∀ p q, [p, q] <+ P.flatten → q ∉ excl p ∧ p ∉ excl q

/-- A decidable check that a concrete order respects the annotations (for
    examples; the theorems take any order). -/
def respects (sb sbr excl unv : Nat → List Nat) : List Nat → Bool
  | [] => true
  | p :: l => l.all (fun q => !(sb p).contains q && !(sbr p).contains q && !(unv p).contains q &&
      !(excl p).contains q && !(excl q).contains p) && respects sb sbr excl unv l

/-- No parent rule calls an SBR (or `unv`) pair. -/
def groupsOK (sbr unv : Nat → List Nat) (P : List (List Nat)) : Bool :=
  P.all (fun g => g.all (fun p => g.all (fun q => p == q || (!(sbr p).contains q && !(unv p).contains q))))

theorem respects_sound (sb sbr excl unv : Nat → List Nat) :
    ∀ (P : List Nat), respects sb sbr excl unv P = true →
      ∀ p q, [p, q] <+ P → (q ∉ sb p ∧ q ∉ sbr p ∧ q ∉ unv p) ∧ q ∉ excl p ∧ p ∉ excl q
  | [], _, p, q, h => by cases List.sublist_nil.mp h
  | c :: l, hr, p, q, h => by
    simp only [respects, Bool.and_eq_true, List.all_eq_true] at hr
    rcases List.sublist_cons_iff.mp h with h' | ⟨r, hr', h'⟩
    · exact respects_sound sb sbr excl unv l hr.2 p q h'
    · simp only [List.cons.injEq] at hr'
      obtain ⟨hpc, hr2⟩ := hr'
      subst hr2
      have hq : q ∈ l := h'.subset (by simp)
      have := hr.1 q hq
      simp only [Bool.and_eq_true, Bool.not_eq_true'] at this
      rw [hpc]
      obtain ⟨⟨⟨⟨h1, h2⟩, h3⟩, h4⟩, h5⟩ := this
      refine ⟨⟨fun m => ?_, fun m => ?_, fun m => ?_⟩, fun m => ?_, fun m => ?_⟩
      · rw [List.contains_iff_mem.mpr m] at h1; cases h1
      · rw [List.contains_iff_mem.mpr m] at h2; cases h2
      · rw [List.contains_iff_mem.mpr m] at h3; cases h3
      · rw [List.contains_iff_mem.mpr m] at h4; cases h4
      · rw [List.contains_iff_mem.mpr m] at h5; cases h5

theorem ParentOrder.of_check {hasEnable : Nat → Bool} {sb sbr excl unv : Nat → List Nat} {en : Nat → Bool}
    {P : List (List Nat)} (hnd : P.flatten.Nodup) (hen : ∀ m, hasEnable m = true → en m = true → m ∈ P.flatten)
    (hr : respects sb sbr excl unv P.flatten = true) (hg : groupsOK sbr unv P = true) :
    ParentOrder hasEnable sb sbr excl unv en P := by
  refine ⟨hnd, hen, fun p q h => (respects_sound _ _ _ _ _ hr p q h).1, ?_,
    fun p q h => (respects_sound _ _ _ _ _ hr p q h).2⟩
  intro g hgP p hp q hq hpq
  have := List.all_eq_true.mp (List.all_eq_true.mp (List.all_eq_true.mp hg g hgP) p hp) q hq
  simp only [Bool.or_eq_true, beq_iff_eq, hpq, false_or, Bool.and_eq_true, Bool.not_eq_true'] at this
  obtain ⟨h1, h2⟩ := this
  refine ⟨fun m => ?_, fun m => ?_⟩
  · rw [List.contains_iff_mem.mpr m] at h1; cases h1
  · rw [List.contains_iff_mem.mpr m] at h2; cases h2

/-- **(C)**, with the parent's contract: if the scan passes with each method's
    allowed bits built from the exported annotations (`allowedOf`), then every
    way a parent may call the methods extends to an order of all the steps that
    keeps each parent rule's calls together and ends in the schedule's state.

    `nba b` lists methods whose steps never fire in a cycle with `b`'s (facts of
    the design, `hnba`; a design gives them group by group, `nba_of_groups`).  Enable-gated methods are among those with an enable
    port (`hge`), and their steps fire only when enabled (`hen`). -/
theorem all_orders_parent (D : Design) (hs : D.Ok) (E : Nat → Eff V)
    (hE : ∀ k, (E k).fp = D.fp k) (Inv : St V → Prop) (hk : Keeps Inv (stepOf E)) (fire : Nat → Bool)
    (hidle : IdleComm Inv (stepOf E) D.n fire)
    (htag : TagComm Inv (stepOf E) D.n D.tag)
    (hasEnable : Nat → Bool) (sb sbr excl nba unv : Nat → List Nat)
    (hallow : ∀ b, D.allowed b = allowedOf D.M (sb b) (sbr b) (excl b) (nba b) (unv b))
    (en : Nat → Bool) (hge : ∀ m, D.gated m = true → hasEnable m = true)
    (hen : ∀ x, x < D.n → fire x = true → D.meth x ≠ 0 → D.gated (D.meth x - 1) = true →
      en (D.meth x - 1) = true)
    (hnba : ∀ a b x y, a ∈ nba b → a ≠ b → x < D.n → y < D.n → D.meth x = a + 1 → D.meth y = b + 1 →
      fire x = true → fire y = true → False)
    (P : List (List Nat)) (hP : ParentOrder hasEnable sb sbr excl unv en P) :
    ∃ π : List Nat, π.Perm (List.range D.n) ∧ Extends D.meth D.n P π ∧ Contiguous D.meth P π ∧
      ∀ s, Inv s → run (stepOf E) π s = run (stepOf E) (List.range D.n) s := by
  apply all_orders D hs E hE Inv hk fire hidle htag P
  refine ⟨hP.nodup, ?_, ?_, ?_⟩
  · intro x hx hf hm hg
    exact hP.enabled _ (hge _ hg) (hen x hx hf hm hg)
  · intro a b x y hx hy hbit haM hmx hmy hfx hfy haP hbP hne
    rw [hallow b] at hbit
    have hmem := allowedOf_direct haM hbit
    rcases either_sublist _ haP hbP hne with h | h
    · exact h
    · exfalso
      obtain ⟨h1, h2, h3⟩ := hP.sb b a h
      have h4 := (hP.c b a h).1
      simp only [List.mem_append] at hmem
      rcases hmem with (((h5 | h5) | h5) | h5) | h5
      · exact h1 h5
      · exact h2 h5
      · exact h4 h5
      · exact hnba a b x y h5 hne hx hy hmx hmy hfx hfy
      · exact h3 h5
  · intro a b x y hx hy hbit haM hmx hmy hfx hfy haP hbP hne
    rw [hallow b] at hbit
    have hmem := allowedOf_thru hbit
    simp only [List.mem_append] at hmem
    rcases either_sublist _ haP hbP hne with h | h
    · rcases flat_order P a b hP.nodup h with h' | ⟨h', _⟩
      · exact h'
      · exfalso
        obtain ⟨g, hg, hag⟩ := List.mem_flatten.mp haP
        have hbg := gid_mem P g a b hP.nodup hg hag hbP h'.symm
        obtain ⟨h2, h3⟩ := hP.sbr g hg b hbg a hag (Ne.symm hne)
        have h4 := (hP.c a b h).2
        rcases hmem with ((h5 | h5) | h5) | h5
        · exact h2 h5
        · exact h4 h5
        · exact hnba a b x y h5 hne hx hy hmx hmy hfx hfy
        · exact h3 h5
    · exfalso
      obtain ⟨_, h2, h3⟩ := hP.sb b a h
      have h4 := (hP.c b a h).1
      rcases hmem with ((h5 | h5) | h5) | h5
      · exact h2 h5
      · exact h4 h5
      · exact hnba a b x y h5 hne hx hy hmx hmy hfx hfy
      · exact h3 h5

/-! ## Facts a design proves with these lemmas -/

/-- Independent steps commute. -/
theorem comm_of_indep (Inv : St V → Prop) (E : Nat → Eff V) (x y : Nat)
    (h : indep (E y).fp (E x).fp = true) : Comm Inv (stepOf E) x y :=
  fun s _ => Eff.comm (E y) (E x) h s

/-- A step that leaves every state as it is (as one that does not fire does)
    commutes with every step. -/
theorem comm_of_id (Inv : St V → Prop) (E : Nat → Eff V) (hk : Keeps Inv (stepOf E)) (x y : Nat)
    (h : ∀ s, Inv s → stepOf E x s = s) : Comm Inv (stepOf E) x y := by
  intro s hs
  rw [h s hs, h _ (hk y s hs)]

/-- A step that acts as another effect `I` independent of step `y` commutes with `y`.
    (A step that does not fire still sets the outputs only it sets, to values
    that do not depend on the state: its `I` sets just those.) -/
theorem comm_of_sim (Inv : St V → Prop) (E : Nat → Eff V) (hk : Keeps Inv (stepOf E)) (x y : Nat)
    (I : Eff V) (hsim : ∀ s, Inv s → stepOf E x s = I.apply s) (h : indep (E y).fp I.fp = true) :
    Comm Inv (stepOf E) x y := by
  intro s hs
  rw [hsim s hs, hsim _ (hk y s hs)]
  exact Eff.comm (E y) I h s

/-- Two steps that never both fire commute, when a step that does not fire
    commutes with every step. -/
theorem comm_of_excl {α : Type} {Inv : α → Prop} {step : Nat → α → α} {n : Nat} {fire : Nat → Bool}
    (hidle : IdleComm Inv step n fire) {x y : Nat} (hx : x < n) (hy : y < n)
    (h : (fire x && fire y) = false) : Comm Inv step x y := by
  cases hfx : fire x
  · exact hidle x y hx hy hfx
  · rw [hfx, Bool.true_and] at h
    exact (hidle y x hy hx h).symm

/-! ### Exception groups, linearly

`TagComm` speaks of every pair of steps; a design proves it group by group.
Each step's group membership is one decided fact per step (`hmem`), and each
group's members commute pairwise: an exclusion group because at most one of
them fires (`groupComm_of_chain`, from bsc's chain-form disjointness facts:
each member is disjoint from the OR of the ones before it), a semantic pair
by its `Comm` lemma (`groupComm_pair`). -/

/-- Every two members of a list of steps commute. -/
def GroupComm {α : Type} (Inv : α → Prop) (step : Nat → α → α) (L : List Nat) : Prop :=
  ∀ x ∈ L, ∀ y ∈ L, x ≠ y → Comm Inv step x y

theorem tagComm_of_groups {α : Type} (Inv : α → Prop) (step : Nat → α → α) (n G : Nat)
    (tag : Nat → Nat) (mem : Nat → List Nat)
    (hmem : ∀ x, x < n → tag x ≠ 0 → tag x < G ∧ x ∈ mem (tag x))
    (hgrp : ∀ t, t ≠ 0 → t < G → GroupComm Inv step (mem t)) : TagComm Inv step n tag := by
  intro x y hx hy hne ht h0
  have hx' := hmem x hx h0
  have hy' := hmem y hy (ht ▸ h0)
  rw [← ht] at hy'
  exact hgrp (tag x) h0 hx'.1 x hx'.2 y hy'.2 hne

/-- The chain form of "at most one fires": no member fires with an earlier one
    (`acc`: an earlier one fires). -/
def chainOK (fire : Nat → Bool) : List Nat → Bool → Bool
  | [], _ => true
  | x :: l, acc => !(acc && fire x) && chainOK fire l (acc || fire x)

theorem chainOK_true (fire : Nat → Bool) : ∀ (l : List Nat), chainOK fire l true = true → ∀ y ∈ l, fire y = false
  | [], _, y, hy => by cases hy
  | x :: l, h, y, hy => by
    simp only [chainOK, Bool.true_and, Bool.true_or, Bool.and_eq_true, Bool.not_eq_true'] at h
    rcases List.mem_cons.mp hy with rfl | hy
    · exact h.1
    · exact chainOK_true fire l h.2 y hy

theorem chainOK_excl (fire : Nat → Bool) : ∀ (l : List Nat) (acc : Bool), chainOK fire l acc = true →
    ∀ x ∈ l, ∀ y ∈ l, x ≠ y → (fire x && fire y) = false
  | [], _, _, x, hx, _, _, _ => by cases hx
  | z :: l, acc, h, x, hx, y, hy, hne => by
    simp only [chainOK, Bool.and_eq_true] at h
    have later : fire z = true → ∀ w ∈ l, fire w = false := fun hz =>
      chainOK_true fire l (by rw [hz, Bool.or_true] at h; exact h.2)
    rcases List.mem_cons.mp hx with ex | hx' <;> rcases List.mem_cons.mp hy with ey | hy'
    · exact absurd (ex.trans ey.symm) hne
    · subst ex
      cases hz : fire x
      · rfl
      · rw [later hz y hy']; rfl
    · subst ey
      cases hz : fire y
      · rw [Bool.and_false]
      · rw [later hz x hx']; rfl
    · exact chainOK_excl fire l _ h.2 x hx' y hy' hne

/-- An exclusion group, from its chain-form disjointness fact. -/
theorem groupComm_of_chain {α : Type} {Inv : α → Prop} {step : Nat → α → α} {n : Nat} {fire : Nat → Bool}
    (hidle : IdleComm Inv step n fire) (L : List Nat) (hL : ∀ x ∈ L, x < n)
    (h : chainOK fire L false = true) : GroupComm Inv step L :=
  fun x hx y hy hne => comm_of_excl hidle (hL x hx) (hL y hy) (chainOK_excl fire L false h x hx y hy hne)

/-- A semantic pair, from its `Comm` lemma. -/
theorem groupComm_pair {α : Type} {Inv : α → Prop} {step : Nat → α → α} {p q : Nat}
    (h : Comm Inv step p q) : GroupComm Inv step [p, q] := by
  intro x hx y hy hne
  simp only [List.mem_cons, List.mem_nil_iff, or_false] at hx hy
  rcases hx with rfl | rfl <;> rcases hy with rfl | rfl
  · exact absurd rfl hne
  · exact h
  · exact h.symm
  · exact absurd rfl hne

/-- Never-both-fire between methods, group by group (so a design states it in
    size linear in the methods, never pair by pair).  `ng b` is method `b`'s group
    (0: none), `gmeth g` the methods of group `g` (the `nba` list of each of
    them), and `gsteps g` the steps of those methods in the order of the group's
    chain-form disjointness fact (`hc`, one per group).  Membership is checked
    per method (`hm`) and per step (`hs`), linearly. -/
theorem nba_of_groups {n G : Nat} {meth : Nat → Nat} {fire : Nat → Bool} {ng : Nat → Nat}
    {gmeth gsteps : Nat → List Nat}
    (hm : ∀ g, g < G → ∀ a ∈ gmeth g, ng a = g) (h0 : gmeth 0 = []) (hG : ∀ g, G ≤ g → gmeth g = [])
    (hs : ∀ x, x < n → meth x ≠ 0 → ng (meth x - 1) ≠ 0 → x ∈ gsteps (ng (meth x - 1)))
    (hc : ∀ g, g ≠ 0 → g < G → chainOK fire (gsteps g) false = true) :
    ∀ a b x y, a ∈ gmeth (ng b) → a ≠ b → x < n → y < n → meth x = a + 1 → meth y = b + 1 →
      fire x = true → fire y = true → False := by
  intro a b x y ha hne hx hy hmx hmy hfx hfy
  have hg0 : ng b ≠ 0 := fun e => by rw [e, h0] at ha; cases ha
  have hgG : ng b < G := Nat.lt_of_not_le (fun e => by rw [hG _ e] at ha; cases ha)
  have hna := hm _ hgG a ha
  have hxs := hs x hx (by omega) (by rw [hmx, Nat.add_sub_cancel, hna]; exact hg0)
  have hys := hs y hy (by omega) (by rw [hmy, Nat.add_sub_cancel]; exact hg0)
  rw [hmx, Nat.add_sub_cancel, hna] at hxs
  rw [hmy, Nat.add_sub_cancel] at hys
  have hxy : x ≠ y := fun e => by rw [e, hmy] at hmx; exact hne (by omega)
  have := chainOK_excl fire _ false (hc _ hg0 hgG) x hxs y hys hxy
  rw [hfx, hfy] at this; cases this

/-! ### Per-step facts, linearly

A fact about every step below `n` is proved from one lemma per step, combined
by a balanced tree of `range_split` (O(n) nodes, each checked in constant
time; no `match` on numerals). -/

theorem range_split {Q : Nat → Prop} (lo mid hi : Nat) (h₁ : ∀ k, lo ≤ k → k < mid → Q k)
    (h₂ : ∀ k, mid ≤ k → k < hi → Q k) : ∀ k, lo ≤ k → k < hi → Q k := by
  intro k h1 h2
  by_cases h : k < mid
  · exact h₁ k h1 h
  · exact h₂ k (by omega) h2

theorem range_one {Q : Nat → Prop} (c : Nat) (h : Q c) : ∀ k, c ≤ k → k < c + 1 → Q k := by
  intro k h1 h2
  have : k = c := by omega
  subst this; exact h

theorem range_all {Q : Nat → Prop} {n : Nat} (h : ∀ k, 0 ≤ k → k < n → Q k) : ∀ k, k < n → Q k :=
  fun k hk => h k (Nat.zero_le _) hk

/-- Two steps that agree on every field either sets or flags commute. -/
theorem Eff.comm_on (x y : Eff V) (s : St V) (D : List Nat)
    (hD : (x.W ++ x.O ++ y.W ++ y.O).all (fun k => D.contains k) = true)
    (h : ∀ k ∈ D, x.apply (y.apply s) k = y.apply (x.apply s) k) :
    x.apply (y.apply s) = y.apply (x.apply s) := by
  funext k
  cases hk : D.contains k
  · have hD' := List.all_eq_true.mp hD
    have off : ∀ l : List Nat, (∀ w ∈ l, D.contains w = true) → l.contains k = false := fun l hl => by
      cases e : l.contains k
      · rfl
      · have := hl k (List.contains_iff_mem.mp e); rw [hk] at this; cases this
    have hxw := off x.W (fun w hw => hD' w (by simp [hw]))
    have hxo := off x.O (fun w hw => hD' w (by simp [hw]))
    have hyw := off y.W (fun w hw => hD' w (by simp [hw]))
    have hyo := off y.O (fun w hw => hD' w (by simp [hw]))
    rw [Eff.apply_other x _ k hxw hxo, Eff.apply_other y _ k hyw hyo,
      Eff.apply_other y _ k hyw hyo, Eff.apply_other x _ k hxw hxo]
  · exact h k (List.contains_iff_mem.mp hk)

theorem lookup_zip_some {α : Type} (l : List Nat) (vs : List α) (k : Nat) (v : α)
    (h : (l.zip vs).lookup k = some v) : (k, v) ∈ l.zip vs := by
  induction l generalizing vs with
  | nil => cases h
  | cons a l ih =>
    cases vs with
    | nil => cases h
    | cons w vs =>
      simp only [List.zip_cons_cons, List.lookup_cons] at h ⊢
      cases e : k == a
      · rw [e] at h; exact List.mem_cons_of_mem _ (ih vs h)
      · rw [e] at h; cases h; rw [beq_iff_eq.mp e]; exact List.mem_cons_self ..

/-- A step keeps a field-wise property `ok` (e.g. "holds a value of the field's
    sort") if every value it sets has it and every flag field takes Booleans. -/
theorem Eff.apply_ok (ok : Nat → V → Prop) (e : Eff V)
    (hW : ∀ rs, ∀ p ∈ e.W.zip (e.U rs), ok p.1 p.2) (hO : ∀ k ∈ e.O, ∀ b, ok k (FlagVal.ofBool b))
    (s : St V) (hs : ∀ k, ok k (s k)) : ∀ k, ok k (e.apply s k) := by
  intro k
  show ok k (e.val (e.R.map s) (s k) k)
  unfold Eff.val
  cases hO' : (e.O.zip (e.C (e.R.map s))).lookup k with
  | some c =>
    exact hO k (List.contains_iff_mem.mp (lookup_zip_mem _ _ _ _ hO')) _
  | none =>
    cases hW' : (e.W.zip (e.U (e.R.map s))).lookup k with
    | some v => exact hW _ (k, v) (lookup_zip_some _ _ _ _ hW')
    | none => exact hs k

/-- A step that sets every field it sets to the value it had, sets no flag, and
    finds every flag field holding a Boolean, leaves the state as it is. -/
theorem Eff.apply_eq_self (e : Eff V) (s : St V)
    (hW : ∀ p ∈ e.W.zip (e.U (e.R.map s)), p.2 = s p.1)
    (hO : ∀ p ∈ e.O.zip (e.C (e.R.map s)), p.2 = false ∧ FlagVal.ofBool (FlagVal.toBool (s p.1)) = s p.1) :
    e.apply s = s := by
  funext k
  show e.val (e.R.map s) (s k) k = s k
  unfold Eff.val
  have hv1 : (match (e.W.zip (e.U (e.R.map s))).lookup k with | some v => v | none => s k) = s k := by
    cases hW' : (e.W.zip (e.U (e.R.map s))).lookup k with
    | some v => exact hW (k, v) (lookup_zip_some _ _ _ _ hW')
    | none => rfl
  rw [hv1]
  cases hO' : (e.O.zip (e.C (e.R.map s))).lookup k with
  | some c =>
    obtain ⟨hc, hb⟩ := hO (k, c) (lookup_zip_some _ _ _ _ hO')
    change c = false at hc
    change FlagVal.ofBool (FlagVal.toBool (s k)) = s k at hb
    show FlagVal.ofBool (FlagVal.toBool (s k) || c) = s k
    rw [hc, Bool.or_false]; exact hb
  | none => rfl

theorem mem_zip_of_map_eq {α β γ : Type} (f : β → γ) (g : α → γ) :
    ∀ (ws : List α) (us : List β), us.map f = ws.map g → ∀ p ∈ ws.zip us, f p.2 = g p.1
  | [], _, _, p, hp => by cases hp
  | _ :: _, [], h, _, _ => by cases h
  | w :: ws, u :: us, h, p, hp => by
    simp only [List.map_cons, List.cons.injEq] at h
    simp only [List.zip_cons_cons, List.mem_cons] at hp
    rcases hp with rfl | hp
    · exact h.1
    · exact mem_zip_of_map_eq f g ws us h.2 p hp

/-- The uniform proof that a step keeps every field of its sort: the sorts of
    the values it sets are, whatever it reads, those of the fields it sets
    (`fun rs => rfl` for an emitted step), and its flag fields are Boolean. -/
theorem Eff.keeps_sorts (sortOf : V → Nat) (fsort : Nat → Nat) (e : Eff V) (hU : ∀ rs, (e.U rs).map sortOf = e.W.map fsort)
    (hO : ∀ k ∈ e.O, ∀ b, sortOf (FlagVal.ofBool b) = fsort k)
    (s : St V) (hs : ∀ k, sortOf (s k) = fsort k) : ∀ k, sortOf (e.apply s k) = fsort k :=
  Eff.apply_ok (fun k v => sortOf v = fsort k) e
    (fun rs p hp => mem_zip_of_map_eq sortOf fsort e.W (e.U rs) (hU rs) p hp) hO s hs

/-- A step that, when it does not fire, still sets some fields only it uses
    (its ready outputs) to values that do not depend on the state, commutes
    with every step then. -/
theorem comm_of_idle_outputs (Inv : St V → Prop) (E : Nat → Eff V) (hk : Keeps Inv (stepOf E))
    (x y : Nat) (I : Eff V) (hR : I.R = []) (hO : I.O = [])
    (hsim : ∀ s, Inv s → stepOf E x s = I.apply s)
    (hown : x ≠ y → ∀ f ∈ I.W, f ∉ uses (E y).fp) : Comm Inv (stepOf E) x y := by
  by_cases hxy : x = y
  · subst hxy; intro s _; rfl
  apply comm_of_sim Inv E hk x y I hsim
  apply indep_of_compat
  intro f u v hu hv
  have hfy := mem_uses_of_useOf hu
  have hfI := mem_uses_of_useOf hv
  simp only [uses, Eff.fp, hR, hO, List.append_nil, List.nil_append] at hfI
  exact absurd hfy (hown hxy f hfI)

end Gen
