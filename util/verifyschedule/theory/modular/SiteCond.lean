/- SiteCond.lean: unconditional sites, weakened conditions, dropping later-site literals for
   `SitesOK`, and weakening against the obligation (`oblU_weakens`), over `Modular`'s site lists.
   A file's conditions need no soundness lemma: its obligation is checked for them, at every
   skipped value with the order fixed first (`OblU`).  Core Lean only. -/
import Modular

set_option autoImplicit false
open scoped List

namespace SiteCond
open Modular

variable {V : Type} {I : Type} {CM : I → Type}

/-! ## 1. Unconditional read sites -/

/-- A site taken whenever its step runs (the default for a value-method read). -/
def Site.always (j : I) (m : CM j) (arg : (Nat → V) → V) : Site I CM V := ⟨j, m, fun _ => true, arg⟩

/-- An unconditional site reads before `n` exactly when its argument does. -/
theorem readsBefore_always (n : Nat) (j : I) (m : CM j) (arg : (Nat → V) → V) :
    ReadsBefore n (Site.always j m arg) ↔
      ∀ ρ ρ' : Nat → V, (∀ i, i < n → ρ i = ρ' i) → arg ρ = arg ρ' := by
  unfold ReadsBefore Site.always
  simp

/-- An always-taken site returns the child's output, whatever the skipped values `u`. -/
theorem envHw_at_always (u : Nat → V) (oc : (j : I) → Outs (CM j) V) (ss : List (Site I CM V))
    (n : Nat) (ρ : Nat → V) (h : SitesOK ss n) (i : Nat) (hi : i < ss.length)
    (ht : ∀ ρ, ss[i].cond ρ = true) :
    envHw u oc ss n ρ (n + i) = (oc ss[i].j).ret ss[i].m := by
  rw [envHw_at u oc ss n ρ h i hi, ht]
  rfl

/-- A taken site's call is in the step's calls (for an always-taken site, in every run). -/
theorem mem_callsOf (ρ : Nat → V) : ∀ (ss : List (Site I CM V)) (s : Site I CM V), s ∈ ss →
    s.cond ρ = true → (⟨s.j, (s.m, s.arg ρ)⟩ : Σ j, CM j × V) ∈ callsOf ρ ss
  | [], _, h, _ => by simp at h
  | s0 :: ss, s, h, hc => by
    simp only [callsOf, List.mem_append]
    rcases List.mem_cons.mp h with e | h
    · subst e; left; simp [hc]
    · right; exact mem_callsOf ρ ss s h hc

/-- Where a method's argument lives: sites are numbered from 1 and `ρ 0` is the argument
    (a tuple for several), so every site may read it. -/
theorem envHw_arg (u : Nat → V) (oc : (j : I) → Outs (CM j) V) (ss : List (Site I CM V))
    (ρ0 : Nat → V) (x : V) : envHw u oc ss 1 (setN ρ0 0 x) 0 = x := by
  rw [envHw_below u oc ss 1 _ 0 (by omega)]
  simp [setN]

/-! ## 2. Weakened conditions: runs of the exact step -/

/-- `Weakens ss ss'`: `ss'` is `ss` with conditions weakened (same ports and arguments; each
    condition of `ss'` holds wherever the matching one of `ss` holds). -/
inductive Weakens : List (Site I CM V) → List (Site I CM V) → Prop
  | nil : Weakens [] []
  | cons (s : Site I CM V) (c : (Nat → V) → Bool) {ss ss' : List (Site I CM V)} :
      (∀ ρ, s.cond ρ = true → c ρ = true) → Weakens ss ss' →
      Weakens (s :: ss) ({ s with cond := c } :: ss')

theorem Weakens.length_eq {ss ss' : List (Site I CM V)} (h : Weakens ss ss') :
    ss'.length = ss.length := by
  induction h with
  | nil => rfl
  | cons _ _ _ _ ih => simp [ih]

theorem Weakens.refl : ∀ (ss : List (Site I CM V)), Weakens ss ss
  | [] => .nil
  | s :: ss => by
    have := Weakens.cons s s.cond (fun _ h => h) (Weakens.refl ss)
    exact this

/-- At one environment, the weakened sites make every call the exact ones make (and more). -/
theorem callsOf_weakens (ρ : Nat → V) {ss ss' : List (Site I CM V)} (h : Weakens ss ss') :
    callsOf ρ ss <+ callsOf ρ ss' := by
  induction h with
  | nil => exact List.Sublist.refl _
  | cons s c hc _ ih =>
    simp only [callsOf]
    cases e : s.cond ρ
    · simp only [Bool.false_eq_true, ↓reduceIte, List.nil_append]
      exact ih.trans (List.sublist_append_right _ _)
    · have : c ρ = true := hc ρ e
      simp only [this, ↓reduceIte, List.singleton_append]
      exact ih.cons_cons _

theorem envHw_above (u : Nat → V) (oc : (j : I) → Outs (CM j) V) :
    ∀ (ss : List (Site I CM V)) (n : Nat) (ρ : Nat → V) (k : Nat), n + ss.length ≤ k →
      envHw u oc ss n ρ k = ρ k
  | [], _, _, _, _ => rfl
  | s :: ss, n, ρ, k, h => by
    simp only [envHw]
    rw [envHw_above u oc ss (n + 1) _ k (by simp at h; omega)]
    simp [setN, show k ≠ n by simp at h; omega]

/-- The i-th site of a weakened list is the i-th exact site with a weaker condition. -/
theorem Weakens.getElem {ss ss' : List (Site I CM V)} (h : Weakens ss ss') :
    ∀ (i : Nat) (hi : i < ss.length), ∃ c : (Nat → V) → Bool,
      ss'[i]'(by rw [h.length_eq]; exact hi) = { ss[i] with cond := c } ∧
      ∀ ρ, ss[i].cond ρ = true → c ρ = true := by
  induction h with
  | nil => intro i hi; simp at hi
  | cons s c hc hw ih =>
    intro i hi
    cases i with
    | zero => exact ⟨c, rfl, hc⟩
    | succ i =>
      obtain ⟨c', e, h'⟩ := ih i (by simp at hi; omega)
      exact ⟨c', by simpa using e, by simpa using h'⟩

/-- In the weakened environment every site whose exact condition holds returns the child's
    output (exact conditions possibly cyclic). -/
theorem weakens_env_exact (u : Nat → V) (oc : (j : I) → Outs (CM j) V)
    {ss ss' : List (Site I CM V)} (hw : Weakens ss ss') (n : Nat) (ρ : Nat → V)
    (h' : SitesOK ss' n) (i : Nat) (hi : i < ss.length)
    (hc : ss[i].cond (envHw u oc ss' n ρ) = true) :
    envHw u oc ss' n ρ (n + i) = (oc ss[i].j).ret ss[i].m := by
  obtain ⟨c, e, hcw⟩ := hw.getElem i hi
  have hi' : i < ss'.length := by rw [hw.length_eq]; exact hi
  rw [envHw_at u oc ss' n ρ h' i hi', e]
  simp only [hcw _ hc, ↓reduceIte]

/-- With `SitesOK` for both, the environment of an `SitesOK` list is the unique solution
    of its site equations. -/
theorem envHw_unique (u : Nat → V) (oc : (j : I) → Outs (CM j) V) :
    ∀ (ss : List (Site I CM V)) (n : Nat) (ρ E : Nat → V), SitesOK ss n →
      (∀ k, k < n → E k = ρ k) → (∀ k, n + ss.length ≤ k → E k = ρ k) →
      (∀ (i : Nat) (h : i < ss.length),
        E (n + i) = if ss[i].cond E then (oc ss[i].j).ret ss[i].m else u (n + i)) →
      envHw u oc ss n ρ = E
  | [], n, ρ, E, _, hlo, hhi, _ => by
    funext k
    simp only [envHw]
    by_cases hk : k < n
    · exact (hlo k hk).symm
    · exact (hhi k (by simp; omega)).symm
  | s :: ss, n, ρ, E, ⟨hs, hss⟩, hlo, hhi, hat => by
    simp only [envHw]
    apply envHw_unique u oc ss (n + 1) _ E hss
    · intro k hk
      by_cases hkn : k = n
      · subst hkn
        have h0 := hat 0 (by simp)
        simp only [Nat.add_zero, List.getElem_cons_zero] at h0
        have hcE : s.cond ρ = s.cond E := (hs ρ E (fun i hi => (hlo i hi).symm)).1
        rw [h0]
        simp [setN, hcE]
      · simp only [setN, hkn, ↓reduceIte]
        exact hlo k (by omega)
    · intro k hk
      simp only [List.length_cons] at hhi
      simp only [setN, show k ≠ n by omega, ↓reduceIte]
      exact hhi k (by omega)
    · intro i hi
      have := hat (i + 1) (by simp; omega)
      rw [show n + (i + 1) = n + 1 + i by omega] at this
      simpa using this

/-- **Every run of the weakened step is a run of the exact step**, at the skipped values the
    weakened environment itself supplies: the same environment, hence the same result. -/
theorem envHw_weakens (u : Nat → V) (oc : (j : I) → Outs (CM j) V)
    {ss ss' : List (Site I CM V)} (hw : Weakens ss ss') (n : Nat) (ρ : Nat → V)
    (h : SitesOK ss n) (h' : SitesOK ss' n) :
    envHw (envHw u oc ss' n ρ) oc ss n ρ = envHw u oc ss' n ρ := by
  apply envHw_unique _ oc ss n ρ _ h
  · intro k hk; exact envHw_below u oc ss' n ρ k hk
  · intro k hk; exact envHw_above u oc ss' n ρ k (by rw [hw.length_eq]; exact hk)
  · intro i hi
    cases e : ss[i].cond (envHw u oc ss' n ρ)
    · simp
    · simp only [↓reduceIte]
      exact weakens_env_exact u oc hw n ρ h' i hi e

/-- **The refinement, on traces**: the weakened step returns what the exact step returns at
    some skipped values, and its calls contain the exact step's calls. -/
theorem trace_weakens (u : Nat → V) (oc : (j : I) → Outs (CM j) V) (res : (Nat → V) → Option V)
    {ss ss' : List (Site I CM V)} (hw : Weakens ss ss') (n : Nat) (ρ : Nat → V)
    (h : SitesOK ss n) (h' : SitesOK ss' n) :
    ∃ u' : Nat → V,
      (trace oc (runSites u' res ss n ρ)).2 = (trace oc (runSites u res ss' n ρ)).2 ∧
      (trace oc (runSites u' res ss n ρ)).1 <+ (trace oc (runSites u res ss' n ρ)).1 := by
  refine ⟨envHw u oc ss' n ρ, ?_, ?_⟩
  · rw [trace_runSites _ oc res ss n ρ h, trace_runSites u oc res ss' n ρ h',
      envHw_weakens u oc hw n ρ h h']
  · rw [trace_runSites _ oc res ss n ρ h, trace_runSites u oc res ss' n ρ h',
      envHw_weakens u oc hw n ρ h h']
    exact callsOf_weakens _ hw

/-- Each caller's calls a sublist of the matching caller's. -/
inductive Sub2 {α : Type} : List (List α) → List (List α) → Prop
  | nil : Sub2 [] []
  | cons {a b : List α} {as bs : List (List α)} : a <+ b → Sub2 as bs → Sub2 (a :: as) (b :: bs)

theorem flatten_sublist_of_sub2 {α : Type} :
    ∀ {σ σ' : List (List α)}, Sub2 σ σ' → σ.flatten <+ σ'.flatten
  | [], [], .nil => List.Sublist.refl _
  | _ :: _, _ :: _, .cons h hs => by
    simp only [List.flatten_cons]
    exact h.append (flatten_sublist_of_sub2 hs)

theorem exists_sup_of_sub2 {α : Type} :
    ∀ {σ σ' : List (List α)}, Sub2 σ σ' → ∀ g ∈ σ, ∃ g' ∈ σ', g <+ g'
  | [], [], .nil, _, h => by simp at h
  | a :: _, b :: _, .cons hab hs, g, h => by
    rcases List.mem_cons.mp h with e | h
    · subst e; exact ⟨b, List.mem_cons_self .., hab⟩
    · obtain ⟨g', hg', hs'⟩ := exists_sup_of_sub2 hs g h
      exact ⟨g', List.mem_cons_of_mem _ hg', hs'⟩

/-- **Legality and readiness survive dropping calls**: each caller's calls a sublist of the
    weakened ones, and no always-enabled call dropped (weakened sites are value reads). -/
theorem legal_of_sublists {M : Type} (A : Ann M) {σ σ' : List (List (M × V))} (h : Legal A σ')
    (hs : Sub2 σ σ')
    (hal : ∀ c ∈ σ'.flatten, A.alwaysEn c.1 = true → c ∈ σ.flatten) : Legal A σ where
  order c1 c2 h12 := h.order c1 c2 (h12.trans (flatten_sublist_of_sub2 hs))
  group g hg c1 c2 h12 := by
    obtain ⟨g', hg', hsub⟩ := exists_sup_of_sub2 hs g hg
    exact h.group g' hg' c1 c2 (h12.trans hsub)
  always m hm := by
    obtain ⟨x, hx⟩ := h.always m hm
    exact ⟨x, hal (m, x) hx hm⟩

theorem ready_of_sublists {M : Type} (A : Ann M) (rdy : M → Bool) {σ σ' : List (List (M × V))}
    (hs : Sub2 σ σ')
    (h : ∀ c ∈ σ'.flatten, A.alwaysRdy c.1 = false → rdy c.1 = true) :
    ∀ c ∈ σ.flatten, A.alwaysRdy c.1 = false → rdy c.1 = true :=
  fun c hc => h c ((flatten_sublist_of_sub2 hs).subset hc)

/-! ## 3. Dropping later-site select literals gives `SitesOK` -/

/-- A select literal: a Boolean of the read environment that reads only positions `rd`. -/
structure Lit (V : Type) where
  rd : List Nat
  f : (Nat → V) → Bool
  loc : ∀ ρ ρ' : Nat → V, (∀ i ∈ rd, ρ i = ρ' i) → f ρ = f ρ'

/-- A reach condition: the OR over paths of the AND of each path's select literals
    (`[[]]` is `true`, an unconditional site; `[]` is `false`). -/
abbrev Cond (V : Type) := List (List (Lit V))

def Cond.eval (c : Cond V) (ρ : Nat → V) : Bool := c.any (fun p => p.all (fun l => l.f ρ))

/-- The literal reads only positions before `n`. -/
def Lit.before (n : Nat) (l : Lit V) : Bool := l.rd.all (fun i => decide (i < n))

/-- Drop every literal that reads a position `n` or later. -/
def Cond.dropFrom (n : Nat) (c : Cond V) : Cond V := c.map (fun p => p.filter (Lit.before n))

theorem Cond.eval_dropFrom (n : Nat) (c : Cond V) (ρ : Nat → V) (h : c.eval ρ = true) :
    (c.dropFrom n).eval ρ = true := by
  unfold Cond.eval Cond.dropFrom at *
  obtain ⟨p, hp, hall⟩ := List.any_eq_true.mp h
  refine List.any_eq_true.mpr ⟨p.filter (Lit.before n), List.mem_map_of_mem hp, ?_⟩
  rw [List.all_eq_true] at hall ⊢
  intro l hl
  exact hall l (List.mem_filter.mp hl).1

theorem all_congr' {α : Type} (l : List α) (f g : α → Bool) (h : ∀ a ∈ l, f a = g a) :
    l.all f = l.all g := by
  induction l with
  | nil => rfl
  | cons a l ih => simp only [List.all_cons, h a (by simp), ih (fun b hb => h b (by simp [hb]))]

theorem any_congr' {α : Type} (l : List α) (f g : α → Bool) (h : ∀ a ∈ l, f a = g a) :
    l.any f = l.any g := by
  induction l with
  | nil => rfl
  | cons a l ih => simp only [List.any_cons, h a (by simp), ih (fun b hb => h b (by simp [hb]))]

theorem Cond.dropFrom_local (n : Nat) (c : Cond V) (ρ ρ' : Nat → V)
    (h : ∀ i, i < n → ρ i = ρ' i) : (c.dropFrom n).eval ρ = (c.dropFrom n).eval ρ' := by
  unfold Cond.eval Cond.dropFrom
  apply any_congr'
  intro p hp
  obtain ⟨q, _, rfl⟩ := List.mem_map.mp hp
  apply all_congr'
  intro l hl
  have hb := (List.mem_filter.mp hl).2
  apply l.loc
  intro i hi
  unfold Lit.before at hb
  exact h i (by simpa using (List.all_eq_true.mp hb) i hi)

/-- A condition whose literals all read before `n` is not changed (an action call's exact
    condition, in an order that puts its reads first). -/
theorem Cond.dropFrom_of_before (n : Nat) (c : Cond V)
    (h : ∀ p ∈ c, ∀ l ∈ p, l.before n = true) : c.dropFrom n = c := by
  unfold Cond.dropFrom
  induction c with
  | nil => rfl
  | cons p c ih =>
    simp only [List.map_cons]
    rw [List.filter_eq_self.mpr (h p (by simp)), ih (fun q hq => h q (by simp [hq]))]

/-- A site with a syntactic condition. -/
structure CSite (I : Type) (CM : I → Type) (V : Type) where
  j : I
  m : CM j
  cond : Cond V
  arg : (Nat → V) → V

/-- The site under its exact condition. -/
def CSite.exact (s : CSite I CM V) : Site I CM V := ⟨s.j, s.m, s.cond.eval, s.arg⟩

/-- The site at position `n`, its later-site literals dropped. -/
def CSite.at (n : Nat) (s : CSite I CM V) : Site I CM V := ⟨s.j, s.m, (s.cond.dropFrom n).eval, s.arg⟩

/-- A list in a fixed order, each site's literals reading a later site dropped. -/
def dropLater : List (CSite I CM V) → Nat → List (Site I CM V)
  | [], _ => []
  | s :: ss, n => s.at n :: dropLater ss (n + 1)

/-- The order respects data dependence: each argument reads only earlier sites. -/
def ArgsBefore : List (CSite I CM V) → Nat → Prop
  | [], _ => True
  | s :: ss, n => (∀ ρ ρ' : Nat → V, (∀ i, i < n → ρ i = ρ' i) → s.arg ρ = s.arg ρ') ∧ ArgsBefore ss (n + 1)

/-- **Dropping gives `SitesOK`**, for any order whose arguments read only earlier sites. -/
theorem sitesOK_dropLater : ∀ (ss : List (CSite I CM V)) (n : Nat), ArgsBefore ss n →
    SitesOK (dropLater ss n) n
  | [], _, _ => trivial
  | s :: ss, n, ⟨ha, hss⟩ =>
    ⟨fun ρ ρ' h => ⟨Cond.dropFrom_local n s.cond ρ ρ' h, ha ρ ρ' h⟩, sitesOK_dropLater ss (n + 1) hss⟩

/-- **Dropping only weakens.** -/
theorem weakens_dropLater : ∀ (ss : List (CSite I CM V)) (n : Nat),
    Weakens (ss.map CSite.exact) (dropLater ss n)
  | [], _ => .nil
  | s :: ss, n =>
    Weakens.cons (CSite.exact s) ((s.cond.dropFrom n).eval)
      (fun ρ h => Cond.eval_dropFrom n s.cond ρ h) (weakens_dropLater ss (n + 1))

/-- An action site whose exact condition reads only earlier sites keeps it. -/
theorem at_eq_exact (n : Nat) (s : CSite I CM V) (h : ∀ p ∈ s.cond, ∀ l ∈ p, l.before n = true) :
    s.at n = s.exact := by
  unfold CSite.at CSite.exact
  rw [Cond.dropFrom_of_before n s.cond h]

/-- An unconditional read in the syntactic form (`cond := [[]]`) is `Site.always` at every
    position: no literal to drop. -/
theorem uncond_at (n : Nat) (j : I) (m : CM j) (arg : (Nat → V) → V) :
    (CSite.mk j m [[]] arg).at n = Site.always j m arg := rfl

/-- For any order respecting the arguments: `SitesOK`, exact sites return the child's output,
    and the calls contain the exact calls (step level; `oblU_weakens` lifts it). -/
theorem dropLater_spec (u : Nat → V) (oc : (j : I) → Outs (CM j) V) (ss : List (CSite I CM V))
    (n : Nat) (ρ : Nat → V) (h : ArgsBefore ss n) :
    SitesOK (dropLater ss n) n ∧
    (∀ (i : Nat) (hi : i < ss.length),
      ss[i].cond.eval (envHw u oc (dropLater ss n) n ρ) = true →
      envHw u oc (dropLater ss n) n ρ (n + i) = (oc ss[i].j).ret ss[i].m) ∧
    callsOf (envHw u oc (dropLater ss n) n ρ) (ss.map CSite.exact) <+
      callsOf (envHw u oc (dropLater ss n) n ρ) (dropLater ss n) := by
  have hok := sitesOK_dropLater ss n h
  have hw := weakens_dropLater ss n
  refine ⟨hok, ?_, callsOf_weakens _ hw⟩
  intro i hi hc
  have hi' : i < (ss.map CSite.exact).length := by simpa using hi
  have e : (ss.map CSite.exact)[i] = (ss[i]).exact := List.getElem_map ..
  have := weakens_env_exact u oc hw n ρ hok i hi' (by rw [e]; exact hc)
  rw [e] at this
  exact this

/-! Decision 1's cycle: `reach A = C ∨ (D ∧ B)`, `reach B = (C ∧ A) ∨ D`.  In the order
`C D A B`, `A` loses the literal `B`; `B` keeps `A`. -/

def litAt (k : Nat) : Lit Bool := ⟨[k], fun ρ => ρ k, fun ρ ρ' h => h k (by simp)⟩

def condA : Cond Bool := [[litAt 0], [litAt 1, litAt 3]]
def condB : Cond Bool := [[litAt 0, litAt 2], [litAt 1]]

example : (condA.dropFrom 2).map (fun p => p.map Lit.rd) = [[[0]], [[1]]] := by decide
example : (condB.dropFrom 3).map (fun p => p.map Lit.rd) = [[[0], [2]], [[1]]] := by decide

/-! ## 4. Weakening against the obligation

`oblU_weakens`: if a family meets its obligation with some conditions, then with weaker ones
(for example the always-read steps) every step at every skipped-value assignment returns the
same result and makes the same calls plus extra ones, at the same order.  So weakening adds
only obligations on the extra calls (their legality and readiness): it costs completeness. -/

/-- `envHw` reads `ρ` only below `n` and from `n + ss.length` on. -/
theorem envHw_congr_rho (u : Nat → V) (oc : (j : I) → Outs (CM j) V) (ss : List (Site I CM V)) (n : Nat)
    (ρ ρ' : Nat → V) (h : SitesOK ss n) (hlo : ∀ k, k < n → ρ k = ρ' k)
    (hhi : ∀ k, n + ss.length ≤ k → ρ k = ρ' k) : envHw u oc ss n ρ = envHw u oc ss n ρ' := by
  apply envHw_unique u oc ss n ρ _ h
  · intro k hk; rw [envHw_below u oc ss n ρ' k hk]; exact (hlo k hk).symm
  · intro k hk; rw [envHw_above u oc ss n ρ' k hk]; exact (hhi k hk).symm
  · intro i hi; exact envHw_at u oc ss n ρ' h i hi

/-- **A weakened step at `u` is the exact step at the weakened environment**: the same
    result, and its calls contain the exact step's (`ρE`: the exact step's start). -/
theorem trace_weakens_at (u : Nat → V) (oc : (j : I) → Outs (CM j) V) (res : (Nat → V) → Option V)
    {ss ss' : List (Site I CM V)} (hw : Weakens ss ss') (n : Nat) (ρ ρE : Nat → V)
    (h : SitesOK ss n) (h' : SitesOK ss' n) (hlo : ∀ k, k < n → ρE k = ρ k)
    (hhi : ∀ k, n + ss.length ≤ k → ρE k = envHw u oc ss' n ρ k) :
    (trace oc (runSites (envHw u oc ss' n ρ) res ss n ρE)).2 = (trace oc (runSites u res ss' n ρ)).2 ∧
    (trace oc (runSites (envHw u oc ss' n ρ) res ss n ρE)).1 <+ (trace oc (runSites u res ss' n ρ)).1 := by
  have hE : envHw (envHw u oc ss' n ρ) oc ss n ρE = envHw u oc ss' n ρ := by
    rw [envHw_congr_rho _ oc ss n ρE ρ h hlo (fun k hk => by
      rw [hhi k hk, envHw_above u oc ss' n ρ k (by rw [hw.length_eq]; exact hk)])]
    exact envHw_weakens u oc hw n ρ h h'
  rw [trace_runSites _ oc res ss n ρE h, trace_runSites u oc res ss' n ρ h', hE]
  exact ⟨rfl, callsOf_weakens _ hw⟩

theorem proj_sublist {J : Type} [DecidableEq J] {CJ : J → Type} (j : J) :
    ∀ {l1 l2 : List (Σ j, CJ j × V)}, l1 <+ l2 → proj j l1 <+ proj j l2
  | _, _, .slnil => .slnil
  | _, _, .cons a h => by
    rcases a with ⟨j', c⟩
    by_cases hj : j' = j
    · subst hj; rw [proj_cons_self]; exact (proj_sublist j' h).cons _
    · rw [proj_cons_ne _ _ hj]; exact proj_sublist j h
  | _, _, .cons₂ a h => by
    rcases a with ⟨j', c⟩
    by_cases hj : j' = j
    · subst hj; rw [proj_cons_self, proj_cons_self]; exact (proj_sublist j' h).cons₂ _
    · rw [proj_cons_ne _ _ hj, proj_cons_ne _ _ hj]; exact proj_sublist j h

theorem flatMap_sublist {α β : Type} (f g : α → List β) :
    ∀ (l : List α), (∀ a ∈ l, f a <+ g a) → l.flatMap f <+ l.flatMap g
  | [], _ => .slnil
  | a :: l, h => by
    simp only [List.flatMap_cons]
    exact (h a (List.mem_cons_self ..)).append (flatMap_sublist f g l (fun b hb => h b (List.mem_cons_of_mem _ hb)))

theorem sub2_map {α β : Type} (f g : α → List β) :
    ∀ (l : List α), (∀ a ∈ l, f a <+ g a) → Sub2 (l.map f) (l.map g)
  | [], _ => .nil
  | a :: l, h => .cons (h a (List.mem_cons_self ..)) (sub2_map f g l (fun b hb => h b (List.mem_cons_of_mem _ hb)))

theorem mem_groups_of_inl {α β : Type} {g : α} : ∀ {τ : List (α ⊕ β)}, .inl g ∈ τ → g ∈ groups τ
  | [], h => by simp at h
  | .inl a :: τ, h => by
    rcases List.mem_cons.mp h with e | h
    · cases e; simp [groups]
    · simp only [groups]; exact List.mem_cons_of_mem _ (mem_groups_of_inl h)
  | .inr b :: τ, h => by
    rcases List.mem_cons.mp h with e | h
    · cases e
    · simp only [groups]; exact mem_groups_of_inl h

/-- **Weakening against the obligation.**  If the family with conditions `ssM`, `ssR` meets
    its obligation, then with weaker conditions, at the same order and at every skipped-value
    assignment `u'`, each step returns the same result and the children's calls contain the
    fixed `σC` caller by caller (`Sub2`): only the extra calls' obligations remain. -/
theorem oblU_weakens (F : ParentU V) (ssM ssM' : F.X.M → List (Site F.I (fun j => (F.CI j).M) V))
    (resM : F.X.M → (Nat → V) → Option V) (ssR ssR' : F.PR → List (Site F.I (fun j => (F.CI j).M) V))
    (resR : F.PR → (Nat → V) → Option V)
    (hwM : ∀ m, Weakens (ssM m) (ssM' m)) (hwR : ∀ r, Weakens (ssR r) (ssR' r))
    (hM : ∀ m, SitesOK (ssM m) 1) (hM' : ∀ m, SitesOK (ssM' m) 1)
    (hR : ∀ r, SitesOK (ssR r) 0) (hR' : ∀ r, SitesOK (ssR' r) 0)
    (hself : ∀ m, F.X.ann.conf m m = true ∨ F.X.ann.share m = true)
    (h : OblU (F.withSites ssM resM ssR resR)) :
    ∀ (oc : (j : F.I) → Outs (F.CI j).M V) (w : Wires F.X.M V) (σ : List (List (F.X.M × V))),
      F.asm oc w → (∀ j, AliasOK F.toB (F.CI j).ann (oc j)) → Legal F.X.ann σ → Drives F.X.ann w σ →
      (∀ c ∈ σ.flatten, F.X.ann.alwaysRdy c.1 = false → (F.comb oc w).2.rdy c.1 = true) →
      F.onehot oc w ∧ ∃ (τP : List (List (F.X.M × V) ⊕ F.PR)) (σC : (j : F.I) → List (List ((F.CI j).M × V))),
        groups τP = σ ∧
        (∀ j, Legal (F.CI j).ann (σC j) ∧ Drives (F.CI j).ann ((F.comb oc w).1 j) (σC j) ∧
          ∀ c ∈ (σC j).flatten, (F.CI j).ann.alwaysRdy c.1 = false → (oc j).rdy c.1 = true) ∧
        ∀ u' : F.Step → Nat → V,
          (∀ e ∈ τP, StepOK ((F.withSites ssM' resM ssR' resR).at u') oc (F.comb oc w).2 e) ∧
          ∀ j, Sub2 (σC j) (sigmaJ ((F.withSites ssM' resM ssR' resR).at u') oc τP j) := by
  intro oc w σ hasm hal hL hD hrdy
  obtain ⟨hoh, τP, σC, hg, hσC, hu⟩ := h oc w σ hasm hal hL hD hrdy
  refine ⟨hoh, τP, σC, hg, hσC, fun u' => ?_⟩
  -- the exact family at the weakened environments, step by step
  let E : F.Step → Nat → V := fun s => match s with
    | .inl m => envHw (u' (.inl m)) oc (ssM' m) 1 (setN (u' (.inl m)) 0 (w.arg m))
    | .inr r => envHw (u' (.inr r)) oc (ssR' r) 0 (u' (.inr r))
  obtain ⟨hst, hsig⟩ := hu E
  have hmeth : ∀ m, (trace oc (runSites (E (.inl m)) (resM m) (ssM m) 1 (setN (E (.inl m)) 0 (w.arg m)))).2 =
        (trace oc (runSites (u' (.inl m)) (resM m) (ssM' m) 1 (setN (u' (.inl m)) 0 (w.arg m)))).2 ∧
      (trace oc (runSites (E (.inl m)) (resM m) (ssM m) 1 (setN (E (.inl m)) 0 (w.arg m)))).1 <+
        (trace oc (runSites (u' (.inl m)) (resM m) (ssM' m) 1 (setN (u' (.inl m)) 0 (w.arg m)))).1 := fun m =>
    trace_weakens_at _ oc _ (hwM m) 1 _ _ (hM m) (hM' m)
      (fun k hk => by obtain rfl : k = 0 := by omega
                      simp [setN])
      (fun k hk => by simp [setN, show k ≠ 0 by omega, E])
  have hrule : ∀ r, (trace oc (runSites (E (.inr r)) (resR r) (ssR r) 0 (E (.inr r)))).2 =
        (trace oc (runSites (u' (.inr r)) (resR r) (ssR' r) 0 (u' (.inr r)))).2 ∧
      (trace oc (runSites (E (.inr r)) (resR r) (ssR r) 0 (E (.inr r)))).1 <+
        (trace oc (runSites (u' (.inr r)) (resR r) (ssR' r) 0 (u' (.inr r)))).1 := fun r =>
    trace_weakens_at _ oc _ (hwR r) 0 _ _ (hR r) (hR' r) (fun k hk => absurd hk (Nat.not_lt_zero _))
      (fun k _ => rfl)
  -- calls in σ carry their port's argument
  have harg : ∀ g, .inl g ∈ τP → ∀ c ∈ g, c.2 = w.arg c.1 := fun g hgτ c hc =>
    call_arg hL hD hself c (List.mem_flatten.mpr ⟨g, hg ▸ mem_groups_of_inl hgτ, hc⟩)
  have hstep : ∀ j, ∀ e ∈ τP, proj j (stepTrace ((F.withSites ssM resM ssR resR).at E) oc e) <+
      proj j (stepTrace ((F.withSites ssM' resM ssR' resR).at u') oc e) := by
    intro j e he
    apply proj_sublist
    rcases e with g | r
    · apply flatMap_sublist
      intro c hc
      rw [harg g he c hc]
      exact (hmeth c.1).2
    · exact (hrule r).2
  refine ⟨fun e he => ?_, fun j => ?_⟩
  · rcases e with g | r
    · intro c hc
      have := hst (.inl g) he c hc
      rw [harg g he c hc] at this ⊢
      exact (hmeth c.1).1 ▸ this
    · have h1 : (trace oc (runSites (E (.inr r)) (resR r) (ssR r) 0 (E (.inr r)))).2.isSome = true :=
        hst (.inr r) he
      show (trace oc (runSites (u' (.inr r)) (resR r) (ssR' r) 0 (u' (.inr r)))).2.isSome = true
      rw [← (hrule r).1]; exact h1
  · rw [← hsig j]
    exact sub2_map _ _ τP (fun e he => hstep j e he)

end SiteCond

#print axioms SiteCond.readsBefore_always
#print axioms SiteCond.envHw_at_always
#print axioms SiteCond.envHw_arg
#print axioms SiteCond.callsOf_weakens
#print axioms SiteCond.weakens_env_exact
#print axioms SiteCond.envHw_weakens
#print axioms SiteCond.trace_weakens
#print axioms SiteCond.legal_of_sublists
#print axioms SiteCond.sitesOK_dropLater
#print axioms SiteCond.weakens_dropLater
#print axioms SiteCond.dropLater_spec
#print axioms SiteCond.ready_of_sublists
#print axioms SiteCond.at_eq_exact
#print axioms SiteCond.uncond_at
#print axioms SiteCond.envHw_unique
#print axioms SiteCond.mem_callsOf
#print axioms SiteCond.envHw_congr_rho
#print axioms SiteCond.trace_weakens_at
#print axioms SiteCond.oblU_weakens
