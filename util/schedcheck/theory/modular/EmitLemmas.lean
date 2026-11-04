/- Library support for the linear emission conventions (conventions.md):
   tables, numbered site lists, ports' uses, children by kind, read-list filters. -/
import Modular
open Modular
open scoped List

set_option autoImplicit false

namespace Emit

/-! ## Balanced tables -/

universe uT

/-- A table by number, as a search tree (round 4's `Tab`, universe-polymorphic
    so that a table of functions of the value type is a closed term). -/
inductive Tab (α : Type uT) where
  | leaf (a : α)
  | node (mid : Nat) (l r : Tab α)

/-- The entry for `k`: left of `mid` below it, right otherwise. -/
def Tab.get {α : Type uT} : Tab α → Nat → α
  | .leaf a, _ => a
  | .node mid l r, k => bif Nat.blt k mid then l.get k else r.get k

/-- `Q k (t.get k)` for every `k` in `[lo, hi)`, as a balanced conjunction:
    a proof is a tree of pairs with one leaf proof per index. -/
def Tab.AllR {α : Type uT} (Q : Nat → α → Prop) : Tab α → Nat → Nat → Prop
  | .leaf a, lo, hi => ∀ k, lo ≤ k → k < hi → Q k a
  | .node mid l r, lo, hi => Tab.AllR Q l lo mid ∧ Tab.AllR Q r mid hi

theorem blt_true {a b : Nat} (h : Nat.blt a b = true) : a < b := Nat.le_of_ble_eq_true h

theorem blt_false {a b : Nat} (h : Nat.blt a b = false) : b ≤ a := by
  have : ¬ a < b := fun h' => by rw [Nat.blt_eq.mpr h'] at h; cases h
  omega

theorem Tab.get_node {α : Type uT} (mid : Nat) (l r : Tab α) (k : Nat) :
    (Tab.node mid l r).get k = bif Nat.blt k mid then l.get k else r.get k := rfl

theorem Tab.get_allR {α : Type uT} {Q : Nat → α → Prop} : ∀ (t : Tab α) (lo hi : Nat), Tab.AllR Q t lo hi →
    ∀ k, lo ≤ k → k < hi → Q k (t.get k)
  | .leaf _, _, _, h, k, h1, h2 => h k h1 h2
  | .node mid l r, lo, hi, h, k, h1, h2 => by
    rw [Tab.get_node]
    cases hb : Nat.blt k mid
    · exact Tab.get_allR r mid hi h.2 k (blt_false hb) h2
    · exact Tab.get_allR l lo mid h.1 k h1 (blt_true hb)

/-- A leaf for exactly one index. -/
theorem Tab.one {α : Type uT} {Q : Nat → α → Prop} {i : Nat} {a : α} (h : Q i a) :
    ∀ k, i ≤ k → k < i + 1 → Q k a :=
  fun k h1 h2 => (show k = i by omega) ▸ h

/-- `Q` at port `pt k` of `Fin n` (a table entry ignored), where `pt k` is the port as the file writes
    it (a closed term): a fact per port of a wide child. -/
def FinQ {α : Type uT} {n : Nat} (pt : Nat → Fin n) (Q : Fin n → Prop) (k : Nat) (_ : α) : Prop := Q (pt k)

/-- `Q m` for every `m : Fin n`, from a balanced tree of proofs shaped as `t` (one leaf per index), so
    that no proof's type holds the rest of the ports (cost review r9b-2 M1). -/
theorem Tab.finAll {α : Type uT} {n : Nat} {Q : Fin n → Prop} (pt : Nat → Fin n) (hpt : ∀ m : Fin n, pt m.val = m)
    (t : Tab α) (h : Tab.AllR (FinQ pt Q) t 0 n) : ∀ m, Q m :=
  fun m => hpt m ▸ Tab.get_allR t 0 n h m.val (Nat.zero_le _) m.isLt

/-- The Boolean version, for the kernel; each leaf must stand for exactly one index. -/
def Tab.allB {α : Type uT} (f : Nat → α → Bool) : Tab α → Nat → Nat → Bool
  | .leaf a, lo, hi => Nat.beq hi (lo + 1) && f lo a
  | .node mid l r, lo, hi => Tab.allB f l lo mid && Tab.allB f r mid hi

theorem Tab.allB_spec {α : Type uT} (f : Nat → α → Bool) : ∀ (t : Tab α) (lo hi : Nat), Tab.allB f t lo hi = true →
    ∀ k, lo ≤ k → k < hi → f k (t.get k) = true
  | .leaf a, lo, hi, h, k, h1, h2 => by
    simp only [Tab.allB, Bool.and_eq_true] at h
    have e := Nat.eq_of_beq_eq_true h.1
    have : k = lo := by omega
    subst this; exact h.2
  | .node mid l r, lo, hi, h, k, h1, h2 => by
    simp only [Tab.allB, Bool.and_eq_true] at h
    rw [Tab.get_node]
    cases hb : Nat.blt k mid
    · exact Tab.allB_spec f r mid hi h.2 k (blt_false hb) h2
    · exact Tab.allB_spec f l lo mid h.1 k h1 (blt_true hb)

/-- A decided `allB` over a node from its halves, each its own kernel decision (cost review r7-2 M2). -/
theorem Tab.allB_node {α : Type uT} {f : Nat → α → Bool} {mid lo hi : Nat} {l r : Tab α}
    (h1 : Tab.allB f l lo mid = true) (h2 : Tab.allB f r mid hi = true) : Tab.allB f (.node mid l r) lo hi = true := by
  simp only [Tab.allB, h1, h2, Bool.and_self]

/-- Congruence in two arguments, with the function given (no higher-order unification). -/
theorem congr2 {α β γ : Type} (f : α → β → γ) {a a' : α} {b b' : β} (h1 : a = a') (h2 : b = b') :
    f a b = f a' b' := by subst h1 h2; rfl

/-! ## A list hypothesis as a conjunction -/

def AllL {α : Type} (P : α → Prop) : List α → Prop
  | [] => True
  | a :: l => P a ∧ AllL P l

theorem allL_of {α : Type} {P : α → Prop} : ∀ {l : List α}, (∀ y ∈ l, P y) → AllL P l
  | [], _ => trivial
  | a :: _, h => ⟨h a (List.mem_cons_self ..), allL_of (fun y hy => h y (List.mem_cons_of_mem _ hy))⟩

theorem AllL.mem {α : Type} {P : α → Prop} : ∀ {l : List α}, AllL P l → ∀ y ∈ l, P y
  | [], _, _, hy => by cases hy
  | _ :: _, h, y, hy => by
    rcases List.mem_cons.mp hy with rfl | hy
    · exact h.1
    · exact AllL.mem h.2 y hy

/-! ## Site lists indexed by number: `site 0, …, site (k-1)`, each a table lookup.
    Child and port come from `key`, condition and argument from `sf`. -/

section Sites
variable {V : Type} {I : Type} {CM : I → Type}

/-- Site `i` from its key (child, port) and its condition and argument. -/
def mkSite (key : Nat → Σ j, CM j) (sf : Nat → ((Nat → V) → Bool) × ((Nat → V) → V)) (i : Nat) :
    Site I CM V :=
  ⟨(key i).1, (key i).2, (sf i).1, (sf i).2⟩

/-- The site list `site 0, …, site (k-1)`. -/
def sitesF (site : Nat → Site I CM V) (k : Nat) : List (Site I CM V) := (List.range' 0 k).map site

theorem sitesF_length (site : Nat → Site I CM V) (k : Nat) : (sitesF site k).length = k := by
  simp [sitesF]

theorem sitesF_get (site : Nat → Site I CM V) (k i : Nat) (h : i < (sitesF site k).length) :
    (sitesF site k)[i] = site i := by
  simp [sitesF, List.getElem_map, List.getElem_range']

theorem sitesOK_range' (site : Nat → Site I CM V) : ∀ (n s : Nat),
    (∀ i, s ≤ i → i < s + n → ReadsBefore i (site i)) → SitesOK ((List.range' s n).map site) s
  | 0, _, _ => trivial
  | n + 1, s, h => ⟨h s (Nat.le_refl _) (by omega),
      sitesOK_range' site n (s + 1) (fun i h1 h2 => h i (by omega) (by omega))⟩

theorem sitesOK_fn (site : Nat → Site I CM V) (k : Nat) (h : ∀ i, i < k → ReadsBefore i (site i)) :
    SitesOK (sitesF site k) 0 :=
  sitesOK_range' site k 0 (fun i _ h2 => h i (by omega))

theorem callsOf_fn_ports (ρ : Nat → V) (site : Nat → Site I CM V) (k : Nat) :
    ∀ c ∈ callsOf ρ (sitesF site k), (⟨c.1, c.2.1⟩ : Σ j, CM j) ∈ (sitesF site k).map (fun s => ⟨s.j, s.m⟩) :=
  fun c hc => (callsOf_ports ρ (sitesF site k)).subset (List.mem_map.mpr ⟨c, hc, rfl⟩)

theorem mem_sitesF {site : Nat → Site I CM V} {k : Nat} {s : Site I CM V} (h : s ∈ sitesF site k) :
    ∃ i, i < k ∧ s = site i := by
  simp only [sitesF, List.mem_map, List.mem_range'] at h
  obtain ⟨i, ⟨n, hn, rfl⟩, rfl⟩ := h
  exact ⟨n, by omega, by simp⟩

theorem site_mem_sitesF (site : Nat → Site I CM V) {k i : Nat} (h : i < k) : site i ∈ sitesF site k := by
  simp only [sitesF, List.mem_map, List.mem_range']
  exact ⟨i, ⟨i, h, by simp⟩, rfl⟩

/-- A site's condition and argument, as a table entry. -/
abbrev SF (V : Type) := ((Nat → V) → Bool) × ((Nat → V) → V)

/-- Site `i`'s condition and argument read only sites below `i` (`ReadsBefore`
    of `mkSite`, definitionally). -/
def SFReads (i : Nat) (c : SF V) : Prop :=
  ∀ ρ ρ' : Nat → V, (∀ i', i' < i → ρ i' = ρ' i') → c.1 ρ = c.1 ρ' ∧ c.2 ρ = c.2 ρ'

theorem sfr_const {i : Nat} (b : Bool) (x : V) : SFReads i ((fun _ => b, fun _ => x) : SF V) :=
  fun _ _ _ => ⟨rfl, rfl⟩

theorem sfr_c {i : Nat} (f : V → Bool) (x : V) (a : Nat) (ha : a < i) :
    SFReads i ((fun ρ => f (ρ a), fun _ => x) : SF V) :=
  fun _ _ h => ⟨congrArg f (h a ha), rfl⟩

theorem sfr_ca {i : Nat} (f : V → Bool) (g : V → V) (a b : Nat) (ha : a < i) (hb : b < i) :
    SFReads i ((fun ρ => f (ρ a), fun ρ => g (ρ b)) : SF V) :=
  fun _ _ h => ⟨congrArg f (h a ha), congrArg g (h b hb)⟩

theorem sitesF_map {β : Type} (site : Nat → Site I CM V) (k : Nat) (g : Site I CM V → β) :
    (sitesF site k).map g = (List.range' 0 k).map (fun i => g (site i)) := by
  simp [sitesF, List.map_map, Function.comp_def]

theorem mem_keys {key : Nat → Σ j, CM j} {sf : Nat → SF V} {k : Nat} {p : Σ j, CM j}
    (hp : p ∈ (sitesF (mkSite key sf) k).map (fun s => (⟨s.j, s.m⟩ : Σ j, CM j))) : ∃ i, i < k ∧ key i = p := by
  rw [List.mem_map] at hp
  obtain ⟨s, hs, rfl⟩ := hp
  obtain ⟨i, hi, rfl⟩ := mem_sitesF hs
  exact ⟨i, hi, rfl⟩

/-- Every call of a numbered step satisfies a property its keys have (decided). -/
theorem calls_all {f : (Σ j, CM j) → Bool} (key : Tab (Σ j, CM j)) (sf : Nat → SF V) (k : Nat)
    (hc : Tab.allB (fun _ p => f p) key 0 k = true) (ρ : Nat → V) :
    ∀ c ∈ callsOf ρ (sitesF (mkSite key.get sf) k), f ⟨c.1, c.2.1⟩ = true := by
  intro c hc'
  obtain ⟨i, hi, e⟩ := mem_keys (callsOf_fn_ports ρ _ k c hc')
  rw [← e]
  exact Tab.allB_spec _ key 0 k hc i (Nat.zero_le _) hi

/-- **A numbered site list makes the same calls at two environments** when every site agrees
    on its condition, and on its argument where taken (per site: `CertU.trU`). -/
theorem callsOf_fn_congr (site : Nat → Site I CM V) (k : Nat) (ρ ρ' : Nat → V)
    (h : ∀ i, i < k → (site i).cond ρ = (site i).cond ρ' ∧ ((site i).cond ρ = true → (site i).arg ρ = (site i).arg ρ')) :
    callsOf ρ (sitesF site k) = callsOf ρ' (sitesF site k) :=
  callsOf_congr ρ ρ' _ (fun s hs => by obtain ⟨i, hi, rfl⟩ := mem_sitesF hs; exact h i hi)

variable (u : Nat → V) (oc : (j : I) → Outs (CM j) V)

/-- **Each value of a numbered site list**, with no list scan. -/
theorem envHw_fn (site : Nat → Site I CM V) (k : Nat) (h : ∀ i, i < k → ReadsBefore i (site i))
    (i : Nat) (hi : i < k) :
    envHw u oc (sitesF site k) 0 u i =
      if (site i).cond (envHw u oc (sitesF site k) 0 u) then (oc (site i).j).ret (site i).m else u i := by
  have hl : i < (sitesF site k).length := by rw [sitesF_length]; exact hi
  have e := envHw_at u oc (sitesF site k) 0 u (sitesOK_fn site k h) i hl
  rw [Nat.zero_add] at e
  rw [e, sitesF_get]

/-- **The trace of a numbered site list.** -/
theorem trace_fn (res : (Nat → V) → Option V) (site : Nat → Site I CM V) (k : Nat)
    (h : ∀ i, i < k → ReadsBefore i (site i)) :
    trace oc (runSites u res (sitesF site k) 0 u) =
      (callsOf (envHw u oc (sitesF site k) 0 u) (sitesF site k), res (envHw u oc (sitesF site k) 0 u)) :=
  trace_runSites u oc res (sitesF site k) 0 u (sitesOK_fn site k h)

variable [DecidableEq I] [∀ j, DecidableEq (CM j)]

theorem onPort_iff (j : I) (cm : CM j) (s : Site I CM V) :
    onPort j cm s = true ↔ (⟨s.j, s.m⟩ : Σ j, CM j) = ⟨j, cm⟩ := by
  rcases s with ⟨sj, sm, sc, sa⟩
  unfold onPort
  simp only
  by_cases h : sj = j
  · subst h; simp
  · simp only [h, dite_false, Bool.false_eq_true, false_iff]
    intro e; exact h (congrArg Sigma.fst e)

/-- **A port's calls in a numbered site list**, from its index list: every
    site on the port is in `idx`, by one decided check per step (`hchk`). -/
theorem mem_proj_fn (ρ : Nat → V) (site : Nat → Site I CM V) (k : Nat) (key : Nat → Σ j, CM j)
    (hkey : ∀ i, (⟨(site i).j, (site i).m⟩ : Σ j, CM j) = key i) (idx : (Σ j, CM j) → List Nat)
    (hchk : ∀ i, i < k → i ∈ idx (key i)) (j : I) (cm : CM j) (x : V)
    (hx : (cm, x) ∈ proj j (callsOf ρ (sitesF site k))) :
    ∃ i ∈ idx ⟨j, cm⟩, (site i).cond ρ = true ∧ (site i).arg ρ = x := by
  obtain ⟨s, hs, hc, ha⟩ := mem_proj_callsOf ρ j cm x _ hx
  rw [List.mem_filter] at hs
  obtain ⟨i, hi, rfl⟩ := mem_sitesF hs.1
  have e : key i = ⟨j, cm⟩ := (hkey i).symm.trans ((onPort_iff j cm _).mp hs.2)
  exact ⟨i, e ▸ hchk i hi, hc, ha⟩

theorem calls_fn (ρ : Nat → V) (site : Nat → Site I CM V) (k : Nat) (key : Nat → Σ j, CM j)
    (hkey : ∀ i, (⟨(site i).j, (site i).m⟩ : Σ j, CM j) = key i) (idx : (Σ j, CM j) → List Nat)
    (hchk : ∀ i, i < k → i ∈ idx (key i)) (j : I) (cm : CM j)
    (hs : ∀ i ∈ idx ⟨j, cm⟩, i < k ∧ key i = ⟨j, cm⟩) :
    (∃ x, (cm, x) ∈ proj j (callsOf ρ (sitesF site k))) ↔ ∃ i ∈ idx ⟨j, cm⟩, (site i).cond ρ = true := by
  constructor
  · rintro ⟨x, hx⟩
    obtain ⟨i, hi, hc, -⟩ := mem_proj_fn ρ site k key hkey idx hchk j cm x hx
    exact ⟨i, hi, hc⟩
  · rintro ⟨i, hi, hc⟩
    obtain ⟨hik, hkk⟩ := hs i hi
    apply mem_proj_callsOf_of ρ j cm (sitesF site k) (site i) _ hc
    rw [List.mem_filter]
    exact ⟨site_mem_sitesF site hik, (onPort_iff j cm _).mpr ((hkey i).trans hkk)⟩

/-- The common case: the port has one site in the step. -/
theorem calls_one (ρ : Nat → V) (site : Nat → Site I CM V) (k : Nat) (key : Nat → Σ j, CM j)
    (hkey : ∀ i, (⟨(site i).j, (site i).m⟩ : Σ j, CM j) = key i) (idx : (Σ j, CM j) → List Nat)
    (hchk : ∀ i, i < k → i ∈ idx (key i)) (j : I) (cm : CM j) (i0 : Nat)
    (hidx : idx ⟨j, cm⟩ = [i0]) (hi0 : Nat.blt i0 k = true) (hk0 : key i0 = ⟨j, cm⟩) :
    (∃ x, (cm, x) ∈ proj j (callsOf ρ (sitesF site k))) ↔ (site i0).cond ρ = true := by
  have hs : ∀ i ∈ idx ⟨j, cm⟩, i < k ∧ key i = ⟨j, cm⟩ := by
    rw [hidx]; intro i hi
    rw [List.mem_singleton] at hi; subst hi
    exact ⟨blt_true hi0, hk0⟩
  rw [calls_fn ρ site k key hkey idx hchk j cm hs, hidx]
  simp

/-- The port has no site in the step. -/
theorem calls_none (ρ : Nat → V) (site : Nat → Site I CM V) (k : Nat) (key : Nat → Σ j, CM j)
    (hkey : ∀ i, (⟨(site i).j, (site i).m⟩ : Σ j, CM j) = key i) (idx : (Σ j, CM j) → List Nat)
    (hchk : ∀ i, i < k → i ∈ idx (key i)) (j : I) (cm : CM j) (hidx : idx ⟨j, cm⟩ = []) (x : V) :
    (cm, x) ∉ proj j (callsOf ρ (sitesF site k)) := by
  intro hx
  obtain ⟨i, hi, -⟩ := mem_proj_fn ρ site k key hkey idx hchk j cm x hx
  rw [hidx] at hi; cases hi

/-- The argument of a port's one site. -/
theorem arg_one (ρ : Nat → V) (site : Nat → Site I CM V) (k : Nat) (key : Nat → Σ j, CM j)
    (hkey : ∀ i, (⟨(site i).j, (site i).m⟩ : Σ j, CM j) = key i) (idx : (Σ j, CM j) → List Nat)
    (hchk : ∀ i, i < k → i ∈ idx (key i)) (j : I) (cm : CM j) (i0 : Nat)
    (hidx : idx ⟨j, cm⟩ = [i0]) (v : V) (hv : (site i0).cond ρ = true → (site i0).arg ρ = v) (x : V)
    (hx : (cm, x) ∈ proj j (callsOf ρ (sitesF site k))) : v = x := by
  obtain ⟨i, hi, hc, ha⟩ := mem_proj_fn ρ site k key hkey idx hchk j cm x hx
  rw [hidx] at hi; simp at hi; subst hi
  rw [← ha, hv hc]

end Sites

/-! ## Ports' uses: users and per-step index lists from one table -/

section Uses
variable {S : Type} [DecidableEq S]

/-- The sites of step `s` on a port, from the port's uses `(step, site)`. -/
def idxOf (uses : List (S × Nat)) (s : S) : List Nat :=
  uses.filterMap (fun e => if e.1 = s then some e.2 else none)

/-- The steps that use a port. -/
def usersOf (uses : List (S × Nat)) : List S := uses.map Prod.fst

theorem mem_usersOf : ∀ {uses : List (S × Nat)} {s : S} {i : Nat}, i ∈ idxOf uses s → s ∈ usersOf uses
  | [], _, _, h => by simp [idxOf] at h
  | e :: l, s, i, h => by
    simp only [usersOf, List.map_cons, List.mem_cons]
    by_cases he : e.1 = s
    · exact .inl he.symm
    · have h' : i ∈ idxOf l s := by simpa [idxOf, List.filterMap_cons, he] using h
      exact .inr (mem_usersOf h')

end Uses

/-! ## A port's uses as a table keyed by step (lookups, not list walks) -/

/-- The leaves of a table, in order. -/
def Tab.toList {α : Type uT} : Tab α → List α
  | .leaf a => [a]
  | .node _ l r => l.toList ++ r.toList

theorem Tab.get_mem {α : Type uT} : ∀ (t : Tab α) (k : Nat), t.get k ∈ t.toList
  | .leaf a, _ => List.mem_singleton_self a
  | .node mid l r, k => by
    rw [Tab.get_node]
    cases Nat.blt k mid
    · exact List.mem_append_right _ (Tab.get_mem r k)
    · exact List.mem_append_left _ (Tab.get_mem l k)

/-- Every leaf of a table has `Q`: a balanced conjunction, so a proof is a tree of pairs. -/
def Tab.All {α : Type uT} (Q : α → Prop) : Tab α → Prop
  | .leaf a => Q a
  | .node _ l r => Tab.All Q l ∧ Tab.All Q r

theorem Tab.all_mem {α : Type uT} {Q : α → Prop} : ∀ {t : Tab α}, Tab.All Q t → ∀ e ∈ t.toList, Q e
  | .leaf _, h, e, he => by rw [List.mem_singleton.mp he]; exact h
  | .node _ l r, h, e, he => by
    rcases List.mem_append.mp he with he | he
    · exact Tab.all_mem h.1 e he
    · exact Tab.all_mem h.2 e he

/-- A port's sites in step `k`: the leaf found for `k`, if it is `k`'s (leaves are `(step, sites)`). -/
def idxT (t : Tab (Nat × List Nat)) (k : Nat) : List Nat :=
  bif Nat.beq (t.get k).1 k then (t.get k).2 else []

/-- The steps with a site on the port. -/
def usersT (t : Tab (Nat × List Nat)) : List Nat :=
  t.toList.filterMap (fun e => bif e.2.isEmpty then none else some e.1)

theorem mem_usersT {t : Tab (Nat × List Nat)} {k i : Nat} (h : i ∈ idxT t k) : k ∈ usersT t := by
  unfold idxT at h
  cases hb : Nat.beq (t.get k).1 k
  · rw [hb] at h; cases h
  · rw [hb] at h
    have ek : (t.get k).1 = k := Nat.eq_of_beq_eq_true hb
    have hne : (t.get k).2.isEmpty = false := by
      cases e : (t.get k).2 with
      | nil => rw [e] at h; cases h
      | cons _ _ => rfl
    unfold usersT
    rw [List.mem_filterMap]
    exact ⟨t.get k, Tab.get_mem t k, by rw [hne, ek]; rfl⟩

/-- A fact for every step with a site on the port, from a balanced proof over its uses table. -/
theorem usersT_all {t : Tab (Nat × List Nat)} {Q : Nat → Prop}
    (h : Tab.All (fun e => e.2.isEmpty = false → Q e.1) t) : ∀ k ∈ usersT t, Q k := by
  intro k hk
  unfold usersT at hk
  obtain ⟨e, he, hk⟩ := List.mem_filterMap.mp hk
  cases hb : e.2.isEmpty
  · rw [hb] at hk
    cases hk
    exact Tab.all_mem h e he hb
  · rw [hb] at hk; cases hk

/-- `l1`'s entries occur in `l2` in `l2`'s order: one pass over both lists (cost review r8-2 M1). -/
def subS : List Nat → List Nat → Bool
  | l1, [] => l1.isEmpty
  | l1, b :: l2 => subS (l1.dropWhile (fun a => Nat.beq a b)) l2

theorem mem_dropWhile_or (p : Nat → Bool) : ∀ (l : List Nat) (x : Nat), x ∈ l → p x = true ∨ x ∈ l.dropWhile p
  | [], _, h => by cases h
  | a :: l, x, h => by
    cases hp : p a
    · right; simp only [List.dropWhile, hp]; exact h
    · rcases List.mem_cons.mp h with e | h
      · left; rw [e]; exact hp
      · simp only [List.dropWhile, hp]; exact mem_dropWhile_or p l x h

theorem subS_mem : ∀ (l2 l1 : List Nat), subS l1 l2 = true → ∀ x ∈ l1, x ∈ l2
  | [], l1, h, x, hx => by
    cases l1 with
    | nil => cases hx
    | cons _ _ => simp [subS] at h
  | b :: l2, l1, h, x, hx => by
    have h' : subS (l1.dropWhile (fun a => Nat.beq a b)) l2 = true := h
    rcases mem_dropWhile_or (fun a => Nat.beq a b) l1 x hx with e | hx
    · rw [Nat.eq_of_beq_eq_true e]; exact List.mem_cons_self ..
    · exact List.mem_cons_of_mem _ (subS_mem l2 _ h' x hx)

/-- An entry of a keyed table's list for `k` comes from a leaf `(k, l)`. -/
theorem mem_idxT {t : Tab (Nat × List Nat)} {k i : Nat} (h : i ∈ idxT t k) :
    ∃ e ∈ t.toList, e.1 = k ∧ i ∈ e.2 := by
  unfold idxT at h
  cases hb : Nat.beq (t.get k).1 k
  · rw [hb] at h; cases h
  · rw [hb] at h
    exact ⟨t.get k, Tab.get_mem t k, Nat.eq_of_beq_eq_true hb, h⟩

/-- From a step's decided check: every site index is in its port's index list. -/
theorem chk_spec {α : Type} (idx : α → List Nat) (key : Tab α) (k : Nat)
    (h : Tab.allB (fun i p => (idx p).contains i) key 0 k = true) : ∀ i, i < k → i ∈ idx (key.get i) :=
  fun i hi => by
    have := Tab.allB_spec _ key 0 k h i (Nat.zero_le _) hi
    simpa using this

/-! ## Distinct children in a step, from a monotone numbering (decided) -/

theorem pairwise_range' {R : Nat → Nat → Prop} : ∀ (n s : Nat),
    (∀ a b, s ≤ a → a < b → b < s + n → R a b) → (List.range' s n).Pairwise R
  | 0, _, _ => List.Pairwise.nil
  | n + 1, s, h => by
    show (s :: List.range' (s + 1) n).Pairwise R
    refine List.Pairwise.cons ?_ (pairwise_range' n (s + 1) (fun a b h1 h2 h3 => h a b (by omega) h2 (by omega)))
    intro b hb
    rw [List.mem_range'_1] at hb
    exact h s b (Nat.le_refl _) (by omega) (by omega)

theorem nodup_of_mono {β : Type} (f : Nat → β) (num : β → Nat) (k : Nat)
    (h : ∀ i, i + 1 < k → num (f i) < num (f (i + 1))) : ((List.range' 0 k).map f).Nodup := by
  have mono : ∀ d i, i + d + 1 < k → num (f i) < num (f (i + d + 1)) := by
    intro d
    induction d with
    | zero => intro i hi; exact h i hi
    | succ d ih => intro i hi; exact Nat.lt_trans (ih i (by omega)) (h (i + d + 1) (by omega))
  refine List.Pairwise.map f (R := fun a b => a < b ∧ b < k) (fun a b hab e => ?_)
    (pairwise_range' k 0 (fun a b _ hab hb => ⟨hab, by omega⟩))
  have := mono (b - a - 1) a (by omega)
  rw [show a + (b - a - 1) + 1 = b by omega, e] at this
  exact Nat.lt_irrefl _ this

theorem mono_spec {α β : Type} (num : β → Nat) (proj1 : α → β) (key : Tab α) (k : Nat)
    (h : Tab.allB (fun i p => !(Nat.blt (i + 1) k) || Nat.blt (num (proj1 p)) (num (proj1 (key.get (i + 1)))))
      key 0 k = true) :
    ∀ i, i + 1 < k → num (proj1 (key.get i)) < num (proj1 (key.get (i + 1))) := by
  intro i hi
  have := Tab.allB_spec _ key 0 k h i (Nat.zero_le _) (by omega)
  have hb : Nat.blt (i + 1) k = true := Nat.blt_eq.mpr hi
  rw [hb] at this
  simp only [Bool.not_true, Bool.false_or] at this
  exact blt_true this

/-! ## Children by kind, and the completeness lemma for `Acyc.dep` -/

section Kinds
variable {K : Type}

/-- Children indexed per kind: `⟨κ, i⟩` is the `i`-th child of kind `κ`. -/
def allKids (kinds : List K) (nk : K → Nat) : List (Σ κ, Fin (nk κ)) :=
  kinds.flatMap (fun κ => (List.finRange (nk κ)).map (Sigma.mk κ))

theorem mem_allKids (kinds : List K) (nk : K → Nat) (hk : ∀ κ, κ ∈ kinds) (j : Σ κ, Fin (nk κ)) :
    j ∈ allKids kinds nk := by
  obtain ⟨κ, i⟩ := j
  simp only [allKids, List.mem_flatMap, List.mem_map]
  exact ⟨κ, hk κ, i, List.mem_finRange i, rfl⟩

theorem allKids_nodup (nk : K → Nat) : ∀ (kinds : List K), kinds.Nodup → (allKids kinds nk).Nodup
  | [], _ => List.Pairwise.nil
  | κ :: ks, hk => by
    rw [List.nodup_cons] at hk
    show ((List.finRange (nk κ)).map (Sigma.mk κ) ++ allKids ks nk).Nodup
    rw [List.nodup_append]
    refine ⟨List.Pairwise.map (Sigma.mk κ) (fun a b hab e => hab (by cases e; rfl)) (List.nodup_finRange _),
      allKids_nodup nk ks hk.2, ?_⟩
    intro a ha b hb e
    subst e
    simp only [List.mem_map] at ha
    obtain ⟨i, -, rfl⟩ := ha
    simp only [allKids, List.mem_flatMap, List.mem_map] at hb
    obtain ⟨κ', hκ', i', -, e⟩ := hb
    have e' : κ' = κ := congrArg Sigma.fst e
    exact hk.1 (e' ▸ hκ')

/-- Every port of every child, by kind. -/
def allPorts (kinds : List K) (nk : K → Nat) (M : K → Type) (ports : (κ : K) → List (M κ)) :
    List (Σ j : (Σ κ, Fin (nk κ)), M j.1) :=
  (allKids kinds nk).flatMap (fun j => (ports j.1).map (Sigma.mk j))

theorem mem_allPorts (kinds : List K) (nk : K → Nat) (M : K → Type) (ports : (κ : K) → List (M κ))
    (hk : ∀ κ, κ ∈ kinds) (hp : ∀ κ (m : M κ), m ∈ ports κ) (y : Σ j : (Σ κ, Fin (nk κ)), M j.1) :
    y ∈ allPorts kinds nk M ports := by
  obtain ⟨j, m⟩ := y
  simp only [allPorts, List.mem_flatMap, List.mem_map]
  exact ⟨j, mem_allKids kinds nk hk j, m, hp j.1 m, rfl⟩

end Kinds

/-- A read list as a filter of a complete list: its hypothesis becomes a predicate,
    so a proof uses one decided fact per direct read, never a membership search. -/
theorem forall_filter {α : Type} (l : List α) (hl : ∀ y, y ∈ l) (f : α → Bool) {P : α → Prop}
    (h : ∀ y ∈ l.filter f, P y) : ∀ y, f y = true → P y :=
  fun y hy => h y (List.mem_filter.mpr ⟨hl y, hy⟩)

theorem Wires.ext' {V M : Type} {w w' : Wires M V} (h : ∀ p, AgreeW w w' p) : w = w' := by
  cases w; cases w'
  congr
  · funext p; exact (h p).1
  · funext p; exact (h p).2

/-- **The completeness lemma for `Acyc.dep`**: with `D := allOuts`, `Dw := allIns`,
    agreement on every output and input is equality; nothing about `comb` is unfolded. -/
theorem dep_of_all {V : Type} (P : Parent V) (allO : List (Σ j, (P.CI j).M)) (hO : ∀ y, y ∈ allO)
    (allI : List P.X.M) (hI : ∀ p, p ∈ allI) (oc oc' : (j : P.I) → Outs (P.CI j).M V) (w w' : Wires P.X.M V)
    (hD : ∀ y ∈ allO, AgreeO (oc y.1) (oc' y.1) y.2) (hW : ∀ p ∈ allI, AgreeW w w' p) (j : P.I)
    (p : (P.CI j).M) : AgreeW ((P.comb oc w).1 j) ((P.comb oc' w').1 j) p := by
  have e1 : oc = oc' := funext fun j => Outs.ext' fun q => hD ⟨j, q⟩ (hO _)
  have e2 : w = w' := Wires.ext' fun p => hW p (hI p)
  subst e1 e2; exact ⟨rfl, rfl⟩

/-! ## Per-port facts with one user step -/

section PerPort
variable {V : Type} (P : Parent V)

theorem en_one (fire : ((j : P.I) → Outs (P.CI j).M V) → Wires P.X.M V → P.PR → Bool)
    (oc : (j : P.I) → Outs (P.CI j).M V) (w : Wires P.X.M V) (j : P.I) (cm : (P.CI j).M) (s : Step P)
    (users : List (Step P)) (hu : users = [s]) (C : Prop) (hcalls : Calls P (trS P oc w s) j cm ↔ C) (e : Bool)
    (he : e = true ↔ On P fire oc w s ∧ C) :
    (e = true ↔ ∃ s' ∈ users, On P fire oc w s' ∧ Calls P (trS P oc w s') j cm) := by
  subst hu; simp only [List.mem_singleton, exists_eq_left]; rw [hcalls]; exact he

theorem en_none (fire : ((j : P.I) → Outs (P.CI j).M V) → Wires P.X.M V → P.PR → Bool)
    (oc : (j : P.I) → Outs (P.CI j).M V) (w : Wires P.X.M V) (j : P.I) (cm : (P.CI j).M)
    (users : List (Step P)) (hu : users = []) (e : Bool) (he : e = false) :
    (e = true ↔ ∃ s' ∈ users, On P fire oc w s' ∧ Calls P (trS P oc w s') j cm) := by
  subst hu he; simp

theorem and_iff_of_eq {a : Bool} {f : V → Bool} {x y : V} (e : y = x) :
    (a && f x) = true ↔ a = true ∧ f y = true := by
  subst e; exact Bool.and_eq_true_iff

/-- `OkT` from port classes: every pair of classes the two steps use may run
    in that order on one child (decided per step, linear). -/
theorem okT_of_cls {C : Type} [DecidableEq C] (cls : (Σ j, (P.CI j).M) → C) (okC : C → C → Bool)
    (hok : ∀ j (a b : (P.CI j).M × V), okC (cls ⟨j, a.1⟩) (cls ⟨j, b.1⟩) = true → (P.CI j).ann.Follow a b)
    (t1 t2 : List (Σ j, (P.CI j).M × V)) (A B : List C)
    (h1 : ∀ c ∈ t1, A.contains (cls ⟨c.1, c.2.1⟩) = true) (h2 : ∀ c ∈ t2, B.contains (cls ⟨c.1, c.2.1⟩) = true)
    (hAB : (A.all fun a => B.all fun b => okC a b) = true) : OkT P t1 t2 := by
  intro j a ha b hb
  apply hok j a b
  have ha' := List.contains_iff_mem.mp (h1 ⟨j, a⟩ (mem_proj j t1 a ha))
  have hb' := List.contains_iff_mem.mp (h2 ⟨j, b⟩ (mem_proj j t2 b hb))
  simp only [List.all_eq_true] at hAB
  exact hAB _ ha' _ hb'

end PerPort

/-! ## Steps whose skipped values are their own (design §18 trust point 7, as a Lean fact) -/

/-- Rules as site lists: each rule's program takes its skipped values `v` only through
    `runSites` (as its skipped values and initial environment); its sites and result do not
    receive `v`. A generated file proves `F ops E = (F ops E).withRuleSites …` by `rfl`. -/
@[reducible] def withRuleSites {V : Type} (F : ParentU V) (ssR : F.PR → List (Site F.I (fun j => (F.CI j).M) V))
    (resR : F.PR → (Nat → V) → Option V) : ParentU V :=
  { F with rbody := fun r v => runSites v (resR r) (ssR r) 0 v }

/-- Rules and methods as site lists; a method's sites and result receive its argument `x`,
    never its skipped values. -/
@[reducible] def withSitesX {V : Type} (F : ParentU V) (ssM : F.X.M → V → List (Site F.I (fun j => (F.CI j).M) V))
    (resM : F.X.M → V → (Nat → V) → Option V) (ssR : F.PR → List (Site F.I (fun j => (F.CI j).M) V))
    (resR : F.PR → (Nat → V) → Option V) : ParentU V :=
  { withRuleSites F ssR resR with body := fun m x v => runSites v (resM m x) (ssM m x) 0 v }

end Emit

#print axioms Emit.Tab.get_allR
#print axioms Emit.Tab.allB_spec
#print axioms Emit.sitesOK_fn
#print axioms Emit.envHw_fn
#print axioms Emit.trace_fn
#print axioms Emit.calls_one
#print axioms Emit.arg_one
#print axioms Emit.allKids_nodup
#print axioms Emit.mem_allPorts
#print axioms Emit.forall_filter
#print axioms Emit.dep_of_all
#print axioms Emit.en_one
#print axioms Emit.mem_usersT
#print axioms Emit.Tab.all_mem
#print axioms Emit.usersT_all
#print axioms Emit.callsOf_fn_congr
#print axioms Emit.Tab.allB_node
#print axioms Emit.subS_mem
#print axioms Emit.mem_idxT
