/- LastCaller.lean: last caller for self-SBR primitive action ports (decision of 2026-11-02).
   Imports `Scan` for the scan-side order facts; the primitive models and laws are below. -/
import Scan

open scoped List

namespace Modular
namespace LC

open Emit (Tab)

variable {V : Type}

/-! ## The certificate from the scan -/

section CertL
variable (P : Parent V)

/-- A `CertL` from the scan's pair fields, the per-step and per-port fields, and the last-caller ones. -/
def PairFacts.toCertL {rules : List P.PR} {fire : ((j : P.I) → Outs (P.CI j).M V) → Wires P.X.M V → P.PR → Bool}
    (pf : Scan.PairFacts P rules fire)
    (fp : Step P → List (Σ j, (P.CI j).M)) (users : (j : P.I) → (P.CI j).M → List (Step P))
    (users_ok : ∀ s, ∀ p ∈ fp s, s ∈ users p.1 p.2)
    (fp_ok : ∀ oc w s, ∀ c ∈ trS P oc w s, (⟨c.1, c.2.1⟩ : Σ j, (P.CI j).M) ∈ fp s)
    (selfAnn : ∀ m, P.X.ann.conf m m = true ∨ P.X.ann.share m = true)
    (resM : ∀ oc w m, Env P oc w → Avail P oc w m → (trM P oc w m).2 = some ((P.comb oc w).2.ret m))
    (resR : ∀ oc w r, Env P oc w → fire oc w r = true → (trR P oc r).2.isSome = true)
    (rdyM : ∀ oc w m, Env P oc w → Avail P oc w m → ∀ c ∈ (trM P oc w m).1,
      (P.CI c.1).ann.alwaysRdy c.2.1 = false → (oc c.1).rdy c.2.1 = true)
    (rdyR : ∀ oc w r, Env P oc w → fire oc w r = true → ∀ c ∈ (trR P oc r).1,
      (P.CI c.1).ann.alwaysRdy c.2.1 = false → (oc c.1).rdy c.2.1 = true)
    (en : ∀ oc w j cm, Env P oc w → (P.CI j).ann.hasEn cm = true →
      (((P.comb oc w).1 j).en cm = true ↔ ∃ s ∈ users j cm, On P fire oc w s ∧ Calls P (trS P oc w s) j cm))
    (enV : ∀ oc w m, Env P oc w → P.X.ann.hasEn m = false → P.X.ann.alwaysEn m = false → Avail P oc w m →
      ∀ c ∈ (trM P oc w m).1, (P.CI c.1).ann.hasEn c.2.1 = false)
    (alw : ∀ oc w j cm, Env P oc w → (P.CI j).ann.alwaysEn cm = true →
      ∃ s ∈ users j cm, On P fire oc w s ∧ Calls P (trS P oc w s) j cm)
    (onehot : ∀ oc w, EnvC P oc w → P.onehot oc w)
    (lc : (j : P.I) → (P.CI j).M → Bool)
    (lc_ok : ∀ j cm, lc j cm = true → (P.CI j).ann.sbr cm cm = true ∧ (P.CI j).ann.hasEn cm = true)
    (lp : Step P → Nat)
    (lpRR : ∀ oc r1 r2 j cm, lc j cm = true → Calls P (trR P oc r1).1 j cm → Calls P (trR P oc r2).1 j cm →
      [r1, r2] <+ rules → lp (.inr r1) < lp (.inr r2))
    (lpMR : ∀ oc w m r j cm, lc j cm = true → Calls P (trM P oc w m).1 j cm → Calls P (trR P oc r).1 j cm →
      lp (.inl m) < lp (.inr r))
    (lcMR : ∀ oc w m r j cm, Env P oc w → Avail P oc w m → fire oc w r = true → lc j cm = true →
      Calls P (trM P oc w m).1 j cm → Calls P (trR P oc r).1 j cm → pf.hit oc w m r = true)
    (lcRR : ∀ oc w m r1 r2 j cm, Env P oc w → Avail P oc w m → fire oc w r1 = true → fire oc w r2 = true →
      lc j cm = true → Calls P (trR P oc r1).1 j cm → Calls P (trR P oc r2).1 j cm →
      lp (.inr r1) < lp (.inr r2) → pf.hit oc w m r1 = true → pf.hit oc w m r2 = true)
    (lcMM : ∀ oc w m1 m2 j cm, Env P oc w → Avail P oc w m1 → Avail P oc w m2 → lc j cm = true →
      Calls P (trM P oc w m1).1 j cm → Calls P (trM P oc w m2).1 j cm → lp (.inl m1) < lp (.inl m2) →
      ¬ P.X.ann.Follow (m2, w.arg m2) (m1, w.arg m1))
    (argN : ∀ oc w j cm, Env P oc w → lc j cm = false → ∀ s ∈ users j cm, Av P fire oc w s →
      ∀ x, (cm, x) ∈ proj j (trS P oc w s) → ((P.comb oc w).1 j).arg cm = x)
    (argL : ∀ oc w j cm, Env P oc w → lc j cm = true → ∀ s ∈ users j cm, On P fire oc w s →
      ∀ x, (cm, x) ∈ proj j (trS P oc w s) →
      (∀ s' ∈ users j cm, lp s < lp s' → On P fire oc w s' → ¬ Calls P (trS P oc w s') j cm) →
      ((P.comb oc w).1 j).arg cm = x) : CertL P where
  rules := rules
  rules_nodup := pf.rules_nodup
  fire := fire
  hit := pf.hit
  fp := fp
  users := users
  users_ok := users_ok
  fp_ok := fp_ok
  selfAnn := selfAnn
  fireIn := pf.fireIn
  resM := resM
  resR := resR
  rdyM := rdyM
  rdyR := rdyR
  grpM := pf.grpM
  grpR := pf.grpR
  mm := pf.mm
  mmg := pf.mmg
  h1 := pf.h1
  h2 := pf.h2
  h3 := pf.h3
  h4 := pf.h4
  en := en
  enV := enV
  alw := alw
  onehot := onehot
  lc := lc
  lc_ok := lc_ok
  lp := lp
  lpRR := lpRR
  lpMR := lpMR
  lcMR := lcMR
  lcRR := lcRR
  lcMM := lcMM
  argN := argN
  argL := argL

end CertL

/-! ## The order facts from the scan: each caller of a last-caller port also uses the port's virtual
slot, whose code follows no other use of it, so the scan passes hits from earlier to later callers. -/

section Virtual
variable {P : Parent V} {D : Scan.Design} {G : Scan.Glue P D}
  {fire : ((j : P.I) → Outs (P.CI j).M V) → Wires P.X.M V → P.PR → Bool}

/-- The scan's step positions: methods by index, then rules by rank. -/
def lpOf (G : Scan.Glue P D) : Step P → Nat
  | .inl m => G.mi m
  | .inr r => D.nM + G.rk r

theorem lpRR_of_scan (H : Scan.GlueOK P D G fire) (lc : (j : P.I) → (P.CI j).M → Bool) :
    ∀ (oc : (j : P.I) → Outs (P.CI j).M V) r1 r2 (j : P.I) (cm : (P.CI j).M), lc j cm = true →
      Calls P (trR P oc r1).1 j cm → Calls P (trR P oc r2).1 j cm →
      [r1, r2] <+ G.rules → lpOf G (.inr r1) < lpOf G (.inr r2) :=
  fun _ _ _ _ _ _ _ _ h => Nat.add_lt_add_left (Scan.rank_lt H h) _

theorem lpMR_of_scan (H : Scan.GlueOK P D G fire) (lc : (j : P.I) → (P.CI j).M → Bool) :
    ∀ (oc : (j : P.I) → Outs (P.CI j).M V) w m r (j : P.I) (cm : (P.CI j).M), lc j cm = true →
      Calls P (trM P oc w m).1 j cm → Calls P (trR P oc r).1 j cm → lpOf G (.inl m) < lpOf G (.inr r) :=
  fun _ _ m r _ _ _ _ _ => Nat.lt_of_lt_of_le (H.mi_lt m) (Nat.le_add_right _ _)

variable (vb : (j : P.I) → (P.CI j).M → Nat) (κv : Nat)

theorem lcMR_of_scan (hok : D.Ok) (H : Scan.GlueOK P D G fire) (lc : (j : P.I) → (P.CI j).M → Bool)
    (hkv : 0 ∈ (D.kc κv 0).cout)
    (hvM : ∀ oc w m (j : P.I) (cm : (P.CI j).M), lc j cm = true → Calls P (trM P oc w m).1 j cm →
      (vb j cm, κv, 0) ∈ D.fpM.get (G.mi m))
    (hvR : ∀ oc r (j : P.I) (cm : (P.CI j).M), lc j cm = true → Calls P (trR P oc r).1 j cm →
      (vb j cm, κv, 0) ∈ D.fpR.get (G.rk r)) :
    ∀ oc w m r (j : P.I) (cm : (P.CI j).M), Env P oc w → Avail P oc w m → fire oc w r = true → lc j cm = true →
      Calls P (trM P oc w m).1 j cm → Calls P (trR P oc r).1 j cm →
      (D.hitS (G.rk r)).testBit (G.mi m) = true := by
  intro oc w m r j cm hE am fr hlc hcm hcr
  rcases D.h1_static hok (H.mi_lt m) (H.rk_lt r) (hvR oc r j cm hlc hcr) (hvM oc w m j cm hlc hcm) hkv with h | h
  · exact h
  · exact (H.exMR oc w m r hE h am fr).elim

theorem lcRR_of_scan (hok : D.Ok) (H : Scan.GlueOK P D G fire) (lc : (j : P.I) → (P.CI j).M → Bool)
    (hkv : 0 ∈ (D.kc κv 0).cout)
    (hvR : ∀ oc r (j : P.I) (cm : (P.CI j).M), lc j cm = true → Calls P (trR P oc r).1 j cm →
      (vb j cm, κv, 0) ∈ D.fpR.get (G.rk r)) :
    ∀ oc w m r1 r2 (j : P.I) (cm : (P.CI j).M), Env P oc w → Avail P oc w m → fire oc w r1 = true →
      fire oc w r2 = true → lc j cm = true → Calls P (trR P oc r1).1 j cm → Calls P (trR P oc r2).1 j cm →
      lpOf G (.inr r1) < lpOf G (.inr r2) → (D.hitS (G.rk r1)).testBit (G.mi m) = true →
      (D.hitS (G.rk r2)).testBit (G.mi m) = true := by
  intro oc w m r1 r2 j cm hE am f1 f2 hlc hc1 hc2 hlt hh
  have hlt' : G.rk r1 < G.rk r2 := Nat.lt_of_add_lt_add_left hlt
  rcases D.h2_static hok hlt' (H.rk_lt r2) (hvR oc r1 j cm hlc hc1) (hvR oc r2 j cm hlc hc2) hkv hh with h | h
  · exact h
  · exact (H.exMR oc w m r2 hE h am f2).elim

theorem lcMM_of_scan (hok : D.Ok) (H : Scan.GlueOK P D G fire) (lc : (j : P.I) → (P.CI j).M → Bool)
    (hkv : 0 ∈ (D.kc κv 0).cout)
    (hvM : ∀ oc w m (j : P.I) (cm : (P.CI j).M), lc j cm = true → Calls P (trM P oc w m).1 j cm →
      (vb j cm, κv, 0) ∈ D.fpM.get (G.mi m)) :
    ∀ oc w m1 m2 (j : P.I) (cm : (P.CI j).M), Env P oc w → Avail P oc w m1 → Avail P oc w m2 → lc j cm = true →
      Calls P (trM P oc w m1).1 j cm → Calls P (trM P oc w m2).1 j cm → lpOf G (.inl m1) < lpOf G (.inl m2) →
      ¬ P.X.ann.Follow (m2, w.arg m2) (m1, w.arg m1) := by
  intro oc w m1 m2 j cm hE a1 a2 hlc hc1 hc2 hlt hf
  have hlt' : G.mi m1 < G.mi m2 := hlt
  rcases D.mm_static_out hok Scan.KC.cout D.nfO (.inl ⟨rfl, rfl⟩) (H.mi_lt m2) (H.mi_lt m1) (Nat.le_of_lt hlt')
      (fun _ => rfl) (hvM oc w m2 j cm hlc hc2) (hvM oc w m1 j cm hlc hc1) hkv with h | ⟨hne, h⟩
  · exact H.nfO m2 m1 h _ _ hf
  · exact hne (H.exM oc w m1 m2 hE h a1 a2).symm

end Virtual

/-! ## The last-caller argument from a priority mux -/

/-- **A priority mux in decreasing `key`**: the selected entry with the greatest key wins. -/
theorem pmux_last {β : Type} (f : β → Bool × V) (key : β → Nat) (d : V) :
    ∀ (l : List β), l.Pairwise (fun a c => key c < key a) → ∀ b ∈ l, (f b).1 = true →
      (∀ a ∈ l, key b < key a → (f a).1 = false) → Scan.pmux (l.map f) d = (f b).2
  | [], _, _, hb, _, _ => by cases hb
  | a :: l, hp, b, hb, hs, hl => by
    rcases List.mem_cons.mp hb with rfl | hb'
    · simp [Scan.pmux, hs]
    · have hlt : key b < key a := List.rel_of_pairwise_cons hp hb'
      have hfa : (f a).1 = false := hl a (List.mem_cons_self ..) hlt
      simp only [List.map_cons, Scan.pmux, hfa, cond_false]
      exact pmux_last f key d l (List.pairwise_cons.mp hp).2 b hb' hs
        (fun a' ha' h => hl a' (List.mem_cons_of_mem _ ha') h)

/-- `CertL.argL` for a port whose argument is a priority mux over table positions `0 … u-1` in
    decreasing `lp` (`keyf`, decided by `incB`), with one selection fact per position. -/
theorem argL_of_pmux (P : Parent V) (fire : ((j : P.I) → Outs (P.CI j).M V) → Wires P.X.M V → P.PR → Bool)
    (oc : (j : P.I) → Outs (P.CI j).M V) (w : Wires P.X.M V) (j : P.I) (cm : (P.CI j).M)
    (lp : Step P → Nat) (us : List (Step P)) (u : Nat) (G : Nat → Bool × V) (stp : Nat → Step P) (d : V)
    (keyf : Nat → Nat) (hk : ∀ t, lp (stp t) = keyf t)
    (hdec : Scan.incB (((List.range' 0 u).map keyf).reverse) = true)
    (hstp : ∀ t ∈ List.range' 0 u, stp t ∈ us)
    (hus : ∀ s ∈ us, s ∈ (List.range' 0 u).map stp)
    (hQ : Emit.AllL (fun t => ((G t).1 = true → On P fire oc w (stp t) ∧ Calls P (trS P oc w (stp t)) j cm) ∧
      (On P fire oc w (stp t) → ∀ x, (cm, x) ∈ proj j (trS P oc w (stp t)) → (G t).1 = true ∧ (G t).2 = x))
      (List.range' 0 u)) :
    ∀ s ∈ us, On P fire oc w s → ∀ x, (cm, x) ∈ proj j (trS P oc w s) →
      (∀ s' ∈ us, lp s < lp s' → On P fire oc w s' → ¬ Calls P (trS P oc w s') j cm) →
      Scan.pmux ((List.range' 0 u).map G) d = x := by
  intro s hs hon x hx hlater
  obtain ⟨t, ht, rfl⟩ := List.mem_map.mp (hus s hs)
  obtain ⟨h1, h2⟩ := (Emit.AllL.mem hQ t ht).2 hon x hx
  have hp := Scan.pairwise_of_incB _ hdec
  rw [List.pairwise_reverse, List.pairwise_map] at hp
  rw [← h2]
  refine pmux_last G (fun t => lp (stp t)) d _ (hp.imp (fun h => by rw [hk, hk]; exact h)) t ht h1 ?_
  intro a ha hlt
  cases e : (G a).1
  · rfl
  · obtain ⟨hon', hc'⟩ := (Emit.AllL.mem hQ a ha).1 e
    exact (hlater (stp a) (hstp a ha) hlt hon' hc').elim

/-- `argL_of_pmux` with the users given by numbers and the mux in their reverse order (decided
    without the parent's step type, linearly). -/
theorem argL_of_pmuxN (P : Parent V) (fire : ((j : P.I) → Outs (P.CI j).M V) → Wires P.X.M V → P.PR → Bool)
    (oc : (j : P.I) → Outs (P.CI j).M V) (w : Wires P.X.M V) (j : P.I) (cm : (P.CI j).M)
    (lp : Step P → Nat) (us : List (Step P)) (u : Nat) (G : Nat → Bool × V) (stp : Nat → Step P) (d : V)
    (keyf : Nat → Nat) (hk : ∀ t, lp (stp t) = keyf t)
    (hdec : Scan.incB (((List.range' 0 u).map keyf).reverse) = true)
    (ks : List Nat) (f : Nat → Step P) (idx : Nat → Nat) (hus_eq : us = ks.map f) (hstpf : ∀ t, stp t = f (idx t))
    (hperm : (List.range' 0 u).map idx = ks.reverse)
    (hQ : Emit.AllL (fun t => ((G t).1 = true → On P fire oc w (stp t) ∧ Calls P (trS P oc w (stp t)) j cm) ∧
      (On P fire oc w (stp t) → ∀ x, (cm, x) ∈ proj j (trS P oc w (stp t)) → (G t).1 = true ∧ (G t).2 = x))
      (List.range' 0 u)) :
    ∀ s ∈ us, On P fire oc w s → ∀ x, (cm, x) ∈ proj j (trS P oc w s) →
      (∀ s' ∈ us, lp s < lp s' → On P fire oc w s' → ¬ Calls P (trS P oc w s') j cm) →
      Scan.pmux ((List.range' 0 u).map G) d = x := by
  subst hus_eq
  refine argL_of_pmux P fire oc w j cm lp _ u G stp d keyf hk hdec (fun t ht => ?_) (fun s hs => ?_) hQ
  · rw [hstpf]
    have : idx t ∈ ks.reverse := by rw [← hperm]; exact List.mem_map_of_mem ht
    exact List.mem_map_of_mem (List.mem_reverse.mp this)
  · obtain ⟨k, hk', rfl⟩ := List.mem_map.mp hs
    have : k ∈ (List.range' 0 u).map idx := by rw [hperm]; exact List.mem_reverse.mpr hk'
    obtain ⟨t, ht, e⟩ := List.mem_map.mp this
    exact List.mem_map.mpr ⟨t, ht, by rw [hstpf, e]⟩

/-! ## The block lemma, from the annotations alone (proved once) -/

section Block
variable {M : Type} [DecidableEq M]

/-- Port `q` can observe calls of `m`: ordered with `m` either way, or in conflict with it. -/
def Obs (A : Ann M) (m q : M) : Bool := A.prec q m || A.prec m q || A.conf m q || A.conf q m

/-- A call between two calls of `m` that may follow one another is of a port CF with `m`. -/
theorem obs_of_follow (A : Ann M) (m : M) {a c b : M × V} (ha : a.1 = m) (hb : b.1 = m) (hc : c.1 ≠ m)
    (h1 : A.Follow a c) (h2 : A.Follow c b) : Obs A m c.1 = false := by
  obtain ⟨p1, c1, c2, -⟩ := h1
  obtain ⟨p2, -, -, -⟩ := h2
  subst ha
  have e1 := p1 (fun e => hc e.symm)
  have e2 := p2 (fun e => hc (e.trans hb))
  rw [hb] at e2
  simp [Obs, e1, e2, c1, c2]

/-- **The block lemma**: in a legal sequence, every call between two calls of `m` is of a port
    that cannot observe `m`; so the calls of `m` form one block for every port that can. -/
theorem block (A : Ann M) {σ : List (List (M × V))} (hL : Legal A σ) (m : M) {a c b : M × V}
    (ha : a.1 = m) (hb : b.1 = m) (hc : c.1 ≠ m) (h : [a, c, b] <+ σ.flatten) : Obs A m c.1 = false :=
  obs_of_follow A m ha hb hc (hL.order a c ((by simp : [a, c] <+ [a, c, b]).trans h))
    (hL.order c b ((by simp : [c, b] <+ [a, c, b]).trans h))

/-- One caller never calls a self-SBR port twice. -/
theorem block_group (A : Ann M) {σ : List (List (M × V))} (hL : Legal A σ) (m : M) (hs : A.sbr m m = true) :
    ∀ g ∈ σ, ∀ a b, [a, b] <+ g → a.1 = m → b.1 = m → False := by
  intro g hg a b hab ha hb
  have := (hL.group g hg a b hab).2.1
  rw [ha, hb, hs] at this; cases this

/-- Every call of `m` but the last removed. -/
def rbl (m : M) : List (M × V) → List (M × V)
  | [] => []
  | c :: l => if c.1 = m ∧ l.any (fun d => d.1 == m) = true then rbl m l else c :: rbl m l

theorem rbl_sublist (m : M) : ∀ (l : List (M × V)), rbl m l <+ l
  | [] => List.Sublist.slnil
  | c :: l => by
    unfold rbl
    split
    · exact (rbl_sublist m l).cons _
    · exact (rbl_sublist m l).cons_cons _

theorem mem_rbl (m : M) : ∀ (l : List (M × V)) (c : M × V), c ∈ l → c.1 ≠ m → c ∈ rbl m l
  | [], _, h, _ => by cases h
  | d :: l, c, h, hc => by
    unfold rbl
    rcases List.mem_cons.mp h with rfl | h'
    · rw [if_neg (fun h'' => hc h''.1)]; exact List.mem_cons_self ..
    · split
      · exact mem_rbl m l c h' hc
      · exact List.mem_cons_of_mem _ (mem_rbl m l c h' hc)

theorem rbl_noM (m : M) : ∀ (l : List (M × V)), l.any (fun d => d.1 == m) = false → rbl m l = l
  | [], _ => rfl
  | d :: l, h => by
    simp only [List.any_cons, Bool.or_eq_false_iff] at h
    unfold rbl
    rw [if_neg (fun h' => by rw [h.2] at h'; exact absurd h'.2 (by simp)), rbl_noM m l h.2]

theorem rbl_any (m : M) : ∀ (l : List (M × V)), (rbl m l).any (fun d => d.1 == m) = l.any (fun d => d.1 == m)
  | [] => rfl
  | d :: l => by
    unfold rbl
    split
    · rename_i h; rw [rbl_any m l, List.any_cons, h.2, Bool.or_true]
    · rw [List.any_cons, List.any_cons, rbl_any m l]

/-- The last call of every port survives (`m`'s last one too). -/
theorem lastArg_rbl (m : M) : ∀ (l : List (M × V)) (p : M), lastArg p (rbl m l) = lastArg p l
  | [], _ => rfl
  | d :: l, p => by
    unfold rbl
    split
    · rename_i h
      rw [lastArg_rbl m l p]
      show _ = (match lastArg p l with | some y => some y | none => if d.1 = p then some d.2 else none)
      cases hl : lastArg p l with
      | some y => rfl
      | none =>
        by_cases hp : d.1 = p
        · exfalso
          obtain ⟨e, he, he'⟩ := List.any_eq_true.mp h.2
          have : e.1 = p := by rw [beq_iff_eq] at he'; rw [he', ← h.1, hp]
          obtain ⟨y, hy⟩ := lastArg_isSome p l e.2 (by rw [← this]; exact he)
          rw [hl] at hy; cases hy
        · simp [hp]
    · show (match lastArg p (rbl m l) with | some y => some y | none => if d.1 = p then some d.2 else none) =
        (match lastArg p l with | some y => some y | none => if d.1 = p then some d.2 else none)
      rw [lastArg_rbl m l p]

/-- At most one call of `m` survives. -/
theorem rbl_one (m : M) : ∀ (l : List (M × V)) (a b : M × V), [a, b] <+ rbl m l → a.1 = m → b.1 = m → False
  | [], a, b, h, _, _ => by simp [rbl] at h
  | d :: l, a, b, h, ha, hb => by
    unfold rbl at h
    split at h
    · exact rbl_one m l a b h ha hb
    · rename_i hn
      rcases List.sublist_cons_iff.mp h with h' | ⟨r, e, h'⟩
      · exact rbl_one m l a b h' ha hb
      · simp only [List.cons.injEq] at e
        obtain ⟨rfl, rfl⟩ := e
        have hbm : b ∈ rbl m l := List.singleton_sublist.mp h'
        apply hn
        refine ⟨ha, List.any_eq_true.mpr ⟨b, (rbl_sublist m l).subset hbm, by simp [hb]⟩⟩

/-- Groups with every call of `m` removed but the last. -/
def rblG (m : M) : List (List (M × V)) → List (List (M × V))
  | [] => []
  | g :: σ => (if σ.flatten.any (fun d => d.1 == m) = true then g.filter (fun d => !(d.1 == m)) else rbl m g) :: rblG m σ

theorem rbl_append (m : M) : ∀ (g l : List (M × V)),
    rbl m (g ++ l) = (if l.any (fun d => d.1 == m) = true then g.filter (fun d => !(d.1 == m)) else rbl m g) ++ rbl m l
  | [], l => by split <;> rfl
  | d :: g, l => by
    have ih := rbl_append m g l
    rw [List.cons_append]
    show (if d.1 = m ∧ (g ++ l).any (fun d => d.1 == m) = true then rbl m (g ++ l) else d :: rbl m (g ++ l)) = _
    by_cases hl : l.any (fun d => d.1 == m) = true
    · rw [if_pos hl] at ih ⊢
      have hany : (g ++ l).any (fun d => d.1 == m) = true := by rw [List.any_append, hl, Bool.or_true]
      rw [ih, List.filter_cons]
      by_cases hd : d.1 = m
      · rw [if_pos ⟨hd, hany⟩, if_neg (by simp [hd])]
      · rw [if_neg (fun h => hd h.1), if_pos (by simp [hd]), List.cons_append]
    · rw [if_neg hl] at ih ⊢
      have hl' : l.any (fun d => d.1 == m) = false := Bool.eq_false_iff.mpr hl
      have hany : (g ++ l).any (fun d => d.1 == m) = g.any (fun d => d.1 == m) := by
        rw [List.any_append, hl', Bool.or_false]
      rw [hany, ih]
      show _ = (if d.1 = m ∧ g.any (fun d => d.1 == m) = true then rbl m g else d :: rbl m g) ++ rbl m l
      split <;> rfl

theorem rblG_flatten (m : M) : ∀ (σ : List (List (M × V))), (rblG m σ).flatten = rbl m σ.flatten
  | [] => rfl
  | g :: σ => by rw [rblG, List.flatten_cons, List.flatten_cons, rblG_flatten m σ, rbl_append]

theorem rblG_sub (m : M) : ∀ (σ : List (List (M × V))), ∀ g1 ∈ rblG m σ, ∃ g ∈ σ, g1 <+ g
  | [], _, h => by cases h
  | g :: σ, g1, h => by
    rcases List.mem_cons.mp h with rfl | h'
    · refine ⟨g, List.mem_cons_self .., ?_⟩
      split
      · exact List.filter_sublist
      · exact rbl_sublist m g
    · obtain ⟨g', hg', hs⟩ := rblG_sub m σ g1 h'
      exact ⟨g', List.mem_cons_of_mem _ hg', hs⟩

end Block

/-! ## Re-inserting the earlier calls: the law and the commutation of CF ports -/

section Insert
variable {X : Iface} (Mo : Module V X) (o : Outs X.M V) (m : X.M)

/-- The per-primitive law, expansion half: a call of `m` is any call followed by it. -/
def LawExp : Prop :=
  ∀ s a b s2, Mo.meth m b s (o.ret m) s2 → ∃ s1, Mo.meth m a s (o.ret m) s1 ∧ Mo.meth m b s1 (o.ret m) s2

/-- A port that cannot observe `m` commutes with it. -/
def CommCF : Prop :=
  ∀ q, Obs X.ann m q = false → q ≠ m → ∀ s y x s2,
    ((∃ s1, Mo.meth q y s (o.ret q) s1 ∧ Mo.meth m x s1 (o.ret m) s2) ↔
      (∃ s1, Mo.meth m x s (o.ret m) s1 ∧ Mo.meth q y s1 (o.ret q) s2))

theorem comm_left (hc : CommCF Mo o m) (x : V) :
    ∀ (B : List (X.M × V)), (∀ e ∈ B, Obs X.ann m e.1 = false ∧ e.1 ≠ m) →
      ∀ s s1 s2, ExecG Mo o s B s1 → Mo.meth m x s1 (o.ret m) s2 →
        ∃ s1', Mo.meth m x s (o.ret m) s1' ∧ ExecG Mo o s1' B s2
  | [], _, s, s1, s2, h, hm => by cases h; exact ⟨s2, hm, .nil _⟩
  | (q, y) :: B, hB, s, s1, s2, h, hm => by
    cases h with
    | cons hq hr =>
      rename_i t
      obtain ⟨t', hm', hB'⟩ := comm_left hc x B (fun e he => hB e (List.mem_cons_of_mem _ he)) t s1 s2 hr hm
      obtain ⟨h1, h2⟩ := hB (q, y) (List.mem_cons_self ..)
      obtain ⟨u, hu1, hu2⟩ := (hc q h1 h2 s y x t').mp ⟨t, hq, hm'⟩
      exact ⟨u, hu1, .cons hu2 hB'⟩

theorem comm_right (hc : CommCF Mo o m) (x : V) :
    ∀ (B : List (X.M × V)), (∀ e ∈ B, Obs X.ann m e.1 = false ∧ e.1 ≠ m) →
      ∀ s s1 s2, Mo.meth m x s (o.ret m) s1 → ExecG Mo o s1 B s2 →
        ∃ s1', ExecG Mo o s B s1' ∧ Mo.meth m x s1' (o.ret m) s2
  | [], _, s, s1, s2, hm, h => by cases h; exact ⟨s, .nil _, hm⟩
  | (q, y) :: B, hB, s, s1, s2, hm, h => by
    cases h with
    | cons hq hr =>
      rename_i t
      obtain ⟨h1, h2⟩ := hB (q, y) (List.mem_cons_self ..)
      obtain ⟨u, hu1, hu2⟩ := (hc q h1 h2 s y x t).mpr ⟨s1, hm, hq⟩
      obtain ⟨t', hB', hm'⟩ := comm_right hc x B (fun e he => hB e (List.mem_cons_of_mem _ he)) u t s2 hu2 hr
      exact ⟨t', .cons hu1 hB', hm'⟩

theorem split_first (l : List (X.M × V)) (h : l.any (fun d => d.1 == m) = true) :
    ∃ B d l', l = B ++ d :: l' ∧ d.1 = m ∧ ∀ e ∈ B, e.1 ≠ m := by
  induction l with
  | nil => simp at h
  | cons c l ih =>
    by_cases hc : c.1 = m
    · exact ⟨[], c, l, rfl, hc, fun e he => by cases he⟩
    · simp only [List.any_cons, Bool.or_eq_true, beq_iff_eq, hc, false_or] at h
      obtain ⟨B, d, l', e, hd, hB⟩ := ih h
      refine ⟨c :: B, d, l', by rw [e]; rfl, hd, fun e' he' => ?_⟩
      rcases List.mem_cons.mp he' with rfl | he''
      · exact hc
      · exact hB e' he''

/-- **The earlier calls of `m` come back**: in a sequence whose pairs may follow one another, a run
    with only `m`'s last call is a run with all of them (the law and the block lemma). -/
theorem execG_rbl (he : LawExp Mo o m) (hc : CommCF Mo o m) :
    ∀ (L : List (X.M × V)), (∀ c1 c2, [c1, c2] <+ L → X.ann.Follow c1 c2) →
      ∀ s t, ExecG Mo o s (rbl m L) t → ExecG Mo o s L t
  | [], _, s, t, h => h
  | c :: L, hf, s, t, h => by
    have hfL : ∀ c1 c2, [c1, c2] <+ L → X.ann.Follow c1 c2 := fun c1 c2 h' => hf c1 c2 (h'.cons _)
    unfold rbl at h
    split at h
    · rename_i hcm
      have ih := execG_rbl he hc L hfL s t h
      obtain ⟨B, ⟨d1, d2⟩, l', rfl, hd, hB⟩ := split_first m L hcm.2
      obtain ⟨c1, c2⟩ := c
      simp only at hd hcm
      obtain ⟨hc1, -⟩ := hcm
      subst d1
      subst c1
      have hBo : ∀ e ∈ B, Obs X.ann m e.1 = false ∧ e.1 ≠ m := by
        intro e heB
        refine ⟨obs_of_follow X.ann m (a := (m, c2)) (b := (m, d2)) rfl rfl (hB e heB) (hf (m, c2) e ?_)
          (hf e (m, d2) ?_), hB e heB⟩
        · exact List.cons_sublist_cons.mpr (List.singleton_sublist.mpr (List.mem_append_left _ heB))
        · exact (sub_pair_left heB).cons _
      obtain ⟨s1, h1, h2⟩ := execG_append Mo o B ((m, d2) :: l') ih
      cases h2 with
      | cons hd' hr =>
        rename_i s2
        obtain ⟨s1', hm1, hB1⟩ := comm_left Mo o m hc d2 B hBo s s1 s2 h1 hd'
        obtain ⟨s0, ha, hb⟩ := he s c2 d2 s1' hm1
        obtain ⟨s1'', hB2, hm2⟩ := comm_right Mo o m hc d2 B hBo s0 s1' s2 hb hB1
        exact .cons ha (execG_append' Mo o B _ hB2 (.cons hm2 hr))
    · cases h with
      | cons hc1 hr => exact .cons hc1 (execG_rbl he hc L hfL _ t hr)

end Insert

/-! ## From one call per cycle to last caller, generically -/

section Generic
variable {X : Iface}

/-- `Realizes` for the annotations `A` instead of the interface's. -/
def RealizesA (Mo : Module V X) (A : Ann X.M) : Prop :=
  ∀ (s : Mo.S) (σ : List (List (X.M × V))) (w : Wires X.M V) (o : Outs X.M V) (s' : Mo.S) (g : Mo.Sig),
    Mo.clean s = s → Legal A σ → Drives A w σ → Mo.hw s w o s' g → Mo.ok g →
    (∀ c ∈ σ.flatten, A.alwaysRdy c.1 = false → o.rdy c.1 = true) →
    ∃ (τ : List (List (X.M × V) ⊕ Mo.R)) (fin : Mo.S), groups τ = σ ∧ Exec Mo o s τ fin ∧ s' = Mo.clean fin ∧
      ∀ l, Mo.trc g l = Mo.log fin l

theorem execG_of_exec (Mo : Module V X) (o : Outs X.M V) (hR : Mo.R → False) :
    ∀ (s t : Mo.S) (τ : List (List (X.M × V) ⊕ Mo.R)), Exec Mo o s τ t → ExecG Mo o s (groups τ).flatten t := by
  intro s t τ h
  induction h with
  | nil s => exact .nil s
  | grp hg _ ih => exact execG_append' Mo o _ _ hg ih
  | @rule _ _ _ r _ _ _ _ => exact (hR r).elim

/-- **Last caller from one call per cycle**: a rule-free module realizing `m` called at most once a
    cycle realizes `m` self-SBR, given the law and that the ports CF with `m` commute with it. -/
theorem realizes_lastWins (Mo : Module V X) (hR : Mo.R → False) (m : X.M) (A1 : Ann X.M)
    (hpre : ∀ p q, A1.prec p q = X.ann.prec p q) (hconf : ∀ p q, ¬ (p = m ∧ q = m) → A1.conf p q = X.ann.conf p q)
    (hsbr1 : ∀ p q, A1.sbr p q = X.ann.sbr p q) (hsh : ∀ p, A1.share p = X.ann.share p)
    (hen : ∀ p, A1.hasEn p = X.ann.hasEn p) (halw : ∀ p, A1.alwaysEn p = X.ann.alwaysEn p)
    (hrdy : ∀ p, A1.alwaysRdy p = X.ann.alwaysRdy p)
    (hsbr : X.ann.sbr m m = true) (halwm : X.ann.alwaysEn m = false)
    (he : ∀ o, LawExp Mo o m) (hc : ∀ o, CommCF Mo o m) (h1 : RealizesA Mo A1) : Realizes Mo := by
  intro s σ w o s' g hcl hL hD hhw hok hrd
  let σ1 := rblG m σ
  have hfl : σ1.flatten = rbl m σ.flatten := rblG_flatten m σ
  have hsub : σ1.flatten <+ σ.flatten := hfl ▸ rbl_sublist m _
  have hfol1 : ∀ c1 c2, [c1, c2] <+ σ1.flatten → A1.Follow c1 c2 := by
    intro c1 c2 h
    have hf := hL.order c1 c2 (h.trans hsub)
    have hnm : ¬ (c1.1 = m ∧ c2.1 = m) := fun ⟨e1, e2⟩ => rbl_one m _ c1 c2 (hfl ▸ h) e1 e2
    obtain ⟨f1, f2, f3, f4⟩ := hf
    refine ⟨fun hne => by rw [hpre]; exact f1 hne, by rw [hconf _ _ hnm]; exact f2,
      by rw [hconf _ _ (fun ⟨a, b⟩ => hnm ⟨b, a⟩)]; exact f3, fun e hs => f4 e (by rw [← hsh]; exact hs)⟩
  have hL1 : Legal A1 σ1 := by
    refine ⟨hfol1, fun g1 hg1 c1 c2 h => ?_, fun p hp => ?_⟩
    · obtain ⟨g, hg, hs⟩ := rblG_sub m σ g1 hg1
      have hG := hL.group g hg c1 c2 (h.trans hs)
      have hnm : ¬ (c1.1 = m ∧ c2.1 = m) := fun ⟨e1, e2⟩ => block_group X.ann hL m hsbr g hg c1 c2 (h.trans hs) e1 e2
      obtain ⟨⟨f1, f2, f3, f4⟩, g1', g2'⟩ := hG
      exact ⟨⟨fun hne => by rw [hpre]; exact f1 hne, by rw [hconf _ _ hnm]; exact f2,
        by rw [hconf _ _ (fun ⟨a, b⟩ => hnm ⟨b, a⟩)]; exact f3, fun e hs' => f4 e (by rw [← hsh]; exact hs')⟩,
        by rw [hsbr1]; exact g1', by rw [hsbr1]; exact g2'⟩
    · rw [halw] at hp
      obtain ⟨x, hx⟩ := hL.always p hp
      have hpm : p ≠ m := fun e => by rw [e, halwm] at hp; cases hp
      exact ⟨x, hfl ▸ mem_rbl m _ (p, x) hx hpm⟩
  have hD1 : Drives A1 w σ1 := by
    refine ⟨fun p hp => ?_, fun p x hx => hD.arg p x (by rw [← lastArg_rbl m σ.flatten p, ← hfl]; exact hx)⟩
    rw [hen] at hp
    rw [hD.en p hp]
    constructor
    · rintro ⟨x, hx⟩
      by_cases hpm : p = m
      · subst hpm
        have hany : σ.flatten.any (fun d => d.1 == p) = true := List.any_eq_true.mpr ⟨(p, x), hx, by simp⟩
        rw [← rbl_any p, ← hfl] at hany
        obtain ⟨d, hd, hd'⟩ := List.any_eq_true.mp hany
        rw [beq_iff_eq] at hd'
        exact ⟨d.2, by rw [← hd']; exact hd⟩
      · exact ⟨x, hfl ▸ mem_rbl m _ (p, x) hx hpm⟩
    · rintro ⟨x, hx⟩
      exact ⟨x, hsub.subset hx⟩
  obtain ⟨τ1, fin, hg, hex, hs', hlog⟩ := h1 s σ1 w o s' g hcl hL1 hD1 hhw hok
    (fun c hc' h' => hrd c (hsub.subset hc') (by rw [← hrdy]; exact h'))
  have hE := execG_of_exec Mo o hR s fin τ1 hex
  have hE2 : ExecG Mo o s σ1.flatten fin := by rw [← hg]; exact hE
  rw [hfl] at hE2
  have hE' := execG_rbl Mo o m (he o) (hc o) σ.flatten hL.order s fin hE2
  exact ⟨σ.map .inl, fin, groups_map_inl σ, exec_map_inl Mo o σ s fin hE', hs', hlog⟩

end Generic


/-! ## The last-caller key and the per-primitive laws -/

section Laws
variable {M : Type} [DecidableEq M]

/-- **The last-caller key**, on a primitive's annotation (its `VModInfo`, not its Verilog name):
    `m` is self-SBR with an enable, and no port that observes `m` after it shares a caller with it. -/
def lcKey (A : Ann M) (ports : List M) (m : M) : Bool :=
  A.sbr m m && A.hasEn m && !A.conf m m && !A.share m &&
  ports.all (fun q => q == m || !A.prec m q || A.sbr m q)

/-- The law `m(a); m(b) = m(b)` on a module's one-at-a-time step. -/
def LastWins {X : Iface} (Mo : Module V X) (m : X.M) : Prop :=
  ∀ s a b v s2, (∃ s1, Mo.meth m a s v s1 ∧ Mo.meth m b s1 v s2) ↔ Mo.meth m b s v s2

theorem lawExp_of_lastWins {X : Iface} (Mo : Module V X) (m : X.M) (h : LastWins Mo m) (o : Outs X.M V) :
    LawExp Mo o m := fun s a b s2 hb => (h s a b (o.ret m) s2).mpr hb |>.imp fun _ h' => h'

end Laws

/-- RegN, RegUN, RegA (`CReg 1`) and every CReg port's `write`: last wins. -/
theorem creg_lastWins (n : Nat) (k : Fin n) : LastWins (CReg (V := V) n) (.wr k) :=
  fun s a b v s2 => ⟨fun ⟨_, _, h⟩ => h, fun h => ⟨a, rfl, h⟩⟩

theorem reg_key : lcKey (cregAnn 1) [.rd 0, .wr 0] (.wr 0) = true := by decide
theorem creg5_key (k : Fin 5) : lcKey (cregAnn 5) ((List.finRange 5).map .rd ++ (List.finRange 5).map .wr) (.wr k) = true := by
  revert k; decide

/-- `mkRWireSBR`'s `wset`: the last value is read (`WGET = WVAL`; `whas` is the enables' OR). -/
theorem rwire_lastWins (ofBool : Bool → V) : LastWins (RWire ofBool true true) .wset :=
  fun s a b v s2 => ⟨fun ⟨_, _, h⟩ => h, fun h => ⟨some a, rfl, h⟩⟩

theorem rwireSBR_key : lcKey (rwireAnn true true) [.wset, .wget, .whas] .wset = true := by decide
/-- The Unsafe variants (`wset SB` the reads) are outside the key (decision: not covered). -/
theorem unsafeRWireSBR_not_key : lcKey (rwireAnn false true) [.wset, .wget, .whas] .wset = false := by decide
theorem unsafeRWire0SBR_not_key : lcKey (rwire0Ann false true) [.wset, .whas] .wset = false := by decide

/-- `mkPulseWireOR` (argument-less): `send; send = send`, and `whas` is the OR of the sends. -/
theorem pulseWireOR_lastWins (ofBool : Bool → V) : LastWins (RWire0 ofBool true true) .wset :=
  fun s a b v s2 => ⟨fun ⟨_, _, h⟩ => h, fun h => ⟨some a, rfl, h⟩⟩

theorem pulseWireOR_key : lcKey (rwire0Ann true true) [.wset, .whas] .wset = true := by decide

/-- FIFO `clear` (argument-less): idempotent. -/
theorem fifo2_clear_lastWins (ofBool : Bool → V) : LastWins (FIFO2 ofBool) .clear := by
  intro s a b v s2
  constructor
  · rintro ⟨s1, ⟨h1, h2, h3, h4⟩, ⟨g1, g2, g3, g4⟩⟩
    exact ⟨g1, g2, g3.trans h3, g4.trans h4⟩
  · rintro ⟨g1, g2, g3, g4⟩
    exact ⟨s2, ⟨g1, g2, g3, g4⟩, ⟨g1, g2, rfl, rfl⟩⟩

theorem fifo2_clear_key :
    lcKey fifoAnn [.enq, .deq, .first, .notEmpty, .notFull, .i_notEmpty, .i_notFull, .clear] .clear = true := by
  decide

/-- FIFO `i_notEmpty`/`i_notFull` are CF with `clear` and read the start-of-cycle status: they
    commute with it. -/
theorem fifo2_clear_comm (ofBool : Bool → V) (o : Outs FifoM V) : CommCF (FIFO2 ofBool) o .clear := by
  intro q hq hne s y x s2
  cases q <;> simp [Obs, fifoAnn] at hq hne
  · constructor
    · rintro ⟨s1, ⟨hv, rfl⟩, h⟩
      exact ⟨s2, h, by rw [h.2.2.1]; exact hv, rfl⟩
    · rintro ⟨s1, h, hv, rfl⟩
      exact ⟨s, ⟨by rw [← h.2.2.1]; exact hv, rfl⟩, h⟩
  · constructor
    · rintro ⟨s1, ⟨hv, rfl⟩, h⟩
      exact ⟨s2, h, by rw [h.2.2.2]; exact hv, rfl⟩
    · rintro ⟨s1, h, hv, rfl⟩
      exact ⟨s, ⟨by rw [← h.2.2.2]; exact hv, rfl⟩, h⟩

/-! ### ConfigRegN, ConfigRegUN, ConfigRegA: `read` returns the cycle's start value -/

inductive CfgM where
  | read | write
  deriving DecidableEq

/-- `ConfigReg.bs`'s schedule: `read CF (read, write)`, `write SBR write`. -/
def cfgSched : List (List CfgM × Rel × List CfgM) := [([.read], .CF, [.read, .write]), ([.write], .SBR, [.write])]

def cfgAnn : Ann CfgM :=
  Ann.ofSched cfgSched (fun m => m == .read) (fun m => m == .write) (fun _ => false) (fun _ => true)

abbrev Cfg.X : Iface := ⟨CfgM, inferInstance, cfgAnn⟩

/-- The model: the current value and the cycle's start value; `ConfigRegN.v` is `RegN.v`. -/
@[reducible] def CfgReg : Module V Cfg.X where
  S := V × V
  R := Empty
  meth := fun (m : CfgM) x s v s' => match m with
    | .read => v = s.2 ∧ s' = s
    | .write => s' = (x, s.2)
  rule := fun r => r.elim
  clean s := (s.1, s.1)
  clean_idem _ := rfl
  Sig := Unit
  hw s w o s' _ := o.ret .read = s.1 ∧ s' = (if w.en .write then (w.arg .write, w.arg .write) else (s.1, s.1))
  ok _ := True

theorem cfg_key : lcKey cfgAnn [.read, .write] .write = true := by decide

theorem cfg_lastWins : LastWins (CfgReg (V := V)) .write := by
  intro s a b v s2
  refine ⟨fun ⟨s1, h1, h2⟩ => ?_, fun h => ⟨(a, s.2), rfl, h⟩⟩
  show s2 = (b, s.2)
  rw [h2, h1]

/-- `read` is CF with `write` and returns the start value: it commutes with `write`. -/
theorem cfg_comm (o : Outs CfgM V) : CommCF (CfgReg (V := V)) o .write := by
  intro q hq hne s y x s2
  cases q
  · constructor
    · rintro ⟨s1, ⟨hv, rfl⟩, rfl⟩
      exact ⟨(x, s1.2), rfl, hv, rfl⟩
    · rintro ⟨s1, rfl, hv, rfl⟩
      exact ⟨s, ⟨hv, rfl⟩, rfl⟩
  · exact absurd rfl hne

/-- One write a cycle (`write C write`): the single-call contract. -/
def cfgAnn1 : Ann CfgM := { cfgAnn with conf := fun p q => cfgAnn.conf p q || (p == .write && q == .write) }

theorem execG_cfg_reads (o : Outs CfgM V) (s : V × V) (ho : o.ret .read = s.2) :
    ∀ (L : List (CfgM × V)), (∀ c ∈ L, c.1 = .read) → ExecG (CfgReg (V := V)) o s L s
  | [], _ => .nil _
  | (m, x) :: L, h => by
    have hm : m = .read := h _ (List.mem_cons_self ..)
    subst hm
    exact .cons ⟨ho, rfl⟩ (execG_cfg_reads o s ho L (fun c hc => h c (List.mem_cons_of_mem _ hc)))

theorem cfg_realizes1 : RealizesA (CfgReg (V := V)) cfgAnn1 := by
  intro s σ w o s' g hc hL hD hhw _ _
  obtain ⟨hret, hs'⟩ := hhw
  have hs : s.2 = s.1 := by
    have := congrArg Prod.snd hc; simpa using this.symm
  have hrd : o.ret .read = s.2 := by rw [hret, hs]
  -- at most one write
  have hone : ∀ a b, [a, b] <+ σ.flatten → a.1 = .write → b.1 = .write → False := by
    intro a b h ha hb
    have := (hL.order a b h).2.1
    rw [ha, hb] at this
    simp [cfgAnn1, cfgAnn, Ann.ofSched, schedRel, cfgSched] at this
  refine ⟨σ.map .inl, ?_, groups_map_inl σ, exec_map_inl _ o σ s _ ?_, ?_, fun _ => rfl⟩
  · exact if (σ.flatten.any (fun c => c.1 == .write)) then ((lastArg .write σ.flatten).getD s.1, s.2) else s
  · by_cases hw : σ.flatten.any (fun c => c.1 == .write) = true
    · rw [if_pos hw]
      obtain ⟨y, hy⟩ : ∃ y, lastArg .write σ.flatten = some y := by
        obtain ⟨c, hc', hc''⟩ := List.any_eq_true.mp hw
        rw [beq_iff_eq] at hc''
        exact lastArg_isSome CfgM.write σ.flatten c.2 (by rw [← hc'']; exact hc')
      obtain ⟨A, B, e, hB⟩ := lastArg_split _ _ y hy
      rw [hy, Option.getD_some, e]
      have hA : ∀ c ∈ A, c.1 = .read := by
        intro c hcA
        cases hc1 : c.1
        · rfl
        · exact (hone c (.write, y) (by rw [e]; exact sub_pair_left hcA) hc1 rfl).elim
      have hB' : ∀ c ∈ B, c.1 = .read := by
        intro c hcB
        cases hc1 : c.1
        · rfl
        · exact absurd hc1 (hB c hcB)
      refine execG_append' _ o A _ (execG_cfg_reads o s hrd A hA) (.cons rfl ?_)
      exact execG_cfg_reads o (y, s.2) hrd B hB'
    · rw [if_neg hw]
      apply execG_cfg_reads o s hrd
      intro c hc'
      cases hc1 : c.1
      · rfl
      · exact absurd (List.any_eq_true.mpr ⟨c, hc', by simp [hc1]⟩) hw
  · rw [hs']
    by_cases hw : σ.flatten.any (fun c => c.1 == .write) = true
    · rw [if_pos hw]
      obtain ⟨c, hc', hc''⟩ := List.any_eq_true.mp hw
      rw [beq_iff_eq] at hc''
      have hen : w.en .write = true := (hD.en .write rfl).mpr ⟨c.2, by rw [← hc'']; exact hc'⟩
      obtain ⟨y, hy⟩ := lastArg_isSome CfgM.write σ.flatten c.2 (by rw [← hc'']; exact hc')
      rw [if_pos hen, hD.arg _ y hy, hy]
      rfl
    · rw [if_neg hw]
      have hen : w.en .write = false := by
        cases h' : w.en .write
        · rfl
        · obtain ⟨x, hx⟩ := (hD.en .write rfl).mp h'
          exact absurd (List.any_eq_true.mpr ⟨_, hx, by simp⟩) hw
      rw [if_neg (by simp [hen])]

/-- **ConfigRegN realizes its contract with `write SBR write`**, by last caller from one write a
    cycle: the block lemma, the law and the commutation of `read`. -/
theorem cfg_realizes : Realizes (CfgReg (V := V)) :=
  realizes_lastWins _ (fun r => r.elim) .write cfgAnn1 (fun _ _ => rfl)
    (fun p q h => by cases p <;> cases q <;> simp_all [cfgAnn1]) (fun _ _ => rfl) (fun _ => rfl)
    (fun _ => rfl) (fun _ => rfl) (fun _ => rfl) (by decide) rfl
    (fun o => lawExp_of_lastWins _ _ cfg_lastWins o) cfg_comm cfg_realizes1

theorem cfg_causal : Causal (CfgReg (V := V)) :=
  ⟨fun s _ => ⟨fun _ => true, fun _ => s.1⟩, fun _ _ _ _ _ => ⟨rfl, rfl⟩,
    fun s w _ => ⟨_, (), rfl, rfl⟩⟩


/-! ### Counter.bs's Counter under the corrected annotation (bug counter): `addA C addA`,
`addB C addB`, `setC SB` the adds, `value SB` every action, `setF SBR` everything and itself. -/

inductive CtrM where
  | value | addA | addB | setC | setF
  deriving DecidableEq

def ctrAnn : Ann CtrM where
  prec p q := match p, q with
    | .value, .addA | .value, .addB | .value, .setC | .value, .setF => true
    | .setC, .addA | .setC, .addB => true
    | .addA, .setF | .addB, .setF | .setC, .setF => true
    | _, _ => false
  sbr p q := match p, q with
    | .addA, .setF | .addB, .setF | .setC, .setF | .setF, .setF => true
    | _, _ => false
  conf p q := match p, q with
    | .addA, .addA | .addB, .addB => true
    | _, _ => false
  share m := m == .value
  hasEn m := m != .value
  alwaysEn _ := false
  alwaysRdy _ := true

abbrev Ctr.X : Iface := ⟨CtrM, inferInstance, ctrAnn⟩

/-- An optional addition. -/
def optAdd (add : V → V → V) : Option V → V → V
  | some a, c => add c a
  | none, c => c

/-- `Counter.v`'s next value: `SETF ? DATA_F : (SETC ? DATA_C : q) + (ADDA ? DATA_A : 0) + (ADDB ? DATA_B : 0)`. -/
def cnext (add : V → V → V) (q : V) (w : Wires CtrM V) : V :=
  if w.en .setF then w.arg .setF else
    (if w.en .addB then (fun c => add c (w.arg .addB)) else id)
      ((if w.en .addA then (fun c => add c (w.arg .addA)) else id) (if w.en .setC then w.arg .setC else q))

/-- The model: the current value and the cycle's start value (`value` reads `Q_OUT`). -/
@[reducible] def Counter (add : V → V → V) : Module V Ctr.X where
  S := V × V
  R := Empty
  meth := fun (m : CtrM) x s v s' => match m with
    | .value => v = s.2 ∧ s' = s
    | .addA => s' = (add s.1 x, s.2)
    | .addB => s' = (add s.1 x, s.2)
    | .setC => s' = (x, s.2)
    | .setF => s' = (x, s.2)
  rule := fun r => r.elim
  clean s := (s.1, s.1)
  clean_idem _ := rfl
  Sig := Unit
  hw s w o s' _ := o.ret .value = s.1 ∧ s' = (cnext add s.1 w, cnext add s.1 w)
  ok _ := True

def cstep (add : V → V → V) (c : V) : CtrM × V → V
  | (.value, _) => c
  | (.addA, x) => add c x
  | (.addB, x) => add c x
  | (.setC, x) => x
  | (.setF, x) => x

def cfold (add : V → V → V) : V → List (CtrM × V) → V
  | c, [] => c
  | c, d :: l => cfold add (cstep add c d) l

theorem execG_ctr (add : V → V → V) (o : Outs CtrM V) (st : V) (ho : o.ret .value = st) :
    ∀ (L : List (CtrM × V)) (c : V), ExecG (Counter add) o (c, st) L (cfold add c L, st)
  | [], c => .nil _
  | (m, x) :: L, c => by
    refine .cons (s1 := (cstep add c (m, x), st)) ?_ (execG_ctr add o st ho L _)
    cases m <;> first | exact ⟨ho, rfl⟩ | rfl

theorem lastArg_none_of {M : Type} [DecidableEq M] (p : M) :
    ∀ (l : List (M × V)), (∀ y, (p, y) ∉ l) → lastArg p l = none
  | [], _ => rfl
  | (m, x) :: l, h => by
    simp only [lastArg, lastArg_none_of p l (fun y hy => h y (List.mem_cons_of_mem _ hy))]
    rw [if_neg (fun e => h x (by rw [← e]; exact List.mem_cons_self ..))]

/-- **The fold without `setF`**: the last `setC` (or the start), then the adds in either order. -/
theorem ctr_fold (add : V → V → V) (hc : ∀ c a b, add (add c a) b = add (add c b) a) :
    ∀ (L : List (CtrM × V)) (c : V), (∀ c1 c2, [c1, c2] <+ L → ctrAnn.Follow c1 c2) → (∀ x, (CtrM.setF, x) ∉ L) →
      cfold add c L = optAdd add (lastArg .addB L) (optAdd add (lastArg .addA L) ((lastArg .setC L).getD c))
  | [], c, _, _ => rfl
  | (m, x) :: l, c, hf, hF => by
    have hfl : ∀ c1 c2, [c1, c2] <+ l → ctrAnn.Follow c1 c2 := fun c1 c2 h => hf c1 c2 (h.cons _)
    have hFl : ∀ y, (CtrM.setF, y) ∉ l := fun y hy => hF y (List.mem_cons_of_mem _ hy)
    have hhead : ∀ d ∈ l, ctrAnn.Follow (m, x) d := fun d hd =>
      hf _ _ (List.cons_sublist_cons.mpr (List.singleton_sublist.mpr hd))
    have ih := ctr_fold add hc l
    cases m with
    | value =>
      simp only [cfold, cstep, lastArg]
      rw [ih c hfl hFl]
      cases lastArg CtrM.addB l <;> cases lastArg CtrM.addA l <;> cases lastArg CtrM.setC l <;> rfl
    | setC =>
      simp only [cfold, cstep, lastArg]
      rw [ih x hfl hFl]
      cases lastArg CtrM.addB l <;> cases lastArg CtrM.addA l <;> cases lastArg CtrM.setC l <;> rfl
    | setF => exact absurd (List.mem_cons_self ..) (hF x)
    | addA =>
      have hC : lastArg CtrM.setC l = none := lastArg_none_of _ l (fun y hy => by
        have := (hhead _ hy).1 (by simp); simp [ctrAnn] at this)
      have hA : lastArg CtrM.addA l = none := lastArg_none_of _ l (fun y hy => by
        have := (hhead _ hy).2.1; simp [ctrAnn] at this)
      simp only [cfold, cstep, lastArg, hC, hA]
      rw [ih (add c x) hfl hFl, hC, hA]
      cases lastArg CtrM.addB l <;> rfl
    | addB =>
      have hC : lastArg CtrM.setC l = none := lastArg_none_of _ l (fun y hy => by
        have := (hhead _ hy).1 (by simp); simp [ctrAnn] at this)
      have hB : lastArg CtrM.addB l = none := lastArg_none_of _ l (fun y hy => by
        have := (hhead _ hy).2.1; simp [ctrAnn] at this)
      simp only [cfold, cstep, lastArg, hC, hB]
      rw [ih (add c x) hfl hFl, hC, hB]
      cases lastArg CtrM.addA l with
      | none => simp [optAdd]
      | some a => simp [optAdd]; exact hc c x a

/-- `en`/`arg` of a driven port as its last call. -/
theorem drives_opt {M : Type} [DecidableEq M] {A : Ann M} {w : Wires M V} {σ : List (List (M × V))}
    (hD : Drives A w σ) (p : M) (hp : A.hasEn p = true) (f : V → V → V) (z : V) :
    (if w.en p = true then f (w.arg p) z else z) = (match lastArg p σ.flatten with | some a => f a z | none => z) := by
  cases h : lastArg p σ.flatten with
  | none =>
    have : w.en p = false := by
      cases h' : w.en p
      · rfl
      · obtain ⟨x, hx⟩ := (hD.en p hp).mp h'
        obtain ⟨y, hy⟩ := lastArg_isSome p _ x hx
        rw [h] at hy; cases hy
    simp [this]
  | some a =>
    have hen : w.en p = true := (hD.en p hp).mpr ⟨a, lastArg_mem p _ a h⟩
    rw [if_pos hen, hD.arg p a h]

theorem drives_optAdd {M : Type} [DecidableEq M] {A : Ann M} {w : Wires M V} {σ : List (List (M × V))}
    (hD : Drives A w σ) (p : M) (hp : A.hasEn p = true) (add : V → V → V) (z : V) :
    (if w.en p = true then (fun c => add c (w.arg p)) else id) z = optAdd add (lastArg p σ.flatten) z := by
  cases h : lastArg p σ.flatten with
  | none =>
    have : w.en p = false := by
      cases h' : w.en p
      · rfl
      · obtain ⟨x, hx⟩ := (hD.en p hp).mp h'
        obtain ⟨y, hy⟩ := lastArg_isSome p _ x hx
        rw [h] at hy; cases hy
    simp [this, optAdd]
  | some a =>
    have hen : w.en p = true := (hD.en p hp).mpr ⟨a, lastArg_mem p _ a h⟩
    rw [if_pos hen, hD.arg p a h]; rfl

/-- **Counter realizes its contract under the corrected annotation**, for an addition whose
    operands commute on the right (`Bit n` addition). -/
theorem counter_realizes (add : V → V → V) (hc : ∀ c a b, add (add c a) b = add (add c b) a) :
    Realizes (Counter add) := by
  intro s σ w o s' g hcl hL hD hhw _ _
  obtain ⟨hret, hs'⟩ := hhw
  have hs : s.2 = s.1 := by have := congrArg Prod.snd hcl; simpa using this.symm
  refine ⟨σ.map .inl, (cfold add s.1 σ.flatten, s.2), groups_map_inl σ,
    exec_map_inl _ o σ s _ (by
      have := execG_ctr add o s.2 (by rw [hret, hs]) σ.flatten s.1
      exact this), ?_, fun _ => rfl⟩
  rw [hs']
  show (cnext add s.1 w, cnext add s.1 w) = (cfold add s.1 σ.flatten, cfold add s.1 σ.flatten)
  suffices h : cnext add s.1 w = cfold add s.1 σ.flatten by rw [h]
  cases hF : lastArg CtrM.setF σ.flatten with
  | some f =>
    have hen : w.en .setF = true := (hD.en .setF rfl).mpr ⟨f, lastArg_mem _ _ f hF⟩
    obtain ⟨L1, L2, e, hL2⟩ := lastArg_split _ _ f hF
    have hnil : L2 = [] := by
      cases L2 with
      | nil => rfl
      | cons d L2' =>
        exfalso
        have hfol := hL.order (CtrM.setF, f) d (by rw [e]; exact sub_pair_right (List.mem_cons_self ..))
        have hd := hL2 d (List.mem_cons_self ..)
        have := hfol.1 (fun h => hd h.symm)
        cases hd1 : d.1 <;> simp_all [ctrAnn]
    unfold cnext
    rw [if_pos hen, hD.arg _ f hF, e, hnil]
    have : ∀ (l : List (CtrM × V)) (c : V), cfold add c (l ++ [(CtrM.setF, f)]) = f := by
      intro l; induction l with
      | nil => intro c; rfl
      | cons d l ih => intro c; exact ih _
    exact (this L1 s.1).symm
  | none =>
    have hnF : ∀ x, (CtrM.setF, x) ∉ σ.flatten := lastArg_none _ _ hF
    have hen : w.en .setF = false := by
      cases h' : w.en .setF
      · rfl
      · obtain ⟨x, hx⟩ := (hD.en .setF rfl).mp h'; exact absurd hx (hnF x)
    rw [ctr_fold add hc σ.flatten s.1 hL.order hnF]
    unfold cnext
    rw [if_neg (by simp [hen])]
    have e3 : (if w.en .setC = true then w.arg .setC else s.1) = (lastArg CtrM.setC σ.flatten).getD s.1 := by
      have := drives_opt hD .setC rfl (fun a _ => a) s.1
      rw [this]; cases lastArg CtrM.setC σ.flatten <;> rfl
    rw [e3, drives_optAdd hD .addA rfl add, drives_optAdd hD .addB rfl add]

theorem counter_causal (add : V → V → V) : Causal (Counter add) :=
  ⟨fun s _ => ⟨fun _ => true, fun _ => s.1⟩, fun _ _ _ _ _ => ⟨rfl, rfl⟩, fun s w _ => ⟨_, (), rfl, rfl⟩⟩

theorem counter_setF_lastWins (add : V → V → V) : LastWins (Counter add) .setF := by
  intro s a b v s2
  refine ⟨fun ⟨s1, h1, h2⟩ => ?_, fun h => ⟨(a, s.2), rfl, h⟩⟩
  show s2 = (b, s.2)
  rw [h2, h1]

theorem counter_setF_key : lcKey ctrAnn [.value, .addA, .addB, .setC, .setF] .setF = true := by decide
/-- `addA` is C with itself in the model: it is not a last-caller port. -/
theorem counter_addA_not_key : lcKey ctrAnn [.value, .addA, .addB, .setC, .setF] .addA = false := by decide

end LC
end Modular

#print axioms Modular.LC.lcMR_of_scan
#print axioms Modular.LC.lcRR_of_scan
#print axioms Modular.LC.lcMM_of_scan
#print axioms Modular.LC.argL_of_pmux
#print axioms Modular.LC.block
#print axioms Modular.LC.execG_rbl
#print axioms Modular.LC.realizes_lastWins
#print axioms Modular.LC.cfg_realizes
#print axioms Modular.LC.cfg_causal
#print axioms Modular.LC.fifo2_clear_comm
#print axioms Modular.LC.counter_realizes
#print axioms Modular.LC.counter_causal
