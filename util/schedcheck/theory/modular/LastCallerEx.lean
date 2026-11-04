/- LastCallerEx.lean: last caller's non-vacuity and negatives (LastCaller.lean). -/
import LastCaller

open scoped List

namespace Modular
namespace LCEx
open LC

/-! ## Rule 0 reads `y` and writes `x := 1`; rule 1 writes `y := 5` and `x := 2`. `y`'s `read SB write`
forces rule 0 first, so rule 1's argument must reach `x` (bsc's priority mux, last caller first). -/

def r0 : Prog (Fin 2) (fun _ => CRegM 1) Nat := .call 1 Reg.read 0 (fun _ => .call 0 Reg.write 1 (fun _ => .ret 0))
def r1 : Prog (Fin 2) (fun _ => CRegM 1) Nat := .call 1 Reg.write 5 (fun _ => .call 0 Reg.write 2 (fun _ => .ret 0))

def noAnn : Ann Empty := ⟨fun _ _ => false, fun _ _ => false, fun _ _ => false, fun _ => false, fun _ => false,
  fun _ => false, fun _ => false, fun _ _ => false, fun _ _ => false, fun _ _ => false⟩

/-- The parent, with `x`'s argument mux giving `xarg`. -/
abbrev P (xarg : Nat) : Parent Nat where
  I := Fin 2
  decI := inferInstance
  kids := [0, 1]
  kids_nodup := by decide
  kids_all := by decide
  CI _ := Reg.X
  X := ⟨Empty, inferInstance, noAnn⟩
  PR := Fin 2
  body m _ := m.elim
  rbody r := if r = 0 then r0 else r1
  comb _ _ := (fun j => if j = 0 then regW' true xarg else regW' true 5, ⟨fun e => e.elim, fun e => e.elim⟩)
  asm _ _ := True
  onehot _ _ := True
where regW' (en : Bool) (d : Nat) : Wires (CRegM 1) Nat :=
  ⟨fun m => match m with | .wr _ => en | .rd _ => false, fun m => match m with | .wr _ => d | .rd _ => 0⟩

def oc0 : (j : Fin 2) → Outs (CRegM 1) Nat := fun _ => ⟨fun _ => true, fun _ => 0⟩

theorem tr0 (xarg : Nat) (oc : (j : (P xarg).I) → Outs ((P xarg).CI j).M Nat) :
    stepTrace (P xarg) oc (.inr 0) = [⟨1, (Reg.read, 0)⟩, ⟨0, (Reg.write, 1)⟩] := rfl
theorem tr1 (xarg : Nat) (oc : (j : (P xarg).I) → Outs ((P xarg).CI j).M Nat) :
    stepTrace (P xarg) oc (.inr 1) = [⟨1, (Reg.write, 5)⟩, ⟨0, (Reg.write, 2)⟩] := rfl

theorem fin2 (r : Fin 2) : r = 0 ∨ r = 1 := by
  obtain ⟨k, hk⟩ := r
  match k, hk with
  | 0, _ => exact .inl rfl
  | 1, _ => exact .inr rfl

/-- **A first-caller-wins mux fails the obligation**: `x` carrying rule 0's argument cannot agree
    with any legal order, since `y` forces rule 0 first. -/
theorem firstCaller_not_obl : ¬ Obl (P 1) := by
  intro h
  have hL : Legal (P 1).X.ann ([] : List (List ((P 1).X.M × Nat))) :=
    ⟨fun _ _ h => (by simp at h), fun _ h => (by cases h), fun m => m.elim⟩
  have hD : Drives (P 1).X.ann ⟨fun e => e.elim, fun e => e.elim⟩ ([] : List (List ((P 1).X.M × Nat))) :=
    ⟨fun m => m.elim, fun m => m.elim⟩
  obtain ⟨-, τP, hg, -, hj⟩ := h oc0 ⟨fun e => e.elim, fun e => e.elim⟩ [] trivial
    (fun _ => aliasOK_of_none _ _ _ (fun _ _ => rfl) (fun _ _ => rfl)) hL hD (fun c hc => by simp at hc)
  obtain ⟨ρ, rfl⟩ := groups_nil_rules τP hg
  -- rule 1 is in the run (`y`'s write is enabled)
  obtain ⟨vy, hvy⟩ := ((hj 1).2.1.en Reg.write rfl).mp rfl
  have h1 : (1 : Fin 2) ∈ ρ := by
    unfold sigmaJ at hvy
    obtain ⟨l, hl, hx⟩ := List.mem_flatten.mp hvy
    obtain ⟨e, he, rfl⟩ := List.mem_map.mp hl
    obtain ⟨r, hr, rfl⟩ := List.mem_map.mp he
    rcases fin2 r with rfl | rfl
    · rw [tr0] at hx; simp [proj] at hx
    · exact hr
  -- the run's last write of `x` carries 1, so it is rule 0's and no later step writes `x`
  obtain ⟨vx, hvx⟩ := ((hj 0).2.1.en Reg.write rfl).mp rfl
  obtain ⟨y, hy⟩ := lastArg_isSome Reg.write _ vx hvx
  have hy1 : y = 1 := ((hj 0).2.1.arg Reg.write y hy).symm
  subst hy1
  have hs : sigmaJ (P 1) oc0 (ρ.map .inr) 0 = (ρ.map .inr).map (fun e => proj 0 (stepTrace (P 1) oc0 e)) := rfl
  rw [hs] at hy
  obtain ⟨pre, e, post, he, hxe, hpost⟩ := lastArg_flatten_split Reg.write _ _ 1 hy
  obtain ⟨r, -, rfl⟩ : ∃ r ∈ ρ, Sum.inr r = e := List.mem_map.mp (by rw [he]; simp)
  rcases fin2 r with rfl | rfl
  · -- rule 1 lies before rule 0 in the run
    have h1pre : (Sum.inr 1 : List (Empty × Nat) ⊕ Fin 2) ∈ pre := by
      have hm : (Sum.inr 1 : List (Empty × Nat) ⊕ Fin 2) ∈ ρ.map .inr := List.mem_map_of_mem h1
      rw [he] at hm
      rcases List.mem_append.mp hm with h' | h'
      · exact h'
      · rcases List.mem_cons.mp h' with h'' | h''
        · exact absurd h'' (by decide)
        · exact absurd (by rw [tr1]; simp [proj]) (hpost _ h'' 2)
    have hsub : [Sum.inr 1, Sum.inr 0] <+ ρ.map (Sum.inr (α := List (Empty × Nat))) := by
      rw [he]; exact sub_pair_left h1pre
    have hsub2 : [(Reg.write, 5), (Reg.read, 0)] <+ (sigmaJ (P 1) oc0 (ρ.map .inr) 1).flatten := by
      unfold sigmaJ
      exact pair_flatten_map (fun e => proj 1 (stepTrace (P 1) oc0 e)) hsub (by rw [tr1]; simp [proj])
        (by rw [tr0]; simp [proj])
    have hfy := (hj 1).1.order (Reg.write, 5) (Reg.read, 0) hsub2
    exact absurd (hfy.1 (by decide)) (by decide)
  · rw [tr1] at hxe; simp [proj] at hxe


theorem fin1 (i : Fin 1) : i = 0 := Fin.ext (by have := i.isLt; omega)

theorem flat_nil {α β : Type} : ∀ (σ : List α), (σ.map (fun _ => ([] : List β))).flatten = []
  | [] => rfl
  | _ :: σ => flat_nil σ

/-- **Last caller passes** in the same design: `x` carrying rule 1's argument (bsc's priority mux,
    the later caller first) meets the obligation, with the forced order. -/
theorem lastCaller_obl : Obl (P 2) := by
  intro oc w σ _ _ hL hD _
  have hσ : ∀ g ∈ σ, g = [] := fun g _ => match g with | [] => rfl | c :: _ => c.1.elim
  refine ⟨trivial, σ.map .inl ++ [.inr 0, .inr 1], ?_, ?_, fun j => ?_⟩
  · rw [groups_append, groups_map_inl]; simp [groups]
  · intro e he
    rcases List.mem_append.mp he with h' | h'
    · obtain ⟨g, -, rfl⟩ := List.mem_map.mp h'
      intro c _; exact c.1.elim
    · simp only [List.mem_cons, List.not_mem_nil, or_false] at h'
      rcases h' with rfl | rfl <;> rfl
  · have hs : sigmaJ (P 2) oc (σ.map .inl ++ [.inr 0, .inr 1]) j =
        σ.map (fun _ => []) ++ [proj j (stepTrace (P 2) oc (.inr 0)), proj j (stepTrace (P 2) oc (.inr 1))] := by
      unfold sigmaJ
      rw [List.map_append, List.map_map]
      congr 1
      apply List.map_congr_left
      intro g hg
      rw [hσ g hg]; rfl
    rw [hs]
    have hf : ∀ (a b : List (CRegM 1 × Nat)), (σ.map (fun _ => []) ++ [a, b]).flatten = a ++ b := by
      intro a b; rw [List.flatten_append, flat_nil]; simp
    have hgrp : ∀ (a b : List (CRegM 1 × Nat)), (∀ c1 c2, ¬ [c1, c2] <+ a) → (∀ c1 c2, ¬ [c1, c2] <+ b) →
        ∀ g ∈ σ.map (fun _ => []) ++ [a, b], ∀ c1 c2, ¬ [c1, c2] <+ g := by
      intro a b ha hb g hg c1 c2 h
      rcases List.mem_append.mp hg with h' | h'
      · obtain ⟨_, -, rfl⟩ := List.mem_map.mp h'; exact pair_sub_nil h
      · simp only [List.mem_cons, List.not_mem_nil, or_false] at h'
        rcases h' with rfl | rfl
        · exact ha c1 c2 h
        · exact hb c1 c2 h
    rcases fin2 j with rfl | rfl
    · rw [tr0, tr1]
      simp only [proj, Fin.isValue, dite_true, dite_false, show ¬ ((1 : Fin 2) = 0) by decide]
      refine ⟨⟨fun c1 c2 h => ?_, fun g hg c1 c2 h => (hgrp _ _ (fun _ _ => pair_sub_single) (fun _ _ => pair_sub_single)
        g hg c1 c2 h).elim, fun m hm => by cases m <;> cases hm⟩, ⟨fun m hm => ?_, fun m x hx => ?_⟩, fun c _ h => by cases c.1 <;> cases h⟩
      · rw [hf] at h
        obtain ⟨rfl, rfl⟩ := pair_sub_two h
        exact ⟨fun h => absurd rfl h, rfl, rfl, fun _ h => by cases h⟩
      · cases m with
        | rd i => cases hm
        | wr i => obtain rfl := fin1 i; exact ⟨fun _ => ⟨2, by rw [hf]; simp⟩, fun _ => rfl⟩
      · rw [hf] at hx
        cases m with
        | rd i => obtain rfl := fin1 i; simp [lastArg] at hx
        | wr i => obtain rfl := fin1 i; simp [lastArg] at hx; rw [← hx]; rfl
    · rw [tr0, tr1]
      simp only [proj, Fin.isValue, dite_true, dite_false, show ¬ ((0 : Fin 2) = 1) by decide]
      refine ⟨⟨fun c1 c2 h => ?_, fun g hg c1 c2 h => (hgrp _ _ (fun _ _ => pair_sub_single) (fun _ _ => pair_sub_single)
        g hg c1 c2 h).elim, fun m hm => by cases m <;> cases hm⟩, ⟨fun m hm => ?_, fun m x hx => ?_⟩, fun c _ h => by cases c.1 <;> cases h⟩
      · rw [hf] at h
        obtain ⟨rfl, rfl⟩ := pair_sub_two h
        exact ⟨fun _ => rfl, rfl, rfl, fun h => by cases h⟩
      · cases m with
        | rd i => cases hm
        | wr i => obtain rfl := fin1 i; exact ⟨fun _ => ⟨5, by rw [hf]; simp⟩, fun _ => rfl⟩
      · rw [hf] at hx
        cases m with
        | rd i => obtain rfl := fin1 i; simp [lastArg] at hx; rw [← hx]; rfl
        | wr i => obtain rfl := fin1 i; simp [lastArg] at hx; rw [← hx]; rfl

/-! ## Negatives -/

/-- **Two arguments at a port defeat today's check**: so an Unsafe wire (outside `lcKey`) driven by
    two callers with different arguments in one cycle cannot get a `Cert` (it may fail to prove). -/
theorem cert_two_args {V : Type} (P : Parent V) (C : Cert P) (oc : (j : P.I) → Outs (P.CI j).M V)
    (w : Wires P.X.M V) (j : P.I) (cm : (P.CI j).M) (hE : Env P oc w) {s1 s2 : Step P}
    (h1 : s1 ∈ C.users j cm) (h2 : s2 ∈ C.users j cm) (a1 : Av P C.fire oc w s1) (a2 : Av P C.fire oc w s2)
    {x1 x2 : V} (hx1 : (cm, x1) ∈ proj j (trS P oc w s1)) (hx2 : (cm, x2) ∈ proj j (trS P oc w s2)) : x1 = x2 :=
  (C.arg oc w j cm hE s1 h1 a1 x1 hx1).symm.trans (C.arg oc w j cm hE s2 h2 a2 x2 hx2)

/-- **A synthesized module cannot export a self-SBR method**: a certificate needs every exported
    method C with itself or sharing one argument (bsc's exported SBR is not a guarantee). -/
theorem no_certL_sbr_self {V : Type} (P : Parent V) (m : P.X.M) (hc : P.X.ann.conf m m = false)
    (hs : P.X.ann.share m = false) : CertL P → False := fun C => by
  rcases C.selfAnn m with h | h
  · rw [hc] at h; cases h
  · rw [hs] at h; cases h

/-- One action port that accumulates (a Counter's `addA`, or a synthesized method whose argument
    reaches such a port), driven by the last caller's argument. -/
inductive AccM where
  | inc
  deriving DecidableEq

def accAnn (sbr : Bool) : Ann AccM :=
  ⟨fun _ _ => false, fun _ _ => sbr, fun _ _ => false, fun _ => false, fun _ => true, fun _ => false, fun _ => true,
    fun _ _ => false, fun _ _ => false, fun _ _ => false⟩

@[reducible] def Acc (sbr : Bool) : Module Nat ⟨AccM, inferInstance, accAnn sbr⟩ where
  S := Nat
  R := Empty
  meth _ x s _ s' := s' = s + x
  rule r := r.elim
  clean s := s
  clean_idem _ := rfl
  Sig := Unit
  hw s w _ s' _ := s' = if w.en .inc then s + w.arg .inc else s
  ok _ := True

/-- **An accumulating port is not last-wins**, annotated SBR with itself (a synthesized child, bug
    synth-sbr) or CF with itself (Counter.bs `addA`, bug counter): two different calls break it. -/
theorem acc_not_realizes (sbr : Bool) : ¬ Realizes (Acc sbr) := by
  intro h
  have hL : Legal (accAnn sbr) [[(AccM.inc, 1)], [(AccM.inc, 2)]] := by
    refine ⟨fun c1 c2 hc => ?_, fun g hg c1 c2 hc => ?_, fun _ h => by cases h⟩
    · simp only [List.flatten] at hc
      obtain ⟨rfl, rfl⟩ := pair_sub_two hc
      exact ⟨fun h => absurd rfl h, rfl, rfl, fun _ h => by cases h⟩
    · simp only [List.mem_cons, List.not_mem_nil, or_false] at hg
      rcases hg with rfl | rfl <;> exact absurd hc pair_sub_single
  have hD : Drives (accAnn sbr) ⟨fun _ => true, fun _ => 2⟩ [[(AccM.inc, 1)], [(AccM.inc, 2)]] := by
    refine ⟨fun m _ => ?_, fun m x hx => ?_⟩
    · cases m; exact ⟨fun _ => ⟨1, by simp⟩, fun _ => rfl⟩
    · cases m; simp [lastArg] at hx; exact hx
  obtain ⟨τ, fin, hg, hex, hs', -⟩ := h 0 _ _ ⟨fun _ => true, fun _ => 0⟩ 2 () rfl hL hD rfl trivial
    (fun c _ h => by cases c.1; cases h)
  have hE := execG_of_exec (Acc sbr) _ (fun r => r.elim) 0 fin τ hex
  rw [hg] at hE
  simp only [List.flatten] at hE
  cases hE with
  | cons h1 hr =>
    cases hr with
    | cons h2 hr' =>
      cases hr'
      simp only at h1 h2 hs'
      subst h1; subst h2
      cases hs'

/-- `addA` modelled C with itself (one call a cycle) realizes its contract. -/
theorem accC_realizes : RealizesA (Acc false) { accAnn false with conf := fun _ _ => true } := by
  intro s σ w o s' g _ hL hD hhw _ _
  have hone : ∀ c1 c2, ¬ [c1, c2] <+ σ.flatten := fun c1 c2 h => by
    have := (hL.order c1 c2 h).2.1; cases this
  rcases hfl : σ.flatten with _ | ⟨⟨m, x⟩, _ | ⟨b, l⟩⟩
  · have h0 : w.en .inc = false := by
      cases h' : w.en .inc
      · rfl
      · obtain ⟨x, hx⟩ := (hD.en .inc rfl).mp h'; rw [hfl] at hx; cases hx
    refine ⟨σ.map .inl, s, groups_map_inl σ, exec_map_inl _ o σ s s (by rw [hfl]; exact .nil _), ?_, fun _ => rfl⟩
    rw [hhw, h0]; rfl
  · cases m
    have h1 : w.en .inc = true := (hD.en .inc rfl).mpr ⟨x, by rw [hfl]; simp⟩
    have h2 : w.arg .inc = x := hD.arg .inc x (by rw [hfl]; rfl)
    refine ⟨σ.map .inl, s + x, groups_map_inl σ, exec_map_inl _ o σ s _ (by rw [hfl]; exact .cons rfl (.nil _)), ?_,
      fun _ => rfl⟩
    rw [hhw, h1, h2]; rfl
  · exact (hone (m, x) b (by rw [hfl]; simp)).elim

end LCEx
end Modular

#print axioms Modular.LCEx.firstCaller_not_obl
#print axioms Modular.LCEx.lastCaller_obl
#print axioms Modular.LCEx.cert_two_args
#print axioms Modular.LCEx.no_certL_sbr_self
#print axioms Modular.LCEx.acc_not_realizes
#print axioms Modular.LCEx.accC_realizes
