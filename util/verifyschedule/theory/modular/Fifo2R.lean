/- Guarded `FIFO2`'s `first`, always ready (design.md §7, §14): `FIFO2.v` drives `D_OUT` from its
   head register, so a read of an empty FIFO returns that register's value at the cycle's start.
   Used only for readiness sites that the readiness rule cannot recover. -/
import Modular

open scoped List

namespace Modular

variable {V : Type}

/-- `fifoAnn` with `first` always ready. -/
def fifoAnnR : Ann FifoM := { fifoAnn with alwaysRdy := fun m => match m with | .enq | .deq => false | _ => true }

abbrev FIFO2R.X : Iface := ⟨FifoM, inferInstance, fifoAnnR⟩

/-- A 2-element FIFO whose `first` is always ready: the state is `FIFO2`'s and the head register
    at the cycle's start; `first` returns the head when the FIFO started the cycle non-empty, and
    that register otherwise (both are `FIFO2.v`'s `D_OUT`). -/
@[reducible] def FIFO2R (ofBool : Bool → V) : Module V FIFO2R.X where
  S := F2 V × V
  R := Empty
  meth := fun (m : FifoM) x s v s' => match m with
    | .first => v = (if s.1.ne0 then s.1.d0 else s.2) ∧ s' = s
    | m' => (FIFO2 ofBool).meth m' x s.1 v s'.1 ∧ s'.2 = s.2
  rule := fun r => r.elim
  clean s := (s.1.snap, s.1.d0)
  clean_idem _ := rfl
  Sig := Unit
  hw s w o s' g := (FIFO2 ofBool).hw s.1 w o s'.1 g ∧ s'.2 = s'.1.d0
  ok _ := True

theorem execG_cons_inv {X : Iface} {Mo : Module V X} {o : Outs X.M V} {s t : Mo.S} {m : X.M} {x : V}
    {l : List (X.M × V)} (h : ExecG Mo o s ((m, x) :: l) t) : ∃ s1, Mo.meth m x s (o.ret m) s1 ∧ ExecG Mo o s1 l t := by
  cases h with
  | cons hm hl => exact ⟨_, hm, hl⟩

theorem exec_nil_eq' {X : Iface} {Mo : Module V X} {o : Outs X.M V} {s t : Mo.S} (h : Exec Mo o s [] t) : s = t := by
  cases h; rfl

theorem exec_inl_inv {X : Iface} {Mo : Module V X} {o : Outs X.M V} {s t : Mo.S} {g : List (X.M × V)}
    {τ : List (List (X.M × V) ⊕ Mo.R)} (h : Exec Mo o s (.inl g :: τ) t) : ∃ s1, ExecG Mo o s g s1 ∧ Exec Mo o s1 τ t := by
  cases h with
  | grp hg hτ => exact ⟨_, hg, hτ⟩

/-- `FIFO2`'s calls keep the start-of-cycle flags. -/
theorem f2_ne0 (ofBool : Bool → V) {m : FifoM} {x v : V} {t t' : F2 V}
    (h : (FIFO2 ofBool).meth m x t v t') : t'.ne0 = t.ne0 := by
  cases m with
  | enq => exact h.2.2.2.1
  | deq => exact h.2.2.2.1
  | first => rw [h.2.2]
  | notEmpty => rw [h.2]
  | notFull => rw [h.2]
  | i_notEmpty => rw [h.2]
  | i_notFull => rw [h.2]
  | clear => exact h.2.2.1

/-- A cycle that starts non-empty: `FIFO2`'s run is a run of `FIFO2R`. -/
theorem execG_lift (ofBool : Bool → V) (o : Outs FifoM V) (d : V) :
    ∀ (l : List (FifoM × V)) (t t' : F2 V), ExecG (FIFO2 ofBool) o t l t' → t.ne0 = true →
      ExecG (FIFO2R ofBool) o (t, d) l (t', d)
  | [], t, t', h, _ => by rw [execG_nil_eq _ _ h]; exact .nil _
  | (m, x) :: l, t, t', h, hn => by
    obtain ⟨t1, hm, hl⟩ := execG_cons_inv h
    have hn1 : t1.ne0 = true := by rw [f2_ne0 ofBool hm]; exact hn
    refine .cons (s1 := (t1, d)) ?_ (execG_lift ofBool o d l t1 t' hl hn1)
    cases m with
    | first =>
      obtain ⟨-, hv, e⟩ := hm
      subst e
      exact ⟨by rw [hn]; exact hv, rfl⟩
    | enq => exact ⟨hm, rfl⟩
    | deq => exact ⟨hm, rfl⟩
    | notEmpty => exact ⟨hm, rfl⟩
    | notFull => exact ⟨hm, rfl⟩
    | i_notEmpty => exact ⟨hm, rfl⟩
    | i_notFull => exact ⟨hm, rfl⟩
    | clear => exact ⟨hm, rfl⟩

theorem exec_lift (ofBool : Bool → V) (o : Outs FifoM V) (d : V) :
    ∀ (σ : List (List (FifoM × V))) (t t' : F2 V), Exec (FIFO2 ofBool) o t (σ.map .inl) t' → t.ne0 = true →
      Exec (FIFO2R ofBool) o (t, d) (σ.map .inl) (t', d)
  | [], t, t', h, _ => by rw [exec_nil_eq' h]; exact .nil _
  | g :: σ, t, t', h, hn => by
    obtain ⟨t1, hg, hτ⟩ := exec_inl_inv h
    have hn1 : t1.ne0 = true := by
      have key : ∀ (l : List (FifoM × V)) (a b : F2 V), ExecG (FIFO2 ofBool) o a l b → b.ne0 = a.ne0 := by
        intro l
        induction l with
        | nil => intro a b h; rw [execG_nil_eq _ _ h]
        | cons c l ih =>
          intro a b h
          rcases c with ⟨m, x⟩
          obtain ⟨a1, hm, hl⟩ := execG_cons_inv h
          rw [ih _ _ hl, f2_ne0 ofBool hm]
      rw [key _ _ _ hg]; exact hn
    exact .grp (execG_lift ofBool o d g t t1 hg hn) (exec_lift ofBool o d σ t1 t' hτ hn1)

/-- A cycle that starts empty: `FIFO2`'s run without the `first` calls, with them put back. -/
theorem execG_insert (ofBool : Bool → V) (o : Outs FifoM V) (d : V) (hf : o.ret .first = d) :
    ∀ (l : List (FifoM × V)) (t t' : F2 V), ExecG (FIFO2 ofBool) o t (l.filter (fun c => c.1 != .first)) t' →
      t.ne0 = false → ExecG (FIFO2R ofBool) o (t, d) l (t', d)
  | [], t, t', h, _ => by rw [execG_nil_eq _ _ h]; exact .nil _
  | (m, x) :: l, t, t', h, hn => by
    cases m with
    | first =>
      simp only [List.filter_cons, bne_self_eq_false, Bool.false_eq_true, ite_false] at h
      exact .cons (s1 := (t, d)) ⟨by rw [hn, hf]; rfl, rfl⟩ (execG_insert ofBool o d hf l t t' h hn)
    | _ =>
      simp only [List.filter_cons, bne_iff_ne, ne_eq, reduceCtorEq, not_false_eq_true, ite_true] at h
      obtain ⟨t1, hm, hl⟩ := execG_cons_inv h
      exact .cons (s1 := (t1, d)) ⟨hm, rfl⟩ (execG_insert ofBool o d hf l t1 t' hl (by rw [f2_ne0 ofBool hm]; exact hn))

theorem exec_inl_flatten {X : Iface} (Mo : Module V X) (o : Outs X.M V) :
    ∀ (σ : List (List (X.M × V))) (s t : Mo.S), Exec Mo o s (σ.map .inl) t → ExecG Mo o s σ.flatten t
  | [], s, t, h => by rw [exec_nil_eq' h]; exact .nil _
  | g :: σ, s, t, h => by
    obtain ⟨s1, hg, hτ⟩ := exec_inl_inv h
    exact execG_append' Mo o _ _ hg (exec_inl_flatten Mo o σ _ t hτ)

theorem groups_eq_map_inl {α : Type} : ∀ (τ : List (α ⊕ Empty)) (σ : List α), groups τ = σ → τ = σ.map .inl
  | [], σ, h => by cases h; rfl
  | .inl a :: τ, σ, h => by
    cases σ with
    | nil => cases h
    | cons b σ =>
      simp only [groups, List.cons.injEq] at h
      rw [groups_eq_map_inl τ σ h.2, h.1]; rfl
  | .inr e :: _, _, _ => e.elim

theorem lastArg_none_of {M : Type} [DecidableEq M] (m : M) :
    ∀ (l : List (M × V)), (∀ x, (m, x) ∉ l) → lastArg m l = none
  | [], _ => rfl
  | c :: l, h => by
    simp only [lastArg, lastArg_none_of m l (fun x hx => h x (List.mem_cons_of_mem _ hx))]
    rw [ite_eq_right_iff.mpr (fun e => absurd (show (m, c.2) ∈ c :: l by rw [← e]; exact List.mem_cons_self ..) (h c.2))]

theorem lastArg_filter_other {M : Type} [DecidableEq M] (p : M × V → Bool) (m : M)
    (hp : ∀ c : M × V, c.1 = m → p c = true) :
    ∀ (l : List (M × V)), lastArg m (l.filter p) = lastArg m l
  | [] => rfl
  | c :: l => by
    by_cases hc : p c = true
    · rw [List.filter_cons_of_pos hc]
      simp only [lastArg, lastArg_filter_other p m hp l]
    · rw [List.filter_cons_of_neg hc, lastArg_filter_other p m hp l]
      simp only [lastArg]
      cases lastArg m l with
      | some y => rfl
      | none => rw [ite_eq_right_iff.mpr (fun e => absurd (hp c e) hc)]

/-- **`FIFO2R` meets its contract.** -/
theorem fifo2r_realizes (ofBool : Bool → V) : Realizes (FIFO2R ofBool) := by
  intro s σ w o s' g hc hL hD hhw hok hrdy
  obtain ⟨hhw1, hs2⟩ := hhw
  have hc1 : s.1.snap = s.1 := congrArg Prod.fst hc
  have hc2 : s.1.d0 = s.2 := congrArg Prod.snd hc
  have hn0 : s.1.ne0 = s.1.ne := by have := congrArg F2.ne0 hc1; simp [F2.snap] at this; exact this.symm
  have hL' : Legal fifoAnn σ := ⟨hL.order, hL.group, hL.always⟩
  have hD' : Drives fifoAnn w σ := ⟨hD.en, hD.arg⟩
  cases hne : s.1.ne
  · -- starts empty: drop the `first` calls, run `FIFO2`, put them back
    let p : FifoM × V → Bool := fun c => c.1 != .first
    let σ' := σ.map (List.filter p)
    have hflat : σ'.flatten = σ.flatten.filter p := by simp [σ', List.filter_flatten]
    have hsub : σ'.flatten <+ σ.flatten := by rw [hflat]; exact List.filter_sublist
    have hL2 : Legal fifoAnn σ' := by
      refine ⟨fun c1 c2 h => hL'.order c1 c2 (h.trans hsub), fun g' hg' c1 c2 h => ?_, fun m hm => by cases m <;> cases hm⟩
      obtain ⟨g0, hg0, rfl⟩ := List.mem_map.mp hg'
      exact hL'.group g0 hg0 c1 c2 (h.trans List.filter_sublist)
    have hD2 : Drives fifoAnn w σ' := by
      refine ⟨fun m hm => ?_, fun m x hx => ?_⟩
      · rw [hD'.en m hm, hflat]
        constructor
        · rintro ⟨x, hx⟩
          refine ⟨x, List.mem_filter.mpr ⟨hx, ?_⟩⟩
          cases m <;> simp_all [p, fifoAnn]
        · rintro ⟨x, hx⟩; exact ⟨x, (List.mem_filter.mp hx).1⟩
      · by_cases hm : m = .first
        · subst hm
          rw [hflat] at hx
          have : lastArg FifoM.first (σ.flatten.filter p) = none := by
            apply lastArg_none_of
            intro x hc
            have := (List.mem_filter.mp hc).2
            simp [p] at this
          rw [this] at hx; cases hx
        · rw [hflat, lastArg_filter_other p m (fun c e => by simp [p, e, hm])] at hx
          exact hD'.arg m x hx
    have hrdy2 : ∀ c ∈ σ'.flatten, fifoAnn.alwaysRdy c.1 = false → o.rdy c.1 = true := by
      intro c hc ha
      rw [hflat] at hc
      obtain ⟨hc1', hc2'⟩ := List.mem_filter.mp hc
      refine hrdy c hc1' ?_
      rcases c with ⟨m, x⟩
      cases m <;> simp_all [p, fifoAnn, fifoAnnR]
    obtain ⟨τ', fin, hg, hex, hs', -⟩ := fifo2_realizes ofBool s.1 σ' w o s'.1 () hc1 hL2 hD2 hhw1 trivial hrdy2
    refine ⟨σ.map .inl, (fin, s.2), groups_map_inl σ, ?_, ?_, fun l => nomatch l⟩
    · rw [groups_eq_map_inl τ' σ' hg] at hex
      have hG := exec_inl_flatten (FIFO2 ofBool) o σ' s.1 fin hex
      rw [hflat] at hG
      have hf : o.ret .first = s.2 := by rw [hhw1.2.2.2.1, hc2]
      exact exec_map_inl _ o σ s (fin, s.2)
        (execG_insert ofBool o s.2 hf σ.flatten s.1 fin hG (by rw [hn0, hne]))
    · show s' = (fin.snap, fin.d0)
      exact Prod.ext hs' (by rw [hs2, hs']; rfl)
  · -- starts non-empty: `first` is ready, so `FIFO2`'s contract applies directly
    have hrdy2 : ∀ c ∈ σ.flatten, fifoAnn.alwaysRdy c.1 = false → o.rdy c.1 = true := by
      intro c hc ha
      rcases c with ⟨m, x⟩
      cases m with
      | first => rw [hhw1.2.2.1, hne]
      | enq => exact hrdy _ hc rfl
      | deq => exact hrdy _ hc rfl
      | _ => cases ha
    obtain ⟨τ', fin, hg, hex, hs', -⟩ := fifo2_realizes ofBool s.1 σ w o s'.1 () hc1 hL' hD' hhw1 trivial hrdy2
    rw [groups_eq_map_inl τ' σ hg] at hex
    refine ⟨σ.map .inl, (fin, s.2), groups_map_inl σ, exec_lift ofBool o s.2 σ s.1 fin hex (by rw [hn0, hne]), ?_,
      fun l => nomatch l⟩
    show s' = (fin.snap, fin.d0)
    exact Prod.ext hs' (by rw [hs2, hs']; rfl)

/-- **`FIFO2R` is causal**, with no paths. -/
theorem fifo2r_causal (ofBool : Bool → V) : Causal (FIFO2R ofBool) :=
  ⟨fun s _ => f2Out ofBool s.1, fun _ _ _ _ _ => ⟨rfl, rfl⟩,
   fun s w _ => ⟨(_, _), (), ⟨rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩, rfl⟩⟩

/-- **`FIFO2R` respects `FIFO2`'s output aliases.** -/
theorem fifo2r_aliased (ofBool : Bool → V) (toB : V → Bool) (h : ∀ b, toB (ofBool b) = b) :
    Aliased toB (FIFO2R ofBool) := by
  intro s w o s' g _ hhw
  obtain ⟨⟨hre, hrd, hrf, -, hne, hnf, hine, hinf, -⟩, -⟩ := hhw
  refine ⟨fun p q hpq => ?_, fun a p hap => ?_⟩
  · cases p <;> cases q <;> simp [fifoAnnR, fifoAnn] at hpq <;> simp only [hne, hnf, hine, hinf]
  · cases a <;> cases p <;> simp [fifoAnnR, fifoAnn] at hap <;> simp only [hre, hrd, hrf, hne, hnf, hine, hinf, h]

end Modular

#print axioms Modular.fifo2r_realizes
#print axioms Modular.fifo2r_causal
#print axioms Modular.fifo2r_aliased
