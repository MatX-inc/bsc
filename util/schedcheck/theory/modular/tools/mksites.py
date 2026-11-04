# Writes probe/Sites<k>.lean: the conditional-site probe of mkcond.py, with
# the step written as a first-order site list (Modular.Site, runSites), the
# form design.md section 7 specifies for generated files (review
# integration 2, M1).
#
#   rule go: s <- sel.read;
#            for i < k: if bit_i(s) then v_i <- a_i.read          (site 2i+1)
#                       if bit_i(s) && e(v_i) then r_i <= g(v_i)  (site 2i+2)
#
# The trace is the library's (trace_runSites); each site's value is the
# library's (envHw_at); each port's facts read only its own site
# (calls_sites, mem_proj_callsOf).
#
# usage: python3 -I mksites.py k out.lean
import sys
k = int(sys.argv[1]); out = sys.argv[2]
n = 2 * k + 1
L = []
w = L.append
def FIX(i):
    return "rw [show (⟨%d, hn0⟩ : Fin %d) = (%d : Fin %d) from rfl] at *" % (i, k, i, k)
def FILT(j, port, val):
    return "show (sites ops).filter (onPort (CM := fun j => (CI j).M) (%s) (%s)) = %s from rfl" % (j, port, val)
def SA(i): return "⟨.a %d, Reg.read, fun ρ => ops.bit %d (ρ 0), fun _ => ops.z⟩" % (i, i)
def SR(i): return "⟨.r %d, Reg.write, fun ρ => ops.bit %d (ρ 0) && ops.e (ρ %d), fun ρ => ops.g (ρ %d)⟩" % (i, i, 2*i+1, 2*i+1)
w("""import Modular
open Modular
open scoped List

set_option autoImplicit false
set_option linter.unusedVariables false
set_option linter.unusedSimpArgs false
set_option linter.deprecated false
set_option maxRecDepth 100000
set_option maxHeartbeats 0

namespace Sites%d

structure Ops (V : Type) where
  z : V
  bit : Nat → V → Bool
  e : V → Bool
  g : V → V

/-- children: the selector, the k read registers, the k written registers -/
inductive K where
  | sel
  | a (i : Fin %d)
  | r (i : Fin %d)
  deriving DecidableEq

def kidsL : List K := .sel :: ((List.finRange %d).map .a ++ (List.finRange %d).map .r)

theorem kids_nodup : kidsL.Nodup := by
  unfold kidsL
  rw [List.nodup_cons, List.nodup_append]
  refine ⟨?_, List.Pairwise.map K.a (fun a b h e => h (by cases e; rfl)) (List.nodup_finRange _),
    List.Pairwise.map K.r (fun a b h e => h (by cases e; rfl)) (List.nodup_finRange _), ?_⟩
  · intro h; rcases List.mem_append.mp h with h | h <;> obtain ⟨x, -, e⟩ := List.mem_map.mp h <;> cases e
  · intro a ha b hb e
    obtain ⟨x, -, rfl⟩ := List.mem_map.mp ha
    obtain ⟨y, -, rfl⟩ := List.mem_map.mp hb
    cases e

theorem kids_all (j : K) : j ∈ kidsL := by
  cases j <;> simp [kidsL, List.mem_finRange]

def noAnn : Ann Empty where
  prec _ _ := false
  sbr _ _ := false
  conf _ _ := false
  share _ := false
  hasEn _ := false
  alwaysEn _ := false
  alwaysRdy _ := true
abbrev NoX : Iface := ⟨Empty, inferInstance, noAnn⟩
abbrev CI : K → Iface := fun _ => Reg.X

theorem fin1 (i : Fin 1) : i = 0 := by
  obtain ⟨k, hk⟩ := i; congr; omega

def regW {V : Type} (z : V) (en : Bool) (d : V) : Wires (CRegM 1) V :=
  ⟨fun m => match m with | .wr _ => en | .rd _ => false, fun m => match m with | .wr _ => d | .rd _ => z⟩

section
variable {V : Type} (ops : Ops V) (u : Nat → V)

/-- The step, as sites over the read environment `ρ`. -/
def sites : List (Site K (fun j => (CI j).M) V) :=
  [⟨.sel, Reg.read, fun _ => true, fun _ => ops.z⟩""" % (k, k, k, k, k))
for i in range(k):
    w(",\n   ⟨.a %d, Reg.read, fun ρ => ops.bit %d (ρ 0), fun _ => ops.z⟩" % (i, i))
    w(",\n   ⟨.r %d, Reg.write, fun ρ => ops.bit %d (ρ 0) && ops.e (ρ %d), fun ρ => ops.g (ρ %d)⟩" % (i, i, 2 * i + 1, 2 * i + 1))
w("""]

/-- The hardware: `r_i`'s enable and data read `a_i` unconditionally. -/
def comb (oc : (j : K) → Outs (CI j).M V) (w : Wires Empty V) : ((j : K) → Wires (CI j).M V) × Outs Empty V :=
  (fun j => match j with
    | .sel => regW ops.z false ops.z
    | .a _ => regW ops.z false ops.z
    | .r i => regW ops.z (ops.bit i.val ((oc .sel).ret Reg.read) && ops.e ((oc (.a i)).ret Reg.read))
        (ops.g ((oc (.a i)).ret Reg.read)),
   ⟨fun m => m.elim, fun m => m.elim⟩)

abbrev P : Parent V where
  I := K
  decI := inferInstance
  kids := kidsL
  kids_nodup := kids_nodup
  kids_all := kids_all
  CI := CI
  X := NoX
  PR := Unit
  body := fun m => m.elim
  rbody := fun _ => runSites u (fun _ => some ops.z) (sites ops) 0 u
  comb := comb ops
  asm _ _ := True
  onehot _ _ := True

/-- Each site reads only earlier sites (one rewrite per read). -/
theorem sitesOK : SitesOK (sites ops) 0 :=
  ⟨fun _ _ _ => ⟨rfl, rfl⟩""")
for i in range(k):
    w(",\n   fun ρ ρ' h => by simp only [h 0 (by decide), and_self]")
    w(",\n   fun ρ ρ' h => by simp only [h 0 (by decide), h %d (by decide), and_self]" % (2 * i + 1))
w(""", trivial⟩

theorem sites_len : (sites ops).length = %d := rfl

variable (oc : (j : K) → Outs (CI j).M V)

theorem trR_go (r : Unit) : trR (P ops u) oc r = (callsOf (envHw u oc (sites ops) 0 u) (sites ops), some ops.z) :=
  trace_runSites u oc (fun _ => some ops.z) (sites ops) 0 u (sitesOK ops)

/-- The selector's value. -/
theorem e0 : envHw u oc (sites ops) 0 u 0 = (oc .sel).ret Reg.read :=
  envHw_at u oc (sites ops) 0 u (sitesOK ops) 0 (by rw [sites_len]; decide)

theorem calls_go (w : Wires Empty V) (j : K) (cm : (CI j).M) :
    Calls (P ops u) (trS (P ops u) oc w (.inr ())) j cm ↔
      ∃ s ∈ (sites ops).filter (onPort j cm), s.cond (envHw u oc (sites ops) 0 u) = true := by
  rw [trS, trR_go]; exact calls_sites (P ops u) _ _ j cm

theorem mem_go (w : Wires Empty V) (j : K) (cm : (CI j).M) (x : V)
    (hx : (cm, x) ∈ proj j (trS (P ops u) oc w (.inr ()))) :
    ∃ s ∈ (sites ops).filter (onPort j cm), s.cond (envHw u oc (sites ops) 0 u) = true ∧
      s.arg (envHw u oc (sites ops) 0 u) = x := by
  rw [trS, trR_go] at hx
  exact mem_proj_callsOf (CM := fun j => (CI j).M) _ j cm x (sites ops) hx
""" % n)
for i in range(k):
    w("""theorem ea%d : envHw u oc (sites ops) 0 u %d =
    if ops.bit %d (envHw u oc (sites ops) 0 u 0) then (oc (.a %d)).ret Reg.read else u %d :=
  envHw_at u oc (sites ops) 0 u (sitesOK ops) %d (by rw [sites_len]; decide)
""" % (i, 2 * i + 1, i, i, 2 * i + 1, 2 * i + 1))
w("""end

def fp : Empty ⊕ Unit → List (Σ j, (CI j).M)
  | .inl m => m.elim
  | .inr _ => [⟨.sel, Reg.read⟩""")
for i in range(k):
    w(", ⟨.a %d, Reg.read⟩, ⟨.r %d, Reg.write⟩" % (i, i))
w("""]

section
variable {V : Type} (ops : Ops V) (u : Nat → V)

theorem fp_eq : (sites ops).map (fun s => (⟨s.j, s.m⟩ : Σ j, (CI j).M)) = fp (.inr ()) := rfl

theorem sites_nodup : ((sites ops).map (fun s => s.j)).Nodup := by
  rw [show (sites ops).map (fun s => s.j) = [K.sel""" + "".join(", .a %d, .r %d" % (i, i) for i in range(k)) + """] from rfl]
  decide

variable (oc : (j : K) → Outs (CI j).M V) (w : Wires Empty V)
""")
for i in range(k):
    w("""theorem fa%(i)d : (sites ops).filter (onPort (CM := fun j => (CI j).M) (K.a %(i)d) (CRegM.wr 0)) = [] := rfl
theorem fr%(i)d : (sites ops).filter (onPort (CM := fun j => (CI j).M) (K.r %(i)d) (CRegM.rd 0)) = [] := rfl
theorem ga%(i)d : (sites ops).filter (onPort (CM := fun j => (CI j).M) (K.a %(i)d) (CRegM.rd 0)) = [%(SA)s] := rfl
theorem gr%(i)d : (sites ops).filter (onPort (CM := fun j => (CI j).M) (K.r %(i)d) (CRegM.wr 0)) = [%(SR)s] := rfl
theorem en_a%(i)d : ((comb ops oc w).1 (K.a %(i)d)).en (CRegM.wr 0) = true ↔
    ∃ s ∈ (sites ops).filter (onPort (K.a %(i)d) (CRegM.wr 0)), s.cond (envHw u oc (sites ops) 0 u) = true := by
  rw [fa%(i)d]; simp [comb, regW]
theorem en_r%(i)d : ((comb ops oc w).1 (K.r %(i)d)).en (CRegM.wr 0) = true ↔
    ∃ s ∈ (sites ops).filter (onPort (K.r %(i)d) (CRegM.wr 0)), s.cond (envHw u oc (sites ops) 0 u) = true := by
  rw [gr%(i)d]
  simp only [List.mem_singleton, exists_eq_left, ea%(i)d, e0, comb, regW]
  try simp only [show ((%(i)d : Fin %(k)d) : Nat) = %(i)d from rfl]
  cases hb : ops.bit %(i)d ((oc .sel).ret Reg.read) <;> simp [hb]
theorem arg_a%(i)d (x : V) (s : Site K (fun j => (CI j).M) V)
    (hs : s ∈ (sites ops).filter (onPort (K.a %(i)d) (CRegM.rd 0)))
    (ha : s.arg (envHw u oc (sites ops) 0 u) = x) : ((comb ops oc w).1 (K.a %(i)d)).arg (CRegM.rd 0) = x := by
  rw [ga%(i)d] at hs
  simp only [List.mem_singleton] at hs; subst hs; simp only at ha; subst ha; rfl
theorem arg_r%(i)d (x : V) (s : Site K (fun j => (CI j).M) V)
    (hs : s ∈ (sites ops).filter (onPort (K.r %(i)d) (CRegM.wr 0)))
    (hc : s.cond (envHw u oc (sites ops) 0 u) = true)
    (ha : s.arg (envHw u oc (sites ops) 0 u) = x) : ((comb ops oc w).1 (K.r %(i)d)).arg (CRegM.wr 0) = x := by
  rw [gr%(i)d] at hs
  simp only [List.mem_singleton] at hs; subst hs
  simp only [Bool.and_eq_true] at hc ha
  rw [ea%(i)d, e0] at hc ha
  simp only [comb, regW]
  rw [if_pos hc.1] at ha
  exact ha
""" % {"i": i, "k": k, "SA": SA(i), "SR": SR(i)})
w("""end

section
variable {V : Type} (ops : Ops V) (u : Nat → V)

def cert : Cert (P ops u) where
  rules := [()]
  rules_nodup := by simp
  fire _ _ _ := true
  hit _ _ m _ := m.elim
  fp := fp
  users _ _ := [.inr ()]
  users_ok s p _ := by rcases s with m | r; exact m.elim; simp
  fp_ok oc w s c hc := by
    rcases s with m | r
    · exact m.elim
    · simp only [trS, trR_go] at hc
      rw [← fp_eq ops]
      exact (callsOf_ports _ _).subset (List.mem_map.mpr ⟨c, hc, rfl⟩)
  selfAnn m := m.elim
  fireIn _ _ r _ := by simp
  resM _ _ m := m.elim
  resR oc w r _ _ := by rw [trR_go]; rfl
  rdyM _ _ m := m.elim
  rdyR oc w r _ _ c _ h := by simp [cregAnn] at h
  grpM _ _ m := m.elim
  grpR oc w r _ _ := by rw [trR_go]; exact grpT_sites (P ops u) _ _ (sites_nodup ops)
  mm _ _ m := m.elim
  mmg _ _ m := m.elim
  h1 _ _ m := m.elim
  h2 _ _ m := m.elim
  h3 _ _ m := m.elim
  h4 _ _ r1 r2 _ _ _ hne := by cases r1; cases r2; exact absurd rfl hne
  en oc w j cm _ hcm := by
    simp only [List.mem_singleton, exists_eq_left, On, true_and]
    rw [calls_go]
    cases j with
    | sel =>
      cases cm with
      | rd _ => simp [cregAnn] at hcm
      | wr i' => rw [fin1 i', show (sites ops).filter (onPort (CM := fun j => (CI j).M) K.sel (CRegM.wr 0)) = [] from rfl]; simp [comb, regW]
    | a i =>
      cases cm with
      | rd _ => simp [cregAnn] at hcm
      | wr i' =>
        rw [fin1 i']
        obtain ⟨n, hn⟩ := i
        match n, hn with
""")
for i in range(k):
    w("        | %d, hn0 => %s; exact en_a%d ops u oc w\n" % (i, FIX(i), i))
w("""        | n + %d, h => exact absurd h (by omega)
    | r i =>
      cases cm with
      | rd _ => simp [cregAnn] at hcm
      | wr i' =>
        rw [fin1 i']
        obtain ⟨n, hn⟩ := i
        match n, hn with
""" % k)
for i in range(k):
    w("        | %d, hn0 => %s; exact en_r%d ops u oc w\n" % (i, FIX(i), i))
w("""        | n + %d, h => exact absurd h (by omega)
  enV _ _ m := m.elim
  arg oc w j cm _ s hs _ x hx := by
    simp only [List.mem_singleton] at hs
    subst hs
    obtain ⟨s, hs, hc, ha⟩ := mem_go ops u oc w j cm x hx
    cases j with
    | sel =>
      cases cm with
      | rd i' =>
        rw [fin1 i', show (sites ops).filter (onPort (CM := fun j => (CI j).M) K.sel (CRegM.rd 0)) =
          [⟨.sel, Reg.read, fun _ => true, fun _ => ops.z⟩] from rfl] at hs
        simp only [List.mem_singleton] at hs; subst hs; simp only at ha; subst ha; rfl
      | wr i' => rw [fin1 i', show (sites ops).filter (onPort (CM := fun j => (CI j).M) K.sel (CRegM.wr 0)) = [] from rfl] at hs; simp at hs
    | a i =>
      obtain ⟨n, hn⟩ := i
      cases cm with
      | rd i' =>
        rw [fin1 i'] at hs ⊢
        match n, hn with
""" % k)
for i in range(k):
    w("        | %d, hn0 => %s; exact arg_a%d ops u oc w x s hs ha\n" % (i, FIX(i), i))
w("""        | n + %d, h => exact absurd h (by omega)
      | wr i' =>
        rw [fin1 i'] at hs
        match n, hn with
""" % k)
for i in range(k):
    w("        | %d, hn0 => %s; rw [fa%d] at hs; simp at hs\n" % (i, FIX(i), i))
w("""        | n + %d, h => exact absurd h (by omega)
    | r i =>
      obtain ⟨n, hn⟩ := i
      cases cm with
      | rd i' =>
        rw [fin1 i'] at hs
        match n, hn with
""" % k)
for i in range(k):
    w("        | %d, hn0 => %s; rw [fr%d] at hs; simp at hs\n" % (i, FIX(i), i))
w("""        | n + %d, h => exact absurd h (by omega)
      | wr i' =>
        rw [fin1 i'] at hs ⊢
        match n, hn with
""" % k)
for i in range(k):
    w("        | %d, hn0 => %s; exact arg_r%d ops u oc w x s hs hc ha\n" % (i, FIX(i), i))
w("""        | n + %d, h => exact absurd h (by omega)
  alw oc w j cm _ h := by simp [cregAnn] at h
  onehot _ _ _ := trivial

/-- **The obligation**, for every value of the skipped sites. -/
theorem obl : Obl (P ops u) := obl_of_cert (P ops u) (cert ops u)
end
end Sites%d

#print axioms Sites%d.obl
""" % (k, k, k))
open(out, "w").write("".join(L))
