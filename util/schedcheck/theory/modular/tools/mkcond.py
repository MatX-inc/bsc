# Writes probe/Cond<k>.lean: a parent with one rule of 2k conditional sites,
# in the form a generated file would take (review integration 2, M1).
#
#   rule go: s <- sel.read;
#            for i < k: if bit_i(s) then v_i <- a_i.read          (conditional read)
#                       if bit_i(s) && e(v_i) then r_i <= g(v_i)  (conditional write)
#
# The value of a skipped read is the parameter u_i.  The certificate is
# proved with the library's site combinators: Prog.callIf (trace_callIf),
# Seq (footprint and one-caller facts in one pass), NoFail (result), the
# reflection calls_iff_any for the enables, and the contextual rewrite
# cond_ctx for values read under a condition.
#
# usage: python3 -I mkcond.py k out.lean
import sys
k = int(sys.argv[1]); out = sys.argv[2]
L = []
w = L.append
w("""import Modular
open Modular
open scoped List

set_option autoImplicit false
set_option linter.unusedVariables false
set_option linter.unusedSimpArgs false
set_option maxRecDepth 100000
set_option maxHeartbeats 0

namespace Cond%d

def k : Nat := %d

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
  simp only [kidsL, List.nodup_cons, List.mem_append, List.mem_map, List.nodup_append]
  refine ⟨by simp, List.Pairwise.map K.a (fun a b h e => h (by cases e; rfl)) (List.nodup_finRange _),
    List.Pairwise.map K.r (fun a b h e => h (by cases e; rfl)) (List.nodup_finRange _), by simp⟩

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
variable {V : Type} (ops : Ops V) (u : Fin %d → V)
""" % (k, k, k, k, k, k, k))
# rbody
body = "  .call .sel Reg.read ops.z (fun s =>\n"
for i in range(k):
    body += "    Prog.callIf (ops.bit %d s) (.a %d) Reg.read ops.z (u %d) (fun v%d =>\n" % (i, i, i, i)
    body += "    Prog.callIf (ops.bit %d s && ops.e v%d) (.r %d) Reg.write (ops.g v%d) ops.z (fun _ =>\n" % (i, i, i, i)
body += "    .ret ops.z" + ")" * (2 * k + 1) + "\n"
w("def rbody : Unit → Prog K (fun j => (CI j).M) V\n  | () =>\n" + body)
w("""
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
  rbody := rbody ops u
  comb := comb ops
  asm _ _ := True
  onehot _ _ := True
end
""")
# footprint: the site list
sites = ["⟨.sel, Reg.read⟩"] + sum([["⟨.a %d, Reg.read⟩" % i, "⟨.r %d, Reg.write⟩" % i] for i in range(k)], [])
w("def siteL : List (Σ j, (CI j).M) :=\n  [" + ",\n   ".join(sites) + "]\n")
w("""
def fp : Empty ⊕ Unit → List (Σ j, (CI j).M)
  | .inl m => m.elim
  | .inr _ => siteL

theorem siteL_nodup : (siteL.map Sigma.fst).Nodup := by decide

section
variable {V : Type} (ops : Ops V) (u : Fin %d → V)

/-- The site list, syntactically: one `Seq.callIf` per site. -/
theorem seq_go : Seq siteL (rbody ops u ()) := by
  refine .call fun s => ?_
""" % k)
for i in range(2 * k):
    w("  refine Seq.callIf fun _ => ?_\n")
w("  exact .ret _ _\n")
w("""
theorem nofail_go : NoFail (rbody ops u ()) := by
  refine .call fun s => ?_
""")
for i in range(2 * k):
    w("  refine NoFail.callIf fun _ => ?_\n")
w("  exact .ret _\n")

EN_ARG = """  en oc w j cm _ hcm := by
    simp only [List.mem_singleton, exists_eq_left, On, trS, trR, calls_iff_any, P, rbody, trace_callIf, trace]
    cases j with
    | sel => cases cm <;> simp [cregAnn, comb, regW, proj_append, proj_ite, proj_cons_self, proj_cons_ne, proj_nil] at hcm ⊢
    | a i =>
      obtain ⟨n, hn⟩ := i
      cases cm with
      | rd _ => simp [cregAnn] at hcm
      | wr _ =>
        simp only [comb, regW]
        match n, hn with
"""
EN_ARG += "".join("        | %d, _ => simp [proj_append, proj_ite, proj_cons_self, proj_cons_ne, proj_nil]\n" % i for i in range(k))
EN_ARG += "        | n + %d, h => exact absurd h (by omega)\n" % k
EN_ARG += """    | r i =>
      obtain ⟨n, hn⟩ := i
      cases cm with
      | rd _ => simp [cregAnn] at hcm
      | wr i' =>
        rw [fin1 i']
        simp only [comb, regW]
        match n, hn with
"""
EN_ARG += "".join("        | %d, _ => cases hb : ops.bit %d ((oc .sel).ret Reg.read) <;> simp [hb, proj_append, proj_ite, proj_cons_self, proj_cons_ne, proj_nil]\n" % (i, i) for i in range(k))
EN_ARG += "        | n + %d, h => exact absurd h (by omega)\n" % k
EN_ARG += """  enV _ _ m := m.elim
  arg oc w j cm _ s hs _ x hx := by
    simp only [List.mem_singleton] at hs
    subst hs
    simp only [trS, trR, P, rbody, trace_callIf, trace] at hx
    cases j with
    | sel => cases cm <;> simp [proj_append, proj_ite, proj_cons_self, proj_cons_ne, proj_nil] at hx <;> simp [comb, regW, hx]
    | a i =>
      obtain ⟨n, hn⟩ := i
      simp only [comb]
      match n, hn with
"""
EN_ARG += "".join("      | %d, _ => cases cm <;> simp [proj_append, proj_ite, proj_cons_self, proj_cons_ne, proj_nil] at hx <;> simp [regW, hx]\n" % i for i in range(k))
EN_ARG += "      | n + %d, h => exact absurd h (by omega)\n" % k
EN_ARG += """    | r i =>
      obtain ⟨n, hn⟩ := i
      simp only [comb]
      match n, hn with
"""
EN_ARG += "".join("      | %d, _ => cases cm <;> simp [proj_append, proj_ite, proj_cons_self, proj_cons_ne, proj_nil] at hx <;> simp_all [regW]\n" % i for i in range(k))
EN_ARG += "      | n + %d, h => exact absurd h (by omega)\n" % k
w("""
/-- A value read under `c` and used under `c` is the hardware's. -/
theorem cond_ctx (c : Bool) (f : V → Bool) (a b : V) : (c && f (if c then a else b)) = (c && f a) := by
  cases c <;> rfl

theorem ite_ctx (c : Bool) (a b : V) (h : c = true) : (if c then a else b) = a := by simp [h]

def cert : Cert (P ops u) where
  rules := [()]
  rules_nodup := by simp
  fire _ _ _ := true
  hit _ _ m _ := m.elim
  fp := fp
  users _ _ := [.inr ()]
  users_ok s p _ := by rcases s with m | r; exact m.elim; simp
  fp_ok := fp_ok_of_seq (P ops u) fp (fun m => m.elim) (fun _ => seq_go ops u)
  selfAnn m := m.elim
  fireIn _ _ r _ := by simp
  resM _ _ m := m.elim
  resR oc w r _ _ := trace_noFail oc (nofail_go ops u)
  rdyM _ _ m := m.elim
  rdyR oc w r _ _ c _ h := by simp [cregAnn] at h
  grpM _ _ m := m.elim
  grpR oc w r _ _ := grpT_of_seq (P ops u) oc (seq_go ops u) siteL_nodup
  mm _ _ m := m.elim
  mmg _ _ m := m.elim
  h1 _ _ m := m.elim
  h2 _ _ m := m.elim
  h3 _ _ m := m.elim
  h4 _ _ r1 r2 _ _ _ hne := by cases r1; cases r2; exact absurd rfl hne
"""
  + EN_ARG +
"""  alw oc w j cm _ h := by simp [cregAnn] at h
  onehot _ _ _ := trivial

/-- **The obligation**, for every value of the skipped sites. -/
theorem obl : Obl (P ops u) := obl_of_cert (P ops u) (cert ops u)
end
end Cond%d

#print axioms Cond%d.obl
""" % (k, k))
open(out, "w").write("".join(L))
