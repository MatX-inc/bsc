"""An escaped pair in a site-list parent with K sites in the second rule.

genesc.py K OUT.lean

Children: registers 0..K (x = 0).  Rule 0 writes x.  Rule 1 reads a_1..a_K
unconditionally, then x under `e ρ0`.  The escape lemma `esc` proves the
pair's OkT from `e ((oc 1).ret read) = false` (the skel fact under fire),
via okT_except and calls_sites.  Only `esc` is the per-pair cost.
"""
import sys

K = int(sys.argv[1])
out = sys.argv[2]
n = K + 1
L = []
L.append('import Scan')
L.append('open Modular Scan')
L.append('set_option maxRecDepth 100000')
L.append('set_option maxHeartbeats 0')
L.append('namespace Esc%d' % K)
L.append('variable {V : Type} (z : V) (e : V → Bool) (u : Nat → V)')
L.append('abbrev CI : Fin %d → Iface := fun _ => Reg.X' % n)
L.append('def noAnn : Ann Empty := ⟨fun _ _ => false, fun _ _ => false, fun _ _ => false, fun _ => false, '
         'fun _ => false, fun _ => false, fun _ => true, fun _ _ => false, fun _ _ => false, fun _ _ => false⟩')
L.append('def ss0 : List (Site (Fin %d) (fun j => (CI j).M) V) := [⟨0, Reg.write, fun _ => true, fun _ => z⟩]' % n)
sites = ['⟨%d, Reg.read, fun _ => true, fun _ => z⟩' % (i + 1) for i in range(K)]
sites.append('⟨0, Reg.read, fun ρ => e (ρ 0), fun _ => z⟩')
L.append('def ss1 : List (Site (Fin %d) (fun j => (CI j).M) V) :=\n  [%s]' % (n, ',\n   '.join(sites)))
L.append('abbrev P : Parent V where')
L.append('  I := Fin %d' % n)
L.append('  decI := inferInstance')
L.append('  kids := List.finRange %d' % n)
L.append('  kids_nodup := List.nodup_finRange %d' % n)
L.append('  kids_all := List.mem_finRange')
L.append('  CI := CI')
L.append('  X := ⟨Empty, inferInstance, noAnn⟩')
L.append('  PR := Fin 2')
L.append('  body := fun m => m.elim')
L.append('  rbody := fun r => if r = 0 then runSites u (fun _ => some z) (ss0 z) 0 u else runSites u (fun _ => some z) (ss1 z e) 0 u')
L.append('  comb := fun _ _ => (fun _ => ⟨fun _ => false, fun _ => z⟩, ⟨fun m => m.elim, fun m => m.elim⟩)')
L.append('  asm := fun _ _ => True')
L.append('  onehot := fun _ _ => True')
# SitesOK: one rewrite per site that reads
ok1 = ['fun _ _ _ => ⟨rfl, rfl⟩'] * K + ['fun ρ ρ\' h => by simp only [h 0 (by decide), and_self]']
L.append('theorem ok0 : SitesOK (ss0 z) 0 := ⟨fun _ _ _ => ⟨rfl, rfl⟩, trivial⟩')
L.append('theorem ok1 : SitesOK (ss1 z e) 0 :=\n  ⟨%s, trivial⟩' % ',\n   '.join(ok1))
L.append('variable (oc : (j : Fin %d) → Outs (CI j).M V)' % n)
L.append('theorem rb0 : (P z e u).rbody 0 = runSites u (fun _ => some z) (ss0 z) 0 u := by simp [P]')
L.append('theorem rb1 : (P z e u).rbody 1 = runSites u (fun _ => some z) (ss1 z e) 0 u := by simp [P]')
L.append('theorem tr0 : trR (P z e u) oc 0 = (callsOf (envHw u oc (ss0 z) 0 u) (ss0 z), some z) := by')
L.append('  unfold trR; rw [rb0]; exact trace_runSites u oc (fun _ => some z) (ss0 z) 0 u (ok0 z)')
L.append('theorem tr1 : trR (P z e u) oc 1 = (callsOf (envHw u oc (ss1 z e) 0 u) (ss1 z e), some z) := by')
L.append('  unfold trR; rw [rb1]; exact trace_runSites u oc (fun _ => some z) (ss1 z e) 0 u (ok1 z e)')
L.append('theorem e0 : envHw u oc (ss1 z e) 0 u 0 = (oc 1).ret Reg.read :=')
L.append('  envHw_at u oc (ss1 z e) 0 u (ok1 z e) 0 (Nat.zero_lt_succ _)')
L.append('def okP (j : Fin %d) (a b : CRegM 1) : Bool := folP (cregAnn 1) a b && (!decide (a = b) || !(cregAnn 1).share a)' % n)
L.append('theorem okP_follow : ∀ (j : Fin %d) (a b : CRegM 1 × V), okP j a.1 b.1 = true → (cregAnn 1).Follow a b := by' % n)
L.append('  intro j a b h')
L.append('  simp only [okP, Bool.and_eq_true, Bool.or_eq_true, Bool.not_eq_true\', decide_eq_false_iff_not] at h')
L.append('  obtain ⟨hf, hs⟩ := h')
L.append('  simp only [folP, Bool.and_eq_true, Bool.or_eq_true, decide_eq_true_eq, Bool.not_eq_true\'] at hf')
L.append('  obtain ⟨⟨h11, h12⟩, h13⟩ := hf')
L.append('  refine ⟨fun hne => ?_, h12, h13, fun he hsh => ?_⟩')
L.append('  · rcases h11 with h | h')
L.append('    · exact absurd h hne')
L.append('    · exact h')
L.append('  · rcases hs with h | h')
L.append('    · exact absurd he h')
L.append('    · rw [hsh] at h; cases h')
L.append('theorem fx_reg : ∀ p : CRegM 1, p = CRegM.rd 0 ∨ p = CRegM.wr 0')
L.append('  | .rd ⟨0, _⟩ => .inl rfl')
L.append('  | .wr ⟨0, _⟩ => .inr rfl')
L.append('set_option profiler true')
L.append('/-- **The escaped pair**: rule 1 skips its read of `x` when rule 0 fires. -/')
L.append('theorem esc (he : e ((oc 1).ret Reg.read) = false) :')
L.append('    OkT (P z e u) (trR (P z e u) oc 0).1 (trR (P z e u) oc 1).1 := by')
L.append('  rw [tr0, tr1]')
L.append('  refine okT_except (P z e u) okP okP_follow _ _ ((ss0 z).map (fun s => ⟨s.j, s.m⟩)) ((ss1 z e).map (fun s => ⟨s.j, s.m⟩))')
L.append('    (fun c hc => (callsOf_ports _ _).subset (List.mem_map_of_mem hc))')
L.append('    (fun c hc => (callsOf_ports _ _).subset (List.mem_map_of_mem hc)) (fun j a b h1 _ hb _ hc2 => ?_)')
L.append('  have hj : j = 0 ∧ a = CRegM.wr 0 := by simpa [ss0] using h1')
L.append('  obtain ⟨rfl, rfl⟩ := hj')
L.append('  rcases fx_reg b with rfl | rfl')
L.append('  · obtain ⟨s, hs, hc⟩ := (calls_sites (P z e u) _ (ss1 z e) 0 (CRegM.rd 0)).mp hc2')
L.append('    have hf : (ss1 z e).filter (onPort (CM := fun j => (CI j).M) 0 (CRegM.rd 0)) = [⟨0, Reg.read, fun ρ => e (ρ 0), fun _ => z⟩] := rfl')
L.append('    rw [hf, List.mem_singleton] at hs')
L.append('    subst hs')
L.append('    have : e (envHw u oc (ss1 z e) 0 u 0) = true := hc')
L.append('    rw [e0, he] at this; cases this')
L.append('  · cases hb')
L.append('set_option profiler false')
L.append('end Esc%d' % K)
L.append('#print axioms Esc%d.esc' % K)
open(out, 'w').write('\n'.join(L) + '\n')
