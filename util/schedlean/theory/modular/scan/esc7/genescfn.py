"""An escaped pair in the numbered site-table form (okT_except_fn), on wide steps.
Rule 0 writes registers 0 .. W-1 (W sites); rule 1 reads registers W .. 2W-2 and, last, register 0
under the condition e(its first read). Rule 0's write of register 0 and rule 1's read of it fail
statically (read after write in a later rule); the escape's dynamic fact is that the condition is
false (the hypothesis `he`, standing for bsc's solver verdict).  usage: genescfn.py W OUT.lean"""
import sys
W, out = int(sys.argv[1]), sys.argv[2]
n = 2 * W


def tab(items):
    def go(lo, hi):
        if hi - lo == 1:
            return '(.leaf (%s))' % items[lo]
        mid = (lo + hi) // 2
        return '(.node %d %s %s)' % (mid, go(lo, mid), go(mid, hi))
    return go(0, len(items))


def fin(i):
    return '(cI %d)' % i


key1 = ['⟨%s, Reg.write⟩' % fin(i) for i in range(W)]
key2 = ['⟨%s, Reg.read⟩' % fin(W + i) for i in range(W - 1)] + ['⟨%s, Reg.read⟩' % fin(0)]
# rule 1's uses per child (read port): (rule, sites); the write port has none
rd = ['(.leaf (0, []))'] * n
rd[0] = '(.leaf (1, [%d]))' % (W - 1)
for i in range(W - 1):
    rd[W + i] = '(.leaf (1, [%d]))' % i
MK = lambda l: '#eval show IO Unit from do let t ← IO.monoMsNow; IO.println s!"@@ %s {t}"' % l
L = ['import Scan', MK('start'), 'open Modular Scan Emit', 'set_option maxRecDepth 100000', 'set_option maxHeartbeats 0',
     'namespace EscFn', 'noncomputable section',
     'abbrev n : Nat := %d' % n,
     'abbrev CI : Fin n → Iface := fun _ => Reg.X',
     'def cI (i : Nat) : Fin n := ⟨i % n, Nat.mod_lt _ (by decide)⟩',
     'def noAnn : Ann Empty := ⟨fun _ _ => false, fun _ _ => false, fun _ _ => false, fun _ => false, fun _ => false, fun _ => false, fun _ => true, fun _ _ => false, fun _ _ => false, fun _ _ => false⟩',
     'def key1 : Tab (Σ j : Fin n, (CI j).M) := ' + tab(key1),
     'def key2 : Tab (Σ j : Fin n, (CI j).M) := ' + tab(key2),
     'def rdT : Tab (Tab (Nat × List Nat)) := ' + tab(rd),
     '/-- Rule 1\'s sites on a port: a lookup in its uses table (no walk). -/',
     'def idx2 (p : Σ j : Fin n, (CI j).M) : List Nat :=',
     '  match p.2 with | .rd _ => idxT (rdT.get p.1.val) 1 | .wr _ => []',
     'variable {V : Type} (z : V) (e : V → Bool) (u : Nat → V)',
     'def keyS : Tab (Tab (Σ j : Fin n, (CI j).M)) := .node 1 (.leaf key1) (.leaf key2)',
     '/-- Site functions by rule: rule 1\'s last site reads register 0 under `e` of its first read. -/',
     'def sfS (k i : Nat) : SF V := bif Nat.beq k 1 && Nat.beq i %d then (fun ρ => e (ρ 0), fun _ => z) else (fun _ => true, fun _ => z)' % (W - 1),
     '/-- Rule `k`\'s sites, through tables, so that no proof unfolds a rule\'s program. -/',
     'def siteS (k : Nat) : Nat → Site (Fin n) (fun j => (CI j).M) V := mkSite (keyS.get k).get (sfS z e k)',
     'abbrev P : Parent V where',
     '  I := Fin n', '  decI := inferInstance', '  kids := List.finRange n', '  kids_nodup := List.nodup_finRange n',
     '  kids_all := List.mem_finRange', '  CI := CI', '  X := ⟨Empty, inferInstance, noAnn⟩', '  PR := Fin 2',
     '  body := fun m => m.elim',
     '  rbody := fun r => runSites u (fun _ => some z) (sitesF (siteS z e r.val) %d) 0 u' % W,
     '  comb := fun _ _ => (fun _ => ⟨fun _ => false, fun _ => z⟩, ⟨fun m => m.elim, fun m => m.elim⟩)',
     '  asm := fun _ _ => True', '  onehot := fun _ _ => True',
     'theorem rbS (k : Nat) : ∀ i, i < %d → ReadsBefore i (siteS z e k i) := by' % W,
     '  intro i hi ρ ρ\' h',
     '  show (sfS z e k i).1 ρ = (sfS z e k i).1 ρ\' ∧ (sfS z e k i).2 ρ = (sfS z e k i).2 ρ\'',
     '  unfold sfS',
     '  cases hb : (Nat.beq k 1 && Nat.beq i %d)' % (W - 1),
     '  · exact ⟨rfl, rfl⟩',
     '  · have : i = %d := Nat.eq_of_beq_eq_true ((Bool.and_eq_true _ _).mp hb).2' % (W - 1),
     '    exact ⟨congrArg e (h 0 (by omega)), rfl⟩',
     'variable (oc : (j : Fin n) → Outs (CI j).M V)',
     'theorem trS (r : Fin 2) : (trR (P z e u) oc r).1 = callsOf (envHw u oc (sitesF (siteS z e r.val) %d) 0 u) (sitesF (siteS z e r.val) %d) :=' % (W, W),
     '  congrArg Prod.fst (trace_fn u oc _ (siteS z e r.val) %d (rbS z e r.val))' % W,
     'def okP (j : Fin n) (a b : CRegM 1) : Bool := folP (cregAnn 1) a b && (!decide (a = b) || !(cregAnn 1).share a)',
     'theorem okP_follow : ∀ (j : Fin n) (a b : CRegM 1 × V), okP j a.1 b.1 = true → (cregAnn 1).Follow a b := by',
     '  intro j a b h',
     '  simp only [okP, Bool.and_eq_true, Bool.or_eq_true, Bool.not_eq_true\', decide_eq_false_iff_not] at h',
     '  obtain ⟨hf, hs⟩ := h',
     '  simp only [folP, Bool.and_eq_true, Bool.or_eq_true, decide_eq_true_eq, Bool.not_eq_true\'] at hf',
     '  obtain ⟨⟨h11, h12⟩, h13⟩ := hf',
     '  refine ⟨fun hne => ?_, h12, h13, fun he hsh => ?_⟩',
     '  · rcases h11 with h | h', '    · exact absurd h hne', '    · exact h',
     '  · rcases hs with h | h', '    · exact absurd he h', '    · rw [hsh] at h; cases h',
     'theorem fin1 (i : Fin 1) : i = 0 := by obtain ⟨k, hk⟩ := i; congr; omega',
     'theorem ports_reg : ∀ (j : Fin n) (p : CRegM 1), p ∈ [Reg.read, Reg.write]',
     '  | _, .rd i => by rw [fin1 i]; simp', '  | _, .wr i => by rw [fin1 i]; simp',
     MK('setup'),
     'set_option profiler true',
     '/-- Rule 1\'s sites are in its index lists (decided, one lookup per site). -/',
     'theorem chk2 : Tab.allB (fun i p => (idx2 p).contains i) key2 0 %d = true := by decide +kernel' % W,
     '/-- Every first-step site against every port of its child: may follow, or the listed bad pair. -/',
     'theorem escChk : Tab.allB (fun i p => ([Reg.read, Reg.write] : List (CRegM 1)).all (fun b => okP p.1 p.2 b ||',
     '    (idx2 ⟨p.1, b⟩).all (fun i2 => ([(0, %d)] : List (Nat × Nat)).contains (i, i2)))) key1 0 %d = true := by decide +kernel' % (W - 1, W),
     'theorem ev2 : envHw u oc (sitesF (siteS z e 1) %d) 0 u 0 = (oc %s).ret Reg.read :=' % (W, fin(W)),
     '  (envHw_fn u oc (siteS z e 1) %d (rbS z e 1) 0 (by decide)).trans rfl' % W,
     '/-- **The escaped pair** over numbered site tables: rule 1 skips its read of register 0. -/',
     'theorem esc (he : e ((oc %s).ret Reg.read) = false) :' % fin(W),
     '    OkT (P z e u) (trR (P z e u) oc 0).1 (trR (P z e u) oc 1).1 := by',
     '  rw [trS z e u oc 0, trS z e u oc 1]',
     '  refine okT_except_fn (P z e u) okP okP_follow (fun _ => [Reg.read, Reg.write]) ports_reg _ _ (siteS z e 0) (siteS z e 1)',
     '    %d %d key1 key2.get (fun _ => rfl) (fun _ => rfl) idx2' % (W, W),
     '    (fun i hi => by simpa using Tab.allB_spec _ key2 0 %d chk2 i (Nat.zero_le _) hi) [(0, %d)] escChk ⟨fun _ h2 => ?_, trivial⟩' % (W, W - 1),
     '  have h2\' : e (envHw u oc (sitesF (siteS z e 1) %d) 0 u 0) = true := h2' % W,
     '  rw [ev2, he] at h2\'',
     '  cases h2\'',
     'set_option profiler false', MK('esc'),
     'end', 'end EscFn', '#print axioms EscFn.esc']
open(out, 'w').write('\n'.join(L) + '\n')
print('W', W, 'sites per step', W)
