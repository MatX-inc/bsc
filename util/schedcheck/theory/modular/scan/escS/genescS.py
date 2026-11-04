"""Many dynamic facts in one escaped pair, in the linear form (cost review r8-2 M1), from
r8-cost2/tools/genescF.py.  Rule 0 has W sites; rule 1 reads under a condition `e z`.
  spread: rule 0 writes registers 0 .. W-1, rule 1 reads each once: W sites with one partner each;
  fan:    rule 0 writes register 0 once, rule 1 reads it at W sites: one site with W partners.
Either way the pair has W dynamic facts.  The check is the emitter's form (Scan.okT_except_fnS): bad
pairs keyed by first-step site (`badT`, leaves `(i, i2s)`), each site's partners in one pass (`subS`),
every table at most 128 entries per definition and every decision one per subtree (C20, C21).
  neg:    spread with the last site's partner left out of badT (must fail at escChk).
usage: genescS.py spread|fan|neg W OUT.lean"""
import sys
shape, W, out = sys.argv[1], int(sys.argv[2]), sys.argv[3]
SPLIT = 128
n = W if shape in ('spread', 'neg') else 1      # registers
K1 = W if shape in ('spread', 'neg') else 1     # sites of rule 0
K2 = W                                 # sites of rule 1
L = []
w = L.append


def tab_range(items, lo, hi, keys=None):
    if hi - lo == 1:
        return '(.leaf (%s))' % items[lo]
    mid = (lo + hi) // 2
    return '(.node %d %s %s)' % (keys[mid] if keys else mid, tab_range(items, lo, mid, keys), tab_range(items, mid, hi, keys))


def split(lo, hi):
    if hi - lo <= SPLIT:
        return ('chunk', lo, hi)
    mid = (lo + hi) // 2
    return ('node', mid, split(lo, mid), split(mid, hi))


def stab(name, ty, items, keys=None):
    st = split(0, len(items))

    def render(x):
        if x[0] == 'chunk':
            nm = '%s_%d_%d' % (name, x[1], x[2])
            w('def %s : %s := %s' % (nm, ty, tab_range(items, x[1], x[2], keys)))
            return nm
        return '(.node %d %s %s)' % (keys[x[1]] if keys else x[1], render(x[2]), render(x[3]))
    w('def %s : %s := %s' % (name, ty, render(st)))
    return st


def allB(thm, f, name, st, n_):
    def render(x):
        if x[0] == 'chunk':
            nm = '%s_%d_%d' % (thm, x[1], x[2])
            w('theorem %s : Tab.allB (%s) %s_%d_%d %d %d = true := by decide +kernel' % (nm, f, name, x[1], x[2], x[1], x[2]))
            return nm
        return '(Tab.allB_node %s %s)' % (render(x[2]), render(x[3]))
    w('theorem %s : Tab.allB (%s) %s 0 %d = true := %s' % (thm, f, name, n_, render(st)))


def pairs(items, lo, hi):
    if hi - lo == 1:
        return items[lo]
    mid = (lo + hi) // 2
    return '⟨%s, %s⟩' % (pairs(items, lo, mid), pairs(items, mid, hi))


def tall(st, leaves):
    if st[0] == 'chunk':
        return pairs(leaves, st[1], st[2])
    return '⟨%s, %s⟩' % (tall(st[2], leaves), tall(st[3], leaves))


def fin(i):
    return '(cI %d)' % i


if shape in ('spread', 'neg'):
    key1 = ['⟨%s, Reg.write⟩' % fin(i) for i in range(W)]
    key2 = ['⟨%s, Reg.read⟩' % fin(i) for i in range(W)]
    rd = ['(1, [%d])' % i for i in range(W)]
    bad_keys, bad = list(range(W)), ['(%d, [%d])' % (i, i) for i in range(W)]
    facts = ['⟨fun _ => c1 z e he _ _, trivial⟩'] * W
if shape == 'neg':   # negative control: the last site's partner missing from badT, so escChk must fail
    shape = 'spread'
    bad[-1] = '(%d, [])' % (W - 1)
    facts[-1] = 'trivial'
elif shape != 'spread':
    key1 = ['⟨%s, Reg.write⟩' % fin(0)]
    key2 = ['⟨%s, Reg.read⟩' % fin(0)] * W
    rd = ['(1, [%s])' % ', '.join(map(str, range(W)))]
    bad_keys, bad = [0], ['(0, [%s])' % ', '.join(map(str, range(W)))]
    facts = ['⟨' + ', '.join(['fun _ => c1 z e he _ _'] * W) + ', trivial⟩']
MK = lambda l: '#eval show IO Unit from do let t ← IO.monoMsNow; IO.println s!"@@ %s {t}"' % l
w('import Scan')
w(MK('start'))
for x in ['open Modular Scan Emit', 'set_option maxRecDepth 100000', 'set_option maxHeartbeats 0',
          'namespace EscS', 'noncomputable section', 'abbrev n : Nat := %d' % n,
          'abbrev CI : Fin n → Iface := fun _ => Reg.X',
          'def cI (i : Nat) : Fin n := ⟨i % n, Nat.mod_lt _ (by decide)⟩',
          'def noAnn : Ann Empty := ⟨fun _ _ => false, fun _ _ => false, fun _ _ => false, fun _ => false, fun _ => false, fun _ => false, fun _ => true, fun _ _ => false, fun _ _ => false, fun _ _ => false⟩']:
    w(x)
st1 = stab('key1', 'Tab (Σ j : Fin n, (CI j).M)', key1)
st2 = stab('key2', 'Tab (Σ j : Fin n, (CI j).M)', key2)
stab('rdT', 'Tab (Tab (Nat × List Nat))', ['.leaf %s' % r for r in rd])
stb = stab('badT', 'Tab (Nat × List Nat)', bad, bad_keys)
for x in ['def idx2 (p : Σ j : Fin n, (CI j).M) : List Nat :=',
          '  match p.2 with | .rd _ => idxT (rdT.get p.1.val) 1 | .wr _ => []',
          'variable {V : Type} (z : V) (e : V → Bool) (u : Nat → V)',
          'def keyS : Tab (Tab (Σ j : Fin n, (CI j).M)) := .node 1 (.leaf key1) (.leaf key2)',
          '/-- Rule 1 reads under `e z`. -/',
          'def sfS (k i : Nat) : SF V := bif Nat.beq k 1 then (fun _ => e z, fun _ => z) else (fun _ => true, fun _ => z)',
          'def siteS (k : Nat) : Nat → Site (Fin n) (fun j => (CI j).M) V := mkSite (keyS.get k).get (sfS z e k)',
          'def kS (k : Nat) : Nat := bif Nat.beq k 1 then %d else %d' % (K2, K1),
          'abbrev P : Parent V where',
          '  I := Fin n', '  decI := inferInstance', '  kids := List.finRange n', '  kids_nodup := List.nodup_finRange n',
          '  kids_all := List.mem_finRange', '  CI := CI', '  X := ⟨Empty, inferInstance, noAnn⟩', '  PR := Fin 2',
          '  body := fun m => m.elim',
          '  rbody := fun r => runSites u (fun _ => some z) (sitesF (siteS z e r.val) (kS r.val)) 0 u',
          '  comb := fun _ _ => (fun _ => ⟨fun _ => false, fun _ => z⟩, ⟨fun m => m.elim, fun m => m.elim⟩)',
          '  asm := fun _ _ => True', '  onehot := fun _ _ => True',
          'theorem rbS (k : Nat) : ∀ i, i < kS k → ReadsBefore i (siteS z e k i) := by',
          '  intro i hi ρ ρ\' h',
          '  show (sfS z e k i).1 ρ = (sfS z e k i).1 ρ\' ∧ (sfS z e k i).2 ρ = (sfS z e k i).2 ρ\'',
          '  unfold sfS',
          '  cases Nat.beq k 1 <;> exact ⟨rfl, rfl⟩',
          'variable (oc : (j : Fin n) → Outs (CI j).M V)',
          'theorem trS (r : Fin 2) : (trR (P z e u) oc r).1 = callsOf (envHw u oc (sitesF (siteS z e r.val) (kS r.val)) 0 u) (sitesF (siteS z e r.val) (kS r.val)) :=',
          '  congrArg Prod.fst (trace_fn u oc _ (siteS z e r.val) (kS r.val) (rbS z e r.val))',
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
          'theorem c1 (he : e z = false) (i : Nat) (ρ : Nat → V) : (siteS z e 1 i).cond ρ = true → False := by',
          '  intro h; have h\' : e z = true := h; rw [he] at h\'; cases h\'',
          MK('setup'), 'set_option profiler true']:
    w(x)
allB('chk2', 'fun i p => (idx2 p).contains i', 'key2', st2, K2)
w('/-- Every first-step site against every port of its child: may follow, or its partners in `badT`. -/')
allB('escChk', 'fun i p => ([Reg.read, Reg.write] : List (CRegM 1)).all (fun b => okP p.1 p.2 b || '
     'subS (idx2 ⟨p.1, b⟩) (idxT badT i))', 'key1', st1, K1)
w(MK('escChk'))
w('/-- **The escaped pair** with %d dynamic facts, one per bad site pair (%s). -/' % (W, shape))
w('theorem esc (he : e z = false) :')
w('    OkT (P z e u) (trR (P z e u) oc 0).1 (trR (P z e u) oc 1).1 := by')
w('  rw [trS z e u oc 0, trS z e u oc 1]')
w('  exact okT_except_fnS (P z e u) okP okP_follow (fun _ => [Reg.read, Reg.write]) ports_reg _ _ (siteS z e 0) (siteS z e 1)')
w('    (kS 0) (kS 1) key1 key2.get (fun _ => rfl) (fun _ => rfl) idx2')
w('    (fun i hi => by simpa using Tab.allB_spec _ key2 0 %d chk2 i (Nat.zero_le _) hi) badT escChk' % K2)
w('    ' + tall(stb, facts))
w('set_option profiler false')
w(MK('esc'))
for x in ['end', 'end EscS', '#print axioms EscS.esc']:
    w(x)
open(out, 'w').write('\n'.join(L) + '\n')
print('shape', shape, 'W', W, 'dynamic facts', W)
