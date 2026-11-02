"""Emit one corpus module's modular obligation file (emit/conventions.md, sites/rule.md).

usage: python3 -I mke2e.py DUMP BA OUT.lean NAME [NSTEPS]

Structure (children, kinds, annotations, steps, sites, users, exclusions) is
the design's; data and guards are opaque operators over their real fan-in.
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import design  # noqa: E402
import model   # noqa: E402

dump, ba, out, NAME = sys.argv[1:5]
NST = int(sys.argv[5]) if len(sys.argv) > 5 else None
D = design.Design(dump, ba)
M = model.Model(D, NST)
need, Dn = M.scan_masks()
U = M.users()
K = M.K
nR = M.nR

L = []
w = L.append
MARK = os.environ.get('MARK') == '1'


def mark(label):
    if MARK:
        w('\n#eval show IO Unit from do let t ← IO.monoMsNow; let h ← IO.getNumHeartbeats; '
          'IO.println s!"@@ %s {t} {h}"\n' % label)


def tab(items, ind='  '):
    def go(lo, hi):
        if hi - lo == 1:
            return '(.leaf (%s))' % items[lo]
        mid = (lo + hi) // 2
        return '(.node %d %s\n%s%s)' % (mid, go(lo, mid), ind, go(mid, hi))
    return go(0, len(items))


def tabproof(items, ind='  '):
    def go(lo, hi):
        if hi - lo == 1:
            return 'Tab.one (%s)' % items[lo]
        mid = (lo + hi) // 2
        return '⟨%s,\n%s%s⟩' % (go(lo, mid), ind, go(mid, hi))
    return go(0, len(items))


def nat(x):
    return '%d' % x


def allL(items):
    if not items:
        return 'trivial'
    return '⟨' + ', '.join(items + ['trivial']) + '⟩'


def tup(xs, z='ops.z'):
    if not xs:
        return z
    if len(xs) == 1:
        return xs[0]
    return 'ops.tup (%s) (%s)' % (xs[0], tup(xs[1:]))


def tupcong(cs):
    """Congruence for tup over equalities cs (each a term)."""
    if not cs:
        return 'rfl'
    if len(cs) == 1:
        return cs[0]
    return 'congr2 ops.tup (%s) (%s)' % (cs[0], tupcong(cs[1:]))


def ocread(p):
    kd, i, m = p
    return '(oc (%s %d)).ret %s' % (M.ihelper(kd), i, K[kd].lports[K[kd].code(m)])


def oc2read(p):
    kd, i, m = p
    return "(oc' (%s %d)).ret %s" % (M.ihelper(kd), i, K[kd].lports[K[kd].code(m)])


# per rule: read site index -> port
def site_port(k, i):
    return M.rules[k]['sites'][i]['port']


def arg_oc(k, a):
    """A call's argument over the hardware's reads."""
    if a[0] == 'z':
        return 'ops.z'
    if a[0] == 'k':
        return 'ops.k %d' % a[1]
    return 'ops.f %d (%s)' % (a[1], tup([ocread(site_port(k, i)) for i in a[2]]))


def arg_rho(a):
    if a[0] == 'z':
        return 'ops.z'
    if a[0] == 'k':
        return 'ops.k %d' % a[1]
    return 'ops.f %d (%s)' % (a[1], tup(['ρ %d' % i for i in a[2]]))


def ev(k, i):
    return 'ev_%d_%d ops E u oc' % (k, i)


def arg_val(k, a):
    """Proof: the site's argument at the hardware's environment is arg_oc."""
    if a[0] in ('z', 'k'):
        return 'rfl'
    return 'congrArg (ops.f %d) (%s)' % (a[1], tupcong([ev(k, i) for i in a[2]]))


def tcond_rho(c):
    return 'true' if c[0] == 'true' else 'ops.g %d (%s)' % (c[1], tup(['ρ %d' % i for i in c[2]]))


def tcond_oc(k, c):
    if c[0] == 'true':
        return 'true'
    if c[0] == 'chk':
        return '(gT.get %d) V ops E oc' % k
    return 'ops.g %d (%s)' % (c[1], tup([ocread(site_port(k, i)) for i in c[2]]))


def tcond_val(k, c):
    """Proof: the site's condition at the hardware's environment is the hardware's."""
    if c[0] == 'true':
        return 'rfl'
    return 'congrArg (ops.g %d) (%s)' % (c[1], tupcong([ev(k, i) for i in c[2]]))


def targ_rho(args):
    return tup([arg_rho(a) for a in args])


def targ_oc(k, args):
    return tup([arg_oc(k, a) for a in args])


def targ_val(k, args):
    return tupcong([arg_val(k, a) for a in args])


def grd_pos(k):
    """Site positions of rule k's guard reads."""
    pos = {}
    for i, st in enumerate(M.rules[k]['sites']):
        if st['kind'] == 'read':
            pos.setdefault(st['port'], i)
    return [pos[p] for p in M.rules[k]['guard'][2]]


evneed = set()
for k, r in enumerate(M.rules):
    for st in r['sites']:
        if st['kind'] == 'call' and st['arg'][0] == 'f':
            for i in st['arg'][2]:
                evneed.add((k, i))
        if st['kind'] == 'task':
            if st['cond'][0] == 'g':
                evneed.update((k, i) for i in st['cond'][2])
            for a in st['args']:
                if a[0] == 'f':
                    evneed.update((k, i) for i in a[2])
    if r['guard'] and r['guard'][0] == 'g':
        evneed.update((k, i) for i in grd_pos(k))
nT = M.nT

# ---------------------------------------------------------------- header
w('''/- %s: the modular obligation of one corpus module, emitted by e2e/tools/mke2e.py.
   Structure (children, steps, sites, users, exclusions) is the design's; data and
   guards are opaque operators over their real fan-in (see e2e/README.md). -/
import Scan
open Modular Emit Scan
open scoped List

set_option autoImplicit false
set_option linter.unusedVariables false
set_option linter.unusedSimpArgs false
set_option maxRecDepth 100000
set_option maxHeartbeats 0

namespace %s

/-- The module's operators: constants, opaque data and Boolean operators (by number), tuples. -/
structure Ops (V : Type) where
  z : V
  tup : V → V → V
  k : Nat → V
  f : Nat → V → V
  g : Nat → V → Bool
  ofBool : Bool → V
''' % (NAME, NAME))

# ---------------------------------------------------------------- interfaces


def sched_lean(sl, ctor):
    rel = {'CF': '.CF', 'SB': '.SB', 'SBR': '.SBR', 'C': '.C'}
    items = []
    for ps, r, qs in sl:
        items.append('([%s], %s, [%s])' % (', '.join(ctor + p for p in ps), rel[r], ', '.join(ctor + q for q in qs)))
    return '[' + ',\n   '.join(items) + ']'


w('/-! ## Child interfaces: the library\'s, and two assumed kinds from the dump\'s schedules -/\n')
if 'fifo' in M.kinds:
    w('''/-- FIFOL1's ports. -/
inductive FL where
  | enq | deq | first | notFull | i_notFull | notEmpty | i_notEmpty | clear
  deriving DecidableEq

/-- `FIFOL1`'s schedule, as the dump prints it. -/
def flSched : List (List FL × Rel × List FL) :=
  %s

/-- FIFOL1 (assumed leaf): its schedule; `deq` has paths to `enq`'s ready and the full flags. -/
def flAnn : Ann FL :=
  { Ann.ofSched flSched (fun m => match m with | .enq | .deq | .clear => false | _ => true)
      (fun m => match m with | .enq | .deq | .clear => true | _ => false) (fun _ => false)
      (fun m => match m with | .enq | .deq | .first => false | _ => true) with
    path := fun p q => match p, q with
      | .deq, .enq | .deq, .notFull | .deq, .i_notFull | .deq, .clear | .enq, .clear => true
      | _, _ => false }

abbrev FL.X : Iface := ⟨FL, inferInstance, flAnn⟩
''' % sched_lean(M.fifo_sched, '.'))
if 'cfg' in M.kinds:
    w('''/-- ConfigRegN's ports. -/
inductive CfgM where
  | read | write
  deriving DecidableEq

/-- ConfigRegN (assumed leaf): `read` returns the cycle's start value. -/
def cfgAnn : Ann CfgM :=
  Ann.ofSched %s (fun m => m == .read) (fun m => m == .write) (fun _ => false) (fun _ => true)

abbrev Cfg.X : Iface := ⟨CfgM, inferInstance, cfgAnn⟩
''' % sched_lean(M.cfg_sched, '.'))

if nT:
    w('/-- The module\'s task statements, numbered in rank order (trust point: bsc\'s Verilog order). -/\n'
      'abbrev nT : Nat := %d\ndef tP (t : Nat) : Fin nT := ⟨t %% nT, Nat.mod_lt _ (by decide)⟩\n' % nT)
w('inductive Kind where\n  | ' + ' | '.join(M.kinds) + '\n  deriving DecidableEq\n')
w('abbrev nk : Kind → Nat\n' + ''.join('  | .%s => %d\n' % (k, len(M.byk[k])) for k in M.kinds))
w('abbrev KI : Kind → Iface\n' + ''.join('  | .%s => %s\n' % (k, K[k].lean_iface) for k in M.kinds))
w('''abbrev I := Σ κ : Kind, Fin (nk κ)
abbrev CI : I → Iface := fun j => KI j.1
abbrev OC (V : Type) := (j : I) → Outs (CI j).M V
''')
for k in M.kinds:
    w('def %s (i : Nat) : I := ⟨.%s, ⟨i %% nk .%s, Nat.mod_lt _ (by decide)⟩⟩' % (M.ihelper(k), k, k))
w('''
def xAnn : Ann Empty where
  prec e _ := e.elim
  sbr e _ := e.elim
  conf e _ := e.elim
  share e := e.elim
  hasEn e := e.elim
  alwaysEn e := e.elim
  alwaysRdy e := e.elim
abbrev X : Iface := ⟨Empty, inferInstance, xAnn⟩

abbrev nR : Nat := %d
abbrev St := Empty ⊕ Fin nR
/-- Rule `k` (rules are numbered in schedule order). -/
def rR (k : Nat) : Fin nR := ⟨k %% nR, Nat.mod_lt _ (by decide)⟩
theorem rR_val (r : Fin nR) : rR r.val = r := Fin.ext (Nat.mod_eq_of_lt r.isLt)
''' % nR)

# library-kind schedule checks
for kd, lst, mtype, ann in [('reg', None, 'CRegM 1', 'cregAnn 1')]:
    pass

w('''/-! ## Per-kind builders, port splits, uses -/

def regW {V : Type} (en : Bool) (d z : V) : Wires (CRegM 1) V :=
  ⟨fun m => match m with | .wr _ => en | .rd _ => false, fun m => match m with | .wr _ => d | .rd _ => z⟩
def wireW {V : Type} (en : Bool) (d z : V) : Wires WireM V :=
  ⟨fun m => match m with | .wset => en | _ => false, fun m => match m with | .wset => d | _ => z⟩
def wire0W {V : Type} (en : Bool) (z : V) : Wires Wire0M V :=
  ⟨fun m => match m with | .wset => en | _ => false, fun _ => z⟩
theorem fin1 (i : Fin 1) : i = 0 := by obtain ⟨k, hk⟩ := i; congr; omega
theorem regAll {Q : CRegM 1 → Prop} (hr : Q Reg.read) (hw : Q Reg.write) : ∀ cm, Q cm := by
  intro cm; cases cm with
  | rd i => rw [fin1 i]; exact hr
  | wr i => rw [fin1 i]; exact hw
theorem wireAll {Q : WireM → Prop} (hs : Q .wset) (hg : Q .wget) (hh : Q .whas) : ∀ cm, Q cm := by
  intro cm; cases cm <;> assumption
theorem reg_nopath (p q : CRegM 1) : (cregAnn 1).path p q = false := by
  cases p <;> cases q <;> rename_i a b <;> rw [fin1 a, fin1 b] <;> rfl
theorem wire0All {Q : Wire0M → Prop} (hs : Q .wset) (hh : Q .whas) : ∀ cm, Q cm := by
  intro cm; cases cm <;> assumption

structure Uses (M : Type) where
  uses : M → List (Nat × Nat)
def regU (rd wr : List (Nat × Nat)) : Uses (CRegM 1) := ⟨fun m => match m with | .rd _ => rd | .wr _ => wr⟩
def wireU (s g h : List (Nat × Nat)) : Uses WireM := ⟨fun m => match m with | .wset => s | .wget => g | .whas => h⟩
def wire0U (s h : List (Nat × Nat)) : Uses Wire0M := ⟨fun m => match m with | .wset => s | .whas => h⟩
''')
if 'cfg' in M.kinds:
    w('''def cfgW {V : Type} (en : Bool) (d z : V) : Wires CfgM V :=
  ⟨fun m => match m with | .write => en | .read => false, fun m => match m with | .write => d | .read => z⟩
theorem cfgAll {Q : CfgM → Prop} (hr : Q .read) (hw : Q .write) : ∀ cm, Q cm := by
  intro cm; cases cm <;> assumption
def cfgU (rd wr : List (Nat × Nat)) : Uses CfgM := ⟨fun m => match m with | .read => rd | .write => wr⟩
''')
if 'fifo' in M.kinds:
    w('''def fifoW {V : Type} (eE dE cE : Bool) (d z : V) : Wires FL V :=
  ⟨fun m => match m with | .enq => eE | .deq => dE | .clear => cE | _ => false,
   fun m => match m with | .enq => d | _ => z⟩
theorem flAll {Q : FL → Prop} (h1 : Q .enq) (h2 : Q .deq) (h3 : Q .first) (h4 : Q .notFull) (h5 : Q .i_notFull)
    (h6 : Q .notEmpty) (h7 : Q .i_notEmpty) (h8 : Q .clear) : ∀ cm, Q cm := by
  intro cm; cases cm <;> assumption
def fifoU (enq deq first nf inf ne ine clr : List (Nat × Nat)) : Uses FL :=
  ⟨fun m => match m with
    | .enq => enq | .deq => deq | .first => first | .notFull => nf | .i_notFull => inf
    | .notEmpty => ne | .i_notEmpty => ine | .clear => clr⟩
''')


def uses_list(p):
    return '[' + ', '.join('(%d, %d)' % (k, i) for k, i in U.get(p, [])) + ']'


for kd in M.kinds:
    if kd == 'tasks':
        w('def tusesT : Tab (List (Nat × Nat)) :=\n  %s' % tab([uses_list(('tasks', 0, t)) for t in range(nT)]))
        w('def uses_tasks : Tab (Uses (KI .tasks).M) := .leaf ⟨fun t => tusesT.get t.val⟩')
        continue
    items = []
    for i in range(len(M.byk[kd])):
        args = ' '.join(uses_list((kd, i, m)) for m in K[kd].ports)
        items.append('%s %s' % (K[kd].usesc, args))
    w('def uses_%s : Tab (Uses (KI .%s).M) :=\n  %s' % (kd, kd, tab(items)))
w('def usesK : (κ : Kind) → Tab (Uses (KI κ).M)\n' + ''.join('  | .%s => uses_%s\n' % (k, k) for k in M.kinds))
w('''def usesAt (p : Σ j : I, (CI j).M) : List (Nat × Nat) := ((usesK p.1.1).get p.1.2.val).uses p.2
/-- A port's uses are (rule number, site) pairs: numbers compare by `Nat.beq`. -/
def idxN (uses : List (Nat × Nat)) (k : Nat) : List Nat :=
  uses.filterMap (fun e => bif Nat.beq e.1 k then some e.2 else none)
def usersN (uses : List (Nat × Nat)) : List St := uses.map (fun e => .inr (rR e.1))
theorem mem_usersN : ∀ {uses : List (Nat × Nat)} {k i : Nat}, i ∈ idxN uses k → (.inr (rR k) : St) ∈ usersN uses
  | [], _, _, h => by simp [idxN] at h
  | e :: l, k, i, h => by
    simp only [usersN, List.map_cons, List.mem_cons]
    cases he : Nat.beq e.1 k
    · right
      have h' : i ∈ idxN l k := by
        unfold idxN at h ⊢; rw [List.filterMap_cons] at h; simp only [he, Bool.cond_false] at h; exact h
      exact (mem_usersN h' : _ ∈ usersN l)
    · left; rw [Nat.eq_of_beq_eq_true he]
def users (j : I) (cm : (CI j).M) : List St := usersN (usesAt ⟨j, cm⟩)
def idx (s : St) (p : Σ j : I, (CI j).M) : List Nat := match s with
  | .inl e => e.elim
  | .inr r => idxN (usesAt p) r.val
''')
mark('uses')

# ---------------------------------------------------------------- site tables
w('/-! ## Steps as numbered site tables -/\n')
for k, r in enumerate(M.rules):
    keys = [M.lport(st['port']) for st in r['sites']] or ['⟨%s 0, %s⟩' % (M.ihelper(M.kinds[0]), K[M.kinds[0]].lports[0])]
    w('def key_%d : Tab (Σ j : I, (CI j).M) :=\n  %s' % (k, tab(keys)))
w('def keyT : Tab (Tab (Σ j : I, (CI j).M)) :=\n  %s' % tab(['key_%d' % k for k in range(nR)]))
w('def kT : Tab Nat :=\n  %s' % tab([nat(len(r['sites'])) for r in M.rules]))
w('abbrev kR (k : Nat) : Nat := kT.get k\n')
w('''section
variable {V : Type}
/-- An unconditional site that passes the dummy argument (reads, calls without data). -/
def rdSF (V : Type) (ops : Ops V) (E : EqC V) : SF V := (fun _ => true, fun _ => ops.z)
end
''')


def sf_leaf(st):
    if st['kind'] == 'task':
        return 'fun V (ops : Ops V) (E : EqC V) => ((fun ρ => %s, fun ρ => %s) : SF V)' % (
            tcond_rho(st['cond']), targ_rho(st['args']))
    if st['kind'] == 'read' or st['arg'][0] == 'z':
        return 'rdSF'
    return 'fun V (ops : Ops V) (E : EqC V) => ((fun _ => true, fun ρ => %s) : SF V)' % arg_rho(st['arg'])


for k, r in enumerate(M.rules):
    leaves = [sf_leaf(st) for st in r['sites']] or ['rdSF']
    w('def sf_%d : Tab ((V : Type) → Ops V → EqC V → SF V) :=\n  %s' % (k, tab(leaves)))
w('def sfT : Tab (Tab ((V : Type) → Ops V → EqC V → SF V)) :=\n  %s' % tab(['sf_%d' % k for k in range(nR)]))
w('''
section
variable {V : Type}
def siteR (ops : Ops V) (E : EqC V) (k : Nat) : Nat → Site I (fun j => (CI j).M) V :=
  mkSite (keyT.get k).get (fun i => (sfT.get k).get i V ops E)
end
''')


def res_leaf(k):
    g = M.rules[k]['guard']
    if g and g[0] == 'g':
        return ('fun V (ops : Ops V) (E : EqC V) (ρ : Nat → V) => bif ops.g %d (%s) then some ops.z else none'
                % (g[1], tup(['ρ %d' % i for i in grd_pos(k)])))
    return 'fun V (ops : Ops V) (E : EqC V) (ρ : Nat → V) => some ops.z'


w('/-- Each rule\'s result: its explicit guard, read at its position (run-time check rules: `some z`). -/')
w('def resT : Tab ((V : Type) → Ops V → EqC V → (Nat → V) → Option V) :=\n  %s' % tab([res_leaf(k) for k in range(nR)]))
mark('sites')

# ---------------------------------------------------------------- guards and firing
w('/-! ## The hardware: guards, firing, run-time checks -/\n')
w('''def alwaysRdyB (p : Σ j : I, (CI j).M) : Bool := (CI p.1).ann.alwaysRdy p.2

section
variable {V : Type}
/-- The ready signals of a rule's guarded calls, ANDed into its CAN_FIRE (calls are unconditional). -/
def RD (oc : OC V) (k : Nat) : Bool :=
  (List.range' 0 (kR k)).all (fun i => alwaysRdyB ((keyT.get k).get i) || (oc ((keyT.get k).get i).1).rdy ((keyT.get k).get i).2)
''')
grd_reads = {}
for k, r in enumerate(M.rules):
    g = r['guard']
    if g[0] == 'g':
        grd_reads[k] = g[2]
        w('def G_%d (V : Type) (ops : Ops V) (E : EqC V) (oc : OC V) : Bool := ops.g %d (%s)'
          % (k, g[1], tup([ocread(p) for p in g[2]])))
w('def bTrue (V : Type) (ops : Ops V) (E : EqC V) (oc : OC V) : Bool := true')
# WFd in dependency order (blockers first)
done = set()
order = []


def visit(k):
    if k in done:
        return
    if k in M.blk:
        visit(M.blk[k])
    done.add(k)
    order.append(k)


for k in range(nR):
    if M.rules[k]['guard'][0] == 'g':
        visit(k)
for k in order:
    if k in M.blk:
        w('def B_%d (V : Type) (ops : Ops V) (E : EqC V) (oc : OC V) : Bool := !(WFd_%d V ops E oc)' % (k, M.blk[k]))
    bname = ('B_%d' % k) if k in M.blk else 'bTrue'
    w('def WFd_%d (V : Type) (ops : Ops V) (E : EqC V) (oc : OC V) : Bool := (RD oc %d && G_%d V ops E oc) && %s V ops E oc'
      % (k, k, k, bname))
for k, r in enumerate(M.rules):
    g = r['guard']
    if g[0] == 'check':
        ch = M.checks[g[1]]
        w('def G_%d (V : Type) (ops : Ops V) (E : EqC V) (oc : OC V) : Bool := WFd_%d V ops E oc && orL [%s]'
          % (k, ch['head'], ', '.join('WFd_%d V ops E oc' % y for y in ch['lst'])))
    elif g[0] == 'false':
        w('def G_%d (V : Type) (ops : Ops V) (E : EqC V) (oc : OC V) : Bool := false' % k)
w('end\n')
w('def gT : Tab ((V : Type) → Ops V → EqC V → OC V → Bool) :=\n  %s' % tab(['G_%d' % k for k in range(nR)]))
w('def bT : Tab ((V : Type) → Ops V → EqC V → OC V → Bool) :=\n  %s'
  % tab([('B_%d' % k) if k in M.blk else 'bTrue' for k in range(nR)]))
nC = len(M.checks)
nD = len(M.disj)
w('def chkHead : Tab Nat :=\n  %s' % tab([nat(c['head']) for c in M.checks] or ['0']))
w('def chkLst : Tab (List Nat) :=\n  %s' % tab(['[' + ', '.join(map(str, c['lst'])) + ']' for c in M.checks] or ['[]']))
w('abbrev nC : Nat := %d' % nC)
w('def dAT : Tab Nat :=\n  %s' % tab([nat(a) for a, b in M.disj] or ['0']))
w('def dBT : Tab Nat :=\n  %s' % tab([nat(b) for a, b in M.disj] or ['0']))
w('abbrev nD : Nat := %d' % nD)
w('''
section
variable {V : Type}
/-- CAN_FIRE: the guarded calls' ready signals and the rule's guard. -/
def CF (ops : Ops V) (E : EqC V) (oc : OC V) (k : Nat) : Bool := RD oc k && (gT.get k) V ops E oc
/-- WILL_FIRE: CAN_FIRE and not blocked by a more urgent rule. -/
def WF (ops : Ops V) (E : EqC V) (oc : OC V) (k : Nat) : Bool := CF ops E oc k && (bT.get k) V ops E oc
def fire (ops : Ops V) (E : EqC V) : OC V → Wires Empty V → Fin nR → Bool := fun oc _ r => WF ops E oc r.val
def stepOn (ops : Ops V) (E : EqC V) (oc : OC V) : St → Bool
  | .inl e => e.elim
  | .inr r => WF ops E oc r.val
/-- The module's assertions: bsc's run-time checks of `mutually_exclusive` (never `WF h && (WF y || …)`). -/
def asmP (ops : Ops V) (E : EqC V) (oc : OC V) : Prop :=
  ∀ c, Nat.blt c nC = true → (WF ops E oc (chkHead.get c) && orL ((chkLst.get c).map (WF ops E oc))) = false
/-- bsc's disjointness verdicts (solver facts, hypotheses as today): CAN_FIRE of each pair. -/
structure Hyps (ops : Ops V) (E : EqC V) : Prop where
  disj : ∀ (oc : OC V) (d : Nat), Nat.blt d nD = true → (CF ops E oc (dAT.get d) && CF ops E oc (dBT.get d)) = false

theorem cf_of_wf {ops : Ops V} {E : EqC V} {oc : OC V} {k : Nat} (h : WF ops E oc k = true) : CF ops E oc k = true :=
  ((Bool.and_eq_true _ _).mp h).1
theorem rd_of_cf {ops : Ops V} {E : EqC V} {oc : OC V} {k : Nat} (h : CF ops E oc k = true) : RD oc k = true :=
  ((Bool.and_eq_true _ _).mp h).1

theorem meX (ops : Ops V) (E : EqC V) (oc : OC V) (hA : asmP ops E oc) (c p : Nat) {y : Nat}
    (hc : Nat.blt c nC = true) (hp : (chkLst.get c)[p]? = some y)
    (h1 : WF ops E oc (chkHead.get c) = true) (h2 : WF ops E oc y = true) : False :=
  me_excl (hA c hc) h1 (p := p) (by rw [List.getElem?_map, hp]; rfl) h2
theorem dX (ops : Ops V) (E : EqC V) (hy : Hyps ops E) (oc : OC V) (d : Nat) (hd : Nat.blt d nD = true)
    (h1 : WF ops E oc (dAT.get d) = true) (h2 : WF ops E oc (dBT.get d) = true) : False :=
  disj_excl (hy.disj oc d hd) (cf_of_wf h1) (cf_of_wf h2)
end
''')


def excl(x, y, hA='hA', hy='hy', h1='h1', h2='h2'):
    """Proof of False from h1 : WF x = true, h2 : WF y = true."""
    s = M.excl_src(x, y)
    if s[0] == 'blk':
        return ('blk_excl %s %s' % (h1, h2)) if not s[1] else ('blk_excl %s %s' % (h2, h1))
    if s[0] == 'd':
        a, b = (h1, h2) if not s[2] else (h2, h1)
        return 'dX ops E %s oc %d rfl %s %s' % (hy, s[1], a, b)
    a, b = (h1, h2) if not s[3] else (h2, h1)
    return 'meX ops E oc %s %d %d rfl rfl %s %s' % (hA, s[1], s[2], a, b)


mark('guards')

# ---------------------------------------------------------------- arms
w('/-! ## The hardware: per-port definitions, per-kind arm tables -/\n')
w('section\nvariable {V : Type}\n')
muxes = []          # (port, users in chain order [(k,i)], plan)
chk_heads = {}
for c_, ch_ in enumerate(M.checks):
    chk_heads.setdefault(ch_['head'], []).append(c_)


def order_mux(us):
    """Members in chain order; per member the check covering most later members, and the rest."""
    rem = set(k for k, _ in us)
    order, plan = [], {}
    while rem:
        best = None
        for x in sorted(rem):
            others = rem - {x}
            cb, cov = None, set()
            for c in chk_heads.get(x, []):
                cs = set(M.checks[c]['lst']) & others
                if len(cs) > len(cov):
                    cb, cov = c, cs
            key = (len(others - cov), x)
            if best is None or key < best[0]:
                best = (key, x, cb, others - cov)
        _, x, cb, unc = best
        order.append(x)
        plan[x] = (cb, unc)
        rem.discard(x)
    return order, plan

portinfo = {}       # port -> dict(en=..., arg=('none'|'const'|'single'|'mux'), ...)
for kd in M.kinds:
    if kd == 'tasks':
        continue
    for i in range(len(M.byk[kd])):
        for m in K[kd].ports:
            p = (kd, i, m)
            if not K[kd].has_en(m):
                continue
            us = U.get(p, [])
            nm = '%s_%d_%s' % (kd, i, m)
            if not us:
                portinfo[p] = dict(users=[], argk='none')
                continue
            w('def en_%s (ops : Ops V) (E : EqC V) (oc : OC V) : Bool := orL [%s]'
              % (nm, ', '.join('WF ops E oc %d' % k for k, _ in us)))
            args = [M.rules[k]['sites'][si]['arg'] for k, si in us]
            if len(set(map(repr, args))) == 1:
                argk = 'const' if args[0][0] in ('z', 'k') else ('single' if len(us) == 1 else 'mux')
            else:
                argk = 'mux'
            if argk == 'mux':
                q = len(muxes)
                order, plan = order_mux(us)
                assert len(order) == len(us), 'a rule uses a mux port twice'
                mus = sorted(us, key=lambda e: order.index(e[0]))
                margs = [M.rules[k]['sites'][si]['arg'] for k, si in mus]
                muxes.append((p, mus, plan))
                w('def mlist_%d (V : Type) (ops : Ops V) (E : EqC V) (oc : OC V) : List (Bool × V) :=\n  [%s]'
                  % (q, ',\n   '.join('(WF ops E oc %d, %s)' % (k, arg_oc(k, a)) for (k, _), a in zip(mus, margs))))
                w('def arg_%s (ops : Ops V) (E : EqC V) (oc : OC V) : V := pmux (mlist_%d V ops E oc) ops.z' % (nm, q))
                portinfo[p] = dict(users=us, argk='mux', mux=q, args=args, mus=mus, margs=margs,
                                   mpos={k: t for t, (k, _) in enumerate(mus)})
            else:
                w('def arg_%s (ops : Ops V) (E : EqC V) (oc : OC V) : V := %s' % (nm, arg_oc(us[0][0], args[0])))
                portinfo[p] = dict(users=us, argk=argk, args=args)
w('end\n')


tuser = {}
for k, r in enumerate(M.rules):
    for i, st in enumerate(r['sites']):
        if st['kind'] == 'task':
            tuser[st['port'][2]] = (k, i, st)
if nT:
    w('/-- Task statement `t`: its enable (the statement\'s condition, `WILL_FIRE` included) and arguments. -/')
    w('def tenT : Tab ((V : Type) → Ops V → EqC V → OC V → Bool) :=\n  %s' % tab([
        'fun V (ops : Ops V) (E : EqC V) (oc : OC V) => WF ops E oc %d && %s' % (tuser[t][0], tcond_oc(tuser[t][0], tuser[t][2]['hw']))
        for t in range(nT)]))
    w('def targT : Tab ((V : Type) → Ops V → EqC V → OC V → V) :=\n  %s' % tab([
        'fun V (ops : Ops V) (E : EqC V) (oc : OC V) => %s' % targ_oc(tuser[t][0], tuser[t][2]['args'])
        for t in range(nT)]))


def en_term(p):
    kd, i, m = p
    return ('en_%s_%d_%s ops E oc' % (kd, i, m)) if portinfo[p]['users'] else 'false'


def arg_term(p):
    kd, i, m = p
    return ('arg_%s_%d_%s ops E oc' % (kd, i, m)) if portinfo[p]['users'] else 'ops.z'


def arm_body(kd, i):
    P = lambda m: (kd, i, m)
    if kd == 'reg':
        return 'regW (%s) (%s) ops.z' % (en_term(P('write')), arg_term(P('write')))
    if kd == 'cfg':
        return 'cfgW (%s) (%s) ops.z' % (en_term(P('write')), arg_term(P('write')))
    if kd == 'fifo':
        return 'fifoW (%s) (%s) (%s) (%s) ops.z' % (en_term(P('enq')), en_term(P('deq')), en_term(P('clear')),
                                                    arg_term(P('enq')))
    if kd in ('rwS', 'rwC'):
        return 'wireW (%s) (%s) ops.z' % (en_term(P('wset')), arg_term(P('wset')))
    if kd in ('rw0S', 'rw0C'):
        return 'wire0W (%s) ops.z' % en_term(P('wset'))
    if kd == 'tasks':
        return '⟨fun t => (tenT.get t.val) V ops E oc, fun t => (targT.get t.val) V ops E oc⟩'
    raise ValueError(kd)


for kd in M.kinds:
    for i in range(len(M.byk[kd])):
        w('def arm_%s_%d (V : Type) (ops : Ops V) (E : EqC V) (oc : OC V) (w : Wires Empty V) : Wires (KI .%s).M V :=\n  %s'
          % (kd, i, kd, arm_body(kd, i)))
for kd in M.kinds:
    w('def arms_%s : Tab ((V : Type) → Ops V → EqC V → OC V → Wires Empty V → Wires (KI .%s).M V) :=\n  %s'
      % (kd, kd, tab(['arm_%s_%d' % (kd, i) for i in range(len(M.byk[kd]))])))
w('''section
variable {V : Type}
def arms : (κ : Kind) → Tab ((V : Type) → Ops V → EqC V → OC V → Wires Empty V → Wires (KI κ).M V)
''' + ''.join('  | .%s => arms_%s\n' % (k, k) for k in M.kinds) + '''
/-- The parent's combinational logic: children through the arm tables. -/
def comb (ops : Ops V) (E : EqC V) (oc : OC V) (w : Wires Empty V) : ((j : I) → Wires (CI j).M V) × Outs Empty V :=
  (fun j => (arms j.1).get j.2.val V ops E oc w, ⟨fun e => e.elim, fun e => e.elim⟩)
/-- The parallel muxes' selectors are one-hot. -/
def onehotP (ops : Ops V) (E : EqC V) (oc : OC V) : Prop :=
  ''' + ' ∧ '.join(['amoChain false ((mlist_%d V ops E oc).map Prod.fst)' % q for q in range(len(muxes))] + ['True']) + '''

abbrev P (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) : Parent V where
  I := I
  decI := inferInstance
  kids := allKids [''' + ', '.join('.' + k for k in M.kinds) + '''] nk
  kids_nodup := allKids_nodup nk _ (by decide)
  kids_all := mem_allKids _ nk (fun κ => by cases κ <;> simp)
  CI := CI
  X := X
  PR := Fin nR
  body := fun m _ => m.elim
  rbody := fun r => runSites (u r.val) ((resT.get r.val) V ops E) (sitesF (siteR ops E r.val) (kR r.val)) 0 (u r.val)
  comb := comb ops E
  asm := fun oc _ => asmP ops E oc
  onehot := fun oc _ => onehotP ops E oc
end
''')
mark('arms')

# ---------------------------------------------------------------- site proofs
w('/-! ## Site facts: reads before, dummy arguments -/\n')
w('''def fxF (κ c : Nat) : Bool := match κ, c with
''' + ''.join(
    ''.join('  | %d, %d => true\n' % (M.kn[kd], K[kd].code(m)) for m in K[kd].ports if K[kd].fx(m))
    for kd in M.kinds) + '''  | _, _ => false
def kindN : Kind → Nat
''' + ''.join('  | .%s => %d\n' % (k, M.kn[k]) for k in M.kinds) + '''def codeK : (κ : Kind) → (KI κ).M → Nat
''')
for kd in M.kinds:
    if kd == 'reg':
        w('  | .reg => fun m => match m with | .rd _ => 0 | .wr _ => 1')
    elif kd == 'tasks':
        w('  | .tasks => fun _ => 0')
    else:
        w('  | .%s => fun m => match m with %s' % (kd, ' '.join('| .%s => %d' % (p, K[kd].code(p)) for p in K[kd].ports)))
w('''def npK : Kind → Nat
''' + ''.join('  | .%s => %d\n' % (k, 1 if k == 'tasks' else len(K[k].ports)) for k in M.kinds) + '''def offK : Kind → Nat
''' + ''.join('  | .%s => %d\n' % (k, M.off[k]) for k in M.kinds) + '''def baseOf (j : I) : Nat := offK j.1 + j.2.val * npK j.1
def fxP (p : Σ j : I, (CI j).M) : Bool := fxF (kindN p.1.1) (codeK p.1.1 p.2)

section
variable {V : Type}
/-- Site `i` of rule `k`: it reads only earlier sites, and a flagged port's call carries `z`. -/
def SQ (ops : Ops V) (E : EqC V) (k i : Nat) (F : (V : Type) → Ops V → EqC V → SF V) : Prop :=
  SFReads i (F V ops E) ∧ (fxP ((keyT.get k).get i) = true → ∀ ρ, (F V ops E).2 ρ = ops.z)
/-- Rule `k`'s site table `t`: every site's facts. -/
def SQT (ops : Ops V) (E : EqC V) (k : Nat) (t : Tab ((V : Type) → Ops V → EqC V → SF V)) : Prop :=
  Tab.AllR (SQ ops E k) t 0 (kT.get k)
theorem sfr_g {i : Nat} (g : (Nat → V) → V) (hg : ∀ ρ ρ' : Nat → V, (∀ i', i' < i → ρ i' = ρ' i') → g ρ = g ρ') :
    SFReads i ((fun _ => true, g) : SF V) := fun ρ ρ' h => ⟨rfl, hg ρ ρ' h⟩
theorem sfr_cg {i : Nat} (c : (Nat → V) → Bool) (g : (Nat → V) → V)
    (hc : ∀ ρ ρ' : Nat → V, (∀ i', i' < i → ρ i' = ρ' i') → c ρ = c ρ')
    (hg : ∀ ρ ρ' : Nat → V, (∀ i', i' < i → ρ i' = ρ' i') → g ρ = g ρ') :
    SFReads i ((c, g) : SF V) := fun ρ ρ' h => ⟨hc ρ ρ' h, hg ρ ρ' h⟩
''')


def cong_rho(a):
    if a[0] in ('z', 'k'):
        return 'rfl'
    return 'congrArg (ops.f %d) (%s)' % (a[1], tupcong(['h %d (by decide)' % j for j in a[2]]))


def sq_leaf(k, i, st):
    if st['kind'] == 'task':
        c = st['cond']
        cc = 'rfl' if c[0] == 'true' else 'congrArg (ops.g %d) (%s)' % (c[1], tupcong(['h %d (by decide)' % j for j in c[2]]))
        ca = tupcong([cong_rho(a) for a in st['args']])
        return "⟨sfr_cg _ _ (fun ρ ρ' h => %s) (fun ρ ρ' h => %s), fun h => absurd h (by decide)⟩" % (cc, ca)
    if st['kind'] == 'read' or st['arg'][0] == 'z':
        return '⟨sfr_const _ _, fun _ _ => rfl⟩'
    a = st['arg']
    nofx = 'fun h => absurd h (by decide)'
    if a[0] == 'k':
        return '⟨sfr_const _ _, %s⟩' % nofx
    cong = tupcong(['h %d (by decide)' % j for j in a[2]])
    return '⟨sfr_g _ (fun ρ ρ\' h => congrArg (ops.f %d) (%s)), %s⟩' % (a[1], cong, nofx)


for k, r in enumerate(M.rules):
    if r['sites']:
        body = tabproof([sq_leaf(k, i, st) for i, st in enumerate(r['sites'])])
    else:
        body = 'fun _ _ h => absurd h (Nat.not_lt_zero _)'
    w('theorem sq_%d (ops : Ops V) (E : EqC V) : SQT ops E %d sf_%d :=\n  %s'
      % (k, k, k, body))
w('theorem sqT (ops : Ops V) (E : EqC V) : Tab.AllR (SQT ops E) sfT 0 nR :=\n  %s'
  % tabproof(['sq_%d ops E' % k for k in range(nR)]))
w('''theorem sqR (ops : Ops V) (E : EqC V) (k : Nat) (hk : k < nR) : Tab.AllR (SQ ops E k) (sfT.get k) 0 (kR k) :=
  Tab.get_allR _ 0 nR (sqT ops E) k (Nat.zero_le _) hk
theorem hOK (ops : Ops V) (E : EqC V) (r : Fin nR) : ∀ i, i < kR r.val → ReadsBefore i (siteR ops E r.val i) :=
  fun i hi => (Tab.get_allR _ 0 (kR r.val) (sqR ops E r.val r.isLt) i (Nat.zero_le _) hi).1

/-- The environment of rule `k`'s sites, the hardware's values. -/
abbrev envR (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) (oc : OC V) (k : Nat) : Nat → V :=
  envHw (u k) oc (sitesF (siteR ops E k) (kR k)) 0 (u k)

theorem trR_eq (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) (oc : OC V) (r : Fin nR) :
    trR (P ops E u) oc r = (callsOf (envR ops E u oc r.val) (sitesF (siteR ops E r.val) (kR r.val)),
      (resT.get r.val) V ops E (envR ops E u oc r.val)) :=
  trace_fn (u r.val) oc _ (siteR ops E r.val) (kR r.val) (hOK ops E r)
theorem trS_eq (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) (oc : OC V) (w : Wires Empty V) (r : Fin nR) :
    trS (P ops E u) oc w (.inr r) = callsOf (envR ops E u oc r.val) (sitesF (siteR ops E r.val) (kR r.val)) :=
  congrArg Prod.fst (trR_eq ops E u oc r)
theorem envR_at (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) (oc : OC V) (k : Nat) (hk : k < nR) (i : Nat) (hi : i < kR k) :
    envR ops E u oc k i = if (siteR ops E k i).cond (envR ops E u oc k) then (oc (siteR ops E k i).j).ret (siteR ops E k i).m else u k i :=
  envHw_fn (u k) oc (siteR ops E k) (kR k) (hOK ops E ⟨k, hk⟩) i hi
end
''')
mark('siteproofs')

# value lemmas
w('section\nvariable {V : Type}\n')
for (k, i) in sorted(evneed):
    w('theorem ev_%d_%d (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) (oc : OC V) : envR ops E u oc %d %d = %s :=\n'
      '  (envR_at ops E u oc %d (by decide) %d (by decide)).trans rfl' % (k, i, k, i, ocread(site_port(k, i)), k, i))
w('end\n')
w('''section
variable {V : Type}
theorem res_some (z : V) {b : Bool} (h : b = true) : (bif b then some z else none).isSome = true := by
  rw [h]; rfl
/-- Rule `k`'s result is defined when its guard holds (the guard read at its position). -/
def RQ (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) (k : Nat) (g : (V : Type) → Ops V → EqC V → OC V → Bool) : Prop :=
  ∀ oc : OC V, g V ops E oc = true → ((resT.get k) V ops E (envR ops E u oc k)).isSome = true
''')


def rq_leaf(k):
    g = M.rules[k]['guard']
    if g and g[0] == 'g':
        return ('fun oc h => res_some ops.z ((congrArg (ops.g %d) (%s)).trans h)'
                % (g[1], tupcong([ev(k, i) for i in grd_pos(k)])))
    return 'fun _ _ => rfl'


w('theorem rqT (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) : Tab.AllR (RQ ops E u) gT 0 nR :=\n  %s'
  % tabproof(['(%s : RQ ops E u %d G_%d)' % (rq_leaf(k), k, k) for k in range(nR)]))
w('''theorem resR_ok (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) (oc : OC V) (w : Wires Empty V) (r : Fin nR)
    (hf : fire ops E oc w r = true) : (trR (P ops E u) oc r).2.isSome = true := by
  rw [trR_eq]
  exact Tab.get_allR _ 0 nR (rqT ops E u) r.val (Nat.zero_le _) r.isLt oc ((Bool.and_eq_true _ _).mp (cf_of_wf hf)).2
end
''')
mark('values')

# ---------------------------------------------------------------- checks (kernel)
w('/-! ## Decided checks over closed tables -/\n')
w('''/-- A check over each rule's sites (rules without sites pass). -/
theorem sitesB_spec (f : Nat → Nat → (Σ j : I, (CI j).M) → Bool)
    (h : Tab.allB (fun k keys => Nat.beq (kT.get k) 0 || Tab.allB (f k) keys 0 (kT.get k)) keyT 0 nR = true)
    (k : Nat) (hk : k < nR) : ∀ i, i < kR k → f k i ((keyT.get k).get i) = true := by
  intro i hi
  have h1 := Tab.allB_spec _ keyT 0 nR h k (Nat.zero_le _) hk
  simp only [Bool.or_eq_true] at h1
  rcases h1 with h1 | h1
  · have := Nat.eq_of_beq_eq_true h1; simp only [kR] at hi; omega
  · exact Tab.allB_spec _ _ 0 (kT.get k) h1 i (Nat.zero_le _) hi
theorem chkAll : Tab.allB (fun k keys => Nat.beq (kT.get k) 0 ||
    Tab.allB (fun i p => (idx (.inr (rR k)) p).contains i) keys 0 (kT.get k)) keyT 0 nR = true := by
  decide +kernel
theorem hchk (r : Fin nR) : ∀ i, i < kR r.val → i ∈ idx (.inr r) ((keyT.get r.val).get i) := by
  intro i hi
  have h := sitesB_spec (fun k i p => (idx (.inr (rR k)) p).contains i) chkAll r.val r.isLt i hi
  rw [rR_val] at h
  simpa using h
''')

# ---------------------------------------------------------------- per-port lemmas (uniform)
w('''section
variable {V : Type}

theorem onIff (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) (oc : OC V) (w : Wires Empty V) :
    ∀ s, On (P ops E u) (fire ops E) oc w s ↔ stepOn ops E oc s = true := by
  intro s; cases s with
  | inl e => exact e.elim
  | inr r => exact Iff.rfl

/-- A rule's unconditional site on a port: the rule calls the port. -/
theorem cR (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) (oc : OC V) (w : Wires Empty V) (r : Fin nR) (j : I) (cm : (CI j).M)
    (i0 : Nat) (hidx : idx (.inr r) ⟨j, cm⟩ = [i0]) (hi0 : Nat.blt i0 (kR r.val) = true)
    (hk0 : (keyT.get r.val).get i0 = ⟨j, cm⟩) (hc : (siteR ops E r.val i0).cond (envR ops E u oc r.val) = true) :
    Calls (P ops E u) (trS (P ops E u) oc w (.inr r)) j cm := by
  show ∃ x, (cm, x) ∈ proj j (trS (P ops E u) oc w (.inr r))
  rw [trS_eq]
  exact (calls_one _ (siteR ops E r.val) (kR r.val) (keyT.get r.val).get (fun _ => rfl) (idx (.inr r)) (hchk r) j cm i0
    hidx hi0 hk0).mpr hc

/-- `cR` with the port's index list stated over closed data (decided by the kernel). -/
theorem cRk (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) (oc : OC V) (w : Wires Empty V) (r : Fin nR) (κ : Kind) (i : Nat)
    (hi : i < nk κ) (cm : (KI κ).M) (i0 : Nat) (hidx : idxN (((usesK κ).get i).uses cm) r.val = [i0])
    (hi0 : Nat.blt i0 (kR r.val) = true) (hk0 : (keyT.get r.val).get i0 = ⟨⟨κ, ⟨i, hi⟩⟩, cm⟩)
    (hc : (siteR ops E r.val i0).cond (envR ops E u oc r.val) = true) :
    Calls (P ops E u) (trS (P ops E u) oc w (.inr r)) ⟨κ, ⟨i, hi⟩⟩ cm :=
  cR ops E u oc w r ⟨κ, ⟨i, hi⟩⟩ cm i0 hidx hi0 hk0 hc

/-- The argument of a rule's one site on a port. -/
theorem aR (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) (oc : OC V) (w : Wires Empty V) (r : Fin nR) (j : I) (cm : (CI j).M)
    (i0 : Nat) (hidx : idx (.inr r) ⟨j, cm⟩ = [i0]) (v : V)
    (hv : (siteR ops E r.val i0).cond (envR ops E u oc r.val) = true → (siteR ops E r.val i0).arg (envR ops E u oc r.val) = v)
    (x : V) (hx : (cm, x) ∈ proj j (trS (P ops E u) oc w (.inr r))) : v = x := by
  rw [trS_eq] at hx
  exact arg_one _ (siteR ops E r.val) (kR r.val) (keyT.get r.val).get (fun _ => rfl) (idx (.inr r)) (hchk r) j cm i0 hidx v hv x hx

theorem aRk (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) (oc : OC V) (w : Wires Empty V) (r : Fin nR) (κ : Kind) (i : Nat)
    (hi : i < nk κ) (cm : (KI κ).M) (i0 : Nat) (hidx : idxN (((usesK κ).get i).uses cm) r.val = [i0]) (v : V)
    (hv : (siteR ops E r.val i0).cond (envR ops E u oc r.val) = true → (siteR ops E r.val i0).arg (envR ops E u oc r.val) = v)
    (x : V) (hx : (cm, x) ∈ proj (⟨κ, ⟨i, hi⟩⟩ : I) (trS (P ops E u) oc w (.inr r))) : v = x :=
  aR ops E u oc w r ⟨κ, ⟨i, hi⟩⟩ cm i0 hidx v hv x hx
end
''')

# ---------------------------------------------------------------- mux chains
w('/-! ## The muxes\' one-hot chains, from the exclusions -/\n')
w('section\nvariable {V : Type}\n')
w('''/-- A run-time check excludes its head with every rule of its list. -/
theorem chkC (ops : Ops V) (E : EqC V) (oc : OC V) (hA : asmP ops E oc) (c : Nat) (hc : Nat.blt c nC = true) :
    ∀ y ∈ chkLst.get c, WF ops E oc (chkHead.get c) = true → WF ops E oc y = true → False :=
  forall_of_check (hA c hc)
''')
nchk_pairs = nuncov = 0
for q, (p, us, plan) in enumerate(muxes):
    ks = [k for k, _ in us]
    rows = []
    for a, x in enumerate(ks):
        tail = ks[a + 1:]
        cb, unc = plan[x]
        uncl = [y for y in tail if y in unc]
        nuncov += len(uncl)
        nchk_pairs += len(tail) - len(uncl)
        pw = allL(['fun h1 h2 => %s' % excl(x, y) for y in uncl])
        T = '[%s]' % ', '.join(map(str, tail))
        if cb is None:
            rows.append('forall_split (T := %s) (cov := []) (unc := [%s]) (by decide +kernel) (fun y hy => absurd hy (List.not_mem_nil)) %s'
                        % (T, ', '.join(map(str, uncl)), pw))
        else:
            rows.append('forall_split (T := %s) (cov := chkLst.get %d) (unc := [%s]) (by decide +kernel) (chkC ops E oc hA %d rfl) %s'
                        % (T, cb, ', '.join(map(str, uncl)), cb, pw))
    w('theorem chain_%d (ops : Ops V) (E : EqC V) (hy : Hyps ops E) (oc : OC V) (hA : asmP ops E oc) :\n'
      '    amoChain false ((mlist_%d V ops E oc).map Prod.fst) :=\n'
      '  amoChain_of_pairsC (fun k => WF ops E oc k) (fun x y => WF ops E oc x = true → WF ops E oc y = true → False)\n'
      '    (fun _ _ h => h) [%s]\n    %s' % (q, q, ', '.join(map(str, ks)), '⟨' + ',\n     '.join(rows + ['trivial']) + '⟩'))
w('end\n')
mark('chains')

# ---------------------------------------------------------------- Glue and scan
w('/-! ## The scan: footprints, kind tables, exception masks -/\n')


def ents(l):
    return '[' + ', '.join('(%d, %d, %d)' % e for e in l) + ']'


def kc(t):
    return '⟨[%s], [%s], [%s], [%s]⟩' % tuple(', '.join(map(str, x)) for x in t)


kt = M.kind_tables()
w('def fpRT : Tab (List Ent) :=\n  %s' % tab([ents(l) for l in Dn.fpR]))
w('def ktT : Tab (Tab KC) :=\n  %s' % tab([tab([kc(t) for t in kd]) for kd in kt]))
w('def exRT : Tab Nat :=\n  %s' % tab([nat(x) for x in need]))
# exception groups: each mux's users (their chain is needed for the mux anyway), then pairs
groups = [[k for k, _ in us] for (p, us, _) in muxes]
gsets = [set(g) for g in groups]
for i in range(nR):
    for j in range(nR):
        if need[i] >> j & 1 and not any(i in g and j in g for g in gsets):
            groups.append([j, i])
            gsets.append({i, j})
gid = [[g for g, gs in enumerate(gsets) if i in gs] for i in range(nR)]
w('def memT : Tab (List Nat) :=\n  %s' % tab(['[' + ', '.join(map(str, g)) + ']' for g in groups] or ['[]']))
w('def gidT : Tab (List Nat) :=\n  %s' % tab(['[' + ', '.join(map(str, g)) + ']' for g in gid]))
w('abbrev ng : Nat := %d' % len(groups))
w('''def D : Design where
  nM := 0
  nR := nR
  d := %d
  fpM := .leaf []
  fpR := fpRT
  kt := ktT
  nfI := .leaf 0
  nfO := .leaf 0
  ngI := .leaf 0
  ngO := .leaf 0
  h3ok := .leaf 0
  exM := .leaf 0
  exR := exRT
  exMR := .leaf 0
  escR := .leaf 0
''' % M.d)
mark('design')
w('''theorem segZ : D.segLast 0 ST.init = true := by decide +kernel
theorem scanOK : D.Ok := D.ok_of_chain [] segZ
''')
mark('scan')
w('/-- Chain children (the task statements) and their positions; each rule\'s statements start at `stF`. -/')
w('def chB (j : I) : Bool := decide (j.1 = .tasks)' if nT else 'def chB (j : I) : Bool := false')
w('def posK : (κ : Kind) → (KI κ).M → Nat\n' + ''.join(
    ('  | .tasks => fun t => t.val\n' if kd == 'tasks' else '  | .%s => fun _ => 0\n' % kd) for kd in M.kinds))
w('def stT : Tab Nat :=\n  %s' % tab([nat(M.tstart[k]) for k in range(nR)]))
w('def stF (g : Nat) : Nat := bif Nat.blt g nR then stT.get g else %d' % nT)
w('''theorem stMono : Tab.allB (fun g x => Nat.ble x %d && Nat.ble x (bif Nat.blt (g + 1) nR then stT.get (g + 1) else %d))
    stT 0 nR = true := by decide +kernel
/-- Each rule's statements: increasing, within its interval. -/
theorem chainAll : Tab.allB (fun k keys =>
    incB ((((List.range' 0 (kT.get k)).map keys.get).filter (fun p => chB p.1)).map (fun p => posK p.1.1 p.2)) &&
    ((((List.range' 0 (kT.get k)).map keys.get).filter (fun p => chB p.1)).map (fun p => posK p.1.1 p.2)).all
      (fun x => Nat.ble (stF k) x && Nat.blt x (stF (k + 1)))) keyT 0 nR = true := by decide +kernel
''' % (nT, nT))
w('''section
variable {V : Type}
def G (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) : Glue (P ops E u) D where
  mi := fun m => m.elim
  ruleAt := rR
  rk := fun r => r.val
  base := baseOf
  kind := fun j => kindN j.1
  code := fun j m => codeK j.1 m
  fx := fxF
  z := ops.z
  ch := chB
  pos := fun j m => posK j.1 m
  clo := fun g => stF g
  chi := fun g => stF (g + 1)
end

def entK (p : Σ j : I, (CI j).M) : Ent := (baseOf p.1, kindN p.1.1, codeK p.1.1 p.2)
theorem fpAll : Tab.allB (fun k l => l == (List.range' 0 (kT.get k)).map (fun i => entK ((keyT.get k).get i))) fpRT 0 nR = true := by
  decide +kernel
theorem ghg : Tab.allB (fun x l => l.all (fun g => Nat.blt g ng && (memT.get g).contains x)) gidT 0 nR = true := by
  decide +kernel
theorem ghT : Tab.allB (fun y n => subB n (orMask (gidT.get y) (fun g => bits (memT.get g)))) exRT 0 nR = true := by
  decide +kernel
theorem hg_ok : ∀ x, x < nR → ∀ g ∈ gidT.get x, g < ng ∧ x ∈ memT.get g := by
  intro x hx g hg
  have h := List.all_eq_true.mp (Tab.allB_spec _ gidT 0 nR ghg x (Nat.zero_le _) hx) g hg
  simp only [Bool.and_eq_true, List.contains_iff_mem] at h
  exact ⟨Nat.blt_eq.mp h.1, h.2⟩
theorem hT_ok : ∀ y, y < nR → Sub (exRT.get y) (orMask (gidT.get y) (fun g => bits (memT.get g))) :=
  fun y hy => sub_of_subB (Tab.allB_spec _ exRT 0 nR ghT y (Nat.zero_le _) hy)
''')
for kd in M.kinds:
    if kd == 'tasks':
        w('theorem ports_tasks : ∀ p : (KI .tasks).M, p ∈ List.finRange nT := List.mem_finRange')
        continue
    ports = ', '.join(K[kd].lports)
    if kd == 'reg':
        w('theorem ports_reg : ∀ p : CRegM 1, p ∈ [Reg.read, Reg.write]\n  | .rd ⟨0, _⟩ => by simp\n  | .wr ⟨0, _⟩ => by simp')
    else:
        w('theorem ports_%s : ∀ p : (KI .%s).M, p ∈ [%s] := by intro p; cases p <;> simp' % (kd, kd, ports))
w('''section
variable {V : Type}
theorem testBit_leaf0 (k i : Nat) : ((Tab.leaf 0 : Tab Nat).get k).testBit i = false := by simp [Tab.get]

theorem fpR_ok (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) (oc : OC V) (r : Fin nR) :
    (trR (P ops E u) oc r).1.map (G ops E u).ent <+ D.fpR.get ((G ops E u).rk r) := by
  rw [trR_eq]
  refine (fp_of_sites (G ops E u) _ _).trans ?_
  have h := Tab.allB_spec _ fpRT 0 nR fpAll r.val (Nat.zero_le _) r.isLt
  rw [beq_iff_eq] at h
  show (sitesF (siteR ops E r.val) (kR r.val)).map (G ops E u).entS <+ fpRT.get r.val
  rw [h, sitesF_map]
  exact List.Sublist.refl _

theorem zR_ok (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) (oc : OC V) (r : Fin nR) :
    ∀ c ∈ (trR (P ops E u) oc r).1, (G ops E u).fx ((G ops E u).kind c.1) ((G ops E u).code c.1 c.2.1) = true → c.2.2 = (G ops E u).z := by
  rw [trR_eq]
  apply z_of_sites (G ops E u)
  intro s hs hf ρ'
  obtain ⟨i, hi, rfl⟩ := mem_sitesF hs
  exact (Tab.get_allR _ 0 (kR r.val) (sqR ops E r.val r.isLt) i (Nat.zero_le _) hi).2 hf ρ'
''')
for kd in M.kinds:
    if kd == 'tasks':
        continue
    w('theorem kind_%s (z : V) : KindOK (V := V) (KI .%s).ann (codeK .%s) (D.kc %d) (fxF %d) z :=\n'
      '  kindOK_of_chk ports_%s (by decide +kernel) z' % (kd, kd, kd, M.kn[kd], M.kn[kd], kd))
w('''theorem chainR (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) (r : Fin nR) :
    (∀ p ∈ (sitesF (siteR ops E r.val) (kR r.val)).map (fun s => (⟨s.j, s.m⟩ : Σ j : I, (CI j).M)),
      (G ops E u).ch p.1 = true → stF r.val ≤ (G ops E u).pos p.1 p.2 ∧ (G ops E u).pos p.1 p.2 < stF (r.val + 1)) ∧
    incB ((((sitesF (siteR ops E r.val) (kR r.val)).map (fun s => (⟨s.j, s.m⟩ : Σ j : I, (CI j).M))).filter
      (fun p => (G ops E u).ch p.1)).map (fun p => (G ops E u).pos p.1 p.2)) = true := by
  rw [sitesF_map]
  exact chain_keys (G := G ops E u) _ _ _ _ (Tab.allB_spec _ keyT 0 nR chainAll r.val (Nat.zero_le _) r.isLt)
''')



def kcase(non, tk):
    return ''.join(('    · %s\n' % tk) if kd == 'tasks' else ('    · %s\n' % non(kd)) for kd in M.kinds)


# exception groups' chains
ch_items = []
for g, mem in enumerate(groups):
    if g < len(muxes):
        ch_items.append('(fun oc w hE => chain_%d ops E hy oc hE.1 : CHQ ops E u %d [%s])' % (g, g, ', '.join(map(str, mem))))
    else:
        a, b = mem
        ch_items.append('(fun oc w hE => amoChain_of_pairsL (fun k => WF ops E oc k) '
                        '(fun x y => WF ops E oc x = true → WF ops E oc y = true → False) (fun _ _ h => h) [%d, %d] '
                        '⟨⟨fun h1 h2 => %s, trivial⟩, trivial, trivial⟩ : CHQ ops E u %d [%d, %d])'
                        % (a, b, excl(a, b, hA='hE.1'), g, a, b))
w('/-- Exception group `l`: its rules never two fire. -/\n'
  'def CHQ (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) (g : Nat) (l : List Nat) : Prop :=\n'
  '  ∀ oc w, Env (P ops E u) oc w → amoChain false (l.map (fun i => fire ops E oc w (rR i)))')
w('theorem chT (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) (hy : Hyps ops E) : Tab.AllR (CHQ ops E u) memT 0 ng :=\n  %s'
  % tabproof(ch_items))
w('''
theorem glueOK (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) (hy : Hyps ops E) : GlueOK (P ops E u) D (G ops E u) (fire ops E) where
  mi_lt m := m.elim
  rk_lt r := r.isLt
  rk_at r := rR_val r
  at_rk i hi := Nat.mod_eq_of_lt hi
  kind j h := by
    obtain ⟨κ, i⟩ := j
    cases κ
''' + kcase(lambda kd: 'exact kind_%s _' % kd, 'simp [G, chB] at h') + '''  kindC j h := by
    obtain ⟨κ, i⟩ := j
    cases κ
''' + kcase(lambda kd: 'simp [G, chB] at h',
              'exact kindOut_chain (fun _ => rfl) (by show (0 : Nat) ∈ [0]; simp) (by show (0 : Nat) ∈ [0]; simp)') + '''  chAnn j h a b hlt := by
    obtain ⟨κ, i⟩ := j
    cases κ
''' + kcase(lambda kd: 'simp [G, chB] at h', 'exact taskAnn_group hlt') + '''  cmono g1 g2 h := mono_of_succ stF (stF_mono stT nR %d stMono) (g1 + 1) g2 h
  cinM _ _ m := m.elim
  cinR oc r := by
    rw [trR_eq, show D.nM + (G ops E u).rk r = r.val from Nat.zero_add _]
    exact chain_in_of_sites (G ops E u) _ _ _ _ (chainR ops E u r).1
  cincM _ _ m := m.elim
  cincR oc r := by
    rw [trR_eq]
    exact chain_inc_of_sites (G ops E u) _ _ (chainR ops E u r).2
  chCode j h p := by
    obtain ⟨κ, i⟩ := j
    cases κ
''' % nT + kcase(lambda kd: 'simp [G, chB] at h', 'rfl') + '''  fpM _ _ m := m.elim
  fpR oc r := fpR_ok ops E u oc r
  zM _ _ m := m.elim
  zR oc r := zR_ok ops E u oc r
  nfI m := m.elim
  nfO m := m.elim
  ngI m := m.elim
  ngO m := m.elim
  h3ok m := m.elim
  exM _ _ m := m.elim
  exR := exR_of_chains (P ops E u) (G ops E u) (fire ops E) (fun r => r.isLt) rR_val memT gidT ng
    (chT ops E u hy) hg_ok hT_ok
  exMR _ _ m := m.elim
  esc _ _ r1 r2 _ h := by
    have e : (D.escR.get ((G ops E u).rk r2)).testBit ((G ops E u).rk r1) = false := testBit_leaf0 _ _
    rw [e] at h; cases h

/-- **The pair fields, from the scan.** -/
def pairs (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) (hy : Hyps ops E) : PairFacts (P ops E u) (G ops E u).rules (fire ops E) :=
  pairFacts_of_scan (P ops E u) scanOK (G ops E u) (fire ops E) (glueOK ops E u hy)
end
''')
mark('glue')

# ---------------------------------------------------------------- EAQ per instance
w('/-! ## Enables and arguments: one lemma per child instance, dispatched by table -/\n')
w('''section
variable {V : Type}
/-- What `Cert.en` and `Cert.arg` say about child `⟨κ, i⟩` whose arm is `A`. -/
def EAQ (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) (hy : Hyps ops E) (κ : Kind) (i : Nat)
    (A : (V : Type) → Ops V → EqC V → OC V → Wires Empty V → Wires (KI κ).M V) : Prop :=
  ∀ (h : i < nk κ) (oc : OC V) (w : Wires Empty V), Env (P ops E u) oc w →
    (∀ cm : (KI κ).M, (KI κ).ann.hasEn cm = true →
      ((A V ops E oc w).en cm = true ↔ ∃ s ∈ users (⟨κ, ⟨i, h⟩⟩ : I) cm, On (P ops E u) (fire ops E) oc w s ∧
        Calls (P ops E u) (trS (P ops E u) oc w s) (⟨κ, ⟨i, h⟩⟩ : I) cm)) ∧
    (∀ cm : (KI κ).M, ∀ s ∈ users (⟨κ, ⟨i, h⟩⟩ : I) cm, Av (P ops E u) (fire ops E) oc w s →
      ∀ x, (cm, x) ∈ proj (⟨κ, ⟨i, h⟩⟩ : I) (trS (P ops E u) oc w s) → (A V ops E oc w).arg cm = x)

/-- A flagged (argument-less value) port's calls carry `z`. -/
theorem zarg (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) (oc : OC V) (w : Wires Empty V) (j : I) (cm : (CI j).M)
    (hf : fxP ⟨j, cm⟩ = true) :
    ∀ s ∈ users j cm, Av (P ops E u) (fire ops E) oc w s → ∀ x, (cm, x) ∈ proj j (trS (P ops E u) oc w s) → ops.z = x := by
  intro s _ _ x hx
  cases s with
  | inl e => exact e.elim
  | inr r =>
    have hc := mem_proj j _ _ hx
    exact (zR_ok ops E u oc r ⟨j, (cm, x)⟩ hc hf).symm
''')


HXS = {}       # the current instance's index-list facts, decided once and shared by cRk and aRk


def hx(kd, i, m, lp, k, si):
    nm = 'hx_%d_%d_%s' % (k, si, m)
    HXS[nm] = 'idxN (((usesK .%s).get %d).uses %s) (rR %d).val = [%d]' % (kd, i, lp, k, si)
    return nm


def en_proof(p, inst):
    pi = portinfo[p]
    kd, i, m = p
    lp = K[kd].lports[K[kd].code(m)]
    if not pi['users']:
        return 'en_none (P ops E u) (fire ops E) oc w _ _ _ rfl _ rfl'
    calls = ['cRk ops E u oc w (rR %d) .%s %d h %s %d %s rfl rfl rfl' % (k, kd, i, lp, si, hx(kd, i, m, lp, k, si)) for k, si in pi['users']]
    return ('en_list (P ops E u) (fire ops E) oc w %s %s (stepOn ops E oc) (onIff ops E u oc w)\n'
            '      (users %s %s) %s' % (inst, lp, inst, lp, allL(calls)))


def arg_proof(p, inst):
    pi = portinfo.get(p)
    kd, i, m = p
    lp = K[kd].lports[K[kd].code(m)]
    if K[kd].fx(m):
        return 'zarg ops E u oc w _ _ rfl'
    if not pi['users']:
        return 'fun s hs => by cases hs'
    items = []
    for t, (k, si) in enumerate(pi['users']):
        a = pi['args'][t]
        if pi['argk'] == 'mux':
            items.append('fun hav x hx => (arg_mux ops.z (chain_%d ops E hy oc hE.1) (p := %d) rfl hav).trans\n'
                         '        (aRk ops E u oc w (rR %d) .%s %d h %s %d %s _ (fun _ => %s) x hx)'
                         % (pi['mux'], pi['mpos'][k], k, kd, i, lp, si, hx(kd, i, m, lp, k, si), arg_val(k, a)))
        else:
            items.append('fun hav x hx => aRk ops E u oc w (rR %d) .%s %d h %s %d %s _ (fun _ => %s) x hx'
                         % (k, kd, i, lp, si, hx(kd, i, m, lp, k, si), arg_val(k, a)))
    return ('arg_users (P ops E u) (fire ops E) oc w %s %s _ (users %s %s)\n      %s'
            % (inst, lp, inst, lp, allL(items)))


w('''/-- A rule's site on a port: calls exactly when its condition holds. -/
theorem cRkIff (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) (oc : OC V) (w : Wires Empty V) (r : Fin nR) (κ : Kind)
    (i : Nat) (hi : i < nk κ) (cm : (KI κ).M) (i0 : Nat) (hidx : idxN (((usesK κ).get i).uses cm) r.val = [i0])
    (hi0 : Nat.blt i0 (kR r.val) = true) (hk0 : (keyT.get r.val).get i0 = ⟨⟨κ, ⟨i, hi⟩⟩, cm⟩) :
    Calls (P ops E u) (trS (P ops E u) oc w (.inr r)) ⟨κ, ⟨i, hi⟩⟩ cm ↔
      (siteR ops E r.val i0).cond (envR ops E u oc r.val) = true := by
  show (∃ x, (cm, x) ∈ proj (⟨κ, ⟨i, hi⟩⟩ : I) (trS (P ops E u) oc w (.inr r))) ↔ _
  rw [trS_eq]
  exact calls_one _ (siteR ops E r.val) (kR r.val) (keyT.get r.val).get (fun _ => rfl) (idx (.inr r)) (hchk r)
    (⟨κ, ⟨i, hi⟩⟩ : I) cm i0 hidx hi0 hk0
''')
if nT:
    w('/-- The task statements\' child. -/\nabbrev jT : I := ⟨.tasks, ⟨0, by decide⟩⟩')
    w('''/-- Task statement `t`'s enable and argument facts (its one user). -/
def TQ (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) (t : Nat) (f : (V : Type) → Ops V → EqC V → OC V → Bool) : Prop :=
  ∀ (oc : OC V) (w : Wires Empty V), Env (P ops E u) oc w →
    (f V ops E oc = true ↔ ∃ s ∈ users jT (tP t), On (P ops E u) (fire ops E) oc w s ∧
      Calls (P ops E u) (trS (P ops E u) oc w s) jT (tP t)) ∧
    (∀ s ∈ users jT (tP t), Av (P ops E u) (fire ops E) oc w s →
      ∀ x, (tP t, x) ∈ proj jT (trS (P ops E u) oc w s) → (targT.get t) V ops E oc = x)
''')

    def tq_leaf(t):
        k, i0, st = tuser[t]
        if st['hw'][0] == 'chk':
            enp = 'en_tchk (fun hw => ((Bool.and_eq_true _ _).mp (cf_of_wf hw)).2)'
        else:
            enp = 'en_tk (%s)' % tcond_val(k, st['cond'])
        return ('(have hx : idxN (((usesK .tasks).get 0).uses (tP %d)) (rR %d).val = [%d] := by decide +kernel\n'
                '    have hu : users jT (tP %d) = [.inr (rR %d)] := by decide +kernel\n'
                '    fun oc w hE => ⟨en_one (P ops E u) (fire ops E) oc w jT (tP %d) (.inr (rR %d)) _ hu _\n'
                '      (cRkIff ops E u oc w (rR %d) .tasks 0 (by decide) (tP %d) %d hx rfl rfl) _ (%s),\n'
                '    arg_users (P ops E u) (fire ops E) oc w jT (tP %d) _ _ ⟨fun hav x hx\' =>\n'
                '      aRk ops E u oc w (rR %d) .tasks 0 (by decide) (tP %d) %d hx _ (fun _ => %s) x hx\', trivial⟩⟩)'
                % (t, k, i0, t, k, t, k, k, t, i0, enp, t, k, t, i0, targ_val(k, st['args'])))
    w('theorem tqAll (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) : Tab.AllR (TQ ops E u) tenT 0 nT :=\n  %s'
      % tabproof([tq_leaf(t) for t in range(nT)]))
    w('''theorem ea_tasks_0 (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) (hy : Hyps ops E) : EAQ ops E u hy .tasks 0 arm_tasks_0 := by
  intro h oc w hE
  refine ⟨fun cm _ => ?_, fun cm => ?_⟩
  · obtain ⟨t, ht⟩ := cm
    have e : (⟨t, ht⟩ : Fin nT) = tP t := Fin.ext (Nat.mod_eq_of_lt ht).symm
    have := (Tab.get_allR _ 0 nT (tqAll ops E u) t (Nat.zero_le _) ht oc w hE).1
    rw [← e] at this
    exact this
  · obtain ⟨t, ht⟩ := cm
    have e : (⟨t, ht⟩ : Fin nT) = tP t := Fin.ext (Nat.mod_eq_of_lt ht).symm
    have := (Tab.get_allR _ 0 nT (tqAll ops E u) t (Nat.zero_le _) ht oc w hE).2
    rw [← e] at this
    exact this
''')

ALL = {'reg': 'regAll', 'cfg': 'cfgAll', 'fifo': 'flAll', 'rwS': 'wireAll', 'rwC': 'wireAll',
       'rw0S': 'wire0All', 'rw0C': 'wire0All'}
for kd in M.kinds:
    if kd == 'tasks':
        continue
    for i in range(len(M.byk[kd])):
        inst = '⟨.%s, ⟨%d, h⟩⟩' % (kd, i)
        ens, args = [], []
        HXS.clear()
        for m in K[kd].ports:
            p = (kd, i, m)
            if K[kd].has_en(m):
                ens.append('fun _ => %s' % en_proof(p, inst))
            else:
                ens.append('fun hh => by cases hh')
            args.append(arg_proof(p, inst))
        w('theorem ea_%s_%d (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) (hy : Hyps ops E) : EAQ ops E u hy .%s %d arm_%s_%d := by\n'
          % (kd, i, kd, i, kd, i) + ''.join('  have %s : %s := by decide +kernel\n' % (nm, st_) for nm, st_ in HXS.items()) +
          '  intro h oc w hE\n'
          '  refine ⟨%s (%s), %s (%s)⟩'
          % (ALL[kd], ')\n    ('.join(ens), ALL[kd], ')\n    ('.join(args)))
for kd in M.kinds:
    n = len(M.byk[kd])
    if kd == 'tasks':
        w('theorem eaAll_tasks (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) (hy : Hyps ops E) : Tab.AllR (EAQ ops E u hy .tasks) arms_tasks 0 1 :=\n'
          '  Tab.one (ea_tasks_0 ops E u hy)')
        continue
    w('theorem eaAll_%s (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) (hy : Hyps ops E) : Tab.AllR (EAQ ops E u hy .%s) arms_%s 0 %d :=\n  %s'
      % (kd, kd, kd, n, tabproof(['ea_%s_%d ops E u hy' % (kd, i) for i in range(n)])))
w('''theorem eaAll (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) (hy : Hyps ops E) (κ : Kind) (i : Nat) (h : i < nk κ) :
    EAQ ops E u hy κ i ((arms κ).get i) := by
  cases κ
''' + ''.join('  · exact Tab.get_allR _ 0 _ (eaAll_%s ops E u hy) i (Nat.zero_le _) h\n' % kd for kd in M.kinds) + 'end\n')
mark('ea')

# ---------------------------------------------------------------- the certificate
w('''/-! ## The certificate and the obligation -/

section
variable {V : Type}
def fp (ops : Ops V) (E : EqC V) : St → List (Σ j : I, (CI j).M)
  | .inl e => e.elim
  | .inr r => (sitesF (siteR ops E r.val) (kR r.val)).map (fun s => ⟨s.j, s.m⟩)

theorem usersOk (ops : Ops V) (E : EqC V) : ∀ (s : St), ∀ p ∈ fp ops E s, s ∈ users p.1 p.2 := by
  intro s p hp
  cases s with
  | inl e => exact e.elim
  | inr r =>
    obtain ⟨i, hi, rfl⟩ := mem_keys hp
    have h := mem_usersN (hchk r i hi)
    rw [rR_val] at h
    exact h

theorem fpOk (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) : ∀ (oc : OC V) (w : Wires Empty V) (s : St),
    ∀ c ∈ trS (P ops E u) oc w s, (⟨c.1, c.2.1⟩ : Σ j : I, (CI j).M) ∈ fp ops E s := by
  intro oc w s c hc
  cases s with
  | inl e => exact e.elim
  | inr r => rw [trS_eq] at hc; exact callsOf_fn_ports _ _ _ c hc

theorem rdyR_ok (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) : ∀ (oc : OC V) (w : Wires Empty V) (r : Fin nR),
    Env (P ops E u) oc w → fire ops E oc w r = true → ∀ c ∈ (trR (P ops E u) oc r).1,
      ((P ops E u).CI c.1).ann.alwaysRdy c.2.1 = false → (oc c.1).rdy c.2.1 = true := by
  intro oc w r _ hf c hc ha
  rw [trR_eq] at hc
  obtain ⟨i, hi, e⟩ := mem_keys (callsOf_fn_ports _ _ _ c hc)
  have hrd := rd_of_cf (cf_of_wf hf)
  have h2 := List.all_eq_true.mp hrd i (List.mem_range'.mpr ⟨i, hi, by omega⟩)
  rw [e] at h2
  simp only [alwaysRdyB, Bool.or_eq_true] at h2
  rcases h2 with h2 | h2
  · rw [ha] at h2; cases h2
  · exact h2

theorem onehot_ok (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) (hy : Hyps ops E) (oc : OC V) (w : Wires Empty V)
    (hC : EnvC (P ops E u) oc w) : onehotP ops E oc :=
  ''' + allL(['chain_%d ops E hy oc hC.1' % q for q in range(len(muxes))]) + '''

def cert (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) (hy : Hyps ops E) : Cert (P ops E u) :=
  PairFacts.toCert (P ops E u) (pairs ops E u hy) (fp ops E) users (usersOk ops E) (fpOk ops E u)
    (fun m => m.elim) (fun _ _ m => m.elim)
    (fun oc w r _ hf => resR_ok ops E u oc w r hf)
    (fun _ _ m => m.elim) (rdyR_ok ops E u)
    (fun oc w j cm hE hcm => by obtain ⟨κ, i, hi⟩ := j; exact (eaAll ops E u hy κ i hi hi oc w hE).1 cm hcm)
    (fun _ _ m => m.elim)
    (fun oc w j cm hE s hs hav x hx => by obtain ⟨κ, i, hi⟩ := j; exact (eaAll ops E u hy κ i hi hi oc w hE).2 cm s hs hav x hx)
    (fun oc w j cm _ h => by obtain ⟨κ, i⟩ := j; cases κ <;> cases cm <;> cases h)
    (onehot_ok ops E u hy)

/-- **The module's obligation**, for every value type, operators, constants, skipped values
    and solver verdicts. -/
theorem obl (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) (hy : Hyps ops E) : Obl (P ops E u) :=
  obl_of_cert (P ops E u) (cert ops E u hy)
end
''')
mark('cert')

# ---------------------------------------------------------------- Acyc
w('/-! ## Acyclicity: read lists as filters of allOuts -/\n')
# ranks: path outputs of each instance get one rank b (fifo: enq, notFull, i_notFull, clear; wires: wget, whas)
path_inputs = {'fifo': ['enq', 'deq'], 'rwS': ['wset'], 'rwC': ['wset'], 'rw0S': ['wset'], 'rw0C': ['wset']}
path_outs = {'fifo': ['enq', 'notFull', 'i_notFull', 'clear'], 'rwS': ['wget', 'whas'], 'rwC': ['wget', 'whas'],
             'rw0S': ['whas'], 'rw0C': ['whas']}
rank = {}       # (kd,i) -> b


def prk(p):
    kd, i, m = p
    if kd == 'fifo':
        b1, b2 = rank.get((kd, i), (0, 0))
        return b2 if m == 'clear' else (b1 if m in ('enq', 'notFull', 'i_notFull') else 0)
    if kd in path_outs and m in path_outs[kd]:
        return rank.get((kd, i), 0)
    return 0


def bound(kd, i, m):
    if kd == 'fifo':
        b1, b2 = rank.get((kd, i), (0, 0))
        return b2 if m == 'enq' else b1
    return rank.get((kd, i), 0)


def bterm(kd, m):
    if kd == 'fifo':
        return '(bnd_fifo.get i).2' if m == 'enq' else '(bnd_fifo.get i).1'
    return 'bnd_%s.get i' % kd


def wf_reads(k, seen=None):
    """Outputs WF k reads: guarded sites' ready signals, guard fan-in, blocker's."""
    out = [st['port'] for st in M.rules[k]['sites'] if M.is_rdy_guarded(st['port'])]
    out += grd_reads.get(k, [])
    if k in M.blk:
        out += wf_reads(M.blk[k])
    return out


def input_reads(p):
    pi = portinfo.get(p)
    if not pi or not pi['users']:
        return []
    out = []
    for t, (k, si) in enumerate(pi['users']):
        out += wf_reads(k)
        a = pi['args'][t]
        if a[0] == 'f':
            out += [site_port(k, j) for j in a[2]]
    return out


for it in range(1000):
    changed = False
    for kd in path_inputs:
        if kd not in M.kinds:
            continue
        for i in range(len(M.byk[kd])):
            fan = {}
            for m in path_inputs[kd]:
                fan[m] = max([-1] + [prk(y) for y in input_reads((kd, i, m))])
            if kd == 'fifo':
                b1 = fan['deq'] + 1
                b = (b1, max(b1, fan['enq'] + 1))
            else:
                b = fan['wset'] + 1
            if rank.get((kd, i), (0, 0) if kd == 'fifo' else 0) != b:
                rank[(kd, i)] = b
                changed = True
    if not changed:
        break
else:
    raise ValueError('combinational cycle')
NN = max([0] + [max(v) if isinstance(v, tuple) else v for v in rank.values()]) + 1


def mxW(k):
    return max([0] + [prk(y) for y in wf_reads(k)])


def mxRD(k):
    return max([0] + [prk(st['port']) for st in M.rules[k]['sites'] if M.is_rdy_guarded(st['port'])])


w(('''def fifoRk (b : Nat × Nat) : FL → Nat := fun m => match m with
  | .enq | .notFull | .i_notFull => b.1
  | .clear => b.2
  | _ => 0
''' if 'fifo' in M.kinds else '') + '''def wireRk (b : Nat) : WireM → Nat := fun m => match m with
  | .wget | .whas => b
  | _ => 0
def wire0Rk (b : Nat) : Wire0M → Nat := fun m => match m with
  | .whas => b
  | _ => 0
''')
RKF = {'fifo': 'fifoRk', 'rwS': 'wireRk', 'rwC': 'wireRk', 'rw0S': 'wire0Rk', 'rw0C': 'wire0Rk'}
for kd in M.kinds:
    if kd in RKF:
        if kd == 'fifo':
            w('def bnd_fifo : Tab (Nat × Nat) :=\n  %s' % tab(['(%d, %d)' % rank.get(('fifo', i), (0, 0)) for i in range(len(M.byk[kd]))]))
        else:
            w('def bnd_%s : Tab Nat :=\n  %s' % (kd, tab([nat(rank.get((kd, i), 0)) for i in range(len(M.byk[kd]))])))
w('def rkK : (κ : Kind) → Nat → (KI κ).M → Nat\n' + ''.join(
    ('  | .%s => fun i m => %s (bnd_%s.get i) m\n' % (kd, RKF[kd], kd)) if kd in RKF else ('  | .%s => fun _ _ => 0\n' % kd)
    for kd in M.kinds))
w('def rk (y : Σ j : I, (CI j).M) : Nat := rkK y.1.1 y.1.2.val y.2')
w('def mxRDT : Tab Nat :=\n  %s' % tab([nat(mxRD(k)) for k in range(nR)]))
w('''def portsK : (κ : Kind) → List (KI κ).M
''' + ''.join(('  | .tasks => List.finRange nT\n' if kd == 'tasks' else '  | .%s => [%s]\n' % (kd, ', '.join(K[kd].lports))) for kd in M.kinds) + '''def allOuts : List (Σ j : I, (CI j).M) := allPorts [''' + ', '.join('.' + k for k in M.kinds) + '''] nk (fun κ => (KI κ).M) portsK
theorem mem_allOuts (y : Σ j : I, (CI j).M) : y ∈ allOuts :=
  mem_allPorts _ nk _ portsK (fun κ => by cases κ <;> simp) (fun κ m => by
    cases κ
''' + ''.join(('    · exact ports_%s m\n' % kd) for kd in M.kinds) + '''  ) y
def allIns : List Empty := []
theorem mem_allIns (p : Empty) : p ∈ allIns := p.elim
def outsBelow (r : Nat) : List (Σ j : I, (CI j).M) := allOuts.filter (fun y => Nat.blt (rk y) r)
def DK : (κ : Kind) → Nat → (KI κ).M → List (Σ j : I, (CI j).M)
''')
for kd in M.kinds:
    if kd == 'fifo':
        w('  | .fifo => fun i m => match m with | .enq => outsBelow (bnd_fifo.get i).2 | .deq => outsBelow (bnd_fifo.get i).1 | _ => allOuts')
    elif kd in ('rwS', 'rwC', 'rw0S', 'rw0C'):
        w('  | .%s => fun i m => match m with | .wset => outsBelow (bnd_%s.get i) | _ => allOuts' % (kd, kd))
    else:
        w('  | .%s => fun _ _ => allOuts' % kd)  # (tasks: no paths)
w('''
theorem rdBnd : Tab.allB (fun k keys => Nat.beq (kT.get k) 0 ||
    Tab.allB (fun i p => alwaysRdyB p || Nat.ble (rk p) (mxRDT.get k)) keys 0 (kT.get k)) keyT 0 nR = true := by
  decide +kernel
theorem rdB (k : Nat) (hk : k < nR) : ∀ i, i < kR k → alwaysRdyB ((keyT.get k).get i) = false →
    rk ((keyT.get k).get i) ≤ mxRDT.get k := by
  intro i hi ha
  have h := sitesB_spec (fun k i p => alwaysRdyB p || Nat.ble (rk p) (mxRDT.get k)) rdBnd k hk i hi
  simp only [ha, Bool.false_or] at h
  exact Nat.le_of_ble_eq_true h

theorem all_congr' {α : Type} {f g : α → Bool} : ∀ (l : List α), (∀ a ∈ l, f a = g a) → l.all f = l.all g
  | [], _ => rfl
  | a :: l, h => by
    simp only [List.all_cons]
    rw [h a (List.mem_cons_self ..), all_congr' l (fun b hb => h b (List.mem_cons_of_mem _ hb))]

section
variable {V : Type}
theorem agO {oc oc' : OC V} {b : Nat} (hR : ∀ y, Nat.blt (rk y) b = true → AgreeO (oc y.1) (oc' y.1) y.2)
    (j : I) (m : (CI j).M) (h : Nat.blt (rk ⟨j, m⟩) b = true) : (oc j).ret m = (oc' j).ret m := (hR ⟨j, m⟩ h).2

theorem agRD {oc oc' : OC V} {b : Nat} (hR : ∀ y, Nat.blt (rk y) b = true → AgreeO (oc y.1) (oc' y.1) y.2)
    (k : Nat) (hk : k < nR) (hb : Nat.blt (mxRDT.get k) b = true) : RD oc k = RD oc' k := by
  unfold RD
  apply all_congr'
  intro i hi
  have hi' : i < kR k := by simp at hi; omega
  cases ha : alwaysRdyB ((keyT.get k).get i)
  · simp only [Bool.false_or]
    exact (hR _ (blt_of_le_blt (rdB k hk i hi' ha) hb)).1
  · rfl
''')

# agreement lemmas for WF of rules that feed path-bearing inputs
agneed = set()
for kd in path_inputs:
    if kd not in M.kinds:
        continue
    for i in range(len(M.byk[kd])):
        for m in path_inputs[kd]:
            pi = portinfo.get((kd, i, m))
            if pi and pi['users']:
                for k, _ in pi['users']:
                    agneed.add(k)
                    kk = k
                    while kk in M.blk:
                        kk = M.blk[kk]
                        agneed.add(kk)


def ag_order():
    seen, out = set(), []

    def go(k):
        if k in seen:
            return
        if k in M.blk:
            go(M.blk[k])
        seen.add(k)
        out.append(k)
    for k in sorted(agneed):
        go(k)
    return out


def oread_ag(p, bterm, hb):
    kd, i, m = p
    return 'agO hR (%s %d) %s (blt_of_le_blt (Nat.le_of_ble_eq_true rfl) %s)' % (
        M.ihelper(kd), i, K[kd].lports[K[kd].code(m)], hb)


for k in ag_order():
    mk = mxW(k)
    g = grd_reads.get(k, [])
    gag = 'congrArg (ops.g %d) (%s)' % (M.rules[k]['guard'][1], tupcong([oread_ag(p, 'b', 'hb') for p in g]))
    bag = ('congrArg not (agWF_%d ops E hR (blt_of_le_blt (Nat.le_of_ble_eq_true rfl) hb))' % M.blk[k]) if k in M.blk else 'rfl'
    w('theorem agWF_%d (ops : Ops V) (E : EqC V) {oc oc\' : OC V} {b : Nat}\n'
      '    (hR : ∀ y, Nat.blt (rk y) b = true → AgreeO (oc y.1) (oc\' y.1) y.2) (hb : Nat.blt %d b = true) :\n'
      '    WF ops E oc %d = WF ops E oc\' %d :=\n'
      '  congr2 and (congr2 and (agRD hR %d (by decide) (blt_of_le_blt (Nat.le_of_ble_eq_true rfl) hb)) (%s)) (%s)'
      % (k, mk, k, k, k, gag, bag))


def oread_dq(p):
    kd, i, m = p
    return 'agO hR (%s %d) %s rfl' % (M.ihelper(kd), i, K[kd].lports[K[kd].code(m)])


def list_cong(terms):
    if not terms:
        return 'rfl'
    return 'congr2 List.cons (%s) (%s)' % (terms[0], list_cong(terms[1:]))


def dq_port(p, bexpr):
    """AgreeW of a path-bearing input under the rank bound."""
    pi = portinfo.get(p)
    kd, i, m = p
    if not pi or not pi['users']:
        return '⟨rfl, rfl⟩'
    hb = '(by decide : Nat.blt %d %s = true)'
    ens = ['agWF_%d ops E hR rfl' % k for k, _ in pi['users']]
    en = 'congrArg orL (%s)' % list_cong(ens)

    def dag(k, a):
        if a[0] in ('z', 'k'):
            return 'rfl'
        return 'congrArg (ops.f %d) (%s)' % (a[1], tupcong([oread_dq(site_port(k, j)) for j in a[2]]))
    if K[kd].has_en(m) and m in ('wset', 'enq') and kd != 'rw0S' and kd != 'rw0C':
        if pi['argk'] == 'mux':
            ents_ = ['congr2 Prod.mk (agWF_%d ops E hR rfl) (%s)' % (k, dag(k, pi['margs'][t])) for t, (k, _) in enumerate(pi['mus'])]
            arg = 'congrArg (fun l => pmux l ops.z) (%s)' % list_cong(ents_)
        else:
            arg = dag(pi['users'][0][0], pi['args'][0])
    else:
        arg = 'rfl'
    return '⟨%s, %s⟩' % (en, arg)


for kd in M.kinds:
    if kd not in path_inputs:
        continue
    n = len(M.byk[kd])
    pins = path_inputs[kd]
    hyp = "(∀ y, Nat.blt (rk y) (%s) = true → AgreeO (oc y.1) (oc' y.1) y.2) → AgreeW (A V ops E oc w) (A V ops E oc' w') %s"
    w('def DQ_%s (ops : Ops V) (E : EqC V) (i : Nat) (A : (V : Type) → Ops V → EqC V → OC V → Wires Empty V → Wires (KI .%s).M V) : Prop :=\n'
      '  ∀ (h : i < nk .%s) (oc oc\' : OC V) (w w\' : Wires Empty V),\n    %s'
      % (kd, kd, kd, ' ∧ '.join('(%s)' % (hyp % (bterm(kd, m), K[kd].lports[K[kd].code(m)])) for m in pins)))
    for i in range(n):
        parts = ['fun hR => %s' % dq_port((kd, i, m), 'bnd') for m in pins]
        w('theorem dq_%s_%d (ops : Ops V) (E : EqC V) : DQ_%s ops E %d arm_%s_%d := fun h oc oc\' w w\' =>\n  %s'
          % (kd, i, kd, i, kd, i, ('⟨' + ', '.join(parts) + '⟩') if len(parts) > 1 else parts[0]))
    w('theorem dqAll_%s (ops : Ops V) (E : EqC V) : Tab.AllR (DQ_%s ops E) arms_%s 0 %d :=\n  %s'
      % (kd, kd, kd, n, tabproof(['dq_%s_%d ops E' % (kd, i) for i in range(n)])))
w('end\n')


def dep_case(kd):
    da = 'dep_of_all (P ops E u) allOuts mem_allOuts allIns mem_allIns oc oc\' w w\' hD hW _ _'
    if kd not in path_inputs:
        return '    | %s => exact %s\n' % (kd, da)
    pins = path_inputs[kd]
    s = '    | %s =>\n      cases p with\n' % kd
    for m in K[kd].ports:
        if m in pins:
            sel = '' if len(pins) == 1 else ('.1' if m == pins[0] else '.2')
            s += ('      | %s => exact (Tab.get_allR _ 0 _ (dqAll_%s ops E) i (Nat.zero_le _) hi hi oc oc\' w w\')%s (forall_filter allOuts mem_allOuts _ hD)\n' % (m, kd, sel))
        else:
            s += '      | %s => exact %s\n' % (m, da)
    return s


def rkok_case(kd):
    if kd not in path_inputs:
        if kd == 'reg':
            return '    | reg => rw [reg_nopath] at hpq; cases hpq\n'
        return '    | %s => cases hpq\n' % kd
    if kd == 'fifo':
        return ('    | fifo => cases p <;> cases q <;> first | exact blt_true (List.mem_filter.mp hy).2 | exact Nat.lt_of_lt_of_le (blt_true (List.mem_filter.mp hy).2) (fifoLe i hi) | cases hpq\n')
    return ('    | %s => cases p <;> cases q <;> first | exact blt_true (List.mem_filter.mp hy).2 | cases hpq\n' % kd)


def rklt_case(kd):
    if kd not in path_inputs:
        return '    | %s => exact Nat.zero_lt_succ _\n' % kd
    if kd == 'fifo':
        return ('    | fifo =>\n      have hb := Tab.allB_spec _ bnd_fifo 0 (nk .fifo) rkB_fifo i (Nat.zero_le _) hi\n'
                '      simp only [Bool.and_eq_true] at hb\n'
                '      cases m <;> first | exact Nat.zero_lt_succ _ | exact Nat.blt_eq.mp hb.1 | exact Nat.blt_eq.mp hb.2\n')
    return ('    | %s =>\n      have hb := Tab.allB_spec _ bnd_%s 0 (nk .%s) rkB_%s i (Nat.zero_le _) hi\n'
            '      cases m <;> first | exact Nat.zero_lt_succ _ | exact Nat.blt_eq.mp hb\n'
            % (kd, kd, kd, kd))


for kd in M.kinds:
    if kd in path_inputs:
        if kd == 'fifo':
            w('theorem rkB_fifo : Tab.allB (fun _ b => Nat.blt b.1 %d && Nat.blt b.2 %d) bnd_fifo 0 (nk .fifo) = true := by decide +kernel' % (NN, NN))
            w('theorem fifoLeB : Tab.allB (fun _ b => Nat.ble b.1 b.2) bnd_fifo 0 (nk .fifo) = true := by decide +kernel')
            w('theorem fifoLe (i : Nat) (hi : i < nk .fifo) : (bnd_fifo.get i).1 ≤ (bnd_fifo.get i).2 :=\n'
              '  Nat.le_of_ble_eq_true (Tab.allB_spec _ bnd_fifo 0 (nk .fifo) fifoLeB i (Nat.zero_le _) hi)')
        else:
            w('theorem rkB_%s : Tab.allB (fun _ b => Nat.blt b %d) bnd_%s 0 (nk .%s) = true := by decide +kernel' % (kd, NN, kd, kd))
w('''
section
variable {V : Type}
def acyc (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) : Acyc (P ops E u) where
  D j p := DK j.1 j.2.val p
  Dw _ _ := []
  Do e := e.elim
  Dwo e := e.elim
  dep oc oc' w w' j p hD hW := by
    obtain ⟨κ, i, hi⟩ := j
    cases κ with
''' + ''.join(dep_case(kd) for kd in M.kinds) + '''  depo _ _ _ _ q := q.elim
  rk := rk
  N := %d
  rk_lt y := by
    obtain ⟨⟨κ, i, hi⟩, m⟩ := y
    cases κ with
''' % NN + ''.join(rklt_case(kd) for kd in M.kinds) + '''  rk_ok j q p hpq y hy := by
    obtain ⟨κ, i, hi⟩ := j
    cases κ with
''' + ''.join(rkok_case(kd) for kd in M.kinds) + '''  reach _ := []
  reach_w _ _ _ _ _ hp := by cases hp
  reach_o _ _ _ _ _ _ _ hp := by cases hp
  path_w q := q.elim
  path_o q := q.elim
end
''')
mark('acyc')

# ---------------------------------------------------------------- Built and top
w('''/-! ## The built module and its top theorem -/

/-- The assumed leaves (library contracts not yet proved): FIFOL1 and ConfigRegN. -/
structure Leaves (V : Type) where
''' + ('''  fifo : Module V FL.X
  fifoR : Realizes fifo
  fifoC : Causal fifo
''' if 'fifo' in M.kinds else '') + ('''  cfg : Module V Cfg.X
  cfgR : Realizes cfg
  cfgC : Causal cfg
''' if 'cfg' in M.kinds else '') + '''  dummy : Unit := ()

section
variable {V : Type}
def implK (ops : Ops V) (lv : Leaves V) : (κ : Kind) → Module V (KI κ)
''')
for kd in M.kinds:
    if kd == 'reg':
        w('  | .reg => Reg')
    elif kd == 'cfg':
        w('  | .cfg => lv.cfg')
    elif kd == 'fifo':
        w('  | .fifo => lv.fifo')
    elif kd == 'tasks':
        w('  | .tasks => Tasks nT')
    elif kd in ('rwS', 'rwC'):
        w('  | .%s => RWire ops.ofBool true %s' % (kd, 'true' if kd == 'rwS' else 'false'))
    else:
        w('  | .%s => RWire0 ops.ofBool true %s' % (kd, 'true' if kd == 'rw0S' else 'false'))


def leaf(kd):
    if kd == 'reg':
        return 'Built.leaf _ reg_realizes (creg_causal 1)'
    if kd == 'cfg':
        return 'Built.leaf _ lv.cfgR lv.cfgC'
    if kd == 'fifo':
        return 'Built.leaf _ lv.fifoR lv.fifoC'
    if kd == 'tasks':
        return 'Built.leaf _ (tasks_realizes nT) (tasks_causal nT)'
    if kd in ('rwS', 'rwC'):
        return 'Built.leaf _ (rwire_realizes _ _ _) (rwire_causal _ _ _)'
    return 'Built.leaf _ (rwire0_realizes _ _ _) (rwire0_causal _ _ _)'


w('''
theorem built [Inhabited V] (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) (hy : Hyps ops E) (lv : Leaves V) :
    Built X ((P ops E u).toModule (fun j => implK ops lv j.1)) :=
  Built.node (P ops E u) (fun j => implK ops lv j.1)
    (fun ⟨κ, _⟩ => by
      cases κ
''' + ''.join('      · exact %s\n' % leaf(kd) for kd in M.kinds) + '''    )
    (fun ⟨κ, _⟩ => aliased_of_none _ _ (fun p q => by cases κ <;> rfl) (fun a p => by cases κ <;> rfl))
    (obl ops E u hy) (acyc ops E u)

/-- **The module's top theorem**: the contract and every input has a hardware cycle. -/
theorem top [Inhabited V] (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) (hy : Hyps ops E) (lv : Leaves V) :
    Realizes ((P ops E u).toModule (fun j => implK ops lv j.1)) ∧
    ∀ s w, ((P ops E u).toModule (fun j => implK ops lv j.1)).clean s = s →
      ∃ o s' g, ((P ops E u).toModule (fun j => implK ops lv j.1)).hw s w o s' g :=
  (built ops E u hy lv).top
end

end %s

#print axioms %s.obl
#print axioms %s.top
''' % (NAME, NAME, NAME))

open(out, 'w').write('\n'.join(L) + '\n')
nsites = sum(len(r['sites']) for r in M.rules)
print('wrote %s: rules %d, children %d, sites %d, task statements %d, muxes %d (member pairs: %d by checks, %d one by one), exR bits %d, ranks N=%d, ag lemmas %d'
      % (out, nR, len(M.inst), nsites, nT, len(muxes), nchk_pairs, nuncov, sum(bin(x).count('1') for x in need), NN, len(agneed)))
