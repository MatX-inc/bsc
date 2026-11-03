# Emits out/Em<m>.lean: a module in the conventions of conventions.md.
#
#   children, indexed per kind (I := Σ κ, Fin (nk κ)):
#     reg 0 = sel, reg k+1 = r_k (k < m), reg m+1 = t0, reg m+2 = t1;  wire k = w_k (RWire)
#   rule go:   sel.read; for k: r_k.read (conditional when k % 4 = 3), w_k.wset(g r_k) when sel == k
#   rule back: for k: w_k.wget; r_k.write(h wget) when p wget
#   method peek: t0.read + t1.read
#
# comb dispatches children through per-kind tables of per-instance arms; each
# arm is built from per-port definitions.  Steps are numbered site tables.
# Acyc: precise read lists only for w_k.wset (the only path-bearing input);
# allOuts/allIns and dep_of_all for every other port.
# usage: python3 -I mkemit.py m out.lean
import os, sys

m = int(sys.argv[1]); out = sys.argv[2]
MARK = os.environ.get("MARK") == "1"

assert m >= 2
NR, NW = m + 3, m
KGO, KBACK, KPEEK = 2 * m + 1, 2 * m, 2
cond = lambda k: k % 4 == 3

L = []
w = L.append


def mark(label):
    if MARK:
        w('\n#eval show IO Unit from do let t ← IO.monoMsNow; let h ← IO.getNumHeartbeats; IO.println s!"@@ %s {t} {h}"\n' % label)


def tab(items, ind="  "):
    def go(lo, hi):
        if hi - lo == 1:
            return "(.leaf (%s))" % items[lo]
        mid = (lo + hi) // 2
        return "(.node %d %s\n%s%s)" % (mid, go(lo, mid), ind, go(mid, hi))
    return go(0, len(items))


def tabproof(items, ind="  "):
    def go(lo, hi):
        if hi - lo == 1:
            return "Tab.one (%s)" % items[lo]
        mid = (lo + hi) // 2
        return "⟨%s,\n%s%s⟩" % (go(lo, mid), ind, go(mid, hi))
    return go(0, len(items))


GO, BACK, PEEK = ".inr .go", ".inr .back", ".inl ()"

# site keys
keyGo = ["⟨rI 0, Reg.read⟩"]
sfGo = ["rdSF"]
sfrGo = ["sfr_const _ _"]
for k in range(m):
    keyGo += ["⟨rI %d, Reg.read⟩" % (k + 1), "⟨wI %d, WireM.wset⟩" % k]
    if cond(k):
        sfGo.append("fun V (ops : Ops V) (E : EqC V) => ((fun ρ => E.eqc (ρ 0) %d, fun _ => ops.z) : SF V)" % k)
        sfrGo.append("sfr_c (fun x => E.eqc x %d) ops.z 0 (by decide)" % k)
    else:
        sfGo.append("rdSF")
        sfrGo.append("sfr_const _ _")
    sfGo.append("fun V (ops : Ops V) (E : EqC V) => ((fun ρ => E.eqc (ρ 0) %d, fun ρ => ops.g (ρ %d)) : SF V)" % (k, 2 * k + 1))
    sfrGo.append("sfr_ca (fun x => E.eqc x %d) ops.g 0 %d (by decide) (by decide)" % (k, 2 * k + 1))
keyBack, sfBack, sfrBack = [], [], []
for k in range(m):
    keyBack += ["⟨wI %d, WireM.wget⟩" % k, "⟨rI %d, Reg.write⟩" % (k + 1)]
    sfBack += ["rdSF", "fun V (ops : Ops V) (E : EqC V) => ((fun ρ => ops.p (ρ %d), fun ρ => ops.h (ρ %d)) : SF V)" % (2 * k, 2 * k)]
    sfrBack += ["sfr_const _ _", "sfr_ca ops.p ops.h %d %d (by decide) (by decide)" % (2 * k, 2 * k)]
keyPeek = ["⟨rI %d, Reg.read⟩" % (m + 1), "⟨rI %d, Reg.read⟩" % (m + 2)]
sfPeek = ["rdSF", "rdSF"]
sfrPeek = ["sfr_const _ _", "sfr_const _ _"]

# ports' uses
usesR = ["regU [(%s, 0)] []" % GO]
for k in range(m):
    usesR.append("regU [(%s, %d)] [(%s, %d)]" % (GO, 2 * k + 1, BACK, 2 * k + 1))
usesR += ["regU [(%s, 0)] []" % PEEK, "regU [(%s, 1)] []" % PEEK]
usesW = ["wireU [(%s, %d)] [(%s, %d)]" % (GO, 2 * k + 2, BACK, 2 * k) for k in range(m)]

w("""import EmitLemmas
open Modular Emit
open scoped List

set_option autoImplicit false
set_option linter.unusedVariables false
set_option linter.unusedSimpArgs false
set_option linter.deprecated false
set_option maxRecDepth 100000
set_option maxHeartbeats 0

namespace Em%(m)d

structure Ops (V : Type) where
  z : V
  g : V → V
  h : V → V
  add : V → V → V
  gd : V → Bool
  p : V → Bool
  ofBool : Bool → V

/-! ## Interfaces and indices -/

inductive Kind where
  | reg | wire
  deriving DecidableEq

abbrev nk : Kind → Nat
  | .reg => %(NR)d
  | .wire => %(NW)d

abbrev KI : Kind → Iface
  | .reg => Reg.X
  | .wire => RWire.X false false

abbrev I := Σ κ : Kind, Fin (nk κ)
abbrev CI : I → Iface := fun j => KI j.1

def rI (i : Nat) : I := ⟨.reg, ⟨i %% nk .reg, Nat.mod_lt _ (by decide)⟩⟩
def wI (i : Nat) : I := ⟨.wire, ⟨i %% nk .wire, Nat.mod_lt _ (by decide)⟩⟩

inductive Rl where
  | go | back
  deriving DecidableEq

def xAnn : Ann Unit where
  prec _ _ := false
  sbr _ _ := false
  conf _ _ := true
  share _ := true
  hasEn _ := false
  alwaysEn _ := false
  alwaysRdy _ := true
abbrev X : Iface := ⟨Unit, inferInstance, xAnn⟩

abbrev St := Unit ⊕ Rl
abbrev OC (V : Type) := (j : I) → Outs (CI j).M V

/-! ## Per-kind builders and port splits (once per kind) -/

def regW {V : Type} (en : Bool) (d z : V) : Wires (CRegM 1) V :=
  ⟨fun m => match m with | .wr _ => en | .rd _ => false, fun m => match m with | .wr _ => d | .rd _ => z⟩
def wireW {V : Type} (en : Bool) (d z : V) : Wires WireM V :=
  ⟨fun m => match m with | .wset => en | _ => false, fun m => match m with | .wset => d | _ => z⟩

theorem fin1 (i : Fin 1) : i = 0 := by obtain ⟨k, hk⟩ := i; congr; omega

theorem regAll {Q : CRegM 1 → Prop} (hr : Q Reg.read) (hw : Q Reg.write) : ∀ cm, Q cm := by
  intro cm; cases cm with
  | rd i => rw [fin1 i]; exact hr
  | wr i => rw [fin1 i]; exact hw
theorem wireAll {Q : WireM → Prop} (hs : Q .wset) (hg : Q .wget) (hh : Q .whas) : ∀ cm, Q cm := by
  intro cm; cases cm <;> assumption

theorem reg_nopath (p q : CRegM 1) : (cregAnn 1).path p q = false := by
  cases p <;> cases q <;> rename_i a b <;> rw [fin1 a, fin1 b] <;> rfl

theorem alwaysRdyAll : ∀ (j : I) (cm : (CI j).M), (CI j).ann.alwaysRdy cm = true := by
  intro ⟨κ, _⟩ cm; cases κ <;> rfl

/-! ## Static tables: ports' uses, site keys -/

structure Uses (M : Type) where
  uses : M → List (St × Nat)
def regU (rd wr : List (St × Nat)) : Uses (CRegM 1) := ⟨fun m => match m with | .rd _ => rd | .wr _ => wr⟩
def wireU (s g : List (St × Nat)) : Uses WireM := ⟨fun m => match m with | .wset => s | .wget => g | .whas => []⟩
""" % dict(m=m, NR=NR, NW=NW))

mark("header")
w("def usesR : Tab (Uses (CRegM 1)) :=\n  %s\n" % tab(usesR))
w("def usesW : Tab (Uses WireM) :=\n  %s\n" % tab(usesW))
w("""def usesK : (κ : Kind) → Tab (Uses (KI κ).M)
  | .reg => usesR
  | .wire => usesW
def usesAt (p : Σ j : I, (CI j).M) : List (St × Nat) := ((usesK p.1.1).get p.1.2.val).uses p.2
def users (j : I) (cm : (CI j).M) : List St := usersOf (usesAt ⟨j, cm⟩)
def idx (s : St) (p : Σ j : I, (CI j).M) : List Nat := idxOf (usesAt p) s

abbrev kGo : Nat := %d
abbrev kBack : Nat := %d
abbrev kPeek : Nat := %d
""" % (KGO, KBACK, KPEEK))
mark("uses")
w("def keyGo : Tab (Σ j : I, (CI j).M) :=\n  %s\n" % tab(keyGo))
w("def keyBack : Tab (Σ j : I, (CI j).M) :=\n  %s\n" % tab(keyBack))
w("def keyPeek : Tab (Σ j : I, (CI j).M) :=\n  %s\n" % tab(keyPeek))

mark("keys")
w("""
/-! ## Decided checks (kernel), one per step and table -/

theorem chkGo : Tab.allB (fun i p => (idx (%s) p).contains i) keyGo 0 kGo = true := by decide +kernel
theorem chkBack : Tab.allB (fun i p => (idx (%s) p).contains i) keyBack 0 kBack = true := by decide +kernel
theorem chkPeek : Tab.allB (fun i p => (idx (%s) p).contains i) keyPeek 0 kPeek = true := by decide +kernel
theorem hchkGo : ∀ i, i < kGo → i ∈ idx (%s) (keyGo.get i) := chk_spec _ keyGo kGo chkGo
theorem hchkBack : ∀ i, i < kBack → i ∈ idx (%s) (keyBack.get i) := chk_spec _ keyBack kBack chkBack
theorem hchkPeek : ∀ i, i < kPeek → i ∈ idx (%s) (keyPeek.get i) := chk_spec _ keyPeek kPeek chkPeek

/-- child numberings, increasing along each step (distinct children per step) -/
def numGo (j : I) : Nat := match j.1 with
  | .reg => 2 * j.2.val - 1
  | .wire => 2 * j.2.val + 2
def numBack (j : I) : Nat := match j.1 with
  | .reg => 2 * j.2.val - 1
  | .wire => 2 * j.2.val
def numPeek (j : I) : Nat := j.2.val
theorem monoGo : Tab.allB (fun i p => !(Nat.blt (i + 1) kGo) || Nat.blt (numGo p.1) (numGo (keyGo.get (i + 1)).1))
  keyGo 0 kGo = true := by decide +kernel
theorem monoBack : Tab.allB (fun i p => !(Nat.blt (i + 1) kBack) || Nat.blt (numBack p.1) (numBack (keyBack.get (i + 1)).1))
  keyBack 0 kBack = true := by decide +kernel
theorem monoPeek : Tab.allB (fun i p => !(Nat.blt (i + 1) kPeek) || Nat.blt (numPeek p.1) (numPeek (keyPeek.get (i + 1)).1))
  keyPeek 0 kPeek = true := by decide +kernel

/-- port classes for the ordering facts: (t0/t1 group, port code) -/
def pcode : (Σ j : I, (CI j).M) → Nat
  | ⟨⟨.reg, _⟩, .rd _⟩ => 0
  | ⟨⟨.reg, _⟩, .wr _⟩ => 1
  | ⟨⟨.wire, _⟩, .wset⟩ => 2
  | ⟨⟨.wire, _⟩, .wget⟩ => 3
  | ⟨⟨.wire, _⟩, .whas⟩ => 4
def grp (j : I) : Nat := match j.1 with
  | .reg => if Nat.blt %d j.2.val then 1 else 0
  | .wire => 0
def cls (p : Σ j : I, (CI j).M) : Nat × Nat := (grp p.1, pcode p)
def baseOk : Nat → Nat → Bool
  | 0, 1 => true | 2, 3 => true | 2, 4 => true
  | 0, 3 => true | 0, 4 => true | 2, 1 => true
  | _, _ => false
def okC (a b : Nat × Nat) : Bool := (a.1 != b.1) || baseOk a.2 b.2
def LGo : List (Nat × Nat) := [(0, 0), (0, 2)]
def LBack : List (Nat × Nat) := [(0, 1), (0, 3)]
def LPeek : List (Nat × Nat) := [(1, 0)]
theorem clsGo : Tab.allB (fun _ p => LGo.contains (cls p)) keyGo 0 kGo = true := by decide +kernel
theorem clsBack : Tab.allB (fun _ p => LBack.contains (cls p)) keyBack 0 kBack = true := by decide +kernel
theorem clsPeek : Tab.allB (fun _ p => LPeek.contains (cls p)) keyPeek 0 kPeek = true := by decide +kernel
theorem enVPeek : Tab.allB (fun _ p => !(CI p.1).ann.hasEn p.2) keyPeek 0 kPeek = true := by decide +kernel

theorem hokC {V : Type} : ∀ (j : I) (a b : (CI j).M × V), okC (cls ⟨j, a.1⟩) (cls ⟨j, b.1⟩) = true →
    (CI j).ann.Follow a b := by
  intro ⟨κ, i⟩ ⟨a, x⟩ ⟨b, y⟩ h
  cases κ
  · cases a <;> cases b <;> rename_i ia ib <;> rw [fin1 ia, fin1 ib] at * <;>
      simp [okC, cls, pcode, baseOk, Ann.Follow, cregAnn] at h ⊢
  · cases a <;> cases b <;> simp [okC, cls, pcode, baseOk, Ann.Follow, rwireAnn] at h ⊢
""" % (GO, BACK, PEEK, GO, BACK, PEEK, m))

mark("checks")
# ---------------- V-dependent part
w("""
/-! ## The hardware: per-port definitions, per-kind arm tables -/

section
variable {V : Type}

def WFgo (ops : Ops V) (E : EqC V) (oc : OC V) (w : Wires Unit V) : Bool := ops.gd ((oc (rI 0)).ret Reg.read)
def WFback (ops : Ops V) (E : EqC V) (oc : OC V) (w : Wires Unit V) : Bool := true
def armIdle (V : Type) (ops : Ops V) (E : EqC V) (oc : OC V) (w : Wires Unit V) : Wires (CRegM 1) V := regW false ops.z ops.z
""")
for k in range(m):
    w("""def enW_%(k)d (ops : Ops V) (E : EqC V) (oc : OC V) (w : Wires Unit V) : Bool :=
  WFgo ops E oc w && E.eqc ((oc (rI 0)).ret Reg.read) %(k)d
def argW_%(k)d (ops : Ops V) (E : EqC V) (oc : OC V) (w : Wires Unit V) : V := ops.g ((oc (rI %(k1)d)).ret Reg.read)
def armW_%(k)d (V : Type) (ops : Ops V) (E : EqC V) (oc : OC V) (w : Wires Unit V) : Wires WireM V :=
  wireW (enW_%(k)d ops E oc w) (argW_%(k)d ops E oc w) ops.z
def enR_%(k1)d (ops : Ops V) (E : EqC V) (oc : OC V) (w : Wires Unit V) : Bool :=
  WFback ops E oc w && ops.p ((oc (wI %(k)d)).ret WireM.wget)
def argR_%(k1)d (ops : Ops V) (E : EqC V) (oc : OC V) (w : Wires Unit V) : V := ops.h ((oc (wI %(k)d)).ret WireM.wget)
def armR_%(k1)d (V : Type) (ops : Ops V) (E : EqC V) (oc : OC V) (w : Wires Unit V) : Wires (CRegM 1) V :=
  regW (enR_%(k1)d ops E oc w) (argR_%(k1)d ops E oc w) ops.z
""" % dict(k=k, k1=k + 1))
mark("portdefs")
armsR = ["armIdle"] + ["armR_%d" % (k + 1) for k in range(m)] + ["armIdle", "armIdle"]
armsW = ["armW_%d" % k for k in range(m)]
w("end\n\n/-- arm tables are closed terms: unfolding one copies nothing -/\ndef armsR : Tab ((V : Type) → Ops V → EqC V → OC V → Wires Unit V → Wires (CRegM 1) V) :=\n  %s\n" % tab(armsR))
w("def armsW : Tab ((V : Type) → Ops V → EqC V → OC V → Wires Unit V → Wires WireM V) :=\n  %s\n\nsection\nvariable {V : Type}\n" % tab(armsW))
w("""def arms : (κ : Kind) → Tab ((V : Type) → Ops V → EqC V → OC V → Wires Unit V → Wires (KI κ).M V)
  | .reg => armsR
  | .wire => armsW
def retPeek (ops : Ops V) (E : EqC V) (oc : OC V) (w : Wires Unit V) : V :=
  ops.add ((oc (rI %d)).ret Reg.read) ((oc (rI %d)).ret Reg.read)

/-- The parent's combinational logic: children through the arm tables. -/
def comb (ops : Ops V) (E : EqC V) (oc : OC V) (w : Wires Unit V) : ((j : I) → Wires (CI j).M V) × Outs Unit V :=
  (fun j => (arms j.1).get j.2.val V ops E oc w, ⟨fun _ => true, fun _ => retPeek ops E oc w⟩)

def fire (ops : Ops V) (E : EqC V) (oc : OC V) (w : Wires Unit V) : Rl → Bool
  | .go => WFgo ops E oc w
  | .back => WFback ops E oc w

/-! ## Steps as numbered site tables -/

def rdSF (V : Type) (ops : Ops V) (E : EqC V) : SF V := (fun _ => true, fun _ => ops.z)
end
""" % (m + 1, m + 2))
w("/-- site tables are closed terms too -/\ndef sfGo : Tab ((V : Type) → Ops V → EqC V → SF V) :=\n  %s\n" % tab(sfGo))
w("def sfBack : Tab ((V : Type) → Ops V → EqC V → SF V) :=\n  %s\n" % tab(sfBack))
w("def sfPeek : Tab ((V : Type) → Ops V → EqC V → SF V) :=\n  %s\n\nsection\nvariable {V : Type}\n" % tab(sfPeek))
w("""def siteGo (ops : Ops V) (E : EqC V) : Nat → Site I (fun j => (CI j).M) V := mkSite keyGo.get (fun i => sfGo.get i V ops E)
def siteBack (ops : Ops V) (E : EqC V) : Nat → Site I (fun j => (CI j).M) V := mkSite keyBack.get (fun i => sfBack.get i V ops E)
def sitePeek (ops : Ops V) (E : EqC V) : Nat → Site I (fun j => (CI j).M) V := mkSite keyPeek.get (fun i => sfPeek.get i V ops E)
def resPeek (ops : Ops V) : (Nat → V) → Option V := fun ρ => some (ops.add (ρ 0) (ρ 1))
""")
mark("tables")
w("theorem rbGo (ops : Ops V) (E : EqC V) : Tab.AllR (fun i F => SFReads i (F V ops E)) sfGo 0 kGo :=\n  %s\n" % tabproof(sfrGo))
w("theorem rbBack (ops : Ops V) (E : EqC V) : Tab.AllR (fun i F => SFReads i (F V ops E)) sfBack 0 kBack :=\n  %s\n" % tabproof(sfrBack))
w("theorem rbPeek (ops : Ops V) (E : EqC V) : Tab.AllR (fun i F => SFReads i (F V ops E)) sfPeek 0 kPeek :=\n  %s\n" % tabproof(sfrPeek))
mark("sitereads")
w("""theorem hOKGo (ops : Ops V) (E : EqC V) : ∀ i, i < kGo → ReadsBefore i (siteGo ops E i) :=
  fun i hi => Tab.get_allR _ 0 kGo (rbGo ops E) i (Nat.zero_le _) hi
theorem hOKBack (ops : Ops V) (E : EqC V) : ∀ i, i < kBack → ReadsBefore i (siteBack ops E i) :=
  fun i hi => Tab.get_allR _ 0 kBack (rbBack ops E) i (Nat.zero_le _) hi
theorem hOKPeek (ops : Ops V) (E : EqC V) : ∀ i, i < kPeek → ReadsBefore i (sitePeek ops E i) :=
  fun i hi => Tab.get_allR _ 0 kPeek (rbPeek ops E) i (Nat.zero_le _) hi

abbrev P (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) : Parent V where
  I := I
  decI := inferInstance
  kids := allKids [.reg, .wire] nk
  kids_nodup := allKids_nodup nk _ (by decide)
  kids_all := mem_allKids _ nk (fun κ => by cases κ <;> simp)
  CI := CI
  X := X
  PR := Rl
  body := fun _ _ => runSites (u 0) (resPeek ops) (sitesF (sitePeek ops E) kPeek) 0 (u 0)
  rbody := fun r => match r with
    | .go => runSites (u 1) (fun _ => some ops.z) (sitesF (siteGo ops E) kGo) 0 (u 1)
    | .back => runSites (u 2) (fun _ => some ops.z) (sitesF (siteBack ops E) kBack) 0 (u 2)
  comb := comb ops E
  asm _ _ := True
  onehot _ _ := True

/-! ## Each step's trace and each read site's value (library lemmas, one line each) -/

theorem trGoF (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) (oc : OC V) : trR (P ops E u) oc .go =
    (callsOf (envHw (u 1) oc (sitesF (siteGo ops E) kGo) 0 (u 1)) (sitesF (siteGo ops E) kGo), some ops.z) :=
  trace_fn (u 1) oc _ (siteGo ops E) kGo (hOKGo ops E)
theorem trBackF (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) (oc : OC V) : trR (P ops E u) oc .back =
    (callsOf (envHw (u 2) oc (sitesF (siteBack ops E) kBack) 0 (u 2)) (sitesF (siteBack ops E) kBack), some ops.z) :=
  trace_fn (u 2) oc _ (siteBack ops E) kBack (hOKBack ops E)
theorem trPeek (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) (oc : OC V) (w : Wires Unit V) : trM (P ops E u) oc w () =
    (callsOf (envHw (u 0) oc (sitesF (sitePeek ops E) kPeek) 0 (u 0)) (sitesF (sitePeek ops E) kPeek),
      resPeek ops (envHw (u 0) oc (sitesF (sitePeek ops E) kPeek) 0 (u 0))) :=
  trace_fn (u 0) oc _ (sitePeek ops E) kPeek (hOKPeek ops E)
theorem trGo (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) (oc : OC V) (w : Wires Unit V) :
    trS (P ops E u) oc w (.inr .go) = callsOf (envHw (u 1) oc (sitesF (siteGo ops E) kGo) 0 (u 1)) (sitesF (siteGo ops E) kGo) :=
  congrArg Prod.fst (trGoF ops E u oc)
theorem trBack (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) (oc : OC V) (w : Wires Unit V) :
    trS (P ops E u) oc w (.inr .back) = callsOf (envHw (u 2) oc (sitesF (siteBack ops E) kBack) 0 (u 2)) (sitesF (siteBack ops E) kBack) :=
  congrArg Prod.fst (trBackF ops E u oc)
theorem trSPeek (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) (oc : OC V) (w : Wires Unit V) :
    trS (P ops E u) oc w (.inl ()) = callsOf (envHw (u 0) oc (sitesF (sitePeek ops E) kPeek) 0 (u 0)) (sitesF (sitePeek ops E) kPeek) :=
  congrArg Prod.fst (trPeek ops E u oc w)
""")


def ev(step, i, rhs):
    S, K = {"Go": ("siteGo", "kGo"), "Back": ("siteBack", "kBack"), "Peek": ("sitePeek", "kPeek")}[step]
    w("""theorem ev%(st)s_%(i)d (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) (oc : OC V) :
    envHw (u %(ux)s) oc (sitesF (%(S)s ops E) %(K)s) 0 (u %(ux)s) %(i)d = %(rhs)s :=
  (envHw_fn (u %(ux)s) oc (%(S)s ops E) %(K)s (hOK%(st)s ops E) %(i)d (by decide)).trans rfl
""" % dict(st=step, i=i, S=S, K=K, rhs=rhs, ux={"Go": "1", "Back": "2", "Peek": "0"}[step]))


mark("P_traces")
ev("Go", 0, "(oc (rI 0)).ret Reg.read")
for k in range(m):
    if cond(k):
        ev("Go", 2 * k + 1, "if E.eqc (envHw (u 1) oc (sitesF (siteGo ops E) kGo) 0 (u 1) 0) %d = true then (oc (rI %d)).ret Reg.read else u 1 %d" % (k, k + 1, 2 * k + 1))
    else:
        ev("Go", 2 * k + 1, "(oc (rI %d)).ret Reg.read" % (k + 1))
    ev("Back", 2 * k, "(oc (wI %d)).ret WireM.wget" % k)
ev("Peek", 0, "(oc (rI %d)).ret Reg.read" % (m + 1))
ev("Peek", 1, "(oc (rI %d)).ret Reg.read" % (m + 2))

mark("values")
# ------------- per-instance enable/argument facts
w("""
/-! ## Enables and arguments: one lemma per child instance, dispatched by table -/

/-- What `Cert.en` and `Cert.arg` say about child `⟨κ, i⟩` whose arm is `a`. -/
def EAQ (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) (κ : Kind) (i : Nat)
    (A : (V : Type) → Ops V → EqC V → OC V → Wires Unit V → Wires (KI κ).M V) : Prop :=
  ∀ (h : i < nk κ) (oc : OC V) (w : Wires Unit V), Env (P ops E u) oc w →
    (∀ cm : (KI κ).M, (KI κ).ann.hasEn cm = true →
      ((A V ops E oc w).en cm = true ↔ ∃ s ∈ users (⟨κ, ⟨i, h⟩⟩ : I) cm, On (P ops E u) (fire ops E) oc w s ∧
        Calls (P ops E u) (trS (P ops E u) oc w s) (⟨κ, ⟨i, h⟩⟩ : I) cm)) ∧
    (∀ cm : (KI κ).M, ∀ s ∈ users (⟨κ, ⟨i, h⟩⟩ : I) cm, Av (P ops E u) (fire ops E) oc w s →
      ∀ x, (cm, x) ∈ proj (⟨κ, ⟨i, h⟩⟩ : I) (trS (P ops E u) oc w s) → (A V ops E oc w).arg cm = x)
""")

STEP = {"go": ("Go", "siteGo", "kGo", "keyGo", GO, "trGo"),
        "back": ("Back", "siteBack", "kBack", "keyBack", BACK, "trBack"),
        "peek": ("Peek", "sitePeek", "kPeek", "keyPeek", PEEK, "trSPeek")}


def argcase(step, i0, hv):
    st, S, K, KEY, SS, TR = STEP[step]
    return """intro s hs hav x hx
    obtain rfl := List.mem_singleton.mp hs
    rw [%(TR)s ops E u oc w] at hx
    exact arg_one _ (%(S)s ops E) %(K)s %(KEY)s.get (fun _ => rfl) (idx (%(SS)s)) hchk%(st)s _ _ %(i0)d rfl _ (%(hv)s) x hx""" % dict(
        TR=TR, S=S, K=K, KEY=KEY, SS=SS, st=st, i0=i0, hv=hv)


def encase(step, i0, kind, idx_, port, f, evname, e, a):
    st, S, K, KEY, SS, TR = STEP[step]
    return """exact en_one (P ops E u) (fire ops E) oc w ⟨.%(kind)s, ⟨%(idx)d, h⟩⟩ %(port)s (%(SS)s) _ rfl _
      (by rw [%(TR)s ops E u oc w]; exact calls_one _ (%(S)s ops E) %(K)s %(KEY)s.get (fun _ => rfl) (idx (%(SS)s)) hchk%(st)s _ _ %(i0)d rfl rfl rfl)
      (%(e)s) (and_iff_of_eq (a := %(a)s) (f := %(f)s) (%(ev)s ops E u oc))""" % dict(
        kind=kind, idx=idx_, port=port, SS=SS, TR=TR, S=S, K=K, KEY=KEY, st=st, i0=i0, f=f, ev=evname, e=e, a=a)


eaR, eaW = [], []
# idle registers
for (i, step, i0) in [(0, "go", 0), (m + 1, "peek", 0), (m + 2, "peek", 1)]:
    w("""theorem eaR_%(i)d (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) : EAQ ops E u .reg %(i)d armIdle := by
  intro h oc w hE
  refine ⟨regAll (fun hh => by cases hh) (fun _ => en_none (P ops E u) (fire ops E) oc w _ _ _ rfl _ rfl),
    regAll ?_ (fun s hs => by cases hs)⟩
  · %(arg)s
""" % dict(i=i, arg=argcase(step, i0, "fun _ => rfl")))
for k in range(m):
    w("""theorem eaR_%(i)d (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) : EAQ ops E u .reg %(i)d armR_%(i)d := by
  intro h oc w hE
  refine ⟨regAll (fun hh => by cases hh) (fun _ => ?_), regAll ?_ ?_⟩
  · %(en)s
  · %(a1)s
  · %(a2)s
""" % dict(i=k + 1, en=encase("back", 2 * k + 1, "reg", k + 1, "Reg.write", "ops.p", "evBack_%d" % (2 * k),
                      "enR_%d ops E oc w" % (k + 1), "WFback ops E oc w"),
           a1=argcase("go", 2 * k + 1, "fun _ => rfl"),
           a2=argcase("back", 2 * k + 1, "fun _ => congrArg ops.h (evBack_%d ops E u oc)" % (2 * k))))
    if cond(k):
        hv = "fun hc => congrArg ops.g ((evGo_%d ops E u oc).trans (if_pos hc))" % (2 * k + 1)
    else:
        hv = "fun _ => congrArg ops.g (evGo_%d ops E u oc)" % (2 * k + 1)
    w("""theorem eaW_%(k)d (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) : EAQ ops E u .wire %(k)d armW_%(k)d := by
  intro h oc w hE
  refine ⟨wireAll (fun _ => ?_) (fun hh => by cases hh) (fun hh => by cases hh), wireAll ?_ ?_ (fun s hs => by cases hs)⟩
  · %(en)s
  · %(a1)s
  · %(a2)s
""" % dict(k=k, en=encase("go", 2 * k + 2, "wire", k, "WireM.wset", "fun x => E.eqc x %d" % k, "evGo_0",
                      "enW_%d ops E oc w" % k, "WFgo ops E oc w"),
           a1=argcase("go", 2 * k + 2, hv), a2=argcase("back", 2 * k, "fun _ => rfl")))
mark("instance_lemmas")
eaR = ["eaR_%d ops E u" % i for i in range(NR)]
eaW = ["eaW_%d ops E u" % k for k in range(m)]
w("theorem eaAllR (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) : Tab.AllR (EAQ ops E u .reg) armsR 0 %d :=\n  %s\n" % (NR, tabproof(eaR)))
w("theorem eaAllW (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) : Tab.AllR (EAQ ops E u .wire) armsW 0 %d :=\n  %s\n" % (NW, tabproof(eaW)))
mark("dispatch_tree")
w("""theorem eaAll (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) (κ : Kind) (i : Nat) (h : i < nk κ) :
    EAQ ops E u κ i ((arms κ).get i) := by
  cases κ
  · exact Tab.get_allR _ 0 _ (eaAllR ops E u) i (Nat.zero_le _) h
  · exact Tab.get_allR _ 0 _ (eaAllW ops E u) i (Nat.zero_le _) h

/-! ## The certificate -/

def fp (ops : Ops V) (E : EqC V) : St → List (Σ j : I, (CI j).M)
  | .inl _ => (sitesF (sitePeek ops E) kPeek).map (fun s => ⟨s.j, s.m⟩)
  | .inr .go => (sitesF (siteGo ops E) kGo).map (fun s => ⟨s.j, s.m⟩)
  | .inr .back => (sitesF (siteBack ops E) kBack).map (fun s => ⟨s.j, s.m⟩)

theorem usersOk (ops : Ops V) (E : EqC V) : ∀ (s : St), ∀ p ∈ fp ops E s, s ∈ users p.1 p.2 := by
  intro s p hp
  rcases s with ⟨⟩ | r
  · obtain ⟨i, hi, rfl⟩ := mem_keys hp; exact mem_usersOf (hchkPeek i hi)
  · cases r
    · obtain ⟨i, hi, rfl⟩ := mem_keys hp; exact mem_usersOf (hchkGo i hi)
    · obtain ⟨i, hi, rfl⟩ := mem_keys hp; exact mem_usersOf (hchkBack i hi)

theorem fpOk (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) : ∀ (oc : OC V) (w : Wires Unit V) (s : St),
    ∀ c ∈ trS (P ops E u) oc w s, (⟨c.1, c.2.1⟩ : Σ j : I, (CI j).M) ∈ fp ops E s := by
  intro oc w s c hc
  rcases s with ⟨⟩ | r
  · rw [trSPeek] at hc; exact callsOf_fn_ports _ _ _ c hc
  · cases r
    · rw [trGo] at hc; exact callsOf_fn_ports _ _ _ c hc
    · rw [trBack] at hc; exact callsOf_fn_ports _ _ _ c hc

theorem nodupGo (ops : Ops V) (E : EqC V) : ((sitesF (siteGo ops E) kGo).map (fun s => s.j)).Nodup := by
  rw [sitesF_map]; exact nodup_of_mono _ numGo kGo (mono_spec numGo Sigma.fst keyGo kGo monoGo)
theorem nodupBack (ops : Ops V) (E : EqC V) : ((sitesF (siteBack ops E) kBack).map (fun s => s.j)).Nodup := by
  rw [sitesF_map]; exact nodup_of_mono _ numBack kBack (mono_spec numBack Sigma.fst keyBack kBack monoBack)
theorem nodupPeek (ops : Ops V) (E : EqC V) : ((sitesF (sitePeek ops E) kPeek).map (fun s => s.j)).Nodup := by
  rw [sitesF_map]; exact nodup_of_mono _ numPeek kPeek (mono_spec numPeek Sigma.fst keyPeek kPeek monoPeek)

theorem okGoBack (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) (oc : OC V) :
    OkT (P ops E u) (trR (P ops E u) oc .go).1 (trR (P ops E u) oc .back).1 := by
  rw [trGoF, trBackF]
  exact okT_of_cls (P ops E u) cls okC hokC _ _ LGo LBack (calls_all keyGo _ kGo clsGo _)
    (calls_all keyBack _ kBack clsBack _) (by decide)

theorem okRPeek (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) (oc : OC V) (w : Wires Unit V) (r : Rl) :
    OkT (P ops E u) (trR (P ops E u) oc r).1 (trM (P ops E u) oc w ()).1 := by
  rw [trPeek]
  cases r
  · rw [trGoF]
    exact okT_of_cls (P ops E u) cls okC hokC _ _ LGo LPeek (calls_all keyGo _ kGo clsGo _)
      (calls_all keyPeek _ kPeek clsPeek _) (by decide)
  · rw [trBackF]
    exact okT_of_cls (P ops E u) cls okC hokC _ _ LBack LPeek (calls_all keyBack _ kBack clsBack _)
      (calls_all keyPeek _ kPeek clsPeek _) (by decide)

theorem rulesNodup : [Rl.go, Rl.back].Nodup := by decide

def cert (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) : Cert (P ops E u) where
  rules := [.go, .back]
  rules_nodup := rulesNodup
  fire := fire ops E
  hit _ _ _ _ := false
  fp := fp ops E
  users := users
  users_ok := usersOk ops E
  fp_ok := fpOk ops E u
  selfAnn _ := .inl rfl
  fireIn _ _ r _ := by cases r <;> simp
  resM oc w m _ _ := by
    cases m; rw [trPeek]
    exact congrArg some (congr2 ops.add (evPeek_0 ops E u oc) (evPeek_1 ops E u oc))
  resR oc w r _ _ := by
    cases r
    · rw [trGoF]; rfl
    · rw [trBackF]; rfl
  rdyM oc w m _ _ c _ h := absurd ((alwaysRdyAll c.1 c.2.1).symm.trans h) (by decide)
  rdyR oc w r _ _ c _ h := absurd ((alwaysRdyAll c.1 c.2.1).symm.trans h) (by decide)
  grpM oc w m _ _ := by cases m; rw [trPeek]; exact grpT_sites (P ops E u) _ _ (nodupPeek ops E)
  grpR oc w r _ _ := by
    cases r
    · rw [trGoF]; exact grpT_sites (P ops E u) _ _ (nodupGo ops E)
    · rw [trBackF]; exact grpT_sites (P ops E u) _ _ (nodupBack ops E)
  mm oc w m1 m2 _ _ _ hF := by cases m1; cases m2; have h := hF.2.1; cases h
  mmg oc w m1 m2 _ _ _ hF := by cases m1; cases m2; have h := hF.1.2.1; cases h
  h1 oc w m r _ _ _ hn := by cases m; exact absurd (okRPeek ops E u oc w r) hn
  h2 oc w m r1 r2 _ _ _ _ h _ := absurd h (by decide)
  h3 oc w m m' r _ _ _ _ h _ := absurd h (by decide)
  h4 oc w r1 r2 _ _ _ hne hn := by
    cases r1 <;> cases r2
    · exact absurd rfl hne
    · exact absurd (okGoBack ops E u oc) hn
    · exact List.Sublist.refl _
    · exact absurd rfl hne
  en oc w j cm hE hcm := by
    obtain ⟨κ, i, hi⟩ := j
    exact (eaAll ops E u κ i hi hi oc w hE).1 cm hcm
  enV oc w m _ _ _ _ c hc := by
    cases m; rw [trPeek] at hc
    have := calls_all keyPeek _ kPeek enVPeek _ c hc
    simpa using this
  arg oc w j cm hE s hs hav x hx := by
    obtain ⟨κ, i, hi⟩ := j
    exact (eaAll ops E u κ i hi hi oc w hE).2 cm s hs hav x hx
  alw oc w j cm _ h := by
    obtain ⟨κ, i⟩ := j
    cases κ <;> cases h
  onehot _ _ _ := trivial

theorem obl (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) : Obl (P ops E u) := obl_of_cert (P ops E u) (cert ops E u)
""")

mark("cert")
# ------------- Acyc
w("""
/-! ## Acyclicity: read lists as filters of allOuts -/

def portsK : (κ : Kind) → List (KI κ).M
  | .reg => [Reg.read, Reg.write]
  | .wire => [.wset, .wget, .whas]
def allOuts : List (Σ j : I, (CI j).M) := allPorts [.reg, .wire] nk (fun κ => (KI κ).M) portsK
theorem mem_allOuts (y : Σ j : I, (CI j).M) : y ∈ allOuts :=
  mem_allPorts _ nk _ portsK (fun κ => by cases κ <;> simp) (fun κ m => by
    cases κ
    · cases m <;> rename_i i <;> rw [fin1 i] <;> simp [portsK]
    · cases m <;> simp [portsK]) y
def allIns : List Unit := [()]
theorem mem_allIns (p : Unit) : p ∈ allIns := by cases p; simp [allIns]

/-- ranks of the children's outputs (here by kind: registers 0, wires 1) -/
def rk (y : Σ j : I, (CI j).M) : Nat := match y.1.1 with
  | .reg => 0
  | .wire => 1
/-- the parent inputs each child output depends on (none here) -/
def reach (y : Σ j : I, (CI j).M) : List Unit := []
/-- a path-bearing input's read list: every output below its rank bound -/
def outsBelow (r : Nat) : List (Σ j : I, (CI j).M) := allOuts.filter (fun y => Nat.blt (rk y) r)
/-- a parent output's read list: every output whose reach its declared paths cover -/
def outsFor (q : Unit) : List (Σ j : I, (CI j).M) := allOuts.filter (fun y => (reach y).all (fun p => xAnn.path p q))
def D (j : I) (p : (CI j).M) : List (Σ j : I, (CI j).M) :=
  match j, p with
  | ⟨.wire, _⟩, .wset => outsBelow 1
  | _, _ => allOuts
def Dw (j : I) (p : (CI j).M) : List Unit :=
  match j, p with
  | ⟨.wire, _⟩, .wset => []
  | _, _ => allIns

/-- agreement of a shared comb node, from its direct reads -/
theorem agWFgo (ops : Ops V) (E : EqC V) (oc oc' : OC V) (w w' : Wires Unit V)
    (hR : ∀ y, Nat.blt (rk y) 1 = true → AgreeO (oc y.1) (oc' y.1) y.2) : WFgo ops E oc w = WFgo ops E oc' w' :=
  congrArg ops.gd (hR ⟨rI 0, Reg.read⟩ rfl).2

def DQ (ops : Ops V) (E : EqC V) (i : Nat) (A : (V : Type) → Ops V → EqC V → OC V → Wires Unit V → Wires WireM V) : Prop :=
  ∀ (h : i < nk .wire) (oc oc' : OC V) (w w' : Wires Unit V),
    (∀ y, Nat.blt (rk y) 1 = true → AgreeO (oc y.1) (oc' y.1) y.2) → AgreeW (A V ops E oc w) (A V ops E oc' w') WireM.wset
""")
for k in range(m):
    w("""theorem depW_%(k)d (ops : Ops V) (E : EqC V) : DQ ops E %(k)d armW_%(k)d := fun h oc oc' w w' hR =>
  ⟨congr2 and (agWFgo ops E oc oc' w w' hR) (congrArg (fun x => E.eqc x %(k)d) (hR ⟨rI 0, Reg.read⟩ rfl).2),
   congrArg ops.g (hR ⟨rI %(k1)d, Reg.read⟩ rfl).2⟩
""" % dict(k=k, k1=k + 1))
w("theorem depAllW (ops : Ops V) (E : EqC V) : Tab.AllR (DQ ops E) armsW 0 %d :=\n  %s\n" % (
    NW, tabproof(["depW_%d ops E" % k for k in range(m)])))
mark("dep_lemmas")
w("""
def acyc (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) : Acyc (P ops E u) where
  D := D
  Dw := Dw
  Do := outsFor
  Dwo _ := []
  dep oc oc' w w' j p hD hW := by
    obtain ⟨κ, i, hi⟩ := j
    cases κ with
    | reg => exact dep_of_all (P ops E u) allOuts mem_allOuts allIns mem_allIns oc oc' w w' hD hW _ p
    | wire =>
      cases p with
      | wset =>
        exact Tab.get_allR _ 0 _ (depAllW ops E) i (Nat.zero_le _) hi hi oc oc' w w'
          (forall_filter allOuts mem_allOuts _ hD)
      | wget => exact dep_of_all (P ops E u) allOuts mem_allOuts allIns mem_allIns oc oc' w w' hD hW _ _
      | whas => exact dep_of_all (P ops E u) allOuts mem_allOuts allIns mem_allIns oc oc' w w' hD hW _ _
  depo oc oc' w w' q hD hW := by
    have hR := forall_filter allOuts mem_allOuts _ hD
    exact ⟨rfl, congr2 ops.add (hR ⟨rI %(t0)d, Reg.read⟩ rfl).2 (hR ⟨rI %(t1)d, Reg.read⟩ rfl).2⟩
  rk := rk
  N := 2
  rk_lt y := by
    obtain ⟨⟨κ, _⟩, _⟩ := y
    cases κ
    · exact Nat.zero_lt_two
    · exact Nat.one_lt_two
  rk_ok j q p hpq y hy := by
    obtain ⟨κ, i, hi⟩ := j
    cases κ with
    | reg => rw [reg_nopath] at hpq; cases hpq
    | wire =>
      cases p with
      | wset => exact blt_true (List.mem_filter.mp hy).2
      | wget => cases q <;> cases hpq
      | whas => cases q <;> cases hpq
  reach := reach
  reach_w j q p hpq p' hp' := by
    obtain ⟨κ, i, hi⟩ := j
    cases κ with
    | reg => rw [reg_nopath] at hpq; cases hpq
    | wire =>
      cases p with
      | wset => cases hp'
      | wget => cases q <;> cases hpq
      | whas => cases q <;> cases hpq
  reach_o _ _ _ _ _ _ _ hp := by cases hp
  path_w _ _ hp := by cases hp
  path_o q y hy p hp := by cases hp

/-! ## The built module and its top theorem -/

def implK (ops : Ops V) : (κ : Kind) → Module V (KI κ)
  | .reg => Reg
  | .wire => RWire ops.ofBool false false

theorem built [Inhabited V] (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) :
    Built X ((P ops E u).toModule (fun j => implK ops j.1)) :=
  Built.node (P ops E u) (fun j => implK ops j.1)
    (fun ⟨κ, _⟩ => by
      cases κ
      · exact Built.leaf _ reg_realizes (creg_causal 1)
      · exact Built.leaf _ (rwire_realizes _ _ _) (rwire_causal _ _ _))
    (fun ⟨κ, _⟩ => aliased_of_none _ _ (fun p q => by cases κ <;> rfl) (fun a p => by cases κ <;> rfl))
    (obl ops E u) (acyc ops E u)

theorem top [Inhabited V] (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) :
    Realizes ((P ops E u).toModule (fun j => implK ops j.1)) ∧
    ∀ s w, ((P ops E u).toModule (fun j => implK ops j.1)).clean s = s →
      ∃ o s' g, ((P ops E u).toModule (fun j => implK ops j.1)).hw s w o s' g :=
  (built ops E u).top

end
end Em%(m)d

#print axioms Em%(m)d.obl
#print axioms Em%(m)d.top
""" % dict(t0=m + 1, t1=m + 2, m=m))
mark("acyc_built")
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import migrate_u  # noqa: E402
open(out, "w").write(migrate_u.migrate("".join(L), m))
