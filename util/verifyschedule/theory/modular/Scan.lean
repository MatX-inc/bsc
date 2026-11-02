/- The scan behind a parent's pair fields, proved (design.md §9): a kernel-decided, checkpointed pass
   (adapted from r4's `scan_ok`/`segFrom_ok`/`ok_of_chain`) and its soundness `pairFacts_of_scan`.
   Imports `EmitLemmas` for its `Tab`. -/
import EmitLemmas

open scoped List

namespace Modular
namespace Scan

/-! ## Tables and trees -/

-- Tables by number are EmitLemmas' balanced `Tab` (round 4's search tree); importers `open Emit (Tab)`.
open Emit (Tab)

/-- Values by the low `d` bits of a number. -/
inductive Tr (α : Type) where
  | lf (v : α)
  | nd (l r : Tr α)

variable {α : Type}

def Tr.left : Tr α → Tr α
  | .lf v => .lf v
  | .nd l _ => l

def Tr.right : Tr α → Tr α
  | .lf v => .lf v
  | .nd _ r => r

def Tr.val (dflt : α) : Tr α → α
  | .lf v => v
  | .nd _ _ => dflt

def Tr.get (dflt : α) (d : Nat) : Tr α → Nat → α :=
  Nat.rec (motive := fun _ => Tr α → Nat → α) (fun t _ => t.val dflt)
    (fun _ ih t k => bif Nat.beq (Nat.land k 1) 0 then ih t.left (Nat.shiftRight k 1)
                     else ih t.right (Nat.shiftRight k 1)) d

def Tr.set (d : Nat) : Tr α → Nat → α → Tr α :=
  Nat.rec (motive := fun _ => Tr α → Nat → α → Tr α) (fun _ _ v => .lf v)
    (fun _ ih t k v => bif Nat.beq (Nat.land k 1) 0 then .nd (ih t.left (Nat.shiftRight k 1) v) t.right
                       else .nd t.left (ih t.right (Nat.shiftRight k 1) v)) d

theorem land_one_beq (k : Nat) : Nat.beq (Nat.land k 1) 0 = (k % 2 == 0) := by
  show (k &&& 1 == 0) = (k % 2 == 0); rw [Nat.and_one_is_mod]

theorem shiftRight_one (k : Nat) : Nat.shiftRight k 1 = k / 2 := by
  show k >>> 1 = k / 2; rw [Nat.shiftRight_eq_div_pow]

theorem Tr.get_succ (dflt : α) (d : Nat) (t : Tr α) (k : Nat) :
    Tr.get dflt (d + 1) t k = bif k % 2 == 0 then Tr.get dflt d t.left (k / 2) else Tr.get dflt d t.right (k / 2) := by
  show (bif Nat.beq (Nat.land k 1) 0 then _ else _) = _
  rw [land_one_beq, shiftRight_one]; rfl

theorem Tr.set_succ (d : Nat) (t : Tr α) (k : Nat) (v : α) :
    Tr.set (d + 1) t k v = bif k % 2 == 0 then .nd (Tr.set d t.left (k / 2) v) t.right
                           else .nd t.left (Tr.set d t.right (k / 2) v) := by
  show (bif Nat.beq (Nat.land k 1) 0 then _ else _) = _
  rw [land_one_beq, shiftRight_one]; rfl

theorem mod_two_pow_succ (k d : Nat) : k % 2 ^ (d + 1) = k % 2 + 2 * (k / 2 % 2 ^ d) := by
  rw [Nat.pow_succ, Nat.mul_comm, Nat.mod_mul]

theorem Tr.get_congr (dflt : α) : ∀ (d : Nat) (t : Tr α) (k k' : Nat), k % 2 ^ d = k' % 2 ^ d →
    Tr.get dflt d t k = Tr.get dflt d t k'
  | 0, _, _, _, _ => rfl
  | d + 1, t, k, k', h => by
    rw [mod_two_pow_succ, mod_two_pow_succ] at h
    have h2 : k % 2 = k' % 2 := by omega
    have h3 : k / 2 % 2 ^ d = k' / 2 % 2 ^ d := by omega
    rw [Tr.get_succ, Tr.get_succ, h2]
    cases k' % 2 == 0
    · exact Tr.get_congr dflt d _ _ _ h3
    · exact Tr.get_congr dflt d _ _ _ h3

/-- **Get after set**, by the low `d` bits. -/
theorem Tr.get_set (dflt : α) : ∀ (d : Nat) (t : Tr α) (k k' : Nat) (v : α),
    Tr.get dflt d (Tr.set d t k v) k' = if k' % 2 ^ d = k % 2 ^ d then v else Tr.get dflt d t k'
  | 0, t, k, k', v => by
    simp only [Nat.pow_zero, Nat.mod_one, ite_true]
    rfl
  | d + 1, t, k, k', v => by
    have ih := Tr.get_set dflt d
    rw [Tr.set_succ, mod_two_pow_succ k', mod_two_pow_succ k]
    cases hk : k % 2 == 0 <;> cases hk' : k' % 2 == 0
    · have p1 : k % 2 = 1 := by have := Nat.mod_two_eq_zero_or_one k; simp at hk; omega
      have p2 : k' % 2 = 1 := by have := Nat.mod_two_eq_zero_or_one k'; simp at hk'; omega
      simp only [Bool.cond_false, Tr.get_succ, hk', Tr.left, Tr.right]
      rw [ih]
      by_cases e : k' / 2 % 2 ^ d = k / 2 % 2 ^ d
      · simp only [e, (by omega : k' % 2 + 2 * (k / 2 % 2 ^ d) = k % 2 + 2 * (k / 2 % 2 ^ d)), ite_true]
      · have e2 : ¬ (k' % 2 + 2 * (k' / 2 % 2 ^ d) = k % 2 + 2 * (k / 2 % 2 ^ d)) := by omega
        simp only [e, e2, ite_false]
    · have p1 : k % 2 = 1 := by have := Nat.mod_two_eq_zero_or_one k; simp at hk; omega
      have p2 : k' % 2 = 0 := by simpa using hk'
      simp only [Bool.cond_false, Bool.cond_true, Tr.get_succ, hk', Tr.left]
      have e2 : ¬ (k' % 2 + 2 * (k' / 2 % 2 ^ d) = k % 2 + 2 * (k / 2 % 2 ^ d)) := by omega
      simp only [e2, ite_false]
    · have p1 : k % 2 = 0 := by simpa using hk
      have p2 : k' % 2 = 1 := by have := Nat.mod_two_eq_zero_or_one k'; simp at hk'; omega
      simp only [Bool.cond_false, Bool.cond_true, Tr.get_succ, hk', Tr.right]
      have e2 : ¬ (k' % 2 + 2 * (k' / 2 % 2 ^ d) = k % 2 + 2 * (k / 2 % 2 ^ d)) := by omega
      simp only [e2, ite_false]
    · have p1 : k % 2 = 0 := by simpa using hk
      have p2 : k' % 2 = 0 := by simpa using hk'
      simp only [Bool.cond_true, Tr.get_succ, hk', Tr.left]
      rw [ih]
      by_cases e : k' / 2 % 2 ^ d = k / 2 % 2 ^ d
      · simp only [e, (by omega : k' % 2 + 2 * (k / 2 % 2 ^ d) = k % 2 + 2 * (k / 2 % 2 ^ d)), ite_true]
      · have e2 : ¬ (k' % 2 + 2 * (k' / 2 % 2 ^ d) = k % 2 + 2 * (k / 2 % 2 ^ d)) := by omega
        simp only [e, e2, ite_false]

/-- A checkpoint, written as `(key, value)` entries set into a leaf. -/
def Tr.ofList (dflt : α) (d : Nat) (l : List (Nat × α)) : Tr α :=
  l.foldl (fun t e => Tr.set d t e.1 e.2) (.lf dflt)

def Tr.beq (eq : α → α → Bool) : Tr α → Tr α → Bool
  | .lf a, .lf b => eq a b
  | .nd l r, .nd l' r' => Tr.beq eq l l' && Tr.beq eq r r'
  | _, _ => false

theorem Tr.eq_of_beq {eq : α → α → Bool} (h : ∀ a b, eq a b = true → a = b) :
    ∀ {a b : Tr α}, Tr.beq eq a b = true → a = b
  | .lf a, .lf b, hb => by rw [h a b hb]
  | .nd l r, .nd l' r', hb => by
    simp only [Tr.beq, Bool.and_eq_true] at hb
    rw [Tr.eq_of_beq h hb.1, Tr.eq_of_beq h hb.2]
  | .lf _, .nd _ _, hb => by simp [Tr.beq] at hb
  | .nd _ _, .lf _, hb => by simp [Tr.beq] at hb

/-! ## Checkpointed passes, generically -/

section Chain
variable {σ : Type} (F : Nat → σ → Option σ) (init : σ)

/-- The state before step `g`. -/
def stAt : Nat → σ
  | 0 => init
  | g + 1 => (F g (stAt g)).getD (stAt g)

/-- Every step below `n` passes. -/
def AllOk (n : Nat) : Prop := ∀ g, g < n → (F g (stAt F init g)).isSome = true

/-- The steps `ks` from state `t`: the state after them if all pass. -/
def segFrom : List Nat → σ → Option σ
  | [], t => some t
  | k :: ks, t => match F k t with
    | some t' => segFrom ks t'
    | none => none

theorem segFrom_ok : ∀ (m a : Nat) (t : σ), segFrom F (List.range' a m) (stAt F init a) = some t →
    t = stAt F init (a + m) ∧ ∀ k, a ≤ k → k < a + m → (F k (stAt F init k)).isSome = true
  | 0, a, t, h => by
    simp only [List.range'_zero, segFrom, Option.some.injEq] at h
    exact ⟨h.symm, fun k h1 h2 => by omega⟩
  | m + 1, a, t, h => by
    rw [List.range'_succ] at h
    simp only [segFrom] at h
    cases hk : F a (stAt F init a) with
    | none => rw [hk] at h; cases h
    | some t' =>
      rw [hk] at h
      have e : t' = stAt F init (a + 1) := by simp [stAt, hk]
      rw [e] at h
      obtain ⟨ht, hr⟩ := segFrom_ok m (a + 1) t h
      refine ⟨by rw [ht]; congr 1; omega, fun k h1 h2 => ?_⟩
      by_cases e' : k = a
      · subst e'; rw [hk]; rfl
      · exact hr k (by omega) (by omega)

variable (beq : σ → σ → Bool)

/-- One checkpointed segment: `m` steps from `a`, from state `ta` to state `tb`. -/
def segChk (a m : Nat) (ta tb : σ) : Bool :=
  match segFrom F (List.range' a m) ta with
  | some t => beq t tb
  | none => false

/-- Checkpointed segments from step `a`; after the last, the rest pass. -/
def ChainFrom (n : Nat) (a : Nat) (ta : σ) : List (Nat × σ) → Prop
  | [] => (segFrom F (List.range' a (n - a)) ta).isSome = true
  | (m, tb) :: rest => segChk F beq a m ta tb = true ∧ ChainFrom n (a + m) tb rest

theorem chainFrom_ok (hb : ∀ a b, beq a b = true → a = b) (n : Nat) :
    ∀ (cs : List (Nat × σ)) (a : Nat), ChainFrom F beq n a (stAt F init a) cs →
      ∀ k, a ≤ k → k < n → (F k (stAt F init k)).isSome = true
  | [], a, h, k, h1, h2 => by
    unfold ChainFrom at h
    cases hs : segFrom F (List.range' a (n - a)) (stAt F init a) with
    | none => rw [hs] at h; cases h
    | some t => exact (segFrom_ok F init (n - a) a t hs).2 k h1 (by omega)
  | (m, tb) :: rest, a, h, k, h1, h2 => by
    obtain ⟨hc, hr⟩ := h
    unfold segChk at hc
    split at hc
    · next t ht =>
      have e := hb _ _ hc
      obtain ⟨ht', hseg⟩ := segFrom_ok F init m a t ht
      rw [← e, ht'] at hr
      by_cases hk : k < a + m
      · exact hseg k h1 hk
      · exact chainFrom_ok hb n rest (a + m) hr k (by omega) h2
    · cases hc

/-- **Every step passes**, from a chain of checkpointed segments: each segment
    is its own kernel check, so memory per check is bounded by the segment. -/
theorem ok_of_chain (hb : ∀ a b, beq a b = true → a = b) (n : Nat) (cs : List (Nat × σ))
    (h : ChainFrom F beq n 0 init cs) : AllOk F init n :=
  fun k hk => chainFrom_ok F init beq hb n cs 0 h k (Nat.zero_le _) hk

end Chain

/-! ## Bit sets -/

/-- Bit set inclusion. -/
def Sub (a b : Nat) : Prop := ∀ i, a.testBit i = true → b.testBit i = true

/-- `a ⊆ b`, decided. -/
def subB (a b : Nat) : Bool := Nat.beq (Nat.land a b) a

/-- `a` without the bits of `b`. -/
def clr (a b : Nat) : Nat := Nat.xor a (Nat.land a b)

theorem testBit_of_subB {a b i : Nat} (h : subB a b = true) (hi : a.testBit i = true) : b.testBit i = true := by
  have e : a &&& b = a := Nat.eq_of_beq_eq_true h
  have := congrArg (fun x => x.testBit i) e
  simp only [Nat.testBit_and, hi, Bool.true_and] at this
  exact this

theorem testBit_clr (a b i : Nat) : (clr a b).testBit i = (a.testBit i && !b.testBit i) := by
  show (a ^^^ (a &&& b)).testBit i = _
  simp only [Nat.testBit_xor, Nat.testBit_and]
  cases a.testBit i <;> cases b.testBit i <;> rfl

theorem testBit_pow (k i : Nat) : (Nat.pow 2 k).testBit i = decide (k = i) := Nat.testBit_two_pow

theorem tb_eq (x i : Nat) : Nat.beq (Nat.land (Nat.shiftRight x i) 1) 1 = x.testBit i := by
  show ((x >>> i) &&& 1 == 1) = (1 &&& (x >>> i) != 0)
  rw [Nat.and_one_is_mod, Nat.one_and_eq_mod_two]
  rcases Nat.mod_two_eq_zero_or_one (x >>> i) with h | h <;> rw [h] <;> rfl

theorem testBit_or_l {a b i : Nat} (h : a.testBit i = true) : (a ||| b).testBit i = true := by
  simp [Nat.testBit_or, h]

theorem testBit_or_r {a b i : Nat} (h : b.testBit i = true) : (a ||| b).testBit i = true := by
  simp [Nat.testBit_or, h]

theorem testBit_or_cases {a b i : Nat} (h : (a ||| b).testBit i = true) : a.testBit i = true ∨ b.testBit i = true := by
  simpa [Nat.testBit_or] using h

theorem Sub.refl (a : Nat) : Sub a a := fun _ h => h
theorem Sub.trans {a b c : Nat} (h1 : Sub a b) (h2 : Sub b c) : Sub a c := fun i h => h2 i (h1 i h)
theorem Sub.or_l (a b : Nat) : Sub a (a ||| b) := fun _ h => testBit_or_l h
theorem Sub.or_r (a b : Nat) : Sub b (a ||| b) := fun _ h => testBit_or_r h
theorem Sub.or_mono {a b c d : Nat} (h1 : Sub a c) (h2 : Sub b d) : Sub (a ||| b) (c ||| d) := fun i h => by
  rcases testBit_or_cases h with h | h
  · exact testBit_or_l (h1 i h)
  · exact testBit_or_r (h2 i h)

/-! ## The scan -/

/-- A slot's record: last step to use it (plus one), methods and rules that used it (bits),
    and the OR of those rules' hit sets. -/
structure SS where
  last : Nat
  um : Nat
  ur : Nat
  hu : Nat

def SS.z : SS := ⟨0, 0, 0, 0⟩

def SS.beq (a b : SS) : Bool := a.last == b.last && a.um == b.um && a.ur == b.ur && a.hu == b.hu

theorem SS.eq_of_beq {a b : SS} (h : SS.beq a b = true) : a = b := by
  cases a; cases b
  simp only [SS.beq, Bool.and_eq_true, beq_iff_eq] at h
  obtain ⟨⟨⟨h1, h2⟩, h3⟩, h4⟩ := h
  subst h1 h2 h3 h4; rfl

abbrev ST := Tr SS

def ST.init : ST := .lf SS.z

def gt (d : Nat) (t : ST) (k : Nat) : SS := Tr.get SS.z d t k

/-- A port use in a footprint: `(base, kind, code)`; its slot is `base + code`. -/
abbrev Ent := Nat × Nat × Nat

def Ent.slot (e : Ent) : Nat := Nat.add e.1 e.2.2

/-- Per kind and port `c`: codes `c` may not follow (`cin`) or be followed by (`cout`);
    the same within one caller (`gin`, `gout`). -/
structure KC where
  cin : List Nat
  cout : List Nat
  gin : List Nat
  gout : List Nat

/-- What the scan knows of a module.  Methods are steps `0 .. nM-1`, rules
    `nM .. nM+nR-1` in rank order. -/
structure Design where
  nM : Nat
  nR : Nat
  /-- slots are taken by their low `d` bits -/
  d : Nat
  fpM : Tab (List Ent)
  fpR : Tab (List Ent)
  kt : Tab (Tab KC)
  /-- by method `m2`, the methods `m1` with `¬ Follow m1 m2` (`I`), by `m1` the
      `m2` with `¬ Follow m1 m2` (`O`); the same for `Group` -/
  nfI : Tab Nat
  nfO : Tab Nat
  ngI : Tab Nat
  ngO : Tab Nat
  /-- by method `m'`: the methods that may hit a rule `m'` may not precede -/
  h3ok : Tab Nat
  /-- exception masks: methods by method, rules by rule, methods by rule -/
  exM : Tab Nat
  exR : Tab Nat
  exMR : Tab Nat
  /-- by rule: earlier rules whose pair has a dynamic `OkT` proof -/
  escR : Tab Nat

namespace Design
variable (D : Design)

def kc (κ c : Nat) : KC := (D.kt.get κ).get c

/-- The OR of `f` over the slots `b + c`, `c ∈ cs`. -/
def orAt (t : ST) (b : Nat) (f : SS → Nat) : List Nat → Nat
  | [] => 0
  | c :: cs => Nat.lor (f (gt D.d t (Nat.add b c))) (orAt t b f cs)

/-- No slot `b + c`, `c ∈ cs`, was used by step `g` so far. -/
def noneLast (t : ST) (b g : Nat) : List Nat → Bool
  | [] => true
  | c :: cs => !Nat.beq (gt D.d t (Nat.add b c)).last (Nat.add g 1) && noneLast t b g cs

/-- A method's record at a slot it uses. -/
def bumpM (k : Nat) (o : SS) : SS := ⟨Nat.add k 1, Nat.lor o.um (Nat.pow 2 k), o.ur, o.hu⟩

def mNew (k : Nat) (t : ST) (e : Ent) : ST := Tr.set D.d t e.slot (bumpM k (gt D.d t e.slot))

def mChk (k : Nat) (e : Ent) (t' : ST) : Bool :=
  subB (D.orAt t' e.1 SS.um (D.kc e.2.1 e.2.2).cin) (Nat.lor (D.nfI.get k) (clr (D.exM.get k) (Nat.pow 2 k))) &&
  subB (D.orAt t' e.1 SS.um (D.kc e.2.1 e.2.2).cout) (Nat.lor (D.nfO.get k) (clr (D.exM.get k) (Nat.pow 2 k))) &&
  subB (D.orAt t' e.1 SS.um (D.kc e.2.1 e.2.2).gin) (Nat.lor (D.ngI.get k) (clr (D.exM.get k) (Nat.pow 2 k))) &&
  subB (D.orAt t' e.1 SS.um (D.kc e.2.1 e.2.2).gout) (Nat.lor (D.ngO.get k) (clr (D.exM.get k) (Nat.pow 2 k)))

def mFin (k : Nat) (e : Ent) (t' : ST) : Option ST := bif D.mChk k e t' then some t' else none

/-- A method's use `e`: one-caller check, record, then the exported-annotation checks
    against every method so far (this one included). -/
def mEnt (k : Nat) (t : ST) (e : Ent) : Option ST :=
  bif D.noneLast t e.1 k (D.kc e.2.1 e.2.2).gin then D.mFin k e (D.mNew k t e) else none

def mEnts (k : Nat) : List Ent → ST → Option ST
  | [], t => some t
  | e :: es, t => match D.mEnt k t e with
    | some t' => mEnts k es t'
    | none => none

/-- A rule's accumulator: state; methods reaching it directly (`dh`) or through earlier rules
    (`ph`); methods it may not follow (`bm`). -/
structure Acc where
  t : ST
  dh : Nat
  ph : Nat
  bm : Nat

def bumpR (g i : Nat) (o : SS) : SS := ⟨Nat.add g 1, o.um, Nat.lor o.ur (Nat.pow 2 i), o.hu⟩

def rNew (i : Nat) (t : ST) (e : Ent) : ST := Tr.set D.d t e.slot (bumpR (Nat.add D.nM i) i (gt D.d t e.slot))

def rChk (i : Nat) (e : Ent) (t' : ST) : Bool :=
  subB (D.orAt t' e.1 SS.ur (D.kc e.2.1 e.2.2).cin) (Nat.lor (Nat.lor (D.exR.get i) (D.escR.get i)) (Nat.pow 2 i))

def rAcc (a : Acc) (e : Ent) (t' : ST) : Acc :=
  ⟨t', Nat.lor a.dh (D.orAt t' e.1 SS.um (D.kc e.2.1 e.2.2).cout), Nat.lor a.ph (D.orAt t' e.1 SS.hu (D.kc e.2.1 e.2.2).cout),
    Nat.lor a.bm (D.orAt t' e.1 SS.um (D.kc e.2.1 e.2.2).cin)⟩

def rFin (i : Nat) (a : Acc) (e : Ent) (t' : ST) : Option Acc := bif D.rChk i e t' then some (D.rAcc a e t') else none

/-- A rule step's use `e` (pass 1): one-caller check, record, then the rank
    check against earlier rules (`h4`), and the hit sets. -/
def rEnt (i : Nat) (a : Acc) (e : Ent) : Option Acc :=
  bif D.noneLast a.t e.1 (Nat.add D.nM i) (D.kc e.2.1 e.2.2).gin then D.rFin i a e (D.rNew i a.t e) else none

def rEnts (i : Nat) : List Ent → Acc → Option Acc
  | [], a => some a
  | e :: es, a => match D.rEnt i a e with
    | some a' => rEnts i es a'
    | none => none

def bumpH (H : Nat) (o : SS) : SS := ⟨o.last, o.um, o.ur, Nat.lor o.hu H⟩

/-- Pass 2: the rule's hit set into its slots. -/
def setHu (H : Nat) : List Ent → ST → ST
  | [], t => t
  | e :: es, t => setHu H es (Tr.set D.d t e.slot (bumpH H (gt D.d t e.slot)))

/-- `h3`: every method hitting the rule is exported SBR before, or C with,
    every method `m'` the rule may not follow. -/
def h3chk (H B : Nat) : Bool :=
  Nat.beq H 0 || (List.range D.nM).all (fun m =>
    !Nat.beq (Nat.land (Nat.shiftRight B m) 1) 1 || subB H (Nat.lor (D.h3ok.get m) (clr (D.exM.get m) (Nat.pow 2 m))))

def hitOf (i : Nat) (a : Acc) : Nat := clr (Nat.lor a.dh a.ph) (D.exMR.get i)

def rStep (i : Nat) (t : ST) : Option ST :=
  match D.rEnts i (D.fpR.get i) ⟨t, 0, 0, 0⟩ with
  | none => none
  | some a =>
    bif D.h3chk (D.hitOf i a) (clr a.bm (D.exMR.get i)) then some (D.setHu (D.hitOf i a) (D.fpR.get i) a.t)
    else none

def mStep (k : Nat) (t : ST) : Option ST := D.mEnts k (D.fpM.get k) t

/-- **One step of the scan.** -/
def F (g : Nat) (t : ST) : Option ST := bif Nat.blt g D.nM then D.mStep g t else D.rStep (Nat.sub g D.nM) t

def n : Nat := D.nM + D.nR

def Ok : Prop := AllOk D.F ST.init D.n

def st (g : Nat) : ST := stAt D.F ST.init g

/-- The scan's hit set of rule `i`: the methods that must precede it. -/
def hitS (i : Nat) : Nat :=
  match D.rEnts i (D.fpR.get i) ⟨D.st (D.nM + i), 0, 0, 0⟩ with
  | some a => D.hitOf i a
  | none => 0

/-- A checkpointed segment, decided by the kernel. -/
def seg (a m : Nat) (ta tb : ST) : Bool := segChk D.F (Tr.beq SS.beq) a m ta tb

/-- The last segment. -/
def segLast (a : Nat) (ta : ST) : Bool := (segFrom D.F (List.range' a (D.n - a)) ta).isSome

theorem ok_of_chain (cs : List (Nat × ST)) (h : ChainFrom D.F (Tr.beq SS.beq) D.n 0 ST.init cs) : D.Ok :=
  Scan.ok_of_chain D.F ST.init (Tr.beq SS.beq) (fun _ _ h => Tr.eq_of_beq (fun _ _ => SS.eq_of_beq) h) D.n cs h

end Design


/-! ## The scan's invariants -/

theorem gt_set (d : Nat) (t : ST) (k k' : Nat) (v : SS) :
    gt d (Tr.set d t k v) k' = if k' % 2 ^ d = k % 2 ^ d then v else gt d t k' := Tr.get_set SS.z d t k k' v

theorem gt_congr (d : Nat) (t : ST) {k k' : Nat} (h : k % 2 ^ d = k' % 2 ^ d) : gt d t k = gt d t k' :=
  Tr.get_congr SS.z d t k k' h

theorem gt_set_self (d : Nat) (t : ST) (k : Nat) (v : SS) : gt d (Tr.set d t k v) k = v := by
  rw [gt_set]; simp

theorem gt_init (d s : Nat) : gt d ST.init s = SS.z := by
  unfold gt ST.init
  induction d generalizing s with
  | zero => rfl
  | succ d ih => rw [Tr.get_succ]; cases s % 2 == 0 <;> exact ih _

/-- A property of slots is kept by a set whose value has it. -/
theorem keep_set {Q : SS → Prop} (d : Nat) (t : ST) (k s : Nat) (v : SS) (h1 : Q (gt d t s)) (h2 : Q v) :
    Q (gt d (Tr.set d t k v) s) := by
  rw [gt_set]; split
  · exact h2
  · exact h1

/-- Bits only grow. -/
def SS.le (a b : SS) : Prop := Sub a.um b.um ∧ Sub a.ur b.ur ∧ Sub a.hu b.hu

theorem SS.le_refl (a : SS) : SS.le a a := ⟨Sub.refl _, Sub.refl _, Sub.refl _⟩

theorem SS.le_trans {a b c : SS} (h1 : SS.le a b) (h2 : SS.le b c) : SS.le a c :=
  ⟨h1.1.trans h2.1, h1.2.1.trans h2.2.1, h1.2.2.trans h2.2.2⟩

def Le (d : Nat) (t t' : ST) : Prop := ∀ s, SS.le (gt d t s) (gt d t' s)

theorem Le.refl (d : Nat) (t : ST) : Le d t t := fun _ => SS.le_refl _

theorem Le.trans {d : Nat} {t1 t2 t3 : ST} (h1 : Le d t1 t2) (h2 : Le d t2 t3) : Le d t1 t3 :=
  fun s => SS.le_trans (h1 s) (h2 s)

theorem le_set (d : Nat) (t : ST) (k : Nat) (v : SS) (h : SS.le (gt d t k) v) : Le d t (Tr.set d t k v) := by
  intro s
  rw [gt_set]
  split
  · next e => rw [gt_congr d t e]; exact h
  · exact SS.le_refl _

theorem le_bumpM (k : Nat) (o : SS) : SS.le o (Design.bumpM k o) := ⟨Sub.or_l _ _, Sub.refl _, Sub.refl _⟩
theorem le_bumpR (g i : Nat) (o : SS) : SS.le o (Design.bumpR g i o) := ⟨Sub.refl _, Sub.or_l _ _, Sub.refl _⟩
theorem le_bumpH (H : Nat) (o : SS) : SS.le o (Design.bumpH H o) := ⟨Sub.refl _, Sub.refl _, Sub.or_l _ _⟩

/-- `[x, y] <+ l` splits `l` at `y` with `x` before it. -/
theorem sub_pair_split {β : Type} {x y : β} : ∀ {l : List β}, [x, y] <+ l → ∃ pre post, l = pre ++ y :: post ∧ x ∈ pre
  | [], h => absurd h.length_le (by simp)
  | z :: l, h => by
    rcases List.sublist_cons_iff.mp h with h' | ⟨r, hr, hs⟩
    · obtain ⟨pre, post, e, hx⟩ := sub_pair_split h'
      exact ⟨z :: pre, post, by rw [e]; rfl, List.mem_cons_of_mem _ hx⟩
    · simp only [List.cons.injEq] at hr
      obtain ⟨rfl, rfl⟩ := hr
      obtain ⟨pre, post, e⟩ := List.append_of_mem (List.singleton_sublist.mp hs)
      exact ⟨x :: pre, post, by rw [e]; rfl, List.mem_cons_self ..⟩

/-- Two members of a list: one of them at a split with the other before it (or equal). -/
theorem either_pos {β : Type} [DecidableEq β] {x y : β} {l : List β} (hx : x ∈ l) (hy : y ∈ l) :
    (∃ pre post, l = pre ++ y :: post ∧ (x ∈ pre ∨ x = y)) ∨ (∃ pre post, l = pre ++ x :: post ∧ y ∈ pre) := by
  by_cases e : x = y
  · subst e
    obtain ⟨pre, post, h⟩ := List.append_of_mem hy
    exact .inl ⟨pre, post, h, .inr rfl⟩
  · rcases either_sublist hx hy e with h | h
    · obtain ⟨pre, post, h1, h2⟩ := sub_pair_split h
      exact .inl ⟨pre, post, h1, .inl h2⟩
    · exact .inr (sub_pair_split h)

namespace Design
variable (D : Design)

theorem testBit_orAt (t : ST) (b : Nat) (f : SS → Nat) {c : Nat} {cs : List Nat} (hc : c ∈ cs) {i : Nat}
    (h : (f (gt D.d t (b + c))).testBit i = true) : (D.orAt t b f cs).testBit i = true := by
  induction cs with
  | nil => cases hc
  | cons c' cs ih =>
    simp only [orAt]
    rcases List.mem_cons.mp hc with rfl | hc'
    · exact testBit_or_l h
    · exact testBit_or_r (ih hc')

theorem noneLast_spec {t : ST} {b g : Nat} {cs : List Nat} (h : D.noneLast t b g cs = true) {c : Nat}
    (hc : c ∈ cs) : (gt D.d t (b + c)).last ≠ g + 1 := by
  induction cs with
  | nil => cases hc
  | cons c' cs ih =>
    simp only [noneLast, Bool.and_eq_true] at h
    rcases List.mem_cons.mp hc with rfl | hc'
    · intro e
      have h1 := h.1
      have h2 : Nat.beq (gt D.d t (Nat.add b c)).last (Nat.add g 1) = true := by
        show Nat.beq (gt D.d t (b + c)).last (g + 1) = true
        rw [e]; exact Nat.beq_refl _
      rw [h2] at h1; cases h1
    · exact ih h.2 hc'

/-! ### Method steps -/

theorem mEnt_spec {k : Nat} {t t' : ST} {e : Ent} (h : D.mEnt k t e = some t') :
    D.noneLast t e.1 k (D.kc e.2.1 e.2.2).gin = true ∧ t' = D.mNew k t e ∧ D.mChk k e t' = true := by
  unfold mEnt at h
  cases hn : D.noneLast t e.1 k (D.kc e.2.1 e.2.2).gin
  · rw [hn] at h; cases h
  · rw [hn, Bool.cond_true] at h
    unfold mFin at h
    cases hc : D.mChk k e (D.mNew k t e)
    · rw [hc] at h; cases h
    · rw [hc, Bool.cond_true] at h
      cases h
      exact ⟨rfl, rfl, hc⟩

theorem mEnt_le {k : Nat} {t t' : ST} {e : Ent} (h : D.mEnt k t e = some t') : Le D.d t t' := by
  rw [(D.mEnt_spec h).2.1]; exact le_set _ _ _ _ (le_bumpM _ _)

/-- What a method step's use leaves at its slot. -/
def MarkM (k : Nat) (o : SS) : Prop := o.last = k + 1 ∧ o.um.testBit k = true

theorem markM_bump (k : Nat) (o : SS) : MarkM k (bumpM k o) :=
  ⟨rfl, testBit_or_r (by rw [testBit_pow]; simp)⟩

theorem mEnt_keep {Q : SS → Prop} (hQ : ∀ o, Q (bumpM k o)) {t t' : ST} {e : Ent} (h : D.mEnt k t e = some t')
    (s : Nat) (hs : Q (gt D.d t s)) : Q (gt D.d t' s) := by
  rw [(D.mEnt_spec h).2.1]; exact keep_set _ _ _ _ _ hs (hQ _)

theorem mEnt_mark {k : Nat} {t t' : ST} {e : Ent} (h : D.mEnt k t e = some t') : MarkM k (gt D.d t' e.slot) := by
  rw [(D.mEnt_spec h).2.1]; unfold mNew; rw [gt_set_self]; exact markM_bump _ _

theorem mEnts_split {k : Nat} : ∀ (pre : List Ent) (e : Ent) (post : List Ent) (t t'' : ST),
    D.mEnts k (pre ++ e :: post) t = some t'' →
    ∃ tp tp', D.mEnts k pre t = some tp ∧ D.mEnt k tp e = some tp' ∧ D.mEnts k post tp' = some t''
  | [], e, post, t, t'', h => by
    simp only [List.nil_append, mEnts] at h
    cases he : D.mEnt k t e with
    | none => rw [he] at h; cases h
    | some t' => rw [he] at h; exact ⟨t, t', rfl, he, h⟩
  | x :: pre, e, post, t, t'', h => by
    simp only [List.cons_append, mEnts] at h
    cases hx : D.mEnt k t x with
    | none => rw [hx] at h; cases h
    | some t1 =>
      rw [hx] at h
      obtain ⟨tp, tp', h1, h2, h3⟩ := mEnts_split pre e post t1 t'' h
      exact ⟨tp, tp', by simp only [mEnts, hx]; exact h1, h2, h3⟩

theorem mEnts_le {k : Nat} : ∀ (es : List Ent) (t t' : ST), D.mEnts k es t = some t' → Le D.d t t'
  | [], t, t', h => by simp only [mEnts, Option.some.injEq] at h; subst h; exact Le.refl _ _
  | e :: es, t, t', h => by
    simp only [mEnts] at h
    cases he : D.mEnt k t e with
    | none => rw [he] at h; cases h
    | some t1 => rw [he] at h; exact (D.mEnt_le he).trans (mEnts_le es t1 t' h)

theorem mEnts_keep {k : Nat} {Q : SS → Prop} (hQ : ∀ o, Q (bumpM k o)) :
    ∀ (es : List Ent) (t t' : ST), D.mEnts k es t = some t' → ∀ s, Q (gt D.d t s) → Q (gt D.d t' s)
  | [], t, t', h, s, hs => by simp only [mEnts, Option.some.injEq] at h; subst h; exact hs
  | e :: es, t, t', h, s, hs => by
    simp only [mEnts] at h
    cases he : D.mEnt k t e with
    | none => rw [he] at h; cases h
    | some t1 => rw [he] at h; exact mEnts_keep hQ es t1 t' h s (D.mEnt_keep hQ he s hs)

theorem mEnts_mark {k : Nat} : ∀ (es : List Ent) (t t' : ST), D.mEnts k es t = some t' →
    ∀ e ∈ es, MarkM k (gt D.d t' e.slot)
  | [], _, _, _, e, he => by cases he
  | x :: es, t, t', h, e, he => by
    simp only [mEnts] at h
    cases hx : D.mEnt k t x with
    | none => rw [hx] at h; cases h
    | some t1 =>
      rw [hx] at h
      rcases List.mem_cons.mp he with rfl | he'
      · exact D.mEnts_keep (markM_bump k) es t1 t' h _ (D.mEnt_mark hx)
      · exact mEnts_mark es t1 t' h e he'

/-! ### Rule steps -/

theorem rEnt_spec {i : Nat} {a a' : Acc} {e : Ent} (h : D.rEnt i a e = some a') :
    D.noneLast a.t e.1 (Nat.add D.nM i) (D.kc e.2.1 e.2.2).gin = true ∧ D.rChk i e (D.rNew i a.t e) = true ∧
      a' = D.rAcc a e (D.rNew i a.t e) := by
  unfold rEnt at h
  cases hn : D.noneLast a.t e.1 (Nat.add D.nM i) (D.kc e.2.1 e.2.2).gin
  · rw [hn] at h; cases h
  · rw [hn, Bool.cond_true] at h
    unfold rFin at h
    cases hc : D.rChk i e (D.rNew i a.t e)
    · rw [hc] at h; cases h
    · rw [hc, Bool.cond_true] at h
      cases h
      exact ⟨rfl, rfl, rfl⟩

/-- The accumulator only grows. -/
def AccLe (d : Nat) (a a' : Acc) : Prop := Le d a.t a'.t ∧ Sub a.dh a'.dh ∧ Sub a.ph a'.ph ∧ Sub a.bm a'.bm

theorem AccLe.refl (d : Nat) (a : Acc) : AccLe d a a := ⟨Le.refl _ _, Sub.refl _, Sub.refl _, Sub.refl _⟩

theorem AccLe.trans {d : Nat} {a b c : Acc} (h1 : AccLe d a b) (h2 : AccLe d b c) : AccLe d a c :=
  ⟨h1.1.trans h2.1, h1.2.1.trans h2.2.1, h1.2.2.1.trans h2.2.2.1, h1.2.2.2.trans h2.2.2.2⟩

theorem rEnt_le {i : Nat} {a a' : Acc} {e : Ent} (h : D.rEnt i a e = some a') : AccLe D.d a a' := by
  rw [(D.rEnt_spec h).2.2]
  exact ⟨le_set _ _ _ _ (le_bumpR _ _ _), Sub.or_l _ _, Sub.or_l _ _, Sub.or_l _ _⟩

def MarkR (g i : Nat) (o : SS) : Prop := o.last = g + 1 ∧ o.ur.testBit i = true

theorem markR_bump (g i : Nat) (o : SS) : MarkR g i (bumpR g i o) :=
  ⟨rfl, testBit_or_r (by rw [testBit_pow]; simp)⟩

theorem rEnt_keep {i : Nat} {Q : SS → Prop} (hQ : ∀ o, Q (bumpR (D.nM + i) i o)) {a a' : Acc} {e : Ent}
    (h : D.rEnt i a e = some a') (s : Nat) (hs : Q (gt D.d a.t s)) : Q (gt D.d a'.t s) := by
  rw [(D.rEnt_spec h).2.2]; exact keep_set _ _ _ _ _ hs (hQ _)

theorem rEnt_mark {i : Nat} {a a' : Acc} {e : Ent} (h : D.rEnt i a e = some a') :
    MarkR (D.nM + i) i (gt D.d a'.t e.slot) := by
  rw [(D.rEnt_spec h).2.2]; unfold rAcc rNew; dsimp only; rw [gt_set_self]; exact markR_bump _ _ _

theorem rEnts_split {i : Nat} : ∀ (pre : List Ent) (e : Ent) (post : List Ent) (a a'' : Acc),
    D.rEnts i (pre ++ e :: post) a = some a'' →
    ∃ ap ap', D.rEnts i pre a = some ap ∧ D.rEnt i ap e = some ap' ∧ D.rEnts i post ap' = some a''
  | [], e, post, a, a'', h => by
    simp only [List.nil_append, rEnts] at h
    cases he : D.rEnt i a e with
    | none => rw [he] at h; cases h
    | some a' => rw [he] at h; exact ⟨a, a', rfl, he, h⟩
  | x :: pre, e, post, a, a'', h => by
    simp only [List.cons_append, rEnts] at h
    cases hx : D.rEnt i a x with
    | none => rw [hx] at h; cases h
    | some a1 =>
      rw [hx] at h
      obtain ⟨ap, ap', h1, h2, h3⟩ := rEnts_split pre e post a1 a'' h
      exact ⟨ap, ap', by simp only [rEnts, hx]; exact h1, h2, h3⟩

theorem rEnts_le {i : Nat} : ∀ (es : List Ent) (a a' : Acc), D.rEnts i es a = some a' → AccLe D.d a a'
  | [], a, a', h => by simp only [rEnts, Option.some.injEq] at h; subst h; exact AccLe.refl _ _
  | e :: es, a, a', h => by
    simp only [rEnts] at h
    cases he : D.rEnt i a e with
    | none => rw [he] at h; cases h
    | some a1 => rw [he] at h; exact (D.rEnt_le he).trans (rEnts_le es a1 a' h)

theorem rEnts_keep {i : Nat} {Q : SS → Prop} (hQ : ∀ o, Q (bumpR (D.nM + i) i o)) :
    ∀ (es : List Ent) (a a' : Acc), D.rEnts i es a = some a' → ∀ s, Q (gt D.d a.t s) → Q (gt D.d a'.t s)
  | [], a, a', h, s, hs => by simp only [rEnts, Option.some.injEq] at h; subst h; exact hs
  | e :: es, a, a', h, s, hs => by
    simp only [rEnts] at h
    cases he : D.rEnt i a e with
    | none => rw [he] at h; cases h
    | some a1 => rw [he] at h; exact rEnts_keep hQ es a1 a' h s (D.rEnt_keep hQ he s hs)

theorem rEnts_mark {i : Nat} : ∀ (es : List Ent) (a a' : Acc), D.rEnts i es a = some a' →
    ∀ e ∈ es, MarkR (D.nM + i) i (gt D.d a'.t e.slot)
  | [], _, _, _, e, he => by cases he
  | x :: es, a, a', h, e, he => by
    simp only [rEnts] at h
    cases hx : D.rEnt i a x with
    | none => rw [hx] at h; cases h
    | some a1 =>
      rw [hx] at h
      rcases List.mem_cons.mp he with rfl | he'
      · exact D.rEnts_keep (markR_bump _ i) es a1 a' h _ (D.rEnt_mark hx)
      · exact rEnts_mark es a1 a' h e he'

theorem setHu_le (H : Nat) : ∀ (es : List Ent) (t : ST), Le D.d t (D.setHu H es t)
  | [], _ => Le.refl _ _
  | _ :: es, _ => (le_set _ _ _ _ (le_bumpH _ _)).trans (setHu_le H es _)

theorem setHu_keep {Q : SS → Prop} (H : Nat) (hQ : ∀ o, Q o → Q (bumpH H o)) :
    ∀ (es : List Ent) (t : ST), (∀ s, Q (gt D.d t s)) → ∀ s, Q (gt D.d (D.setHu H es t) s)
  | [], _, h, s => h s
  | _ :: es, _, h, s => setHu_keep H hQ es _ (fun s' => keep_set _ _ _ _ _ (h s') (hQ _ (h _))) s

theorem setHu_mark (H : Nat) : ∀ (es : List Ent) (t : ST), ∀ e ∈ es, Sub H (gt D.d (D.setHu H es t) e.slot).hu
  | [], _, _, he => by cases he
  | x :: es, t, e, he => by
    rcases List.mem_cons.mp he with rfl | he'
    · have h0 : Sub H (gt D.d (Tr.set D.d t e.slot (bumpH H (gt D.d t e.slot))) e.slot).hu := by
        rw [gt_set_self]; exact Sub.or_r _ _
      have := (D.setHu_le H es (Tr.set D.d t e.slot (bumpH H (gt D.d t e.slot)))) e.slot
      exact h0.trans this.2.2
    · exact setHu_mark H es _ e he'

theorem rStep_spec {i : Nat} {t t' : ST} (h : D.rStep i t = some t') :
    ∃ a, D.rEnts i (D.fpR.get i) ⟨t, 0, 0, 0⟩ = some a ∧ D.h3chk (D.hitOf i a) (clr a.bm (D.exMR.get i)) = true ∧
      t' = D.setHu (D.hitOf i a) (D.fpR.get i) a.t := by
  unfold rStep at h
  cases ha : D.rEnts i (D.fpR.get i) ⟨t, 0, 0, 0⟩ with
  | none => rw [ha] at h; cases h
  | some a =>
    rw [ha] at h
    simp only at h
    cases hc : D.h3chk (D.hitOf i a) (clr a.bm (D.exMR.get i))
    · rw [hc] at h; cases h
    · rw [hc, Bool.cond_true] at h
      cases h
      exact ⟨a, rfl, hc, rfl⟩

/-! ### The invariant along the pass -/

theorem F_lt {g : Nat} (h : g < D.nM) (t : ST) : D.F g t = D.mStep g t := by
  have : Nat.blt g D.nM = true := Nat.blt_eq.mpr h
  simp only [F, this, Bool.cond_true]

theorem F_ge {g : Nat} (h : ¬ g < D.nM) (t : ST) : D.F g t = D.rStep (g - D.nM) t := by
  have : Nat.blt g D.nM = false := by
    cases hb : Nat.blt g D.nM
    · rfl
    · exact absurd (Nat.blt_eq.mp hb) h
  simp only [F, this, Bool.cond_false]
  rfl

theorem st_succ {g : Nat} {t' : ST} (h : D.F g (D.st g) = some t') : D.st (g + 1) = t' := by
  show (D.F g (D.st g)).getD (D.st g) = t'
  rw [h]; rfl

structure Inv (g : Nat) (t : ST) : Prop where
  um : ∀ m, m < g → m < D.nM → ∀ e ∈ D.fpM.get m, (gt D.d t e.slot).um.testBit m = true
  ur : ∀ i, D.nM + i < g → ∀ e ∈ D.fpR.get i, (gt D.d t e.slot).ur.testBit i = true
  hu : ∀ i, D.nM + i < g → ∀ e ∈ D.fpR.get i, Sub (D.hitS i) (gt D.d t e.slot).hu
  last : ∀ s, (gt D.d t s).last ≤ g

theorem inv_zero : D.Inv 0 ST.init where
  um := fun m h => absurd h (Nat.not_lt_zero _)
  ur := fun i h => absurd h (Nat.not_lt_zero _)
  hu := fun i h => absurd h (Nat.not_lt_zero _)
  last := fun s => by rw [gt_init]; exact Nat.le_refl _

theorem inv_succ {g : Nat} (hI : D.Inv g (D.st g)) (hok : (D.F g (D.st g)).isSome = true) :
    D.Inv (g + 1) (D.st (g + 1)) := by
  cases hF : D.F g (D.st g) with
  | none => rw [hF] at hok; cases hok
  | some t' =>
    rw [D.st_succ hF]
    by_cases hg : g < D.nM
    · rw [D.F_lt hg] at hF
      have hle := D.mEnts_le _ _ _ hF
      refine ⟨fun m hm hmM e he => ?_, fun i hi => absurd hi (by omega), fun i hi => absurd hi (by omega), ?_⟩
      · by_cases e' : m < g
        · exact (hle e.slot).1 m (hI.um m e' hmM e he)
        · have : m = g := by omega
          subst this
          exact (D.mEnts_mark _ _ _ hF e he).2
      · exact fun s => D.mEnts_keep (Q := fun o => o.last ≤ g + 1) (fun o => Nat.le_refl _) _ _ _ hF s
          (Nat.le_succ_of_le (hI.last s))
    · rw [D.F_ge hg] at hF
      obtain ⟨a, ha, -, rfl⟩ := D.rStep_spec hF
      have hle1 := (D.rEnts_le _ _ _ ha).1
      have hle2 := D.setHu_le (D.hitOf (g - D.nM) a) (D.fpR.get (g - D.nM)) a.t
      have hle : Le D.d (D.st g) _ := hle1.trans hle2
      have hg' : D.nM + (g - D.nM) = g := by omega
      refine ⟨fun m hm hmM e he => (hle e.slot).1 m (hI.um m (by omega) hmM e he), fun i hi e he => ?_,
        fun i hi e he => ?_, ?_⟩
      · by_cases e' : D.nM + i < g
        · exact (hle e.slot).2.1 i (hI.ur i e' e he)
        · have : i = g - D.nM := by omega
          subst this
          exact (hle2 e.slot).2.1 _ (D.rEnts_mark _ _ _ ha e he).2
      · by_cases e' : D.nM + i < g
        · exact (hI.hu i e' e he).trans (hle e.slot).2.2
        · have : i = g - D.nM := by omega
          subst this
          have hh : D.hitS (g - D.nM) = D.hitOf (g - D.nM) a := by
            unfold hitS; rw [hg', ha]
          rw [hh]
          exact D.setHu_mark _ _ _ e he
      · have h1 : ∀ s, (gt D.d a.t s).last ≤ g + 1 := fun s =>
          D.rEnts_keep (Q := fun o => o.last ≤ g + 1) (fun o => by show D.nM + (g - D.nM) + 1 ≤ g + 1; omega)
            _ _ _ ha s (Nat.le_succ_of_le (hI.last s))
        exact D.setHu_keep (Q := fun o => o.last ≤ g + 1) _ (fun o h => h) _ _ h1

theorem inv_st (hok : D.Ok) : ∀ g, g ≤ D.n → D.Inv g (D.st g)
  | 0, _ => D.inv_zero
  | g + 1, h => D.inv_succ (inv_st hok g (by omega)) (hok g (by omega))

/-! ### What each step's checks give -/

theorem mStep_at (hok : D.Ok) {k : Nat} (hk : k < D.nM) {pre post : List Ent} {e : Ent}
    (hl : D.fpM.get k = pre ++ e :: post) :
    ∃ tp tp', D.mEnts k pre (D.st k) = some tp ∧ D.mEnt k tp e = some tp' ∧ Le D.d (D.st k) tp ∧
      ∀ x ∈ pre, MarkM k (gt D.d tp x.slot) := by
  have h : (D.F k (D.st k)).isSome = true := hok k (by unfold n; omega)
  rw [D.F_lt hk] at h
  cases hs : D.mStep k (D.st k) with
  | none => rw [hs] at h; cases h
  | some t'' =>
    unfold mStep at hs; rw [hl] at hs
    obtain ⟨tp, tp', h1, h2, -⟩ := D.mEnts_split pre e post _ _ hs
    exact ⟨tp, tp', h1, h2, D.mEnts_le _ _ _ h1, fun x hx => D.mEnts_mark _ _ _ h1 x hx⟩

theorem rStep_at (hok : D.Ok) {i : Nat} (hi : i < D.nR) :
    ∃ a, D.rEnts i (D.fpR.get i) ⟨D.st (D.nM + i), 0, 0, 0⟩ = some a ∧ D.hitS i = D.hitOf i a ∧
      D.h3chk (D.hitOf i a) (clr a.bm (D.exMR.get i)) = true := by
  have h : (D.F (D.nM + i) (D.st (D.nM + i))).isSome = true := hok (D.nM + i) (by unfold n; omega)
  rw [D.F_ge (by omega)] at h
  have e : D.nM + i - D.nM = i := by omega
  rw [e] at h
  cases hs : D.rStep i (D.st (D.nM + i)) with
  | none => rw [hs] at h; cases h
  | some t' =>
    obtain ⟨a, ha, hc, -⟩ := D.rStep_spec hs
    exact ⟨a, ha, by unfold hitS; rw [ha], hc⟩

/-- At a rule's use `e`: the state before it, the accumulator after it, and the final one. -/
theorem rEnt_at (hok : D.Ok) {i : Nat} (hi : i < D.nR) {pre post : List Ent} {e : Ent}
    (hl : D.fpR.get i = pre ++ e :: post) :
    ∃ ap ap' a, D.rEnt i ap e = some ap' ∧ Le D.d (D.st (D.nM + i)) ap.t ∧ AccLe D.d ap' a ∧
      D.hitS i = D.hitOf i a ∧ D.h3chk (D.hitOf i a) (clr a.bm (D.exMR.get i)) = true ∧
      ∀ x ∈ pre, MarkR (D.nM + i) i (gt D.d ap.t x.slot) := by
  obtain ⟨a, ha, hh, hc⟩ := D.rStep_at hok hi
  rw [hl] at ha
  obtain ⟨ap, ap', h1, h2, h3⟩ := D.rEnts_split pre e post _ _ ha
  exact ⟨ap, ap', a, h2, (D.rEnts_le _ _ _ h1).1, D.rEnts_le _ _ _ h3, hh, hc,
    fun x hx => D.rEnts_mark _ _ _ h1 x hx⟩

/-- The checks of a method's use, at the state after its record. -/
theorem mChk_at {k : Nat} {e : Ent} {t' : ST} (h : D.mChk k e t' = true) (sel : KC → List Nat) (T : Tab Nat)
    (hsel : sel = KC.cin ∧ T = D.nfI ∨ sel = KC.cout ∧ T = D.nfO ∨ sel = KC.gin ∧ T = D.ngI ∨
      sel = KC.gout ∧ T = D.ngO)
    {c i : Nat} (hc : c ∈ sel (D.kc e.2.1 e.2.2)) (hb : (gt D.d t' (e.1 + c)).um.testBit i = true) :
    (T.get k).testBit i = true ∨ (i ≠ k ∧ (D.exM.get k).testBit i = true) := by
  unfold mChk at h
  simp only [Bool.and_eq_true] at h
  obtain ⟨⟨⟨h1, h2⟩, h3⟩, h4⟩ := h
  have key : ∀ (l : List Nat) (T' : Tab Nat), c ∈ l →
      subB (D.orAt t' e.1 SS.um l) (Nat.lor (T'.get k) (clr (D.exM.get k) (Nat.pow 2 k))) = true →
      (T'.get k).testBit i = true ∨ (i ≠ k ∧ (D.exM.get k).testBit i = true) := by
    intro l T' hl hs
    have := testBit_of_subB hs (D.testBit_orAt t' e.1 SS.um hl hb)
    rcases testBit_or_cases this with h | h
    · exact .inl h
    · rw [testBit_clr, testBit_pow] at h
      simp only [Bool.and_eq_true, Bool.not_eq_true', decide_eq_false_iff_not] at h
      exact .inr ⟨fun e => h.2 e.symm, h.1⟩
  rcases hsel with ⟨rfl, rfl⟩ | ⟨rfl, rfl⟩ | ⟨rfl, rfl⟩ | ⟨rfl, rfl⟩
  · exact key _ _ hc h1
  · exact key _ _ hc h2
  · exact key _ _ hc h3
  · exact key _ _ hc h4

/-! ### The static facts the scan establishes -/

/-- One caller, within a method step: a use may follow the step's earlier uses. -/
theorem grpM_static (hok : D.Ok) {k : Nat} (hk : k < D.nM) {b κ c1 c2 : Nat}
    (h : [(b, κ, c1), (b, κ, c2)] <+ D.fpM.get k) : c1 ∉ (D.kc κ c2).gin := by
  intro hc
  obtain ⟨pre, post, hl, hx⟩ := sub_pair_split h
  obtain ⟨tp, tp', -, h2, -, hm⟩ := D.mStep_at hok hk hl
  exact D.noneLast_spec (D.mEnt_spec h2).1 hc (hm _ hx).1

theorem grpR_static (hok : D.Ok) {i : Nat} (hi : i < D.nR) {b κ c1 c2 : Nat}
    (h : [(b, κ, c1), (b, κ, c2)] <+ D.fpR.get i) : c1 ∉ (D.kc κ c2).gin := by
  intro hc
  obtain ⟨pre, post, hl, hx⟩ := sub_pair_split h
  obtain ⟨ap, ap', a, h2, -, -, -, -, hm⟩ := D.rEnt_at hok hi hl
  exact D.noneLast_spec (D.rEnt_spec h2).1 hc (hm _ hx).1

/-- Two methods' uses in conflict: the methods are exported so, or exclusive.
    (`selI`, `selO`, `TI`, `TO`: the `Follow` tables, or the `Group` ones.) -/
theorem mm_static_gen (hok : D.Ok) (selI selO : KC → List Nat) (TI TO : Tab Nat)
    (hI : selI = KC.cin ∧ TI = D.nfI ∨ selI = KC.gin ∧ TI = D.ngI)
    (hO : selO = KC.cout ∧ TO = D.nfO ∨ selO = KC.gout ∧ TO = D.ngO)
    {m1 m2 : Nat} (h1 : m1 < D.nM) (h2 : m2 < D.nM) {b κ c1 c2 : Nat}
    (e1 : (b, κ, c1) ∈ D.fpM.get m1) (e2 : (b, κ, c2) ∈ D.fpM.get m2)
    (hin : c1 ∈ selI (D.kc κ c2)) (hout : c2 ∈ selO (D.kc κ c1)) :
    (TI.get m2).testBit m1 = true ∨ (TO.get m1).testBit m2 = true ∨
      (m1 ≠ m2 ∧ ((D.exM.get m2).testBit m1 = true ∨ (D.exM.get m1).testBit m2 = true)) := by
  have hI' : selI = KC.cin ∧ TI = D.nfI ∨ selI = KC.cout ∧ TI = D.nfO ∨ selI = KC.gin ∧ TI = D.ngI ∨
      selI = KC.gout ∧ TI = D.ngO := by
    rcases hI with h | h
    · exact .inl h
    · exact .inr (.inr (.inl h))
  have hO' : selO = KC.cin ∧ TO = D.nfI ∨ selO = KC.cout ∧ TO = D.nfO ∨ selO = KC.gin ∧ TO = D.ngI ∨
      selO = KC.gout ∧ TO = D.ngO := by
    rcases hO with h | h
    · exact .inr (.inl h)
    · exact .inr (.inr (.inr h))
  rcases Nat.lt_trichotomy m1 m2 with hlt | heq | hgt
  · -- m1 first: checked at m2's use, against m1's record
    obtain ⟨pre, post, hl⟩ := List.append_of_mem e2
    obtain ⟨tp, tp', -, h2', hle, -⟩ := D.mStep_at hok h2 hl
    have hb := ((hle.trans (D.mEnt_le h2')) (b + c1)).1 m1
      ((D.inv_st hok m2 (by unfold n; omega)).um m1 hlt h1 _ e1)
    rcases D.mChk_at (D.mEnt_spec h2').2.2 selI TI hI' hin hb with h | ⟨hne, h⟩
    · exact .inl h
    · exact .inr (.inr ⟨fun e => hne e, .inl h⟩)
  · subst heq
    rcases either_pos e1 e2 with ⟨pre, post, hl, hx⟩ | ⟨pre, post, hl, hy⟩
    · obtain ⟨tp, tp', -, h2', -, hm⟩ := D.mStep_at hok h1 hl
      have hb : (gt D.d tp' (b + c1)).um.testBit m1 = true := by
        rcases hx with hx | hx
        · exact ((D.mEnt_le h2') (b + c1)).1 m1 (hm _ hx).2
        · cases hx; exact (D.mEnt_mark h2').2
      rcases D.mChk_at (D.mEnt_spec h2').2.2 selI TI hI' hin hb with h | ⟨hne, -⟩
      · exact .inl h
      · exact absurd rfl hne
    · obtain ⟨tp, tp', -, h2', -, hm⟩ := D.mStep_at hok h1 hl
      have hb : (gt D.d tp' (b + c2)).um.testBit m1 = true := ((D.mEnt_le h2') (b + c2)).1 m1 (hm _ hy).2
      rcases D.mChk_at (D.mEnt_spec h2').2.2 selO TO hO' hout hb with h | ⟨hne, -⟩
      · exact .inr (.inl h)
      · exact absurd rfl hne
  · obtain ⟨pre, post, hl⟩ := List.append_of_mem e1
    obtain ⟨tp, tp', -, h2', hle, -⟩ := D.mStep_at hok h1 hl
    have hb := ((hle.trans (D.mEnt_le h2')) (b + c2)).1 m2
      ((D.inv_st hok m1 (by unfold n; omega)).um m2 hgt h2 _ e2)
    rcases D.mChk_at (D.mEnt_spec h2').2.2 selO TO hO' hout hb with h | ⟨hne, h⟩
    · exact .inr (.inl h)
    · exact .inr (.inr ⟨fun e => hne e.symm, .inr h⟩)

/-- `mm_static_gen` for a later (or the same) method `m1`, from `hout` alone; in the
    same method the two uses share their code (chain kinds: one slot per instance). -/
theorem mm_static_out (hok : D.Ok) (selO : KC → List Nat) (TO : Tab Nat)
    (hO : selO = KC.cout ∧ TO = D.nfO ∨ selO = KC.gout ∧ TO = D.ngO)
    {m1 m2 : Nat} (h1 : m1 < D.nM) (h2 : m2 < D.nM) (hle : m2 ≤ m1) {b κ c1 c2 : Nat}
    (hc : m1 = m2 → c1 = c2)
    (e1 : (b, κ, c1) ∈ D.fpM.get m1) (e2 : (b, κ, c2) ∈ D.fpM.get m2) (hout : c2 ∈ selO (D.kc κ c1)) :
    (TO.get m1).testBit m2 = true ∨ (m1 ≠ m2 ∧ (D.exM.get m1).testBit m2 = true) := by
  have hO' : selO = KC.cin ∧ TO = D.nfI ∨ selO = KC.cout ∧ TO = D.nfO ∨ selO = KC.gin ∧ TO = D.ngI ∨
      selO = KC.gout ∧ TO = D.ngO := by
    rcases hO with h | h
    · exact .inr (.inl h)
    · exact .inr (.inr (.inr h))
  rcases Nat.lt_or_eq_of_le hle with hgt | heq
  · obtain ⟨pre, post, hl⟩ := List.append_of_mem e1
    obtain ⟨tp, tp', -, h2', hle', -⟩ := D.mStep_at hok h1 hl
    have hb := ((hle'.trans (D.mEnt_le h2')) (b + c2)).1 m2
      ((D.inv_st hok m1 (by unfold n; omega)).um m2 hgt h2 _ e2)
    rcases D.mChk_at (D.mEnt_spec h2').2.2 selO TO hO' hout hb with h | ⟨hne, h⟩
    · exact .inl h
    · exact .inr ⟨fun e => hne e.symm, h⟩
  · subst heq
    have hcc := hc rfl
    subst hcc
    rcases either_pos e2 e1 with ⟨pre, post, hl, hx⟩ | ⟨pre, post, hl, hy⟩
    · obtain ⟨tp, tp', -, h2', -, hm⟩ := D.mStep_at hok h1 hl
      have hb : (gt D.d tp' (b + c1)).um.testBit m2 = true := by
        rcases hx with hx | hx
        · exact ((D.mEnt_le h2') (b + c1)).1 m2 (hm _ hx).2
        · exact (D.mEnt_mark h2').2
      rcases D.mChk_at (D.mEnt_spec h2').2.2 selO TO hO' hout hb with h | ⟨hne, -⟩
      · exact .inl h
      · exact absurd rfl hne
    · obtain ⟨tp, tp', -, h2', -, hm⟩ := D.mStep_at hok h1 hl
      have hb : (gt D.d tp' (b + c1)).um.testBit m2 = true := (D.mEnt_mark h2').2
      rcases D.mChk_at (D.mEnt_spec h2').2.2 selO TO hO' hout hb with h | ⟨hne, -⟩
      · exact .inl h
      · exact absurd rfl hne

/-- Rules in rank order: a later rule's use may follow an earlier rule's, or
    the two are exclusive, or escaped. -/
theorem h4_static (hok : D.Ok) {i1 i2 : Nat} (hlt : i1 < i2) (h2 : i2 < D.nR) {b κ c1 c2 : Nat}
    (e1 : (b, κ, c1) ∈ D.fpR.get i1) (e2 : (b, κ, c2) ∈ D.fpR.get i2) (hin : c1 ∈ (D.kc κ c2).cin) :
    (D.exR.get i2).testBit i1 = true ∨ (D.escR.get i2).testBit i1 = true := by
  obtain ⟨pre, post, hl⟩ := List.append_of_mem e2
  obtain ⟨ap, ap', a, h2', hle, -⟩ := D.rEnt_at hok h2 hl
  obtain ⟨-, hc, -⟩ := D.rEnt_spec h2'
  have hb : (gt D.d (D.rNew i2 ap.t (b, κ, c2)) (b + c1)).ur.testBit i1 = true :=
    ((hle.trans (le_set _ _ _ _ (le_bumpR _ _ _))) (b + c1)).2.1 i1
      ((D.inv_st hok (D.nM + i2) (by unfold n; omega)).ur i1 (by omega) _ e1)
  have := testBit_of_subB hc (D.testBit_orAt _ _ SS.ur hin hb)
  rcases testBit_or_cases this with h | h
  · rcases testBit_or_cases h with h | h
    · exact .inl h
    · exact .inr h
  · rw [testBit_pow] at h; simp at h; omega

/-- A rule's use a method's use may not follow: the method hits the rule. -/
theorem h1_static (hok : D.Ok) {m i : Nat} (hm : m < D.nM) (hi : i < D.nR) {b κ c1 c2 : Nat}
    (e1 : (b, κ, c1) ∈ D.fpR.get i) (e2 : (b, κ, c2) ∈ D.fpM.get m) (hout : c2 ∈ (D.kc κ c1).cout) :
    (D.hitS i).testBit m = true ∨ (D.exMR.get i).testBit m = true := by
  obtain ⟨pre, post, hl⟩ := List.append_of_mem e1
  obtain ⟨ap, ap', a, h2', hle, hfin, hh, -⟩ := D.rEnt_at hok hi hl
  obtain ⟨-, -, he⟩ := D.rEnt_spec h2'
  have hb : (gt D.d (D.rNew i ap.t (b, κ, c1)) (b + c2)).um.testBit m = true :=
    ((hle.trans (le_set _ _ _ _ (le_bumpR _ _ _))) (b + c2)).1 m
      ((D.inv_st hok (D.nM + i) (by unfold n; omega)).um m (by omega) hm _ e2)
  have hdh : a.dh.testBit m = true := by
    apply hfin.2.1
    rw [he]
    exact testBit_or_r (D.testBit_orAt _ _ SS.um hout hb)
  rw [hh]
  unfold hitOf
  cases hx : (D.exMR.get i).testBit m
  · left; rw [testBit_clr, hx]; simp only [Bool.not_false, Bool.and_true]; exact testBit_or_l hdh
  · exact .inr rfl

/-- Hits pass along: a later rule whose use an earlier rule's may not follow. -/
theorem h2_static (hok : D.Ok) {i1 i2 : Nat} (hlt : i1 < i2) (h2 : i2 < D.nR) {b κ c1 c2 : Nat}
    (e1 : (b, κ, c1) ∈ D.fpR.get i1) (e2 : (b, κ, c2) ∈ D.fpR.get i2) (hout : c1 ∈ (D.kc κ c2).cout)
    {m : Nat} (hm : (D.hitS i1).testBit m = true) :
    (D.hitS i2).testBit m = true ∨ (D.exMR.get i2).testBit m = true := by
  obtain ⟨pre, post, hl⟩ := List.append_of_mem e2
  obtain ⟨ap, ap', a, h2', hle, hfin, hh, -⟩ := D.rEnt_at hok h2 hl
  obtain ⟨-, -, he⟩ := D.rEnt_spec h2'
  have hb : (gt D.d (D.rNew i2 ap.t (b, κ, c2)) (b + c1)).hu.testBit m = true :=
    ((hle.trans (le_set _ _ _ _ (le_bumpR _ _ _))) (b + c1)).2.2 m
      ((D.inv_st hok (D.nM + i2) (by unfold n; omega)).hu i1 (by omega) _ e1 m hm)
  have hph : a.ph.testBit m = true := by
    apply hfin.2.2.1
    rw [he]
    exact testBit_or_r (D.testBit_orAt _ _ SS.hu hout hb)
  rw [hh]
  unfold hitOf
  cases hx : (D.exMR.get i2).testBit m
  · left; rw [testBit_clr, hx]; simp only [Bool.not_false, Bool.and_true]; exact testBit_or_r hph
  · exact .inr rfl

/-- A method hitting a rule that method `m'` may not precede: exported so. -/
theorem h3_static (hok : D.Ok) {m' i : Nat} (hm' : m' < D.nM) (hi : i < D.nR) {b κ c1 c2 : Nat}
    (e1 : (b, κ, c1) ∈ D.fpM.get m') (e2 : (b, κ, c2) ∈ D.fpR.get i) (hin : c1 ∈ (D.kc κ c2).cin)
    {m : Nat} (hm : (D.hitS i).testBit m = true) :
    (D.exMR.get i).testBit m' = true ∨ (D.h3ok.get m').testBit m = true ∨
      (m ≠ m' ∧ (D.exM.get m').testBit m = true) := by
  obtain ⟨pre, post, hl⟩ := List.append_of_mem e2
  obtain ⟨ap, ap', a, h2', hle, hfin, hh, hc, -⟩ := D.rEnt_at hok hi hl
  obtain ⟨-, -, he⟩ := D.rEnt_spec h2'
  have hb : (gt D.d (D.rNew i ap.t (b, κ, c2)) (b + c1)).um.testBit m' = true :=
    ((hle.trans (le_set _ _ _ _ (le_bumpR _ _ _))) (b + c1)).1 m'
      ((D.inv_st hok (D.nM + i) (by unfold n; omega)).um m' (by omega) hm' _ e1)
  have hbm : a.bm.testBit m' = true := by
    apply hfin.2.2.2
    rw [he]
    exact testBit_or_r (D.testBit_orAt _ _ SS.um hin hb)
  cases hx : (D.exMR.get i).testBit m'
  · right
    rw [hh] at hm
    unfold h3chk at hc
    have hne : Nat.beq (D.hitOf i a) 0 = false := by
      cases e : Nat.beq (D.hitOf i a) 0
      · rfl
      · rw [Nat.eq_of_beq_eq_true e] at hm; simp at hm
    simp only [tb_eq] at hc
    rw [hne, Bool.false_or, List.all_eq_true] at hc
    have := hc m' (List.mem_range.mpr hm')
    rw [testBit_clr, hbm, hx] at this
    simp only [Bool.not_false, Bool.and_true, Bool.not_true, Bool.false_or] at this
    rcases testBit_or_cases (testBit_of_subB this hm) with h | h
    · exact .inl h
    · rw [testBit_clr, testBit_pow] at h
      simp only [Bool.and_eq_true, Bool.not_eq_true', decide_eq_false_iff_not] at h
      exact .inr ⟨fun e => h.2 e.symm, h.1⟩
  · exact .inl rfl

end Design


/-! ## From the scan to the certificate's pair fields -/

section Glue
variable {V : Type} (P : Parent V)

/-- A call on a port whose code is flagged has the dummy argument `z`. -/
def ArgZ {M : Type} (fx : Nat → Bool) (code : M → Nat) (z : V) (a : M × V) : Prop :=
  fx (code a.1) = true → a.2 = z

/-- **A kind's tables are sound for its annotations.** A `share` port may follow itself only
    if flagged (`fx`: argument-less, every call carries `z`). -/
structure KindOK {M : Type} (A : Ann M) (code : M → Nat) (kc : Nat → KC) (fx : Nat → Bool) (z : V) : Prop where
  fin : ∀ a b : M × V, code a.1 ∉ (kc (code b.1)).cin → ArgZ fx code z a → ArgZ fx code z b → A.Follow a b
  fout : ∀ a b : M × V, code b.1 ∉ (kc (code a.1)).cout → ArgZ fx code z a → ArgZ fx code z b → A.Follow a b
  gin : ∀ a b : M × V, code a.1 ∉ (kc (code b.1)).gin → ArgZ fx code z a → ArgZ fx code z b → A.Group a b
  gout : ∀ a b : M × V, code b.1 ∉ (kc (code a.1)).gout → ArgZ fx code z a → ArgZ fx code z b → A.Group a b

/-- The backward half of `KindOK` (what a chain kind's table must give). -/
structure KindOut {M : Type} (A : Ann M) (code : M → Nat) (kc : Nat → KC) (fx : Nat → Bool) (z : V) : Prop where
  fout : ∀ a b : M × V, code b.1 ∉ (kc (code a.1)).cout → ArgZ fx code z a → ArgZ fx code z b → A.Follow a b
  gout : ∀ a b : M × V, code b.1 ∉ (kc (code a.1)).gout → ArgZ fx code z a → ArgZ fx code z b → A.Group a b

/-- A chain kind's table: every port code 0, whose `cout` and `gout` contain 0. -/
theorem kindOut_chain {M : Type} {A : Ann M} {code : M → Nat} {kc : Nat → KC} {fx : Nat → Bool} {z : V}
    (hc : ∀ p, code p = 0) (ho : 0 ∈ (kc 0).cout) (hg : 0 ∈ (kc 0).gout) : KindOut A code kc fx z :=
  ⟨fun a b h => absurd (by rw [hc a.1, hc b.1]; exact ho) h, fun a b h => absurd (by rw [hc a.1, hc b.1]; exact hg) h⟩

/-- `Follow` on ports, arguments aside. -/
def folP {M : Type} [DecidableEq M] (A : Ann M) (p q : M) : Bool :=
  (decide (p = q) || !A.prec q p) && !A.conf p q && !A.conf q p

def grpP {M : Type} [DecidableEq M] (A : Ann M) (p q : M) : Bool := folP A p q && !A.sbr p q && !A.sbr q p

/-- Two calls of one `share` port agree only if it is flagged. -/
def argP {M : Type} [DecidableEq M] (A : Ann M) (fx : Nat → Bool) (code : M → Nat) (p q : M) : Bool :=
  !decide (p = q) || !A.share p || fx (code p)

/-- The decided form of `KindOK`, over a complete list of the kind's ports. -/
def kindChk {M : Type} [DecidableEq M] (A : Ann M) (code : M → Nat) (kc : Nat → KC) (fx : Nat → Bool)
    (ports : List M) : Bool :=
  ports.all fun p => ports.all fun q =>
    ((kc (code q)).cin.contains (code p) || (folP A p q && argP A fx code p q)) &&
    ((kc (code p)).cout.contains (code q) || (folP A p q && argP A fx code p q)) &&
    ((kc (code q)).gin.contains (code p) || (grpP A p q && argP A fx code p q)) &&
    ((kc (code p)).gout.contains (code q) || (grpP A p q && argP A fx code p q))

theorem follow_of_folP {M : Type} [DecidableEq M] {A : Ann M} {fx : Nat → Bool} {code : M → Nat} {z : V}
    {a b : M × V} (h1 : folP A a.1 b.1 = true) (h2 : argP A fx code a.1 b.1 = true)
    (ha : ArgZ fx code z a) (hb : ArgZ fx code z b) : A.Follow a b := by
  simp only [folP, Bool.and_eq_true, Bool.or_eq_true, decide_eq_true_eq, Bool.not_eq_true'] at h1
  obtain ⟨⟨h11, h12⟩, h13⟩ := h1
  refine ⟨fun hne => ?_, h12, h13, fun he hs => ?_⟩
  · rcases h11 with h | h
    · exact absurd h hne
    · exact h
  · simp only [argP, Bool.or_eq_true, Bool.not_eq_true', decide_eq_false_iff_not] at h2
    rcases h2 with (h | h) | h
    · exact absurd he h
    · rw [hs] at h; cases h
    · have hb' : fx (code b.1) = true := by rw [← he]; exact h
      rw [ha h, hb hb']

theorem group_of_grpP {M : Type} [DecidableEq M] {A : Ann M} {fx : Nat → Bool} {code : M → Nat} {z : V}
    {a b : M × V} (h1 : grpP A a.1 b.1 = true) (h2 : argP A fx code a.1 b.1 = true)
    (ha : ArgZ fx code z a) (hb : ArgZ fx code z b) : A.Group a b := by
  simp only [grpP, Bool.and_eq_true, Bool.not_eq_true'] at h1
  exact ⟨follow_of_folP h1.1.1 h2 ha hb, h1.1.2, h1.2⟩

/-- A kind's tables, checked by `decide`, are sound. -/
theorem kindOK_of_chk {M : Type} [DecidableEq M] {A : Ann M} {code : M → Nat} {kc : Nat → KC} {fx : Nat → Bool}
    {ports : List M} (hp : ∀ p, p ∈ ports) (h : kindChk A code kc fx ports = true) (z : V) :
    KindOK A code kc fx z := by
  have hpq : ∀ p q, _ := fun p q => List.all_eq_true.mp (List.all_eq_true.mp h p (hp p)) q (hp q)
  refine ⟨fun a b hn ha hb => ?_, fun a b hn ha hb => ?_, fun a b hn ha hb => ?_, fun a b hn ha hb => ?_⟩
  all_goals
    have := hpq a.1 b.1
    simp only [Bool.and_eq_true, Bool.or_eq_true, List.contains_iff_mem] at this
    obtain ⟨⟨⟨h1, h2⟩, h3⟩, h4⟩ := this
  · rcases h1 with h | h
    · exact absurd h hn
    · exact follow_of_folP h.1 h.2 ha hb
  · rcases h2 with h | h
    · exact absurd h hn
    · exact follow_of_folP h.1 h.2 ha hb
  · rcases h3 with h | h
    · exact absurd h hn
    · exact group_of_grpP h.1 h.2 ha hb
  · rcases h4 with h | h
    · exact absurd h hn
    · exact group_of_grpP h.1 h.2 ha hb

/-- How the scan's numbers name the parent's methods, rules and child ports. -/
structure Glue (D : Design) where
  mi : P.X.M → Nat
  ruleAt : Nat → P.PR
  rk : P.PR → Nat
  base : P.I → Nat
  kind : P.I → Nat
  code : (j : P.I) → (P.CI j).M → Nat
  fx : Nat → Nat → Bool
  z : V
  /-- chain children (a total `SB` order on their ports, such as `Tasks n`): all their ports
      share one slot (code 0), and the order is checked by positions instead of the kind table -/
  ch : P.I → Bool := fun _ => false
  pos : (j : P.I) → (P.CI j).M → Nat := fun _ _ => 0
  /-- each step's chain calls lie in `[clo g, chi g)` (methods `g < nM`, then rules by rank) -/
  clo : Nat → Nat := fun _ => 0
  chi : Nat → Nat := fun _ => 0

namespace Glue
variable {P} {D : Design} (G : Glue P D)

/-- A child call's footprint entry. -/
def ent (c : Σ j, (P.CI j).M × V) : Ent := (G.base c.1, G.kind c.1, G.code c.1 c.2.1)

/-- The rules in rank order. -/
def rules : List P.PR := (List.range D.nR).map G.ruleAt

end Glue

/-- **What a module proves besides the scan**, each linear or per kind: numbering, kind tables,
    footprints and dummy arguments, exported tables, exception masks, escaped pairs. -/
structure GlueOK (D : Design) (G : Glue P D)
    (fire : ((j : P.I) → Outs (P.CI j).M V) → Wires P.X.M V → P.PR → Bool) : Prop where
  mi_lt : ∀ m, G.mi m < D.nM
  rk_lt : ∀ r, G.rk r < D.nR
  rk_at : ∀ r, G.ruleAt (G.rk r) = r
  at_rk : ∀ i, i < D.nR → G.rk (G.ruleAt i) = i
  kind : ∀ j, G.ch j = false → KindOK (P.CI j).ann (G.code j) (D.kc (G.kind j)) (G.fx (G.kind j)) G.z
  /-- chain children: the kind table covers only the backward direction; forward is by position -/
  kindC : ∀ j, G.ch j = true → KindOut (P.CI j).ann (G.code j) (D.kc (G.kind j)) (G.fx (G.kind j)) G.z
  chAnn : ∀ j, G.ch j = true → ∀ a b : (P.CI j).M × V, G.pos j a.1 < G.pos j b.1 → (P.CI j).ann.Group a b
  cmono : ∀ g1 g2, g1 < g2 → G.chi g1 ≤ G.clo g2
  cinM : ∀ oc w m, ∀ c ∈ (trM P oc w m).1, G.ch c.1 = true →
    G.clo (G.mi m) ≤ G.pos c.1 c.2.1 ∧ G.pos c.1 c.2.1 < G.chi (G.mi m)
  cinR : ∀ oc r, ∀ c ∈ (trR P oc r).1, G.ch c.1 = true →
    G.clo (D.nM + G.rk r) ≤ G.pos c.1 c.2.1 ∧ G.pos c.1 c.2.1 < G.chi (D.nM + G.rk r)
  cincM : ∀ oc w m j (a b : (P.CI j).M × V), G.ch j = true → [a, b] <+ proj j (trM P oc w m).1 →
    G.pos j a.1 < G.pos j b.1
  cincR : ∀ oc r j (a b : (P.CI j).M × V), G.ch j = true → [a, b] <+ proj j (trR P oc r).1 →
    G.pos j a.1 < G.pos j b.1
  chCode : ∀ j, G.ch j = true → ∀ p, G.code j p = 0
  fpM : ∀ oc w m, (trM P oc w m).1.map G.ent <+ D.fpM.get (G.mi m)
  fpR : ∀ oc r, (trR P oc r).1.map G.ent <+ D.fpR.get (G.rk r)
  zM : ∀ oc w m, ∀ c ∈ (trM P oc w m).1, G.fx (G.kind c.1) (G.code c.1 c.2.1) = true → c.2.2 = G.z
  zR : ∀ oc r, ∀ c ∈ (trR P oc r).1, G.fx (G.kind c.1) (G.code c.1 c.2.1) = true → c.2.2 = G.z
  nfI : ∀ m1 m2, (D.nfI.get (G.mi m2)).testBit (G.mi m1) = true → ∀ x y : V, ¬ P.X.ann.Follow (m1, x) (m2, y)
  nfO : ∀ m1 m2, (D.nfO.get (G.mi m1)).testBit (G.mi m2) = true → ∀ x y : V, ¬ P.X.ann.Follow (m1, x) (m2, y)
  ngI : ∀ m1 m2, (D.ngI.get (G.mi m2)).testBit (G.mi m1) = true → ∀ x y : V, ¬ P.X.ann.Group (m1, x) (m2, y)
  ngO : ∀ m1 m2, (D.ngO.get (G.mi m1)).testBit (G.mi m2) = true → ∀ x y : V, ¬ P.X.ann.Group (m1, x) (m2, y)
  h3ok : ∀ m m', (D.h3ok.get (G.mi m')).testBit (G.mi m) = true → m ≠ m' ∧
    ((P.X.ann.prec m m' = true ∧ P.X.ann.sbr m m' = true) ∨ P.X.ann.conf m m' = true ∨ P.X.ann.conf m' m = true)
  exM : ∀ oc w m1 m2, Env P oc w → (D.exM.get (G.mi m2)).testBit (G.mi m1) = true →
    Avail P oc w m1 → Avail P oc w m2 → G.mi m1 = G.mi m2
  exR : ∀ oc w r1 r2, Env P oc w → (D.exR.get (G.rk r2)).testBit (G.rk r1) = true →
    fire oc w r1 = true → fire oc w r2 = true → G.rk r1 = G.rk r2
  exMR : ∀ oc w m r, Env P oc w → (D.exMR.get (G.rk r)).testBit (G.mi m) = true →
    Avail P oc w m → fire oc w r = true → False
  esc : ∀ oc w r1 r2, Env P oc w → (D.escR.get (G.rk r2)).testBit (G.rk r1) = true →
    fire oc w r1 = true → fire oc w r2 = true → OkT P (trR P oc r1).1 (trR P oc r2).1

/-- The pair fields of `Cert`, with the rules, firing and hits they share. -/
structure PairFacts (rules : List P.PR) (fire : ((j : P.I) → Outs (P.CI j).M V) → Wires P.X.M V → P.PR → Bool) where
  hit : ((j : P.I) → Outs (P.CI j).M V) → Wires P.X.M V → P.X.M → P.PR → Bool
  rules_nodup : rules.Nodup
  fireIn : ∀ oc w r, fire oc w r = true → r ∈ rules
  grpM : ∀ oc w m, Env P oc w → Avail P oc w m → GrpT P (trM P oc w m).1
  grpR : ∀ oc w r, Env P oc w → fire oc w r = true → GrpT P (trR P oc r).1
  mm : ∀ oc w m1 m2, Env P oc w → Avail P oc w m1 → Avail P oc w m2 →
    P.X.ann.Follow (m1, w.arg m1) (m2, w.arg m2) → OkT P (trM P oc w m1).1 (trM P oc w m2).1
  mmg : ∀ oc w m1 m2, Env P oc w → Avail P oc w m1 → Avail P oc w m2 →
    P.X.ann.Group (m1, w.arg m1) (m2, w.arg m2) → TogT P (trM P oc w m1).1 (trM P oc w m2).1
  h1 : ∀ oc w m r, Env P oc w → Avail P oc w m → fire oc w r = true →
    ¬ OkT P (trR P oc r).1 (trM P oc w m).1 → hit oc w m r = true
  h2 : ∀ oc w m r1 r2, Env P oc w → Avail P oc w m → fire oc w r1 = true → fire oc w r2 = true →
    hit oc w m r1 = true → ¬ OkT P (trR P oc r2).1 (trR P oc r1).1 → hit oc w m r2 = true
  h3 : ∀ oc w m m' r, Env P oc w → Avail P oc w m → Avail P oc w m' → fire oc w r = true →
    hit oc w m r = true → ¬ OkT P (trM P oc w m').1 (trR P oc r).1 →
    m ≠ m' ∧ ((P.X.ann.prec m m' = true ∧ P.X.ann.sbr m m' = true) ∨
      P.X.ann.conf m m' = true ∨ P.X.ann.conf m' m = true)
  h4 : ∀ oc w r1 r2, Env P oc w → fire oc w r1 = true → fire oc w r2 = true → r1 ≠ r2 →
    ¬ OkT P (trR P oc r1).1 (trR P oc r2).1 → [r2, r1] <+ rules

theorem not_okT {t1 t2 : List (Σ j, (P.CI j).M × V)} (h : ¬ OkT P t1 t2) :
    ∃ j a b, a ∈ proj j t1 ∧ b ∈ proj j t2 ∧ ¬ (P.CI j).ann.Follow a b :=
  Classical.byContradiction fun hn => h fun j a ha b hb =>
    Classical.byContradiction fun hf => hn ⟨j, a, b, ha, hb, hf⟩

theorem not_togT {t1 t2 : List (Σ j, (P.CI j).M × V)} (h : ¬ TogT P t1 t2) :
    ∃ j a b, a ∈ proj j t1 ∧ b ∈ proj j t2 ∧ ¬ (P.CI j).ann.Group a b :=
  Classical.byContradiction fun hn => h fun j a ha b hb =>
    Classical.byContradiction fun hf => hn ⟨j, a, b, ha, hb, hf⟩

theorem proj_pair_sub {I : Type} [DecidableEq I] {CM : I → Type} (j : I) {a b : CM j × V} :
    ∀ (t : List (Σ j, CM j × V)), [a, b] <+ proj j t → [⟨j, a⟩, ⟨j, b⟩] <+ t
  | [], h => by simp [proj] at h
  | ⟨j', c⟩ :: t, h => by
    by_cases hj : j' = j
    · subst hj
      rw [proj_cons_self] at h
      rcases List.sublist_cons_iff.mp h with h' | ⟨r, hr, hs⟩
      · exact (proj_pair_sub j' t h').cons _
      · simp only [List.cons.injEq] at hr
        obtain ⟨rfl, rfl⟩ := hr
        exact List.cons_sublist_cons.mpr (List.singleton_sublist.mpr
          (mem_proj j' t b (List.singleton_sublist.mp hs)))
    · rw [proj_cons_ne _ _ hj] at h
      exact (proj_pair_sub j t h).cons _

section
variable {P} {D : Design} {G : Glue P D}
  {fire : ((j : P.I) → Outs (P.CI j).M V) → Wires P.X.M V → P.PR → Bool}

theorem ent_mem {t : List (Σ j, (P.CI j).M × V)} {F : List Ent} (hF : t.map G.ent <+ F) {j : P.I}
    {a : (P.CI j).M × V} (ha : a ∈ proj j t) : (G.base j, G.kind j, G.code j a.1) ∈ F :=
  hF.subset (List.mem_map_of_mem (f := G.ent) (mem_proj j t a ha))

theorem argZ_of {t : List (Σ j, (P.CI j).M × V)}
    (hz : ∀ c ∈ t, G.fx (G.kind c.1) (G.code c.1 c.2.1) = true → c.2.2 = G.z) {j : P.I}
    {a : (P.CI j).M × V} (ha : a ∈ proj j t) : ArgZ (G.fx (G.kind j)) (G.code j) G.z a :=
  hz ⟨j, a⟩ (mem_proj j t a ha)

theorem rank_lt (H : GlueOK P D G fire) {r1 r2 : P.PR} (h : [r1, r2] <+ G.rules) : G.rk r1 < G.rk r2 := by
  obtain ⟨x, y, hxy, rfl, rfl⟩ := sublist_map_pair h
  have hlt : x < y := by
    have := List.pairwise_lt_range.sublist hxy
    simpa using this
  have hy : y < D.nR := List.mem_range.mp (hxy.subset (by simp))
  rw [H.at_rk x (by omega), H.at_rk y hy]
  exact hlt

theorem range_pair : ∀ (n a b : Nat), a < b → b < n → [a, b] <+ List.range n
  | 0, _, _, _, h => absurd h (Nat.not_lt_zero _)
  | n + 1, a, b, hab, hb => by
    rw [List.range_succ]
    by_cases e : b < n
    · exact (range_pair n a b hab e).trans (List.sublist_append_left _ _)
    · have : b = n := by omega
      subst this
      exact List.Sublist.append (List.singleton_sublist.mpr (List.mem_range.mpr hab)) (List.Sublist.refl _)

theorem rules_sub (H : GlueOK P D G fire) {r1 r2 : P.PR} (h : G.rk r1 < G.rk r2) : [r1, r2] <+ G.rules := by
  have := (range_pair D.nR _ _ h (H.rk_lt r2)).map G.ruleAt
  simp only [List.map_cons, List.map_nil, H.rk_at] at this
  exact this

theorem rules_nodup' (H : GlueOK P D G fire) : G.rules.Nodup := by
  unfold Glue.rules
  rw [List.Nodup, List.pairwise_map]
  refine List.Pairwise.imp_of_mem ?_ List.pairwise_lt_range
  intro a b ha hb hab e
  have := congrArg G.rk e
  rw [H.at_rk a (List.mem_range.mp ha), H.at_rk b (List.mem_range.mp hb)] at this
  omega

theorem memc {t : List (Σ j, (P.CI j).M × V)} {j : P.I} {a : (P.CI j).M × V} (h : a ∈ proj j t) :
    (⟨j, a⟩ : Σ j, (P.CI j).M × V) ∈ t := mem_proj j t a h

theorem fout_of (H : GlueOK P D G fire) (j : P.I) {a b : (P.CI j).M × V}
    (hc : G.code j b.1 ∉ (D.kc (G.kind j) (G.code j a.1)).cout)
    (za : ArgZ (G.fx (G.kind j)) (G.code j) G.z a) (zb : ArgZ (G.fx (G.kind j)) (G.code j) G.z b) :
    (P.CI j).ann.Follow a b := by
  cases hj : G.ch j
  · exact (H.kind j hj).fout a b hc za zb
  · exact (H.kindC j hj).fout a b hc za zb

theorem gout_of (H : GlueOK P D G fire) (j : P.I) {a b : (P.CI j).M × V}
    (hc : G.code j b.1 ∉ (D.kc (G.kind j) (G.code j a.1)).gout)
    (za : ArgZ (G.fx (G.kind j)) (G.code j) G.z a) (zb : ArgZ (G.fx (G.kind j)) (G.code j) G.z b) :
    (P.CI j).ann.Group a b := by
  cases hj : G.ch j
  · exact (H.kind j hj).gout a b hc za zb
  · exact (H.kindC j hj).gout a b hc za zb

/-- Chain calls of an earlier step come before those of a later one. -/
theorem chain_lt (H : GlueOK P D G fire) {g1 g2 x y : Nat} (hg : g1 < g2)
    (hx : G.clo g1 ≤ x ∧ x < G.chi g1) (hy : G.clo g2 ≤ y ∧ y < G.chi g2) : x < y :=
  Nat.lt_of_lt_of_le hx.2 (Nat.le_trans (H.cmono g1 g2 hg) hy.1)

/-- `h4` from the scan. -/
theorem h4_of_scan (hok : D.Ok) (H : GlueOK P D G fire) :
    ∀ oc w r1 r2, Env P oc w → fire oc w r1 = true → fire oc w r2 = true → r1 ≠ r2 →
      ¬ OkT P (trR P oc r1).1 (trR P oc r2).1 → [r2, r1] <+ G.rules := by
  intro oc w r1 r2 hE f1 f2 hne hn
  have hrk : G.rk r1 ≠ G.rk r2 := fun e => hne (by rw [← H.rk_at r1, ← H.rk_at r2, e])
  rcases Nat.lt_or_gt_of_ne hrk with hlt | hlt
  · exfalso
    obtain ⟨j, a, b, ha, hb, hf⟩ := not_okT P hn
    cases hj : G.ch j
    · have hin : G.code j a.1 ∈ (D.kc (G.kind j) (G.code j b.1)).cin :=
        Classical.byContradiction fun hc =>
          hf ((H.kind j hj).fin a b hc (argZ_of (H.zR oc r1) ha) (argZ_of (H.zR oc r2) hb))
      rcases D.h4_static hok hlt (H.rk_lt r2) (ent_mem (H.fpR oc r1) ha) (ent_mem (H.fpR oc r2) hb) hin with h | h
      · exact hrk (H.exR oc w r1 r2 hE h f1 f2)
      · exact hn (H.esc oc w r1 r2 hE h f1 f2)
    · exact hf (H.chAnn j hj a b (chain_lt H (Nat.add_lt_add_left hlt D.nM)
        (H.cinR oc r1 ⟨j, a⟩ (memc ha) hj) (H.cinR oc r2 ⟨j, b⟩ (memc hb) hj))).1
  · exact rules_sub H hlt

theorem grpM_of_scan (hok : D.Ok) (H : GlueOK P D G fire) :
    ∀ oc w m, Env P oc w → Avail P oc w m → GrpT P (trM P oc w m).1 := by
  intro oc w m _ _ j a b hab
  cases hj : G.ch j
  · have hs := ((proj_pair_sub j _ hab).map G.ent).trans (H.fpM oc w m)
    have ha : a ∈ proj j (trM P oc w m).1 := hab.subset (by simp)
    have hb : b ∈ proj j (trM P oc w m).1 := hab.subset (by simp)
    exact (H.kind j hj).gin a b (D.grpM_static hok (H.mi_lt m) hs) (argZ_of (H.zM oc w m) ha)
      (argZ_of (H.zM oc w m) hb)
  · exact H.chAnn j hj a b (H.cincM oc w m j a b hj hab)

theorem grpR_of_scan (hok : D.Ok) (H : GlueOK P D G fire) :
    ∀ oc w r, Env P oc w → fire oc w r = true → GrpT P (trR P oc r).1 := by
  intro oc w r _ _ j a b hab
  cases hj : G.ch j
  · have hs := ((proj_pair_sub j _ hab).map G.ent).trans (H.fpR oc r)
    have ha : a ∈ proj j (trR P oc r).1 := hab.subset (by simp)
    have hb : b ∈ proj j (trR P oc r).1 := hab.subset (by simp)
    exact (H.kind j hj).gin a b (D.grpR_static hok (H.rk_lt r) hs) (argZ_of (H.zR oc r) ha)
      (argZ_of (H.zR oc r) hb)
  · exact H.chAnn j hj a b (H.cincR oc r j a b hj hab)

theorem mm_of_scan (hok : D.Ok) (H : GlueOK P D G fire) :
    ∀ oc w m1 m2, Env P oc w → Avail P oc w m1 → Avail P oc w m2 →
      P.X.ann.Follow (m1, w.arg m1) (m2, w.arg m2) → OkT P (trM P oc w m1).1 (trM P oc w m2).1 := by
  intro oc w m1 m2 hE a1 a2 hf
  apply Classical.byContradiction
  intro hn
  obtain ⟨j, a, b, ha, hb, hnf⟩ := not_okT P hn
  have za := argZ_of (H.zM oc w m1) ha
  have zb := argZ_of (H.zM oc w m2) hb
  have hout : G.code j b.1 ∈ (D.kc (G.kind j) (G.code j a.1)).cout :=
    Classical.byContradiction fun hc => hnf (fout_of H j hc za zb)
  cases hj : G.ch j
  · have hin : G.code j a.1 ∈ (D.kc (G.kind j) (G.code j b.1)).cin :=
      Classical.byContradiction fun hc => hnf ((H.kind j hj).fin a b hc za zb)
    rcases D.mm_static_gen hok KC.cin KC.cout D.nfI D.nfO (.inl ⟨rfl, rfl⟩) (.inl ⟨rfl, rfl⟩) (H.mi_lt m1)
        (H.mi_lt m2) (ent_mem (H.fpM oc w m1) ha) (ent_mem (H.fpM oc w m2) hb) hin hout with h | h | ⟨hne, h | h⟩
    · exact H.nfI m1 m2 h _ _ hf
    · exact H.nfO m1 m2 h _ _ hf
    · exact hne (H.exM oc w m1 m2 hE h a1 a2)
    · exact hne (H.exM oc w m2 m1 hE h a2 a1).symm
  · rcases Nat.lt_or_ge (G.mi m1) (G.mi m2) with hlt | hge
    · exact hnf (H.chAnn j hj a b (chain_lt H hlt (H.cinM oc w m1 ⟨j, a⟩ (memc ha) hj)
        (H.cinM oc w m2 ⟨j, b⟩ (memc hb) hj))).1
    · rcases D.mm_static_out hok KC.cout D.nfO (.inl ⟨rfl, rfl⟩) (H.mi_lt m1) (H.mi_lt m2) hge
          (fun _ => by rw [H.chCode j hj a.1, H.chCode j hj b.1])
          (ent_mem (H.fpM oc w m1) ha) (ent_mem (H.fpM oc w m2) hb) hout with h | ⟨hne, h⟩
      · exact H.nfO m1 m2 h _ _ hf
      · exact hne (H.exM oc w m2 m1 hE h a2 a1).symm

theorem mmg_of_scan (hok : D.Ok) (H : GlueOK P D G fire) :
    ∀ oc w m1 m2, Env P oc w → Avail P oc w m1 → Avail P oc w m2 →
      P.X.ann.Group (m1, w.arg m1) (m2, w.arg m2) → TogT P (trM P oc w m1).1 (trM P oc w m2).1 := by
  intro oc w m1 m2 hE a1 a2 hg
  apply Classical.byContradiction
  intro hn
  obtain ⟨j, a, b, ha, hb, hng⟩ := not_togT P hn
  have za := argZ_of (H.zM oc w m1) ha
  have zb := argZ_of (H.zM oc w m2) hb
  have hout : G.code j b.1 ∈ (D.kc (G.kind j) (G.code j a.1)).gout :=
    Classical.byContradiction fun hc => hng (gout_of H j hc za zb)
  cases hj : G.ch j
  · have hin : G.code j a.1 ∈ (D.kc (G.kind j) (G.code j b.1)).gin :=
      Classical.byContradiction fun hc => hng ((H.kind j hj).gin a b hc za zb)
    rcases D.mm_static_gen hok KC.gin KC.gout D.ngI D.ngO (.inr ⟨rfl, rfl⟩) (.inr ⟨rfl, rfl⟩) (H.mi_lt m1)
        (H.mi_lt m2) (ent_mem (H.fpM oc w m1) ha) (ent_mem (H.fpM oc w m2) hb) hin hout with h | h | ⟨hne, h | h⟩
    · exact H.ngI m1 m2 h _ _ hg
    · exact H.ngO m1 m2 h _ _ hg
    · exact hne (H.exM oc w m1 m2 hE h a1 a2)
    · exact hne (H.exM oc w m2 m1 hE h a2 a1).symm
  · rcases Nat.lt_or_ge (G.mi m1) (G.mi m2) with hlt | hge
    · exact hng (H.chAnn j hj a b (chain_lt H hlt (H.cinM oc w m1 ⟨j, a⟩ (memc ha) hj)
        (H.cinM oc w m2 ⟨j, b⟩ (memc hb) hj)))
    · rcases D.mm_static_out hok KC.gout D.ngO (.inr ⟨rfl, rfl⟩) (H.mi_lt m1) (H.mi_lt m2) hge
          (fun _ => by rw [H.chCode j hj a.1, H.chCode j hj b.1])
          (ent_mem (H.fpM oc w m1) ha) (ent_mem (H.fpM oc w m2) hb) hout with h | ⟨hne, h⟩
      · exact H.ngO m1 m2 h _ _ hg
      · exact hne (H.exM oc w m2 m1 hE h a2 a1).symm

theorem h1_of_scan (hok : D.Ok) (H : GlueOK P D G fire) :
    ∀ oc w m r, Env P oc w → Avail P oc w m → fire oc w r = true →
      ¬ OkT P (trR P oc r).1 (trM P oc w m).1 → (D.hitS (G.rk r)).testBit (G.mi m) = true := by
  intro oc w m r hE am fr hn
  obtain ⟨j, a, b, ha, hb, hnf⟩ := not_okT P hn
  have hout : G.code j b.1 ∈ (D.kc (G.kind j) (G.code j a.1)).cout :=
    Classical.byContradiction fun hc =>
      hnf (fout_of H j hc (argZ_of (H.zR oc r) ha) (argZ_of (H.zM oc w m) hb))
  rcases D.h1_static hok (H.mi_lt m) (H.rk_lt r) (ent_mem (H.fpR oc r) ha) (ent_mem (H.fpM oc w m) hb) hout with h | h
  · exact h
  · exact (H.exMR oc w m r hE h am fr).elim

theorem h2_of_scan (hok : D.Ok) (H : GlueOK P D G fire) :
    ∀ oc w m r1 r2, Env P oc w → Avail P oc w m → fire oc w r1 = true → fire oc w r2 = true →
      (D.hitS (G.rk r1)).testBit (G.mi m) = true → ¬ OkT P (trR P oc r2).1 (trR P oc r1).1 →
      (D.hitS (G.rk r2)).testBit (G.mi m) = true := by
  intro oc w m r1 r2 hE am f1 f2 hh hn
  by_cases e : r1 = r2
  · subst e; exact hh
  have hlt := rank_lt H (h4_of_scan hok H oc w r2 r1 hE f2 f1 (fun e' => e e'.symm) hn)
  obtain ⟨j, a, b, ha, hb, hnf⟩ := not_okT P hn
  have hout : G.code j b.1 ∈ (D.kc (G.kind j) (G.code j a.1)).cout :=
    Classical.byContradiction fun hc =>
      hnf (fout_of H j hc (argZ_of (H.zR oc r2) ha) (argZ_of (H.zR oc r1) hb))
  rcases D.h2_static hok hlt (H.rk_lt r2) (ent_mem (H.fpR oc r1) hb) (ent_mem (H.fpR oc r2) ha) hout hh with h | h
  · exact h
  · exact (H.exMR oc w m r2 hE h am f2).elim

theorem h3_of_scan (hok : D.Ok) (H : GlueOK P D G fire) :
    ∀ oc w m m' r, Env P oc w → Avail P oc w m → Avail P oc w m' → fire oc w r = true →
      (D.hitS (G.rk r)).testBit (G.mi m) = true → ¬ OkT P (trM P oc w m').1 (trR P oc r).1 →
      m ≠ m' ∧ ((P.X.ann.prec m m' = true ∧ P.X.ann.sbr m m' = true) ∨
        P.X.ann.conf m m' = true ∨ P.X.ann.conf m' m = true) := by
  intro oc w m m' r hE am am' fr hh hn
  obtain ⟨j, a, b, ha, hb, hnf⟩ := not_okT P hn
  cases hj : G.ch j
  · have hin : G.code j a.1 ∈ (D.kc (G.kind j) (G.code j b.1)).cin :=
      Classical.byContradiction fun hc =>
        hnf ((H.kind j hj).fin a b hc (argZ_of (H.zM oc w m') ha) (argZ_of (H.zR oc r) hb))
    rcases D.h3_static hok (H.mi_lt m') (H.rk_lt r) (ent_mem (H.fpM oc w m') ha) (ent_mem (H.fpR oc r) hb) hin hh
      with h | h | ⟨hne, h⟩
    · exact (H.exMR oc w m' r hE h am' fr).elim
    · exact H.h3ok m m' h
    · exact (hne (H.exM oc w m m' hE h am am')).elim
  · have hlt : G.mi m' < D.nM + G.rk r := Nat.lt_of_lt_of_le (H.mi_lt m') (Nat.le_add_right _ _)
    exact (hnf (H.chAnn j hj a b (chain_lt H hlt (H.cinM oc w m' ⟨j, a⟩ (memc ha) hj)
      (H.cinR oc r ⟨j, b⟩ (memc hb) hj))).1).elim

end

/-- **The pair fields from the scan.**  `hit m r` is the scan's hit set of
    `r`'s rank at `m`'s index. -/
def pairFacts_of_scan {D : Design} (hok : D.Ok) (G : Glue P D)
    (fire : ((j : P.I) → Outs (P.CI j).M V) → Wires P.X.M V → P.PR → Bool) (H : GlueOK P D G fire) :
    PairFacts P G.rules fire where
  hit := fun _ _ m r => (D.hitS (G.rk r)).testBit (G.mi m)
  rules_nodup := rules_nodup' H
  fireIn := fun _ _ r _ => by
    unfold Glue.rules
    rw [← H.rk_at r]
    exact List.mem_map_of_mem (List.mem_range.mpr (H.rk_lt r))
  grpM := grpM_of_scan hok H
  grpR := grpR_of_scan hok H
  mm := mm_of_scan hok H
  mmg := mmg_of_scan hok H
  h1 := h1_of_scan hok H
  h2 := h2_of_scan hok H
  h3 := h3_of_scan hok H
  h4 := h4_of_scan hok H

end Glue


/-! ## The certificate and the obligation from the scan -/

section CertOfScan
variable {V : Type} (P : Parent V)

/-- A `Cert` from the pair fields and the remaining (per-step, per-port) fields. -/
def PairFacts.toCert {rules : List P.PR} {fire : ((j : P.I) → Outs (P.CI j).M V) → Wires P.X.M V → P.PR → Bool}
    (pf : PairFacts P rules fire)
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
    (arg : ∀ oc w j cm, Env P oc w → ∀ s ∈ users j cm, Av P fire oc w s →
      ∀ x, (cm, x) ∈ proj j (trS P oc w s) → ((P.comb oc w).1 j).arg cm = x)
    (alw : ∀ oc w j cm, Env P oc w → (P.CI j).ann.alwaysEn cm = true →
      ∃ s ∈ users j cm, On P fire oc w s ∧ Calls P (trS P oc w s) j cm)
    (onehot : ∀ oc w, EnvC P oc w → P.onehot oc w) : Cert P where
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
  arg := arg
  alw := alw
  onehot := onehot

end CertOfScan

/-! ## Exception groups, escapes, muxes -/

section Groups

/-- At most one of a chain holds: two distinct members are never both on. -/
theorem amoChain_true {α : Type} (f : α → Bool) : ∀ (l : List α), amoChain true (l.map f) → ∀ y ∈ l, f y = false
  | [], _, _, hy => by cases hy
  | x :: l, h, y, hy => by
    simp only [List.map_cons, amoChain, Bool.true_and, Bool.true_or] at h
    rcases List.mem_cons.mp hy with rfl | hy'
    · exact h.1
    · exact amoChain_true f l h.2 y hy'

theorem amoChain_excl {α : Type} (f : α → Bool) : ∀ (l : List α) (acc : Bool), amoChain acc (l.map f) →
    ∀ x ∈ l, ∀ y ∈ l, x ≠ y → f x = true → f y = true → False
  | [], _, _, x, hx, _, _, _, _, _ => by cases hx
  | z :: l, acc, h, x, hx, y, hy, hne, fx, fy => by
    simp only [List.map_cons, amoChain] at h
    have later : f z = true → ∀ w ∈ l, f w = false := fun hz =>
      amoChain_true f l (by rw [hz, Bool.or_true] at h; exact h.2)
    rcases List.mem_cons.mp hx with rfl | hx' <;> rcases List.mem_cons.mp hy with rfl | hy'
    · exact hne rfl
    · rw [later fx y hy'] at fy; cases fy
    · rw [later fy x hx'] at fx; cases fx
    · exact amoChain_excl f l _ h.2 x hx' y hy' hne fx fy

/-- A chain from a pairwise exclusion (for `Env`'s exported-C part: methods
    pairwise C are never both called). -/
theorem amoChain_of_pairwise {α : Type} (f : α → Bool) (R : α → α → Prop)
    (hR : ∀ x y, R x y → f x = true → f y = true → False) :
    ∀ (l : List α) (acc : Bool), l.Pairwise R → (acc = true → ∀ x ∈ l, f x = false) → amoChain acc (l.map f)
  | [], _, _, _ => trivial
  | x :: l, acc, hp, hacc => by
    simp only [List.map_cons, amoChain]
    have hp' := List.pairwise_cons.mp hp
    refine ⟨?_, amoChain_of_pairwise f R hR l _ hp'.2 (fun h y hy => ?_)⟩
    · cases acc
      · rfl
      · rw [hacc rfl x (List.mem_cons_self ..)]; rfl
    · cases hfy : f y
      · rfl
      · exfalso
        rcases Bool.or_eq_true _ _ ▸ h with h | h
        · rw [hacc h y (List.mem_cons_of_mem _ hy)] at hfy; cases hfy
        · exact hR x y (hp'.1 y hy) h hfy

/-- Methods pairwise exported C are never two called: a chain for `skel`. -/
theorem calledChain {V : Type} (P : Parent V) {oc : (j : P.I) → Outs (P.CI j).M V} {w : Wires P.X.M V}
    (hE : Env P oc w) (ms : List P.X.M) (hp : ms.Pairwise (fun m m' => m ≠ m' ∧ P.X.ann.conf m m' = true)) :
    amoChain false (ms.map (called P w)) :=
  amoChain_of_pairwise _ _ (fun m m' h h1 h2 => by
    have := hE.2.2.2 m m' h.1 h1 h2; rw [h.2] at this; cases this) ms false hp (fun h => by cases h)

/-- A bit set of numbers. -/
def bits (l : List Nat) : Nat := l.foldr (fun a acc => 2 ^ a ||| acc) 0

theorem mem_of_testBit_bits : ∀ {l : List Nat} {a : Nat}, (bits l).testBit a = true → a ∈ l
  | [], a, h => by simp [bits] at h
  | x :: l, a, h => by
    simp only [bits, List.foldr_cons] at h
    rcases testBit_or_cases h with h | h
    · rw [Nat.testBit_two_pow] at h; simp at h; subst h; exact List.mem_cons_self ..
    · exact List.mem_cons_of_mem _ (mem_of_testBit_bits h)

/-- The OR of the groups' masks. -/
def orMask (gs : List Nat) (gmask : Nat → Nat) : Nat := gs.foldr (fun g acc => gmask g ||| acc) 0

theorem testBit_orMask : ∀ {gs : List Nat} {gmask : Nat → Nat} {a : Nat}, (orMask gs gmask).testBit a = true →
    ∃ g ∈ gs, (gmask g).testBit a = true
  | [], _, a, h => by simp [orMask] at h
  | g :: gs, gmask, a, h => by
    simp only [orMask, List.foldr_cons] at h
    rcases testBit_or_cases h with h | h
    · exact ⟨g, List.mem_cons_self .., h⟩
    · obtain ⟨g', hg', h'⟩ := testBit_orMask h
      exact ⟨g', List.mem_cons_of_mem _ hg', h'⟩

/-- **An exclusion mask from exception groups** (each with an at-most-one chain from `skel`):
    `hm`, `hg`, `hT` are decided checks, linear in the groups' sizes. -/
theorem excl_of_groups (act : Nat → Bool) (mem gid : Nat → List Nat) (gmask T : Nat → Nat)
    (hch : ∀ g, amoChain false ((mem g).map act))
    (hm : ∀ g, gmask g = bits (mem g))
    (hg : ∀ x g, g ∈ gid x → x ∈ mem g)
    (hT : ∀ y, Sub (T y) (orMask (gid y) gmask)) :
    ∀ x y, (T y).testBit x = true → act x = true → act y = true → x = y := by
  intro x y hb hx hy
  obtain ⟨g, hgy, hgx⟩ := testBit_orMask (hT y x hb)
  rw [hm] at hgx
  apply Classical.byContradiction
  intro hne
  exact amoChain_excl act (mem g) false (hch g) x (mem_of_testBit_bits hgx) y (hg y g hgy) hne hx hy

/-- Rules' exception groups as `GlueOK.exR` (codes are rank positions). -/
theorem exR_of_groups {V : Type} (P : Parent V) {D : Design} (G : Glue P D)
    (fire : ((j : P.I) → Outs (P.CI j).M V) → Wires P.X.M V → P.PR → Bool)
    (rk_at : ∀ r, G.ruleAt (G.rk r) = r) (mem gid : Nat → List Nat) (gmask : Nat → Nat)
    (hch : ∀ oc w, Env P oc w → ∀ g, amoChain false ((mem g).map (fun i => fire oc w (G.ruleAt i))))
    (hm : ∀ g, gmask g = bits (mem g)) (hg : ∀ x g, g ∈ gid x → x ∈ mem g)
    (hT : ∀ y, Sub (D.exR.get y) (orMask (gid y) gmask)) :
    ∀ oc w r1 r2, Env P oc w → (D.exR.get (G.rk r2)).testBit (G.rk r1) = true →
      fire oc w r1 = true → fire oc w r2 = true → G.rk r1 = G.rk r2 := by
  intro oc w r1 r2 hE hb f1 f2
  exact excl_of_groups (fun i => fire oc w (G.ruleAt i)) mem gid gmask (fun y => D.exR.get y) (hch oc w hE) hm hg hT
    _ _ hb (by simp only [rk_at]; exact f1) (by simp only [rk_at]; exact f2)

/-- **Escaped pairs**: a list of rank-position pairs, each with its own
    dynamic proof, as a mask. -/
theorem esc_of_list {Q : Nat → Nat → Prop} (L : List (Nat × Nat)) (hL : ∀ p ∈ L, Q p.1 p.2) (T : Nat → Nat)
    (hT : ∀ i, Sub (T i) (bits ((L.filter (fun p => p.2 == i)).map Prod.fst))) :
    ∀ i' i, (T i).testBit i' = true → Q i' i := by
  intro i' i hb
  obtain ⟨p, hp, e⟩ := List.mem_map.mp (mem_of_testBit_bits (hT i i' hb))
  have hp' := List.mem_filter.mp hp
  have e2 : p.2 = i := beq_iff_eq.mp hp'.2
  have := hL p hp'.1
  rw [e, e2] at this
  exact this

/-- `GlueOK.esc` from escaped pairs. -/
theorem esc_of_pairs {V : Type} (P : Parent V) {D : Design} (G : Glue P D)
    (fire : ((j : P.I) → Outs (P.CI j).M V) → Wires P.X.M V → P.PR → Bool)
    (rk_at : ∀ r, G.ruleAt (G.rk r) = r) (L : List (Nat × Nat))
    (hL : ∀ p ∈ L, ∀ oc w, Env P oc w → fire oc w (G.ruleAt p.1) = true → fire oc w (G.ruleAt p.2) = true →
      OkT P (trR P oc (G.ruleAt p.1)).1 (trR P oc (G.ruleAt p.2)).1)
    (hT : ∀ i, Sub (D.escR.get i) (bits ((L.filter (fun p => p.2 == i)).map Prod.fst))) :
    ∀ oc w r1 r2, Env P oc w → (D.escR.get (G.rk r2)).testBit (G.rk r1) = true →
      fire oc w r1 = true → fire oc w r2 = true → OkT P (trR P oc r1).1 (trR P oc r2).1 := by
  intro oc w r1 r2 hE hb f1 f2
  have := esc_of_list (Q := fun i1 i2 => ∀ oc w, Env P oc w → fire oc w (G.ruleAt i1) = true →
    fire oc w (G.ruleAt i2) = true → OkT P (trR P oc (G.ruleAt i1)).1 (trR P oc (G.ruleAt i2)).1) L hL _ hT _ _ hb
  have h := this oc w hE (by rw [rk_at]; exact f1) (by rw [rk_at]; exact f2)
  rw [rk_at, rk_at] at h
  exact h

/-- `subB`, as `Sub`. -/
theorem sub_of_subB {a b : Nat} (h : subB a b = true) : Sub a b := fun _ hi => testBit_of_subB h hi

/-- A priority mux: the first selected value, else `d`.  A parallel (AND-OR)
    mux is read this way under its one-hot condition. -/
def pmux {V : Type} : List (Bool × V) → V → V
  | [], d => d
  | e :: l, d => bif e.1 then e.2 else pmux l d

/-- **Selection under a chain**: with at most one selector on, the mux is any selected value
    (user `k`'s `arg` fact: one application, entry by `l[k]? = some _`). -/
theorem pmux_of_amo {V : Type} : ∀ (l : List (Bool × V)) (acc : Bool) (d : V), amoChain acc (l.map Prod.fst) →
    ∀ e ∈ l, e.1 = true → pmux l d = e.2
  | [], _, _, _, _, he, _ => by cases he
  | e' :: l, acc, d, h, e, he, hs => by
    simp only [List.map_cons, amoChain] at h
    rcases List.mem_cons.mp he with rfl | he'
    · simp [pmux, hs]
    · cases h1 : e'.1
      · simp only [pmux, h1, Bool.cond_false]
        exact pmux_of_amo l _ d h.2 e he' hs
      · rw [h1, Bool.or_true] at h
        rw [amoChain_true Prod.fst l h.2 e he'] at hs; cases hs

theorem pmux_get {V : Type} {l : List (Bool × V)} {k : Nat} {v : V} (d : V) (h : amoChain false (l.map Prod.fst))
    (hk : l[k]? = some (true, v)) : pmux l d = v :=
  pmux_of_amo l false d h _ (List.mem_of_getElem? hk) rfl

/-- **Selection when the selected values agree** (a `share` port's users). -/
theorem pmux_of_agree {V : Type} (v d : V) : ∀ (l : List (Bool × V)), (∀ e ∈ l, e.1 = true → e.2 = v) →
    (∃ e ∈ l, e.1 = true) → pmux l d = v
  | [], _, ⟨_, he, _⟩ => by cases he
  | e' :: l, h, ⟨e, he, hs⟩ => by
    cases h1 : e'.1
    · simp only [pmux, h1, Bool.cond_false]
      rcases List.mem_cons.mp he with rfl | he'
      · rw [h1] at hs; cases hs
      · exact pmux_of_agree v d l (fun e'' h'' => h e'' (List.mem_cons_of_mem _ h'')) ⟨e, he', hs⟩
    · simp only [pmux, h1, Bool.cond_true]
      exact h e' (List.mem_cons_self ..) h1

end Groups

/-! ## Site lists give the footprint and dummy-argument facts -/

section SitesGlue
variable {V : Type} {P : Parent V} {D : Design} (G : Glue P D)

/-- A site's footprint entry. -/
def Glue.entS (s : Site P.I (fun j => (P.CI j).M) V) : Ent := (G.base s.j, G.kind s.j, G.code s.j s.m)

/-- A site-list step's trace lies in the entries of its sites, in order. -/
theorem fp_of_sites (ρ : Nat → V) (ss : List (Site P.I (fun j => (P.CI j).M) V)) :
    (callsOf ρ ss).map G.ent <+ ss.map G.entS := by
  have h := (callsOf_ports ρ ss).map (fun p : (Σ j, (P.CI j).M) => (G.base p.1, G.kind p.1, G.code p.1 p.2))
  simp only [List.map_map] at h
  exact h

/-- Sites on flagged ports carry the dummy argument. -/
theorem z_of_sites (ρ : Nat → V) : ∀ (ss : List (Site P.I (fun j => (P.CI j).M) V)),
    (∀ s ∈ ss, G.fx (G.kind s.j) (G.code s.j s.m) = true → ∀ ρ', s.arg ρ' = G.z) →
    ∀ c ∈ callsOf ρ ss, G.fx (G.kind c.1) (G.code c.1 c.2.1) = true → c.2.2 = G.z
  | [], _, c, hc, _ => by simp [callsOf] at hc
  | s :: ss, h, c, hc, hf => by
    simp only [callsOf, List.mem_append] at hc
    rcases hc with hc | hc
    · split at hc
      · simp only [List.mem_singleton] at hc
        subst hc
        exact h s (List.mem_cons_self ..) hf ρ
      · cases hc
    · exact z_of_sites ρ ss (fun s' hs' => h s' (List.mem_cons_of_mem _ hs')) c hc hf

/-- Adjacent `<` on a list of positions, decided linearly. -/
def incB : List Nat → Bool
  | [] => true
  | [_] => true
  | a :: b :: l => Nat.blt a b && incB (b :: l)

theorem pairwise_of_incB : ∀ (l : List Nat), incB l = true → l.Pairwise (· < ·)
  | [], _ => List.Pairwise.nil
  | [_], _ => List.pairwise_singleton _ _
  | a :: b :: l, h => by
    simp only [incB, Bool.and_eq_true] at h
    have ih := pairwise_of_incB (b :: l) h.2
    have hab : a < b := Nat.blt_eq.mp h.1
    refine List.Pairwise.cons (fun x hx => ?_) ih
    rcases List.mem_cons.mp hx with e | hx
    · rw [e]; exact hab
    · exact Nat.lt_trans hab (List.rel_of_pairwise_cons ih hx)

/-- A site list's chain calls lie in `[lo, hi)` when its chain sites do. -/
theorem chain_in_of_sites (ρ : Nat → V) (ss : List (Site P.I (fun j => (P.CI j).M) V)) (lo hi : Nat)
    (h : ∀ p ∈ ss.map (fun s => (⟨s.j, s.m⟩ : Σ j, (P.CI j).M)), G.ch p.1 = true →
      lo ≤ G.pos p.1 p.2 ∧ G.pos p.1 p.2 < hi) :
    ∀ c ∈ callsOf ρ ss, G.ch c.1 = true → lo ≤ G.pos c.1 c.2.1 ∧ G.pos c.1 c.2.1 < hi := by
  intro c hc
  exact h ⟨c.1, c.2.1⟩ ((callsOf_ports ρ ss).subset (List.mem_map_of_mem (f := fun c => (⟨c.1, c.2.1⟩ : Σ j, (P.CI j).M)) hc))

/-- A site list's chain calls on one child come in increasing positions when its chain sites do. -/
theorem chain_inc_of_sites (ρ : Nat → V) (ss : List (Site P.I (fun j => (P.CI j).M) V))
    (h : incB (((ss.map (fun s => (⟨s.j, s.m⟩ : Σ j, (P.CI j).M))).filter (fun p => G.ch p.1)).map
      (fun p => G.pos p.1 p.2)) = true) :
    ∀ j (a b : (P.CI j).M × V), G.ch j = true → [a, b] <+ proj j (callsOf ρ ss) → G.pos j a.1 < G.pos j b.1 := by
  intro j a b hj hab
  have h1 := ((proj_pair_sub j _ hab).map (fun c : (Σ j, (P.CI j).M × V) => (⟨c.1, c.2.1⟩ : Σ j, (P.CI j).M))).trans
    (callsOf_ports ρ ss)
  have h2 := (h1.filter (fun p => G.ch p.1)).map (fun p => G.pos p.1 p.2)
  simp only [List.map_cons, List.map_nil, List.filter_cons, hj, if_true, List.filter_nil] at h2
  have := (pairwise_of_incB _ h).sublist h2
  simpa using this

/-- The decided per-step check of a numbered site list's chain sites. -/
theorem chain_keys (key : Nat → Σ j, (P.CI j).M) (n lo hi : Nat)
    (h : (incB ((((List.range' 0 n).map key).filter (fun p => G.ch p.1)).map (fun p => G.pos p.1 p.2)) &&
      ((((List.range' 0 n).map key).filter (fun p => G.ch p.1)).map (fun p => G.pos p.1 p.2)).all
        (fun x => Nat.ble lo x && Nat.blt x hi)) = true) :
    (∀ p ∈ (List.range' 0 n).map key, G.ch p.1 = true → lo ≤ G.pos p.1 p.2 ∧ G.pos p.1 p.2 < hi) ∧
    incB ((((List.range' 0 n).map key).filter (fun p => G.ch p.1)).map (fun p => G.pos p.1 p.2)) = true := by
  rw [Bool.and_eq_true] at h
  refine ⟨fun p hp hc => ?_, h.1⟩
  have hm : G.pos p.1 p.2 ∈ (((List.range' 0 n).map key).filter (fun p => G.ch p.1)).map (fun p => G.pos p.1 p.2) :=
    List.mem_map_of_mem (List.mem_filter.mpr ⟨hp, hc⟩)
  have := List.all_eq_true.mp h.2 _ hm
  simp only [Bool.and_eq_true] at this
  exact ⟨Nat.le_of_ble_eq_true this.1, Nat.blt_eq.mp this.2⟩

/-- Monotone from successive steps: a table up to `N`, constant after. -/
theorem mono_of_succ (f : Nat → Nat) (h : ∀ g, f g ≤ f (g + 1)) : ∀ a b, a ≤ b → f a ≤ f b := by
  intro a b hab
  induction b with
  | zero => rw [Nat.le_zero.mp hab]; exact Nat.le_refl _
  | succ b ih =>
    rcases Nat.lt_or_eq_of_le hab with h' | h'
    · exact Nat.le_trans (ih (Nat.le_of_lt_succ h')) (h b)
    · rw [h']; exact Nat.le_refl _

/-- Step starts from a table: monotone, decided per step. -/
theorem stF_mono (T : Tab Nat) (N M : Nat)
    (h : Emit.Tab.allB (fun g x => Nat.ble x M && Nat.ble x (bif Nat.blt (g + 1) N then T.get (g + 1) else M)) T 0 N = true) :
    ∀ g, (bif Nat.blt g N then T.get g else M) ≤ (bif Nat.blt (g + 1) N then T.get (g + 1) else M) := by
  intro g
  by_cases hg : g < N
  · have h1 := Emit.Tab.allB_spec _ T 0 N h g (Nat.zero_le _) hg
    simp only [Bool.and_eq_true] at h1
    have e : Nat.blt g N = true := Nat.blt_eq.mpr hg
    rw [e]
    exact Nat.le_of_ble_eq_true h1.2
  · have e1 : Nat.blt g N = false := by cases e : Nat.blt g N; rfl; exact absurd (Nat.blt_eq.mp e) hg
    have e2 : Nat.blt (g + 1) N = false := by
      cases e : Nat.blt (g + 1) N; rfl; exact absurd (Nat.lt_of_succ_lt (Nat.blt_eq.mp e)) hg
    rw [e1, e2]; exact Nat.le_refl _

/-- Statement order is the `Tasks` annotation's order. -/
theorem taskAnn_group {n : Nat} {a b : Fin n × V} (h : a.1.val < b.1.val) : (taskAnn n).Group a b := by
  refine ⟨⟨fun _ => ?_, ?_, ?_, fun _ hs => by simp [taskAnn] at hs⟩, rfl, rfl⟩
  · simp only [taskAnn, decide_eq_false_iff_not]; exact fun h' => Nat.lt_asymm h h'
  · simp only [taskAnn, decide_eq_false_iff_not]; exact fun e => by rw [e] at h; exact Nat.lt_irrefl _ h
  · simp only [taskAnn, decide_eq_false_iff_not]; exact fun e => by rw [e] at h; exact Nat.lt_irrefl _ h

/-- **A dynamic `OkT`, for an escaped pair**: static `Follow` except on bad port pairs, which
    the two steps never both call (from site conditions under `fire`, `Env`). -/
theorem okT_except {V : Type} (P : Parent V) (okP : (j : P.I) → (P.CI j).M → (P.CI j).M → Bool)
    (hok : ∀ j (a b : (P.CI j).M × V), okP j a.1 b.1 = true → (P.CI j).ann.Follow a b)
    (t1 t2 : List (Σ j, (P.CI j).M × V)) (F1 F2 : List (Σ j, (P.CI j).M))
    (h1 : ∀ c ∈ t1, (⟨c.1, c.2.1⟩ : Σ j, (P.CI j).M) ∈ F1) (h2 : ∀ c ∈ t2, (⟨c.1, c.2.1⟩ : Σ j, (P.CI j).M) ∈ F2)
    (hbad : ∀ j a b, (⟨j, a⟩ : Σ j, (P.CI j).M) ∈ F1 → (⟨j, b⟩ : Σ j, (P.CI j).M) ∈ F2 → okP j a b = false →
      Calls P t1 j a → Calls P t2 j b → False) :
    OkT P t1 t2 := by
  intro j a ha b hb
  cases e : okP j a.1 b.1
  · exact (hbad j a.1 b.1 (h1 _ (mem_proj j t1 a ha)) (h2 _ (mem_proj j t2 b hb)) e ⟨a.2, ha⟩ ⟨b.2, hb⟩).elim
  · exact hok j a b e

end SitesGlue

/-! ## Glue for generated modules: enables over users, muxes, exclusions -/

section GenGlue
variable {V : Type}

/-- The OR of a list: a port's enable over its users. -/
def orL : List Bool → Bool
  | [] => false
  | b :: l => b || orL l

theorem orL_true : ∀ {l : List Bool}, orL l = true ↔ ∃ b ∈ l, b = true
  | [] => by simp [orL]
  | b :: l => by
    simp only [orL, Bool.or_eq_true, List.mem_cons, exists_eq_or_imp]
    rw [orL_true]

/-- **An enable over a port's users**, each of whose traces calls the port
    (unconditional calls): one `Calls` fact per user, no list scan. -/
theorem en_list (P : Parent V) (fire : ((j : P.I) → Outs (P.CI j).M V) → Wires P.X.M V → P.PR → Bool)
    (oc : (j : P.I) → Outs (P.CI j).M V) (w : Wires P.X.M V) (j : P.I) (cm : (P.CI j).M)
    (on : Step P → Bool) (hOn : ∀ s, On P fire oc w s ↔ on s = true) :
    ∀ (us : List (Step P)), Emit.AllL (fun s => Calls P (trS P oc w s) j cm) us →
      (orL (us.map on) = true ↔ ∃ s ∈ us, On P fire oc w s ∧ Calls P (trS P oc w s) j cm) := by
  intro us hc
  rw [orL_true]
  constructor
  · rintro ⟨b, hb, e⟩
    obtain ⟨s, hs, rfl⟩ := List.mem_map.mp hb
    exact ⟨s, hs, (hOn s).mpr e, Emit.AllL.mem hc s hs⟩
  · rintro ⟨s, hs, ho, -⟩
    exact ⟨on s, List.mem_map_of_mem hs, (hOn s).mp ho⟩

/-- The argument facts of a port, one per user. -/
theorem arg_users (P : Parent V) (fire : ((j : P.I) → Outs (P.CI j).M V) → Wires P.X.M V → P.PR → Bool)
    (oc : (j : P.I) → Outs (P.CI j).M V) (w : Wires P.X.M V) (j : P.I) (cm : (P.CI j).M) (a : V)
    (us : List (Step P))
    (h : Emit.AllL (fun s => Av P fire oc w s → ∀ x, (cm, x) ∈ proj j (trS P oc w s) → a = x) us) :
    ∀ s ∈ us, Av P fire oc w s → ∀ x, (cm, x) ∈ proj j (trS P oc w s) → a = x :=
  fun s hs => Emit.AllL.mem h s hs

/-- **Selection of a mux entry** whose selector is on, under a chain. -/
theorem arg_mux {l : List (Bool × V)} {p : Nat} {b : Bool} {v : V} (d : V)
    (h : amoChain false (l.map Prod.fst)) (hp : l[p]? = some (b, v)) (hb : b = true) : pmux l d = v := by
  subst hb; exact pmux_get d h hp

/-- A run-time check `a && (b₁ || …)` that never holds excludes `a` with each `bᵢ`. -/
theorem me_excl {a b : Bool} {l : List Bool} {p : Nat} (hc : (a && orL l) = false) (ha : a = true)
    (hl : l[p]? = some b) (hb : b = true) : False := by
  have : orL l = true := orL_true.mpr ⟨b, List.mem_of_getElem? hl, hb⟩
  rw [ha, this] at hc; cases hc

theorem disj_excl {a b : Bool} (h : (a && b) = false) (ha : a = true) (hb : b = true) : False := by
  rw [ha, hb] at h; cases h

/-- Urgency blocking: `a && !b` and `b` exclude each other. -/
theorem blk_excl {a b : Bool} (h : (a && !b) = true) (hb : b = true) : False := by
  rw [hb] at h; simp at h

/-- Pairwise facts over a list, as nested conjunctions (linear terms). -/
def PairsL {α : Type} (R : α → α → Prop) : List α → Prop
  | [] => True
  | x :: l => Emit.AllL (R x) l ∧ PairsL R l

theorem amoChain_of_pairsL {α : Type} (f : α → Bool) (R : α → α → Prop)
    (hR : ∀ x y, R x y → f x = true → f y = true → False) :
    ∀ (l : List α), PairsL R l → amoChain false (l.map f) := by
  intro l hl
  have hp : l.Pairwise R := by
    induction l with
    | nil => exact List.Pairwise.nil
    | cons x l ih => exact List.Pairwise.cons (Emit.AllL.mem hl.1) (ih hl.2)
  exact amoChain_of_pairwise f R hR l false hp (fun h => by cases h)

/-- Each member excludes every later one, a member's facts as one `∀`. -/
def PairsC {α : Type} (R : α → α → Prop) : List α → Prop
  | [] => True
  | x :: l => (∀ y ∈ l, R x y) ∧ PairsC R l

theorem amoChain_of_pairsC {α : Type} (f : α → Bool) (R : α → α → Prop)
    (hR : ∀ x y, R x y → f x = true → f y = true → False) :
    ∀ (l : List α), PairsC R l → amoChain false (l.map f) := by
  intro l hl
  have hp : l.Pairwise R := by
    induction l with
    | nil => exact List.Pairwise.nil
    | cons x l ih => exact List.Pairwise.cons hl.1 (ih hl.2)
  exact amoChain_of_pairwise f R hR l false hp (fun h => by cases h)

/-- A run-time check `a && (b₁ || …)` that never holds excludes `a` with every `bᵢ`. -/
theorem forall_of_check {f : Nat → Bool} {a : Bool} {lst : List Nat} (hc : (a && orL (lst.map f)) = false) :
    ∀ y ∈ lst, a = true → f y = true → False := by
  intro y hy ha hfy
  have : orL (lst.map f) = true := orL_true.mpr ⟨f y, List.mem_map_of_mem hy, hfy⟩
  rw [ha, this] at hc; cases hc

/-- A member's later members: those one check covers, and the rest one by one. -/
theorem forall_split {R : Nat → Prop} {T cov unc : List Nat}
    (h : T.all (fun y => cov.contains y || unc.contains y) = true) (h1 : ∀ y ∈ cov, R y)
    (h2 : Emit.AllL R unc) : ∀ y ∈ T, R y := by
  intro y hy
  have := List.all_eq_true.mp h y hy
  simp only [Bool.or_eq_true, List.contains_iff_mem] at this
  rcases this with h' | h'
  · exact h1 y h'
  · exact Emit.AllL.mem h2 y h'

/-- A conditional call's enable: `WILL_FIRE` and the condition, whose value at the step's
    environment is the hardware's. -/
theorem en_tk {a b c : Bool} (h : c = b) : (a && b) = true ↔ a = true ∧ c = true := by
  subst h; exact Bool.and_eq_true_iff

/-- A run-time check rule's statement: its condition is the rule's own guard, true when it fires. -/
theorem en_tchk {a b : Bool} (h : a = true → b = true) : (a && b) = true ↔ a = true ∧ true = true := by
  constructor
  · intro h'; exact ⟨((Bool.and_eq_true _ _).mp h').1, rfl⟩
  · intro h'; rw [h'.1, h h'.1]; rfl

theorem blt_of_le_blt {a m b : Nat} (h1 : a ≤ m) (h2 : Nat.blt m b = true) : Nat.blt a b = true :=
  Nat.blt_eq.mpr (Nat.lt_of_le_of_lt h1 (Nat.blt_eq.mp h2))

/-- `GlueOK.exR` from per-rule partner lists, each partner with its exclusion proof. -/
theorem exR_of_partners (P : Parent V) {D : Design} (G : Glue P D)
    (fire : ((j : P.I) → Outs (P.CI j).M V) → Wires P.X.M V → P.PR → Bool)
    (rk_lt : ∀ r, G.rk r < D.nR) (rk_at : ∀ r, G.ruleAt (G.rk r) = r) (pl : Tab (List Nat))
    (hT : ∀ i, i < D.nR → D.exR.get i = bits (pl.get i))
    (hL : Emit.Tab.AllR (fun i l => Emit.AllL (fun i' => ∀ oc w, Env P oc w →
      fire oc w (G.ruleAt i') = true → fire oc w (G.ruleAt i) = true → False) l) pl 0 D.nR) :
    ∀ oc w r1 r2, Env P oc w → (D.exR.get (G.rk r2)).testBit (G.rk r1) = true →
      fire oc w r1 = true → fire oc w r2 = true → G.rk r1 = G.rk r2 := by
  intro oc w r1 r2 hE hb f1 f2
  rw [hT _ (rk_lt r2)] at hb
  have hm := mem_of_testBit_bits hb
  have := Emit.AllL.mem (Emit.Tab.get_allR _ 0 D.nR hL (G.rk r2) (Nat.zero_le _) (rk_lt r2)) _ hm oc w hE
  rw [rk_at, rk_at] at this
  exact (this f1 f2).elim

/-- `GlueOK.exR` from exception groups, each with its at-most-one chain (for example a mux's
    users, whose chain the mux needs anyway); the masks and memberships are decided checks. -/
theorem exR_of_chains (P : Parent V) {D : Design} (G : Glue P D)
    (fire : ((j : P.I) → Outs (P.CI j).M V) → Wires P.X.M V → P.PR → Bool)
    (rk_lt : ∀ r, G.rk r < D.nR) (rk_at : ∀ r, G.ruleAt (G.rk r) = r) (memT gidT : Tab (List Nat)) (ng : Nat)
    (hch : Emit.Tab.AllR (fun _ l => ∀ oc w, Env P oc w → amoChain false (l.map (fun i => fire oc w (G.ruleAt i))))
      memT 0 ng)
    (hg : ∀ x, x < D.nR → ∀ g ∈ gidT.get x, g < ng ∧ x ∈ memT.get g)
    (hT : ∀ y, y < D.nR → Sub (D.exR.get y) (orMask (gidT.get y) (fun g => bits (memT.get g)))) :
    ∀ oc w r1 r2, Env P oc w → (D.exR.get (G.rk r2)).testBit (G.rk r1) = true →
      fire oc w r1 = true → fire oc w r2 = true → G.rk r1 = G.rk r2 := by
  intro oc w r1 r2 hE hb f1 f2
  obtain ⟨g, hgy, hgx⟩ := testBit_orMask (hT _ (rk_lt r2) _ hb)
  have hx := mem_of_testBit_bits hgx
  obtain ⟨hng, hy⟩ := hg _ (rk_lt r2) g hgy
  have hch' := Emit.Tab.get_allR _ 0 ng hch g (Nat.zero_le _) hng oc w hE
  apply Classical.byContradiction
  intro hne
  exact amoChain_excl (fun i => fire oc w (G.ruleAt i)) (memT.get g) false hch' _ hx _ hy hne
    (by simp only [rk_at]; exact f1) (by simp only [rk_at]; exact f2)

end GenGlue

end Scan
end Modular

#print axioms Modular.Scan.Design.ok_of_chain
#print axioms Modular.Scan.kindOK_of_chk
#print axioms Modular.Scan.kindOut_chain
#print axioms Modular.Scan.Design.mm_static_out
#print axioms Modular.Scan.h4_of_scan
#print axioms Modular.Scan.mm_of_scan
#print axioms Modular.Scan.mmg_of_scan
#print axioms Modular.Scan.h1_of_scan
#print axioms Modular.Scan.h2_of_scan
#print axioms Modular.Scan.h3_of_scan
#print axioms Modular.Scan.grpM_of_scan
#print axioms Modular.Scan.grpR_of_scan
#print axioms Modular.Scan.pairFacts_of_scan
#print axioms Modular.Scan.PairFacts.toCert
#print axioms Modular.Scan.excl_of_groups
#print axioms Modular.Scan.exR_of_groups
#print axioms Modular.Scan.exR_of_chains
#print axioms Modular.Scan.esc_of_pairs
#print axioms Modular.Scan.chain_in_of_sites
#print axioms Modular.Scan.chain_inc_of_sites
#print axioms Modular.Scan.chain_keys
#print axioms Modular.Scan.stF_mono
#print axioms Modular.Scan.taskAnn_group
#print axioms Modular.Scan.amoChain_of_pairsC
#print axioms Modular.Scan.forall_of_check
#print axioms Modular.Scan.forall_split
#print axioms Modular.Scan.en_list
#print axioms Modular.Scan.arg_mux
