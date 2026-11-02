/-
Example.lean: a toy design in the style of an emitted file, whose headline
theorem covers every way a parent may call its methods, through Generic.lean.
Build:

  lean -o build/Generic.olean Generic.lean
  LEAN_PATH=build:<toolchain>/lib/lean lean Example.lean

The module (σ, bsc's order of its steps):

  0. getR    value method: returns r; flags illegal_access if r.write was
             already called (r: read SB write)
  1. getR#2  its second read copy (the same step, with its own result field)
  2. setR    action method, enable EN_setR: r.write(setR_x)
  3. RL_a    rule (guard ga): q.write(r written ? qa : qb)
  4. RL_b    rule (guard gb): q.write(qb), p.write(True)
  5. clearA  action method, enable EN_clearA: t.write(False); also reads p
  6. clearB  action method, enable EN_clearB: t.write(False)

Exported: setR, clearA, clearB pairwise CF; getR SB setR; getR CF clearA,
clearB.  getR has no arguments, so a parent may read it from several of its
rules in one cycle: the generator gives it a fixed number of read copies (two
here), each its own method with getR's annotations, and each read in the
parent's order is a call of the next copy (`Method.getR j`).  bsc proved RL_a's and RL_b's guards disjoint (an exclusion group).
clearA and clearB both write t, with the same value: they commute, but only a
semantic argument (`bv_decide`) shows it (an exception pair).  Without the
group the schedule would order setR before clearA through two rules
(setR → RL_a → RL_b → clearA); without the pair, clearA before clearB.

Every per-step fact is one lemma per step, combined by one dispatch tree
(`Run.forallSteps`); group facts are one lemma per group.  Nothing here is
quadratic in the number of steps.
-/
import Std.Tactic.BVDecide
import Generic
set_option maxHeartbeats 0
set_option maxRecDepth 100000
set_option autoImplicit false
set_option linter.unusedVariables false
open scoped List

namespace Ex

abbrev Tok8 := BitVec 3

-- ===========================================================================
-- 1. Submodule & Method State Structure
-- ===========================================================================

structure State where
  «r$EN_write» : Bool    -- 0  r.write called
  «r$write_1» : Tok8     -- 1    argument 1
  «t$EN_write» : Bool    -- 2  t.write called
  «t$write_1» : Bool     -- 3    argument 1
  «q$EN_write» : Bool    -- 4  q.write called
  «q$write_1» : Tok8     -- 5    argument 1
  getR : Tok8            -- 6  module output
  illegal_access : Bool  -- 7  a submodule saw an order its annotations forbid
  «p$write_1» : Bool     -- 8  p.write's argument (clearA reads it)
  guard_violated : Bool  -- 9  a step fired with its guard false
  «getR#2» : Tok8        -- 10 module output, getR's second read

structure Inputs where
  EN_setR : Bool
  setR_x : Tok8
  EN_clearA : Bool
  EN_clearB : Bool
  «r$read» : Tok8
  ga : Bool
  gb : Bool
  qa : Tok8
  qb : Tok8

-- ===========================================================================
-- 3. Pre-Scheduled Sequential Semantics: each step as an effect
-- ===========================================================================

/-- A field's value, of one of the sorts of `State`'s fields. -/
inductive Run.Val where
  | vBool (x : Bool)
  | vTok8 (x : Tok8)

def Run.Val.toBool : Run.Val → Bool | .vBool x => x | _ => false
def Run.Val.toTok8 : Run.Val → Tok8 | .vTok8 x => x | _ => 0#3

instance : Gen.FlagVal Run.Val := ⟨Run.Val.toBool, .vBool, fun _ => rfl⟩

/-- The `k`-th field a step reads (its `R` list is sorted). -/
def Run.rd (rs : List Run.Val) (k : Nat) : Run.Val := rs.getD k (.vBool false)

/-- Fields 1, 5, 6 and 10 hold tokens, the others Booleans. -/
def Run.isTok (k : Nat) : Bool := k == 1 || k == 5 || k == 6 || k == 10

/-- The sort of a value, and of a field. -/
def Run.Val.sort : Run.Val → Nat | .vBool _ => 0 | .vTok8 _ => 1
def Run.fsort (k : Nat) : Nat := if Run.isTok k then 1 else 0

/-- Every field holds a value of its sort. -/
def Run.Inv (s : Gen.St Run.Val) : Prop := ∀ k, (s k).sort = Run.fsort k

noncomputable def Run.init (k : Nat) : Run.Val := if Run.isTok k then .vTok8 0#3 else .vBool false

def Run.fp.getR : Gen.FP := ([0], [6], [7])
noncomputable def Run.U.getR (i : Inputs) (rs : List Run.Val) : List Run.Val := [.vTok8 i.«r$read»]
noncomputable def Run.C.getR (i : Inputs) (rs : List Run.Val) : List Bool := [(Run.rd rs 0).toBool]

def Run.fp.getR_2 : Gen.FP := ([0], [10], [7])
noncomputable def Run.U.getR_2 (i : Inputs) (rs : List Run.Val) : List Run.Val := [.vTok8 i.«r$read»]
noncomputable def Run.C.getR_2 (i : Inputs) (rs : List Run.Val) : List Bool := [(Run.rd rs 0).toBool]

def Run.fp.setR : Gen.FP := ([1], [1], [0])
noncomputable def Run.U.setR (i : Inputs) (rs : List Run.Val) : List Run.Val :=
  [.vTok8 (bif i.EN_setR then i.setR_x else (Run.rd rs 0).toTok8)]
noncomputable def Run.C.setR (i : Inputs) (rs : List Run.Val) : List Bool := [i.EN_setR]

def Run.fp.RL_a : Gen.FP := ([0, 5], [5], [4])
noncomputable def Run.U.RL_a (i : Inputs) (rs : List Run.Val) : List Run.Val :=
  [.vTok8 (bif i.ga then (bif (Run.rd rs 0).toBool then i.qa else i.qb) else (Run.rd rs 1).toTok8)]
noncomputable def Run.C.RL_a (i : Inputs) (rs : List Run.Val) : List Bool := [i.ga]

def Run.fp.RL_b : Gen.FP := ([5, 8], [5, 8], [4])
noncomputable def Run.U.RL_b (i : Inputs) (rs : List Run.Val) : List Run.Val :=
  [.vTok8 (bif i.gb then i.qb else (Run.rd rs 0).toTok8), .vBool (bif i.gb then true else (Run.rd rs 1).toBool)]
noncomputable def Run.C.RL_b (i : Inputs) (rs : List Run.Val) : List Bool := [i.gb]

def Run.fp.clearA : Gen.FP := ([3, 8], [3], [2])
noncomputable def Run.U.clearA (i : Inputs) (rs : List Run.Val) : List Run.Val :=
  [.vBool (bif i.EN_clearA then false else (Run.rd rs 0).toBool)]
noncomputable def Run.C.clearA (i : Inputs) (rs : List Run.Val) : List Bool := [i.EN_clearA]

def Run.fp.clearB : Gen.FP := ([3], [3], [2])
noncomputable def Run.U.clearB (i : Inputs) (rs : List Run.Val) : List Run.Val :=
  [.vBool (bif i.EN_clearB then false else (Run.rd rs 0).toBool)]
noncomputable def Run.C.clearB (i : Inputs) (rs : List Run.Val) : List Bool := [i.EN_clearB]

def Run.n : Nat := 7

/-! Tables by step are if-trees over the step number (round 3's `natTree`),
    with an explicit default past the last step. -/

/-- The footprints by step: a search tree, data rather than code (`Gen.Tab`),
    as for every table the kernel reads during the scan. -/
def Run.fpT : Gen.Tab Gen.FP :=
  .node 7
    (.node 3 (.node 1 (.leaf Run.fp.getR) (.node 2 (.leaf Run.fp.getR_2) (.leaf Run.fp.setR)))
      (.node 5 (.node 4 (.leaf Run.fp.RL_a) (.leaf Run.fp.RL_b)) (.node 6 (.leaf Run.fp.clearA) (.leaf Run.fp.clearB))))
    (.leaf ([], [], []))

def Run.fpOf (k : Nat) : Gen.FP := Run.fpT.get k

/-- The table at each step (`rfl`, an O(log n) walk down the tree) and past the last. -/
theorem Run.fp_0 : Run.fpOf 0 = Run.fp.getR := rfl
theorem Run.fp_1 : Run.fpOf 1 = Run.fp.getR_2 := rfl
theorem Run.fp_2 : Run.fpOf 2 = Run.fp.setR := rfl
theorem Run.fp_3 : Run.fpOf 3 = Run.fp.RL_a := rfl
theorem Run.fp_4 : Run.fpOf 4 = Run.fp.RL_b := rfl
theorem Run.fp_5 : Run.fpOf 5 = Run.fp.clearA := rfl
theorem Run.fp_6 : Run.fpOf 6 = Run.fp.clearB := rfl
theorem Run.fp_ge {k : Nat} (h : ¬ k < 7) : Run.fpOf k = ([], [], []) := by
  have e : Nat.blt k 7 = false := by
    cases hb : Nat.blt k 7
    · rfl
    · exact absurd (Nat.le_of_ble_eq_true hb) h
  simp only [Run.fpOf, Run.fpT, Gen.Tab.get, e, Bool.cond_false]

noncomputable def Run.uOf (i : Inputs) (k : Nat) : List Run.Val → List Run.Val :=
  if k < 7 then
    if k < 3 then (if k < 1 then Run.U.getR i else if k < 2 then Run.U.getR_2 i else Run.U.setR i)
    else (if k < 5 then (if k < 4 then Run.U.RL_a i else Run.U.RL_b i)
      else if k < 6 then Run.U.clearA i else Run.U.clearB i)
  else fun _ => []

noncomputable def Run.cOf (i : Inputs) (k : Nat) : List Run.Val → List Bool :=
  if k < 7 then
    if k < 3 then (if k < 1 then Run.C.getR i else if k < 2 then Run.C.getR_2 i else Run.C.setR i)
    else (if k < 5 then (if k < 4 then Run.C.RL_a i else Run.C.RL_b i)
      else if k < 6 then Run.C.clearA i else Run.C.clearB i)
  else fun _ => []

/-- The step at position `k` of the schedule's order. -/
noncomputable def Run.eff (i : Inputs) (k : Nat) : Gen.Eff Run.Val :=
  { R := (Run.fpOf k).1, W := (Run.fpOf k).2.1, O := (Run.fpOf k).2.2, U := Run.uOf i k, C := Run.cOf i k }

/-- Run the steps listed, in that order. -/
noncomputable def Run.run (i : Inputs) (l : List Nat) (s : Gen.St Run.Val) : Gen.St Run.Val :=
  Gen.run (Gen.stepOf (Run.eff i)) l s

/-- Whether each step fires this cycle: its hardware WILL_FIRE (a function of
    the inputs) for steps of enable-gated methods and of exclusion groups;
    `true` for every other step (here getR and its copy). -/
def Run.fire (i : Inputs) (k : Nat) : Bool :=
  if k < 7 then
    if k < 3 then (if k < 1 then true else if k < 2 then true else i.EN_setR)
    else (if k < 5 then (if k < 4 then i.ga else i.gb)
      else if k < 6 then i.EN_clearA else i.EN_clearB)
  else false

noncomputable def Run.toState (s : Gen.St Run.Val) : State :=
  { «r$EN_write» := (s 0).toBool, «r$write_1» := (s 1).toTok8, «t$EN_write» := (s 2).toBool,
    «t$write_1» := (s 3).toBool, «q$EN_write» := (s 4).toBool, «q$write_1» := (s 5).toTok8,
    getR := (s 6).toTok8, illegal_access := (s 7).toBool, «p$write_1» := (s 8).toBool,
    guard_violated := (s 9).toBool, «getR#2» := (s 10).toTok8 }

/-- One cycle, one rule at a time, in the schedule's order. -/
noncomputable def stepSequential (i : Inputs) : State := Run.toState (Run.run i (List.range Run.n) Run.init)

-- ===========================================================================
-- 4. Post-Scheduled Synchronous Hardware Semantics
-- ===========================================================================

noncomputable def stepScheduled (i : Inputs) : State :=
  { «r$EN_write» := i.EN_setR, «r$write_1» := i.setR_x,
    «t$EN_write» := i.EN_clearA || i.EN_clearB, «t$write_1» := false,
    «q$EN_write» := i.ga || i.gb, «q$write_1» := bif i.gb then i.qb else bif i.EN_setR then i.qa else i.qb,
    getR := i.«r$read», illegal_access := false, «p$write_1» := i.gb, guard_violated := false,
    «getR#2» := i.«r$read» }

-- ===========================================================================
-- 5. Disjointness Oracles & Main Schedule Soundness Theorem
-- ===========================================================================

structure Hyps (i : Inputs) : Prop where
  /-- bsc proved the guards of RL_a and RL_b disjoint (chain form, as `Hw.disjoint_N`) -/
  disjoint_RL_a_RL_b : (i.ga && i.gb) = false

/-- No submodule saw a forbidden order, no step fired with its guard false, and
    the hardware agrees with the sequential execution on every submodule input
    (an argument when its enable is set) and every output. -/
noncomputable def Agree (i : Inputs) (s h : State) : Bool :=
  !s.illegal_access && !s.guard_violated &&
  s.«r$EN_write» == h.«r$EN_write» && (!h.«r$EN_write» || s.«r$write_1» == h.«r$write_1») &&
  s.«t$EN_write» == h.«t$EN_write» && (!h.«t$EN_write» || s.«t$write_1» == h.«t$write_1») &&
  s.«q$EN_write» == h.«q$EN_write» && (!h.«q$EN_write» || s.«q$write_1» == h.«q$write_1») &&
  s.getR == h.getR && s.«getR#2» == h.getR

theorem Run.range_n : List.range Run.n = [0, 1, 2, 3, 4, 5, 6] := rfl

/-- The schedule's order agrees with the hardware (here by `bv_decide`, as in
    an emitted file). -/
theorem schedule_sound (i : Inputs) (h : Hyps i) : Agree i (stepSequential i) (stepScheduled i) = true := by
  obtain ⟨hd⟩ := h
  simp only [stepSequential, Run.run, Run.range_n, Gen.run, Gen.stepOf, Run.toState, Agree, stepScheduled]
  simp [Gen.Eff.apply, Gen.Eff.val, Run.eff, Run.fp_0, Run.fp_1, Run.fp_2, Run.fp_3, Run.fp_4, Run.fp_5,
    Run.fp_6, Run.uOf, Run.cOf, Run.fp.getR, Run.fp.setR,
    Run.fp.RL_a, Run.fp.RL_b, Run.fp.clearA, Run.fp.clearB, Run.U.getR, Run.U.setR, Run.U.RL_a,
    Run.U.RL_b, Run.U.clearA, Run.U.clearB, Run.C.getR, Run.C.setR, Run.C.RL_a, Run.C.RL_b,
    Run.fp.getR_2, Run.U.getR_2, Run.C.getR_2,
    Run.C.clearA, Run.C.clearB, Run.init, Run.isTok, Run.rd, Gen.FlagVal.ofBool,
    Gen.FlagVal.toBool, Run.Val.toBool, Run.Val.toTok8]
  bv_decide

/-! ### All orders: the data -/

/-- The methods, by name.  `getR j` is the `j`-th read of getR in a cycle (its
    read copies).  Every table the headline theorem mentions is written by
    method name, so it can be checked by eye against bsc's schedule output. -/
inductive Method where
  | setR
  | clearA
  | clearB
  | getR (j : Fin 2)
  deriving DecidableEq

/-- The numbering the proof uses. -/
def Method.toNat : Method → Nat
  | .setR => 0
  | .clearA => 1
  | .clearB => 2
  | .getR j => 3 + j.val

def Method.ofNat (m : Nat) : Method :=
  if m < 3 then (if m < 1 then .setR else if m < 2 then .clearA else .clearB)
  else if m < 4 then .getR 0 else .getR 1

def Ann.M : Nat := 5

/-- The exported annotations by method `b`: the methods exported SB with `b`
    (they must be called first), SBR with `b`, and C with `b`.  Every other pair
    is CF.  Each read copy of getR carries getR's annotations. -/
def Ann.sbM : Method → List Method
  | .setR => [.getR 0, .getR 1]
  | _ => []
def Ann.sbrM : Method → List Method := fun _ => []
def Ann.exclM : Method → List Method := fun _ => []
/-- Exported CF pairs the generator could not verify (none here). -/
def Ann.unvM : Method → List Method := fun _ => []
/-- The methods with an enable port, and the port. -/
def Ann.hasEnableM : Method → Bool
  | .getR _ => false
  | _ => true
def Ann.enM (i : Inputs) : Method → Bool
  | .setR => i.EN_setR
  | .clearA => i.EN_clearA
  | .clearB => i.EN_clearB
  | .getR _ => false

/-- The same tables by number, for the proof. -/
def Ann.byNum (t : Method → List Method) (b : Nat) : List Nat :=
  if b < Ann.M then (t (Method.ofNat b)).map Method.toNat else []
def Ann.sb := Ann.byNum Ann.sbM
def Ann.sbr := Ann.byNum Ann.sbrM
def Ann.excl := Ann.byNum Ann.exclM
def Ann.unv := Ann.byNum Ann.unvM
def Ann.hasEnable (m : Nat) : Bool := m < Ann.M && Ann.hasEnableM (Method.ofNat m)
def Ann.en (i : Inputs) (m : Nat) : Bool := Ann.enM i (Method.ofNat m)

/-- The parent's calls by number. -/
def Ann.calls (P : List (List Method)) : List (List Nat) := P.map (·.map Method.toNat)

/-- Methods whose steps never fire in a cycle together, by group (none here:
    `Gen.nba_of_groups` with no groups).  `ng b` is `b`'s group, `gmeth g` the
    group's methods, `gsteps g` their steps in chain order. -/
def Ann.ng (b : Nat) : Nat := 0
def Ann.gmeth (g : Nat) : List Nat := []
def Ann.gsteps (g : Nat) : List Nat := []
def Ann.nba (b : Nat) : List Nat := Ann.gmeth (Ann.ng b)

/-- Each step's method (`m + 1`, 0 for a rule), each method's steps (from the
    generator's method-step table), and the exception groups: 1 is clearA and
    clearB (a semantic pair), 2 is RL_a and RL_b (never both fire). -/
def Scan.meth (k : Nat) : Nat :=
  if k < 7 then (if k < 3 then (if k < 1 then 4 else if k < 2 then 5 else 1)
    else (if k < 5 then 0 else if k < 6 then 2 else 3)) else 0
def Scan.stepsOf (m : Nat) : List Nat :=
  if m < 3 then (if m < 1 then [2] else if m < 2 then [5] else [6])
  else if m < 4 then [0] else if m < 5 then [1] else []
def Scan.tag (k : Nat) : Nat :=
  if k < 7 then (if k < 3 then 0 else if k < 5 then 2 else 1) else 0
def Scan.groups : Nat := 3
def Scan.mem (t : Nat) : List Nat := if t < 2 then (if t < 1 then [] else [5, 6]) else if t < 3 then [3, 4] else []

/-- The steps labelled with a method are exactly that method's steps, the
    numbering is a bijection on the methods, and no method is annotated with
    itself (linear; this checks the tables against one another, while the
    tables' meaning is read off the named tables above). -/
theorem Scan.meth_ok :
    ((∀ k, k < Run.n → Scan.meth k ≠ 0 → Scan.meth k - 1 < Ann.M ∧ k ∈ Scan.stepsOf (Scan.meth k - 1)) ∧
    ∀ m, m < Ann.M → (∀ k ∈ Scan.stepsOf m, k < Run.n ∧ Scan.meth k = m + 1) ∧
      (Method.ofNat m).toNat = m ∧ m ∉ Ann.sb m ++ Ann.sbr m ++ Ann.excl m ++ Ann.unv m) := by
  decide +kernel

def Scan.allowed (sb sbr : Nat → List Nat) (b : Nat) : Nat :=
  Gen.allowedOf Ann.M (sb b) (sbr b) (Ann.excl b) (Ann.nba b) (Ann.unv b)

def Scan.D : Gen.Design :=
  ⟨Run.n, 4, Ann.M, Run.fpOf, Scan.meth, Ann.hasEnable, Scan.tag, Scan.allowed Ann.sb Ann.sbr⟩

/-- **The check**, in checkpointed segments, each decided by the kernel on its
    own: steps 0-2 from the initial state end in `Scan.cp3` (the fields' scan
    states before step 3, written down by the generator), and steps 3-6 pass
    from there.  A large design has one segment per few thousand uses, so no
    single kernel check grows with the design. -/
def Scan.cp3 : Gen.Tr := Gen.Tr.ofList 4 [(0, ⟨3, 2, 1, 24, 1⟩), (1, ⟨3, 1, 2, 0, 1⟩), (6, ⟨1, 1, 2, 0, 8⟩), (7, ⟨2, 1, 1, 0, 24⟩),
    (10, ⟨2, 1, 2, 0, 16⟩)]

theorem Scan.seg0 : Scan.D.segChk 0 3 (.lf Gen.FS.init) Scan.cp3 = true := by decide +kernel
theorem Scan.seg1 : Scan.D.scanFrom (List.range' 3 (Run.n - 3)) Scan.cp3 = true := by decide +kernel

theorem Scan.ok : Scan.D.Ok := Scan.D.ok_of_chain [(3, Scan.cp3)] ⟨Scan.seg0, Scan.seg1⟩

/-- The same check in one piece. -/
theorem Scan.ok_whole : Scan.D.scan = true := by decide +kernel

/-- A wrong checkpoint is rejected. -/
theorem Scan.bad_checkpoint : Scan.D.segChk 0 3 (.lf Gen.FS.init) (.lf Gen.FS.init) = false := by decide +kernel

/-- It fails if getR were exported CF with setR (it should be SB) ... -/
theorem Scan.bad_annotation :
    ({ Scan.D with allowed := Scan.allowed (fun _ => []) Ann.sbr } : Gen.Design).scan = false := by
  decide +kernel

/-- ... without the exclusion group (setR → RL_a → RL_b → clearA, all CF) ... -/
theorem Scan.needs_group :
    ({ Scan.D with tag := fun k => if k = 5 ∨ k = 6 then 1 else 0 } : Gen.Design).scan = false := by
  decide +kernel

/-- ... and without the exception pair (clearA and clearB both write t). -/
theorem Scan.needs_pair :
    ({ Scan.D with tag := fun k => if k = 3 ∨ k = 4 then 2 else 0 } : Gen.Design).scan = false := by
  decide +kernel

/-- Without the group, the path setR → RL_a → RL_b → clearA (and getR's, which
    reaches RL_a too) runs through rules: SB is not enough (a parent rule calling
    setR and clearA could not be explained) ... -/
theorem Scan.sb_not_enough :
    ({ Scan.D with tag := fun k => if k = 5 ∨ k = 6 then 1 else 0,
                   allowed := Scan.allowed (fun b => if b = 0 then [3, 4] else if b = 1 then [0, 3, 4] else [])
                     Ann.sbr } : Gen.Design).scan = false := by
  decide +kernel

/-- ... while SBR is. -/
theorem Scan.sbr_ok :
    ({ Scan.D with tag := fun k => if k = 5 ∨ k = 6 then 1 else 0,
                   allowed := Scan.allowed Ann.sb (fun b => if b = 1 then [0, 3, 4] else []) } : Gen.Design).scan
      = true := by
  decide +kernel

/-! ### All orders: per-step facts, one lemma per step -/

/-- The one dispatch tree: a fact of every step from one lemma per step. -/
theorem Run.forallSteps {Q : Nat → Prop} (h0 : Q 0) (h1 : Q 1) (h2 : Q 2) (h3 : Q 3) (h4 : Q 4) (h5 : Q 5)
    (h6 : Q 6) : ∀ k, k < Run.n → Q k :=
  Gen.range_all (Gen.range_split 0 3 7
    (Gen.range_split 0 1 3 (Gen.range_one 0 h0) (Gen.range_split 1 2 3 (Gen.range_one 1 h1) (Gen.range_one 2 h2)))
    (Gen.range_split 3 5 7 (Gen.range_split 3 4 5 (Gen.range_one 3 h3) (Gen.range_one 4 h4))
      (Gen.range_split 5 6 7 (Gen.range_one 5 h5) (Gen.range_one 6 h6))))

theorem Run.vTok8_toTok8 {k : Nat} {v : Run.Val} (h : v.sort = Run.fsort k) (hk : Run.isTok k = true) :
    Run.Val.vTok8 v.toTok8 = v := by
  cases v with
  | vBool x => simp [Run.Val.sort, Run.fsort, hk] at h
  | vTok8 x => rfl

theorem Run.vBool_toBool {k : Nat} {v : Run.Val} (h : v.sort = Run.fsort k) (hk : Run.isTok k = false) :
    Run.Val.vBool v.toBool = v := by
  cases v with
  | vBool x => rfl
  | vTok8 x => simp [Run.Val.sort, Run.fsort, hk] at h

theorem Run.init_ok : Run.Inv Run.init := by
  intro k; unfold Run.init Run.fsort; cases h : Run.isTok k <;> rfl

/-- The sorts of the values step `k` sets are those of the fields it sets. -/
def Run.SortsAt (i : Inputs) (k : Nat) : Prop :=
  ∀ rs, (Run.uOf i k rs).map Run.Val.sort = (Run.fpOf k).2.1.map Run.fsort

/-- Every step keeps every field of its sort: `fun rs => rfl` per step, and one
    kernel-decided fact for the flags. -/
theorem Run.keeps (i : Inputs) : Gen.Keeps Run.Inv (Gen.stepOf (Run.eff i)) := by
  have hO : ∀ k, k < Run.n → ∀ j ∈ (Run.fpOf k).2.2, Run.fsort j = 0 := by decide +kernel
  have hU : ∀ k, k < Run.n → Run.SortsAt i k :=
    Run.forallSteps (fun rs => rfl) (fun rs => rfl) (fun rs => rfl) (fun rs => rfl) (fun rs => rfl)
      (fun rs => rfl) (fun rs => rfl)
  intro k s hs
  apply Gen.Eff.keeps_sorts Run.Val.sort Run.fsort (Run.eff i k) _ _ s hs
  · by_cases hk : k < Run.n
    · exact hU k hk
    · intro rs; simp [Run.eff, Run.uOf, Run.fp_ge hk, show ¬ k < 7 from hk]
  · intro j hj b
    show 0 = Run.fsort j
    by_cases hk : k < Run.n
    · exact (hO k hk j hj).symm
    · simp [Run.eff, Run.fp_ge hk] at hj

/-- A step that does not fire leaves the state as it is.  Each step's lemma
    rewrites the tables at that step by `rfl` equations (an O(log n) walk down
    the if-tree), never unfolding a whole table. -/
def Run.IdleAt (i : Inputs) (k : Nat) : Prop :=
  Run.fire i k = false → ∀ s, Run.Inv s → Gen.stepOf (Run.eff i) k s = s

theorem Run.idle_0 (i : Inputs) : Run.IdleAt i 0 := by intro hx; simp [Run.fire] at hx

theorem Run.idle_1 (i : Inputs) : Run.IdleAt i 1 := by intro hx; simp [Run.fire] at hx

theorem Run.idle_2 (i : Inputs) : Run.IdleAt i 2 := by
  have e1 : Run.fpOf 2 = Run.fp.setR := rfl
  have e2 : Run.uOf i 2 = Run.U.setR i := rfl
  have e3 : Run.cOf i 2 = Run.C.setR i := rfl
  have e4 : Run.fire i 2 = i.EN_setR := rfl
  intro hx s hs
  rw [e4] at hx
  apply Gen.Eff.apply_eq_self
  · intro p hp
    simp [Run.eff, e1, e2, Run.fp.setR, Run.U.setR, hx, Run.rd] at hp
    subst hp; exact Run.vTok8_toTok8 (hs 1) rfl
  · intro p hp
    simp [Run.eff, e1, e3, Run.fp.setR, Run.C.setR, hx] at hp
    subst hp; exact ⟨rfl, Run.vBool_toBool (hs 0) rfl⟩

theorem Run.idle_3 (i : Inputs) : Run.IdleAt i 3 := by
  have e1 : Run.fpOf 3 = Run.fp.RL_a := rfl
  have e2 : Run.uOf i 3 = Run.U.RL_a i := rfl
  have e3 : Run.cOf i 3 = Run.C.RL_a i := rfl
  have e4 : Run.fire i 3 = i.ga := rfl
  intro hx s hs
  rw [e4] at hx
  apply Gen.Eff.apply_eq_self
  · intro p hp
    simp [Run.eff, e1, e2, Run.fp.RL_a, Run.U.RL_a, hx, Run.rd] at hp
    subst hp; exact Run.vTok8_toTok8 (hs 5) rfl
  · intro p hp
    simp [Run.eff, e1, e3, Run.fp.RL_a, Run.C.RL_a, hx] at hp
    subst hp; exact ⟨rfl, Run.vBool_toBool (hs 4) rfl⟩

theorem Run.idle_4 (i : Inputs) : Run.IdleAt i 4 := by
  have e1 : Run.fpOf 4 = Run.fp.RL_b := rfl
  have e2 : Run.uOf i 4 = Run.U.RL_b i := rfl
  have e3 : Run.cOf i 4 = Run.C.RL_b i := rfl
  have e4 : Run.fire i 4 = i.gb := rfl
  intro hx s hs
  rw [e4] at hx
  apply Gen.Eff.apply_eq_self
  · intro p hp
    simp [Run.eff, e1, e2, Run.fp.RL_b, Run.U.RL_b, hx, Run.rd] at hp
    rcases hp with rfl | rfl
    · exact Run.vTok8_toTok8 (hs 5) rfl
    · exact Run.vBool_toBool (hs 8) rfl
  · intro p hp
    simp [Run.eff, e1, e3, Run.fp.RL_b, Run.C.RL_b, hx] at hp
    subst hp; exact ⟨rfl, Run.vBool_toBool (hs 4) rfl⟩

theorem Run.idle_5 (i : Inputs) : Run.IdleAt i 5 := by
  have e1 : Run.fpOf 5 = Run.fp.clearA := rfl
  have e2 : Run.uOf i 5 = Run.U.clearA i := rfl
  have e3 : Run.cOf i 5 = Run.C.clearA i := rfl
  have e4 : Run.fire i 5 = i.EN_clearA := rfl
  intro hx s hs
  rw [e4] at hx
  apply Gen.Eff.apply_eq_self
  · intro p hp
    simp [Run.eff, e1, e2, Run.fp.clearA, Run.U.clearA, hx, Run.rd] at hp
    subst hp; exact Run.vBool_toBool (hs 3) rfl
  · intro p hp
    simp [Run.eff, e1, e3, Run.fp.clearA, Run.C.clearA, hx] at hp
    subst hp; exact ⟨rfl, Run.vBool_toBool (hs 2) rfl⟩

theorem Run.idle_6 (i : Inputs) : Run.IdleAt i 6 := by
  have e1 : Run.fpOf 6 = Run.fp.clearB := rfl
  have e2 : Run.uOf i 6 = Run.U.clearB i := rfl
  have e3 : Run.cOf i 6 = Run.C.clearB i := rfl
  have e4 : Run.fire i 6 = i.EN_clearB := rfl
  intro hx s hs
  rw [e4] at hx
  apply Gen.Eff.apply_eq_self
  · intro p hp
    simp [Run.eff, e1, e2, Run.fp.clearB, Run.U.clearB, hx, Run.rd] at hp
    subst hp; exact Run.vBool_toBool (hs 3) rfl
  · intro p hp
    simp [Run.eff, e1, e3, Run.fp.clearB, Run.C.clearB, hx] at hp
    subst hp; exact ⟨rfl, Run.vBool_toBool (hs 2) rfl⟩

theorem Run.idle_comm (i : Inputs) : Gen.IdleComm Run.Inv (Gen.stepOf (Run.eff i)) Run.n (Run.fire i) := by
  have h := Run.forallSteps (Run.idle_0 i) (Run.idle_1 i) (Run.idle_2 i) (Run.idle_3 i) (Run.idle_4 i)
    (Run.idle_5 i) (Run.idle_6 i)
  intro x y hx hy hf
  exact Gen.comm_of_id Run.Inv (Run.eff i) (Run.keeps i) x y (h x hx hf)

/-- A step of an enable-gated method fires only when its method is enabled. -/
def Run.EnAt (i : Inputs) (k : Nat) : Prop :=
  Run.fire i k = true → Scan.meth k ≠ 0 → Ann.hasEnable (Scan.meth k - 1) = true →
    Ann.en i (Scan.meth k - 1) = true

theorem Run.hen (i : Inputs) : ∀ x, x < Run.n → Run.EnAt i x :=
  Run.forallSteps (by simp [Run.EnAt, Scan.meth, Ann.hasEnable, Ann.hasEnableM, Method.ofNat, Ann.M])
    (by simp [Run.EnAt, Scan.meth, Ann.hasEnable, Ann.hasEnableM, Method.ofNat, Ann.M])
    (fun hf _ _ => by simpa [Run.fire, Scan.meth, Ann.en, Ann.enM, Method.ofNat] using hf)
    (by simp [Run.EnAt, Scan.meth]) (by simp [Run.EnAt, Scan.meth])
    (fun hf _ _ => by simpa [Run.fire, Scan.meth, Ann.en, Ann.enM, Method.ofNat] using hf)
    (fun hf _ _ => by simpa [Run.fire, Scan.meth, Ann.en, Ann.enM, Method.ofNat] using hf)

@[simp] theorem Run.Val.toBool_vBool (b : Bool) : (Run.Val.vBool b).toBool = b := rfl
@[simp] theorem Run.flag_toBool (v : Run.Val) : Gen.FlagVal.toBool v = v.toBool := rfl
@[simp] theorem Run.flag_ofBool (b : Bool) : (Gen.FlagVal.ofBool b : Run.Val) = .vBool b := rfl

/-! ### All orders: group facts, one lemma per group -/

/-- The exception pair, a semantic fact (`bv_decide`): clearA and clearB leave
    t's call the same in either order. -/
theorem Comm.clearA.clearB (i : Inputs) : Gen.Comm Run.Inv (Gen.stepOf (Run.eff i)) 5 6 := by
  intro s _
  show (Run.eff i 6).apply ((Run.eff i 5).apply s) = (Run.eff i 5).apply ((Run.eff i 6).apply s)
  apply Gen.Eff.comm_on _ _ s [2, 3] rfl
  intro k hk
  simp only [List.mem_cons, List.mem_nil_iff, or_false] at hk
  rcases hk with rfl | rfl <;>
  simp [Gen.Eff.apply, Gen.Eff.val, Run.eff, Run.fp_5, Run.fp_6, Run.uOf, Run.cOf, Run.fp.clearA, Run.fp.clearB,
    Run.U.clearA, Run.U.clearB, Run.C.clearA, Run.C.clearB, Run.rd, List.lookup_cons] <;>
  bv_decide

/-- The exclusion group: from the chain-form disjointness fact, at most one of
    RL_a, RL_b fires. -/
theorem Run.group_2 (i : Inputs) (h : Hyps i) : Gen.GroupComm Run.Inv (Gen.stepOf (Run.eff i)) [3, 4] :=
  Gen.groupComm_of_chain (Run.idle_comm i) [3, 4] (by decide)
    (by simp [Gen.chainOK, Run.fire, h.disjoint_RL_a_RL_b])

/-- Exception groups, dispatched by one `range_split` tree over the group
    numbers `1 .. groups - 1`. -/
theorem Run.tag_comm (i : Inputs) (h : Hyps i) : Gen.TagComm Run.Inv (Gen.stepOf (Run.eff i)) Run.n Scan.tag :=
  Gen.tagComm_of_groups _ _ Run.n Scan.groups Scan.tag Scan.mem (by decide +kernel)
    (fun t ht0 htG => Gen.range_split (Q := fun t => Gen.GroupComm Run.Inv (Gen.stepOf (Run.eff i)) (Scan.mem t))
      1 2 3 (Gen.range_one 1 (Gen.groupComm_pair (Comm.clearA.clearB i)))
      (Gen.range_one 2 (Run.group_2 i h)) t (by omega) htG)

/-- Never both fire, between methods: from the (here empty) group tables. -/
theorem Run.nba (i : Inputs) : ∀ a b x y, a ∈ Ann.nba b → a ≠ b → x < Run.n → y < Run.n →
    Scan.meth x = a + 1 → Scan.meth y = b + 1 → Run.fire i x = true → Run.fire i y = true → False :=
  Gen.nba_of_groups (G := 1) (ng := Ann.ng) (gmeth := Ann.gmeth) (gsteps := Ann.gsteps)
    (fun g hg a ha => by simp [Ann.gmeth] at ha) rfl (fun g _ => rfl) (fun x hx _ h => (h rfl).elim)
    (fun g h0 h1 => by omega)

/-! ### The headline theorem -/

/-- **The headline theorem.**  Whatever methods the parent enables this cycle,
    from whatever parent rules and in whatever order, so long as it keeps to the
    exported annotations (`Gen.ParentOrder`: `P` lists each firing parent rule's
    calls, by method name), some order of all the module's steps runs the calls
    in that order, keeps each parent rule's calls together, and agrees with the
    hardware.  getR may be read up to twice per cycle (`Method.getR 0`, then
    `Method.getR 1`), from one parent rule or several; each read's result is a
    module output `Agree` compares with the hardware's. -/
theorem all_orders (i : Inputs) (h : Hyps i) (P : List (List Method))
    (hP : Gen.ParentOrder Ann.hasEnable Ann.sb Ann.sbr Ann.excl Ann.unv (Ann.en i) (Ann.calls P)) :
    ∃ π : List Nat, π.Perm (List.range Run.n) ∧ Gen.Extends Scan.meth Run.n (Ann.calls P) π ∧
      Gen.Contiguous Scan.meth (Ann.calls P) π ∧
      Agree i (Run.toState (Run.run i π Run.init)) (stepScheduled i) = true := by
  obtain ⟨π, hperm, hext, hcon, hrun⟩ :=
    Gen.all_orders_parent Scan.D Scan.ok (Run.eff i) (fun k => rfl) Run.Inv (Run.keeps i) (Run.fire i)
      (Run.idle_comm i) (Run.tag_comm i h) Ann.hasEnable Ann.sb Ann.sbr Ann.excl Ann.nba Ann.unv
      (fun b => rfl) (Ann.en i) (fun m hm => hm) (Run.hen i) (Run.nba i) (Ann.calls P) hP
  refine ⟨π, hperm, hext, hcon, ?_⟩
  show Agree i (Run.toState (Run.run i π Run.init)) (stepScheduled i) = true
  rw [show Run.run i π Run.init = Run.run i (List.range Run.n) Run.init from hrun Run.init Run.init_ok]
  exact schedule_sound i h

open Method in
/-- Ways to call the methods: the twelve orders of setR, clearA, clearB and one
    read of getR before setR, one call per parent rule (among them all six
    orders of the three CF methods, and orders the schedule never runs, like
    clearB, clearA, getR, setR); calls grouped into parent rules; and getR read
    twice, by two parent rules or by one. -/
def Ann.orders : List (List (List Method)) :=
  ([[getR 0, setR, clearA, clearB], [getR 0, setR, clearB, clearA], [getR 0, clearA, setR, clearB],
    [getR 0, clearA, clearB, setR], [getR 0, clearB, setR, clearA], [getR 0, clearB, clearA, setR],
    [clearA, getR 0, setR, clearB], [clearA, getR 0, clearB, setR], [clearB, getR 0, setR, clearA],
    [clearB, getR 0, clearA, setR], [clearA, clearB, getR 0, setR], [clearB, clearA, getR 0, setR]].map
    (fun l => l.map (fun m => [m]))) ++
  [[[getR 0, setR, clearA, clearB]], [[clearB, clearA], [getR 0, setR]], [[clearB], [getR 0, setR, clearA]],
   [[getR 0], [clearB, clearA, setR]], [[clearA, getR 0], [clearB, setR]],
   [[getR 0, clearA], [getR 1, clearB], [setR]], [[clearB, getR 0, getR 1, setR, clearA]],
   [[getR 0], [clearA], [clearB, getR 1], [setR]]]

theorem every_order (i : Inputs) (h : Hyps i) : ∀ P ∈ Ann.orders,
    ∃ π : List Nat, π.Perm (List.range Run.n) ∧ Gen.Extends Scan.meth Run.n (Ann.calls P) π ∧
      Gen.Contiguous Scan.meth (Ann.calls P) π ∧
      Agree i (Run.toState (Run.run i π Run.init)) (stepScheduled i) = true := by
  intro P hP
  have hchk : ∀ P ∈ Ann.orders, (Ann.calls P).flatten.Nodup ∧ (∀ m, m < 3 → m ∈ (Ann.calls P).flatten) ∧
      Gen.respects Ann.sb Ann.sbr Ann.excl Ann.unv (Ann.calls P).flatten = true ∧
      Gen.groupsOK Ann.sbr Ann.unv (Ann.calls P) = true := by
    decide +kernel
  obtain ⟨hnd, hall, hr, hg⟩ := hchk P hP
  exact all_orders i h P (Gen.ParentOrder.of_check hnd
    (fun m hm _ => hall m (by
      simp only [Ann.hasEnable, Bool.and_eq_true, decide_eq_true_eq] at hm
      have : m < 5 := hm.1
      have e : m = 0 ∨ m = 1 ∨ m = 2 ∨ m = 3 ∨ m = 4 := by omega
      rcases e with rfl | rfl | rfl | rfl | rfl <;> simp_all [Ann.hasEnableM, Method.ofNat])) hr hg)

/-- A cycle in which setR is not enabled: one parent rule calls clearB, then
    clearA, then reads getR. -/
example (i : Inputs) (h : Hyps i) (hs : i.EN_setR = false) :
    ∃ π : List Nat, π.Perm (List.range Run.n) ∧
      Gen.Extends Scan.meth Run.n (Ann.calls [[.clearB, .clearA, .getR 0]]) π ∧
      Gen.Contiguous Scan.meth (Ann.calls [[.clearB, .clearA, .getR 0]]) π ∧
      Agree i (Run.toState (Run.run i π Run.init)) (stepScheduled i) = true :=
  all_orders i h [[.clearB, .clearA, .getR 0]] (Gen.ParentOrder.of_check (by decide)
    (fun m hg he => by
      simp only [Ann.hasEnable, Bool.and_eq_true, decide_eq_true_eq] at hg
      have : m < 5 := hg.1
      have e : m = 0 ∨ m = 1 ∨ m = 2 ∨ m = 3 ∨ m = 4 := by omega
      rcases e with rfl | rfl | rfl | rfl | rfl <;>
        simp_all [Ann.en, Ann.enM, Ann.hasEnableM, Method.ofNat, Ann.calls, Method.toNat])
    (by decide) (by decide))

end Ex

#print axioms Ex.Scan.ok
#print axioms Ex.Scan.bad_annotation
#print axioms Ex.Scan.sb_not_enough
#print axioms Ex.all_orders
#print axioms Ex.every_order
