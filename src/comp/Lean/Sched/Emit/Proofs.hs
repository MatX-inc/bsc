-- | Per-site proofs (mke2e.py lines 2006-2424): reads before, value lemmas,
-- results, calls at every skipped value (HQ), readiness (RY), decided checks.
module Lean.Sched.Emit.Proofs
    ( emitSiteProofs, emitValues, emitHQ, emitRY, emitChecks, evNeeds
    ) where

import Control.Monad (forM_, when)
import Data.Array ((!))
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import qualified Data.Foldable as F (Foldable(toList))
import qualified Data.Map.Strict as M ((!))
import qualified Data.Set as S (Set, empty, fromList, singleton, toAscList, toList, union)

import Lean.Print (allBThm, andc, andget, cut, fmt, stab, stabproof, tab, tabOr, tabproof, tabproofI, tupcong, w, W)
import Lean.Sched.Emit.Ctx (argVal, condHw, condVal, evt, ihelper, isCond, isUncond, kd, lportOf, mxKind,
    ocread, site, sitePort, step, svHw, val, vctx, yof, EC(ecMet, ecM, ecKinds, ecNM, ecHyp, ecHya, ecNS, ecNR,
    ecRDC, ecWXM, ecExport), VCtx(cX, VCtx), exported)
import Lean.Sched.Export (Export(exMeths), XMeth(xmAR), isSynthKind)
import Lean.Sched.Kinds (kdCode, kdFx, KindDef(kdAlwaysRdy, kdPorts), PM(PT, PS))
import Lean.Sched.Model (mxRepr, MSite(sPort, sR, sArgs, sArg, sKind, sCond), MStep(tRes, tSites, tGuard),
    MX(MLit, MK, MXarg, MGK, MF, MSplit, MG, MZ, MTrue, MFalse), Model(mOff))

sitesOf :: EC -> Int -> [MSite]
sitesOf ec k = F.toList (tSites (step ec k))

-- | The value lemmas a proof uses: unconditional reads, conditional reads, site values.
evNeeds :: EC -> (S.Set (Int, Int), S.Set (Int, Int), S.Set (Int, Int))
evNeeds ec = (S.union ev0 ev1, evc, evs)
  where
    collect k e = case e of
        MF _ js -> mconcat [ if isCond ec k j then (S.empty, S.singleton (k, j)) else (S.singleton (k, j), S.empty) | j <- js ]
        MG _ js -> mconcat [ if isCond ec k j then (S.empty, S.singleton (k, j)) else (S.singleton (k, j), S.empty) | j <- js ]
        MLit j -> (S.singleton (k, j), S.empty)
        MSplit is a b -> mconcat ([(S.empty, S.fromList [ (k, i) | i <- is ])] ++ [collect k a, collect k b])
        _ -> (S.empty, S.empty)
    perStep k = let r = step ec k
                    siteC st = if sKind st == "task" then mconcat (collect k (sCond st) : map (collect k) (fromMaybe [] (sArgs st)))
                               else mconcat [collect k (sCond st), collect k (sArg st)]
                    g = case tGuard r of
                            Just gx | mxKind gx `notElem` ["check", "false"] -> collect k gx
                            _ -> (S.empty, S.empty)
                in mconcat (map siteC (sitesOf ec k) ++ [g, collect k (tRes r)])
    (ev0, evc) = mconcat (map perStep [0 .. ecNS ec - 1])
    (ev1, evs) = mconcat [ one k i | (k, i) <- S.toList evc ]
    one k i =
        let s = site ec k i
            js = snd (gOf (sR s))
            y = yof ec k i
            condReads c = case c of
                MLit j -> S.singleton (k, j)
                MG _ js2 -> S.fromList [ (k, j) | j <- js2 ]
                _ -> S.empty
            parts = [ if j `elem` y then (condReads (sCond (site ec k j)), S.singleton (k, j)) else (S.singleton (k, j), S.empty) | j <- js ]
        in mconcat (parts ++ [(condReads (sCond s), S.empty)])

-- | A site's R (absent: no reads).
gOf :: Maybe MX -> (Int, [Int])
gOf r = case r of
    Nothing -> (0, [])
    Just (MG n js) -> (n, js)
    Just e -> error ("schedlean: R is not a G: " ++ mxRepr e)

rcong :: MX -> String
rcong e = case e of
    MZ -> "rfl"; MK _ -> "rfl"; MXarg -> "rfl"; MTrue -> "rfl"; MFalse -> "rfl"; MGK _ -> "rfl"
    MLit j -> "congrArg ops.toB (h " ++ show j ++ " (by decide))"
    MF n js -> "congrArg (ops.f " ++ show n ++ ") (" ++ tupcong [ "h " ++ show j ++ " (by decide)" | j <- js ] ++ ")"
    MG n js -> "congrArg (ops.g " ++ show n ++ ") (" ++ tupcong [ "h " ++ show j ++ " (by decide)" | j <- js ] ++ ")"
    _ -> error ("schedlean: rcong: " ++ mxRepr e)

rcongK :: EC -> Int -> MX -> String
rcongK ec k e = case e of
    MSplit is a b ->
        let rs = andc [ "congrArg (ops.g " ++ show n ++ ") (" ++ tupcong [ "h " ++ show j ++ " (by decide)" | j <- js ] ++ ")"
                      | i <- is, let (n, js) = gOf (sR (site ec k i)) ]
        in "cond_congr (" ++ rs ++ ") (" ++ rcongK ec k a ++ ") (" ++ rcongK ec k b ++ ")"
    _ -> rcong e

sqLeaf :: EC -> Int -> MSite -> String
sqLeaf ec k st
    | sKind st == "task" = fmt "fun x => ⟨fun ρ ρ' h => ⟨%s, %s⟩, %s⟩" [rcongK ec k (sCond st), tupcong (map (rcongK ec k) (fromMaybe [] (sArgs st))), nofx]
    | sCond st == MTrue && sArg st == MZ = "fun x => ⟨sfr_const _ _, fun _ _ => rfl⟩"
    | otherwise = fmt "fun x => ⟨fun ρ ρ' h => ⟨%s, %s⟩, %s⟩" [rcongK ec k (sCond st), rcongK ec k (sArg st), if sArg st == MZ then "fun _ _ => rfl" else nofx]
  where nofx = "fun h => absurd h (by decide)"

emitSiteProofs :: EC -> W ()
emitSiteProofs ec = do
    let m = ecM ec
        kinds = ecKinds ec
        kdOf = kd ec
        nS = ecNS ec
        kn k = fromMaybe 0 (elemIndex k kinds)
    w "/-! ## Site facts: reads before, dummy arguments -/\n"
    -- a synthesized kind's codes as one balanced table (a case per code made each query linear in its ports)
    let fxCodes k = [ kdCode (kdOf k) p | p <- kdPorts (kdOf k), kdFx (kdOf k) p ]
        fxCase k | isSynthKind k = "  | " ++ show (kn k) ++ ", c => fxT_" ++ k ++ ".get c\n"
                 | otherwise = concat [ "  | " ++ show (kn k) ++ ", " ++ show c ++ " => true\n" | c <- fxCodes k ]
    forM_ [ k | k <- kinds, isSynthKind k ] $ \k ->
        w ("def fxT_" ++ k ++ " : Tab Bool :=\n  " ++ tabOr "false" [ if kdFx (kdOf k) p then "true" else "false" | p <- kdPorts (kdOf k) ])
    w ("def fxF (κ c : Nat) : Bool := match κ, c with\n"
       ++ concatMap fxCase kinds
       ++ "  | _, _ => false\ndef kindN : Kind → Nat\n" ++ concat [ "  | ." ++ k ++ " => " ++ show (kn k) ++ "\n" | k <- kinds ]
       ++ "def codeK : (κ : Kind) → (KI κ).M → Nat\n")
    forM_ kinds $ \k -> case k of
        "reg" -> w "  | .reg => fun m => match m with | .rd _ => 0 | .wr _ => 1"
        "tasks" -> w "  | .tasks => fun _ => 0"
        _ | isSynthKind k -> w ("  | ." ++ k ++ " => fun m => m.val")
        _ -> w ("  | ." ++ k ++ " => fun m => match m with " ++ unwords [ "| ." ++ pn p ++ " => " ++ show (kdCode (kdOf k) p) | p <- kdPorts (kdOf k) ])
    w ("def npK : Kind → Nat\n" ++ concat [ "  | ." ++ k ++ " => " ++ show (if k == "tasks" then 1 else length (kdPorts (kdOf k))) ++ "\n" | k <- kinds ]
       ++ "def offK : Kind → Nat\n" ++ concat [ "  | ." ++ k ++ " => " ++ show (mOff m M.! k) ++ "\n" | k <- kinds ]
       ++ "def baseOf (j : I) : Nat := offK j.1 + j.2.val * npK j.1\n\
\def fxP (p : Σ j : I, (CI j).M) : Bool := fxF (kindN p.1.1) (codeK p.1.1 p.2)\n\
\\n\
\section\n\
\variable {V : Type}\n\
\/-- Site `i` of step `k`: it reads only earlier sites, and a flagged port's call carries `z`. -/\n\
\def SQ (ops : Ops V) (E : EqC V) (k i : Nat) (F : (V : Type) → Ops V → EqC V → V → SF V) : Prop :=\n\
\  ∀ x : V, SFReads i (F V ops E x) ∧ (fxP ((keyT.get k).get i) = true → ∀ ρ, (F V ops E x).2 ρ = ops.z)\n\
\def SQT (ops : Ops V) (E : EqC V) (k : Nat) (t : Tab ((V : Type) → Ops V → EqC V → V → SF V)) : Prop :=\n\
\  Tab.AllR (SQ ops E k) t 0 (kT.get k)\n")
    forM_ [0 .. nS - 1] $ \k -> do
        body <- case sitesOf ec k of
            [] -> return "fun _ _ h => absurd h (Nat.not_lt_zero _)"
            ss -> stabproof ("sf_" ++ show k) ("sq_" ++ show k) "(ops : Ops V) (E : EqC V)" "ops E"
                            (\nm lo hi -> fmt "Tab.AllR (SQ ops E %d) %s %d %d" [show k, nm, show lo, show hi]) (map (sqLeaf ec k) ss) True
        w (fmt "theorem sq_%d (ops : Ops V) (E : EqC V) : SQT ops E %d sf_%d :=\n  %s" [show k, show k, show k, body])
    prf <- stabproof "sfT" "sqT" "(ops : Ops V) (E : EqC V)" "ops E" (\nm lo hi -> fmt "Tab.AllR (SQT ops E) %s %d %d" [nm, show lo, show hi])
                     [ "sq_" ++ show k ++ " ops E" | k <- [0 .. nS - 1] ] True
    w ("theorem sqT (ops : Ops V) (E : EqC V) : Tab.AllR (SQT ops E) sfT 0 nS :=\n  " ++ prf)
    w (fmt envText [ecWXM ec] ++ (if ecMet ec then trMText else "") ++ "end\n")
    cut
  where pn (PS s) = s
        pn (PT t) = show t

emitValues :: EC -> W ()
emitValues ec = do
    let (evneed, evcneed, evsneed) = evNeeds ec
        hyp = ecHyp ec
        hya = ecHya ec
        nR = ecNR ec
        nS = ecNS ec
        met = ecMet ec
    w "section\nvariable {V : Type}\n"
    forM_ (S.toAscList evneed) $ \(k, i) ->
        w (fmt "theorem ev_%d_%d (ops : Ops V) (E : EqC V) (v : Nat → V) (oc : OC V) (x : V) : envS ops E v oc %d x %d = %s :=\n  (envS_at ops E v oc %d (by decide) x %d (by decide)).trans rfl"
               [show k, show i, show k, show i, ocread ec (sitePort ec k i), show k, show i])
    forM_ (S.toAscList evcneed) $ \(k, i) -> do
        let s = site ec k i
            c = (vctx "v") { cX = "x" }
        w (fmt "theorem evc_%d_%d (ops : Ops V) (E : EqC V) (v : Nat → V) (oc : OC V) (x : V) (h : %s = true) : envS ops E v oc %d x %d = %s :=\n  (envS_at ops E v oc %d (by decide) x %d (by decide)).trans (if_pos ((%s).trans h))"
               [show k, show i, condHw ec k s, show k, show i, ocread ec (sitePort ec k i), show k, show i, val ec k (sCond s) c Nothing])
    forM_ (S.toAscList evsneed) $ \(k, i) -> do
        let s = site ec k i
            c = (vctx "v") { cX = "x" }
        w (fmt "theorem evs_%d_%d (ops : Ops V) (E : EqC V) (v : Nat → V) (oc : OC V) (x : V) : envS ops E v oc %d x %d = %s :=\n  (envS_at ops E v oc %d (by decide) x %d (by decide)).trans (ite_bcongr (%s) _ _)"
               [show k, show i, show k, show i, svHw ec k i ("v " ++ show i), show k, show i, val ec k (sCond s) c Nothing])
    w "end\n"
    w ("section\nvariable {V : Type}\n\
\theorem res_some (z : V) {b : Bool} (h : b = true) : (bif b then some z else none).isSome = true := by\n\
\  rw [h]; rfl\n\
\theorem res_eq {b : Bool} {r r' : V} (hb : b = true) (hr : r = r') : (bif b then some r else none) = some r' := by\n\
\  subst hb hr; rfl\n\
\/-- A step's run-time precondition: a rule fires, a method can fire. -/\n\
\def PRE (ops : Ops V) (E : EqC V) (oc : OC V) (k : Nat) : Bool := bif Nat.blt k nR then WF ops E oc k else CF ops E oc k\n\
\theorem pre_r {ops : Ops V} {E : EqC V} {oc : OC V} {r : Fin nR} (h : WF ops E oc r.val = true) : PRE ops E oc r.val = true := by\n\
\  unfold PRE; rw [show Nat.blt r.val nR = true from Nat.blt_eq.mpr r.isLt]; exact h\n"
       ++ (if met then "theorem pre_m {ops : Ops V} {E : EqC V} {oc : OC V} {m : Fin nM} (h : CF ops E oc (nR + m.val) = true) :\n\
\    PRE ops E oc (nR + m.val) = true := by\n\
\  unfold PRE; rw [show Nat.blt (nR + m.val) nR = false from Bool.eq_false_iff.mpr (fun h' => absurd (Nat.blt_eq.mp h') (Nat.not_lt.mpr (Nat.le_add_right nR m.val)))]; exact h\n" else "")
       ++ fmt "/-- Rule `k`'s result is defined when it fires (its guard read at its position). -/\n\
\def RQ (ops : Ops V) (E : EqC V) %s (k : Nat) (g : (V : Type) → Ops V → EqC V → OC V → Bool) : Prop :=\n\
\  ∀ (v : Nat → V) (oc : OC V), PRE ops E oc k = true → ((resT.get k) V ops E ops.z (envR ops E v oc k)).isSome = true\n" [hyp])
    let hgTerm k hf = let g = if k < nR then "g_of_cf (cf_of_wf " ++ hf ++ ")" else "g_of_cf (show CF ops E oc " ++ show k ++ " = true from " ++ hf ++ ")"
                      in if null (ecRDC ec ! k) then g else "((Bool.and_eq_true _ _).mp (" ++ g ++ ")).1"
        rqLeaf k = case tGuard (step ec k) of
            Just g | mxKind g `notElem` ["check", "false"] ->
                "fun v oc hf => res_some _ ((" ++ val ec k g (vctx "v") Nothing ++ ").trans (" ++ hgTerm k "hf" ++ "))"
            _ -> "fun _ _ _ => rfl"
    prf <- stabproof "gT" "rqT" ("(ops : Ops V) (E : EqC V) " ++ hyp) ("ops E " ++ hya) (\nm lo hi -> fmt "Tab.AllR (RQ ops E %s) %s %d %d" [hya, nm, show lo, show hi])
                     [ fmt "(%s : RQ ops E %s %d G_%d)" [rqLeaf k, hya, show k, show k] | k <- [0 .. nS - 1] ] True
    w (fmt "theorem rqT (ops : Ops V) (E : EqC V) %s : Tab.AllR (RQ ops E %s) gT 0 nS :=\n  %s" [hyp, hya, prf])
    w (fmt "theorem resR_ok (ops : Ops V) (E : EqC V) %s (u : St → Nat → V) (oc : OC V) (w : %s) (r : Fin nR)\n\
\    (hf : fire ops E oc w r = true) : (trR (P ops E u) oc r).2.isSome = true := by\n\
\  rw [trR_eq]\n\
\  exact Tab.get_allR _ 0 nS (rqT ops E %s) r.val (Nat.zero_le _) (rlt r) (u (.inr r)) oc (pre_r hf)\n" [hyp, ecWXM ec, hya])
    when met $ do
        w (fmt "/-- Method `m`'s result: its result expression, when it can fire. -/\n\
\def RQM (ops : Ops V) (E : EqC V) %s (m : Nat) (_n : Nat) : Prop :=\n\
\  ∀ (v : Nat → V) (oc : OC V) (x : V), CF ops E oc (nR + m) = true →\n\
\    (resT.get (nR + m)) V ops E x (envS ops E v oc (nR + m) x) = some ((retT.get m) V ops E oc)\n" [hyp])
        stab "mIdx" "Tab Nat" [ show mi | mi <- [0 .. ecNM ec - 1] ]
        let rqmLeaf mi = let k = nR + mi
                             c = VCtx "v" "x" "hf"
                         in fmt "fun v oc x hf => res_eq ((%s).trans (%s)) (%s)"
                                [val ec k (fromMaybe MTrue (tGuard (step ec k))) c Nothing, hgTerm k "hf", val ec k (tRes (step ec k)) c Nothing]
        w (fmt "theorem rqM (ops : Ops V) (E : EqC V) %s : Tab.AllR (RQM ops E %s) mIdx 0 nM :=\n  %s"
               [hyp, hya, tabproof [ fmt "(%s : RQM ops E %s %d %d)" [rqmLeaf mi, hya, show mi, show mi] | mi <- [0 .. ecNM ec - 1] ]])
        when (exported ec) $ w (cfArText ec)
        w (fmt "theorem cf_of_avail {ops : Ops V} {E : EqC V} {u : St → Nat → V} {oc : OC V} {w : Wires (Fin nM) V} {m : Fin nM}\n\
\    (h : Avail (P ops E u) oc w m) : CF ops E oc (nR + m.val) = true :=\n\
\  %s\n\
\theorem resM_ok" [if exported ec then "h.1.elim (fun h' => cf_ar ops E oc m h') id" else "h.1.resolve_left (fun h' => by cases h')"]
           ++ fmt " (ops : Ops V) (E : EqC V) %s (u : St → Nat → V) (oc : OC V) (w : Wires (Fin nM) V) (m : Fin nM)\n\
\    (h : Avail (P ops E u) oc w m) : (trM (P ops E u) oc w m).2 = some (((P ops E u).comb oc w).2.ret m) := by\n\
\  rw [trM_eq]\n\
\  exact Tab.get_allR _ 0 nM (rqM ops E %s) m.val (Nat.zero_le _) m.isLt (u (.inl m)) oc (w.arg m) (cf_of_avail h)\n" [hyp, hya])
    w "end\n"
    cut

emitHQ :: EC -> W ()
emitHQ ec = do
    let hyp = ecHyp ec
        hya = ecHya ec
        nS = ecNS ec
        both f = let a = f (VCtx "v" "x" "hf"); b = f (VCtx "v'" "x" "hf")
                 in if a == "rfl" then "rfl" else "(" ++ a ++ ").trans (" ++ b ++ ").symm"
        hqLeaf k st
            | isUncond st && sKind st /= "task" && mxKind (sArg st) `elem` ["z", "k", "x"] = "fun _ _ _ _ _ => ⟨rfl, fun _ => rfl⟩"
            | otherwise = fmt "fun oc v v' x hf => ⟨%s, fun _ => %s⟩" [both (condVal ec k st), both (argVal ec k st)]
    w (fmt "/-! ## Each step's calls at every skipped value (`CertU.trU`) -/\n\
\\n\
\section\n\
\variable {V : Type}\n\
\/-- Site `i` of step `k`, when the step may run, agrees at any two skipped values of its step:\n\
\    its condition, and its argument where taken. -/\n\
\def HQ (ops : Ops V) (E : EqC V) %s (k i : Nat) (F : (V : Type) → Ops V → EqC V → V → SF V) : Prop :=\n\
\  ∀ (oc : OC V) (v v' : Nat → V) (x : V), PRE ops E oc k = true →\n\
\    (F V ops E x).1 (envS ops E v oc k x) = (F V ops E x).1 (envS ops E v' oc k x) ∧\n\
\    ((F V ops E x).1 (envS ops E v oc k x) = true → (F V ops E x).2 (envS ops E v oc k x) = (F V ops E x).2 (envS ops E v' oc k x))\n\
\def HQT (ops : Ops V) (E : EqC V) %s (k : Nat) (t : Tab ((V : Type) → Ops V → EqC V → V → SF V)) : Prop :=\n\
\  Tab.AllR (HQ ops E %s k) t 0 (kT.get k)\n" [hyp, hyp, hya])
    forM_ [0 .. nS - 1] $ \k -> do
        body <- case sitesOf ec k of
            [] -> return "fun _ _ h => absurd h (Nat.not_lt_zero _)"
            ss -> stabproof ("sf_" ++ show k) ("hq_" ++ show k) ("(ops : Ops V) (E : EqC V) " ++ hyp) ("ops E " ++ hya)
                            (\nm lo hi -> fmt "Tab.AllR (HQ ops E %s %d) %s %d %d" [hya, show k, nm, show lo, show hi]) (map (hqLeaf k) ss) True
        w (fmt "theorem hq_%d (ops : Ops V) (E : EqC V) %s : HQT ops E %s %d sf_%d :=\n  %s" [show k, hyp, hya, show k, show k, body])
    prf <- stabproof "sfT" "hqT" ("(ops : Ops V) (E : EqC V) " ++ hyp) ("ops E " ++ hya) (\nm lo hi -> fmt "Tab.AllR (HQT ops E %s) %s %d %d" [hya, nm, show lo, show hi])
                     [ "hq_" ++ show k ++ " ops E " ++ hya | k <- [0 .. nS - 1] ] True
    w (fmt "theorem hqT (ops : Ops V) (E : EqC V) %s : Tab.AllR (HQT ops E %s) sfT 0 nS :=\n  %s" [hyp, hya, prf])
    w (fmt "/-- A step that may run makes the same calls at any two skipped values of its step. -/\n\
\theorem trUS (ops : Ops V) (E : EqC V) %s (oc : OC V) (k : Nat) (hk : k < nS) (v v' : Nat → V) (x : V)\n\
\    (hf : PRE ops E oc k = true) :\n\
\    callsOf (envS ops E v oc k x) (sitesF (siteS ops E k x) (kR k)) = callsOf (envS ops E v' oc k x) (sitesF (siteS ops E k x) (kR k)) :=\n\
\  callsOf_fn_congr _ _ _ _ (fun i hi =>\n\
\    Tab.get_allR _ 0 (kR k) (Tab.get_allR _ 0 nS (hqT ops E %s) k (Nat.zero_le _) hk) i (Nat.zero_le _) hi oc v v' x hf)\n\
\end\n" [hyp, hya])
    cut

-- | Readiness of each taken guarded site (only where some guarded site is conditional).
emitRY :: EC -> Bool -> W ()
emitRY ec needuc = do
    let hyp = ecHyp ec
        hya = ecHya ec
        nS = ecNS ec
        nR = ecNR ec
        rdyG (k', _, p) = not (kdAlwaysRdy (kd ec k') p)
        ryLeaf k i st
            | not (rdyG (sPort st)) = "fun _ _ _ _ _ _ ha => absurd ha (by decide)"
            | isUncond st = fmt "fun oc v x _ hf _ ha => rd_at (%s) %d (by decide) ha (by decide)" [hrd, show i]
            | sKind st == "read" =
                let lsite = case sCond st of
                        MLit j -> j
                        _ -> error ("schedlean: a ready-guarded read in step " ++ show k ++ " under a condition that is not one literal (not emitted yet)")
                    (kk, ii, mm) = sPort st
                    (lk, _, lm) = sitePort ec k lsite
                in fmt "fun oc v x hal hf hc ha => ((hal (%s %d)).2 %s %s rfl).trans ((congrArg ops.toB (%s)).symm.trans hc)"
                       [ihelper kk, show ii, lportOf ec kk mm, lportOf ec lk lm, evt ec k lsite ((vctx "v") { cX = "x" })]
            | otherwise =
                let rdc = ecRDC ec ! k
                    n = fromMaybe 0 (elemIndex i rdc)
                    gpart = "((Bool.and_eq_true _ _).mp (g_of_cf " ++ (if k < nR then "(cf_of_wf hf)" else "hf") ++ ")).2"
                in fmt "fun oc v x _ hf hc ha => rdy_of_rdc (%s) hc (%s)" [andget gpart (length rdc + 1) n, condVal ec k st (VCtx "v" "x" "hf")]
          where hrd = if k < nR then "rd_of_cf (cf_of_wf hf)" else "rd_of_cf hf"
    when needuc $ do
        w (fmt "section\n\
\variable {V : Type}\n\
\theorem rd_at {oc : OC V} {k : Nat} (h : RD oc k = true) (i : Nat) (hi : i < kR k)\n\
\    (ha : alwaysRdyB ((keyT.get k).get i) = false) (hu : (ucT.get k).testBit i = false) :\n\
\    (oc ((keyT.get k).get i).1).rdy ((keyT.get k).get i).2 = true := by\n\
\  unfold RD at h\n\
\  have h2 := List.all_eq_true.mp h i (List.mem_range'.mpr ⟨i, hi, by omega⟩)\n\
\  simp only [ha, hu, Bool.false_or] at h2\n\
\  exact h2\n\
\theorem rdy_of_rdc {c c' r : Bool} (h : (!c || r) = true) (hc : c' = true) (e : c' = c) : r = true := by\n\
\  subst e; rw [hc] at h; simpa using h\n\
\/-- A taken guarded site of step `k` is ready: from CAN_FIRE (unconditional), from the guard's\n\
\    lifted readiness (a conditional call), or from the child's alias (a readiness read). -/\n\
\def RY (ops : Ops V) (E : EqC V) %s (k i : Nat) (F : (V : Type) → Ops V → EqC V → V → SF V) : Prop :=\n\
\  ∀ (oc : OC V) (v : Nat → V) (x : V), (∀ j, AliasOK ops.toB (CI j).ann (oc j)) → PRE ops E oc k = true →\n\
\    (F V ops E x).1 (envS ops E v oc k x) = true → alwaysRdyB ((keyT.get k).get i) = false →\n\
\    (oc ((keyT.get k).get i).1).rdy ((keyT.get k).get i).2 = true\n\
\def RYT (ops : Ops V) (E : EqC V) %s (k : Nat) (t : Tab ((V : Type) → Ops V → EqC V → V → SF V)) : Prop :=\n\
\  Tab.AllR (RY ops E %s k) t 0 (kT.get k)\n" [hyp, hyp, hya])
        forM_ [0 .. nS - 1] $ \k -> do
            body <- case sitesOf ec k of
                [] -> return "fun _ _ h => absurd h (Nat.not_lt_zero _)"
                ss -> stabproof ("sf_" ++ show k) ("ry_" ++ show k) ("(ops : Ops V) (E : EqC V) " ++ hyp) ("ops E " ++ hya)
                                (\nm lo hi -> fmt "Tab.AllR (RY ops E %s %d) %s %d %d" [hya, show k, nm, show lo, show hi])
                                [ ryLeaf k i st | (i, st) <- zip [0 ..] ss ] True
            w (fmt "theorem ry_%d (ops : Ops V) (E : EqC V) %s : RYT ops E %s %d sf_%d :=\n  %s" [show k, hyp, hya, show k, show k, body])
        prf <- stabproof "sfT" "ryT" ("(ops : Ops V) (E : EqC V) " ++ hyp) ("ops E " ++ hya) (\nm lo hi -> fmt "Tab.AllR (RYT ops E %s) %s %d %d" [hya, nm, show lo, show hi])
                         [ "ry_" ++ show k ++ " ops E " ++ hya | k <- [0 .. nS - 1] ] True
        w (fmt "theorem ryT (ops : Ops V) (E : EqC V) %s : Tab.AllR (RYT ops E %s) sfT 0 nS :=\n  %s" [hyp, hya, prf])
        w (fmt "theorem rdyS (ops : Ops V) (E : EqC V) %s (oc : OC V) (k : Nat) (hk : k < nS) (v : Nat → V) (x : V)\n\
\    (hal : ∀ j, AliasOK ops.toB (CI j).ann (oc j)) (hf : PRE ops E oc k = true) :\n\
\    ∀ c ∈ callsOf (envS ops E v oc k x) (sitesF (siteS ops E k x) (kR k)),\n\
\      (CI c.1).ann.alwaysRdy c.2.1 = false → (oc c.1).rdy c.2.1 = true := by\n\
\  intro c hc ha\n\
\  obtain ⟨s, hs, hcs, rfl⟩ := mem_callsOf_inv _ _ c hc\n\
\  obtain ⟨i, hi, rfl⟩ := mem_sitesF hs\n\
\  exact Tab.get_allR _ 0 (kR k) (Tab.get_allR _ 0 nS (ryT ops E %s) k (Nat.zero_le _) hk) i (Nat.zero_le _) hi oc v x hal hf hcs ha\n\
\end\n" [hyp, hya])
    cut

-- | An exported module's always-ready methods: CAN_FIRE is true in every cycle, its guard the constant
-- and its reads always ready (by computation), so `Avail` needs no RDY (design.md §3.1).
cfArText :: EC -> String
cfArText ec =
    let xs = zip [0 :: Int ..] (maybe [] exMeths (ecExport ec))
    in concat [ fmt "theorem cf_ar_%d (ops : Ops V) (E : EqC V) (oc : OC V) : CF ops E oc (nR + %d) = true := by\n  unfold CF RD; rfl\n" [show i, show i]
              | (i, x) <- xs, xmAR x ]
       ++ "def arT : Tab Unit :=\n  " ++ tab (map (const "()") xs) ++ "\n\
\/-- An always-ready method can fire in every cycle: a balanced tree of proofs shaped as `arT`, one per method\n\
\    (cost review r9b-3; a decided membership in the list of them grew faster than the methods). -/\n\
\theorem cf_ar (ops : Ops V) (E : EqC V) (oc : OC V) (m : Fin nM) (h : xAnn.alwaysRdy m = true) : CF ops E oc (nR + m.val) = true :=\n\
\  Tab.finAll (Q := fun m : Fin nM => xAnn.alwaysRdy m = true → CF ops E oc (nR + m.val) = true) mM mM_val arT\n    ("
       ++ tabproofI "    " [ if xmAR x then "fun _ => cf_ar_" ++ show i ++ " ops E oc" else "fun h => absurd h (by decide)" | (i, x) <- xs ]
       ++ ") m h\n"

emitChecks :: EC -> W ()
emitChecks ec = do
    let wxm = ecWXM ec
    w "/-! ## Decided checks over closed tables -/\n"
    w "/-- A check over each step's sites (steps without sites pass). -/\n\
\theorem sitesB_spec (f : Nat → Nat → (Σ j : I, (CI j).M) → Bool)\n\
\    (h : Tab.allB (fun k keys => Nat.beq (kT.get k) 0 || Tab.allB (f k) keys 0 (kT.get k)) keyT 0 nS = true)\n\
\    (k : Nat) (hk : k < nS) : ∀ i, i < kR k → f k i ((keyT.get k).get i) = true := by\n\
\  intro i hi\n\
\  have h1 := Tab.allB_spec _ keyT 0 nS h k (Nat.zero_le _) hk\n\
\  simp only [Bool.or_eq_true] at h1\n\
\  rcases h1 with h1 | h1\n\
\  · have := Nat.eq_of_beq_eq_true h1; simp only [kR] at hi; omega\n\
\  · exact Tab.allB_spec _ _ 0 (kT.get k) h1 i (Nat.zero_le _) hi\n"
    allBThm "chkAll" "fun k keys => Nat.beq (kT.get k) 0 ||\n    Tab.allB (fun i p => (idxT (usesAt p) k).contains i) keys 0 (kT.get k)" "keyT" "nS"
    w ("theorem hchkT (k : Nat) (hk : k < nS) : ∀ i, i < kR k → i ∈ idxT (usesAt ((keyT.get k).get i)) k := by\n\
\  intro i hi\n\
\  have h := sitesB_spec (fun k i p => (idxT (usesAt p) k).contains i) chkAll k hk i hi\n\
\  simpa using h\n\
\theorem hchk (r : Fin nR) : ∀ i, i < kR r.val → i ∈ idx (.inr r) ((keyT.get r.val).get i) :=\n\
\  hchkT r.val (rlt r)\n"
       ++ (if ecMet ec then "theorem hchkM (m : Fin nM) : ∀ i, i < kR (nR + m.val) → i ∈ idx (.inl m) ((keyT.get (nR + m.val)).get i) :=\n\
\  hchkT (nR + m.val) (mlt m)\n" else ""))
    w (fmt "section\nvariable {V : Type}\n\ntheorem onIff (ops : Ops V) (E : EqC V) (u : St → Nat → V) (oc : OC V) (w : %s) :\n\
\    ∀ s, On (P ops E u) (fire ops E) oc w s ↔ %s = true := by\n\
\  intro s; cases s with\n\
\  | inl e => %s\n\
\  | inr r => exact Iff.rfl\n" [wxm, if ecMet ec then "stepOn ops E oc w s" else "stepOn ops E oc s", if ecMet ec then "exact Iff.rfl" else "exact e.elim"]
       ++ fmt callsText (replicate 6 wxm) ++ (if ecMet ec then methCallsText else "") ++ "end\n")

envText :: String
envText = "theorem sqR (ops : Ops V) (E : EqC V) (k : Nat) (hk : k < nS) : Tab.AllR (SQ ops E k) (sfT.get k) 0 (kR k) :=\n\
\  Tab.get_allR _ 0 nS (sqT ops E) k (Nat.zero_le _) hk\n\
\theorem hOK (ops : Ops V) (E : EqC V) (k : Nat) (hk : k < nS) (x : V) : ∀ i, i < kR k → ReadsBefore i (siteS ops E k x i) :=\n\
\  fun i hi => ((Tab.get_allR _ 0 (kR k) (sqR ops E k hk) i (Nat.zero_le _) hi) x).1\n\
\\n\
\/-- The environment of step `k`'s sites at argument `x`, the hardware's values. -/\n\
\abbrev envS (ops : Ops V) (E : EqC V) (v : Nat → V) (oc : OC V) (k : Nat) (x : V) : Nat → V :=\n\
\  envHw v oc (sitesF (siteS ops E k x) (kR k)) 0 v\n\
\abbrev envR (ops : Ops V) (E : EqC V) (v : Nat → V) (oc : OC V) (k : Nat) : Nat → V := envS ops E v oc k ops.z\n\
\\n\
\theorem trR_eq (ops : Ops V) (E : EqC V) (u : St → Nat → V) (oc : OC V) (r : Fin nR) :\n\
\    trR (P ops E u) oc r = (callsOf (envR ops E (u (.inr r)) oc r.val) (sitesF (siteR ops E r.val) (kR r.val)),\n\
\      (resT.get r.val) V ops E ops.z (envR ops E (u (.inr r)) oc r.val)) :=\n\
\  trace_fn (u (.inr r)) oc _ (siteR ops E r.val) (kR r.val) (hOK ops E r.val (rlt r) ops.z)\n\
\theorem trS_eq (ops : Ops V) (E : EqC V) (u : St → Nat → V) (oc : OC V) (w : %s) (r : Fin nR) :\n\
\    trS (P ops E u) oc w (.inr r) = callsOf (envR ops E (u (.inr r)) oc r.val) (sitesF (siteR ops E r.val) (kR r.val)) :=\n\
\  congrArg Prod.fst (trR_eq ops E u oc r)\n\
\theorem envS_at (ops : Ops V) (E : EqC V) (v : Nat → V) (oc : OC V) (k : Nat) (hk : k < nS) (x : V) (i : Nat) (hi : i < kR k) :\n\
\    envS ops E v oc k x i = if (siteS ops E k x i).cond (envS ops E v oc k x) then (oc (siteS ops E k x i).j).ret (siteS ops E k x i).m else v i :=\n\
\  envHw_fn v oc (siteS ops E k x) (kR k) (hOK ops E k hk x) i hi\n\
\theorem envR_at (ops : Ops V) (E : EqC V) (v : Nat → V) (oc : OC V) (k : Nat) (hk : k < nS) (i : Nat) (hi : i < kR k) :\n\
\    envR ops E v oc k i = if (siteR ops E k i).cond (envR ops E v oc k) then (oc (siteR ops E k i).j).ret (siteR ops E k i).m else v i :=\n\
\  envS_at ops E v oc k hk ops.z i hi\n"

trMText :: String
trMText = "theorem trM_eq (ops : Ops V) (E : EqC V) (u : St → Nat → V) (oc : OC V) (w : Wires (Fin nM) V) (m : Fin nM) :\n\
\    trM (P ops E u) oc w m = (callsOf (envS ops E (u (.inl m)) oc (nR + m.val) (w.arg m)) (sitesF (siteS ops E (nR + m.val) (w.arg m)) (kR (nR + m.val))),\n\
\      (resT.get (nR + m.val)) V ops E (w.arg m) (envS ops E (u (.inl m)) oc (nR + m.val) (w.arg m))) :=\n\
\  trace_fn (u (.inl m)) oc _ (siteS ops E (nR + m.val) (w.arg m)) (kR (nR + m.val)) (hOK ops E (nR + m.val) (mlt m) (w.arg m))\n\
\theorem trSM_eq (ops : Ops V) (E : EqC V) (u : St → Nat → V) (oc : OC V) (w : Wires (Fin nM) V) (m : Fin nM) :\n\
\    trS (P ops E u) oc w (.inl m) = callsOf (envS ops E (u (.inl m)) oc (nR + m.val) (w.arg m)) (sitesF (siteS ops E (nR + m.val) (w.arg m)) (kR (nR + m.val))) :=\n\
\  congrArg Prod.fst (trM_eq ops E u oc w m)\n"

callsText :: String
callsText = "\n\
\/-- A rule's site on a port: the rule calls the port when the site's condition holds. -/\n\
\theorem cR (ops : Ops V) (E : EqC V) (u : St → Nat → V) (oc : OC V) (w : %s) (r : Fin nR) (j : I) (cm : (CI j).M)\n\
\    (i0 : Nat) (hidx : idx (.inr r) ⟨j, cm⟩ = [i0]) (hi0 : Nat.blt i0 (kR r.val) = true)\n\
\    (hk0 : (keyT.get r.val).get i0 = ⟨j, cm⟩) (hc : (siteR ops E r.val i0).cond (envR ops E (u (.inr r)) oc r.val) = true) :\n\
\    Calls (P ops E u) (trS (P ops E u) oc w (.inr r)) j cm := by\n\
\  show ∃ x, (cm, x) ∈ proj j (trS (P ops E u) oc w (.inr r))\n\
\  rw [trS_eq]\n\
\  exact (calls_one _ (siteR ops E r.val) (kR r.val) (keyT.get r.val).get (fun _ => rfl) (idx (.inr r)) (hchk r) j cm i0\n\
\    hidx hi0 hk0).mpr hc\n\
\theorem cRk (ops : Ops V) (E : EqC V) (u : St → Nat → V) (oc : OC V) (w : %s) (r : Fin nR) (κ : Kind) (i : Nat)\n\
\    (hi : i < nk κ) (cm : (KI κ).M) (i0 : Nat) (hidx : idxT (((usesK κ).get i).uses cm) r.val = [i0])\n\
\    (hi0 : Nat.blt i0 (kR r.val) = true) (hk0 : (keyT.get r.val).get i0 = ⟨⟨κ, ⟨i, hi⟩⟩, cm⟩)\n\
\    (hc : (siteR ops E r.val i0).cond (envR ops E (u (.inr r)) oc r.val) = true) :\n\
\    Calls (P ops E u) (trS (P ops E u) oc w (.inr r)) ⟨κ, ⟨i, hi⟩⟩ cm :=\n\
\  cR ops E u oc w r ⟨κ, ⟨i, hi⟩⟩ cm i0 hidx hi0 hk0 hc\n\
\theorem aR (ops : Ops V) (E : EqC V) (u : St → Nat → V) (oc : OC V) (w : %s) (r : Fin nR) (j : I) (cm : (CI j).M)\n\
\    (i0 : Nat) (hidx : idx (.inr r) ⟨j, cm⟩ = [i0]) (v : V)\n\
\    (hv : (siteR ops E r.val i0).cond (envR ops E (u (.inr r)) oc r.val) = true → (siteR ops E r.val i0).arg (envR ops E (u (.inr r)) oc r.val) = v)\n\
\    (x : V) (hx : (cm, x) ∈ proj j (trS (P ops E u) oc w (.inr r))) : v = x := by\n\
\  rw [trS_eq] at hx\n\
\  exact arg_one _ (siteR ops E r.val) (kR r.val) (keyT.get r.val).get (fun _ => rfl) (idx (.inr r)) (hchk r) j cm i0 hidx v hv x hx\n\
\theorem aRk (ops : Ops V) (E : EqC V) (u : St → Nat → V) (oc : OC V) (w : %s) (r : Fin nR) (κ : Kind) (i : Nat)\n\
\    (hi : i < nk κ) (cm : (KI κ).M) (i0 : Nat) (hidx : idxT (((usesK κ).get i).uses cm) r.val = [i0]) (v : V)\n\
\    (hv : (siteR ops E r.val i0).cond (envR ops E (u (.inr r)) oc r.val) = true → (siteR ops E r.val i0).arg (envR ops E (u (.inr r)) oc r.val) = v)\n\
\    (x : V) (hx : (cm, x) ∈ proj (⟨κ, ⟨i, hi⟩⟩ : I) (trS (P ops E u) oc w (.inr r))) : v = x :=\n\
\  aR ops E u oc w r ⟨κ, ⟨i, hi⟩⟩ cm i0 hidx v hv x hx\n\
\theorem cRkIff (ops : Ops V) (E : EqC V) (u : St → Nat → V) (oc : OC V) (w : %s) (r : Fin nR) (κ : Kind)\n\
\    (i : Nat) (hi : i < nk κ) (cm : (KI κ).M) (i0 : Nat) (hidx : idxT (((usesK κ).get i).uses cm) r.val = [i0])\n\
\    (hi0 : Nat.blt i0 (kR r.val) = true) (hk0 : (keyT.get r.val).get i0 = ⟨⟨κ, ⟨i, hi⟩⟩, cm⟩) :\n\
\    Calls (P ops E u) (trS (P ops E u) oc w (.inr r)) ⟨κ, ⟨i, hi⟩⟩ cm ↔\n\
\      (siteR ops E r.val i0).cond (envR ops E (u (.inr r)) oc r.val) = true := by\n\
\  show (∃ x, (cm, x) ∈ proj (⟨κ, ⟨i, hi⟩⟩ : I) (trS (P ops E u) oc w (.inr r))) ↔ _\n\
\  rw [trS_eq]\n\
\  exact calls_one _ (siteR ops E r.val) (kR r.val) (keyT.get r.val).get (fun _ => rfl) (idx (.inr r)) (hchk r)\n\
\    (⟨κ, ⟨i, hi⟩⟩ : I) cm i0 hidx hi0 hk0\n\
\/-- A rule's call on a port was made: its site's condition held. -/\n\
\theorem aRkC (ops : Ops V) (E : EqC V) (u : St → Nat → V) (oc : OC V) (w : %s) (r : Fin nR) (κ : Kind) (i : Nat)\n\
\    (hi : i < nk κ) (cm : (KI κ).M) (i0 : Nat) (hidx : idxT (((usesK κ).get i).uses cm) r.val = [i0])\n\
\    (x : V) (hx : (cm, x) ∈ proj (⟨κ, ⟨i, hi⟩⟩ : I) (trS (P ops E u) oc w (.inr r))) :\n\
\    (siteR ops E r.val i0).cond (envR ops E (u (.inr r)) oc r.val) = true := by\n\
\  rw [trS_eq] at hx\n\
\  obtain ⟨i', hi', hc, -⟩ := mem_proj_fn _ (siteR ops E r.val) (kR r.val) (keyT.get r.val).get (fun _ => rfl) (idx (.inr r))\n\
\    (hchk r) (⟨κ, ⟨i, hi⟩⟩ : I) cm x hx\n\
\  have e : idx (.inr r) ⟨⟨κ, ⟨i, hi⟩⟩, cm⟩ = [i0] := hidx\n\
\  rw [e, List.mem_singleton] at hi'\n\
\  subst hi'\n\
\  exact hc\n"

methCallsText :: String
methCallsText = "/-- A method's site on a port, and its argument (the method's argument is `w.arg m`). -/\n\
\theorem cMk (ops : Ops V) (E : EqC V) (u : St → Nat → V) (oc : OC V) (w : Wires (Fin nM) V) (m : Fin nM) (κ : Kind) (i : Nat)\n\
\    (hi : i < nk κ) (cm : (KI κ).M) (i0 : Nat) (hidx : idxT (((usesK κ).get i).uses cm) (nR + m.val) = [i0])\n\
\    (hi0 : Nat.blt i0 (kR (nR + m.val)) = true) (hk0 : (keyT.get (nR + m.val)).get i0 = ⟨⟨κ, ⟨i, hi⟩⟩, cm⟩)\n\
\    (hc : (siteS ops E (nR + m.val) (w.arg m) i0).cond (envS ops E (u (.inl m)) oc (nR + m.val) (w.arg m)) = true) :\n\
\    Calls (P ops E u) (trS (P ops E u) oc w (.inl m)) ⟨κ, ⟨i, hi⟩⟩ cm := by\n\
\  show ∃ x, (cm, x) ∈ proj (⟨κ, ⟨i, hi⟩⟩ : I) (trS (P ops E u) oc w (.inl m))\n\
\  rw [trSM_eq]\n\
\  exact (calls_one _ (siteS ops E (nR + m.val) (w.arg m)) (kR (nR + m.val)) (keyT.get (nR + m.val)).get (fun _ => rfl)\n\
\    (idx (.inl m)) (hchkM m) (⟨κ, ⟨i, hi⟩⟩ : I) cm i0 hidx hi0 hk0).mpr hc\n\
\theorem aMk (ops : Ops V) (E : EqC V) (u : St → Nat → V) (oc : OC V) (w : Wires (Fin nM) V) (m : Fin nM) (κ : Kind) (i : Nat)\n\
\    (hi : i < nk κ) (cm : (KI κ).M) (i0 : Nat) (hidx : idxT (((usesK κ).get i).uses cm) (nR + m.val) = [i0]) (v : V)\n\
\    (hv : (siteS ops E (nR + m.val) (w.arg m) i0).cond (envS ops E (u (.inl m)) oc (nR + m.val) (w.arg m)) = true →\n\
\      (siteS ops E (nR + m.val) (w.arg m) i0).arg (envS ops E (u (.inl m)) oc (nR + m.val) (w.arg m)) = v)\n\
\    (x : V) (hx : (cm, x) ∈ proj (⟨κ, ⟨i, hi⟩⟩ : I) (trS (P ops E u) oc w (.inl m))) : v = x := by\n\
\  rw [trSM_eq] at hx\n\
\  exact arg_one _ (siteS ops E (nR + m.val) (w.arg m)) (kR (nR + m.val)) (keyT.get (nR + m.val)).get (fun _ => rfl)\n\
\    (idx (.inl m)) (hchkM m) (⟨κ, ⟨i, hi⟩⟩ : I) cm i0 hidx v hv x hx\n"
