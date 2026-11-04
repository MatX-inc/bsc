-- | Last-caller ports, enables and arguments per child instance (EAQ), the
-- certificate and the obligation (mke2e.py lines 3138-3606; C25, CertL).
module Lean.Sched.Emit.Cert
    ( emitLC, emitEAQ, emitCert
    ) where

import Control.Monad (forM_, forM, when, unless)
import Control.Monad.State.Strict (gets, modify', evalStateT, StateT)
import Data.Array ((!), listArray)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M ((!), findWithDefault, lookup, toList, fromList, fromListWith)

import Lean.Print (allL, cut, fmt, stab, stabproof, tabproofI, w, W)
import Lean.Sched.Emit.Arms (tuser)
import Lean.Sched.Emit.Ctx (argVal, condVal, entry, exported, isUncond, kd, lportOf, site, step, EC(ecMuxes, ecVB, ecKV,
    ecU, ecNR, ecSelmux, ecPortInfo, ecM, ecNT, ecKinds, ecHyp, ecHya, ecWXM, ecMet, ecLCP, ecNS, ecSynth),
    PInfo(piMargs, piArgk, piMpos, piQ, piMus), VCtx(VCtx))
import Lean.Sched.Export (isSynthKind, portFn, Export(exNS), SynthKid(skExport, skOff))
import Lean.Sched.Kinds (kdFx, KindDef(kdAlwaysRdy, kdPorts, kdHasEn), PM(..))
import Lean.Sched.Model (MPort, MSite(sPort, sHw), MStep(tSites), MX(MChk, MCfT), Model(mByk))

-- | The last-caller section (after the glue); returns hvR's text for the certificate.
emitLC :: EC -> W String
emitLC ec
    | null (ecLCP ec) = return ""
    | otherwise = do
        let m = ecM ec
            kinds = ecKinds ec
            lcp = ecLCP ec
            vb = ecVB ec
            met = ecMet ec
            w0 = if met then "⟨fun _ => false, fun _ => ops.z⟩" else "⟨fun e => e.elim, fun e => e.elim⟩"
            lckd = M.fromList [ ("reg", (".wr _", ".rd _", "regAll", "Reg.write", "Reg")), ("cfg", (".write", ".read", "cfgAll", "CfgM.write", "Cfg")) ]
        rows <- forM kinds $ \k -> do
            let insts = [ i | i <- [0 .. length (mByk m M.! k) - 1], (k, i, PS "write") `elem` lcp ]
            case M.lookup k lckd of
                Just (pat, oth, allk, lpw, tn) | not (null insts) -> do
                    stab ("vb" ++ tn ++ "T") "Tab Nat" [ show (M.findWithDefault 0 (k, i, PS "write") vb) | i <- [0 .. length (mByk m M.! k) - 1] ]
                    return ( fmt "  | .%s => fun i m => match m with | %s => [%s].contains i | %s => false" [k, pat, intercalate ", " (map show insts), oth]
                           , fmt "  | .%s => fun i _ => vb%sT.get i" [k, tn]
                           , [fmt "theorem vchk_%s : ∀ i, i < nk .%s → lcK .%s i %s = true →\n\
\    (((usesK .%s).get i).us %s).all (fun k => Nat.blt k nR && (virtT.get k).contains (vbK .%s i %s, κv, 0)) = true := by\n\
\  decide +kernel" [k, k, k, lpw, k, lpw, k, lpw]]
                           , fmt "  · revert hl\n\
\    exact %s (Q := fun cm => lcK .%s i cm = true → (((usesK .%s).get i).us cm).all\n\
\      (fun k => Nat.blt k nR && (virtT.get k).contains (vbK .%s i cm, κv, 0)) = true)\n\
\      (fun h => absurd h (by simp [lcK])) (vchk_%s i hi) cm" [allk, k, k, k, k]
                           , fmt "  · revert hl\n\
\    exact %s (Q := fun cm => lcK .%s i cm = true → (KI .%s).ann.sbr cm cm = true ∧ (KI .%s).ann.hasEn cm = true)\n\
\      (fun h => absurd h (by simp [lcK])) (fun _ => ⟨by decide, by decide⟩) cm" [allk, k, k, k] )
                _ -> do
                    unless (null insts) $ error "schedlean: a last-caller port of a kind without a last-caller model"
                    return ( "  | ." ++ k ++ " => fun _ _ => false", "  | ." ++ k ++ " => fun _ _ => 0", []
                           , "  · simp [lcJ, lcK] at hl", "  · simp [lcJ, lcK] at hl" )
        let col f = [ f r | r <- rows ]
            a1 (x, _, _, _, _) = x
            a2 (_, x, _, _, _) = x
            a3 (_, _, x, _, _) = x
            a4 (_, _, _, x, _) = x
            a5 (_, _, _, _, x) = x
        w ("/-! ## Last caller: the ports, their virtual scan slots, the order facts -/\n\
\\n\
\/-- The last-caller ports: self-SBR primitive write ports two callers may drive in one cycle. -/\n\
\def lcK : (κ : Kind) → Nat → (KI κ).M → Bool\n" ++ intercalate "\n" (col a1) ++ "\n\
\def lcJ (j : I) (cm : (CI j).M) : Bool := lcK j.1 j.2.val cm\n\
\/-- Each last-caller port's virtual slot (one per port, after the children's). -/\n\
\def vbK : (κ : Kind) → Nat → (KI κ).M → Nat\n" ++ intercalate "\n" (col a2) ++ "\n\
\def vbJ (j : I) (cm : (CI j).M) : Nat := vbK j.1 j.2.val cm\n\
\abbrev κv : Nat := " ++ show (ecKV ec) ++ "\n\
\theorem hkv : 0 ∈ (D.kc κv 0).cout := by decide +kernel\n" ++ intercalate "\n" (concat (col a3)) ++ "\n\
\theorem vchk (j : I) (cm : (CI j).M) (hl : lcJ j cm = true) :\n\
\    (usL ⟨j, cm⟩).all (fun k => Nat.blt k nR && (virtT.get k).contains (vbJ j cm, κv, 0)) = true := by\n\
\  obtain ⟨κ, i, hi⟩ := j\n\
\  cases κ\n" ++ intercalate "\n" (col a4) ++ "\n\
\theorem lcOk : ∀ (j : I) (cm : (CI j).M), lcJ j cm = true → (CI j).ann.sbr cm cm = true ∧ (CI j).ann.hasEn cm = true := by\n\
\  intro j cm hl\n\
\  obtain ⟨κ, i, hi⟩ := j\n\
\  cases κ\n" ++ intercalate "\n" (col a5) ++ "\n")
        return (fmt "section\n\
\variable {V : Type}\n\
\/-- A rule calling a last-caller port uses the port's virtual slot. -/\n\
\theorem hvR (ops : Ops V) (E : EqC V) (u : St → Nat → V) : ∀ (oc : OC V) (r : Fin nR) (j : I) (cm : (CI j).M),\n\
\    lcJ j cm = true → Calls (P ops E u) (trR (P ops E u) oc r).1 j cm →\n\
\    (vbJ j cm, κv, 0) ∈ D.fpR.get ((G ops E u).rk r) := by\n\
\  intro oc r j cm hl ⟨x, hx⟩\n\
\  have hus : (.inr r : St) ∈ users j cm :=\n\
\    usersOk ops E (.inr r) ⟨j, cm⟩ (fpOk ops E u oc %s (.inr r) ⟨j, (cm, x)⟩ (mem_proj j _ _ hx))\n\
\  obtain ⟨k, hk, e⟩ := List.mem_map.mp hus\n\
\  have h2 := List.all_eq_true.mp (vchk j cm hl) k hk\n\
\  simp only [Bool.and_eq_true] at h2\n\
\  have hkR : k < nR := Nat.blt_eq.mp h2.1\n\
\  %s\n\
\  have er : r.val = k := by rw [← Sum.inr.inj e]; exact Nat.mod_eq_of_lt hkR\n\
\  show (vbJ j cm, κv, 0) ∈ fpRT.get r.val\n\
\  have h := Tab.allB_spec _ fpRT 0 nR fpAll r.val (Nat.zero_le _) r.isLt\n\
\  rw [beq_iff_eq] at h\n\
\  rw [h, er]\n\
\  exact List.mem_append_right _ (List.contains_iff_mem.mp h2.2)\n" [w0, if met then "simp only [stOf, h2.1, cond_true] at e" else ""]
                ++ (if met then "/-- No method calls a last-caller port (its users are rules). -/\n\
\theorem hvM (ops : Ops V) (E : EqC V) (u : St → Nat → V) : ∀ (oc : OC V) (w : Wires (Fin nM) V) (m : Fin nM) (j : I)\n\
\    (cm : (CI j).M), lcJ j cm = true → Calls (P ops E u) (trM (P ops E u) oc w m).1 j cm →\n\
\    (vbJ j cm, κv, 0) ∈ D.fpM.get ((G ops E u).mi m) := by\n\
\  intro oc w m j cm hl ⟨x, hx⟩\n\
\  have hus : (.inl m : St) ∈ users j cm :=\n\
\    usersOk ops E (.inl m) ⟨j, cm⟩ (fpOk ops E u oc w (.inl m) ⟨j, (cm, x)⟩ (mem_proj j _ _ hx))\n\
\  obtain ⟨k, hk, e⟩ := List.mem_map.mp hus\n\
\  have h2 := List.all_eq_true.mp (vchk j cm hl) k hk\n\
\  simp only [Bool.and_eq_true] at h2\n\
\  simp only [stOf, h2.1, cond_true] at e\n\
\  cases e\n" else "") ++ "end\n")

-- | An exported module's own interface facts: each method C with itself or one argument wire, and a
-- method without an enable (a value method) calls only ports without one (design.md §3.1).
exportCertText :: String
exportCertText = "theorem selfAnnX : ∀ m : Fin nM, xAnn.conf m m = true ∨ xAnn.share m = true := by decide\n\
\theorem enVB : Tab.allB (fun k keys => Nat.beq (kT.get k) 0 || Tab.allB (fun _ p => Nat.blt k nR || xAnn.hasEn (mM (k - nR)) ||\n\
\    !(CI p.1).ann.hasEn p.2) keys 0 (kT.get k)) keyT 0 nS = true := by decide +kernel\n\
\theorem enV_ok (ops : Ops V) (E : EqC V) (u : St → Nat → V) : ∀ (oc : OC V) (w : Wires (Fin nM) V) (m : Fin nM),\n\
\    Env (P ops E u) oc w → (P ops E u).X.ann.hasEn m = false → (P ops E u).X.ann.alwaysEn m = false →\n\
\    Avail (P ops E u) oc w m → ∀ c ∈ (trM (P ops E u) oc w m).1, ((P ops E u).CI c.1).ann.hasEn c.2.1 = false := by\n\
\  intro oc w m _ hen _ _ c hc\n\
\  rw [trM_eq] at hc\n\
\  obtain ⟨i, hi, e⟩ := mem_keys (callsOf_fn_ports _ _ _ c hc)\n\
\  have h := sitesB_spec (fun k _ p => Nat.blt k nR || xAnn.hasEn (mM (k - nR)) || !(CI p.1).ann.hasEn p.2) enVB (nR + m.val) (mlt m) i hi\n\
\  have h1 : Nat.blt (nR + m.val) nR = false :=\n\
\    Bool.eq_false_iff.mpr (fun h' => absurd (Nat.blt_eq.mp h') (Nat.not_lt.mpr (Nat.le_add_right nR m.val)))\n\
\  rw [h1, Nat.add_sub_cancel_left, mM_val, show xAnn.hasEn m = false from hen, e] at h\n\
\  simpa using h\n"

pmName :: PM -> String
pmName (PS s) = s
pmName (PT t) = show t

-- | The per-instance hypotheses of the idx lookups a proof uses (HXS), in first-use order.
type HX = StateT [(String, String)] W

hx :: EC -> String -> Int -> PM -> String -> Int -> Int -> HX String
hx ec k i m lp st si = do
    let nm = "hx_" ++ show st ++ "_" ++ show si ++ "_" ++ pmName m
        stmt = if st < ecNR ec then fmt "idxT (((usesK .%s).get %d).uses %s) (rR %d).val = [%d]" [k, show i, lp, show st, show si]
               else fmt "idxT (((usesK .%s).get %d).uses %s) (nR + (mM %d).val) = [%d]" [k, show i, lp, show (st - ecNR ec), show si]
    have <- gets (lookup nm)
    case have of
        Just _ -> modify' (map (\(a, b) -> if a == nm then (a, stmt) else (a, b)))
        Nothing -> modify' (++ [(nm, stmt)])
    return nm

usesLeaves :: EC -> MPort -> [(Int, [Int])]
usesLeaves ec p = M.toList (M.fromListWith (flip (++)) [ (k, [i]) | (k, i) <- M.findWithDefault [] p (ecU ec) ])

usersProof :: EC -> MPort -> (Int -> [Int] -> HX String) -> HX String
usersProof ec p leaf = do
    ls <- mapM (\(k, sts) -> ("fun _ => " ++) <$> leaf k sts) (usesLeaves ec p)
    return ("users_all\n      " ++ tabpairI6 ls)
  where tabpairI6 items = go 0 (length items)
          where v = listArray (0, length items - 1) items
                go lo hi | hi - lo == 1 = v ! lo
                         | otherwise = let mid = (lo + hi) `div` 2 in "⟨" ++ go lo mid ++ ",\n      " ++ go mid hi ++ "⟩"

bigPort :: Int
bigPort = 8

-- | A users leaf's first site (usesLeaves never gives an empty list).
site0 :: [Int] -> Int
site0 (s : _) = s
site0 [] = error "schedlean: a users leaf with no site"

enProof :: EC -> MPort -> String -> HX String
enProof ec p@(k, i, m) inst = do
    let lp = lportOf ec k m
        lv = usesLeaves ec p
        nR = ecNR ec
        simple = not (ecMet ec) && all (\(st, sts) -> isUncond (site ec st (site0 sts))) lv
        leafS st sts = do
            h <- hx ec k i m lp st (site0 sts)
            return (fmt "cRk ops E u oc w (rR %d) .%s %d h %s %d %s rfl rfl rfl" [show st, k, show i, lp, show (site0 sts), h])
    if null lv then return "en_none (P ops E u) (fire ops E) oc w _ _ _ rfl _ rfl"
    else if simple then
        if length lv <= bigPort then do
            ls <- mapM (uncurry leafS) lv
            return (fmt "en_list (P ops E u) (fire ops E) oc w %s %s (stepOn ops E oc) (onIff ops E u oc w)\n      (users %s %s) %s" [inst, lp, inst, lp, allL ls])
        else do
            up <- usersProof ec p leafS
            return (fmt "en_lit (P ops E u) (fire ops E) oc w %s %s (stepOn ops E oc) (onIff ops E u oc w)\n      (users %s %s) _ rfl (%s)" [inst, lp, inst, lp, up])
    else do
        rs <- forM lv $ \(st, sts) -> do
            let si = site0 sts
                s = site ec st si
                stepT = if st < nR then ".inr (rR " ++ show st ++ ")" else ".inl (mM " ++ show (st - nR) ++ ")"
            h <- hx ec k i m lp st si
            pf <- if st < nR
                then if isUncond s
                    then return (fmt "en_unc Iff.rfl (fun hf => cRk ops E u oc w (rR %d) .%s %d h %s %d %s rfl rfl rfl)" [show st, k, show i, lp, show si, h])
                    else let cv = condVal ec st s (VCtx ("(u (.inr (rR " ++ show st ++ ")))") "ops.z" "hf")
                         in return (fmt "en_cnd Iff.rfl (fun hf => (cRkIff ops E u oc w (rR %d) .%s %d h %s %d %s rfl rfl).trans (iff_of_eqB (%s)))" [show st, k, show i, lp, show si, h, cv])
                else do
                    unless (isUncond s) $ error "schedlean: AssertionError: conditional method call"
                    return (fmt "en_unc Iff.rfl (fun hf => cMk ops E u oc w (mM %d) .%s %d h %s %d %s rfl rfl rfl)" [show (st - nR), k, show i, lp, show si, h])
            return ((stepT, "(" ++ stepT ++ ", " ++ entry ec st s ++ ")"), pf)
        let ents = map (snd . fst) rs
            us = "[" ++ intercalate ", " (map (fst . fst) rs) ++ "]"
        return (fmt "by\n      rw [show users %s %s = %s from rfl]\n      exact en_pairs (P ops E u) (fire ops E) oc w %s %s [%s]\n        %s"
                    [inst, lp, us, inst, lp, intercalate ", " ents, allL (map snd rs)])

argProof :: EC -> MPort -> String -> HX String
argProof ec p@(k, i, m) inst = do
    let pinfo = M.lookup p (ecPortInfo ec)
        lp = lportOf ec k m
        lv = usesLeaves ec p
        nR = ecNR ec
        hya = ecHya ec
        leaf st sts = do
            let si = site0 sts
                s = site ec st si
            h <- hx ec k i m lp st si
            let core = if st < nR
                    then fmt "aRk ops E u oc w (rR %d) .%s %d h %s %d %s _ (fun _ => %s) x hx"
                             [show st, k, show i, lp, show si, h, argVal ec st s (VCtx ("(u (.inr (rR " ++ show st ++ ")))") "ops.z" "hav")]
                    else fmt "aMk ops E u oc w (mM %d) .%s %d h %s %d %s _ (fun _ => %s) x hx"
                             [show (st - nR), k, show i, lp, show si, h
                             , argVal ec st s (VCtx ("(u (.inl (mM " ++ show (st - nR) ++ ")))") ("(w.arg (mM " ++ show (st - nR) ++ "))") "(cf_of_avail hav)")]
            case pinfo of
                Just pi | piArgk pi == "mux" -> do
                    let selq = M.findWithDefault False (piQ pi) (ecSelmux ec)
                        sel = if selq && not (isUncond s)
                              then fmt "((Bool.and_eq_true _ _).mpr ⟨hav, (%s).symm.trans (aRkC ops E u oc w (rR %d) .%s %d h %s %d %s x hx)⟩)"
                                       [condVal ec st s (VCtx ("(u (.inr (rR " ++ show st ++ ")))") "ops.z" "hav"), show st, k, show i, lp, show si, h]
                              else "hav"
                    return (fmt "fun hav x hx => (arg_muxT ops.z (chain_%d ops E %s oc hE.1) (p := %d) rfl %s).trans\n        (%s)"
                                [show (piQ pi), hya, show (piMpos pi M.! st), sel, core])
                _ -> return ("fun hav x hx => " ++ core)
    if kdFx (kd ec k) m then return "zarg ops E u oc w _ _ rfl"
    else if null lv then return "fun s hs => by cases hs"
    else if length lv <= bigPort then do
        ls <- mapM (uncurry leaf) lv
        return (fmt "arg_users (P ops E u) (fire ops E) oc w %s %s _ (users %s %s)\n      %s" [inst, lp, inst, lp, allL ls])
    else usersProof ec p leaf

lcProof :: EC -> MPort -> String -> HX String
lcProof ec p@(k, i, m) inst = do
    let pi = fromMaybe (error ("schedlean: last-caller port " ++ show p ++ " has no info")) (M.lookup p (ecPortInfo ec))
        lp = lportOf ec k m
        q = piQ pi
        mus = piMus pi
    ents <- forM (zip mus (piMargs pi)) $ \((st, si), s) -> do
        h <- hx ec k i m lp st si
        let cR = "(u (.inr (rR " ++ show st ++ ")))"
            ark = fmt "aRk ops E u oc w (rR %d) .%s %d h %s %d %s _ (fun _ => %s) x hx" [show st, k, show i, lp, show si, h, argVal ec st s (VCtx cR "ops.z" "hav")]
        if isUncond s
            then return (fmt "⟨fun hs => ⟨hs, cRk ops E u oc w (rR %d) .%s %d h %s %d %s rfl rfl rfl⟩,\n        fun hav x hx => ⟨hav, %s⟩⟩"
                             [show st, k, show i, lp, show si, h, ark])
            else let cvf = condVal ec st s (VCtx cR "ops.z" "((Bool.and_eq_true _ _).mp hs).1")
                     cva = condVal ec st s (VCtx cR "ops.z" "hav")
                 in return (fmt "⟨fun hs => ⟨((Bool.and_eq_true _ _).mp hs).1, (cRkIff ops E u oc w (rR %d) .%s %d h %s %d %s rfl rfl).mpr\n          ((iff_of_eqB (%s)).mpr ((Bool.and_eq_true _ _).mp hs).2)⟩,\n        fun hav x hx => ⟨(Bool.and_eq_true _ _).mpr ⟨hav, (%s).symm.trans\n          (aRkC ops E u oc w (rR %d) .%s %d h %s %d %s x hx)⟩, %s⟩⟩"
                                [show st, k, show i, lp, show si, h, cvf, cva, show st, k, show i, lp, show si, h, ark])
    return (fmt "LC.argL_of_pmuxN (P ops E u) (fire ops E) oc w %s %s (LC.lpOf (G ops E u)) (users %s %s) %d\n\
\      (fun t => (lcT_%d.get t) V ops E oc) (fun t => .inr (rR (lcsT_%d.get t))) ops.z\n\
\      (fun t => D.nM + lcsT_%d.get t %% nR) (fun _ => rfl) (by decide +kernel)\n\
\      (((usesK .%s).get %d).us %s) (fun k => .inr (rR k)) (fun t => lcsT_%d.get t) rfl (fun _ => rfl)\n\
\      (by decide +kernel)\n      %s" [inst, lp, inst, lp, show (length mus), show q, show q, show q, k, show i, lp, show q, allL ents])

allName :: String -> String
allName k = case k of
    "reg" -> "regAll"; "cfg" -> "cfgAll"; "fifo" -> "flAll"; "f2" -> "f2All"; "sf" -> "f2All"; "rf" -> "rfAll"
    "rwS" -> "wireAll"; "rwC" -> "wireAll"; "rw0S" -> "wire0All"; "rw0C" -> "wire0All"
    _ -> error ("schedlean: no ports lemma for kind " ++ k)

emitEAQ :: EC -> W ()
emitEAQ ec = do
    let hyp = ecHyp ec
        hya = ecHya ec
        wxm = ecWXM ec
        lcp = ecLCP ec
        m = ecM ec
        nT = ecNT ec
        eaqArg = if null lcp
            then "(∀ cm : (KI κ).M, ∀ s ∈ users (⟨κ, ⟨i, h⟩⟩ : I) cm, Av (P ops E u) (fire ops E) oc w s →\n\
\      ∀ x, (cm, x) ∈ proj (⟨κ, ⟨i, h⟩⟩ : I) (trS (P ops E u) oc w s) → (A V ops E oc w).arg cm = x)"
            else "(∀ cm : (KI κ).M, lcK κ i cm = false → ∀ s ∈ users (⟨κ, ⟨i, h⟩⟩ : I) cm, Av (P ops E u) (fire ops E) oc w s →\n\
\      ∀ x, (cm, x) ∈ proj (⟨κ, ⟨i, h⟩⟩ : I) (trS (P ops E u) oc w s) → (A V ops E oc w).arg cm = x) ∧\n\
\    (∀ cm : (KI κ).M, lcK κ i cm = true → ∀ s ∈ users (⟨κ, ⟨i, h⟩⟩ : I) cm, On (P ops E u) (fire ops E) oc w s →\n\
\      ∀ x, (cm, x) ∈ proj (⟨κ, ⟨i, h⟩⟩ : I) (trS (P ops E u) oc w s) →\n\
\      (∀ s' ∈ users (⟨κ, ⟨i, h⟩⟩ : I) cm, LC.lpOf (G ops E u) s < LC.lpOf (G ops E u) s' →\n\
\        On (P ops E u) (fire ops E) oc w s' → ¬ Calls (P ops E u) (trS (P ops E u) oc w s') (⟨κ, ⟨i, h⟩⟩ : I) cm) →\n\
\      (A V ops E oc w).arg cm = x)"
    w "/-! ## Enables and arguments: one lemma per child instance, dispatched by table -/\n"
    w (fmt "section\n\
\variable {V : Type}\n\
\/-- What `Cert.en` and `Cert.arg` say about child `⟨κ, i⟩` whose arm is `A`. -/\n\
\def EAQ (ops : Ops V) (E : EqC V) %s (u : St → Nat → V) (κ : Kind) (i : Nat)\n\
\    (A : (V : Type) → Ops V → EqC V → OC V → %s → Wires (KI κ).M V) : Prop :=\n\
\  ∀ (h : i < nk κ) (oc : OC V) (w : %s), Env (P ops E u) oc w →\n\
\    (∀ cm : (KI κ).M, (KI κ).ann.hasEn cm = true →\n\
\      ((A V ops E oc w).en cm = true ↔ ∃ s ∈ users (⟨κ, ⟨i, h⟩⟩ : I) cm, On (P ops E u) (fire ops E) oc w s ∧\n\
\        Calls (P ops E u) (trS (P ops E u) oc w s) (⟨κ, ⟨i, h⟩⟩ : I) cm)) ∧\n\
\    %s\n\
\\n\
\/-- A flagged (argument-less value) port's calls carry `z`. -/\n\
\theorem zarg (ops : Ops V) (E : EqC V) (u : St → Nat → V) (oc : OC V) (w : %s) (j : I) (cm : (CI j).M)\n\
\    (hf : fxP ⟨j, cm⟩ = true) :\n\
\    ∀ s ∈ users j cm, Av (P ops E u) (fire ops E) oc w s → ∀ x, (cm, x) ∈ proj j (trS (P ops E u) oc w s) → ops.z = x := by\n\
\  intro s _ _ x hx\n\
\  cases s with\n\
\  | inl e => %s\n\
\  | inr r =>\n\
\    have hc := mem_proj j _ _ hx\n\
\    exact (zR_ok ops E u oc r ⟨j, (cm, x)⟩ hc hf).symm\n"
           [hyp, wxm, wxm, eaqArg, wxm, if ecMet ec then "exact (zM_ok ops E u oc w e ⟨j, (cm, x)⟩ (mem_proj j _ _ hx) hf).symm" else "exact e.elim"])
    when (nT > 0) $ do
        w "/-- The task statements' child. -/\nabbrev jT : I := ⟨.tasks, ⟨0, by decide⟩⟩"
        w (fmt "/-- Task statement `t`'s enable and argument facts (its one user). -/\n\
\def TQ (ops : Ops V) (E : EqC V) %s (u : St → Nat → V) (t : Nat) (f : (V : Type) → Ops V → EqC V → OC V → Bool) : Prop :=\n\
\  ∀ (oc : OC V) (w : %s), Env (P ops E u) oc w →\n\
\    (f V ops E oc = true ↔ ∃ s ∈ users jT (tP t), On (P ops E u) (fire ops E) oc w s ∧\n\
\      Calls (P ops E u) (trS (P ops E u) oc w s) jT (tP t)) ∧\n\
\    (∀ s ∈ users jT (tP t), Av (P ops E u) (fire ops E) oc w s →\n\
\      ∀ x, (tP t, x) ∈ proj jT (trS (P ops E u) oc w s) → (targT.get t) V ops E oc = x)\n" [hyp, wxm])
        let tu = tuser ec
            tqLeaf t = let (k, i0, st) = tu M.! t
                           cR = "(u (.inr (rR " ++ show k ++ ")))"
                           enp | sHw st == Just MChk = "en_tchk (fun hw => ((Bool.and_eq_true _ _).mp (cf_of_wf hw)).2)"
                               | Just (MCfT c) <- sHw st = "en_tcf (cfA_" ++ show c ++ " ops E oc hE.1)"
                               | isUncond st = "en_tk rfl"
                               | otherwise = "en_cnd Iff.rfl (fun hf => iff_of_eqB (" ++ condVal ec k st (VCtx cR "ops.z" "hf") ++ "))"
                           av = argVal ec k st (VCtx cR "ops.z" "hav")
                           s = show
                       in fmt "(have hx : idxT (((usesK .tasks).get 0).uses (tP %d)) (rR %d).val = [%d] := by decide +kernel\n\
\    have hu : users jT (tP %d) = [.inr (rR %d)] := by decide +kernel\n\
\    fun oc w hE => ⟨en_one (P ops E u) (fire ops E) oc w jT (tP %d) (.inr (rR %d)) _ hu _\n\
\      (cRkIff ops E u oc w (rR %d) .tasks 0 (by decide) (tP %d) %d hx rfl rfl) _ (%s),\n\
\    arg_users (P ops E u) (fire ops E) oc w jT (tP %d) _ _ ⟨fun hav x hx' =>\n\
\      aRk ops E u oc w (rR %d) .tasks 0 (by decide) (tP %d) %d hx _ (fun _ => %s) x hx', trivial⟩⟩)"
                              [s t, s k, s i0, s t, s k, s t, s k, s k, s t, s i0, enp, s t, s k, s t, s i0, av]
        prf <- stabproof "tenT" "tqAll" ("(ops : Ops V) (E : EqC V) " ++ hyp ++ " (u : St → Nat → V)") ("ops E " ++ hya ++ " u")
                         (\nm lo hi -> fmt "Tab.AllR (TQ ops E %s u) %s %d %d" [hya, nm, show lo, show hi]) (map tqLeaf [0 .. nT - 1]) True
        w (fmt "theorem tqAll (ops : Ops V) (E : EqC V) %s (u : St → Nat → V) : Tab.AllR (TQ ops E %s u) tenT 0 nT :=\n  %s" [hyp, hya, prf])
        w (fmt "theorem ea_tasks_0 (ops : Ops V) (E : EqC V) %s (u : St → Nat → V) : EAQ ops E %s u .tasks 0 arm_tasks_0 := by\n\
\  intro h oc w hE\n\
\  refine ⟨fun cm _ => ?_, %s⟩\n\
\  · obtain ⟨t, ht⟩ := cm\n\
\    have e : (⟨t, ht⟩ : Fin nT) = tP t := Fin.ext (Nat.mod_eq_of_lt ht).symm\n\
\    have := (Tab.get_allR _ 0 nT (tqAll ops E %s u) t (Nat.zero_le _) ht oc w hE).1\n\
\    rw [← e] at this\n\
\    exact this\n\
\  · obtain ⟨t, ht⟩ := cm\n\
\    have e : (⟨t, ht⟩ : Fin nT) = tP t := Fin.ext (Nat.mod_eq_of_lt ht).symm\n\
\    have := (Tab.get_allR _ 0 nT (tqAll ops E %s u) t (Nat.zero_le _) ht oc w hE).2\n\
\    rw [← e] at this\n\
\    exact this\n" [hyp, hya, if null lcp then "fun cm => ?_" else "fun cm _ => ?_, fun cm hl => absurd hl (by simp [lcK])", hya, hya])
    forM_ (ecKinds ec) $ \k -> unless (k == "tasks") $ forM_ [0 .. length (mByk m M.! k) - 1] $ \i -> do
        let inst = "⟨." ++ k ++ ", ⟨" ++ show i ++ ", h⟩⟩"
        (ens, args, argsL, hxs) <- flip evalStateT [] $ do
            parts <- forM (kdPorts (kd ec k)) $ \pm -> do
                let p = (k, i, pm)
                e <- if kdHasEn (kd ec k) pm then ("fun _ => " ++) <$> enProof ec p inst else return "fun hh => by cases hh"
                (a, al) <- if null lcp then (\x -> (x, [])) <$> argProof ec p inst
                           else if p `elem` lcp then (\x -> ("fun hl => absurd hl (by decide)", ["fun _ => " ++ x])) <$> lcProof ec p inst
                           else (\x -> ("fun _ => " ++ x, ["fun hl => absurd hl (by decide)"])) <$> argProof ec p inst
                return (e, a, al)
            st <- gets id
            return ([ e | (e, _, _) <- parts ], [ a | (_, a, _) <- parts ], concat [ al | (_, _, al) <- parts ], st)
        let an = case lookup k (ecSynth ec) of
                Just sk -> let pt = maybe (exNS (skExport sk) ++ ".mM") (const (portFn k)) (skOff sk)
                           in "Tab.finAll " ++ pt ++ " " ++ pt ++ "_val fxT_" ++ k
                Nothing -> allName k
            -- a synthesized kind's ports' facts are a balanced tree shaped as its fxT table (Tab.finAll)
            portsArg xs | isSynthKind k = "(" ++ tabproofI "    " xs ++ ")"
                        | otherwise = "(" ++ intercalate ")\n    (" xs ++ ")"
        w (fmt "theorem ea_%s_%d (ops : Ops V) (E : EqC V) %s (u : St → Nat → V) : EAQ ops E %s u .%s %d arm_%s_%d := by\n" [k, show i, hyp, hya, k, show i, k, show i]
           ++ concat [ "  have " ++ nm ++ " : " ++ st ++ " := by decide +kernel\n" | (nm, st) <- hxs ]
           ++ "  intro h oc w hE\n"
           ++ fmt "  refine ⟨%s %s, %s %s%s⟩" [an, portsArg ens, an, portsArg args
                                             , if null lcp then "" else ", " ++ an ++ " " ++ portsArg argsL])
    forM_ (ecKinds ec) $ \k -> do
        let n = length (mByk m M.! k)
        if k == "tasks"
            then w (fmt "theorem eaAll_tasks (ops : Ops V) (E : EqC V) %s (u : St → Nat → V) : Tab.AllR (EAQ ops E %s u .tasks) arms_tasks 0 1 :=\n  Tab.one (ea_tasks_0 ops E %s u)" [hyp, hya, hya])
            else do
                prf <- stabproof ("arms_" ++ k) ("eaAll_" ++ k) ("(ops : Ops V) (E : EqC V) " ++ hyp ++ " (u : St → Nat → V)") ("ops E " ++ hya ++ " u")
                                 (\nm lo hi -> fmt "Tab.AllR (EAQ ops E %s u .%s) %s %d %d" [hya, k, nm, show lo, show hi])
                                 [ "ea_" ++ k ++ "_" ++ show i ++ " ops E " ++ hya ++ " u" | i <- [0 .. n - 1] ] True
                w (fmt "theorem eaAll_%s (ops : Ops V) (E : EqC V) %s (u : St → Nat → V) : Tab.AllR (EAQ ops E %s u .%s) arms_%s 0 %d :=\n  %s" [k, hyp, hya, k, k, show n, prf])
    w (fmt "theorem eaAll (ops : Ops V) (E : EqC V) %s (u : St → Nat → V) (κ : Kind) (i : Nat) (h : i < nk κ) :\n\
\    EAQ ops E %s u κ i ((arms κ).get i) := by\n\
\  cases κ\n" [hyp, hya] ++ concat [ fmt "  · exact Tab.get_allR _ 0 _ (eaAll_%s ops E %s u) i (Nat.zero_le _) h\n" [k, hya] | k <- ecKinds ec ] ++ "end\n")
    cut

emitCert :: EC -> String -> W ()
emitCert ec lcHvr = do
    let hyp = ecHyp ec
        hya = ecHya ec
        wxm = ecWXM ec
        met = ecMet ec
        lc = not (null (ecLCP ec))
        needuc = any (\k -> not (null [ () | st <- siteList k, not (isUncond st), rdyG (sPort st) ])) [0 .. ecNS ec - 1]
        siteList k = foldr (:) [] (tSites (step ec k))
        rdyG (k, _, p) = not (kdAlwaysRdy (kd ec k) p)
        (rdyRBody, rdyMBody) = if needuc
            then ("intro oc w r hE hf c hc ha\n  rw [trR_eq] at hc\n  exact rdyS ops E " ++ hya ++ " oc r.val (rlt r) _ ops.z hE.2.1 (pre_r hf) c hc ha",
                  "intro oc w m hE hav c hc ha\n  rw [trM_eq] at hc\n  exact rdyS ops E " ++ hya ++ " oc (nR + m.val) (mlt m) _ (w.arg m) hE.2.1 (pre_m (cf_of_avail hav)) c hc ha")
            else ("intro oc w r _ hf c hc ha\n\
\  rw [trR_eq] at hc\n\
\  obtain ⟨i, hi, e⟩ := mem_keys (callsOf_fn_ports _ _ _ c hc)\n\
\  have hrd := rd_of_cf (cf_of_wf hf)\n\
\  unfold RD at hrd\n\
\  have h2 := List.all_eq_true.mp hrd i (List.mem_range'.mpr ⟨i, hi, by omega⟩)\n\
\  rw [e] at h2\n\
\  simp only [alwaysRdyB, Bool.or_eq_true] at h2\n\
\  rcases h2 with h2 | h2\n\
\  · rw [ha] at h2; cases h2\n\
\  · exact h2",
                  "intro oc w m hE hav c hc ha\n\
\  rw [trM_eq] at hc\n\
\  obtain ⟨i, hi, e⟩ := mem_keys (callsOf_fn_ports _ _ _ c hc)\n\
\  have hrd := rd_of_cf (cf_of_avail hav)\n\
\  unfold RD at hrd\n\
\  have h2 := List.all_eq_true.mp hrd i (List.mem_range'.mpr ⟨i, hi, by omega⟩)\n\
\  rw [e] at h2\n\
\  simp only [alwaysRdyB, Bool.or_eq_true] at h2\n\
\  rcases h2 with h2 | h2\n\
\  · rw [ha] at h2; cases h2\n\
\  · exact h2")
        hvm = if met then "(hvM ops E u)" else "(fun _ _ m => m.elim)"
        rep = concatMap (\c -> if c == '\0' then hvm else [c])
    w ("/-! ## The certificate and the obligation -/\n\
\\n\
\section\n\
\variable {V : Type}\n\
\def fp (ops : Ops V) (E : EqC V) : St → List (Σ j : I, (CI j).M)\n"
       ++ (if met then "  | .inl m => (sitesF (siteS ops E (nR + m.val) ops.z) (kR (nR + m.val))).map (fun s => ⟨s.j, s.m⟩)\n" else "  | .inl e => e.elim\n")
       ++ "  | .inr r => (sitesF (siteR ops E r.val) (kR r.val)).map (fun s => ⟨s.j, s.m⟩)\n\
\\n\
\theorem usersOk (ops : Ops V) (E : EqC V) : ∀ (s : St), ∀ p ∈ fp ops E s, s ∈ users p.1 p.2 := by\n\
\  intro s p hp\n\
\  cases s with\n"
       ++ (if met then "  | inl m =>\n    obtain ⟨i, hi, rfl⟩ := mem_keys hp\n    exact mem_users (hchkM m i hi)\n" else "  | inl e => exact e.elim\n")
       ++ "  | inr r =>\n\
\    obtain ⟨i, hi, rfl⟩ := mem_keys hp\n\
\    exact mem_users (hchk r i hi)\n\
\\n\
\theorem fpOk (ops : Ops V) (E : EqC V) (u : St → Nat → V) : ∀ (oc : OC V) (w : " ++ wxm ++ ") (s : St),\n\
\    ∀ c ∈ trS (P ops E u) oc w s, (⟨c.1, c.2.1⟩ : Σ j : I, (CI j).M) ∈ fp ops E s := by\n\
\  intro oc w s c hc\n\
\  cases s with\n"
       ++ (if met then "  | inl m =>\n\
\    rw [trSM_eq] at hc\n\
\    have h := callsOf_fn_ports _ _ _ c hc\n\
\    rw [sitesF_map] at h\n\
\    show _ ∈ (sitesF (siteS ops E (nR + m.val) ops.z) (kR (nR + m.val))).map (fun s => (⟨s.j, s.m⟩ : Σ j : I, (CI j).M))\n\
\    rw [sitesF_map]\n\
\    exact h\n" else "  | inl e => exact e.elim\n")
       ++ "  | inr r => rw [trS_eq] at hc; exact callsOf_fn_ports _ _ _ c hc\n\
\\n\
\theorem rdyR_ok (ops : Ops V) (E : EqC V) " ++ hyp ++ " (u : St → Nat → V) : ∀ (oc : OC V) (w : " ++ wxm ++ ") (r : Fin nR),\n\
\    Env (P ops E u) oc w → fire ops E oc w r = true → ∀ c ∈ (trR (P ops E u) oc r).1,\n\
\      ((P ops E u).CI c.1).ann.alwaysRdy c.2.1 = false → (oc c.1).rdy c.2.1 = true := by\n\
\  " ++ rdyRBody ++ "\n"
       ++ (if met then "theorem rdyM_ok (ops : Ops V) (E : EqC V) " ++ hyp ++ " (u : St → Nat → V) : ∀ (oc : OC V) (w : Wires (Fin nM) V) (m : Fin nM),\n\
\    Env (P ops E u) oc w → Avail (P ops E u) oc w m → ∀ c ∈ (trM (P ops E u) oc w m).1,\n\
\      ((P ops E u).CI c.1).ann.alwaysRdy c.2.1 = false → (oc c.1).rdy c.2.1 = true := by\n\
\  " ++ rdyMBody ++ "\n" else "")
       ++ "\n\
\theorem onehot_ok (ops : Ops V) (E : EqC V) " ++ hyp ++ " (u : St → Nat → V) (oc : OC V) (w : " ++ wxm ++ ")\n\
\    (hC : EnvC (P ops E u) oc w) : onehotP ops E oc :=\n\
\  " ++ allL [ "chain_" ++ show q ++ " ops E " ++ hya ++ " oc hC.1" | q <- [0 .. length (ecMuxes ec) - 1] ] ++ "\n\
\\n" ++ lcHvr ++ (if met && exported ec then exportCertText else "") ++ "def cert (ops : Ops V) (E : EqC V) " ++ hyp ++ " (u : St → Nat → V) : " ++ (if lc then "CertL" else "Cert") ++ " (P ops E u) :=\n\
\  " ++ (if lc then "LC.PairFacts.toCertL" else "PairFacts.toCert") ++ " (P ops E u) (pairs ops E " ++ hya ++ " u) (fp ops E) users (usersOk ops E) (fpOk ops E u)\n"
       ++ (if met then "    " ++ (if exported ec then "(fun m => selfAnnX m)" else "(fun m => Or.inl (decide_eq_true rfl))") ++ " (fun oc w m _ hav => resM_ok ops E " ++ hya ++ " u oc w m hav)\n"
           else "    (fun m => m.elim) (fun _ _ m => m.elim)\n")
       ++ "    (fun oc w r _ hf => resR_ok ops E " ++ hya ++ " u oc w r hf)\n"
       ++ (if met then "    (rdyM_ok ops E " ++ hya ++ " u) (rdyR_ok ops E " ++ hya ++ " u)\n" else "    (fun _ _ m => m.elim) (rdyR_ok ops E " ++ hya ++ " u)\n")
       ++ "    (fun oc w j cm hE hcm => by obtain ⟨κ, i, hi⟩ := j; exact (eaAll ops E " ++ hya ++ " u κ i hi hi oc w hE).1 cm hcm)\n"
       ++ (if met && exported ec then "    (enV_ok ops E u)\n" else if met then "    (fun _ _ m _ h => by simp [X, xAnn] at h)\n" else "    (fun _ _ m => m.elim)\n")
       ++ (if not lc
           then fmt "    (fun oc w j cm hE s hs hav x hx => by obtain ⟨κ, i, hi⟩ := j; exact (eaAll ops E %s u κ i hi hi oc w hE).2 cm s hs hav x hx)\n\
\    (fun oc w j cm _ h => by obtain ⟨κ, i⟩ := j; cases κ <;> cases cm <;> cases h)\n\
\    (onehot_ok ops E %s u)\n" [hya, hya]
           else rep (fmt "    (fun oc w j cm _ h => by obtain ⟨κ, i⟩ := j; cases κ <;> cases cm <;> cases h)\n\
\    (onehot_ok ops E %s u)\n\
\    lcJ lcOk (LC.lpOf (G ops E u)) (LC.lpRR_of_scan (glueOK ops E %s u) lcJ) (LC.lpMR_of_scan (glueOK ops E %s u) lcJ)\n\
\    (LC.lcMR_of_scan vbJ κv scanOK (glueOK ops E %s u) lcJ hkv \0 (hvR ops E u))\n\
\    (LC.lcRR_of_scan vbJ κv scanOK (glueOK ops E %s u) lcJ hkv (hvR ops E u))\n\
\    (LC.lcMM_of_scan vbJ κv scanOK (glueOK ops E %s u) lcJ hkv \0)\n\
\    (fun oc w j cm hE hl s hs hav x hx => by obtain ⟨κ, i, hi⟩ := j; exact (eaAll ops E %s u κ i hi hi oc w hE).2.1 cm hl s hs hav x hx)\n\
\    (fun oc w j cm hE hl s hs hon x hx hlt => by\n\
\      obtain ⟨κ, i, hi⟩ := j; exact (eaAll ops E %s u κ i hi hi oc w hE).2.2 cm hl s hs hon x hx hlt)\n" (replicate 8 hya)))
       ++ (if met then "theorem cf_of_availB {ops : Ops V} {E : EqC V} {oc : OC V} {w : Wires (Fin nM) V} {m : Fin nM}\n\
\    (h : Avail (F ops E).base oc w m) : CF ops E oc (nR + m.val) = true :=\n\
\  " ++ (if exported ec then "h.1.elim (fun h' => cf_ar ops E oc m h') id" else "h.1.resolve_left (fun h' => by cases h')") ++ "\n" else "")
       ++ "\n\
\/-- **The certificate fixed before the skipped values**: rules, firing and hits do not mention\n\
\    `u`, and a step's calls are the same at every `u` (`trUS`). -/\n\
\def certU (ops : Ops V) (E : EqC V) " ++ hyp ++ " : " ++ (if lc then "CertUL" else "CertU") ++ " (F ops E) where\n\
\  rules := (List.range nR).map rR\n\
\  fire := fire ops E\n\
\  hit := (pairs ops E " ++ hya ++ " (fun _ _ => ops.z)).hit\n\
\  cert u := cert ops E " ++ hya ++ " u\n\
\  rules_eq _ := rfl\n\
\  fire_eq _ := rfl\n\
\  hit_eq _ := rfl\n\
\  trU oc w s u u' _ hav := by\n\
\    cases s with\n"
       ++ (if met then "    | inl m => exact (trSM_eq ops E u oc w m).trans ((trUS ops E " ++ hya ++ " oc (nR + m.val) (mlt m) _ _ (w.arg m)\n\
\        (pre_m (cf_of_availB hav))).trans (trSM_eq ops E u' oc w m).symm)\n" else "    | inl e => exact e.elim\n")
       ++ "    | inr r => exact (trS_eq ops E u oc w r).trans ((trUS ops E " ++ hya ++ " oc r.val (rlt r) _ _ ops.z (pre_r hav)).trans\n\
\        (trS_eq ops E u' oc w r).symm)\n\
\\n\
\/-- **The module's obligation**: the order and every child's calls are fixed before the skipped\n\
\    values; for every value type, operators, constants and solver verdicts. -/\n\
\theorem obl [Inhabited V] (ops : Ops V) (E : EqC V) " ++ hyp ++ " : OblU (F ops E) :=\n\
\  " ++ (if lc then "oblU_of_certL" else "oblU_of_cert") ++ " (F ops E) (certU ops E " ++ hya ++ ")\n\
\end\n")
    cut
