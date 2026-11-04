-- | The hardware's per-port arms, the family F, and the fact structures
-- Hyps and Unconfirmed in groups (mke2e.py lines 1627-1870, 1941-2004; C25, C30).
module Lean.Sched.Emit.Arms
    ( emitArms, emitHyps, tuser
    ) where

import Control.Monad (when, forM_)
import Data.List (intercalate)
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Foldable as F (Foldable(toList))
import qualified Data.Map.Strict as M (Map, (!), findWithDefault, lookup, union, fromList)

import Lean.Print (cut, fmt, stab, tabOr, w, W)
import Lean.Sched.Emit.Ctx (argHw, asmChk, condHw, entry, entryOn, ex, hw, isUncond, kd, ocread, rsv, rterm, site, sitePort,
    step, EC(ecHasUnconf, ecNS, ecWXM, ecPortInfo, ecNR, ecNT, ecSelmux, ecNM, ecXM, ecMuxes, ecMet, ecM,
    ecKinds, ecFactK, ecEFacts, ecCFacts, ecEdit), HwEdit(..), PInfo(piArgk, piQ, piMus, piMargs, piUsers))
import Lean.Sched.Export (isSynthKind)
import Lean.Sched.Kinds (KindDef(kdHasEn, kdPorts, kdMType), PM(..))
import Lean.Sched.Model (MFact(fKind, fSite, fY, fStep, fLabel, fOk, fLate), MSite(sLabel, sKind, sPort, sHw,
    sCond), MStep(tName, tSites, tRes), MX(MLit, MChk, MCfT), Model(mFacts, mByk, mOpsF), MPort)
import Lean.Sched.Py (omSize)

pmName :: PM -> String
pmName (PS s) = s
pmName (PT t) = show t

-- | Each task statement's one user: step, site, and the site.
tuser :: EC -> M.Map Int (Int, Int, MSite)
tuser ec = M.fromList [ (t, (k, i, st)) | k <- [0 .. ecNS ec - 1], (i, st) <- zip [0 ..] (F.toList (tSites (step ec k)))
                                        , sKind st == "task", (_, _, PT t) <- [sPort st] ]

-- | A mutant's arguments and arms ('HwEdit'): user n's argument text, wrapped in a fresh
-- operator for 'ArgWrap'; a mux's or last-caller mux's arms, reordered or exchanged.
hwArg :: EC -> MPort -> Int -> String -> String
hwArg ec p n a = case ecEdit ec of
    ArgWrap p' n' | p' == p && n' == n -> freshF ec a
    _ -> a

freshF :: EC -> String -> String
freshF ec a = "ops.f " ++ show (omSize (mOpsF (ecM ec))) ++ " (" ++ a ++ ")"

hwArms :: EC -> MPort -> [(String, String)] -> [(String, String)]
hwArms ec p rows = case ecEdit ec of
    MuxSwap p' n | p' == p, n + 1 < length rows ->
        let as = map snd rows
            as' = take n as ++ [as !! (n + 1), as !! n] ++ drop (n + 2) as
        in zip (map fst rows) as'
    LcReverse p' | p' == p -> reverse rows
    _ -> rows

emitArms :: EC -> W ()
emitArms ec = do
    let m = ecM ec
        wxm = ecWXM ec
        kinds = ecKinds ec
        pinfo = ecPortInfo ec
        nR = ecNR ec
        nT = ecNT ec
    w "/-! ## The hardware: per-port definitions, per-kind arm tables -/\n"
    w "section\nvariable {V : Type}\n"
    forM_ [ (k, i, pm) | k <- kinds, k /= "tasks", i <- [0 .. length (mByk m M.! k) - 1], pm <- kdPorts (kd ec k) ] $ \p@(k, i, pm) ->
        case M.lookup p pinfo of
            Just pi | not (null (piUsers pi)) -> do
                let nm = k ++ "_" ++ show i ++ "_" ++ pmName pm
                    us = piUsers pi
                    enBody = case ecEdit ec of
                        EnForce p' | p' == p -> "true"
                        EnDrop p' n | p' == p -> "orL [" ++ intercalate ", " [ entry ec k' (site ec k' si) | (j, (k', si)) <- zip [0 ..] us, j /= n ] ++ "]"
                        CondDrop p' n | p' == p -> "orL [" ++ intercalate ", " [ if j == n then entryOn ec k' else entry ec k' (site ec k' si)
                                                                               | (j, (k', si)) <- zip [0 ..] us ] ++ "]"
                        _ -> "orL [" ++ intercalate ", " [ entry ec k' (site ec k' si) | (k', si) <- us ] ++ "]"
                when (kdHasEn (kd ec k) pm) $
                    w (fmt "def en_%s (ops : Ops V) (E : EqC V) (oc : OC V) (w : %s) : Bool := %s"
                           [nm, wxm, enBody])
                case piArgk pi of
                    "lc" -> do
                        let q = piQ pi
                            mus = piMus pi
                        stab ("lcT_" ++ show q) "Tab ((V : Type) → Ops V → EqC V → OC V → Bool × V)"
                             [ fmt "fun V (ops : Ops V) (E : EqC V) (oc : OC V) => ((%s, %s) : Bool × V)" [e, a]
                             | (e, a) <- hwArms ec p [ (entry ec k' st, hwArg ec p n (argHw ec k' st))
                                                     | (n, ((k', _), st)) <- zip [0 ..] (zip mus (piMargs pi)) ] ]
                        stab ("lcsT_" ++ show q) "Tab Nat" [ show k' | (k', _) <- mus ]
                        w (fmt "def lclist_%d (V : Type) (ops : Ops V) (E : EqC V) (oc : OC V) : List (Bool × V) :=\n  (List.range' 0 %d).map (fun t => (lcT_%d.get t) V ops E oc)"
                               [show q, show (length mus), show q])
                        w (fmt "def arg_%s (ops : Ops V) (E : EqC V) (oc : OC V) (w : %s) : V := pmux (lclist_%d V ops E oc) ops.z" [nm, wxm, show q])
                    "mux" -> do
                        let q = piQ pi
                            mus = piMus pi
                            sel = M.findWithDefault False q (ecSelmux ec)
                        stab ("mT_" ++ show q) "Tab ((V : Type) → Ops V → EqC V → OC V → Bool × V)"
                             [ fmt "fun V (ops : Ops V) (E : EqC V) (oc : OC V) => ((%s, %s) : Bool × V)" [e, a]
                             | (e, a) <- hwArms ec p [ (if sel then entry ec k' st else "WF ops E oc " ++ show k', hwArg ec p n (argHw ec k' st))
                                                     | (n, ((k', _), st)) <- zip [0 ..] (zip mus (piMargs pi)) ] ]
                        w (fmt "def mlist_%d (V : Type) (ops : Ops V) (E : EqC V) (oc : OC V) : List (Bool × V) :=\n  (List.range' 0 %d).map (fun t => (mT_%d.get t) V ops E oc)"
                               [show q, show (length mus), show q])
                        w (fmt "def arg_%s (ops : Ops V) (E : EqC V) (oc : OC V) (w : %s) : V := pmux (mlist_%d V ops E oc) ops.z" [nm, wxm, show q])
                    "none" -> return ()
                    _ -> let (k', si) = head us
                         in w (fmt "def arg_%s (ops : Ops V) (E : EqC V) (oc : OC V) (w : %s) : V := %s" [nm, wxm, hwArg ec p 0 (argHw ec k' (site ec k' si))])
            _ -> return ()
    w "end\n"
    let tu = tuser ec
        tcondHw k st = case sHw st of
            Just MChk -> "(gT.get " ++ show k ++ ") V ops E oc"
            Just (MCfT c) -> "cfL_" ++ show c ++ " V ops E oc"
            _ -> condHw ec k st
    when (nT > 0) $ do
        w "/-- Task statement `t`: its enable (the statement's condition, `WILL_FIRE` included) and arguments. -/"
        stab "tenT" "Tab ((V : Type) → Ops V → EqC V → OC V → Bool)"
             [ let (k, _, st) = tu M.! t
               in "fun V (ops : Ops V) (E : EqC V) (oc : OC V) => "
                  ++ (case ecEdit ec of
                        TaskEn t' b | t' == t -> if b then "true" else "false"
                        _ -> fmt "WF ops E oc %d && %s" [show k, tcondHw k st])
             | t <- [0 .. nT - 1] ]
        stab "targT" "Tab ((V : Type) → Ops V → EqC V → OC V → V)"
             [ let (k, _, st) = tu M.! t
                   a = argHw ec k st
               in "fun V (ops : Ops V) (E : EqC V) (oc : OC V) => " ++ (if ecEdit ec == TaskArgWrap t then freshF ec a else a)
             | t <- [0 .. nT - 1] ]
    let enT p@(k, i, pm) = if maybe False (not . null . piUsers) (M.lookup p pinfo)
                           then "en_" ++ k ++ "_" ++ show i ++ "_" ++ pmName pm ++ " ops E oc w" else "false"
        argT p@(k, i, pm) = case M.lookup p pinfo of
            Just pi | not (null (piUsers pi)), piArgk pi /= "none" -> "arg_" ++ k ++ "_" ++ show i ++ "_" ++ pmName pm ++ " ops E oc w"
            _ -> "ops.z"
        armBody k i = let p x = (k, i, PS x) in case k of
            "reg" -> fmt "regW (%s) (%s) ops.z" [enT (p "write"), argT (p "write")]
            "cfg" -> fmt "cfgW (%s) (%s) ops.z" [enT (p "write"), argT (p "write")]
            "fifo" -> fmt "fifoW (%s) (%s) (%s) (%s) ops.z" [enT (p "enq"), enT (p "deq"), enT (p "clear"), argT (p "enq")]
            _ | k `elem` ["f2", "sf"] -> fmt "f2W (%s) (%s) (%s) (%s) ops.z" [enT (p "enq"), enT (p "deq"), enT (p "clear"), argT (p "enq")]
            "rf" -> fmt "rfW (%s) (%s) %s" [enT (p "upd"), argT (p "upd"), unwords [ "(" ++ argT (p ('s' : show c)) ++ ")" | c <- [0 .. 4 :: Int] ]]
            _ | k `elem` ["rwS", "rwC"] -> fmt "wireW (%s) (%s) ops.z" [enT (p "wset"), argT (p "wset")]
            _ | k `elem` ["rw0S", "rw0C"] -> fmt "wire0W (%s) ops.z" [enT (p "wset")]
            "tasks" -> "⟨fun t => (tenT.get t.val) V ops E oc, fun t => (targT.get t.val) V ops E oc⟩"
            _ | isSynthKind k -> fmt "finW (n := %s) %s\n  %s"
                                     [ drop 4 (fromMaybe "" (kdMType (kd ec k))), tabOr "false" [ if kdHasEn (kd ec k) pm then enT (k, i, pm) else "false" | pm <- kdPorts (kd ec k) ]
                                     , tabOr "ops.z" [ argT (k, i, pm) | pm <- kdPorts (kd ec k) ] ]
            _ -> error ("schedcheck: no arm for kind " ++ k)
    forM_ kinds $ \k -> forM_ [0 .. length (mByk m M.! k) - 1] $ \i ->
        w (fmt "def arm_%s_%d (V : Type) (ops : Ops V) (E : EqC V) (oc : OC V) (w : %s) : Wires (KI .%s).M V :=\n  %s"
               [k, show i, wxm, k, armBody k i])
    forM_ kinds $ \k -> stab ("arms_" ++ k) (fmt "Tab ((V : Type) → Ops V → EqC V → OC V → %s → Wires (KI .%s).M V)" [wxm, k])
                             [ "arm_" ++ k ++ "_" ++ show i | i <- [0 .. length (mByk m M.! k) - 1] ]
    when (ecMet ec) $
        stab "retT" "Tab ((V : Type) → Ops V → EqC V → OC V → V)"
             [ "fun V (ops : Ops V) (E : EqC V) (oc : OC V) => " ++ ex ec (nR + mi) (tRes (step ec (nR + mi))) (hw ec (nR + mi)) "ops.z"
             | mi <- [0 .. ecNM ec - 1] ]
    let outs = if not (ecMet ec) then "⟨fun e => e.elim, fun e => e.elim⟩"
               else "⟨fun m => CF ops E oc (nR + m.val), fun m => (retT.get m.val) V ops E oc⟩"
    w (fmt "section\nvariable {V : Type}\ndef arms : (κ : Kind) → Tab ((V : Type) → Ops V → EqC V → OC V → %s → Wires (KI κ).M V)\n" [wxm]
       ++ concat [ "  | ." ++ k ++ " => arms_" ++ k ++ "\n" | k <- kinds ]
       ++ fmt "\n/-- The parent's combinational logic: children through the arm tables; a method's RDY is its\n\
\    CAN_FIRE, its result the result expression. -/\n\
\def comb (ops : Ops V) (E : EqC V) (oc : OC V) (w : %s) : ((j : I) → Wires (CI j).M V) × Outs %s V :=\n\
\  (fun j => (arms j.1).get j.2.val V ops E oc w, %s)\n\
\/-- The parallel muxes' selectors are one-hot. -/\n\
\def onehotP (ops : Ops V) (E : EqC V) (oc : OC V) : Prop :=\n\
\  " [wxm, ecXM ec, outs]
       ++ intercalate " ∧ " ([ "amoChain false ((mlist_" ++ show q ++ " V ops E oc).map Prod.fst)" | q <- [0 .. length (ecMuxes ec) - 1] ] ++ ["True"])
       ++ "\n\n/-- **The family**: each step's program is `runSites v … v` of its own skipped values `v`. -/\n\
\abbrev F (ops : Ops V) (E : EqC V) : ParentU V where\n\
\  I := I\n\
\  decI := inferInstance\n\
\  kids := allKids [" ++ intercalate ", " (map ('.' :) kinds) ++ "] nk\n\
\  kids_nodup := allKids_nodup nk _ (by decide)\n\
\  kids_all := mem_allKids _ nk (fun κ => by cases κ <;> simp)\n\
\  CI := CI\n\
\  X := X\n\
\  PR := Fin nR\n"
       ++ (if not (ecMet ec) then "  body := fun m _ _ => m.elim\n"
           else "  body := fun m x v => runSites v ((resT.get (nR + m.val)) V ops E x) (sitesF (siteS ops E (nR + m.val) x) (kR (nR + m.val))) 0 v\n")
       ++ "  rbody := fun r v => runSites v ((resT.get r.val) V ops E ops.z) (sitesF (siteR ops E r.val) (kR r.val)) 0 v\n\
\  comb := comb ops E\n\
\  asm := fun oc _ => asmP ops E oc\n\
\  onehot := fun oc _ => onehotP ops E oc\n\
\  toB := ops.toB\n\
\\n\
\/-- The parent at skipped values `u` (step `s` sees only `u s`). -/\n\
\abbrev P (ops : Ops V) (E : EqC V) (u : St → Nat → V) : Parent V := (F ops E).at u\n"
       ++ (if not (ecMet ec) then "/-- Each step's program takes its skipped values only through `runSites`; its sites and result\n\
\    do not receive them (design §18 trust point 7, checked by `rfl`). -/\n\
\theorem F_sites (ops : Ops V) (E : EqC V) : F ops E = withRuleSites (F ops E)\n\
\    (fun r => sitesF (siteR ops E r.val) (kR r.val)) (fun r => (resT.get r.val) V ops E ops.z) := rfl\n"
           else "/-- Each step's program takes its skipped values only through `runSites`; its sites and result\n\
\    receive the method's argument, never the skipped values (design §18 trust point 7, by `rfl`). -/\n\
\theorem F_sites (ops : Ops V) (E : EqC V) : F ops E = withSitesX (F ops E)\n\
\    (fun m x => sitesF (siteS ops E (nR + m.val) x) (kR (nR + m.val))) (fun m x => (resT.get (nR + m.val)) V ops E x)\n\
\    (fun r => sitesF (siteR ops E r.val) (kR r.val)) (fun r => (resT.get r.val) V ops E ops.z) := rfl\n")
       ++ "end\n")
    cut

-- | The fact structures; returns each fact field's group (FPATH) and the counts the bounds report.
emitHyps :: EC -> W (M.Map (String, String) String, Int, Int, Int)
emitHyps ec = do
    let m = ecM ec
        facts = mFacts m
        hasF2 = "f2" `elem` ecKinds ec || "sf" `elem` ecKinds ec
        factStmt n f =
            let k = ecFactK ec M.! n
                i = fSite f
                s = site ec k i
                y = fY f
                bind = "(oc : OC V)" ++ concat [ " (t" ++ show j ++ " : V)" | j <- y ]
                lhs = rsv ec k i (\j -> "t" ++ show j)
            in if fKind f == "inv"
                then fmt "∀ %s, WF ops E oc %d = true → %s = %s" [bind, show k, lhs, rterm ec k i (hw ec k)]
                else let MLit j = sCond s
                     in fmt "∀ %s, WF ops E oc %d = true → %s = true → ops.toB (%s) = true" [bind, show k, lhs, ocread ec (sitePort ec k j)]
        hs2 r s = [ condHw ec r s ++ " = true" | not (isUncond s) ]
        -- a fact confirmed only with the conflict_free checks assumed holds where they pass
        prem how = [ "asmP ops E oc" | how == Just "assert" ]
        efactStmt (r1, r2, i1, i2, how, _) =
            let s1 = site ec r1 i1
                s2 = site ec r2 i2
            in "∀ oc : OC V, " ++ intercalate " → " (prem how ++ ["WF ops E oc " ++ show r1 ++ " = true", "WF ops E oc " ++ show r2 ++ " = true"]
                                                    ++ hs2 r1 s1 ++ hs2 r2 s2 ++ ["False"])
        cfactStmt (_, x, y, sx, sy, how, _) =
            "∀ oc : OC V, " ++ intercalate " → " (prem how ++ concat [ ("WF ops E oc " ++ show k ++ " = true") : hs2 k (site ec k si) | (k, si) <- [(x, sx), (y, sy)] ] ++ ["False"])
        name k = tName (step ec k)
        label k i = fromMaybe "None" (sLabel (site ec k i))
        fixed = ["  disj : ∀ (oc : OC V) (d : Nat), Nat.blt d nD = true → (CF ops E oc (dAT.get d) && CF ops E oc (dBT.get d)) = false"]
                ++ [ "  tob : ∀ b, ops.toB (ops.ofBool b) = b" | hasF2 ]
        factF = [ (fOk f, ("  /-- " ++ (if fKind f == "rdy" then "readiness" else "invariance of R") ++ ", " ++ fStep f ++ " / " ++ fLabel f
                            ++ ": this formula " ++ (if fOk f then "confirmed" else "not confirmed") ++ " by schedcheck (site-value form"
                            ++ (if fLate f /= 0 then ", " ++ show (fLate f) ++ " later-site literals dropped" else "") ++ ") -/\n  "
                            ++ fKind f ++ "_" ++ show n ++ " : " ++ factStmt n f, fKind f ++ "_" ++ show n))
                | (n, f) <- zip [0 :: Int ..] facts ]
        escF = [ (isJustS how, ("  /-- escape " ++ name r1 ++ " → " ++ name r2 ++ ": " ++ label r1 i1 ++ " then " ++ label r2 i2 ++ " ("
                                ++ howText how bt ++ ") -/\n  esc_" ++ show n ++ " : " ++ efactStmt f, "esc_" ++ show n))
               | (n, f@(r1, r2, i1, i2, how, bt)) <- zip [0 :: Int ..] (ecEFacts ec) ]
        howText how bt = case how of
            Just "solver" -> "this fact confirmed by the solver over these conditions"
            Just "bsc" -> "bsc: " ++ fromMaybe "" bt ++ "; these conditions imply bsc's"
            Just "assert" -> "confirmed by the solver with the conflict_free checks naming these steps assumed: under `asmP`"
            _ -> "not confirmed"
        cxF = [ (isJustS how, ("  /-- callers of one port never both call: " ++ name x ++ ", " ++ name y ++ " ("
                               ++ (case how of { Just "solver" -> "solver: exclusive"; Just "bsc" -> "bsc: " ++ fromMaybe "" bt
                                               ; Just "assert" -> "solver, with the conflict_free checks naming them assumed: under `asmP`"
                                               ; _ -> "solver: may both call" })
                               ++ ") -/\n  cx_" ++ show n ++ " : " ++ cfactStmt f, "cx_" ++ show n))
              | (n, f@(_, x, y, _, _, how, bt)) <- zip [0 :: Int ..] (ecCFacts ec) ]
        allF = factF ++ escF ++ cxF
        hyF = [ x | (True, x) <- allF ]
        unF = [ x | (False, x) <- allF ]
        groups sname src fs =
            let gs = chunks 128 fs
                txt = concat [ fmt "/-- %s, facts %d to %d. -/\nstructure %sG%d (ops : Ops V) (E : EqC V) : Prop where\n%s\n"
                                   [sname, show a, show (a + length g - 1), sname, show q, intercalate "\n" (map fst g)]
                             | (q, (a, g)) <- zip [0 :: Int ..] (zip [0, 128 ..] gs) ]
                path = M.fromList [ ((src, nm), "g" ++ show q) | (q, g) <- zip [0 :: Int ..] gs, (_, nm) <- g ]
                fields = [ fmt "  g%d : %sG%d ops E" [show q, sname, show q] | q <- [0 .. length gs - 1] ]
            in (txt, path, fields)
        (hgT, hgP, hgF) = groups "Hyps" "hy" hyF
        (ugT, ugP, ugF) = groups "Unconfirmed" "hr" unF
    w ("section\nvariable {V : Type}\n" ++ hgT ++ ugT
       ++ "/-- bsc's solver verdicts, hypotheses as today: disjointness (CAN_FIRE of each pair); the\n\
\    use-level readiness facts over R_seq, each escape's dynamic exclusion and the callers'\n\
\    exclusivity at a mux, as confirmed by the solver over schedcheck's conditions (in groups `gK`). -/\n\
\structure Hyps (ops : Ops V) (E : EqC V) : Prop where\n" ++ intercalate "\n" (fixed ++ hgF) ++ "\n"
       ++ (if ecHasUnconf ec then "/-- Facts schedcheck refutes (or bsc did not confirm): a module that needs one is off path A\n\
\    (fail safe); this file states them so that its cost and structure can be measured. -/\n\
\structure Unconfirmed (ops : Ops V) (E : EqC V) : Prop where\n" ++ intercalate "\n" ugF ++ "\n" else "")
       ++ "\n\
\theorem meX (ops : Ops V) (E : EqC V) (oc : OC V) (hA : asmP ops E oc) (c p : Nat) {y : Nat}\n\
\    (hc : Nat.blt c nC = true) (hp : (chkLst.get c)[p]? = some y)\n\
\    (h1 : WF ops E oc (chkHead.get c) = true) (h2 : WF ops E oc y = true) : False :=\n\
\  me_excl (" ++ asmChk ec "hA" ++ " c hc) h1 (p := p) (by rw [List.getElem?_map, hp]; rfl) h2\n\
\theorem meG (ops : Ops V) (E : EqC V) (oc : OC V) (hA : asmP ops E oc) (g a b : Nat) {x y : Nat}\n\
\    (hg : Nat.blt g nG = true) (hx : (grpT.get g)[a]? = some x) (hy : (grpT.get g)[b]? = some y)\n\
\    (hab : Nat.blt a b = true) (h1 : WF ops E oc x = true) (h2 : WF ops E oc y = true) : False :=\n\
\  sufAsm_excl (hA.1 g hg) hx hy hab h1 h2\n\
\theorem dX (ops : Ops V) (E : EqC V) (hy : Hyps ops E) (oc : OC V) (d : Nat) (hd : Nat.blt d nD = true)\n\
\    (h1 : WF ops E oc (dAT.get d) = true) (h2 : WF ops E oc (dBT.get d) = true) : False :=\n\
\  disj_excl (hy.disj oc d hd) (cf_of_wf h1) (cf_of_wf h2)\n\
\end\n")
    return (M.union hgP ugP, length hyF + length unF, length hyF, length unF)
  where
    isJustS = not . isNothing
    chunks n xs = if null xs then [] else take n xs : chunks n (drop n xs)
