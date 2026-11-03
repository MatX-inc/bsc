-- | Acyclicity (read lists as filters of allOuts, ranks of path inputs) and
-- the built module's top theorem (mke2e.py lines 3608-4139).
module Lean.Sched.Emit.Acyc
    ( acycPlan, AcycPlan(..), emitAcyc, emitTop
    ) where

import Control.Monad (forM_, when, unless)
import Data.Array ((!))
import Data.List (intercalate, elemIndex)
import Data.Maybe (fromMaybe)
import qualified Data.Foldable as F (Foldable(toList))
import qualified Data.Map.Strict as M (Map, (!), elems, empty, findWithDefault, lookup, member, insert,
    fromList)
import qualified Data.Set as S (Set, member, empty, insert, toAscList)

import Lean.Print (andc, cut, fmt, listCong, stabproof, tab, tupcong, w, W)
import Lean.Sched.Emit.Ctx (ihelper, isUncond, kd, lportOf, mxKind, site, sitePort, step, EC(ecM, ecU, ecNR,
    ecMet, ecNS, ecWXM, ecMultiBlk, ecRDC, ecPortInfo, ecSelmux, ecKinds, ecHyp, ecHya, ecName, ecCfgLib),
    PInfo(piUsers, piQ, piArgk, piMus, piMargs))
import Lean.Sched.Kinds (KindDef(kdLib, kdAlwaysRdy, kdHasEn, kdPorts), PM(..))
import Lean.Sched.Model (mxRepr, MPort, MSite(sArg, sR, sPort, sCond), MStep(tGuard, tSites), MX(MTrue, MZ, MK,
    MXarg, MGK, MLit, MF, MSplit, MG), Model(mMTasks, mBlk, mByk))

-- | A child's rank: one bound, or FIFOL1's two (deq's, then enq's and clear's).
data Rank = R1 Int | R2 Int Int
    deriving (Eq)

data AcycPlan = AcycPlan { apRank :: M.Map (String, Int) Rank, apNoAcyc :: Bool, apNN :: Int, apAgNeed :: S.Set Int }

pathInputs :: [(String, [String])]
pathInputs = [ ("fifo", ["enq", "deq"]), ("rwS", ["wset"]), ("rwC", ["wset"]), ("rw0S", ["wset"]), ("rw0C", ["wset"])
             , ("rf", [ 's' : show c | c <- [0 .. 4 :: Int] ]) ]

pathOuts :: M.Map String [String]
pathOuts = M.fromList [ ("fifo", ["enq", "notFull", "i_notFull", "clear"]), ("rwS", ["wget", "whas"]), ("rwC", ["wget", "whas"])
                      , ("rw0S", ["whas"]), ("rw0C", ["whas"]), ("rf", [ 's' : show c | c <- [0 .. 4 :: Int] ]) ]

pmS :: PM -> String
pmS (PS s) = s
pmS (PT t) = show t

prk :: M.Map (String, Int) Rank -> MPort -> Int
prk rank (k, i, m)
    | k == "fifo" = let (b1, b2) = case M.lookup (k, i) rank of { Just (R2 a b) -> (a, b); _ -> (0, 0) }
                    in if pmS m == "clear" then b2 else if pmS m `elem` ["enq", "notFull", "i_notFull"] then b1 else 0
    | Just outs <- M.lookup k pathOuts, pmS m `elem` outs = case M.lookup (k, i) rank of { Just (R1 b) -> b; _ -> 0 }
    | otherwise = 0

ereads :: EC -> Int -> MX -> [MPort]
ereads ec k e = case e of
    MF _ js -> map (sitePort ec k) js
    MG _ js -> map (sitePort ec k) js
    MLit j -> [sitePort ec k j]
    MSplit is a b -> concat [ map (sitePort ec k) js | i <- is, let MG _ js = fromMaybe (MG 0 []) (sR (site ec k i)) ] ++ ereads ec k a ++ ereads ec k b
    _ -> []

rdyG :: EC -> MPort -> Bool
rdyG ec (k, _, p) = not (kdAlwaysRdy (kd ec k) p)

guardOK :: EC -> Int -> Maybe MX
guardOK ec k = case tGuard (step ec k) of
    Just g | mxKind g `notElem` ["check", "false"] -> Just g
    Nothing -> Just MTrue
    _ -> Nothing

wfReads :: EC -> Int -> [MPort]
wfReads ec k = [ sPort st | st <- F.toList (tSites (step ec k)), rdyG ec (sPort st) ]
               ++ maybe [] (ereads ec k) (guardOK ec k)
               ++ concat [ ereads ec k (sCond (site ec k i)) | i <- ecRDC ec ! k ]
               ++ concat [ wfReads ec b | b <- M.findWithDefault [] k (mBlk (ecM ec)) ]

inputReads :: EC -> MPort -> [MPort]
inputReads ec p@(k, _, m) = case M.lookup p (ecPortInfo ec) of
    Just pi | not (null (piUsers pi)) ->
        let en = kdHasEn (kd ec k) m || piArgk pi == "mux"
        in concat [ (if en then wfReads ec st else [])
                    ++ (if piArgk pi /= "none" then ereads ec st (sArg s) else [])
                    ++ (if en && not (isUncond s) then ereads ec st (sCond s) else [])
                  | (st, si) <- piUsers pi, let s = site ec st si ]
    _ -> []

acycPlan :: EC -> AcycPlan
acycPlan ec = AcycPlan rank noAcyc nn agneed
  where
    m = ecM ec
    kinds = ecKinds ec
    nInst k = length (M.findWithDefault [] k (mByk m))
    insts = [ (k, i, ms) | (k, ms) <- pathInputs, k `elem` kinds, i <- [0 .. nInst k - 1] ]
    pass r0 = foldl (\(r, ch) (k, i, ms) ->
                        let fan mm = maximum ((-1) : [ prk r y | y <- inputReads ec (k, i, PS mm) ])
                            b = if k == "fifo" then let b1 = fan "deq" + 1 in R2 b1 (max b1 (fan "enq" + 1))
                                else R1 (maximum (map fan ms) + 1)
                            old = M.findWithDefault (if k == "fifo" then R2 0 0 else R1 0) (k, i) r
                        in if old /= b then (M.insert (k, i) b r, True) else (r, ch)) (r0, False) insts
    iter n r | n >= (1000 :: Int) = Nothing
             | otherwise = let (r', ch) = pass r in if ch then iter (n + 1) r' else Just r'
    (rank0, loopStop) = case iter 0 M.empty of { Just r -> (r, False); Nothing -> (M.empty, True) }
    methDriven = or [ st >= ecNR ec | (k, i, ms) <- insts, mm <- ms, (st, _) <- M.findWithDefault [] (k, i, PS mm) (ecU ec) ]
    noAcyc = loopStop || methDriven
    rank = rank0
    nn = maximum (0 : [ case v of { R1 b -> b; R2 a b -> max a b } | v <- M.elems rank ]) + 1
    blk = mBlk m
    agneed = foldl (\s k0 -> closeBlk (S.insert k0 s) [k0]) S.empty
                   [ st | (k, i, ms) <- insts, mm <- ms, Just pi <- [M.lookup (k, i, PS mm) (ecPortInfo ec)], not (null (piUsers pi))
                        , kdHasEn (kd ec k) (PS mm) || piArgk pi == "mux", (st, _) <- piUsers pi ]
    closeBlk s [] = s
    closeBlk s (x : xs) = let new = [ b | b <- M.findWithDefault [] x blk, not (S.member b s) ]
                          in closeBlk (foldr S.insert s new) (new ++ xs)

agx :: EC -> Int -> MX -> (MPort -> String) -> String
agx ec k e rd = case e of
    MZ -> "rfl"; MK _ -> "rfl"; MXarg -> "rfl"; MTrue -> "rfl"; MGK _ -> "rfl"
    MLit j -> "congrArg ops.toB (" ++ rd (sitePort ec k j) ++ ")"
    MF n js -> "congrArg (ops.f " ++ show n ++ ") (" ++ tupcong (map (rd . sitePort ec k) js) ++ ")"
    MG n js -> "congrArg (ops.g " ++ show n ++ ") (" ++ tupcong (map (rd . sitePort ec k) js) ++ ")"
    MSplit is a b ->
        let rs = andc [ "congrArg (ops.g " ++ show n ++ ") (" ++ tupcong (map (rd . sitePort ec k) js) ++ ")"
                      | i <- is, let MG n js = fromMaybe (MG 0 []) (sR (site ec k i)) ]
        in "cond_congr (" ++ rs ++ ") (" ++ agx ec k a rd ++ ") (" ++ agx ec k b rd ++ ")"
    _ -> error ("schedlean: agx: " ++ mxRepr e)

-- | Acyc; returns False when the module has none (the file then ends after obl).
emitAcyc :: EC -> AcycPlan -> W ()
emitAcyc ec ap = do
    let m = ecM ec
        kinds = ecKinds ec
        met = ecMet ec
        rank = apRank ap
        nn = apNN ap
        nS = ecNS ec
        wxm = ecWXM ec
        blk = mBlk m
        multi = ecMultiBlk ec
        nInst k = length (M.findWithDefault [] k (mByk m))
        mxW k = maximum (0 : map (prk rank) (wfReads ec k))
        mxRD k = maximum (0 : [ prk rank (sPort st) | st <- F.toList (tSites (step ec k)), rdyG ec (sPort st) ])
        rkf = M.fromList [ ("fifo", "fifoRk"), ("rwS", "wireRk"), ("rwC", "wireRk"), ("rw0S", "wire0Rk"), ("rw0C", "wire0Rk"), ("rf", "rfRk") ]
        lport' k mm = lportOf ec k (PS mm)
        pins k = fromMaybe [] (lookup k pathInputs)
    w ((if "fifo" `elem` kinds then "def fifoRk (b : Nat × Nat) : FL → Nat := fun m => match m with\n\
\  | .enq | .notFull | .i_notFull => b.1\n\
\  | .clear => b.2\n\
\  | _ => 0\n" else "") ++ (if "rf" `elem` kinds then "def rfRk (b : Nat) : RFM → Nat := fun m => match m with\n\
\  | .upd => 0\n\
\  | _ => b\n" else "") ++ "def wireRk (b : Nat) : WireM → Nat := fun m => match m with\n\
\  | .wget | .whas => b\n\
\  | _ => 0\n\
\def wire0Rk (b : Nat) : Wire0M → Nat := fun m => match m with\n\
\  | .whas => b\n\
\  | _ => 0\n")
    forM_ kinds $ \k -> when (M.member k rkf) $
        if k == "fifo"
            then w ("def bnd_fifo : Tab (Nat × Nat) :=\n  " ++ tab [ let R2 a b = M.findWithDefault (R2 0 0) ("fifo", i) rank in "(" ++ show a ++ ", " ++ show b ++ ")" | i <- [0 .. nInst k - 1] ])
            else w ("def bnd_" ++ k ++ " : Tab Nat :=\n  " ++ tab [ let R1 b = M.findWithDefault (R1 0) (k, i) rank in show b | i <- [0 .. nInst k - 1] ])
    w ("def rkK : (κ : Kind) → Nat → (KI κ).M → Nat\n" ++ concat [ case M.lookup k rkf of
                                                                      Just f -> "  | ." ++ k ++ " => fun i m => " ++ f ++ " (bnd_" ++ k ++ ".get i) m\n"
                                                                      Nothing -> "  | ." ++ k ++ " => fun _ _ => 0\n" | k <- kinds ])
    w "def rk (y : Σ j : I, (CI j).M) : Nat := rkK y.1.1 y.1.2.val y.2"
    w ("def mxRDT : Tab Nat :=\n  " ++ tab [ show (mxRD k) | k <- [0 .. nS - 1] ])
    w ("def allOuts : List (Σ j : I, (CI j).M) := allPorts [" ++ intercalate ", " (map ('.' :) kinds) ++ "] nk (fun κ => (KI κ).M) portsK\n\
\theorem mem_allOuts (y : Σ j : I, (CI j).M) : y ∈ allOuts :=\n\
\  mem_allPorts _ nk _ portsK (fun κ => by cases κ <;> simp) (fun κ m => by\n\
\    cases κ\n" ++ concat [ "    · exact ports_" ++ k ++ " m\n" | k <- kinds ] ++ "  ) y\n"
       ++ (if met then "def allIns : List (Fin nM) := List.finRange nM\ntheorem mem_allIns (p : Fin nM) : p ∈ allIns := List.mem_finRange p\n"
           else "def allIns : List Empty := []\ntheorem mem_allIns (p : Empty) : p ∈ allIns := p.elim\n")
       ++ "def outsBelow (r : Nat) : List (Σ j : I, (CI j).M) := allOuts.filter (fun y => Nat.blt (rk y) r)\n\
\def DK : (κ : Kind) → Nat → (KI κ).M → List (Σ j : I, (CI j).M)\n")
    forM_ kinds $ \k -> w $ case k of
        "fifo" -> "  | .fifo => fun i m => match m with | .enq => outsBelow (bnd_fifo.get i).2 | .deq => outsBelow (bnd_fifo.get i).1 | _ => allOuts"
        _ | k `elem` ["rwS", "rwC", "rw0S", "rw0C"] -> "  | ." ++ k ++ " => fun i m => match m with | .wset => outsBelow (bnd_" ++ k ++ ".get i) | _ => allOuts"
        "rf" -> "  | .rf => fun i m => match m with | .upd => allOuts | _ => outsBelow (bnd_rf.get i)"
        _ -> "  | ." ++ k ++ " => fun _ _ => allOuts"
    when met $
        w ("/-- A child input reached by one of its paths reads no parent input (no method drives one). -/\n\
\def pinK : (κ : Kind) → (KI κ).M → Bool\n" ++ concat [ case k of
                                                    "rf" -> "  | .rf => fun m => m.isS\n"
                                                    "fifo" -> "  | .fifo => fun m => match m with | .enq | .deq => true | _ => false\n"
                                                    _ | k `elem` ["rwS", "rwC", "rw0S", "rw0C"] -> "  | ." ++ k ++ " => fun m => match m with | .wset => true | _ => false\n"
                                                    _ -> "  | ." ++ k ++ " => fun _ => false\n" | k <- kinds ]
           ++ "def DwK (κ : Kind) (m : (KI κ).M) : List (Fin nM) := bif pinK κ m then [] else allIns\n\
\theorem pin_all (κ : Kind) (p q : (KI κ).M) (h : (KI κ).ann.path p q = true) : pinK κ p = true := by\n\
\  cases κ\n" ++ concat [ if k == "reg" then "  · rw [reg_nopath] at h; cases h\n" else "  · cases p <;> cases q <;> first | rfl | cases h\n" | k <- kinds ])
    w rdBndText
    let agOrder = let go (seen, out) k | S.member k seen = (seen, out)
                                       | otherwise = let (s', o') = foldl go (seen, out) (M.findWithDefault [] k blk) in (S.insert k s', o' ++ [k])
                  in snd (foldl go (S.empty, []) (S.toAscList (apAgNeed ap)))
        oreadAg (k, i, mm) hb = fmt "agO hR (%s %d) %s (blt_of_le_blt (Nat.le_of_ble_eq_true rfl) %s)" [ihelper k, show i, lportOf ec k mm, hb]
        rdyAg (k, i, mm) hb = fmt "agR hR (%s %d) %s (blt_of_le_blt (Nat.le_of_ble_eq_true rfl) %s)" [ihelper k, show i, lportOf ec k mm, hb]
    forM_ agOrder $ \k -> do
        let rd p = oreadAg p "hb"
            g0 = agx ec k (fromMaybe MTrue (tGuard (step ec k))) rd
            rdc = ecRDC ec ! k
            gag = if null rdc then g0
                  else "congr2 and (" ++ g0 ++ ") (" ++ andc ([ "congr2 or (congrArg not (" ++ agx ec k (sCond (site ec k i)) rd ++ ")) (" ++ rdyAg (sitePort ec k i) "hb" ++ ")" | i <- rdc ] ++ ["rfl"]) ++ ")"
            bl = [ "agWF_" ++ show b ++ " ops E hR (blt_of_le_blt (Nat.le_of_ble_eq_true rfl) hb)" | b <- M.findWithDefault [] k blk ]
            bag | not (M.member k blk) = "rfl"
                | multi = "congrArg (fun l => !orL l) (" ++ listCong bl ++ ")"
                | otherwise = "congrArg not (" ++ head bl ++ ")"
        w (fmt "theorem agWF_%d (ops : Ops V) (E : EqC V) {oc oc' : OC V} {b : Nat}\n\
\    (hR : ∀ y, Nat.blt (rk y) b = true → AgreeO (oc y.1) (oc' y.1) y.2) (hb : Nat.blt %d b = true) :\n\
\    WF ops E oc %d = WF ops E oc' %d :=\n\
\  congr2 and (congr2 and (agRD hR %d (by decide) (blt_of_le_blt (Nat.le_of_ble_eq_true rfl) hb)) (%s)) (%s)"
               [show k, show (mxW k), show k, show k, show k, gag, bag])
    let oreadDq (k, i, mm) = fmt "agO hR (%s %d) %s rfl" [ihelper k, show i, lportOf ec k mm]
        dqPort p@(k, _, mm) = case M.lookup p (ecPortInfo ec) of
            Just pi | not (null (piUsers pi)) ->
                let en = if kdHasEn (kd ec k) mm
                         then "congrArg orL (" ++ listCong [ if isUncond s then "agWF_" ++ show st ++ " ops E hR rfl"
                                                             else "congr2 and (agWF_" ++ show st ++ " ops E hR rfl) (" ++ agx ec st (sCond s) oreadDq ++ ")"
                                                           | (st, si) <- piUsers pi, let s = site ec st si ] ++ ")"
                         else "rfl"
                    sel st s = if M.findWithDefault False (piQ pi) (ecSelmux ec) && not (isUncond s)
                               then "congr2 and (agWF_" ++ show st ++ " ops E hR rfl) (" ++ agx ec st (sCond s) oreadDq ++ ")"
                               else "agWF_" ++ show st ++ " ops E hR rfl"
                    arg = case piArgk pi of
                        "mux" -> "congrArg (fun l => pmux l ops.z) (" ++ listCong [ "congr2 Prod.mk (" ++ sel st s ++ ") (" ++ agx ec st (sArg s) oreadDq ++ ")"
                                                                                 | ((st, _), s) <- zip (piMus pi) (piMargs pi) ] ++ ")"
                        a | a `elem` ["single", "const"] -> let (st, si) = head (piUsers pi) in agx ec st (sArg (site ec st si)) oreadDq
                        _ -> "rfl"
                in "⟨" ++ en ++ ", " ++ arg ++ "⟩"
            _ -> "⟨rfl, rfl⟩"
        bterm k mm = if k == "fifo" then (if mm == "enq" then "(bnd_fifo.get i).2" else "(bnd_fifo.get i).1") else "bnd_" ++ k ++ ".get i"
    forM_ kinds $ \k -> when (k `elem` map fst pathInputs) $ do
        let n = nInst k
            ps = pins k
            hyp mm = fmt "(∀ y, Nat.blt (rk y) (%s) = true → AgreeO (oc y.1) (oc' y.1) y.2) → AgreeW (A V ops E oc w) (A V ops E oc' w') %s" [bterm k mm, lport' k mm]
        w (fmt "def DQ_%s (ops : Ops V) (E : EqC V) (i : Nat) (A : (V : Type) → Ops V → EqC V → OC V → %s → Wires (KI .%s).M V) : Prop :=\n\
\  ∀ (h : i < nk .%s) (oc oc' : OC V) (w w' : %s),\n    %s" [k, wxm, k, k, wxm, intercalate " ∧ " [ "(" ++ hyp mm ++ ")" | mm <- ps ]])
        forM_ [0 .. n - 1] $ \i -> do
            let parts = [ "fun hR => " ++ dqPort (k, i, PS mm) | mm <- ps ]
            w (fmt "theorem dq_%s_%d (ops : Ops V) (E : EqC V) : DQ_%s ops E %d arm_%s_%d := fun h oc oc' w w' =>\n  %s"
                   [k, show i, k, show i, k, show i, if length parts > 1 then "⟨" ++ intercalate ", " parts ++ "⟩" else head parts])
        prf <- stabproof ("arms_" ++ k) ("dqAll_" ++ k) "(ops : Ops V) (E : EqC V)" "ops E" (\nm lo hi -> fmt "Tab.AllR (DQ_%s ops E) %s %d %d" [k, nm, show lo, show hi])
                         [ "dq_" ++ k ++ "_" ++ show i ++ " ops E" | i <- [0 .. n - 1] ] True
        w (fmt "theorem dqAll_%s (ops : Ops V) (E : EqC V) : Tab.AllR (DQ_%s ops E) arms_%s 0 %d :=\n  %s" [k, k, k, show n, prf])
    w "end\n"
    let projSel ps mm = let i = fromMaybe 0 (elemIndex mm ps) in concat (replicate i ".2") ++ (if i < length ps - 1 then ".1" else "")
        da = if met then "dep_of_all (F ops E).base allOuts mem_allOuts allIns mem_allIns oc oc' w w' hD (by simpa [DwK, pinK] using hW) _ _"
             else "dep_of_all (F ops E).base allOuts mem_allOuts allIns mem_allIns oc oc' w w' hD hW _ _"
        depCase k = case lookup k pathInputs of
            Nothing -> "    | " ++ k ++ " => exact " ++ da ++ "\n"
            Just ps -> "    | " ++ k ++ " =>\n      cases p with\n"
                       ++ concat [ if pmS p `elem` ps
                                   then fmt "      | %s => exact (Tab.get_allR _ 0 _ (dqAll_%s ops E) i (Nat.zero_le _) hi hi oc oc' w w')%s (forall_filter allOuts mem_allOuts _ hD)\n"
                                            [pmS p, k, if length ps > 1 then projSel ps (pmS p) else ""]
                                   else "      | " ++ pmS p ++ " => exact " ++ da ++ "\n" | p <- kdPorts (kd ec k) ]
        rkokCase k = case lookup k pathInputs of
            Nothing -> if k == "reg" then "    | reg => rw [reg_nopath] at hpq; cases hpq\n" else "    | " ++ k ++ " => cases hpq\n"
            Just _ | k == "fifo" -> "    | fifo => cases p <;> cases q <;> first | exact blt_true (List.mem_filter.mp hy).2 | exact Nat.lt_of_lt_of_le (blt_true (List.mem_filter.mp hy).2) (fifoLe i hi) | cases hpq\n"
                   | otherwise -> "    | " ++ k ++ " => cases p <;> cases q <;> first | exact blt_true (List.mem_filter.mp hy).2 | cases hpq\n"
        rkltCase k = case lookup k pathInputs of
            Nothing -> "    | " ++ k ++ " => exact Nat.zero_lt_succ _\n"
            Just _ | k == "fifo" -> "    | fifo =>\n      have hb := Tab.allB_spec _ bnd_fifo 0 (nk .fifo) rkB_fifo i (Nat.zero_le _) hi\n\
\      simp only [Bool.and_eq_true] at hb\n\
\      cases m <;> first | exact Nat.zero_lt_succ _ | exact Nat.blt_eq.mp hb.1 | exact Nat.blt_eq.mp hb.2\n"
                   | otherwise -> fmt "    | %s =>\n      have hb := Tab.allB_spec _ bnd_%s 0 (nk .%s) rkB_%s i (Nat.zero_le _) hi\n\
\      cases m <;> first | exact Nat.zero_lt_succ _ | exact Nat.blt_eq.mp hb\n" [k, k, k, k]
    forM_ kinds $ \k -> when (k `elem` map fst pathInputs) $
        if k == "fifo"
            then do
                w (fmt "theorem rkB_fifo : Tab.allB (fun _ b => Nat.blt b.1 %d && Nat.blt b.2 %d) bnd_fifo 0 (nk .fifo) = true := by decide +kernel" [show nn, show nn])
                w "theorem fifoLeB : Tab.allB (fun _ b => Nat.ble b.1 b.2) bnd_fifo 0 (nk .fifo) = true := by decide +kernel"
                w "theorem fifoLe (i : Nat) (hi : i < nk .fifo) : (bnd_fifo.get i).1 ≤ (bnd_fifo.get i).2 :=\n  Nat.le_of_ble_eq_true (Tab.allB_spec _ bnd_fifo 0 (nk .fifo) fifoLeB i (Nat.zero_le _) hi)"
            else w (fmt "theorem rkB_%s : Tab.allB (fun _ b => Nat.blt b %d) bnd_%s 0 (nk .%s) = true := by decide +kernel" [k, show nn, k, k])
    w ("\nsection\nvariable {V : Type}\ndef acyc (ops : Ops V) (E : EqC V) : Acyc (F ops E).base where\n\
\  D j p := DK j.1 j.2.val p\n\
\  Dw " ++ (if met then "j p := DwK j.1 p" else "_ _ := []") ++ "\n\
\  Do " ++ (if met then "_ := allOuts" else "e := e.elim") ++ "\n\
\  Dwo " ++ (if met then "_ := []" else "e := e.elim") ++ "\n\
\  dep oc oc' w w' j p hD hW := by\n\
\    obtain ⟨κ, i, hi⟩ := j\n\
\    cases κ with\n" ++ concatMap depCase kinds ++ "  depo " ++ (if met then "oc oc' w w' q hD _ := by\n\
\    have e : oc = oc' := funext fun j => Outs.ext' fun q => hD ⟨j, q⟩ (mem_allOuts _)\n\
\    subst e; exact ⟨rfl, rfl⟩" else "_ _ _ _ q := q.elim") ++ "\n\
\  rk := rk\n\
\  N := " ++ show nn ++ "\n\
\  rk_lt y := by\n\
\    obtain ⟨⟨κ, i, hi⟩, m⟩ := y\n\
\    cases κ with\n" ++ concatMap rkltCase kinds ++ "  rk_ok j q p hpq y hy := by\n\
\    obtain ⟨κ, i, hi⟩ := j\n\
\    cases κ with\n" ++ concatMap rkokCase kinds ++ "  reach _ := []\n\
\  reach_w " ++ (if met then "j q p hpq p' hp := by\n\
\    have h := pin_all j.1 p q hpq\n\
\    simp only [DwK, h, cond_true] at hp\n\
\    cases hp" else "_ _ _ _ _ hp := by cases hp") ++ "\n\
\  reach_o _ _ _ _ _ _ _ hp := by cases hp\n\
\  path_w " ++ (if met then "_ _ hp := by cases hp" else "q := q.elim") ++ "\n\
\  path_o " ++ (if met then "_ _ _ _ hp := by cases hp" else "q := q.elim") ++ "\n\
\end\n")
    cut

emitTop :: EC -> W ()
emitTop ec = do
    let kinds = ecKinds ec
        hyp = ecHyp ec
        hya = ecHya ec
        name = ecName ec
        cfgLib = ecCfgLib ec
        assumed = M.fromList [ ("fifo", "FIFOL1"), ("cfg", "ConfigRegN"), ("sf", "SizedFIFO"), ("rf", "RegFile") ]
        leaves = [ assumed M.! k | k <- kinds, M.member k assumed, not (k == "cfg" && cfgLib) ]
    unless (and [ M.member k assumed || kdLib (kd ec k) || (k == "cfg" && cfgLib) | k <- kinds, k /= "tasks" ]) $
        error "schedlean: AssertionError: a child that is neither a library model nor an assumed primitive leaf"
    let leavesDoc = if null leaves then "No assumed leaf: every child is a library model with a proved contract."
                    else "The assumed leaves (primitives whose library contracts are not yet proved): " ++ intercalate ", " leaves ++ "."
        impl k = case k of
            "reg" -> "Reg"; "cfg" -> if cfgLib then "LC.CfgReg" else "lv.cfg"; "fifo" -> "lv.fifo"; "tasks" -> "Tasks nT"
            "f2" -> "FIFO2 ops.ofBool"; "sf" -> "lv.sf"; "rf" -> "lv.rf"; "rwS" -> "RWire ops.ofBool true true"
            "rwC" -> "RWire ops.ofBool true false"; "rw0S" -> "RWire0 ops.ofBool true true"; "rw0C" -> "RWire0 ops.ofBool true false"
            _ -> error ("schedlean: no implementation for kind " ++ k)
        leaf k = case k of
            "cfg" | cfgLib -> "Built.leaf _ LC.cfg_realizes LC.cfg_causal"
            "reg" -> "Built.leaf _ reg_realizes (creg_causal 1)"
            "cfg" -> "Built.leaf _ lv.cfgR lv.cfgC"
            "fifo" -> "Built.leaf _ lv.fifoR lv.fifoC"
            "tasks" -> "Built.leaf _ (tasks_realizes nT) (tasks_causal nT)"
            "f2" -> "Built.leaf _ (fifo2_realizes _) (fifo2_causal _)"
            "sf" -> "Built.leaf _ lv.sfR lv.sfC"
            "rf" -> "Built.leaf _ lv.rfR lv.rfC"
            _ | k `elem` ["rwS", "rwC"] -> "Built.leaf _ (rwire_realizes _ _ _) (rwire_causal _ _ _)"
            _ -> "Built.leaf _ (rwire0_realizes _ _ _) (rwire0_causal _ _ _)"
        alias k = case k of
            "f2" -> "(fifo2_aliased ops.ofBool ops.toB hy.tob).toU"
            "sf" -> "lv.sfA.toU"
            "reg" -> "aliased_of_none _ _ (fun p q => rfl) (fun a p => rfl)"
            "tasks" -> "aliased_of_none _ _ (fun p q => rfl) (fun a p => rfl)"
            _ -> "aliased_of_none _ _ (fun p q => by cases p <;> cases q <;> rfl) (fun a p => by cases a <;> cases p <;> rfl)"
        topm = "((F ops E).toModuleU (fun j => (implK ops lv j.1).toU))"
        note = concatMap (\c -> c) (replaceNL (mtaskNoteE ec))
    w ("/-! ## The built module and its top theorem -/\n\
\\n\
\/-- " ++ leavesDoc ++ " -/\n\
\structure Leaves (V : Type) (toB : V → Bool) where\n"
       ++ (if "fifo" `elem` kinds then "  fifo : Module V FL.X\n  fifoR : Realizes fifo\n  fifoC : Causal fifo\n" else "")
       ++ (if "cfg" `elem` kinds && not cfgLib then "  cfg : Module V Cfg.X\n  cfgR : Realizes cfg\n  cfgC : Causal cfg\n" else "")
       ++ (if "sf" `elem` kinds then "  sf : Module V FIFO2.X\n  sfR : Realizes sf\n  sfC : Causal sf\n  sfA : Aliased toB sf\n" else "")
       ++ (if "rf" `elem` kinds then "  rf : Module V RF.X\n  rfR : Realizes rf\n  rfC : Causal rf\n" else "")
       ++ "  dummy : Unit := ()\n\
\\n\
\section\n\
\variable {V : Type}\n\
\def implK (ops : Ops V) (lv : Leaves V ops.toB) : (κ : Kind) → Module V (KI κ)\n")
    forM_ kinds $ \k -> w ("  | ." ++ k ++ " => " ++ impl k)
    w (fmt "\n\
\theorem built [Inhabited V] (ops : Ops V) (E : EqC V) %s (lv : Leaves V ops.toB) :\n\
\    Built X %s :=\n\
\  Built.node (F ops E) (fun j => (implK ops lv j.1).toU)\n\
\    (fun ⟨κ, _⟩ => by\n\
\      cases κ\n" [hyp, topm] ++ concat [ "      · exact " ++ leaf k ++ "\n" | k <- kinds ] ++ "    )\n\
\    (fun ⟨κ, _⟩ => by\n\
\      cases κ\n" ++ concat [ "      · exact " ++ alias k ++ "\n" | k <- kinds ] ++ fmt "    )\n\
\    (obl ops E %s) (acyc ops E)\n\
\\n\
\/-- **The module's top theorem**: the contract, with the run fixed before every skipped value\n\
\    of the tree, and every input has a hardware cycle.%s -/\n\
\theorem top [Inhabited V] (ops : Ops V) (E : EqC V) %s (lv : Leaves V ops.toB) :\n\
\    RealizesU %s ∧\n\
\    ∀ s w, %s.clean s = s → ∃ o s' g, %s.hw s w o s' g :=\n\
\  (built ops E %s lv).top\n\
\end\n\
\\n\
\end\n\
\end %s\n\
\\n\
\#print axioms %s.obl\n\
\#print axioms %s.top\n" [hya, note, hyp, topm, topm, topm, hya, name, name, name])
  where
    replaceNL s = case s of
        ('\n' : ' ' : ' ' : ' ' : r) -> "\n    " : replaceNL r
        (c : r) -> [c] : replaceNL r
        [] -> []

mtaskNoteE :: EC -> String
mtaskNoteE ec = case mMTasks (ecM ec) of
    [] -> ""
    ts -> "\n   Task calls inside interface methods (bsc G0020) are left out of task agreement: "
          ++ intercalate ", " [ a ++ " (" ++ show n ++ ")" | (a, n) <- ts ] ++ "."

rdBndText :: String
rdBndText = "\n\
\theorem rdBnd : Tab.allB (fun k keys => Nat.beq (kT.get k) 0 ||\n\
\    Tab.allB (fun i p => alwaysRdyB p || Nat.ble (rk p) (mxRDT.get k)) keys 0 (kT.get k)) keyT 0 nS = true := by\n\
\  decide +kernel\n\
\theorem rdB (k : Nat) (hk : k < nS) : ∀ i, i < kR k → alwaysRdyB ((keyT.get k).get i) = false →\n\
\    rk ((keyT.get k).get i) ≤ mxRDT.get k := by\n\
\  intro i hi ha\n\
\  have h := sitesB_spec (fun k i p => alwaysRdyB p || Nat.ble (rk p) (mxRDT.get k)) rdBnd k hk i hi\n\
\  simp only [ha, Bool.false_or] at h\n\
\  exact Nat.le_of_ble_eq_true h\n\
\\n\
\theorem all_congr' {α : Type} {f g : α → Bool} : ∀ (l : List α), (∀ a ∈ l, f a = g a) → l.all f = l.all g\n\
\  | [], _ => rfl\n\
\  | a :: l, h => by\n\
\    simp only [List.all_cons]\n\
\    rw [h a (List.mem_cons_self ..), all_congr' l (fun b hb => h b (List.mem_cons_of_mem _ hb))]\n\
\\n\
\section\n\
\variable {V : Type}\n\
\theorem agO {oc oc' : OC V} {b : Nat} (hR : ∀ y, Nat.blt (rk y) b = true → AgreeO (oc y.1) (oc' y.1) y.2)\n\
\    (j : I) (m : (CI j).M) (h : Nat.blt (rk ⟨j, m⟩) b = true) : (oc j).ret m = (oc' j).ret m := (hR ⟨j, m⟩ h).2\n\
\theorem agR {oc oc' : OC V} {b : Nat} (hR : ∀ y, Nat.blt (rk y) b = true → AgreeO (oc y.1) (oc' y.1) y.2)\n\
\    (j : I) (m : (CI j).M) (h : Nat.blt (rk ⟨j, m⟩) b = true) : (oc j).rdy m = (oc' j).rdy m := (hR ⟨j, m⟩ h).1\n\
\\n\
\theorem agRD {oc oc' : OC V} {b : Nat} (hR : ∀ y, Nat.blt (rk y) b = true → AgreeO (oc y.1) (oc' y.1) y.2)\n\
\    (k : Nat) (hk : k < nS) (hb : Nat.blt (mxRDT.get k) b = true) : RD oc k = RD oc' k := by\n\
\  unfold RD\n\
\  apply all_congr'\n\
\  intro i hi\n\
\  have hi' : i < kR k := by simp at hi; omega\n\
\  cases ha : alwaysRdyB ((keyT.get k).get i)\n\
\  · rw [(hR _ (blt_of_le_blt (rdB k hk i hi' ha) hb)).1]\n\
\  · rfl\n"
