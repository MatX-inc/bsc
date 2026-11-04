-- | The muxes' one-hot chains, the scan's tables and segments, the glue and
-- the escaped pairs (mke2e.py lines 2426-3137; C18, C20, C24, C27, C29).
module Lean.Sched.Emit.ScanTabs
    ( emitChains, emitScan, Counts(..)
    ) where

import Control.Monad (forM, forM_, when, unless)
import Data.Array (elems, (!), listArray)
import Data.Bits (Bits(shiftL, shiftR, (.|.)))
import Data.List (elemIndex, intercalate, sort)
import Data.Maybe (fromMaybe, isNothing, isJust)
import qualified Data.IntMap.Strict as IM (findWithDefault, toList, fromListWith, insertWith)
import qualified Data.Map.Strict as M (null, Map, (!), elems, findWithDefault, keys, lookup, size, toList,
    fromList, fromListWith)
import qualified Data.Set as S (member, empty, fromList, intersection, null, singleton, size, toAscList,
    toList, union)

import Lean.Print (allBThm, allL, cut, fmt, splitN, splitW, stab, stabKeyed, stabproof, tab, taball, tabproof,
    w, W)
import Lean.Sched.Bounds (defaultBounds, Bounds(bnGateSeg, bnGateSlots, bnGateCodes, bnGateStep,
    bnGatePortUsers, bnGateGroup, bnGateLcUsers, bnGateEsc, bnGateEscFacts, bnGateModFacts, bnGateStruct,
    bnGateSelmux))
import Lean.Sched.Emit.Ctx (asmChk, condVal, entry, excl, exported, isUncond, kd, site, step, EC(ecM, ecLiti, ecCFIdx, ecNM,
    ecNT, ecHyp, ecHya, ecMet, ecLCP, ecDn, ecKinds, ecU, ecMuxes, ecSelmux, ecVirt, ecESC, ecNeed, ecEsc,
    ecVB, ecMann, ecD, ecMgrp, ecNSlots, ecGroups, ecEFacts, ecWXM, ecNR, ecExport, ecSynth), VCtx(VCtx))
import Lean.Sched.Export (isSynthKind, Export(exNS), SynthKid(skExport, skOff))
import Lean.Sched.Kinds (KindDef(kdLPorts, kdHasEn, kdMType))
import Lean.Sched.Model (exclSrc, DView(dvCF), MSite, MStep(tName, tSites), Model(mDV, mChecks, mTStart,
    mRules))
import Lean.Sched.Scan (mannExport, mannPlain, runScanStates, scanDesign, Ent, MAnn, ScanDesign(scD, scKt, scFpM, scFpR),
    St)

-- | What the summary line and the bounds need from the earlier sections.
data Counts = Counts { cnChkPairs, cnUncov :: Int }

argsite :: EC -> Int -> Int -> MSite
argsite = site

wfxOf :: EC -> Int -> Int -> String -> String
wfxOf ec k si h = if isUncond (site ec k si) then h else "((Bool.and_eq_true _ _).mp " ++ h ++ ").1"

cxOf :: EC -> Int -> Int -> String -> [String]
cxOf ec k si h = if isUncond (site ec k si) then [] else ["((Bool.and_eq_true _ _).mp " ++ h ++ ").2"]

exclD :: EC -> Int -> Int -> String
exclD ec x y = excl ec x y "hA" "hy" "h1" "h2"

emitChains :: EC -> W Counts
emitChains ec = do
    let hyp = ecHyp ec
        hya = ecHya ec
        nR = ecNR ec
        m = ecM ec
        cvq = "fun e => ∀ y, e.2.testBit y = true → WF ops E oc e.1 = true → WF ops E oc y = true → False"
        liti = ecLiti ec
    w "/-! ## The muxes' one-hot chains, from the exclusions -/\n"
    w "section\nvariable {V : Type}\n"
    w ("/-- A run-time check excludes its head with every rule of its list. -/\n\
\theorem chkC (ops : Ops V) (E : EqC V) (oc : OC V) (hA : asmP ops E oc) (c : Nat) (hc : Nat.blt c nC = true) :\n\
\    ∀ y ∈ chkLst.get c, WF ops E oc (chkHead.get c) = true → WF ops E oc y = true → False :=\n\
\  forall_of_check (" ++ asmChk ec "hA" ++ " c hc)\n")
    cs <- forM (zip [0 :: Int ..] (ecMuxes ec)) $ \(q, (_, us, plan)) -> do
        let ks = map fst us
            n = length ks
            sel = M.findWithDefault False q (ecSelmux ec)
            gq0 = M.lookup q (ecMgrp ec)
            qs = show q
        unless (all (< nR) ks) $ error "schedlean: AssertionError: a mux with method users"
        if sel && maybe False (\(_, g) -> length g == n) gq0 then do
            let Just (g0, gks) = gq0
            unless (gks == ks) $ error "schedlean: a group mux out of order"
            stab ("gsT_" ++ qs) "Tab Nat" (map show ks)
            let selq = "(fun (t : Nat) (e : (V : Type) → Ops V → EqC V → OC V → Bool × V) => (e V ops E oc).1 = true → WF ops E oc (gsT_" ++ qs ++ ".get t) = true)"
            prf <- stabproof ("mT_" ++ qs) ("sel_" ++ qs) "(ops : Ops V) (E : EqC V) (oc : OC V)" "ops E oc"
                             (\nm lo hi -> fmt "Tab.AllR %s %s %d %d" [selq, nm, show lo, show hi])
                             [ fmt "Tab.one (Q := %s) (i := %d) (a := mT_%d.get %d) (%s)"
                                   [selq, show t, qs, show t, if isUncond (argsite ec k si) then "fun h => h" else "fun h => ((Bool.and_eq_true _ _).mp h).1"]
                             | (t, (k, si)) <- zip [0 :: Int ..] us ] False
            w (fmt "theorem sel_%d (ops : Ops V) (E : EqC V) (oc : OC V) : Tab.AllR %s mT_%d 0 %d :=\n  %s" [qs, selq, qs, show n, prf])
            w (fmt "theorem chain_%d (ops : Ops V) (E : EqC V) %s (oc : OC V) (hA : asmP ops E oc) :\n\
\    amoChain false ((mlist_%d V ops E oc).map Prod.fst) := by\n\
\  have hg := amoChain_of_grp (fun k => WF ops E oc k) ((List.range' 0 %d).map gsT_%d.get) (grpT.get %d) (by decide +kernel) (hA.1 %d rfl)\n\
\  rw [List.map_map] at hg\n\
\  have h := amoChain_le_range (fun t => ((mT_%d.get t) V ops E oc).1) (fun t => WF ops E oc (gsT_%d.get t)) %d 0\n\
\    false false id (fun t _ ht => Tab.get_allR _ 0 %d (sel_%d ops E oc) t (Nat.zero_le _) (by omega)) hg\n\
\  unfold mlist_%d\n\
\  rw [List.map_map]\n\
\  exact h" [qs, hyp, qs, show n, qs, show g0, show g0, qs, qs, show n, show n, qs, qs])
            return (n * (n - 1) `div` 2, 0)
        else if sel then do
            let sis = M.fromList us
            w (fmt "def selq_%d (V : Type) (ops : Ops V) (E : EqC V) (oc : OC V) (k : Nat) : Bool :=\n  %s"
                   [qs, concat [ "bif Nat.beq k " ++ show k ++ " then " ++ entry ec k (site ec k (sis M.! k)) ++ " else " | k <- ks ] ++ "false"])
            let rows = [ (x, tl, prs)
                       | (a, x) <- zip [0 ..] ks, let tl = drop (a + 1) ks
                       , let prs = [ if isJust (exclSrc m x y)
                                     then "fun h1 h2 => " ++ excl ec x y "hA" "hy" (wfxOf ec x (sis M.! x) "h1") (wfxOf ec y (sis M.! y) "h2")
                                     else let (nfi, how) = cfIndex ec (q, x, y)
                                          in "fun h1 h2 => " ++ (if isNothing how then "hr" else "hy") ++ ".cx_" ++ show nfi ++ " oc "
                                             ++ (if how == Just "assert" then "hA " else "")
                                             ++ unwords ([wfxOf ec x (sis M.! x) "h1"] ++ cxOf ec x (sis M.! x) "h1" ++ [wfxOf ec y (sis M.! y) "h2"] ++ cxOf ec y (sis M.! y) "h2")
                                   | y <- tl ] ]
            stab ("msT_" ++ qs) "Tab (Nat × Nat)" [ "(" ++ show x ++ ", cvS [] [" ++ intercalate ", " (map show tl) ++ "])" | (x, tl, _) <- rows ]
            let cvs = "fun e => ∀ y, e.2.testBit y = true → selq_" ++ qs ++ " V ops E oc e.1 = true → selq_" ++ qs ++ " V ops E oc y = true → False"
            prf <- stabproof ("msT_" ++ qs) ("cv_" ++ qs) ("(ops : Ops V) (E : EqC V) " ++ hyp ++ " (oc : OC V) (hA : asmP ops E oc)")
                             ("ops E " ++ hya ++ " oc hA") (\nm _ _ -> "Tab.All (" ++ cvs ++ ") " ++ nm)
                             [ "cover_of (fun y hy => absurd hy (List.not_mem_nil)) " ++ allL prs | (_, _, prs) <- rows ] False
            w (fmt "theorem chain_%d (ops : Ops V) (E : EqC V) %s (oc : OC V) (hA : asmP ops E oc) :\n\
\    amoChain false ((mlist_%d V ops E oc).map Prod.fst) :=\n\
\  amoChain_of_cover (fun k => selq_%d V ops E oc k) (fun x y => selq_%d V ops E oc x = true → selq_%d V ops E oc y = true → False)\n\
\    (fun _ _ h => h) msT_%d [%s] (by decide +kernel) (by decide +kernel)\n    %s"
                   [qs, hyp, qs, qs, qs, qs, qs, intercalate ", " (map show ks), prf])
            return (0, sum [ length tl | (_, tl, _) <- rows ])
        else do
            let gq = gq0
                gs = maybe [] snd gq
                ksr = take (n - length gs) ks
            unless (drop (n - length gs) ks == gs) $ error "schedlean: a mux's group members are not last"
            let p0 = length gs * (length gs - 1) `div` 2
            if isJust gq && null ksr then do
                let Just (g0, _) = gq
                w (fmt "def gsm_%d : List Nat := [%s]" [qs, intercalate ", " (map show gs)])
                w (fmt "theorem chain_%d (ops : Ops V) (E : EqC V) %s (oc : OC V) (hA : asmP ops E oc) :\n\
\    amoChain false ((mlist_%d V ops E oc).map Prod.fst) :=\n\
\  amoChain_of_grp (fun k => WF ops E oc k) gsm_%d (grpT.get %d) (by decide +kernel) (hA.1 %d rfl)" [qs, hyp, qs, qs, show g0, show g0])
                return (p0, 0)
            else do
                let rows = [ (x, cb, uncl, length tl)
                           | (a, x) <- zip [0 ..] ksr, let tl = drop (a + 1) ks
                           , let (cb, unc) = M.findWithDefault (Nothing, S.empty) x plan
                                 uncl = [ y | y <- tl, S.member y unc ] ]
                    pfs = [ let pw = allL [ "fun h1 h2 => " ++ exclD ec x y | y <- uncl ]
                            in case cb of
                                 Nothing -> "cover_of (fun y hy => absurd hy (List.not_mem_nil)) " ++ pw
                                 Just c -> "cover_of (chkC ops E oc hA " ++ show (liti M.! c) ++ " rfl) " ++ pw
                          | (x, cb, uncl, _) <- rows ]
                    cvs = [ case cb of
                              Nothing -> "(" ++ show x ++ ", cvS [] [" ++ intercalate ", " (map show uncl) ++ "])"
                              Just c -> "(" ++ show x ++ ", cvS (chkLst.get " ++ show (liti M.! c) ++ ") [" ++ intercalate ", " (map show uncl) ++ "])"
                          | (x, cb, uncl, _) <- rows ]
                    unc = sum [ length uncl | (_, _, uncl, _) <- rows ]
                    chk = p0 + sum [ ntl - length uncl | (_, _, uncl, ntl) <- rows ]
                stab ("msT_" ++ qs) "Tab (Nat × Nat)" cvs
                prf <- stabproof ("msT_" ++ qs) ("cv_" ++ qs) ("(ops : Ops V) (E : EqC V) " ++ hyp ++ " (oc : OC V) (hA : asmP ops E oc)")
                                 ("ops E " ++ hya ++ " oc hA") (\nm _ _ -> "Tab.All (" ++ cvq ++ ") " ++ nm) pfs False
                case gq of
                    Just (g0, _) -> do
                        w (fmt "def gsm_%d : List Nat := [%s]" [qs, intercalate ", " (map show gs)])
                        w (fmt "theorem chain_%d (ops : Ops V) (E : EqC V) %s (oc : OC V) (hA : asmP ops E oc) :\n\
\    amoChain false ((mlist_%d V ops E oc).map Prod.fst) :=\n\
\  amoChain_of_cover_grp (fun k => WF ops E oc k) msT_%d [%s] gsm_%d (grpT.get %d) (by decide +kernel) (by decide +kernel)\n\
\    (%s) (by decide +kernel) (hA.1 %d rfl)" [qs, hyp, qs, qs, intercalate ", " (map show ksr), qs, show g0, prf, show g0])
                    Nothing ->
                        w (fmt "theorem chain_%d (ops : Ops V) (E : EqC V) %s (oc : OC V) (hA : asmP ops E oc) :\n\
\    amoChain false ((mlist_%d V ops E oc).map Prod.fst) :=\n\
\  amoChain_of_cover (fun k => WF ops E oc k) (fun x y => WF ops E oc x = true → WF ops E oc y = true → False)\n\
\    (fun _ _ h => h) msT_%d [%s] (by decide +kernel) (by decide +kernel)\n    %s" [qs, hyp, qs, qs, intercalate ", " (map show ks), prf])
                return (chk, unc)
    w "end\n"
    cut
    return (Counts (sum (map fst cs)) (sum (map snd cs)))

cfIndex :: EC -> (Int, Int, Int) -> (Int, Maybe String)
cfIndex ec key = M.findWithDefault (error ("schedlean: KeyError: no caller-exclusivity fact " ++ show key)) key (ecCFIdx ec)

bitsI :: Integer -> [Int]
bitsI = go 0
  where go _ 0 = []
        go i y = (if odd y then (i :) else id) (go (i + 1) (y `div` 2))

ents :: [Ent] -> String
ents l = "[" ++ intercalate ", " [ "(" ++ show a ++ ", " ++ show b ++ ", " ++ show c ++ ")" | (a, b, c) <- l ] ++ "]"

kc :: ([Int], [Int], [Int], [Int]) -> String
kc (a, b, c, d) = "⟨[" ++ l a ++ "], [" ++ l b ++ "], [" ++ l c ++ "], [" ++ l d ++ "]⟩"
  where l = intercalate ", " . map show

-- | scanlib.emit_tree: a scan state as the tree literal (subtrees of at most 512 set slots their own definitions).
emitTree :: St -> Int -> String -> String
emitTree st d name = intercalate "\n" (defs ++ ["noncomputable def " ++ name ++ " : ST := " ++ top])
  where
    keys = M.fromList [ (s `mod` (2 ^ d), v) | (s, v) <- IM.toList st ]
    nl x = "(nat_lit " ++ show x ++ ")"
    leafF (a, b, c, e) = "⟨" ++ nl (toInteger a) ++ ", " ++ nl b ++ ", " ++ nl c ++ ", " ++ nl e ++ "⟩"
    halves ks = (M.fromList [ (k `shiftR` 1, v) | (k, v) <- M.toList ks, even k ], M.fromList [ (k `shiftR` 1, v) | (k, v) <- M.toList ks, odd k ])
    go :: Int -> M.Map Int (Int, Integer, Integer, Integer) -> String
    go depth ks
        | M.null ks = "(.lf SS.z)"
        | depth == 0 = "(.lf " ++ leafF (head (M.elems ks)) ++ ")"
        | otherwise = let (l, r) = halves ks in "(.nd " ++ go (depth - 1) l ++ " " ++ go (depth - 1) r ++ ")"
    big = M.size keys > 512
    (top, defs) = split d keys ""
    split depth ks path
        | M.size ks <= 512 || depth == 0 =
            if M.size ks <= 512 && big
                then let nm = name ++ "_" ++ (if null path then "r" else path)
                     in (nm, ["noncomputable def " ++ nm ++ " : ST := " ++ go depth ks])
                else (go depth ks, [])
        | otherwise = let (l, r) = halves ks
                          (a, da) = split (depth - 1) l (path ++ "0")
                          (b, db) = split (depth - 1) r (path ++ "1")
                      in ("(.nd " ++ a ++ " " ++ b ++ ")", da ++ db)

-- | The scan section through the glue; returns the segments and the segment bound used.
emitScan :: EC -> Bounds -> (Int, Int, Int, Int) -> W (Int, Int)
emitScan ec bnd (modFacts, nHy, nUn, _) = do
    let m = ecM ec
        nR = ecNR ec
        nT = ecNT ec
        hyp = ecHyp ec
        hya = ecHya ec
        met = ecMet ec
        lcp = ecLCP ec
        dn = ecDn ec
        kinds = ecKinds ec
        kn k = fromMaybe 0 (elemIndex k kinds)
        u = ecU ec
        muxes = ecMuxes ec
        selmux q = M.findWithDefault False q (ecSelmux ec)
    w "/-! ## The scan: footprints, kind tables, exception masks, escapes -/\n"
    stab "fpRT" "Tab (List Ent)" (map ents (scFpR dn))
    unless (null lcp) $ stab "virtT" "Tab (List Ent)" (map ents (ecVirt ec))
    when met $ stab "fpMT" "Tab (List Ent)" (map ents (scFpM dn))
    stab "ktT" "Tab (Tab KC)" [ tab (map kc (elems t)) | t <- elems (scKt dn) ]
    let gmux = [ q | q <- [0 .. length muxes - 1], not (selmux q) ]
        muxA = listArray (0, length muxes - 1) muxes
        groups0 = [ map fst ((\(_, us, _) -> us) (muxA ! q)) | q <- gmux ]
        -- every needed exception bit not inside one mux group becomes a pair group, in (i, j) order
        memOf0 = IM.fromListWith S.union [ (x, S.singleton g) | (g, gr) <- zip [0 :: Int ..] groups0, x <- gr ]
        addPairs (gs, n, mo) (i, j)
            | not (S.null (S.intersection (IM.findWithDefault S.empty i mo) (IM.findWithDefault S.empty j mo))) = (gs, n, mo)
            | otherwise = ([j, i] : gs, n + 1, IM.insertWith S.union i (S.singleton n) (IM.insertWith S.union j (S.singleton n) mo))
        (pairsRev, _, _) = foldl addPairs ([], length groups0, memOf0) [ (i, j) | (i, nd) <- zip [0 ..] (ecNeed ec), j <- bitsI nd ]
        groups = groups0 ++ reverse pairsRev
        gidM = IM.fromListWith (flip (++)) [ (x, [g]) | (g, gr) <- zip [0 :: Int ..] groups, x <- S.toAscList (S.fromList gr) ]
        gid = [ IM.findWithDefault [] i gidM | i <- [0 .. nR - 1] ]
        escl = ecESC ec
        epairs = [ (r1, r2) | (r1, r2, _) <- escl ]
        members = S.toAscList (S.union (S.fromList (concat groups)) (S.fromList (map fst epairs)))
        rcodeM = M.fromList (zip members [1 ..])
        rcode = [ M.findWithDefault 0 x rcodeM | x <- [0 .. nR - 1] ]
        rcA = listArray (0, nR - 1) rcode
        rinv = 0 : members
        codeMask x = foldl (.|.) 0 [ (1 :: Integer) `shiftL` (rcA ! j) | j <- bitsI x, j < nR
                                   , rcA ! j /= 0 || error "schedlean: AssertionError: exception bit of a rule in no group" ]
        exRc = map codeMask (ecNeed ec)
        escRc = map codeMask (ecEsc ec)
        lcSlots = [ (ecVB ec M.! p, S.toAscList (S.fromList (map fst (u M.! p)))) | p <- lcp ]
        mann = fmap (const ()) (ecMann ec)
    let dc = case scanDesign m lcSlots exRc escRc (if met then Just (mannOf ec) else Nothing) of
            Right x -> x
            Left e -> error ("schedlean: " ++ e)
        states = case runScanStates dc (Just rcode) of
            Right x -> x
            Left info -> error ("schedlean: AssertionError: " ++ info)
    _ <- return mann
    stab "exRT" "Tab Nat" (map show exRc)
    stab "escRT" "Tab Nat" (map show escRc)
    stab "rcT" "Tab Nat" (map show rcode)
    stab "rinvT" "Tab Nat" (map show rinv)
    stab "memT" "Tab (List Nat)" (case groups of { [] -> ["[]"]; _ -> [ "[" ++ intercalate ", " (map show g) ++ "]" | g <- groups ] })
    stab "gidT" "Tab (List Nat)" [ "[" ++ intercalate ", " (map show g) ++ "]" | g <- gid ]
    stab "cmT" "Tab Nat" (case groups of { [] -> ["0"]; _ -> [ show (sum [ (1 :: Integer) `shiftL` (rcA ! x) | x <- S.toList (S.fromList g) ]) | g <- groups ] })
    w ("abbrev ng : Nat := " ++ show (length groups))
    when met $ do
        let Just (nfI, nfO, ngI, ngO, h3ok) = ecMann ec
        when (any (/= 0) h3ok && not (exported ec)) $ error "schedlean: AssertionError: h3ok entries"
        forM_ [("nfIT", nfI), ("nfOT", nfO), ("ngIT", ngI), ("ngOT", ngO)] $ \(nm, tb) -> stab nm "Tab Nat" (map show tb)
        when (exported ec) $ stab "h3okT" "Tab Nat" (map show h3ok)
    let dArgs = if met then ["nM", show (ecD ec), "fpMT", "nfIT", "nfOT", "ngIT", "ngOT"]
                else ["0", show (ecD ec), ".leaf []", ".leaf 0", ".leaf 0", ".leaf 0", ".leaf 0"]
    w (fmt "def D : Design where\n\
\  nM := %s\n\
\  nR := nR\n\
\  d := %d\n\
\  fpM := %s\n\
\  fpR := fpRT\n\
\  kt := ktT\n\
\  nfI := %s\n\
\  nfO := %s\n\
\  ngI := %s\n\
\  ngO := %s\n\
\  h3ok := %s\n\
\  exM := .leaf 0\n\
\  exR := exRT\n\
\  exMR := .leaf 0\n\
\  escR := escRT\n\
\  rc := fun i => rcT.get i\n" (take 7 dArgs ++ [if met && exported ec then "h3okT" else ".leaf 0"]))
    cut
    -- the gate's numeric bounds, as generator checks, written into the file
    let maxL xs = maximum (0 : xs)
        df = defaultBounds
        selPairs = maxL ([ length us | (q, (_, us, _)) <- zip [0 ..] muxes, selmux q
                                     , not (maybe False (\(_, g) -> length g == length us) (M.lookup q (ecMgrp ec))) ]
                         ++ [ S.size (S.fromList (map fst us)) | ((k, _, pm), us) <- M.toList u, k /= "tasks", kdHasEn (kd ec k) pm
                                                               , met || not (all (\(k', si) -> isUncond (site ec k' si)) us) ])
        bounds =
            [ ("uses in one scan segment (the module: uses of its largest step)", False, bnGateSeg df, bnGateSeg bnd, maxL (map length (scFpM dc) ++ map length (scFpR dc)))
            , ("slots in the state", False, bnGateSlots df, bnGateSlots bnd, ecNSlots ec)
            , ("rule codes", False, bnGateCodes df, bnGateCodes bnd, length rinv)
            , ("sites in one step", False, bnGateStep df, bnGateStep bnd, maxL [ length (tSites r) | r <- mRules m ])
            , ("users of one port (mux members)", False, bnGatePortUsers df, bnGatePortUsers bnd, maxL (map length (M.elems u)))
            , ("members of one exception group", False, bnGateGroup df, bnGateGroup bnd, maxL (map length (groups ++ ecGroups ec)))
            , ("callers of one last-caller port", False, bnGateLcUsers df, bnGateLcUsers bnd, maxL [ S.size (S.fromList (map fst (u M.! p))) | p <- lcp ])
            , ("escaped rule pairs", False, bnGateEsc df, bnGateEsc bnd, length escl)
            , ("dynamic facts of one escaped pair", False, bnGateEscFacts df, bnGateEscFacts bnd, maxL [ length b | (_, _, b) <- escl ])
            , ("facts of one module (escape, caller-exclusivity, readiness and invariance)", False, bnGateModFacts df, bnGateModFacts bnd, modFacts)
            , ("fields of one fact group (Hyps and Unconfirmed in groups gK)", False, bnGateStruct df, bnGateStruct bnd,
               max (min (bnGateStruct df) nHy) (min (bnGateStruct df) nUn))
            , ("entries per table definition (C21)", True, 128, 128, splitN)
            , ("source characters per table definition (C21)", True, 20000, 20000, splitW)
            , ("users of a port listed entry by entry (en_list / en_lit threshold)", True, 8, 8, 8)
            , ("users of one port proved pair by pair (a conditional user, or a module with methods)", False, bnGateSelmux df, bnGateSelmux bnd, selPairs) ]
    forM_ bounds $ \(bn, _, _, used, v) -> when (v > used) $ error ("schedlean: AssertionError: gate: " ++ bn ++ " " ++ show v ++ " above the bound " ++ show used)
    w ("/-! Gate bounds (design.md §11 item 6), each with this module's value (a table setting: the value used):\n"
       ++ concat [ "  * " ++ bn ++ ": at most " ++ show used ++ (if used == bd then "" else " (lowered from " ++ show bd ++ ")") ++ ", "
                   ++ (if setting then "setting used" else "this module") ++ " " ++ show v ++ "\n" | (bn, setting, bd, used, v) <- bounds ] ++ "-/")
    let segUses = bnGateSeg bnd
        uses = map length (scFpM dc) ++ map length (scFpR dc)
        (cuts, _) = foldl (\(cs, acc) (g, n) -> if acc + n > segUses && acc > 0 then (cs ++ [g], n) else (cs, acc + n)) ([], 0) (zip [0 ..] uses)
        stArr = listArray (0, length states - 1) states
    forM_ (zip [1 :: Int ..] cuts) $ \(j, c) -> w (emitTree (stArr ! (c - 1)) (scD dc) ("cp" ++ show j))
    let segs = [ (j, prev, c, pn) | (j, (prev, c, pn)) <- zip [0 :: Int ..] (zip3 (0 : cuts) cuts ("ST.init" : [ "cp" ++ show i | i <- [1 .. length cuts] ])) ]
    forM_ segs $ \(j, prev, c, pn) -> w (fmt "theorem seg%d : D.seg %d %d %s cp%d = true := by decide +kernel" [show j, show prev, show (c - prev), pn, show (j + 1)])
    let lastPrev = if null cuts then 0 else last cuts
        lastName = if null cuts then "ST.init" else "cp" ++ show (length cuts)
    w (fmt "theorem segZ : D.segLast %d %s = true := by decide +kernel" [show lastPrev, lastName])
    w (fmt "theorem scanOK : D.Ok := D.ok_of_chain [%s] %s\n"
           [intercalate ", " [ "(" ++ show (c - prev) ++ ", cp" ++ show (j + 1) ++ ")" | (j, prev, c, _) <- segs ]
           , if null segs then "segZ" else "⟨" ++ intercalate ", " [ "seg" ++ show j | (j, _, _, _) <- segs ] ++ ", segZ⟩"])
    let nseg = length cuts + 1
    cut
    w (if nT > 0 then "def chB (j : I) : Bool := decide (j.1 = .tasks)" else "def chB (j : I) : Bool := false")
    w ("def posK : (κ : Kind) → (KI κ).M → Nat\n" ++ concat [ if k == "tasks" then "  | .tasks => fun t => t.val\n" else "  | ." ++ k ++ " => fun _ => 0\n" | k <- kinds ])
    stab "stT" "Tab Nat" (map show (take nR (mTStart m)))
    w (fmt "def stF (g : Nat) : Nat := bif Nat.blt g nR then stT.get g else %d" [show nT])
    allBThm "stMono" (fmt "fun g x => Nat.ble x %d && Nat.ble x (bif Nat.blt (g + 1) nR then stT.get (g + 1) else %d)" [show nT, show nT]) "stT" "nR"
    allBThm "chainAll" "fun k keys =>\n\
\    incB ((((List.range' 0 (kT.get k)).map keys.get).filter (fun p => chB p.1)).map (fun p => posK p.1.1 p.2)) &&\n\
\    ((((List.range' 0 (kT.get k)).map keys.get).filter (fun p => chB p.1)).map (fun p => posK p.1.1 p.2)).all\n\
\      (fun x => Nat.ble (bif Nat.blt k nR then stF k else 0) x && Nat.blt x (bif Nat.blt k nR then stF (k + 1) else 0))" "keyT" "nS"
    w (fmt cloText [show nT])
    w (fmt glueGText [if met then "m.val" else "m.elim"])
    allBThm "fpAll" ("fun k l => l == (List.range' 0 (kT.get k)).map (fun i => entK ((keyT.get k).get i))" ++ (if null lcp then "" else " ++ virtT.get k")) "fpRT" "nR"
    when met $ allBThm "fpAllM" "fun m l => l == (List.range' 0 (kT.get (nR + m))).map (fun i => entK ((keyT.get (nR + m)).get i))" "fpMT" "nM"
    cut
    if not (null groups)
        then do
            allBThm "cmAll" "fun g l => Nat.beq (cmT.get g) (bits (l.map D.rc))" "memT" "ng"
            w "theorem hcm_ok : ∀ g, g < ng → cmT.get g = bits ((memT.get g).map D.rc) := cm_of_check memT cmT D.rc ng cmAll"
            allBThm "ghg" "fun x l => l.all (fun g => Nat.blt g ng && Nat.beq (Nat.land (Nat.shiftRight (cmT.get g) (D.rc x)) 1) 1)" "gidT" "nR"
            w "theorem hg_ok : ∀ x, x < nR → ∀ g ∈ gidT.get x, g < ng ∧ (cmT.get g).testBit (D.rc x) = true :=\n  gid_of_check gidT cmT D.rc nR ng ghg"
            allBThm "ghT" "fun y n => subB n (orMask (gidT.get y) cmT.get)" "exRT" "nR"
            w "theorem hinj_ok : ∀ g, g < ng → ∀ x ∈ memT.get g, ∀ x1, x1 < D.nR → D.rc x1 = D.rc x → x1 = x :=\n  chains_inj D rinvT.get memT ng (by decide +kernel) (by decide +kernel)"
        else do
            w "theorem hcm_ok : ∀ g, g < ng → cmT.get g = bits ((memT.get g).map D.rc) := fun _ h => absurd h (Nat.not_lt_zero _)\n\
\theorem hg_ok : ∀ x, x < nR → ∀ g ∈ gidT.get x, g < ng ∧ (cmT.get g).testBit (D.rc x) = true := by\n\
\  intro x hx g hg\n\
\  have h := Tab.allB_spec (fun _ l => l.isEmpty) gidT 0 nR (by decide +kernel) x (Nat.zero_le _) hx\n\
\  simp only [List.isEmpty_iff] at h\n\
\  rw [h] at hg\n\
\  simp at hg\n\
\theorem hinj_ok : ∀ g, g < ng → ∀ x ∈ memT.get g, ∀ x1, x1 < D.nR → D.rc x1 = D.rc x → x1 = x := fun _ h => absurd h (Nat.not_lt_zero _)"
            allBThm "ghT" "fun y n => Nat.beq n 0" "exRT" "nR"
    w ("theorem hT_ok : ∀ y, y < nR → Sub (exRT.get y) (orMask (gidT.get y) cmT.get) :=\n"
       ++ (if not (null groups) then "  fun y hy => sub_of_subB (Tab.allB_spec _ exRT 0 nR ghT y (Nat.zero_le _) hy)"
           else "  fun y hy i h => by rw [Nat.eq_of_beq_eq_true (Tab.allB_spec _ exRT 0 nR ghT y (Nat.zero_le _) hy)] at h; simp at h"))
    cut
    let lps k = intercalate ", " (fromMaybe [] (kdLPorts (kd ec k)))
        mty k = fromMaybe "" (kdMType (kd ec k))
    forM_ kinds $ \k -> case k of
        "tasks" -> w "theorem ports_tasks : ∀ p : (KI .tasks).M, p ∈ List.finRange nT := List.mem_finRange"
        "reg" -> w "theorem ports_reg : ∀ p : CRegM 1, p ∈ [Reg.read, Reg.write]\n  | .rd ⟨0, _⟩ => by simp\n  | .wr ⟨0, _⟩ => by simp"
        _ | isSynthKind k -> w (fmt "theorem ports_%s : ∀ p : %s, p ∈ List.finRange %s := portsU_%s" [k, mty k, drop 4 (mty k), k])
        _ -> w (fmt "theorem ports_%s : ∀ p : (KI .%s).M, p ∈ [%s] := by intro p; cases p <;> simp" [k, k, lps k])
    w ("def portsK : (κ : Kind) → List (KI κ).M\n"
       ++ concat [ if k == "tasks" then "  | .tasks => List.finRange nT\n"
                   else if isSynthKind k then "  | ." ++ k ++ " => List.finRange " ++ drop 4 (mty k) ++ "\n"
                   else "  | ." ++ k ++ " => [" ++ lps k ++ "]\n" | k <- kinds ]
       ++ "def portsJ (j : I) : List (CI j).M := portsK j.1\n\
\theorem portsJ_ok (j : I) (m : (CI j).M) : m ∈ portsJ j := by\n\
\  obtain ⟨κ, i⟩ := j\n\
\  cases κ\n" ++ concat [ "  · exact ports_" ++ k ++ " m\n" | k <- kinds ])
    w ("/-- The `Tasks` child, and a task port's statement number (0 elsewhere); statements are numbered in rank order. -/\n\
\def isT : Kind → Bool\n" ++ concat [ "  | ." ++ k ++ " => " ++ (if k == "tasks" then "true" else "false") ++ "\n" | k <- kinds ]
       ++ "def tnum : (κ : Kind) → (KI κ).M → Nat\n" ++ concat [ "  | ." ++ k ++ " => " ++ (if k == "tasks" then "fun t => t.val" else "fun _ => 0") ++ "\n" | k <- kinds ])
    w (fmt fpOkText [if null lcp then "List.Sublist.refl _" else "List.sublist_append_left _ _"] ++ (if met then fpMOkText else ""))
    forM_ kinds $ \k -> unless (k == "tasks") $ case lookup k (ecSynth ec) of
        -- a synthesized kind's table checked by rows, its annotation's rows and columns as masks
        Just sk -> let a = maybe (exNS (skExport sk) ++ ".xAnn") (const ("sA_" ++ k)) (skOff sk)
                       hyp = "(fun _ _ h => (Bool.and_eq_true _ _).mp h)"
                   in w (fmt "theorem kind_%s (z : V) : KindOK (V := V) (KI .%s).ann (codeK .%s) (D.kc %d) (fxF %d) z :=\n\
\  kindOK_of_rows (n := %s) (pr := %sPrecT.get) (pc := %sPrecCT.get) (sr := %sSbrT.get) (sc := %sSbrCT.get)\n\
\    (cr := %sConfT.get) (cc := %sConfCT.get) (sh := fun i => %sShareB.testBit i) %s %s %s (fun _ h => h)\n\
\    (by decide +kernel) z"
               [k, k, k, show (kn k), show (kn k), drop 4 (mty k), a, a, a, a, a, a, a, hyp, hyp, hyp])
        Nothing -> w (fmt "theorem kind_%s (z : V) : KindOK (V := V) (KI .%s).ann (codeK .%s) (D.kc %d) (fxF %d) z :=\n  kindOK_of_chk ports_%s (by decide +kernel) z"
               [k, k, k, show (kn k), show (kn k), k])
    w chainSText
    let gmuxA = listArray (0, length gmux - 1) gmux
        chItems = [ if g < length gmux
                    then fmt "(fun oc w hE => chain_%d ops E %s oc hE.1 : CHQ ops E u %d [%s])" [show (gmuxA ! g), hya, show g, intercalate ", " (map show mem)]
                    else let [a, b] = mem
                         in fmt "(fun oc w hE => amoChain_of_pairsL (fun k => WF ops E oc k) (fun x y => WF ops E oc x = true → WF ops E oc y = true → False) (fun _ _ h => h) [%d, %d] ⟨⟨fun h1 h2 => %s, trivial⟩, trivial, trivial⟩ : CHQ ops E u %d [%d, %d])"
                                [show a, show b, excl ec a b "hE.1" "hy" "h1" "h2", show g, show a, show b]
                  | (g, mem) <- zip [0 ..] groups ]
    w "/-- Exception group `l`: its rules never two fire. -/\n\
\def CHQ (ops : Ops V) (E : EqC V) (u : St → Nat → V) (g : Nat) (l : List Nat) : Prop :=\n\
\  ∀ oc w, Env (P ops E u) oc w → amoChain false (l.map (fun i => fire ops E oc w (rR i)))"
    w (fmt "theorem chT (ops : Ops V) (E : EqC V) (u : St → Nat → V) %s : Tab.AllR (CHQ ops E u) memT 0 ng :=\n  %s"
           [hyp, if null chItems then "fun _ _ h => absurd h (Nat.not_lt_zero _)" else tabproof chItems])
    w okPText
    w ("/-- Two task statements in number order may follow each other (`taskAnn`: the lower one runs first). -/\n\
\theorem okP_task (j : I) (a b : (CI j).M) (h : isT j.1 = true) (hab : tnum j.1 a < tnum j.1 b) : okP j a b = true := by\n\
\  obtain ⟨κ, i⟩ := j\n\
\  cases κ <;> simp only [isT, Bool.false_eq_true] at h" ++ (if "tasks" `elem` kinds then okPTaskText else ""))
    w okTText
    -- escapes: one dynamic proof per escaped pair
    let efidx = M.fromList [ ((r1, r2, i1, i2), n) | (n, (r1, r2, i1, i2, _, _)) <- zip [0 :: Int ..] (ecEFacts ec) ]
        efA = listArray (0, length (ecEFacts ec) - 1) (ecEFacts ec)
        tstA = listArray (0, length (mTStart m) - 1) (mTStart m)
        wxm = ecWXM ec
    forM_ (zip [0 :: Int ..] escl) $ \(ne, (r1, r2, bad)) -> do
        when (null bad) $ error "schedlean: AssertionError: an escaped pair without a failing site pair"
        let byi = M.fromListWith (flip (++)) [ (i1, [i2]) | (i1, i2) <- bad ]
            keys = M.keys byi
            hb = [ allL [ let fi = efidx M.! (r1, r2, i1, i2)
                              (_, _, _, _, how, _) = efA ! fi
                              s1 = site ec r1 i1
                              s2 = site ec r2 i2
                              c1 = VCtx ("(u (.inr (rR " ++ show r1 ++ ")))") "ops.z" "f1"
                              c2 = VCtx ("(u (.inr (rR " ++ show r2 ++ ")))") "ops.z" "f2"
                              args = ["f1", "f2"] ++ [ "((" ++ condVal ec r1 s1 c1 ++ ").symm.trans h1)" | not (isUncond s1) ]
                                     ++ [ "((" ++ condVal ec r2 s2 c2 ++ ").symm.trans h2)" | not (isUncond s2) ]
                          in "fun h1 h2 => " ++ (if isNothing how then "hr" else "hy") ++ ".esc_" ++ show fi ++ " oc "
                             ++ (if how == Just "assert" then "hE.1 " else "") ++ unwords args
                        | i2 <- sort (byi M.! i1) ]
                 | i1 <- keys ]
        stabKeyed ("escB_" ++ show ne) "Tab (Nat × List Nat)" keys [ "(" ++ show i1 ++ ", [" ++ intercalate ", " (map show (sort (byi M.! i1))) ++ "])" | i1 <- keys ]
        let lo = if "tasks" `elem` kinds then tstA ! r2 else 0
            fchk = fmt "fun i p => (isT p.1.1 && Nat.blt (tnum p.1.1 p.2) %d) || (portsJ p.1).all (fun b => okP p.1 p.2 b || subS (idxT (usesAt ⟨p.1, b⟩) %d) (idxT escB_%d i))"
                       [show lo, show r2, show ne]
        allBThm ("escChkS_" ++ show ne) fchk ("key_" ++ show r1) (show (length (tSites (step ec r1))))
        w (fmt "theorem escChk_%d : Tab.allB (%s) (keyT.get %d) 0 (kT.get %d) = true := escChkS_%d" [show ne, fchk, show r1, show r1, show ne])
        let fchk2 = "fun _ p => !isT p.1.1 || Nat.ble " ++ show lo ++ " (tnum p.1.1 p.2)"
        allBThm ("escT2S_" ++ show ne) fchk2 ("key_" ++ show r2) (show (length (tSites (step ec r2))))
        w (fmt "theorem escT2_%d : Tab.allB (%s) (keyT.get %d) 0 (kT.get %d) = true := escT2S_%d" [show ne, fchk2, show r2, show r2, show ne])
        tb <- taball ("escB_" ++ show ne) hb
        let s = show
        w (fmt "theorem esc_%d (ops : Ops V) (E : EqC V) %s (u : St → Nat → V) (oc : OC V) (w : %s)\n\
\    (hE : Env (P ops E u) oc w) (f1 : fire ops E oc w (rR %d) = true) (f2 : fire ops E oc w (rR %d) = true) :\n\
\    OkT (P ops E u) (trR (P ops E u) oc (rR %d)).1 (trR (P ops E u) oc (rR %d)).1 := by\n\
\  have hz1 := zR_ok ops E u oc (rR %d)\n\
\  have hz2 := zR_ok ops E u oc (rR %d)\n\
\  rw [trR_eq] at hz1 hz2 ⊢\n\
\  rw [trR_eq]\n\
\  exact okT_exceptZ ops.z _ _ (siteR ops E (rR %d).val) (siteR ops E (rR %d).val)\n\
\    (kR (rR %d).val) (kR (rR %d).val) (keyT.get (rR %d).val) (keyT.get (rR %d).val) (fun _ => rfl) (fun _ => rfl)\n\
\    (idx (.inr (rR %d))) (hchk (rR %d)) escB_%d %d hz1 hz2 escChk_%d escT2_%d\n\
\    %s" [s ne, hyp, wxm, s r1, s r2, s r1, s r2, s r1, s r2, s r1, s r2, s r1, s r2, s r1, s r2, s r2, s r2, s ne, s lo, s ne, s ne, tb])
    escField <- if not (null escl)
        then do
            w ("def escL : List (Nat × Nat) := [" ++ intercalate ", " [ "(" ++ show a ++ ", " ++ show b ++ ")" | (a, b) <- epairs ] ++ "]")
            stab "escMemT" "Tab (List Nat)" ["[" ++ intercalate ", " (map (show . fst) epairs) ++ "]"]
            w escInjText
            return ("esc_of_pairsB u ops E escL (fun p hp => AllL.mem (show AllL (fun p : Nat × Nat => ∀ oc w, Env (P ops E u) oc w → "
                    ++ "fire ops E oc w (rR p.1) = true → fire ops E oc w (rR p.2) = true → "
                    ++ "OkT (P ops E u) (trR (P ops E u) oc (rR p.1)).1 (trR (P ops E u) oc (rR p.2)).1) escL from "
                    ++ allL [ "esc_" ++ show n ++ " ops E " ++ hya ++ " u" | n <- [0 .. length escl - 1] ] ++ ") p hp) escInj escT_ok")
        else return "fun _ _ r1 r2 _ h => by\n    have e : (D.escR.get ((G ops E u).rk r2)).testBit (D.rc ((G ops E u).rk r1)) = false := by\n      rw [show D.escR.get ((G ops E u).rk r2) = 0 from Tab.allB_spec (fun _ n => Nat.beq n 0) escRT 0 nR (by decide +kernel) r2.val (Nat.zero_le _) r2.isLt |> Nat.eq_of_beq_eq_true]\n      exact Nat.zero_testBit _\n    rw [e] at h; cases h"
    let methFields = if met then "  mi_lt m := m.isLt\n" else "  mi_lt m := m.elim\n"
    nf <- if met && exported ec then w (nfXText ++ nfRest) >> return nfXMet
          else if met then w nfText >> return nfMet else return nfNoMet
    let kcase non tk = concat [ if k == "tasks" then "    · " ++ tk ++ "\n" else "    · " ++ non k ++ "\n" | k <- kinds ]
    w ("\ntheorem glueOK (ops : Ops V) (E : EqC V) " ++ hyp ++ " (u : St → Nat → V) : GlueOK (P ops E u) D (G ops E u) (fire ops E) where\n"
       ++ methFields ++ "  rk_lt r := r.isLt\n\
\  rk_at r := rR_val r\n\
\  at_rk i hi := Nat.mod_eq_of_lt hi\n\
\  kind j h := by\n\
\    obtain ⟨κ, i⟩ := j\n\
\    cases κ\n" ++ kcase (\k -> "exact kind_" ++ k ++ " _") "simp [G, chB] at h" ++ "  kindC j h := by\n\
\    obtain ⟨κ, i⟩ := j\n\
\    cases κ\n" ++ kcase (const "simp [G, chB] at h") "exact kindOut_chain (fun _ => rfl) (by show (0 : Nat) ∈ [0]; simp) (by show (0 : Nat) ∈ [0]; simp)"
       ++ "  chAnn j h a b hlt := by\n\
\    obtain ⟨κ, i⟩ := j\n\
\    cases κ\n" ++ kcase (const "simp [G, chB] at h") "exact taskAnn_group hlt" ++ "  cmono g1 g2 h := cmonoG g1 g2 h\n\
\  cinM " ++ (if met then "oc w m := cinM_ok ops E u oc w m" else "_ _ m := m.elim") ++ "\n\
\  cinR oc r := by\n\
\    rw [trR_eq]\n\
\    have hc := chain_in_of_sites (G ops E u) (envR ops E (u (.inr r)) oc r.val) _ _ _ (chainS ops E u r.val (rlt r) ops.z).1\n\
\    have e0 : Nat.blt r.val nR = true := Nat.blt_eq.mpr r.isLt\n\
\    simp only [e0, cond_true] at hc\n\
\    intro c hcm hch\n\
\    have := hc c hcm hch\n\
\    show cloG (D.nM + r.val) ≤ _ ∧ _ < chiG (D.nM + r.val)\n\
\    rw [cloG_r, chiG_r]\n\
\    exact this\n\
\  cincM " ++ (if met then "oc w m j a b hj hab := cincM_ok ops E u oc w m j a b hj hab" else "_ _ m := m.elim") ++ "\n\
\  cincR oc r := by\n\
\    rw [trR_eq]\n\
\    exact chain_inc_of_sites (G ops E u) _ _ (chainS ops E u r.val (rlt r) ops.z).2\n\
\  chCode j h p := by\n\
\    obtain ⟨κ, i⟩ := j\n\
\    cases κ\n" ++ kcase (const "simp [G, chB] at h") "rfl" ++ "  fpM " ++ (if met then "oc w m := fpM_ok ops E u oc w m" else "_ _ m := m.elim") ++ "\n\
\  fpR oc r := fpR_ok ops E u oc r\n\
\  zM " ++ (if met then "oc w m := zM_ok ops E u oc w m" else "_ _ m := m.elim") ++ "\n\
\  zR oc r := zR_ok ops E u oc r\n" ++ nf ++ "  exR := exR_of_cmask (P ops E u) (G ops E u) (fire ops E) (fun r => r.isLt) rR_val memT gidT cmT ng\n\
\    (chT ops E u " ++ hya ++ ") hcm_ok hg_ok hinj_ok hT_ok\n\
\  exMR " ++ (if met then "_ _ m r _ h := by simp [D, Tab.get] at h" else "_ _ m := m.elim") ++ "\n\
\  esc := " ++ escField ++ "\n\
\\n\
\/-- **The pair fields, from the scan.** -/\n\
\def pairs (ops : Ops V) (E : EqC V) " ++ hyp ++ " (u : St → Nat → V) : PairFacts (P ops E u) (G ops E u).rules (fire ops E) :=\n\
\  pairFacts_of_scan (P ops E u) scanOK (G ops E u) (fire ops E) (glueOK ops E " ++ hya ++ " u)\n\
\end\n")
    cut
    return (nseg, segUses)

mannOf :: EC -> MAnn
mannOf ec = maybe (mannPlain [ tName r | r <- drop (ecNR ec) (mRules (ecM ec)) ] (dvCF (mDV (ecM ec)))) mannExport (ecExport ec)

cloText :: String
cloText = "/-- Chain intervals by the scan's step position (methods first, then rules by rank). -/\n\
\def cloG (g : Nat) : Nat := if g < D.nM then 0 else stF (g - D.nM)\n\
\def chiG (g : Nat) : Nat := if g < D.nM then 0 else stF (g - D.nM + 1)\n\
\theorem cloG_r (r : Nat) : cloG (D.nM + r) = stF r := by\n\
\  unfold cloG; rw [if_neg (Nat.not_lt.mpr (Nat.le_add_right _ _)), Nat.add_sub_cancel_left]\n\
\theorem chiG_r (r : Nat) : chiG (D.nM + r) = stF (r + 1) := by\n\
\  unfold chiG; rw [if_neg (Nat.not_lt.mpr (Nat.le_add_right _ _)), Nat.add_sub_cancel_left]\n\
\theorem cmonoG (g1 g2 : Nat) (h : g1 < g2) : chiG g1 ≤ cloG g2 := by\n\
\  have hm := mono_of_succ stF (stF_mono stT nR %d stMono)\n\
\  unfold chiG cloG\n\
\  split <;> split\n\
\  · exact Nat.le_refl _\n\
\  · exact Nat.zero_le _\n\
\  · omega\n\
\  · exact hm _ _ (by omega)\n"

glueGText :: String
glueGText = "section\n\
\variable {V : Type}\n\
\def G (ops : Ops V) (E : EqC V) (u : St → Nat → V) : Glue (P ops E u) D where\n\
\  mi := fun m => %s\n\
\  ruleAt := rR\n\
\  rk := fun r => r.val\n\
\  base := baseOf\n\
\  kind := fun j => kindN j.1\n\
\  code := fun j m => codeK j.1 m\n\
\  fx := fxF\n\
\  z := ops.z\n\
\  ch := chB\n\
\  pos := fun j m => posK j.1 m\n\
\  clo := cloG\n\
\  chi := chiG\n\
\end\n\
\\n\
\def entK (p : Σ j : I, (CI j).M) : Ent := (baseOf p.1, kindN p.1.1, codeK p.1.1 p.2)\n"

fpOkText :: String
fpOkText = "section\n\
\variable {V : Type}\n\
\theorem testBit_leaf0 (k i : Nat) : ((Tab.leaf 0 : Tab Nat).get k).testBit i = false := by simp [Tab.get]\n\
\\n\
\theorem fpR_ok (ops : Ops V) (E : EqC V) (u : St → Nat → V) (oc : OC V) (r : Fin nR) :\n\
\    (trR (P ops E u) oc r).1.map (G ops E u).ent <+ D.fpR.get ((G ops E u).rk r) := by\n\
\  rw [trR_eq]\n\
\  refine (fp_of_sites (G ops E u) _ _).trans ?_\n\
\  have h := Tab.allB_spec _ fpRT 0 nR fpAll r.val (Nat.zero_le _) r.isLt\n\
\  rw [beq_iff_eq] at h\n\
\  show (sitesF (siteR ops E r.val) (kR r.val)).map (G ops E u).entS <+ fpRT.get r.val\n\
\  rw [h, sitesF_map]\n\
\  exact %s\n\
\\n\
\theorem zR_ok (ops : Ops V) (E : EqC V) (u : St → Nat → V) (oc : OC V) (r : Fin nR) :\n\
\    ∀ c ∈ (trR (P ops E u) oc r).1, (G ops E u).fx ((G ops E u).kind c.1) ((G ops E u).code c.1 c.2.1) = true → c.2.2 = (G ops E u).z := by\n\
\  rw [trR_eq]\n\
\  apply z_of_sites (G ops E u)\n\
\  intro s hs hf ρ'\n\
\  obtain ⟨i, hi, rfl⟩ := mem_sitesF hs\n\
\  exact ((Tab.get_allR _ 0 (kR r.val) (sqR ops E r.val (rlt r)) i (Nat.zero_le _) hi) ops.z).2 hf ρ'\n"

fpMOkText :: String
fpMOkText = "theorem fpM_ok (ops : Ops V) (E : EqC V) (u : St → Nat → V) (oc : OC V) (w : Wires (Fin nM) V) (m : Fin nM) :\n\
\    (trM (P ops E u) oc w m).1.map (G ops E u).ent <+ D.fpM.get ((G ops E u).mi m) := by\n\
\  rw [trM_eq]\n\
\  refine (fp_of_sites (G ops E u) _ _).trans ?_\n\
\  have h := Tab.allB_spec _ fpMT 0 nM fpAllM m.val (Nat.zero_le _) m.isLt\n\
\  rw [beq_iff_eq] at h\n\
\  show (sitesF (siteS ops E (nR + m.val) (w.arg m)) (kR (nR + m.val))).map (G ops E u).entS <+ fpMT.get m.val\n\
\  rw [h, sitesF_map]\n\
\  exact List.Sublist.refl _\n\
\theorem zM_ok (ops : Ops V) (E : EqC V) (u : St → Nat → V) (oc : OC V) (w : Wires (Fin nM) V) (m : Fin nM) :\n\
\    ∀ c ∈ (trM (P ops E u) oc w m).1, (G ops E u).fx ((G ops E u).kind c.1) ((G ops E u).code c.1 c.2.1) = true → c.2.2 = (G ops E u).z := by\n\
\  rw [trM_eq]\n\
\  apply z_of_sites (G ops E u)\n\
\  intro s hs hf ρ'\n\
\  obtain ⟨i, hi, rfl⟩ := mem_sitesF hs\n\
\  exact ((Tab.get_allR _ 0 (kR (nR + m.val)) (sqR ops E (nR + m.val) (mlt m)) i (Nat.zero_le _) hi) (w.arg m)).2 hf ρ'\n"

chainSText :: String
chainSText = "theorem chainS (ops : Ops V) (E : EqC V) (u : St → Nat → V) (k : Nat) (hk : k < nS) (x : V) :\n\
\    (∀ p ∈ (sitesF (siteS ops E k x) (kR k)).map (fun s => (⟨s.j, s.m⟩ : Σ j : I, (CI j).M)),\n\
\      (G ops E u).ch p.1 = true → (bif Nat.blt k nR then stF k else 0) ≤ (G ops E u).pos p.1 p.2 ∧\n\
\        (G ops E u).pos p.1 p.2 < (bif Nat.blt k nR then stF (k + 1) else 0)) ∧\n\
\    incB ((((sitesF (siteS ops E k x) (kR k)).map (fun s => (⟨s.j, s.m⟩ : Σ j : I, (CI j).M))).filter\n\
\      (fun p => (G ops E u).ch p.1)).map (fun p => (G ops E u).pos p.1 p.2)) = true := by\n\
\  rw [sitesF_map]\n\
\  exact chain_keys (G := G ops E u) _ _ _ _ (Tab.allB_spec _ keyT 0 nS chainAll k (Nat.zero_le _) hk)\n"

okPText :: String
okPText = "\n\
\/-- Ports' static `Follow`: a flagged (argument-less) port's calls carry `z`. -/\n\
\def okP (j : I) (a b : (CI j).M) : Bool :=\n\
\  folP (CI j).ann a b && (!decide (a = b) || !(CI j).ann.share a || fxP ⟨j, a⟩)\n\
\theorem okP_follow (z : V) : ∀ j (a b : (CI j).M × V), okP j a.1 b.1 = true →\n\
\    (fxP ⟨j, a.1⟩ = true → a.2 = z) → (fxP ⟨j, b.1⟩ = true → b.2 = z) → (CI j).ann.Follow a b := by\n\
\  intro j a b h ha hb\n\
\  simp only [okP, Bool.and_eq_true, Bool.or_eq_true, Bool.not_eq_true', decide_eq_false_iff_not] at h\n\
\  obtain ⟨hf, hs⟩ := h\n\
\  simp only [folP, Bool.and_eq_true, Bool.or_eq_true, decide_eq_true_eq, Bool.not_eq_true'] at hf\n\
\  obtain ⟨⟨h11, h12⟩, h13⟩ := hf\n\
\  refine ⟨fun hne => ?_, h12, h13, fun he hsh => ?_⟩\n\
\  · rcases h11 with h | h\n\
\    · exact absurd h hne\n\
\    · exact h\n\
\  · rcases hs with (h | h) | h\n\
\    · exact absurd he h\n\
\    · rw [hsh] at h; cases h\n\
\    · have e2 : fxP ⟨j, b.1⟩ = true := by rw [← he]; exact h\n\
\      rw [ha h, hb e2]\n"

okPTaskText :: String
okPTaskText = "\n\
\  have hab' : (a : Fin nT).val < (b : Fin nT).val := hab\n\
\  have hne : ¬ (a : Fin nT) = b := fun e => by rw [e] at hab'; exact Nat.lt_irrefl _ hab'\n\
\  have hne' : ¬ (b : Fin nT) = a := fun e => hne e.symm\n\
\  have hnl : ¬ (b : Fin nT) < a := fun e => Nat.lt_asymm hab' e\n\
\  simp [okP, folP, taskAnn, hne, hne', hnl]"

okTText :: String
okTText = "/-- An escaped pair over numbered site tables, with flagged ports' calls carrying `z`; its bad site pairs\n\
\    keyed by first-step site (`badT`, leaves `(i, i2s)`), each site's partners in one pass (`subS`): linear in\n\
\    the facts (cost review r8-2 M1). A first-step task statement below `lo`, with every second-step one at\n\
\    least `lo` (`hc2`), is one comparison, independent of `nT` (cost review r8b-1 M1). -/\n\
\theorem okT_exceptZ (z : V) (ρ1 ρ2 : Nat → V) (site1 site2 : Nat → Site I (fun j => (CI j).M) V) (k1 k2 : Nat)\n\
\    (key1 key2 : Tab (Σ j, (CI j).M))\n\
\    (hkey1 : ∀ i, (⟨(site1 i).j, (site1 i).m⟩ : Σ j, (CI j).M) = key1.get i)\n\
\    (hkey2 : ∀ i, (⟨(site2 i).j, (site2 i).m⟩ : Σ j, (CI j).M) = key2.get i)\n\
\    (idx2 : (Σ j, (CI j).M) → List Nat) (hchk2 : ∀ i, i < k2 → i ∈ idx2 (key2.get i)) (badT : Tab (Nat × List Nat))\n\
\    (lo : Nat)\n\
\    (hz1 : ∀ c ∈ callsOf ρ1 (sitesF site1 k1), fxP ⟨c.1, c.2.1⟩ = true → c.2.2 = z)\n\
\    (hz2 : ∀ c ∈ callsOf ρ2 (sitesF site2 k2), fxP ⟨c.1, c.2.1⟩ = true → c.2.2 = z)\n\
\    (hc : Tab.allB (fun i p => (isT p.1.1 && Nat.blt (tnum p.1.1 p.2) lo) || (portsJ p.1).all (fun b => okP p.1 p.2 b ||\n\
\      subS (idx2 ⟨p.1, b⟩) (idxT badT i))) key1 0 k1 = true)\n\
\    (hc2 : Tab.allB (fun _ p => !isT p.1.1 || Nat.ble lo (tnum p.1.1 p.2)) key2 0 k2 = true)\n\
\    (hbad : Tab.All (fun e : Nat × List Nat =>\n\
\      AllL (fun i2 => (site1 e.1).cond ρ1 = true → (site2 i2).cond ρ2 = true → False) e.2) badT) :\n\
\    ∀ j, ∀ a ∈ proj j (callsOf ρ1 (sitesF site1 k1)), ∀ b ∈ proj j (callsOf ρ2 (sitesF site2 k2)), (CI j).ann.Follow a b := by\n\
\  intro j ⟨a1, a2⟩ ha b hb\n\
\  obtain ⟨s, hs, hc1, ha2⟩ := mem_proj_callsOf (I := I) (CM := fun j => (CI j).M) ρ1 j a1 a2 (sitesF site1 k1) ha\n\
\  rw [List.mem_filter] at hs\n\
\  obtain ⟨i1, hi1, rfl⟩ := mem_sitesF hs.1\n\
\  have e1 : key1.get i1 = ⟨j, a1⟩ := (hkey1 i1).symm.trans ((onPort_iff (I := I) (CM := fun j => (CI j).M) (V := V) j a1 _).mp hs.2)\n\
\  obtain ⟨s2, hs2, hc2', -⟩ := mem_proj_callsOf (I := I) (CM := fun j => (CI j).M) ρ2 j b.1 b.2 (sitesF site2 k2) hb\n\
\  rw [List.mem_filter] at hs2\n\
\  obtain ⟨i2, hi2k, rfl⟩ := mem_sitesF hs2.1\n\
\  have e2 : key2.get i2 = ⟨j, b.1⟩ := (hkey2 i2).symm.trans ((onPort_iff (I := I) (CM := fun j => (CI j).M) (V := V) j b.1 _).mp hs2.2)\n\
\  have hi2 : i2 ∈ idx2 ⟨j, b.1⟩ := e2 ▸ hchk2 i2 hi2k\n\
\  have hzF : okP j a1 b.1 = true → (CI j).ann.Follow (a1, a2) b := fun h' =>\n\
\    okP_follow z j (a1, a2) b h' (fun hf => hz1 ⟨j, (a1, a2)⟩ (mem_proj j _ _ ha) hf)\n\
\      (fun hf => hz2 ⟨j, b⟩ (mem_proj j _ _ hb) hf)\n\
\  have h := Tab.allB_spec _ key1 0 k1 hc i1 (Nat.zero_le _) hi1\n\
\  rw [e1, Bool.or_eq_true] at h\n\
\  rcases h with h | h\n\
\  · rw [Bool.and_eq_true] at h\n\
\    have h2 := Tab.allB_spec _ key2 0 k2 hc2 i2 (Nat.zero_le _) hi2k\n\
\    rw [e2, h.1, Bool.not_true, Bool.false_or] at h2\n\
\    exact hzF (okP_task j a1 b.1 h.1 (Nat.lt_of_lt_of_le (Eq.mp Nat.blt_eq h.2) (Nat.le_of_ble_eq_true h2)))\n\
\  have h' := List.all_eq_true.mp h b.1 (portsJ_ok j b.1)\n\
\  rw [Bool.or_eq_true] at h'\n\
\  rcases h' with h' | h'\n\
\  · exact hzF h'\n\
\  · obtain ⟨e, he, ek, hm⟩ := mem_idxT (subS_mem _ _ h' i2 hi2)\n\
\    have hf := AllL.mem (Tab.all_mem hbad e he) i2 hm\n\
\    rw [ek] at hf\n\
\    exact (hf hc1 hc2').elim\n\
\/-- `esc_of_pairs` with its table check needed only below `nR` (the escape table's last leaf may\n\
\    be a rule's). -/\n\
\theorem esc_of_pairsB (u : St → Nat → V) (ops : Ops V) (E : EqC V) (L : List (Nat × Nat))\n\
\    (hL : ∀ p ∈ L, ∀ oc w, Env (P ops E u) oc w → fire ops E oc w (rR p.1) = true → fire ops E oc w (rR p.2) = true →\n\
\      OkT (P ops E u) (trR (P ops E u) oc (rR p.1)).1 (trR (P ops E u) oc (rR p.2)).1)\n\
\    (hinj : ∀ p ∈ L, ∀ x1, x1 < D.nR → D.rc x1 = D.rc p.1 → x1 = p.1)\n\
\    (hT : ∀ i, i < D.nR → Sub (D.escR.get i) (bits ((L.filter (fun p => p.2 == i)).map (fun p => D.rc p.1)))) :\n\
\    ∀ oc w r1 r2, Env (P ops E u) oc w → (D.escR.get ((G ops E u).rk r2)).testBit (D.rc ((G ops E u).rk r1)) = true →\n\
\      fire ops E oc w r1 = true → fire ops E oc w r2 = true → OkT (P ops E u) (trR (P ops E u) oc r1).1 (trR (P ops E u) oc r2).1 := by\n\
\  intro oc w r1 r2 hE hb f1 f2\n\
\  obtain ⟨p, hp, e⟩ := List.mem_map.mp (mem_of_testBit_bits (hT _ r2.isLt _ hb))\n\
\  have hp' := List.mem_filter.mp hp\n\
\  have e2 : p.2 = r2.val := beq_iff_eq.mp hp'.2\n\
\  have e1 : r1.val = p.1 := hinj p hp'.1 _ r1.isLt e.symm\n\
\  have h := hL p hp'.1 oc w hE (by rw [← e1, rR_val]; exact f1) (by rw [e2, rR_val]; exact f2)\n\
\  rw [← e1, e2, rR_val, rR_val] at h\n\
\  exact h\n"

escInjText :: String
escInjText = "theorem escInj : ∀ p ∈ escL, ∀ x1, x1 < D.nR → D.rc x1 = D.rc p.1 → x1 = p.1 := by\n\
\  have h := chains_inj D rinvT.get escMemT 1 (by decide +kernel) (by decide +kernel)\n\
\  intro p hp\n\
\  exact h 0 (by decide) p.1 (by revert p hp; decide)\n\
\theorem escTab : Tab.allB (fun i n => subB n (bits ((escL.filter (fun p => p.2 == i)).map (fun p => D.rc p.1)))) escRT 0 nR = true := by\n\
\  decide +kernel\n\
\theorem escT_ok : ∀ i, i < D.nR → Sub (D.escR.get i) (bits ((escL.filter (fun p => p.2 == i)).map (fun p => D.rc p.1))) :=\n\
\  fun i hi => sub_of_subB (Tab.allB_spec _ escRT 0 nR escTab i (Nat.zero_le _) hi)\n"

nfMet :: String
nfMet = "  nfI m1 m2 h x y := by have e := nf_self nfIT nfOK_I m1 m2 h; subst e; exact notFollow_self m1 x y\n\
\  nfO m1 m2 h x y := by have e := nf_self nfOT nfOK_O m2 m1 h; subst e; exact notFollow_self _ x y\n\
\  ngI m1 m2 h x y := by have e := nf_self ngIT ngOK_I m1 m2 h; subst e; exact fun hg => notFollow_self _ x y hg.1\n\
\  ngO m1 m2 h x y := by have e := nf_self ngOT ngOK_O m2 m1 h; subst e; exact fun hg => notFollow_self _ x y hg.1\n\
\  h3ok m m' h := by simp [D, Tab.get] at h\n\
\  exM _ _ m1 m2 _ h := by simp [D, Tab.get] at h\n"

nfNoMet :: String
nfNoMet = "  nfI m := m.elim\n\
\  nfO m := m.elim\n\
\  ngI m := m.elim\n\
\  ngO m := m.elim\n\
\  h3ok m := m.elim\n\
\  exM _ _ m := m.elim\n"

-- | An exported module's method tables, checked against its annotation by rows (§3.1).
nfXMet :: String
nfXMet = "  nfI m1 m2 h x y := nfI_of m1 m2 h x y\n\
\  nfO m1 m2 h x y := nfO_of m1 m2 h x y\n\
\  ngI m1 m2 h x y := ngI_of m1 m2 h x y\n\
\  ngO m1 m2 h x y := ngO_of m1 m2 h x y\n\
\  h3ok m m' h := h3_of m m' h\n\
\  exM _ _ m1 m2 _ h := by simp [D, Tab.get] at h\n"

nfXText :: String
nfXText = "/-- The exported annotation's column tables are its row tables' transpose (`trB`, by rows), so the\n\
\    scan's method tables are checked against it by rows (cost review r9b-3 M1). -/\n\
\theorem precTr : trB nM xAnnPrecT xAnnPrecCT = true := by decide +kernel\n\
\theorem sbrTr : trB nM xAnnSbrT xAnnSbrCT = true := by decide +kernel\n\
\theorem confTr : trB nM xAnnConfT xAnnConfCT = true := by decide +kernel\n\
\theorem xprec (p q : Fin nM) : xAnn.prec p q = (xAnnPrecT.get p.val).testBit q.val := by\n\
\  show ((xAnnPrecT.get p.val).testBit q.val && (xAnnPrecCT.get q.val).testBit p.val) = _\n\
\  rw [← tr_spec precTr p.isLt q.isLt, Bool.and_self]\n\
\theorem xsbr (p q : Fin nM) : xAnn.sbr p q = (xAnnSbrT.get p.val).testBit q.val := by\n\
\  show ((xAnnSbrT.get p.val).testBit q.val && (xAnnSbrCT.get q.val).testBit p.val) = _\n\
\  rw [← tr_spec sbrTr p.isLt q.isLt, Bool.and_self]\n\
\theorem xconf (p q : Fin nM) : xAnn.conf p q = (xAnnConfT.get p.val).testBit q.val := by\n\
\  show ((xAnnConfT.get p.val).testBit q.val && (xAnnConfCT.get q.val).testBit p.val) = _\n\
\  rw [← tr_spec confTr p.isLt q.isLt, Bool.and_self]\n\
\theorem xprecC (p q : Fin nM) : xAnn.prec p q = (xAnnPrecCT.get q.val).testBit p.val := by\n\
\  rw [xprec, tr_spec precTr p.isLt q.isLt]\n\
\theorem xsbrC (p q : Fin nM) : xAnn.sbr p q = (xAnnSbrCT.get q.val).testBit p.val := by\n\
\  rw [xsbr, tr_spec sbrTr p.isLt q.isLt]\n\
\theorem xconfC (p q : Fin nM) : xAnn.conf p q = (xAnnConfCT.get q.val).testBit p.val := by\n\
\  rw [xconf, tr_spec confTr p.isLt q.isLt]\n\
\/-- Where the scan's method tables set a bit, the exported annotation forbids that order for every\n\
\    argument (`nf`), or that grouping (`ng`), or lets a method hit before (`h3`): each row in its mask. -/\n\
\theorem nfRowsI : Tab.allB (fun e x => subB x (nfMask (xAnnPrecT.get e) (xAnnConfT.get e) (xAnnConfCT.get e) e)) nfIT 0 nM = true := by\n\
\  decide +kernel\n\
\theorem nfRowsO : Tab.allB (fun e x => subB x (nfMask (xAnnPrecCT.get e) (xAnnConfT.get e) (xAnnConfCT.get e) e)) nfOT 0 nM = true := by\n\
\  decide +kernel\n\
\theorem ngRowsI : Tab.allB (fun e x => subB x (ngMask (xAnnPrecT.get e) (xAnnConfT.get e) (xAnnConfCT.get e) (xAnnSbrT.get e)\n\
\    (xAnnSbrCT.get e) e)) ngIT 0 nM = true := by\n\
\  decide +kernel\n\
\theorem ngRowsO : Tab.allB (fun e x => subB x (ngMask (xAnnPrecCT.get e) (xAnnConfT.get e) (xAnnConfCT.get e) (xAnnSbrT.get e)\n\
\    (xAnnSbrCT.get e) e)) ngOT 0 nM = true := by\n\
\  decide +kernel\n\
\theorem h3Rows : Tab.allB (fun e x => subB x (h3Mask (xAnnPrecCT.get e) (xAnnSbrCT.get e) (xAnnConfCT.get e) (xAnnConfT.get e) e))\n\
\    h3okT 0 nM = true := by\n\
\  decide +kernel\n\
\theorem nfI_of (m1 m2 : Fin nM) (h : (nfIT.get m2.val).testBit m1.val = true) (x y : V) : ¬ X.ann.Follow (m1, x) (m2, y) := by\n\
\  rintro ⟨h1, h2, h3, -⟩\n\
\  rcases nfMask_bit (rowB nfRowsI m2.isLt h) with ⟨hne, hb⟩ | hb | hb\n\
\  · have e : xAnn.prec m2 m1 = false := h1 (fun e => hne (congrArg Fin.val e))\n\
\    exact Bool.noConfusion (e.symm.trans ((xprec m2 m1).trans hb))\n\
\  · have e : xAnn.conf m2 m1 = false := h3\n\
\    exact Bool.noConfusion (e.symm.trans ((xconf m2 m1).trans hb))\n\
\  · have e : xAnn.conf m1 m2 = false := h2\n\
\    exact Bool.noConfusion (e.symm.trans ((xconfC m1 m2).trans hb))\n\
\theorem nfO_of (m1 m2 : Fin nM) (h : (nfOT.get m1.val).testBit m2.val = true) (x y : V) : ¬ X.ann.Follow (m1, x) (m2, y) := by\n\
\  rintro ⟨h1, h2, h3, -⟩\n\
\  rcases nfMask_bit (rowB nfRowsO m1.isLt h) with ⟨hne, hb⟩ | hb | hb\n\
\  · have e : xAnn.prec m2 m1 = false := h1 (fun e => hne (congrArg Fin.val e).symm)\n\
\    exact Bool.noConfusion (e.symm.trans ((xprecC m2 m1).trans hb))\n\
\  · have e : xAnn.conf m1 m2 = false := h2\n\
\    exact Bool.noConfusion (e.symm.trans ((xconf m1 m2).trans hb))\n\
\  · have e : xAnn.conf m2 m1 = false := h3\n\
\    exact Bool.noConfusion (e.symm.trans ((xconfC m2 m1).trans hb))\n\
\theorem ngI_of (m1 m2 : Fin nM) (h : (ngIT.get m2.val).testBit m1.val = true) (x y : V) : ¬ X.ann.Group (m1, x) (m2, y) := by\n\
\  rintro ⟨⟨h1, h2, h3, -⟩, h4, h5⟩\n\
\  rcases ngMask_bit (rowB ngRowsI m2.isLt h) with hb | hb | hb\n\
\  · rcases nfMask_bit hb with ⟨hne, hb⟩ | hb | hb\n\
\    · have e : xAnn.prec m2 m1 = false := h1 (fun e => hne (congrArg Fin.val e))\n\
\      exact Bool.noConfusion (e.symm.trans ((xprec m2 m1).trans hb))\n\
\    · have e : xAnn.conf m2 m1 = false := h3\n\
\      exact Bool.noConfusion (e.symm.trans ((xconf m2 m1).trans hb))\n\
\    · have e : xAnn.conf m1 m2 = false := h2\n\
\      exact Bool.noConfusion (e.symm.trans ((xconfC m1 m2).trans hb))\n\
\  · have e : xAnn.sbr m2 m1 = false := h5\n\
\    exact Bool.noConfusion (e.symm.trans ((xsbr m2 m1).trans hb))\n\
\  · have e : xAnn.sbr m1 m2 = false := h4\n\
\    exact Bool.noConfusion (e.symm.trans ((xsbrC m1 m2).trans hb))\n\
\theorem ngO_of (m1 m2 : Fin nM) (h : (ngOT.get m1.val).testBit m2.val = true) (x y : V) : ¬ X.ann.Group (m1, x) (m2, y) := by\n\
\  rintro ⟨⟨h1, h2, h3, -⟩, h4, h5⟩\n\
\  rcases ngMask_bit (rowB ngRowsO m1.isLt h) with hb | hb | hb\n\
\  · rcases nfMask_bit hb with ⟨hne, hb⟩ | hb | hb\n\
\    · have e : xAnn.prec m2 m1 = false := h1 (fun e => hne (congrArg Fin.val e).symm)\n\
\      exact Bool.noConfusion (e.symm.trans ((xprecC m2 m1).trans hb))\n\
\    · have e : xAnn.conf m1 m2 = false := h2\n\
\      exact Bool.noConfusion (e.symm.trans ((xconf m1 m2).trans hb))\n\
\    · have e : xAnn.conf m2 m1 = false := h3\n\
\      exact Bool.noConfusion (e.symm.trans ((xconfC m2 m1).trans hb))\n\
\  · have e : xAnn.sbr m1 m2 = false := h4\n\
\    exact Bool.noConfusion (e.symm.trans ((xsbr m1 m2).trans hb))\n\
\  · have e : xAnn.sbr m2 m1 = false := h5\n\
\    exact Bool.noConfusion (e.symm.trans ((xsbrC m2 m1).trans hb))\n\
\theorem h3_of (m m' : Fin nM) (h : (h3okT.get m'.val).testBit m.val = true) : m ≠ m' ∧\n\
\    ((X.ann.prec m m' = true ∧ X.ann.sbr m m' = true) ∨ X.ann.conf m m' = true ∨ X.ann.conf m' m = true) := by\n\
\  obtain ⟨hne, hb⟩ := h3Mask_bit (rowB h3Rows m'.isLt h)\n\
\  refine ⟨fun e => hne (congrArg Fin.val e), ?_⟩\n\
\  rcases hb with ⟨hp, hs⟩ | hb | hb\n\
\  · exact Or.inl ⟨(xprecC m m').trans hp, (xsbrC m m').trans hs⟩\n\
\  · exact Or.inr (Or.inl ((xconfC m m').trans hb))\n\
\  · exact Or.inr (Or.inr ((xconf m' m).trans hb))\n"

-- | nfText after its method-table lemmas (the chain facts every module with methods states).
nfRest :: String
nfRest = drop (length (fst (breakOn "/-- No method calls a chain" nfText))) nfText
  where breakOn pat str = go "" str
          where go acc r@(c : cs) | take (length pat) r == pat = (reverse acc, r)
                                  | otherwise = go (c : acc) cs
                go acc [] = (reverse acc, [])

nfText :: String
nfText = "theorem nf_self (T : Tab Nat) (hT : Tab.allB (fun m2 n => (List.range nM).all (fun m1 => !n.testBit m1 || Nat.beq m1 m2)) T 0 nM = true)\n\
\    (m1 m2 : Fin nM) (h : (T.get m2.val).testBit m1.val = true) : m1 = m2 := by\n\
\  have h1 := List.all_eq_true.mp (Tab.allB_spec _ T 0 nM hT m2.val (Nat.zero_le _) m2.isLt) m1.val (List.mem_range.mpr m1.isLt)\n\
\  rw [h] at h1\n\
\  exact Fin.ext (Nat.eq_of_beq_eq_true (by simpa using h1))\n\
\theorem notFollow_self (m : Fin nM) (x y : V) : ¬ X.ann.Follow (m, x) (m, y) := fun hf => by\n\
\  have h := hf.2.1; simp [X, xAnn] at h\n\
\theorem nfOK_I : Tab.allB (fun m2 n => (List.range nM).all (fun m1 => !n.testBit m1 || Nat.beq m1 m2)) nfIT 0 nM = true := by decide +kernel\n\
\theorem nfOK_O : Tab.allB (fun m2 n => (List.range nM).all (fun m1 => !n.testBit m1 || Nat.beq m1 m2)) nfOT 0 nM = true := by decide +kernel\n\
\theorem ngOK_I : Tab.allB (fun m2 n => (List.range nM).all (fun m1 => !n.testBit m1 || Nat.beq m1 m2)) ngIT 0 nM = true := by decide +kernel\n\
\theorem ngOK_O : Tab.allB (fun m2 n => (List.range nM).all (fun m1 => !n.testBit m1 || Nat.beq m1 m2)) ngOT 0 nM = true := by decide +kernel\n\
\/-- No method calls a chain (task) port. -/\n\
\theorem cinM_ok (ops : Ops V) (E : EqC V) (u : St → Nat → V) (oc : OC V) (w : Wires (Fin nM) V) (m : Fin nM) :\n\
\    ∀ c ∈ (trM (P ops E u) oc w m).1, (G ops E u).ch c.1 = true →\n\
\      (G ops E u).clo ((G ops E u).mi m) ≤ (G ops E u).pos c.1 c.2.1 ∧ (G ops E u).pos c.1 c.2.1 < (G ops E u).chi ((G ops E u).mi m) := by\n\
\  rw [trM_eq]\n\
\  have hc := chain_in_of_sites (G ops E u) (envS ops E (u (.inl m)) oc (nR + m.val) (w.arg m)) _ _ _ (chainS ops E u (nR + m.val) (mlt m) (w.arg m)).1\n\
\  have e1 : (bif Nat.blt (nR + m.val) nR then stF (nR + m.val) else 0) = 0 := by\n\
\    rw [show Nat.blt (nR + m.val) nR = false from Bool.eq_false_iff.mpr (fun h' => absurd (Nat.blt_eq.mp h') (Nat.not_lt.mpr (Nat.le_add_right nR m.val)))]; rfl\n\
\  have e2 : (bif Nat.blt (nR + m.val) nR then stF (nR + m.val + 1) else 0) = 0 := by\n\
\    rw [show Nat.blt (nR + m.val) nR = false from Bool.eq_false_iff.mpr (fun h' => absurd (Nat.blt_eq.mp h') (Nat.not_lt.mpr (Nat.le_add_right nR m.val)))]; rfl\n\
\  intro c hcm hch\n\
\  have := hc c hcm hch\n\
\  rw [e1, e2] at this\n\
\  omega\n\
\theorem cincM_ok (ops : Ops V) (E : EqC V) (u : St → Nat → V) (oc : OC V) (w : Wires (Fin nM) V) (m : Fin nM) (j : I)\n\
\    (a b : (CI j).M × V) (hj : (G ops E u).ch j = true) (hab : [a, b] <+ proj j (trM (P ops E u) oc w m).1) :\n\
\    (G ops E u).pos j a.1 < (G ops E u).pos j b.1 := by\n\
\  have ha := mem_proj j _ a (hab.subset (List.mem_cons_self ..))\n\
\  have := (cinM_ok ops E u oc w m ⟨j, a⟩ ha hj).2\n\
\  simp only [G, chiG, show m.val < D.nM from m.isLt, if_true] at this\n\
\  omega\n"
