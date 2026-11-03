-- | The emitter's plan of one module (mke2e.py 77-972, 1630-1760, 1872-1960): check groups,
-- last-caller ports, scan masks, escape facts, muxes and caller-exclusivity facts.
module Lean.Sched.Emit.Plan
    ( mkPlan, orderMux
    ) where

import Control.Monad (forM, forM_, when, unless)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Trans (liftIO)
import Data.Array ((!), listArray)
import Data.Bits (Bits(testBit))
import Data.List (maximumBy, elemIndex, sort, sortOn)
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Foldable as F (Foldable(toList))
import qualified Data.Map.Strict as M (Map, (!), elems, empty, findWithDefault, lookup, member, insert,
    fromList, fromListWith)
import qualified Data.Set as S (Set, member, delete, difference, empty, fromList, intersection, null, size,
    toAscList, union)
import qualified Data.Sequence as Sq (index)

import Lean.Sched.Bounds (Bounds(bnGateSelmux, bnGateMuxPairs, bnGateEsc, bnGateEscFacts, bnGateModFacts))
import Lean.Sched.BscFacts (BscLookup)
import Lean.Sched.Emit.Ctx (argHw, isUncond, mxKind, EC(..), PInfo(PInfo))
import Lean.Sched.Facts (FactEngine(..), FactShare, newFactEngine)
import Lean.Sched.Kinds (kdFx, KindDef(kdPorts, kdAlwaysRdy, kdHasEn, kdHasArg))
import Lean.Sched.MECheck (render)
import Lean.Sched.Model (exclSrc, mportRepr, muxPairs, users, DView(dvCF), InvRef(InvFact), MCheck(ckLst,
    ckFull, ckLine, ckHead), MFact(fOk), MSite(sArg, sKind, sPort, sFact, sInv), MStep(tSites, tName),
    Model(mByk, mNR, mNM, mNS, mChecks, mCfChecks, mNSlots, mD, mK, mFacts, mRules, mFanOutside, mNT, mBlk,
    mKinds))
import Lean.Sched.Py (strip)
import Lean.Sched.Scan (escPairs, mannPlain, methTables, scanMasks, MAnn(maRel))
import Lean.Sched.Sites (Sites)
import Lean.Solver (Solver)

type PM' = ExceptT String IO

assertP :: Bool -> String -> PM' ()
assertP c msg = unless c (throwError ("AssertionError: " ++ msg))

-- | The plan, or the prototype's stop message (its exception's last line).
mkPlan :: Solver -> FactShare -> DView -> Sites -> BscLookup -> Bounds -> String -> Model -> IO (Either String EC)
mkPlan sv share dv sites bl bnd name m = runExceptT $ do
    let nR = mNR m
        nM = mNM m
        nS = mNS m
        met = nM > 0
        rulesA = listArray (0, nS - 1) (mRules m)
        siteOf k i = Sq.index (tSites (rulesA ! k)) i
        mnames = [ tName r | r <- drop nR (mRules m) ]
        plain = mannPlain mnames (dvCF dv)
        mconf a b = a == b || not (testBit (maRel plain ! b) a)
        checks = mChecks m
        chkA = listArray (0, length checks - 1) checks
    -- conflict_free checks (user assertions) enter the gate's solver calls; a file cannot state them yet
    assertP (null (mCfChecks m)) "conflict_free run-time checks (user assertions) are not emitted yet"
    when met $ assertP (and [ not (mconf a b) | a <- [0 .. nM - 1], b <- [0 .. nM - 1], a /= b ]) "non-CF method pairs"
    -- bsc's mutually_exclusive checks as suffix groups: a group [x0, .., xn] has the checks (x_j, [x_j+1 .. xn])
    let bylst = M.fromListWith (flip (++)) [ (ckLst ch, [c]) | (c, ch) <- zip [0 :: Int ..] checks ]
        grpStep (groups, chkG) (c0, ch0)
            | length (ckLst ch0) /= 1 || M.member c0 chkG = return (groups, chkG)
            | otherwise = do
                let grow sq chain = case [ c | c <- M.findWithDefault [] sq bylst, not (M.member c chkG), c `notElem` chain ] of
                        [] -> (sq, chain)
                        (n : _) -> grow (ckHead (chkA ! n) : sq) (chain ++ [n])
                    (sq, chain) = grow (ckHead ch0 : ckLst ch0) [c0]
                    g = length groups
                cs <- forM chain $ \c -> do
                    let j = fromMaybe 0 (elemIndex (ckHead (chkA ! c)) sq)
                    assertP (ckLst (chkA ! c) == drop (j + 1) sq) "a check is not its group suffix"
                    return (c, (g, j))
                return (groups ++ [sq], foldl' (\mp (c, x) -> M.insert c x mp) chkG cs)
    (groups, chkG) <- foldlM' grpStep ([], M.empty) (zip [0 ..] checks)
    let litc = [ c | c <- [0 .. length checks - 1], not (M.member c chkG) ]
        liti = M.fromList (zip litc [0 ..])
        ruleName k = tName (rulesA ! k)
    -- trust point 3: each check a file states, re-rendered, is bsc's line
    forM_ checks $ \ch -> when (ckFull ch) $
        assertP (render [ruleName (ckHead ch)] (map ruleName (ckLst ch)) == strip (ckLine ch))
                ("a stated check differs from bsc's line: " ++ take 80 (ckLine ch))
    let u0 = users m
    assertP (muxPairs m <= bnGateMuxPairs bnd) ("gate: " ++ show (muxPairs m) ++ " mux member pairs proved one by one, above the bound " ++ show (bnGateMuxPairs bnd))
    assertP (and [ S.size (S.fromList ps) == length ps | r <- mRules m, let ps = map sPort (F.toList (tSites r)) ])
            "two sites of one step on one port"
    fe <- liftIO (newFactEngine sv share dv sites bl m mconf)
    lcp0 <- liftIO (feLCP fe)
    forM_ lcp0 $ \(_, _, allRules) -> assertP allRules "a last-caller port with method users (not emitted yet)"
    let lcp = [ p | (p, _, _) <- lcp0 ]
        vb = M.fromList [ (p, mNSlots m + t) | (t, p) <- zip [0 ..] lcp ]
        kv = length (mKinds m)
        lcSlots = [ (vb M.! p, S.toAscList (S.fromList (map fst (u0 M.! p)))) | p <- lcp ]
        nslots = mNSlots m + length lcp
        bitLen x = if x <= 0 then 0 else 1 + bitLen (x `div` 2) :: Int
        d = if null lcp then mD m else max 1 (bitLen (nslots - 1))
        virt = [ [ (vb M.! p, kv, 0) | p <- lcp, any ((== k) . fst) (u0 M.! p) ] | k <- [0 .. nR - 1] ]
        mann = if met then Just plain else Nothing
    (need, esc, dn) <- case scanMasks m lcSlots mann of
        Left err -> throwError (pyErrLine err)
        Right x -> return x
    let escl = escPairs m esc
        u = u0
        kdOf k = mK m M.! k
        rdyGuarded (k, _, p) = not (kdAlwaysRdy (kdOf k) p)
        rdc = listArray (0, nS - 1) [ [ i | (i, s) <- zip [0 ..] (F.toList (tSites r)), sKind s == "call", not (isUncond s), rdyGuarded (sPort s) ]
                                    | r <- mRules m ]
        facts = mFacts m
        factK = M.fromList (concat [ [ (f, k) | Just f <- [sFact s] ] ++ [ (f, k) | Just (InvFact f) <- [sInv s] ]
                                   | (k, r) <- zip [0 ..] (mRules m), s <- F.toList (tSites r) ])
    assertP (mFanOutside m == 0) "an R literal reads a port that is not a read site of its step"
    assertP (length escl <= bnGateEsc bnd && maximum (0 : [ length b | (_, _, b) <- escl ]) <= bnGateEscFacts bnd
             && sum [ length b | (_, _, b) <- escl ] <= bnGateModFacts bnd) "gate: escaped pairs or their facts above the bound"
    efacts <- liftIO (feEscape fe escl)
    -- the muxes and every port's arms (computed before the text: the caller-exclusivity facts need them)
    let ec0 = EC { ecM = m, ecName = name, ecRules = rulesA, ecNR = nR, ecNM = nM, ecNS = nS, ecNT = mNT m, ecMet = met
                 , ecKinds = mKinds m, ecGroups = groups, ecChkG = chkG, ecLitc = litc, ecLiti = liti
                 , ecLCP = lcp, ecVB = vb, ecKV = kv, ecCfgLib = "cfg" `elem` mKinds m, ecVirt = virt
                 , ecNSlots = nslots, ecD = d, ecNeed = need, ecEsc = esc, ecDn = dn, ecESC = escl, ecU = u
                 , ecRDC = rdc, ecFactOk = listArray (0, length facts - 1) (map fOk facts), ecFactK = factK
                 , ecEFacts = efacts, ecCFacts = [], ecCFIdx = M.empty, ecMuxes = [], ecSelmux = M.empty, ecMgrp = M.empty, ecLCMux = []
                 , ecPortInfo = M.empty, ecMultiBlk = any ((> 1) . length) (M.elems (mBlk m))
                 , ecHasUnconf = False, ecHyp = "", ecHya = ""
                 , ecWXM = "Wires " ++ xm ++ " V", ecXM = xm
                 , ecMann = fmap (\ma -> let (a, b, c, e, f) = methTables ma in (a, b, c, e, f)) mann }
        xm = if met then "(Fin nM)" else "Empty"
        chkHeads = M.fromListWith (flip (++)) [ (ckHead ch, [c]) | (c, ch) <- zip [0 ..] checks, not (M.member c chkG) ]
        exclNone a b = isNothing (exclSrc m a b)
    (muxes, mgrp, selmux, lcmux, pinfo) <- foldlM' (\acc@(mx, mg, sm, lm, pim) p@(kd', _, pm) -> do
            let kdef = kdOf kd'
                us = M.findWithDefault [] p u
                args = [ sArg (siteOf k si) | (k, si) <- us ]
                hwargs = [ argHw ec0 k (siteOf k si) | (k, si) <- us ]
            if not (kdHasEn kdef pm || (kdHasArg kdef pm && not (kdFx kdef pm))) then return acc
            else if null us then return (mx, mg, sm, lm, M.insert p (PInfo [] "none" 0 [] [] M.empty) pim)
            else do
                when (any ((>= nR) . fst) us) $ assertP (all ((>= nR) . fst) us) ("a port with method and rule users: " ++ show (mportRepr p))
                let argk0 | not (kdHasArg kdef pm) = "none"
                          | S.size (S.fromList hwargs) == 1 = if mxKind (head args) `elem` ["z", "k"] then "const"
                                                              else if length us == 1 then "single" else "mux"
                          | otherwise = "mux"
                    argk = if not (null args) && mxKind (head args) == "x" && argk0 /= "none"
                           then (if length us == 1 then "single" else "mux") else argk0
                if argk == "mux" && p `elem` lcp then do
                    let q = length lm
                        mus = sortOn (negate . fst) us
                    assertP (map fst us == sort (map fst us)) "users of a port not in rank order"
                    assertP (S.size (S.fromList (map fst mus)) == length mus) "a step uses a last-caller port twice"
                    return (mx, mg, sm, lm ++ [(p, mus)], M.insert p (PInfo us "lc" q mus [ siteOf k si | (k, si) <- mus ] M.empty) pim)
                else if argk == "mux" then do
                    let q = length mx
                        (order, plan, grp) = orderMux groups checks chkHeads us
                    assertP (length order == length us) "a step uses a mux port twice"
                    let pos k = fromMaybe 0 (elemIndex k order)
                        mus = sortOn (pos . fst) us
                        margs = [ siteOf k si | (k, si) <- mus ]
                        sel = any (not . isUncond) margs || or [ exclNone a b | (a, _) <- mus, (b, _) <- mus, a < b ]
                    return ( mx ++ [(p, mus, plan)], maybe mg (\g -> M.insert q g mg) grp, M.insert q sel sm, lm
                           , M.insert p (PInfo us "mux" q mus margs (M.fromList (zip (map fst mus) [0 ..]))) pim )
                else return (mx, mg, sm, lm, M.insert p (PInfo us argk 0 [] [] M.empty) pim))
        ([], M.empty, M.empty, [], M.empty)
        [ (k, i, pm) | k <- mKinds m, k /= "tasks", i <- [0 .. length (mByk m M.! k) - 1], pm <- kdPorts (kdOf k) ]
    -- caller exclusivity of mux users that no recorded exclusion covers
    cfacts <- fmap concat $ forM (zip [0 ..] muxes) $ \(q, (p, mus, _)) -> if not (M.findWithDefault False q selmux) then return [] else do
        let ks = map fst mus
        assertP ((maybe False (\(_, g) -> length g == length mus) (M.lookup q mgrp)) || length ks <= bnGateSelmux bnd)
                ("gate: users of one port proved pair by pair " ++ show (length ks) ++ " above the bound " ++ show (bnGateSelmux bnd))
        fmap concat $ forM [ (x, y) | (a, x) <- zip [0 :: Int ..] ks, y <- drop (a + 1) ks ] $ \(x, y) ->
            if not (exclNone x y) then return [] else do
                let sx = fromMaybe 0 (lookup x mus)
                    sy = fromMaybe 0 (lookup y mus)
                (how, bt) <- liftIO (feCx fe p (x, sx) (y, sy))
                return [(q, x, y, sx, sy, how, bt)]
    let hasUnconf = any (not . fOk) facts || any (\(_, _, _, _, h, _) -> isNothing h) efacts || any (\(_, _, _, _, _, h, _) -> isNothing h) cfacts
        hrArg = if hasUnconf then " (hr : Unconfirmed ops E)" else ""
    return ec0 { ecCFacts = cfacts, ecCFIdx = M.fromList [ ((q, x, y), (n, h)) | (n, (q, x, y, _, _, h, _)) <- zip [0 ..] cfacts ], ecMuxes = muxes, ecSelmux = selmux, ecMgrp = mgrp, ecLCMux = lcmux, ecPortInfo = pinfo
               , ecHasUnconf = hasUnconf, ecHyp = "(hy : Hyps ops E)" ++ hrArg, ecHya = "hy" ++ (if hasUnconf then " hr" else "") }
  where
    pyErrLine err = case break (== ' ') err of
        (t, ' ' : r) | t `elem` ["ValueError", "AssertionError"] -> t ++ ": " ++ r
        _ -> err

foldlM' :: Monad m => (b -> a -> m b) -> b -> [a] -> m b
foldlM' f z0 xs = go z0 xs
  where go z [] = return z
        go z (y : ys) = f z y >>= \z' -> z' `seq` go z' ys

-- | mke2e.order_mux: a group's members last, the rest greedily by the check covering most
-- others; each member's covering check and the members it leaves uncovered.
orderMux :: [[Int]] -> [MCheck] -> M.Map Int [Int] -> [(Int, Int)] -> ([Int], M.Map Int (Maybe Int, S.Set Int), Maybe (Int, [Int]))
orderMux groups checks chkHeads us = (order, plan, grp)
  where
    chkA = listArray (0, length checks - 1) checks
    mem = S.fromList (map fst us)
    bestG = if null groups then Nothing
            else Just (fst (maximumBy (\(i, a) (j, b) -> compare a b <> compare j i)
                                      [ (g, S.size (S.intersection mem (S.fromList gr))) | (g, gr) <- zip [0 ..] groups ]))
    gs0 = maybe [] (\g -> [ x | x <- groups !! g, S.member x mem ]) bestG
    gs = if length gs0 < 2 then [] else gs0
    grp = if null gs then Nothing else fmap (\g -> (g, gs)) bestG
    gset = S.fromList gs
    go rem acc pl
        | S.null rem = (reverse acc, pl)
        | otherwise =
            let cands = [ ((S.size (S.difference others cov), x), (x, cb, S.difference others cov))
                        | x <- S.toAscList rem
                        , let others = S.delete x rem
                              (cb, cov) = foldl' (\(b, cv) c -> let cs = S.intersection (S.fromList (ckLst (chkA ! c))) others
                                                               in if S.size cs > S.size cv then (Just c, cs) else (b, cv))
                                                 (Nothing, S.empty) (M.findWithDefault [] x chkHeads) ]
                (_, (x, cb, unc)) = minimumOn fst cands
            in go (S.delete x rem) (x : acc) (M.insert x (cb, S.union unc gset) pl)
    (order0, plan0) = go (S.difference mem gset) [] M.empty
    order = order0 ++ gs
    plan = foldl' (\pl x -> M.insert x (Nothing, S.empty) pl) plan0 gs
    minimumOn f = foldr1 (\a b -> if f a <= f b then a else b)

