-- | The scan (scanlib.py, model.scan_masks): Scan.lean's pass mirrored, the
-- exception masks it needs, the escaped rule pairs and the rule codes.
module Lean.Sched.Scan
    ( MAnn(..), mannPlain, mannGate, mannExport, ScanDesign(..), Ent, kindTable, runScan, compressCodes
    , scanDesign, scanMasks, escPairs, ScanErr, St, runScanStates, methTables
    ) where

import Data.Array (Array, (!), listArray)
import Data.Maybe (fromMaybe)
import Data.Bits (Bits((.&.), setBit, xor, testBit, shiftR, clearBit, bit, (.|.), complement))
import qualified Data.Foldable as F (Foldable(toList))
import qualified Data.IntMap.Strict as IM (IntMap, empty, findWithDefault, fromListWith, insert)
import qualified Data.Map.Strict as M ((!), findWithDefault, keys, toList, fromList, fromListWith)
import qualified Data.Set as S (fromList, size, toAscList, toList)

import Lean.Sched.Export (Export(exMeths), exConfB, exPrecB, exSbrB)
import Lean.Sched.Kinds (kdFx, KindDef(kdShare, kdPorts, kdSbr, kdPrec, kdConf, kdCodeRel), PM)
import Lean.Sched.Model (ent, MSite(sPort), MStep(tSites), Model(mK, mNSlots, mNM, mKinds, mBlk, mDFind,
    mMEFind, mRules, mNR), PyErr(PyErr))

-- | The module's own annotation over its method steps, as bit masks per method:
-- @maRel m@ the others it is not C with; prec and sbr both ways.
data MAnn = MAnn { maN :: Int, maRel, maPrecTo, maPrecFrom, maSbrTo, maSbrFrom :: Array Int Integer
                 , maTabs :: Maybe ([Integer], [Integer], [Integer], [Integer], [Integer]) }

maskOf :: Int -> [(Int, Int)] -> Array Int Integer
maskOf n ps = listArray (0, n - 1) [ foldl' setBit 0 (IM.findWithDefault [] m acc) | m <- [0 .. n - 1] ]
  where acc = IM.fromListWith (++) [ (m, [a]) | (m, a) <- ps ]

-- | MAnn: methods pairwise CF where bsc exports CF, each C with itself.
mannPlain :: [String] -> [(String, String)] -> MAnn
mannPlain names cf = MAnn n (maskOf n rel) e e e e Nothing
  where
    n = length names
    idx = M.fromListWith (++) (zip names (map pure [0 ..]))
    ix x = M.findWithDefault [] x idx
    rel = [ (bi, ai) | (x, y) <- concat [ [(a, b), (b, a)] | (a, b) <- cf ], ai <- ix x, bi <- ix y, ai /= bi ]
    e = maskOf n []

-- | GMAnn (the gate's run): bsc's exported CF, SB and SBR; unlisted pairs C.
mannGate :: [String] -> [(String, String)] -> [(String, String, String)] -> MAnn
mannGate names cf sbsbr = MAnn n (maskOf n rel) (maskOf n [ (a, b) | (a, b) <- precs ]) (maskOf n [ (b, a) | (a, b) <- precs ])
                               (maskOf n [ (a, b) | (a, b) <- sbrs ]) (maskOf n [ (b, a) | (a, b) <- sbrs ]) Nothing
  where
    n = length names
    idx = M.fromListWith (++) (zip names (map pure [0 ..]))
    ix x = M.findWithDefault [] x idx
    sb = [ (a, b) | (a, r, b) <- sbsbr, r == "SB" ]
    sbr = [ (a, b) | (a, r, b) <- sbsbr, r == "SBR" ]
    -- related both ways (duplicates only set a bit twice)
    related = concat [ [(a, b), (b, a)] | (a, b) <- cf ++ sb ++ sbr ]
    rel = [ (bi, ai) | (x, y) <- related, ai <- ix x, bi <- ix y, ai /= bi ]
    precs = [ (ai, bi) | (x, y) <- sb ++ sbr, ai <- ix x, bi <- ix y, ai /= bi ]
    sbrs = [ (ai, bi) | (x, y) <- sbr, ai <- ix x, bi <- ix y ]

-- | A synthesized child's exported annotation (design.md §3.1): its tables say exactly where Follow
-- and Group fail for every argument (a value method's own bit is clear: two calls share one wire).
mannExport :: Export -> MAnn
mannExport ex = MAnn n (maskOf n [ (m, a) | m <- ms, a <- ms, a /= m, not (exConfB ex a m) ]) e e e e (Just (nfI, nfO, ngI, ngO, h3ok))
  where
    n = length (exMeths ex)
    ms = [0 .. n - 1]
    e = maskOf n []
    bits xs = foldl' setBit 0 xs :: Integer
    nf a b = if a == b then exConfB ex a a else exPrecB ex b a || exConfB ex a b || exConfB ex b a
    ng a b = nf a b || exSbrB ex a b || exSbrB ex b a
    nfI = [ bits [ a | a <- ms, nf a b ] | b <- ms ]
    nfO = [ bits [ b | b <- ms, nf a b ] | a <- ms ]
    ngI = [ bits [ a | a <- ms, ng a b ] | b <- ms ]
    ngO = [ bits [ b | b <- ms, ng a b ] | a <- ms ]
    h3ok = [ bits [ m | m <- ms, m /= m', (exPrecB ex m m' && exSbrB ex m m') || exConfB ex m m' || exConfB ex m' m ] | m' <- ms ]

-- | The parent's interface tables (nfI, nfO, ngI, ngO, h3ok) from its annotation.
methTables :: MAnn -> ([Integer], [Integer], [Integer], [Integer], [Integer])
methTables ma | Just t <- maTabs ma = t
methTables ma = (nfI, nfO, ngI, ngO, h3ok)
  where
    n = maN ma
    full = bit n - 1 :: Integer
    notRel m = full .&. complement (maRel ma ! m)
    nfI = [ bit m .|. maPrecTo ma ! m .|. notRel m | m <- [0 .. n - 1] ]
    nfO = [ bit m .|. maPrecFrom ma ! m .|. notRel m | m <- [0 .. n - 1] ]
    ngI = [ x .|. maSbrFrom ma ! m .|. maSbrTo ma ! m | (m, x) <- zip [0 ..] nfI ]
    ngO = [ x .|. maSbrTo ma ! m .|. maSbrFrom ma ! m | (m, x) <- zip [0 ..] nfO ]
    h3ok = [ clearBit ((maPrecFrom ma ! m .&. maSbrFrom ma ! m) .|. notRel m) m | m <- [0 .. n - 1] ]

type Ent = (Int, Int, Int)
type KC = ([Int], [Int], [Int], [Int])

-- | scanlib.Kind.kc for every code of a kind.
kindTable :: KindDef -> [KC]
kindTable k = [ kc q0 | q0 <- codes ]
  where
    ps = kdPorts k
    n = length ps
    codes = [0 .. n - 1]
    pa = listArray (0, n - 1) ps :: Array Int PM
    (precI, sbrI, confI) = fromMaybe (onPM kdPrec, onPM kdSbr, onPM kdConf) (kdCodeRel k)
    onPM f a b = f k (pa ! a) (pa ! b)
    shareA = listArray (0, n - 1) (map (kdShare k) ps) :: Array Int Bool
    fxA = listArray (0, n - 1) (map (kdFx k) ps) :: Array Int Bool
    folP p q = (p == q || not (precI q p)) && not (confI p q) && not (confI q p)
    grpP p q = folP p q && not (sbrI p q) && not (sbrI q p)
    argP p q = p /= q || not (shareA ! p) || fxA ! p
    kc q0 = ( [ p | p <- codes, not (folP p q0 && argP p q0) ], [ q | q <- codes, not (folP q0 q && argP q0 q) ]
            , [ p | p <- codes, not (grpP p q0 && argP p q0) ], [ q | q <- codes, not (grpP q0 q && argP q0 q) ] )

data ScanDesign = ScanDesign
    { scNM, scNR, scD :: Int, scFpM, scFpR :: [[Ent]], scKt :: Array Int (Array Int KC)
    , scNfI, scNfO, scNgI, scNgO, scH3ok, scExM :: Array Int Integer
    , scExR, scExMR, scEscR :: [Integer] }

type ScanErr = String

arr :: [a] -> Array Int a
arr xs = listArray (0, length xs - 1) xs

-- | model.design, with the last-caller ports' virtual slots (@lc@: slot and users).
scanDesign :: Model -> [(Int, [Int])] -> [Integer] -> [Integer] -> Maybe MAnn -> Either ScanErr ScanDesign
scanDesign m lc exR escR mann = do
    fps <- mapM (\r -> mapM (\s -> either (\(PyErr t msg) -> Left (t ++ " " ++ msg)) Right (ent m (sPort s))) (F.toList (tSites r))) (mRules m)
    let nR = mNR m
        nM = mNM m
        kv = length (mKinds m)
        virt k = [ (slot, kv, 0) | (slot, us) <- lc, k `elem` us ]
        fp = [ f ++ virt k | (k, f) <- zip [0 ..] fps ]
        kt = [ if k == "tasks" then arr [([], [0], [], [0])] else arr (kindTable (mK m M.! k)) | k <- mKinds m ] ++ (if null lc then [] else [arr [([], [0], [], [])]])
        z = arr [0]
    case mann of
        Just ma | nM > 0 -> do
            let (nfI, nfO, ngI, ngO, h3ok) = methTables ma
            return (ScanDesign nM nR (dOf lc) (drop nR fp) (take nR fp) (arr kt) (arr nfI) (arr nfO) (arr ngI) (arr ngO) (arr h3ok)
                               (arr (replicate nM 0)) exR (replicate nR 0) escR)
        _ -> return (ScanDesign 0 nR (dOf lc) [] (take nR fp) (arr kt) z z z z z z exR (replicate nR 0) escR)
  where dOf lc' = let o = mNSlots m + length lc' in max 1 (bitLen (o - 1))
        bitLen x = if x <= 0 then 0 else 1 + bitLen (x `div` 2)

type St = IM.IntMap (Int, Integer, Integer, Integer)

getS :: St -> Int -> (Int, Integer, Integer, Integer)
getS st s = IM.findWithDefault (0, 0, 0, 0) s st

subB :: Integer -> Integer -> Bool
subB a b = a .&. b == a

clr :: Integer -> Integer -> Integer
clr a b = a `xor` (a .&. b)

entS :: Ent -> String
entS (b, kd, c) = "(" ++ show b ++ ", " ++ show kd ++ ", " ++ show c ++ ")"

-- | scanlib.run_scan: Right () when the pass succeeds, else its failure as Python prints it.
runScan :: ScanDesign -> Maybe [Int] -> Either String ()
runScan sd rc = fmap (const ()) (runScanStates sd rc)

-- | run_scan's states: the slot map after each step (Python's per-step dict).
runScanStates :: ScanDesign -> Maybe [Int] -> Either String [St]
runScanStates sd rc = go 0 IM.empty
  where
    nM = scNM sd
    n = nM + scNR sd
    code i = maybe i (!! i) rc
    fpM = arr (scFpM sd)
    fpR = arr (scFpR sd)
    orOf st b f cs = foldl' (.|.) 0 [ f (getS st (b + c2)) | c2 <- cs ]
    f1 (_, x, _, _) = x
    f2 (_, _, x, _) = x
    f3 (_, _, _, x) = x
    go g st
        | g >= n = Right []
        | g < nM = do
            let k = g
            st' <- foldlE (\s e@(b, kd, c) -> do
                let (cin, cout, gin, gout) = scKt sd ! kd ! c
                if any (\c2 -> let (x, _, _, _) = getS s (b + c2) in x == k + 1) gin
                    then Left ("('grpM', " ++ show k ++ ", " ++ entS e ++ ")") else Right ()
                let (_, o1, o2, o3) = getS s (b + c)
                    s' = IM.insert (b + c) (k + 1, o1 .|. bit k, o2, o3) s
                    ex = clr (scExM sd ! k) (bit k)
                if subB (orOf s' b f1 cin) (scNfI sd ! k .|. ex) && subB (orOf s' b f1 cout) (scNfO sd ! k .|. ex)
                   && subB (orOf s' b f1 gin) (scNgI sd ! k .|. ex) && subB (orOf s' b f1 gout) (scNgO sd ! k .|. ex)
                    then Right s' else Left ("('mm', " ++ show k ++ ", " ++ entS e ++ ")")) st (fpM ! k)
            (st' :) <$> go (g + 1) st'
        | otherwise = do
            let i = g - nM
            (st1, dh, ph, bm) <- foldlE (\(s, dh, ph, bm) e@(b, kd, c) -> do
                let (cin, cout, gin, _) = scKt sd ! kd ! c
                if any (\c2 -> let (x, _, _, _) = getS s (b + c2) in x == g + 1) gin
                    then Left ("('grpR', " ++ show i ++ ", " ++ entS e ++ ")") else Right ()
                let (_, o1, o2, o3) = getS s (b + c)
                    s' = IM.insert (b + c) (g + 1, o1, o2, o3) s
                    bad = orOf s' b f2 cin
                if not (subB bad ((scExR sd !! i) .|. (scEscR sd !! i)))
                    then Left ("('h4', " ++ show i ++ ", " ++ entS e ++ ", " ++ show bad ++ ")") else Right ()
                return (s', dh .|. orOf s' b f1 cout, ph .|. orOf s' b f3 cout, bm .|. orOf s' b f1 cin)) (st, 0, 0, 0) (fpR ! i)
            let h = clr (dh .|. ph) (scExMR sd !! i)
                bb = clr bm (scExMR sd !! i)
            case [ mm | h /= 0, mm <- bitsOf bb, not (subB h (scH3ok sd ! mm .|. clr (scExM sd ! mm) (bit mm))) ] of
                (mm : _) -> Left ("('h3', " ++ show i ++ ", " ++ show mm ++ ")")
                [] -> return ()
            let st2 = foldl' (\s (b, _, c) -> let (o0, o1, o2, o3) = getS s (b + c)
                                              in IM.insert (b + c) (o0, o1, o2 .|. bit (code i), o3 .|. h) s) st1 (fpR ! i)
            (st2 :) <$> go (g + 1) st2

foldlE :: (b -> a -> Either e b) -> b -> [a] -> Either e b
foldlE f = go
  where go acc [] = Right acc
        go acc (x : xs) = f acc x >>= \acc' -> acc' `seq` go acc' xs

bitsOf :: Integer -> [Int]
bitsOf x = go 0 x
  where go _ 0 = []
        go i y = (if testBit y 0 then (i :) else id) (go (i + 1) (y `shiftR` 1))

-- | scanlib.compress: how many distinct rule codes.
compressCodes :: ScanDesign -> Int
compressCodes sd = S.size (S.fromList rc)
  where
    part = S.fromList (concat [ bitsOf (a .|. b) | (a, b) <- zip (scExR sd) (scEscR sd) ])
    codeOf = M.fromList (zip (S.toAscList part) [1 :: Int ..])
    rc = [ M.findWithDefault 0 j codeOf | j <- [0 .. scNR sd - 1] ]

-- | model.scan_masks: the exception bits the scan needs and the escaped pairs,
-- and the design they make; or the failure.
scanMasks :: Model -> [(Int, [Int])] -> Maybe MAnn -> Either ScanErr ([Integer], [Integer], ScanDesign)
scanMasks m lc mann = do
    let nR = mNR m
        pairs = S.fromList (concat [ [(x, y), (y, x)] | (x, ys) <- M.toList (mBlk m), y <- ys ]
                            ++ M.keys (mDFind m) ++ M.keys (mMEFind m))
        fullM = IM.fromListWith (.|.) [ (y, bit x) | (x, y) <- S.toList pairs, x /= y, x < nR, y < nR, x >= 0, y >= 0 ]
        full = [ IM.findWithDefault 0 y fullM | y <- [0 .. nR - 1] ]
        fullA = arr full
    dz <- scanDesign m lc full (replicate nR 0) mann
    let nM = scNM dz
        st0 = foldl' (\s (k, fps) -> foldl' (\s' (b, _, c) -> let (_, o1, o2, o3) = getS s' (b + c)
                                                            in IM.insert (b + c) (k + 1, o1 .|. bit k, o2, o3) s') s fps)
                     IM.empty (zip [0 ..] (scFpM dz))
    (_, needL, escL) <- foldlE (\(s, nd, es) (i, fps) -> do
            let g = nM + i
            (s1, bad) <- foldlE (\(s', badAcc) (b, kd, c) -> do
                let (cin, _, gin, _) = scKt dz ! kd ! c
                if any (\c2 -> let (x, _, _, _) = getS s' (b + c2) in x == g + 1) gin
                    then Left ("ValueError grp fails at rule " ++ show i) else Right ()
                let (_, o1, o2, o3) = getS s' (b + c)
                    s'' = IM.insert (b + c) (g + 1, o1, o2, o3) s'
                    bd = clearBit (foldl' (.|.) 0 [ let (_, _, x, _) = getS s'' (b + c2) in x | c2 <- cin ]) i
                return (s'', badAcc .|. bd)) (s, 0) fps
            let s2 = foldl' (\s' (b, _, c) -> let (o0, o1, o2, o3) = getS s' (b + c) in IM.insert (b + c) (o0, o1, o2 .|. bit i, o3) s') s1 fps
                fi = fullA ! i
            return (s2, (bad .&. fi) : nd, (bad .&. complement fi) : es)) (st0, [], []) (zip [0 ..] (scFpR dz))
    let need = reverse needL
        esc = reverse escL
    dn <- scanDesign m lc need esc mann
    case runScan dn Nothing of
        Left info -> Left ("AssertionError " ++ info)
        Right () -> Right (need, esc, dn)

-- | model.esc_pairs: per escaped pair (r1 earlier, r2), the statically failing site pairs.
escPairs :: Model -> [Integer] -> [(Int, Int, [(Int, Int)])]
escPairs m esc =
    [ (r1, r2, bad)
    | (r2, e) <- zip [0 ..] esc, r1 <- bitsOf e, r1 < mNR m
    , let s1s = zip [0 ..] (F.toList (tSites (rules ! r1)))
          -- r2's sites by child (kind, instance), in order
          s2by = M.fromListWith (flip (++)) [ ((k2, n2), [(i2, s2)]) | (i2, s2) <- zip [0 ..] (F.toList (tSites (rules ! r2)))
                                                                     , let (k2, n2, _) = sPort s2 ]
          bad = [ (i1, i2) | (i1, s1) <- s1s, let (k1, n1, p) = sPort s1, k1 /= "tasks"
                           , (i2, s2) <- M.findWithDefault [] (k1, n1) s2by, let (_, _, q) = sPort s2
                           , let k = mK m M.! k1
                                 folP = (p == q || not (kdPrec k q p)) && not (kdConf k p q) && not (kdConf k q p)
                                 argP = p /= q || not (kdShare k p) || kdFx k p
                           , not (folP && argP) ] ]
  where rules = arr (mRules m)
