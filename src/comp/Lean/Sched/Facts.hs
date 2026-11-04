-- | The gate's checks on the model (mke2e.py's gate run): annotation, conflict_free assumptions,
-- last-caller ports, the scan, escape facts (solver, bsc or assert) and counts.
module Lean.Sched.Facts
    ( J(..), jsonDumps, GateFacts(..), gateFacts, wfeOf, cfaOf, encOrB, FactEngine(..), newFactEngine, FactShare, newFactShare
    ) where

import Control.Monad (foldM, forM)
import Control.Monad.State.Strict (runState)
import Data.Array (Array, listArray, (!))
import Data.Bits (popCount, testBit)
import Data.Char (ord)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (intercalate, sort)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Numeric (showHex)
import Prelude ((++), zip, otherwise, map, ($), fromIntegral, Eq((==), (/=)), Integral(div, mod), Monad(return,
    (>>), (>>=)), Functor(fmap), Num((+), negate, (-), (*)), Ord((<=), (>), (>=), (<)), Show(show),
    Applicative((<*>)), Foldable(length, elem, null, maximum, sum, foldl), Traversable(mapM), Bool(..), String,
    Int, Integer, Maybe(..), IO, Either(..), (.), (=<<), const, flip, id, (&&), not, (||), either, all, and,
    any, concat, concatMap, or, (<$>), maybe, fst, snd, break, drop, replicate, reverse, take, odd)
import qualified Data.Array as A ((!))
import qualified Data.List as L ((!!))
import qualified Data.Map.Strict as M (Map, (!), empty, findWithDefault, lookup, member, toList, insert,
    fromList, fromListWith)
import qualified Data.Set as S (Set, member, fromList, intersection, null, size, toAscList)
import qualified Data.Sequence as Sq (index)

import Lean.Sched.Bounds (Bounds(bnGateSelmux, bnGateEscFacts))
import Lean.Sched.BscFacts (BscLookup, bscCovers, bscUseCond, bscUseGuard)
import Lean.Sched.Enc (encAnd, encB, encOr, encSatA, mkAssume, newEnc, portAtom, Assume, Enc, EncM)
import Lean.Sched.Expr (SD(..), aexprE)
import Lean.Sched.Kinds (KindDef(kdHasArg, kdHasEn), PM(PT, PS))
import Lean.Sched.MECheck (cfNames)
import Lean.Sched.Export (exConfB)
import Lean.Sched.Model (exclSrc, users, DView(dvSBSBR, dvA, dvDisj, dvUrg, dvMe, dvCF, dvExport), MPort, MSite(sLabel,
    sArg, sKind, sCond), MStep(tFallback, tName, tSites), MX(MTrue), Model(mUnguarded, mByk, mNR, mNM, mRules,
    mNSlots, mK, mFacts, mMTasks, mGcls, mGen, mChecks, mCfChecks, mStepFallback), PyErr(..))
import Lean.Sched.Py (omEmpty, omInsert, omToList, startsWith)
import Lean.Sched.Reach (encFormulaShared)
import Lean.Sched.Scan (compressCodes, escPairs, mannExport, mannGate, mannPlain, scanMasks, MAnn(maRel), ScanDesign(scFpM,
    scFpR))
import Lean.Sched.Sites (Sites(..), SiteRec(..), RecKind(..))
import Lean.Sched.Tree (APkg(apInsts, apRules), Act(ACallA), E(..))
import Lean.Sched.Types (Design)
import Lean.Solver (Solver, Verdict(Unsat))

-- | A JSON value, printed as Python's json.dumps prints it.
data J = JS String | JI Integer | JNull | JB Bool | JL [J] | JO [(String, J)]

jsonDumps :: J -> String
jsonDumps v = case v of
    JS s -> jstr s
    JI n -> show n
    JNull -> "null"
    JB b -> if b then "true" else "false"
    JL xs -> "[" ++ intercalate ", " (map jsonDumps xs) ++ "]"
    JO kvs -> "{" ++ intercalate ", " [ jstr k ++ ": " ++ jsonDumps x | (k, x) <- kvs ] ++ "}"
  where
    jstr s = "\"" ++ concatMap esc s ++ "\""
    esc c = case c of
        '"' -> "\\\""
        '\\' -> "\\\\"
        '\n' -> "\\n"
        '\r' -> "\\r"
        '\t' -> "\\t"
        '\b' -> "\\b"
        '\f' -> "\\f"
        _ | ord c < 0x20 || ord c > 0x7e -> concatMap u4 (utf16 (ord c))
          | otherwise -> [c]
    u4 n = "\\u" ++ replicate (4 - length h) '0' ++ h where h = showHex n ""
    utf16 n | n < 0x10000 = [n]
            | otherwise = let m = n - 0x10000 in [0xD800 + m `div` 0x400, 0xDC00 + m `mod` 0x400]

data GateFacts = GateFacts
    { gfRecord :: [(String, J)]
    , gfLCP    :: [MPort]
    , gfEsc    :: [(Int, Int, Int, Int, Maybe String, Maybe String)]
    , gfScan   :: Either String ([Integer], [Integer])
    }

wfeOf :: SD -> String -> E
wfeOf dx n = let x = "WILL_FIRE_" ++ n in if M.member x (sdDefs dx) then EId x else EId ("CAN_FIRE_" ++ n)

-- | The conflict_free check lines as assumptions: (rule, the rules they name, the line resolved).
cfaOf :: APkg -> SD -> (String -> E) -> Model -> Either String [(String, S.Set String, E)]
cfaOf ap dx whas m = forM (mCfChecks m) $ \(rn, (_, c)) -> do
    e <- aexprE (apInsts ap) c
    return (rn, S.fromList (cfNames rn c), cfResolve' e)
  where cfResolve' = cfResolveWith ap whas

-- | cfResolve with the instrumentation wire's whas read as @whas rule@.
cfResolveWith :: APkg -> (String -> E) -> E -> E
cfResolveWith ap whas = res
  where
    writers = M.fromListWith (flip (++)) [ (i, [(rn, c, as)]) | (rn, (_, acts)) <- omToList (apRules ap), ACallA i "wset" c as <- acts ]
    true c = case c of { Nothing -> True; Just (ELit s) -> s `elem` ["1'd1", "1'b1"]; _ -> False }
    res e = case e of
        ECall i mt [] | mt `elem` ["wget", "whas"] -> case M.findWithDefault [] i writers of
            [(rn, c, a : _)] | true c -> if mt == "wget" then a else whas rn
            _ -> e
        ECall i mt as -> ECall i mt (map res as)
        EIf a b c -> EIf (res a) (res b) (res c)
        ECase x arms d -> ECase (res x) [ (res l, res v) | (l, v) <- arms ] (fmap res d)
        ESel a b -> ESel (res a) (res b)
        EApp h as -> EApp (res h) (map res as)
        EBin o a b -> EBin o (res a) (res b)
        EUn o a -> EUn o (res a)
        ETup xs -> ETup (map res xs)
        _ -> e

-- | Steps by index (an array, read with the list's operator).
newtype RA = RA (Array Int MStep)

(!!) :: RA -> Int -> MStep
(!!) (RA a) i = a A.! i

bitsOfI :: Integer -> [Int]
bitsOfI = go 0
  where go _ 0 = []
        go i y = (if odd y then (i :) else id) (go (i + 1) (y `div` 2))

encOrB :: [E] -> EncM Int
encOrB es = mapM encB es >>= encOr

-- | The conditions of a step's calls of one child method (ccl): true when none.
callCondsOf :: APkg -> String -> String -> String -> [E]
callCondsOf ap = \n inst meth -> case M.lookup (n, inst, meth) tbl of
    Nothing -> [ELit "1'd1"]
    Just cs -> cs
  where tbl = M.fromListWith (flip (++)) [ ((n, i, mt), [fromMaybe (ELit "1'd1") c]) | (n, (_, acts)) <- omToList (apRules ap), ACallA i mt c _ <- acts ]

-- | The solver checks a gate run and a file share: last-caller ports, escape
-- facts and caller exclusivity, over one circuit (@mconf@: two methods never both called).
data FactEngine = FactEngine
    { feLCP :: IO [(MPort, String, Bool)]
    , feEscape :: [(Int, Int, [(Int, Int)])] -> IO [(Int, Int, Int, Int, Maybe String, Maybe String)]
    , feCx :: MPort -> (Int, Int) -> (Int, Int) -> IO (Maybe String, Maybe String)
    , feCfa :: [(String, S.Set String, E)] }

-- | What the solver checks on one model share across runs (the gate's and the file's):
-- the circuit, the encoded conflict_free assumptions and the verdicts.
data FactShare = FactShare { fsEnc :: IORef Enc, fsAsm :: IORef (Maybe Assume), fsMemo :: IORef (M.Map ([Int], [String]) Verdict) }

newFactShare :: Solver -> Sites -> IO FactShare
newFactShare sv sites = FactShare <$> (newEnc sv (sxDX sites) >>= newIORef) <*> newIORef Nothing <*> newIORef M.empty

newFactEngine :: Solver -> FactShare -> DView -> Sites -> BscLookup -> Model -> (Int -> Int -> Bool) -> IO FactEngine
newFactEngine sv share dv sites bl m mconf = do
    let qref = fsEnc share
        aref = fsAsm share
    let ap = dvA dv
        callConds _ = cconds
        cconds = callCondsOf ap
        dx = sxDX sites
        nR = mNR m
        wfe = wfeOf dx
        rulesA = listArray (0, length (mRules m) - 1) (mRules m)
        rules = RA rulesA
        stepName k = tName (rules !! k)
        cfa = either (const []) id (cfaOf ap dx wfe m)
        u0 = users m
        csl = M.fromList [ ((srStep e, srLabel e), e) | e <- sxRecs sites ]
        rmExcl = S.fromList (concat [ [(a, b), (b, a)] | (a, b) <- dvDisj dv ++ dvUrg dv ++ dvMe dv ])
        exclLc x y
            | (x >= nR) /= (y >= nR) = S.member (stepName x, stepName y) rmExcl
            | x >= nR = mconf (x - nR) (y - nR)
            | otherwise = isJust (exclSrc m x y)
        -- one circuit for every query: gates and atoms shared, so bsc's solver is given each once
        query roots = queryCf roots []
        -- with the conflict_free lines naming one of @names@ as assumptions, each encoded once
        queryCf roots names = do
            e0 <- readIORef qref
            ma <- readIORef aref
            let ((rs, asm), e1) = runState (do { a <- maybe (mkAssume =<< mapM cfLit cfa) return ma; r <- roots; return (r, a) }) e0
                nset = S.fromList names
                sel i = not (S.null (S.intersection (cfNm A.! i) nset))
            writeIORef aref (Just asm)
            memo <- readIORef (fsMemo share)
            let key = (rs, S.toAscList nset)
            case M.lookup key memo of
                Just v -> writeIORef qref e1 >> return v
                Nothing -> do
                    (v, e2) <- encSatA sv rs asm sel e1
                    writeIORef qref e2
                    modifyIORef' (fsMemo share) (M.insert key v)
                    return v
        cfLit (rn, _, ex) = do { a <- encB (wfe rn); b <- encB ex; x <- encAnd [a, b]; return (negate x) }
        cfNm = listArray (0, length cfa - 1) [ nm | (_, nm, _) <- cfa ] :: Array Int (S.Set String)
        -- last caller: self-SBR primitive ports two callers may drive in one cycle
        lcpF = fmap concat $ forM (M.toList u0) $ \(p@(kd, i, pm), us) -> do
            let ks = S.toAscList (S.fromList (map fst us))
                meth = case pm of { PS s -> s; PT t -> show t }
                inst = (mByk m M.! kd) L.!! i
                args = S.fromList [ sArg (Sq.index (tSites (rules !! k)) si) | (k, si) <- us ]   -- (MX equality is repr equality)
            if not ((kd, meth) `elem` [("reg", "write"), ("cfg", "write")]) || length us < 2 || S.size args < 2 then return [] else do
                sh <- foldM (\shadow (x, y) -> if shadow || exclLc x y then return shadow else do
                        v <- queryCf (do
                            a <- encB (wfe (stepName x))
                            b <- encB (wfe (stepName y))
                            c <- encOrB (callConds ap (stepName x) inst meth)
                            c2 <- encOrB (callConds ap (stepName y) inst meth)
                            return [a, b, c, c2]) [stepName x, stepName y]
                        return (v /= Unsat)) False [ (x, y) | (ai, x) <- zip [0 :: Int ..] ks, y <- drop (ai + 1) ks ]
                return [ (p, inst ++ "." ++ meth, all (< nR) ks) | sh ]
        isUncond s = sCond s == MTrue
        siteLit k s = case sKind s of
            "read" | isUncond s -> return 1
                   | otherwise -> case M.lookup (stepName k, fromMaybe "" (sLabel s)) csl of
                        Just e -> case srKind e of
                            RRdy { rRdy = p } -> portAtom p >>= \v -> encOr [v]
                            RReach { rCond = f } -> encFormulaShared (sxRS sites) f
                        Nothing -> return 1
            "task" -> return 1
            _ -> let lab = fromMaybe "" (sLabel s)
                     (inst, meth0) = let r = reverse lab; (b, a) = break (== '.') r in (reverse (drop 1 a), reverse b)
                     meth = if startsWith meth0 "sub" then "sub" else meth0
                 in encOrB (callConds ap (stepName k) inst meth)
        parseA c = aexprE (apInsts ap) c
        bscFact (k1, s1, lit1) (k2, s2, lit2)
            | sKind s1 == "task" || sKind s2 == "task" = return Nothing
            | otherwise = do
                let n1 = stepName k1
                    n2 = stepName k2
                text <- bscCovers bl n1 (fromMaybe "" (sLabel s1)) n2 (fromMaybe "" (sLabel s2))
                case text of
                    Nothing -> return Nothing
                    Just t -> do
                        oks <- forM [(n1, s1, lit1), (n2, s2, lit2)] $ \(n, s, litf) ->
                            case (mapM parseA (bscUseCond bl n (fromMaybe "" (sLabel s))), maybe (Right (ELit "1'd1")) parseA (bscUseGuard bl n)) of
                                (Right cs, Right g) | not (null cs) -> do
                                    v <- query (do
                                        w <- encB (wfe n)
                                        l <- litf
                                        gb <- encB g
                                        co <- encOrB cs
                                        x <- encAnd [gb, co]
                                        return [w, l, negate x])
                                    return (v == Unsat)
                                _ -> return False
                        return (if and oks then Just t else Nothing)
        escapeFacts escl = fmap concat $ forM escl $ \(r1, r2, bad) -> forM bad $ \(i1, i2) -> do
            let s1 = Sq.index (tSites (rules !! r1)) i1
                s2 = Sq.index (tSites (rules !! r2)) i2
                n1 = stepName r1
                n2 = stepName r2
                base = do { a <- encB (wfe n1); b <- encB (wfe n2); x <- siteLit r1 s1; y <- siteLit r2 s2; return [a, b, x, y] }
            v <- query base
            (how, bt) <- if v == Unsat then return (Just "solver", Nothing) else do
                bt <- bscFact (r1, s1, siteLit r1 s1) (r2, s2, siteLit r2 s2)
                case bt of
                    Just t -> return (Just "bsc", Just t)
                    Nothing | not (null cfa) -> do
                        v2 <- queryCf base [n1, n2]
                        return (if v2 == Unsat then Just "assert" else Nothing, Nothing)
                            | otherwise -> return (Nothing, Nothing)
            return (r1, r2, i1, i2, how, bt)
        -- caller exclusivity at a mux: the two users' firing and call conditions (mke2e's CFACTS)
        cxFact (kd, i, pm) (x, sx) (y, sy) = do
            let inst = (mByk m M.! kd) L.!! i
                meth0 = case pm of { PS s -> s; PT t -> show t }
                meth = if kd == "rf" && meth0 /= "upd" then "sub" else meth0
                cc k = encOrB (callConds ap (stepName k) inst meth)
                base = do { a <- encB (wfe (stepName x)); b <- encB (wfe (stepName y)); c <- cc x; c2 <- cc y; return [a, b, c, c2] }
            v <- query base
            if v == Unsat then return (Just "solver", Nothing) else do
                bt <- bscFact (x, Sq.index (tSites (rules !! x)) sx, cc x) (y, Sq.index (tSites (rules !! y)) sy, cc y)
                case bt of
                    Just t -> return (Just "bsc", Just t)
                    -- with the conflict_free checks naming either step assumed (a file states it under asmP)
                    Nothing | not (null cfa) -> do
                        v2 <- queryCf base [stepName x, stepName y]
                        return (if v2 == Unsat then Just "assert" else Nothing, Nothing)
                            | otherwise -> return (Nothing, Nothing)
    return (FactEngine lcpF escapeFacts cxFact cfa)

-- | The gate's facts and counts for a model (or the model's failure).
gateFacts :: Solver -> FactShare -> Design -> DView -> Sites -> BscLookup -> Bounds -> String -> Either PyErr Model -> IO GateFacts
gateFacts _ _ _ _ _ _ _ name (Left err) =
    return (GateFacts [("name", JS name), ("error", JS ("model: " ++ take 200 (pyStrErrTrunc err)))] [] [] (Left ""))
  where pyStrErrTrunc (PyErr t msg) = t ++ ": " ++ take 200 msg
gateFacts sv share d dv sites bl bnd name (Right m) = do
    let nR = mNR m
        nM = mNM m
        met = nM > 0
        mnames = [ tName r | r <- drop nR (mRules m) ]
        plain = mannPlain mnames (dvCF dv)
        nonCF = met && any (\k -> popCount (maRel plain ! k) < nM - 1) [0 .. nM - 1]
        mann = maybe (mannGate mnames (dvCF dv) (dvSBSBR dv)) mannExport (dvExport dv)
        mconf a b = maybe (a == b || not (testBit (maRel mann ! b) a)) (\ex -> exConfB ex a b) (dvExport dv)
        rulesA = listArray (0, length (mRules m) - 1) (mRules m)
        rules = RA rulesA
        stepName k = tName (rules !! k)
        u0 = users m
        isUncond s = sCond s == MTrue
    fe <- newFactEngine sv share dv sites bl m mconf
    lcp <- feLCP fe
    let lcPorts = [ p | (p, _, _) <- lcp ]
        lcMeth = [ l | (_, l, False) <- lcp ]
        lcSlots = [ (mNSlots m + t, S.toAscList (S.fromList (map fst (u0 M.! p)))) | (t, p) <- zip [0 ..] lcPorts ]
        nslots = mNSlots m + length lcPorts
        scanR = scanMasks m lcSlots (if met then Just mann else Nothing)
        escapeFacts = feEscape fe
    (scanJ, codesJ, segJ, escapesJ, efs, fpp, nef, meNeed) <- case scanR of
        Left err -> return (JS err, JNull, JNull, JNull, Nothing, 0, 0, 0)
        Right (need, esc, dn) -> do
            let escl = escPairs m esc
                fpp' = maximum (0 : [ length b | (_, _, b) <- escl ])
                nef' = sum [ length b | (_, _, b) <- escl ]
            efs' <- if fpp' <= bnGateEscFacts bnd then Just <$> escapeFacts escl else return Nothing
            let meN = length [ () | (i, nd) <- zip [0 ..] need, x <- bitsOfI nd, x < nR, exclSrc m x i == Just "me" ]
                seg = maximum (0 : map length (scFpR dn) ++ map length (scFpM dn))
            return (JS "ok", JI (fromIntegral (compressCodes dn)), JI (fromIntegral seg), JI (fromIntegral (sum (map popCount esc))), efs', fpp', nef', meN)
    -- the pairwise bound and the caller-exclusivity upper bound
    let pwc = [ (length ks, cx)
              | (p@(kd, _, pm), us) <- M.toList u0, kd /= "tasks", kdHasEn (mK m M.! kd) pm
              , let ks = S.toAscList (S.fromList (map fst us)), length ks >= 2
              , let cond = any (\(k, si) -> not (isUncond (Sq.index (tSites (rules !! k)) si))) us
                    noex = kdHasArg (mK m M.! kd) pm && or [ isNothing (exclSrc m a b) | (ai, a) <- zip [0 :: Int ..] ks, b <- drop (ai + 1) ks, a < nR, b < nR ]
                    n = length ks
                    cx = if n <= bnGateSelmux bnd then length [ () | (ai, a) <- zip [0 :: Int ..] ks, b <- drop (ai + 1) ks, isNothing (exclSrc m a b) ]
                         else n * (n - 1) `div` 2
              , cond || met || noex
              , let _ = p ]
        pw = maximum (0 : map fst pwc)
        cxub = sum (map snd pwc)
        lab (r1, i1, r2, i2) = stepName r1 ++ "/" ++ maybe "None" id (sLabel (Sq.index (tSites (rules !! r1)) i1)) ++ " "
                               ++ stepName r2 ++ "/" ++ maybe "None" id (sLabel (Sq.index (tSites (rules !! r2)) i2))
        howCount h = JI (fromIntegral (length [ () | Just fs <- [efs], (_, _, _, _, h', _) <- fs, h' == h ]))
        record =
            [ ("name", JS name), ("scan", scanJ), ("slots", JI (fromIntegral nslots)), ("codes", codesJ), ("seg_uses", segJ)
            , ("mod_facts", JI (fromIntegral (nef + length (mFacts m) + cxub))), ("escapes", escapesJ)
            , ("esc_facts", maybe JNull (JI . fromIntegral . length) efs), ("esc_facts_max_per_pair", JI (fromIntegral fpp))
            , ("esc_facts_how", maybe JNull (const (JO [ ("solver", howCount (Just "solver")), ("bsc", howCount (Just "bsc"))
                                                       , ("assert", howCount (Just "assert")), ("null", howCount Nothing) ])) efs)
            , ("esc_facts_unconfirmed", maybe JNull (\fs -> JL (take 8 [ JS (lab (r1, i1, r2, i2)) | (r1, r2, i1, i2, Nothing, _) <- fs ])) efs)
            , ("esc_facts_err", JNull)
            , ("esc_facts_fallback", maybe JNull (\fs -> JI (fromIntegral (length [ () | (r1, r2, _, _, _, _) <- fs
                                                                                      , tFallback (rules !! r1) || tFallback (rules !! r2) ]))) efs)
            , ("lc", JL [ JL [JS l, JI (fromIntegral (S.size (S.fromList (map fst (u0 M.! p)))))] | (p, l, _) <- lcp ])
            , ("lc_method", JL (map JS lcMeth)), ("noncf_methods", JB nonCF)
            , ("mtasks", JL [ JL [JS a, JI (fromIntegral b)] | (a, b) <- mMTasks m ])
            , ("generic", JL (map JS (sort [ t | ((t, _), _) <- omToList (mGen m) ])))
            , ("generic_class", JO (omToList (foldl (\om ((t, _), g) -> omInsert t (JS (mGcls m M.! g)) om) omEmpty (omToList (mGen m)))))
            , ("me_checks", JI (fromIntegral (length (mChecks m)))), ("cf_checks", JI (fromIntegral (length (mCfChecks m))))
            , ("me_needed_pairs", JI (fromIntegral meNeed)), ("pairwise", JI (fromIntegral pw))
            , ("step_fallback", JL (map JS (mStepFallback m))), ("unguarded", JL (map JS (mUnguarded m))) ]
    return (GateFacts record lcPorts (fromMaybe [] efs) (either Left (\(a, b, _) -> Right (a, b)) scanR))
