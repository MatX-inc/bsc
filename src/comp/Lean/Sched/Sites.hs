-- | Site conditions (sitecond.analyse, condsites.conditional_sites): which value
-- reads of a step need a condition, and the condition the file states.
--
-- Action calls keep their exact conditions.  A value read is unconditional
-- unless (a) the static footprint scan fails for it, (b) it reads a shared copy
-- that steps which may fire together read with different arguments, (c) it is
-- the earlier read of a value/value pair that may not follow, or (r) its port has
-- a ready signal the step's guard does not imply.  A conditional read's condition
-- is the exact hardware one where the step can compute it, else its reach
-- condition with literals over later sites, hardware wires and inputs dropped; a
-- readiness read states its ready literal, checked in site-value form.
module Lean.Sched.Sites
    ( Sites(..), SiteRec(..), RecKind(..), SStep(..), SSite(..), RdySV(..), analyseSites, showSites, annOf
    ) where

import ASchedule (MethodId(..))
import AScheduleInfo (AScheduleInfo(..))
import ASyntax (AExpr, ADef(..))
import Control.Monad (forM_, unless, foldM, forM)
import Control.Monad.State.Strict (runState)
import qualified Data.Array as A (listArray, (!))
import Data.Array.Unboxed (UArray, (!), accumArray)
import Data.Word (Word8)
import Data.List (intercalate, sort, sortOn)
import Data.Maybe (fromMaybe)
import Id (getIdString, getIdBaseString)
import qualified Data.IntMap.Strict as IM ((!), findWithDefault, fromList, fromListWith, insert, map, toList)
import qualified Data.IntSet as IS (member, empty, insert, delete, fromList, null, singleton, size, toList, union)
import qualified Data.Map.Strict as M (Map, (!), elems, empty, findWithDefault, keys, keysSet, lookup, member,
    size, toList, filter, insert, fromList, fromListWith, fromDistinctAscList, lookupIndex, map, unionWith)
import qualified Data.Map.Lazy as ML ((!), map)
import qualified Data.Set as S (Set, member, delete, difference, empty, filter, fromList, insert, intersection,
    minView, null, singleton, size, split, toAscList, toList, union, unions)

import Lean.Sched.Enc (encAnd, encAtom, encB, encOr, encSat, encSetOn, encSetSub, newEnc, portAtom, AKey(AKey),
    KP(KS))
import Lean.Sched.Expr (aexprE, canonExpr, defReads, exactSD, exprCallsMany, exprHw, exprReads, exprSources, fsF, fsLit, fsLits, fsNode,
    fsSubst, fsSummary, fsSummaryFrom, fsT, fsText, mkSD, LitKey, Node(NOr, NLit, NAnd), Port2, SD(sdDefs))
import Lean.Sched.MECheck (cfNames, cfResolve, meParse, samePairs, suffixGroups)
import Lean.Sched.Py (omLookup)
import Lean.Sched.Reach (encFormula, liftFS, litExprs, litKey, newRS, reachTop, RC(rcSeq, RC), RS(rsFS, rsLit))
import Lean.Sched.Tree (APkg(apInsts, apGuarded, apRules, apIfcVals, apMethArgs), Act(ACallA, ATask), E(EId,
    ELit))
import Lean.Sched.Types (portLabel, Call(clPort), Child(chMethods, chStmts, chOrder), Design(dUserME, dUseDefs,
    dChildren, dDisjoint, dBlocks, dChildOrder, dSteps), Meth(mtCopies, mtKind, mtConst), MethKind(ValueMethod),
    Port(ptCopy, ptInst, ptMeth), Step(stName, stDescr, stReads, stCalls, stChecks))
import Lean.Solver (solverQueries, Solver, Verdict(Unsat))

-- | A step as the site analysis sees it: name, kind (rule, method, value), method.
data SStep = SStep { ssName :: String, ssKind :: String, ssMeth :: Maybe String, ssIdx :: Int, ssSites :: [SSite], ssStep :: Step }

-- | A use of a child port by a step: a read or a call, in the dump's order.
data SSite = SSite { siRead :: Bool, siLabel :: String, siInst :: String, siMeth :: String, siCopy :: Maybe Integer, siPos :: Int
                   , siMIx :: Int }   -- ^ siMeth's number among its child's methods

type SK = (Int, Int)        -- (step, site)

-- | The readiness facts of a site in site-value form.
data RdySV = RdySV { svF :: Int, svY :: [Port2], svOthers :: [Port2], svRdy :: Bool, svInv :: Maybe Bool
                   , svOk :: Bool, svLit :: Maybe Port2, svHw :: Int }

data RecKind
    = RReach { rTrue :: Bool, rFan :: [Port2], rPartners :: [(String, String, String)], rCond :: Int }
    | RRdy { rRdy :: Port2, rSV :: RdySV, rRTrue :: Bool, rRFan :: [Port2] }

-- | condsites' record of a conditional site.
data SiteRec = SiteRec { srStep :: String, srLabel :: String, srNeed :: String, srInst :: String, srMeth :: String
                       , srCopy :: Maybe Integer, srKind :: RecKind }

data Sites = Sites
    { sxRecs   :: [SiteRec]
    , sxRS     :: RS                -- the conservative reach formulas
    , sxRSb    :: RS                -- the readiness (sequenced) reach formulas
    , sxSD     :: SD
    , sxDX     :: SD                -- every port its own atom
    , sxChecks :: [(String, Either String (Maybe ([String], [String])), AExpr)]
    , sxQueries :: Int
    , sxRecheck :: String -> String -> S.Set Port2 -> RS -> IO (Maybe (RdySV, RS))
      -- ^ rdy_site_check of a readiness site with the literals over some reads dropped
      -- (the model drops later-site literals); its formula lives in the returned RS
    }

-- ---------------------------------------------------------------- the analysis

analyseSites :: Solver -> Design -> APkg -> AScheduleInfo -> IO (Either String Sites)
analyseSites sv d ap asi = do
    q0 <- solverQueries sv
    let insts = apInsts ap
        pe e = either (const Nothing) Just (aexprE insts e)
        useConds = [ (getIdString (adef_objid u), e) | u <- dUseDefs d, Just e <- [pe (adef_expr u)] ]
        sd = mkSD ap d useConds
        dx = exactSD sd
        kids = dChildren d
        steps = [ mkStep i st | (i, st) <- zip [0 ..] (dSteps d) ]
        stepArr = M.fromList [ (ssIdx s, s) | s <- steps ]
        stepby = M.fromList [ (ssName s, s) | s <- steps ]     -- the last of a name, as a dict
        mkStep i st =
            let (kind, meth) = case words (filter (/= ']') (stDescr st)) of
                    ("value" : "method" : m : _) -> ("value", Just m)
                    ("value" : "method" : _) -> ("value", Just "")
                    ("method" : m : _) -> ("method", Just m)
                    ("method" : _) -> ("method", Just "")
                    _ -> ("rule", Nothing)
                ss = [ (True, portLabel p, p) | (p, _) <- stReads st, M.member (ptInst p) kids ]
                     ++ [ (False, portLabel (clPort c), clPort c) | c <- stCalls st, M.member (ptInst (clPort c)) kids ]
            in SStep (stName st) kind meth i
                     [ SSite r l (ptInst p) (ptMeth p) (ptCopy p) k (mIx p)
                     | (k, (r, l, p)) <- zip [0 ..] ss ] st
        kidIx = M.map (\c -> M.fromDistinctAscList (zip (M.keys (chMethods c)) [0 ..])) kids
        mIx p = M.findWithDefault (error ("schedlean: a site on " ++ portLabel p ++ ", which its child does not list")) (ptMeth p) (kidIx M.! ptInst p)
        -- the child's relation by method number, the last statement naming a pair winning (annOf);
        -- bsc's default C for unlisted pairs, two copies of one method CF
        relOf = M.map (\c -> let ms = chMethods c; nm = M.size ms
                             in (nm, accumArray (\_ new -> new) 0 (0, nm * nm - 1)
                                       [ (a * nm + b, relCode r) | (ps, r, qs) <- chStmts c, Just a <- map (`M.lookupIndex` ms) ps
                                                                 , Just b <- map (`M.lookupIndex` ms) qs ] :: UArray Int Word8)) kids
        relCode r = case r of { "CF" -> 1; "SB" -> 2; "SBR" -> 3; "C" -> 4; _ -> 5 } :: Word8
        crel k (p0, p1) (q0', q1)
            | p0 == q0' && p1 /= q1 = 1
            | r /= 0 = r
            | rt ! (q0' * nm + p0) /= 0 = 6
            | otherwise = 4
          where (nm, rt) = relOf M.! k
                r = rt ! (p0 * nm + q0')
        prec k p q = p /= q && crel k p q `elem` [2, 3]
        conf k p q = crel k p q == 4 || crel k q p == 4
        follow k a b = not (prec k b a) && not (conf k a b)
        use s = (siMIx s, maybe "" (\c -> "[" ++ show c ++ "]") (siCopy s))
        exc = S.fromList (concat [ [(a, b), (b, a)] | (a, b) <- dDisjoint d ++ dUserME d ++ S.toList (dBlocks d) ])
        rules = [ s | s <- steps, ssKind s == "rule" ]
        ruleArr = A.listArray (0, length rules - 1) rules
        -- (a)/(c): the footprint scan per use, in the prototype's loop order
        scan = sortOn fst
            [ ((x, y, i1, i2), ev)
            | let byInst = M.fromListWith (flip (++)) [ (siInst s, [(x, s)]) | (x, r) <- zip [0 :: Int ..] rules, s <- ssSites r ]
            , us <- M.elems byInst
            , (x, s1) <- us, (y, s2) <- us, x < y, siRead s1 || siRead s2
            , let r1 = ruleArr A.! x, let r2 = ruleArr A.! y
            , not (S.member (ssName r1, ssName r2) exc)
            , not (follow (siInst s1) (use s1) (use s2))
            , let i1 = siPos s1, let i2 = siPos s2
            , Just ev <- [scanEv r1 s1 r2 s2] ]
        scanEv r1 s1 r2 s2
            | siRead s2 && not (siRead s1) = Just ((ssIdx r2, siPos s2), 'a', (ssIdx r1, siPos s1))
            | siRead s1 && not (siRead s2) = Just ((ssIdx r1, siPos s1), 'a', (ssIdx r2, siPos s2))
            | siRead s1 && siRead s2 = Just ((ssIdx r1, siPos s1), 'c', (ssIdx r2, siPos s2))
            | otherwise = Nothing
        -- the needs of the scan without listing its pairs: a use of a class meets one of
        -- another class in an earlier (later) rule not exclusive with its own
        ruleNo = M.fromListWith (++) [ (ssName r, [x]) | (x, r) <- zip [0 :: Int ..] rules ]
        excAdj = M.fromListWith S.union [ (x, S.fromList ys) | (a, b) <- S.toList exc, Just xs <- [M.lookup a ruleNo]
                                                           , Just ys <- [M.lookup b ruleNo], x <- xs ]
        classes = M.fromListWith S.union [ ((siInst s, use s, siRead s), S.singleton x) | (x, r) <- zip [0 :: Int ..] rules, s <- ssSites r ]
        byInstC = M.fromListWith (++) [ (k, [(u, rd, xs)]) | ((k, u, rd), xs) <- M.toList classes ]
        before xs y = S.size (fst (S.split y xs)) > S.size (S.filter (< y) (S.intersection xs (M.findWithDefault S.empty y excAdj)))
        after xs x = S.size (snd (S.split x xs)) > S.size (S.filter (> x) (S.intersection xs (M.findWithDefault S.empty x excAdj)))
        need0 = M.fromListWith (flip S.union)
            [ ((ssIdx r, siPos s), S.singleton c)
            | (x, r) <- zip [0 :: Int ..] rules, s <- ssSites r, siRead s
            , let k = siInst s, let u = use s
            , c <- "ac"
            , or [ hit | (u2, rd2, xs) <- M.findWithDefault [] k byInstC
                       , hit <- if c == 'a'
                                then [ not rd2 && not (follow k u2 u) && before xs x
                                     , not rd2 && not (follow k u u2) && after xs x ]
                                else [ rd2 && not (follow k u u2) && after xs x ] ] ]
        partners = M.fromListWith (flip (++)) [ (k, [p]) | (_, (k, _, p)) <- scan ]
        -- (b): shared value copies read with different arguments by steps that may fire together
        users = M.fromList [ (methKey mid, [ (u, map getIdString (r1 ++ r2) ++ map getIdString r3) | (u, (r1, r2, r3)) <- us ])
                           | (mid, us) <- M.toList (asi_method_uses_map asi) ]
        ralloc = M.fromList [ (methKey mid, M.toList t) | (mid, t) <- M.toList (asi_resource_alloc_table asi) ]
        methKey (MethodId o m) = (getIdString o, getIdBaseString m)
        shared = concat
            [ marks
            | k <- dChildOrder d, let c = kids M.! k, m <- chOrder c, let mt = chMethods c M.! m
            , mtKind mt == ValueMethod, length (mtCopies mt) >= 2
            , let al = M.findWithDefault [] (k, m) ralloc
            , let umap = M.fromList (M.findWithDefault [] (k, m) users)
            , let byport = M.fromListWith (flip (++)) [ (pn, [src]) | (src, pn) <- al ]
            , (pn, srcs0) <- M.toList byport
            , let srcs = S.toAscList (S.fromList srcs0), length srcs >= 2
            , let us = [ (src, r) | src <- srcs, r <- M.findWithDefault [] src umap ]
            , let clash = [ (r1, r2) | (s1, r1) <- us, (s2, r2) <- us, s1 < s2, r1 /= r2, not (S.member (r1, r2) exc) ]
            , not (null clash)
            , let involved = S.fromList (concat [ [a, b] | (a, b) <- clash ])
            , let order = sort [ (ssIdx st, r) | r <- S.toList involved, Just st <- [M.lookup r stepby] ]
            , let top = case order of { ((_, r) : _) -> Just r; [] -> Nothing }
            , let marks = [ ((ssIdx st, siPos s), Just r == top)
                          | r <- S.toList involved, Just st <- [M.lookup r stepby], s <- ssSites st
                          , siRead s, siInst s == k, siMeth s == m, siCopy s == Just (pn - 1) ] ]
        exactHw = M.fromList shared
        need1 = M.unionWith S.union need0 (M.fromList [ (k, S.singleton 'b') | (k, _) <- shared ])
        -- (r): readiness of value reads whose port has a ready signal
        rdyPort k m
            | m == "first" && (M.member "i_notEmpty" ms || M.member "notEmpty" ms) =
                if omLookup k (apGuarded ap) == Just False then Nothing
                else Just [ (k, x) | x <- ["i_notEmpty", "notEmpty"], M.member x ms ]
            | Just r <- M.lookup ("RDY_" ++ m) ms, not (mtConst r && not (S.member (k, "RDY_" ++ m) readRdy)) = Just [(k, "RDY_" ++ m)]
            | otherwise = Nothing
          where ms = chMethods (kids M.! k)
        -- a constant RDY no step reads: bsc's wrapper of a synthesized child assumes it is 1 (bsc.hs true_ifc_ids)
        readRdy = S.fromList [ (siInst s, siMeth s) | st <- steps, s <- ssSites st, siRead s ]
        guardExpr st = case omLookup (ssName st) (apRules ap) of
            Just (p, _) -> p
            Nothing | M.member ("CAN_FIRE_" ++ ssName st) (sdDefs sd) -> EId ("CAN_FIRE_" ++ ssName st)
                    | otherwise -> ELit "1'd1"
        stepItems st =
            let (it0, acts) = case omLookup (ssName st) (apRules ap) of
                    Just (p, as) -> ([(p, Nothing)], as)
                    Nothing | ssKind st == "value", M.member ("CAN_FIRE_" ++ ssName st) (sdDefs sd) ->
                                ([(EId ("CAN_FIRE_" ++ ssName st), Nothing)], [])
                            | otherwise -> ([], [])
                it1 = [ (v, Nothing) | ssKind st `elem` ["value", "method"], Just m <- [ssMeth st], Just v <- [omLookup m (apIfcVals ap)] ]
                it2 = [ (v, Nothing) | ssKind st == "value", Just v <- [omLookup (ssName st) (apIfcVals ap)] ]
                it3 = concat [ maybe [] (\c -> [(c, Nothing)]) cond ++ [ (x, cond) | x <- args ]
                             | a <- acts, Just (cond, args) <- [actCA a] ]
            in (it0 ++ it1 ++ it2 ++ it3, acts)
        actCA a = case a of { ACallA _ _ c as -> Just (c, as); ATask _ c as -> Just (c, as); _ -> Nothing }
    rdyRes <- forM [ (st, s, rp) | st <- steps, s <- ssSites st, siRead s, Just rp <- [rdyPort (siInst s) (siMeth s)] ] $
        \(st, s, rp) -> do
            e0 <- newEnc sv sd
            let ((gl, rl), e1) = runState (do { g <- encB (guardExpr st); r <- mapM portAtom rp >>= encOr; return (g, r) }) e0
            (v, _) <- encSat sv [gl, negate rl] e1
            return [ ((ssIdx st, siPos s), rp) | v /= Unsat ]
    let rdyOf = M.fromList (concat rdyRes)
        need = M.unionWith S.union need1 (M.map (const (S.singleton 'r')) rdyOf)
        needOf k = M.findWithDefault S.empty k need
        siteAt (i, j) = ssSites (stepArr M.! i) !! j
        isNeedRead st s = siRead s && not (S.null (needOf (ssIdx st, siPos s)))
        -- reach conditions for the needed sites
        rc = RC sd (S.fromList [ (siInst s, siMeth s) | st <- steps, s <- ssSites st, isNeedRead st s ]) False
        topOf rcx st = do
            let (items, _) = stepItems st
            its <- forM items $ \(e, c) -> case c of
                Nothing -> return (e, fsT)
                Just ce -> do { k <- litKey "c" [ce]; l <- liftFS (fsLit k True); return (e, l) }
            reachTop rcx its
        (reach, rs1) = flip runState newRS $ fmap M.fromList $ fmap concat $
            forM [ st | st <- steps, any (isNeedRead st) (ssSites st) ] $ \st -> do
                top <- topOf rc st
                return [ ((ssIdx st, siPos s), M.findWithDefault fsF (siInst s, siMeth s) top) | s <- ssSites st, isNeedRead st s ]
        litInfo l = (S.unions (map (exprReads sd) (litExprs l)), S.unions (map (exprHw sd) (litExprs l)))
        liA = ML.map litInfo (rsLit rs1)          -- each literal's reads and wires, once
        linfoA _ _ key = liA ML.! key
    -- the site order and the literals dropped (order_pass)
    case orderPass sd ap stepItems prec use linfoA rc rs1 steps (\st s -> isNeedRead st s) reach of
      Left err -> return (Left err)
      Right (reachD, rs2) -> do
        let condUsed k = if M.findWithDefault False k exactHw then fsT else reachD M.! k
            rsites = [ (st, s) | st <- steps, s <- ssSites st, isNeedRead st s, S.member 'r' (needOf (ssIdx st, siPos s)) ]
            rcb = RC sd (S.fromList [ (siInst s, siMeth s) | (_, s) <- rsites ]) True
            (reachB, rsb) = flip runState newRS $ fmap M.fromList $ forM rsites $ \(st, s) -> do
                top <- topOf rcb st
                return ((ssIdx st, siPos s), M.findWithDefault fsF (siInst s, siMeth s) top)
            liB = ML.map litInfo (rsLit rsb)
            linfo rcx rsx key = if rcSeq rcx then liB ML.! key else linfoA rcx rsx key
            rdyLit st x = let rd = S.fromList [ (siInst y, siMeth y) | y <- ssSites st, siRead y ]
                          in case [ p | p <- rdyOf M.! (ssIdx st, siPos x), S.member p rd ] of { (p : _) -> Just p; [] -> Nothing }
            condReads st = M.fromList (concat
                [ if S.member 'r' nd then [((siInst x, siMeth x), Left (rdyLit st x))]
                  else [ ((siInst x, siMeth x), Right cu) | let cu = condUsed (ssIdx st, siPos x), cu /= fsT ]
                | x <- ssSites st, isNeedRead st x, let nd = needOf (ssIdx st, siPos x) ])
            fReads rcx rsx f = S.unions [ fst (linfo rcx rsx k) | (k, _) <- S.toList (fsLits (rsFS rsx) f) ]
            -- the literals' reads and wires per formula node, once per store
            infoSum rcx rsx = fsSummary (\k _ -> linfo rcx rsx k) (\xs -> (S.unions (map fst xs), S.unions (map snd xs))) (S.empty, S.empty) (rsFS rsx)
            sumA = infoSum rc rs2
        -- readiness in site-value form (rdy_site_check): R_seq as the file states it, literals over
        -- a wire, an input or the reads in @drop@ replaced by true (monotone weakening)
        let rdyFormula drop k rsx =
                let f0 = reachB M.! k
                    hwk = S.fromList [ key | (key, _) <- S.toList (fsLits (rsFS rsx) f0), not (S.null (snd (linfo rcb rsx key))) ]
                    (f1, fs1) = if S.null hwk then (f0, rsFS rsx) else fsSubst hwk f0 (rsFS rsx)
                    dk = S.fromList [ key | (key, _) <- S.toList (fsLits fs1 f1), not (S.null (S.intersection (fst (linfo rcb rsx key)) drop)) ]
                    (f2, fs2) = if S.null drop || S.null dk then (f1, fs1) else fsSubst dk f1 fs1
                in (f2, S.size hwk, rsx { rsFS = fs2 })
            check drop (acc, rsx) (st, s) = do
                let key = (ssIdx st, siPos s)
                    (f, nhw, rsx') = rdyFormula drop key rsx
                    crs = condReads st
                    ys = S.toAscList (S.intersection (fReads rcb rsx' f) (M.keysSet crs))
                    lit = rdyLit st s
                    others = [ y | y <- ys, y /= (siInst s, siMeth s) ]
                e0 <- newEnc sv dx
                let setup = do
                        forM_ ys $ \y -> do
                            v <- case crs M.! y of
                                Left (Just p) -> portAtom p
                                Left Nothing -> encAtom (AKey "cfree" [KS (fst y), KS (snd y)])
                                Right c | S.null (S.intersection (fst (sumA c)) (M.keysSet crs)) -> encFormula rs2 c
                                        | otherwise -> encAtom (AKey "cfree" [KS (fst y), KS (snd y)])
                            encSetSub y v
                        g <- encB (guardExpr st)
                        encSetOn True
                        rS <- encFormula rsx' f
                        encSetOn False
                        return (g, rS)
                    ((g, rS), e1) = runState setup e0
                case lit of
                    Nothing -> return (acc ++ [(key, RdySV f ys others False (if null others then Nothing else Just False) False Nothing nhw)], rsx')
                    Just lp -> do
                        let (rl, e2) = runState (portAtom lp) e1
                        (v1, e3) <- encSat sv [g, rS, negate rl] e2
                        let okR = v1 == Unsat
                        okI <- if null others then return Nothing else do
                            let (x, e4) = runState (do { rP <- encFormula rsx' f
                                                       ; a <- encAnd [rS, negate rP]; b <- encAnd [negate rS, rP]
                                                       ; encOr [a, b] }) e3
                            (v2, _) <- encSat sv [g, x] e4
                            return (Just (v2 == Unsat))
                        return (acc ++ [(key, RdySV f ys others okR okI (okR && okI /= Just False) lit nhw)], rsx')
        (svs, rsb') <- foldM (check S.empty) ([], rsb) rsites
        let recheck stn lab drop rsx = case [ x | x@(st, s) <- rsites, ssName st == stn, siLabel s == lab ] of
                (x : _) -> do { ([(_, r)], rsx') <- check drop ([], rsx) x; return (Just (r, rsx')) }
                [] -> return Nothing
        let svMap = M.fromList svs
            needSites = [ (st, s) | st <- steps, s <- ssSites st, isNeedRead st s ]
            sumB = infoSum rcb rsb'
            litReads sm f = S.toAscList (fst (sm f))
            litHw sm f = snd (sm f)
            recOf (st, s) = do
                let key = (ssIdx st, siPos s)
                    nd = needOf key
                    base = SiteRec (ssName st) (siLabel s) (sort (S.toList nd)) (siInst s) (siMeth s) (siCopy s)
                if S.member 'r' nd then do
                    let svr = svMap M.! key
                    lp <- maybe (Left ("readiness site " ++ ssName st ++ " / " ++ siLabel s ++ ": its step reads no ready port")) Right (svLit svr)
                    unless (S.null (litHw sumB (svF svr))) (Left "an R literal reads a hardware wire")
                    return (base (RRdy lp svr (svF svr == fsT) (litReads sumB (svF svr))))
                else do
                    let f = condUsed key
                    unless (S.null (litHw sumA f)) $
                        Left ("the condition of " ++ ssName st ++ " / " ++ siLabel s ++ " reads a hardware wire or an input")
                    let ps = [ (ssName (stepArr M.! i), siLabel (siteAt (i, j)), if siRead (siteAt (i, j)) then "read" else "call")
                             | (i, j) <- M.findWithDefault [] key partners ]
                    return (base (RReach (f == fsT) (litReads sumA f) ps f))
            checks = [ (stName st, meParse c, c) | st <- dSteps d, c <- stChecks st ]
        q1 <- solverQueries sv
        return $ do
            recs <- mapM recOf needSites
            return (Sites recs rs2 rsb' sd dx checks (q1 - q0) recheck)

-- | sitecond.parse_ann: a child's schedule annotations, (p, q) -> relation (the last statement wins).
annOf :: [([String], String, [String])] -> M.Map (String, String) String
annOf stmts = foldl' (\m (ps, rel, qs) -> foldl' (\m' k -> M.insert k rel m') m [ (a, b) | a <- ps, b <- qs ]) M.empty stmts

-- | sitecond's order_pass: per step a site order (data and SB order fixed, placed reads preferred),
-- each condition without literals over a later site, a wire or another method's input.
orderPass :: SD -> APkg -> (SStep -> ([(E, Maybe E)], [Act])) -> (String -> (Int, String) -> (Int, String) -> Bool)
          -> (SSite -> (Int, String)) -> (RC -> RS -> LitKey -> (S.Set Port2, S.Set String)) -> RC -> RS -> [SStep]
          -> (SStep -> SSite -> Bool) -> M.Map SK Int -> Either String (M.Map SK Int, RS)
orderPass sd ap stepItems prec use linfo rc rs0 steps isc reach = foldM one (M.empty, rs0) steps
  where
    sum0 = fsSummary (\k _ -> fst (linfo rc rs0 k)) S.unions S.empty (rsFS rs0)
    one (acc, rs) st
        | not (any (isc st) ss) = Right (acc, rs)
        | not (all (`M.member` pos) [0 .. n - 1]) = Left ("a hard cycle of sites in " ++ ssName st)
        | otherwise = Right (foldl' dropOne (acc, rs) [ s | s <- ss, isc st s ])
      where
        ss = ssSites st
        n = length ss
        own = S.fromList (fromMaybe [] (omLookup (fromMaybe "" (ssMeth st)) (apMethArgs ap)))
              `S.union` maybe S.empty (\m -> if null m then S.empty else S.singleton ("EN_" ++ m)) (ssMeth st)
        byport = M.fromListWith (flip (++)) [ ((siInst s, siMeth s), [siPos s]) | s <- ss ]
        (items, acts) = stepItems st
        argreads = exprCallsMany sd (map fst items)
        callacts = M.fromListWith (flip (++)) [ ((i, m), [(c, as)]) | ACallA i m c as <- acts ]
        -- data order: the sites on the ports a site's data reads, through one node per shared read set
        -- (a read port's argument reads, a definition's reads); a set holding the site's own port is expanded
        srcsOf s
            | siRead s = [ Right (Left (siInst s, siMeth s), M.findWithDefault S.empty (siInst s, siMeth s) argreads) ]
            | otherwise = concat [ concatMap src (maybe as (: as) c) | (c, as) <- M.findWithDefault [] (siInst s, siMeth s) callacts ]
          where src e = let (ps, ns) = exprSources sd e in map Left ps ++ [ Right (Right nm, defReads sd nm) | nm <- ns ]
        nb = M.size byport
        setSites rs | S.size rs <= nb = [ j | p <- S.toList rs, j <- M.findWithDefault [] p byport ]
                    | otherwise = [ j | (p, js) <- M.toList byport, S.member p rs, j <- js ]
        siteSrcs = [ (s, srcsOf s) | s <- ss ]
        vSets = M.fromList [ (key, rs) | (s, srcs) <- siteSrcs, Right (key, rs) <- srcs, not (S.member (siInst s, siMeth s) rs) ]
        vPreds = M.filter (not . IS.null) (M.map (IS.fromList . setSites) vSets)
        vId = M.fromList (zip (M.keys vPreds) [n ..])
        precPreds = IM.fromListWith IS.union [ (siPos t, IS.singleton (siPos s)) | grp <- M.elems byInstS, s <- grp, t <- grp
                                                                                , siPos s /= siPos t, prec (siInst s) (use s) (use t) ]
        sitePreds (s, srcs) =
            let own = (siInst s, siMeth s)
                direct = [ j | Left p <- srcs, j <- M.findWithDefault [] p byport ]
                expanded = [ j | Right (_, rs) <- srcs, S.member own rs, j <- setSites rs ]
                virt = [ v | Right (key, rs) <- srcs, not (S.member own rs), Just v <- [M.lookup key vId] ]
            in IS.delete (siPos s) (IS.fromList (direct ++ expanded)) `IS.union` IS.fromList virt
               `IS.union` IM.findWithDefault IS.empty (siPos s) precPreds
        -- every node's predecessors: sites 0 .. n-1, then the read-set nodes
        preds = IM.fromList ([ (siPos s, sitePreds x) | x@(s, _) <- siteSrcs ] ++ [ (vId M.! key, js) | (key, js) <- M.toList vPreds ])
        byInstS = M.fromListWith (flip (++)) [ (siInst s, [s]) | s <- ss ]
        soft = M.fromList [ (siPos s, if isc st s then S.fromList [ j | p <- S.toList (sum0 (reach M.! (ssIdx st, siPos s)))
                                                                      , j <- M.findWithDefault [] p byport ] else S.empty) | s <- ss ]
        -- a literal is dropped at site i when it reads a wire or input, an unsited port, or a site not before i
        score k = let (rd, hw) = linfo rc rs0 k
                  in if not (S.null (S.difference hw own)) || any (\p -> not (M.member p byport)) (S.toList rd) then maxBound
                     else maximum (minBound : [ M.findWithDefault n j pos | p <- S.toList rd, j <- byport M.! p ])
        maxScore = fsSummaryFrom (\k _ -> score k) (maximum . (minBound :)) minBound (rsFS rs0)
                                 [ reach M.! (ssIdx st, siPos s) | s <- ss, isc st s ]
        dropsOf f0 pi0 = go [f0] IS.empty S.empty
          where go [] _ acc = acc
                go (f : q) seen acc
                    | IS.member f seen || maxScore f < pi0 = go q seen acc
                    | otherwise = case fsNode (rsFS rs0) f of
                        NLit k _ -> go q (IS.insert f seen) (if score k >= pi0 then S.insert k acc else acc)
                        NAnd ys -> go (ys ++ q) (IS.insert f seen) acc
                        NOr ys -> go (ys ++ q) (IS.insert f seen) acc
                        _ -> go q (IS.insert f seen) acc
        succs = IM.fromListWith (++) [ (j, [i]) | (i, js) <- IM.toList preds, j <- IS.toList js ]
        pos = kahn (IM.map IS.size preds) cnt0
                   (S.fromList [ (cnt0 M.! i, i) | (i, js) <- IM.toList preds, i < n, IS.null js ]) S.empty M.empty
        -- the placeable site with the fewest soft predecessors left; counts kept as sites are placed
        cnt0 = M.map S.size soft
        softRev = M.fromListWith (++) [ (j, [i]) | (i, js) <- M.toList soft, j <- S.toList js ]
        kahn indeg cnt avail placed ps = case S.minView avail of
            Nothing -> ps
            Just ((_, best), avail1) ->
                let ps' = M.insert best (S.size placed) ps
                    placed' = S.insert best placed
                    (cnt', avail2) = foldl' (\(cm, av) i -> let c = cm M.! i
                                                            in (M.insert i (c - 1) cm, if S.member (c, i) av then S.insert (c - 1, i) (S.delete (c, i) av) else av))
                                            (cnt, avail1) (M.findWithDefault [] best softRev)
                    (indeg', newly) = foldl' release (indeg, []) (IM.findWithDefault [] best succs)
                in kahn indeg' cnt' (foldl' (\av k -> S.insert (cnt' M.! k, k) av) avail2 [ k | k <- newly, not (S.member k placed') ]) placed' ps'
        -- a read-set node is passed as soon as its last site is placed
        release (dg, nw) k = let v = dg IM.! k - 1; dg' = IM.insert k v dg
                             in if v /= 0 then (dg', nw)
                                else if k >= n then foldl' release (dg', nw) (IM.findWithDefault [] k succs)
                                else (dg', k : nw)
        dropOne (acc', rs') s =
            let i = siPos s
                f = reach M.! (ssIdx st, i)
                drops = dropsOf f (pos M.! i)
                (f', fs') = if S.null drops then (f, rsFS rs') else fsSubst drops f (rsFS rs')
            in (M.insert (ssIdx st, i) f' acc', rs' { rsFS = fs' })

-- ---------------------------------------------------------------- the view

-- | --show-model=sites: the conditional sites and the check lines' readings.
showSites :: Design -> APkg -> Sites -> IO [String]
showSites d ap st = do
  rls <- mapM recLines (sxRecs st)
  return $ concat rls
    ++ concat [ case r of
                  Left err -> ["check " ++ n ++ " error " ++ take 60 err]
                  Right Nothing -> ["check " ++ n ++ " cf"]
                  Right (Just (as, bs)) -> ["check " ++ n ++ " me " ++ unwords as ++ " / " ++ unwords bs]
              | (n, r, _) <- sxChecks st ]
    ++ [ if samePairs mes (dUserME d) then "same_pairs ok" else "same_pairs differ" ]
    ++ [ "group " ++ unwords g | g <- groups ]
    ++ [ "pairs " ++ unwords [ a ++ "/" ++ b | (a, b) <- prs ] ]
    ++ [ "cf " ++ n ++ " " ++ unwords (cfNames n c) ++ " " ++ canonExpr (cfResolve ap (sdDefs (sxDX st)) e)
       | (n, Right Nothing, c) <- sxChecks st, Right e <- [aexprE (apInsts ap) c] ]
  where
    mes = [ x | (_, Right (Just x), _) <- sxChecks st ]
    (groups, prs) = suffixGroups mes
    plist xs = "[" ++ intercalate ", " [ a ++ "." ++ b | (a, b) <- xs ] ++ "]"
    pyB b = if b then "True" else "False"
    recLines r = do
      extra <- case srKind r of
        RRdy _ sv _ _ | not (null (svOthers sv)) -> do
            m <- sxRecheck st (srStep r) (srLabel r) (S.fromList (svOthers sv)) (sxRSb st)
            return [ "  drop others: Y=" ++ plist (svY c2) ++ " rdy=" ++ pyB (svRdy c2) ++ " inv=" ++ maybe "None" pyB (svInv c2)
                     ++ " ok=" ++ pyB (svOk c2) ++ " f=" ++ fsText (rsFS rs') (svF c2) | Just (c2, rs') <- [m] ]
        _ -> return []
      return $ (("site " ++ srStep r ++ " " ++ srLabel r ++ " need=" ++ srNeed r ++ " copy=" ++ maybe "None" show (srCopy r)
         ++ " kind=" ++ (case srKind r of { RReach {} -> "reach"; RRdy {} -> "rdy" }))
        : case srKind r of
            RReach t fan ps c ->
                [ "  true=" ++ pyB t ++ " fan=" ++ plist fan
                , "  partners=" ++ unwords [ a ++ "/" ++ b ++ "/" ++ k | (a, b, k) <- ps ]
                , "  cond=" ++ fsText (rsFS (sxRS st)) c ]
            RRdy (ri, rm) sv t fan ->
                [ "  rdy=" ++ ri ++ "." ++ rm ++ " ok=" ++ pyB (svOk sv) ++ " R_true=" ++ pyB t ++ " R_fan=" ++ plist fan
                , "  Y=" ++ plist (svY sv) ++ " others=" ++ plist (svOthers sv) ++ " rdy=" ++ pyB (svRdy sv)
                  ++ " inv=" ++ maybe "None" pyB (svInv sv) ++ " hw=" ++ show (svHw sv)
                , "  f=" ++ fsText (rsFS (sxRSb st)) (svF sv) ]) ++ extra
