{-# LANGUAGE LambdaCase, MagicHash, BangPatterns, DeriveGeneric, DeriveAnyClass, StandaloneDeriving #-}
-- | Disjointness facts bsc relied on but did not record, confirmed by its solver.
module Lean.Sched.BscFacts
    ( DisjointFact(..), Chain(..), Candidate(..), BscLookup
    , disjointCandidates, confirmDisjointnessMore, invertedFacts, newBscLookup, bscCovers, bscUseCond, bscUseGuard
    ) where

import ASyntax (aTBool, aTrue, ADef(ADef), AExpr(ASDef), APackage(apkg_state_instances))
import ASyntaxUtil (aAnd, aOr, aOrs)
import AUses (ruleAncestor, ruleName, rulePred)
import Control.DeepSeq (NFData)
import Control.Monad (foldM)
import Data.Char (isDigit)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import Data.List (sortOn)
import DisjointTest (initDisjointTestState, checkDisjointExprWithCtx)
import Error (ErrorHandle)
import Flags (Flags)
import GHC.Generics (Generic)
import Id (mk_homeless_id)
import qualified Data.Foldable as F (Foldable(foldl', toList))
import qualified Data.IntMap.Strict as IM ((!), empty, findWithDefault, toList, fromList, insertWith, map)
import qualified Data.IntSet as IS (member, fromList)
import qualified Data.Map.Strict as M (Map, (!), elems, empty, findWithDefault, keys, lookup, member, size,
    insert, fromList, fromListWith, insertWith)
import qualified Data.Set as S (Set, member, empty, fromList, insert, singleton, toList, union)
import qualified Data.Sequence as Sq (Seq, empty, length, null, (|>))

import Lean.Solver (Solver, newLive, liveRun, newSolverOn)
import Lean.Sched.Conflicts (conflicting, conflictingIn, emptyUseIndex, mergeOn, useIndex, useIndexCons)
import Lean.Sched.Extract (freePrefix, scheduledRules)
import Lean.Sched.Order (orderBlock, ordersOf)
import Lean.Sched.Types (portKey, portLabel, Call(clCond, clPort), ConflictKind(NoFollow, NoPrecede, NoShare),
    Design(dSteps, dOrders, dSeqDefs, dDisjointSet, dBlocks, dMethodSteps, dExportedCF, dStepMap, dExecEdges),
    Port(ptMeth, ptInst), Step(stGuard, stMethod, stReads, stCalls, stName))

-- | Two uses whose conditions must be disjoint (of one step or two), or a use and a chain's
-- first k members ('dfChain'), whose condition is then their OR.
data DisjointFact = DisjointFact
    { dfStep1, dfStep2 :: String
    , dfCond1, dfCond2 :: AExpr        -- ^ each including its rule's predicate
    , dfWhy :: String
    , dfChain :: Maybe (Chain, Int)
    }

-- | A child method's (or port's) uses in step order, with their guarded conditions: a later
-- use that may not follow them is checked once against the OR of those before it.
data Chain = Chain { cnName :: String, cnMembers :: [(String, AExpr)] }

-- | What the solver is asked: a fact, or a chained fact with the facts to
-- ask instead if it is not confirmed (one for each earlier use).
data Candidate = Single DisjointFact | Chained DisjointFact [DisjointFact]

-- | The candidate facts: one per use against a chain of earlier uses from two or more steps,
-- pairs within a step pair by pair, and each step pair a CF-swap order inverts once.
disjointCandidates :: Design -> [Candidate]
disjointCandidates d = seqCands ++ map Single (nubFacts (invFacts ++ within))
  where
    stepArr = IM.fromList (zip [0 :: Int ..] (dSteps d))
    pos = M.fromList (zip (map stName (dSteps d)) [0 :: Int ..])
    distinct = M.size pos == length (dSteps d)
    uses s = stReads s ++ [ (clPort c, clCond c) | c <- stCalls s ]
    usesArr = IM.map (zip [0 :: Int ..] . uses) stepArr
    withGuard s c = maybe c (\g -> aAnd g c) (stGuard s)
    excluded a b = S.member (min a b, max a b) (dDisjointSet d)
                   || S.member (a, b) (dBlocks d) || S.member (b, a) (dBlocks d)
    seqFact s1 s2 p1 c1 p2 c2 =
        DisjointFact (stName s1) (stName s2) (withGuard s1 c1) (withGuard s2 c2)
                     (portLabel p1 ++ " then " ++ portLabel p2) Nothing

    -- the schedule's order: going forwards, each step meets an index of the
    -- earlier uses (by method, and by port), each bucket in step order
    seqCands = map snd (sortOn fst (concat (snd (F.foldl' visit ((M.empty, M.empty), []) (IM.toList usesArr)))))
    visit ((byMeth, byPort), out) (b, us) =
        let sb = stepArr IM.! b
            here = concat [ cands sb b j2 p2 c2 bucket
                          | (j2, (p2, c2)) <- us
                          , bucket <- conflictingIn d NoPrecede p2 byMeth byPort ]
            add idx k u = M.insertWith (\_ old -> addUse old u) k (addUse emptyBucket u) idx
            !byMeth' = F.foldl' (\m (j, (p, c)) -> add m (ptInst p, ptMeth p) (b, j, p, c)) byMeth us
            !byPort' = F.foldl' (\m (j, (p, c)) -> add m (portKey p) (b, j, p, c)) byPort us
        in ((byMeth', byPort'), here : out)
    -- the facts of a later use against a bucket of earlier ones, in pair order (the earlier
    -- uses listed only for the pairs or a method's longest chain)
    cands sb b j2 p2 c2 bucket =
        let earlier = F.toList (bkUses bucket)
            pairs = [ ((a, b, j1, j2), seqFact (stepArr IM.! a) sb p1 c1 p2 c2)
                    | (a, j1, p1, c1) <- earlier, not (excluded (stName (stepArr IM.! a)) (stName sb)) ]
            (a0, j0, p0, _) = bkLast bucket
            n = Sq.length (bkUses bucket)
            chainKey = if ptMeth p0 == ptMeth p2 then portLabel p0 else ptInst p0 ++ "." ++ ptMeth p0
            chain = Chain chainKey [ (stName (stepArr IM.! a), withGuard (stepArr IM.! a) c1) | (a, _, _, c1) <- earlier ]
            chained = DisjointFact (stName (stepArr IM.! a0)) (stName sb) (aOrs (map snd (cnMembers chain)))
                                   (withGuard sb c2) (chainKey ++ " (any of the " ++ show n
                                                      ++ " uses before) then " ++ portLabel p2)
                                   (Just (chain, n))
        in if bkSteps bucket >= 2 then [((a0, b, j0, j2), Chained chained (map snd pairs))]
           else [ (k, Single f) | (k, f) <- pairs ]

    -- each step pair some other order runs the other way round, once (only steps past the
    -- furthest an earlier order moved a step are new)
    invFacts
        | not distinct = []
        | otherwise = concat (snd (F.foldl' invert (IM.empty, []) (drop 1 (dOrders d))))
    invert (seen, out) o = case orderBlock pos o of
        Nothing -> (seen, out)
        Just (_, t, [x]) ->
            let from = max (x + 1) (IM.findWithDefault 0 x seen)
                !seen' = IM.insertWith max x t seen   -- strict: one order per exported CF pair
            in (seen', concat [ pairFacts y x | y <- [from .. t - 1] ] : out)
        Just (f, t, ms) ->
            let mset = IS.fromList ms
            in (seen, concat [ pairFacts y x | y <- [f .. t - 1], not (IS.member y mset), x <- ms, x < y ] : out)
    -- the facts of a step and an earlier one run after it
    pairFacts y x =
        let sy = stepArr IM.! y
            sx = stepArr IM.! x
            later = useIndex (fst . snd) (usesArr IM.! x)
        in if excluded (stName sy) (stName sx) then []
           else [ seqFact sy sx p1 c1 p2 c2
                | (_, (p1, c1)) <- usesArr IM.! y
                , (_, (p2, c2)) <- mergeOn fst (conflicting d NoFollow p1 later) ]

    -- the pairs of uses of each step: going backwards, each use meets an
    -- index of the later ones
    within = concatMap withinStep (dSteps d)
    withinStep s = fst (foldr meet ([], emptyUseIndex) (zip [0 :: Int ..] (uses s)))
      where
        meet (j, (p1, c1)) (facts, later) =
            ( [ DisjointFact (stName s) (stName s) (withGuard s c1) (withGuard s c2)
                             (portLabel p1 ++ " with " ++ portLabel p2 ++ " in one rule") Nothing
              | (_, (p2, c2)) <- mergeOn fst (conflicting d NoShare p1 later) ]
              ++ facts
            , useIndexCons (fst . snd) (j, (p1, c1)) later )

-- | The earlier uses of a method (or port), in order, with how many steps
-- they come from and the last.
data Bucket = Bucket { bkUses :: !(Sq.Seq (Int, Int, Port, AExpr)), bkSteps :: !Int
                     , bkLast :: (Int, Int, Port, AExpr) }

emptyBucket :: Bucket
emptyBucket = Bucket Sq.empty 0 (error "schedcheck: an empty bucket")

addUse :: Bucket -> (Int, Int, Port, AExpr) -> Bucket
addUse bk u@(a, _, _, _) =
    let (lastStep, _, _, _) = bkLast bk
        newStep = Sq.null (bkUses bk) || lastStep /= a
    in bk { bkUses = bkUses bk Sq.|> u, bkSteps = bkSteps bk + (if newStep then 1 else 0), bkLast = u }

-- | Facts with one text (steps and reason) kept once per pair of conditions, so that a
-- label with two uses has each use's fact checked (bscUseCond ORs their conditions).
nubFacts :: [DisjointFact] -> [DisjointFact]
nubFacts = go M.empty
  where go _ [] = []
        go seen (f : fs)
            | any (\(a, b) -> a == dfCond1 f && b == dfCond2 f) old = go seen fs
            | otherwise = f : go (M.insert k ((dfCond1 f, dfCond2 f) : old) seen) fs
          where k = (dfStep1 f, dfStep2 f, dfWhy f)
                old = M.findWithDefault [] k seen

deriving instance Generic DisjointFact
instance NFData DisjointFact
deriving instance Generic Chain
instance NFData Chain
deriving instance Generic Candidate
instance NFData Candidate

-- ---------------------------------------------------------------- the modular path

-- | bsc's solver on each candidate, keeping the confirmed facts (a chain: definitions, each the OR of the
-- one before and one more member), a way to ask more, and the skeleton solver on the same live state.
confirmDisjointnessMore :: ErrorHandle -> Flags -> APackage -> Design -> [Candidate]
                        -> IO ([DisjointFact], [DisjointFact] -> IO [DisjointFact], Solver)
confirmDisjointnessMore errh flags apkg d candidates = do
    let rules = [ (ruleName r, rulePred r, ruleAncestor r) | r <- scheduledRules apkg ]
    live <- newLive (initDisjointTestState "schedcheck" errh flags (M.elems (dSeqDefs d) ++ chainDefs) (apkg_state_instances apkg) rules)
    kept <- liveRun live (\st0 -> do { (st1, ks) <- foldM check (st0, []) candidates; return (ks, st1) })
    let more fs = liveRun live $ \st -> do
            (st', ks) <- foldM (\(s, ks) f -> do { (ok, s') <- ask s f; return (s', if ok then f : ks else ks) }) (st, []) fs
            return (reverse ks, st')
    sv <- newSolverOn live (freePrefix "sl_" (M.keys (dSeqDefs d)))
    return (reverse kept, more, sv)
  where
    chains = map fst (M.elems (M.fromListWith (\new old -> if snd new > snd old then new else old)
                                   [ (cnName c, (c, n)) | Chained (DisjointFact { dfChain = Just (c, n) }) _ <- candidates ]))
    prefix = freePrefix "uses_upto_" (M.keys (dSeqDefs d))
    prefixId c k = mk_homeless_id (prefix ++ show (chainIdx M.! cnName c) ++ "_" ++ show k)
    chainIdx = M.fromList (zip (map cnName chains) [0 :: Int ..])
    chainDefs = concat [ [ ADef (prefixId c k) aTBool (if k == 1 then e else aOr (ASDef aTBool (prefixId c (k - 1))) e) []
                         | (k, (_, e)) <- zip [1 :: Int ..] (cnMembers c) ]
                       | c <- chains ]
    ask st f = do
        let lhs = case dfChain f of
                Just (c, k) -> ASDef aTBool (prefixId c k)
                Nothing     -> dfCond1 f
        (res, st') <- checkDisjointExprWithCtx st aTrue aTrue lhs (dfCond2 f)
        return (res == Just True, st')
    check (st, kept) cand = case cand of
        Single f -> do
            (ok, st') <- ask st f
            return (st', if ok then f : kept else kept)
        Chained f fbs -> do
            (ok, st') <- ask st f
            if ok then return (st', f : kept)
            else foldM (\(s1, ks) g -> do { (ok', s2) <- ask s1 g; return (s2, if ok' then g : ks else ks) }) (st', kept) fbs

-- | The CF-swap candidates of one pair (@y@ later, @x@ earlier), made only when a swap can move
-- @x@ past @y@: the earlier method moves @x@, and the later method's last step is not before @y@.
invertedFacts :: Design -> String -> String -> [DisjointFact]
invertedFacts d yn xn
    | not distinct = []
    | Just y <- M.lookup yn pos, Just x <- M.lookup xn pos, x < y, inverted x y = nubFacts (pairFacts y x)
    | otherwise = []
  where
    steps = dSteps d
    names = map stName steps
    pos = M.fromList (zip names [0 :: Int ..])
    distinct = M.size pos == length steps
    stepArr = IM.fromList (zip [0 :: Int ..] steps)
    uses s = stReads s ++ [ (clPort c, clCond c) | c <- stCalls s ]
    withGuard s c = maybe c (\g -> aAnd g c) (stGuard s)
    excluded a b = S.member (min a b, max a b) (dDisjointSet d)
                   || S.member (a, b) (dBlocks d) || S.member (b, a) (dBlocks d)
    pairFacts y x =
        let sy = stepArr IM.! y
            sx = stepArr IM.! x
            later = useIndex (fst . snd) (zip [0 :: Int ..] (uses sx))
        in if excluded (stName sy) (stName sx) then []
           else [ DisjointFact (stName sy) (stName sx) (withGuard sy c1) (withGuard sx c2)
                               (portLabel p1 ++ " then " ++ portLabel p2) Nothing
                | (p1, c1) <- uses sy
                , (_, (p2, c2)) <- mergeOn fst (conflicting d NoFollow p1 later) ]
    edges = [ (a, b) | (a, b) <- dExecEdges d, M.member a pos, M.member b pos ]
    predsOf = M.fromListWith (++) [ (b, [a]) | (a, b) <- edges ]
    ancestors x0 = go [x0] (S.singleton x0)
      where go [] seen = seen
            go (v : st) seen = let ps = [ p | p <- M.findWithDefault [] v predsOf, not (S.member p seen) ]
                               in go (ps ++ st) (foldr S.insert seen ps)
    stepsOf m = M.findWithDefault [] m (dMethodSteps d)
    cfBy = M.fromListWith (++) (concat [ [(a, [(a, b)]), (b, [(a, b)])] | (a, b) <- dExportedCF d ])
    inverted x y =
        let xs = ancestors (names !! x)
            ms = S.fromList [ m | n <- S.toList xs, Just st <- [M.lookup n (dStepMap d)], Just m <- [stMethod st] ]
            firstOf m = minimum (map (pos M.!) (stepsOf m))
            lastOf m = maximum (map (pos M.!) (stepsOf m))
            cands = S.toList (S.fromList
                [ cf | m <- S.toList ms, cf@(a, b) <- M.findWithDefault [] m cfBy
                     , not (null (stepsOf a)), not (null (stepsOf b))
                     , let (e, l) = if firstOf a < firstOf b then (a, b) else (b, a)
                     , S.member e ms, firstOf e <= x, lastOf l >= y ])
        in any (\cf -> case ordersOf names (dExecEdges d) stepsOf [cf] of
                         Right (_ : o : _, _) -> case orderBlock pos o of
                             Just (_, t, [xi]) -> xi == x && y < t
                             Just (f, t, ms') -> x `elem` ms' && f <= y && y < t && y `notElem` ms'
                             Nothing -> False
                         _ -> False) cands

-- | sitecond.BscFacts: bsc's confirmed facts keyed by both steps and uses (the dump's lines);
-- a pair's CF-swap facts are asked when it is first looked up ('invertedFacts').
data BscLookup = BscLookup
    { blPos    :: M.Map String Int
    , blSingle :: IORef (M.Map (String, String, String, String) String)
    , blChain  :: M.Map (String, String) [(String, String, String)]
    , blBad    :: IORef (S.Set String)
    , blDone   :: IORef (S.Set (String, String))
    , blUses   :: M.Map (String, String) [AExpr]
    , blGuard  :: M.Map String (Maybe AExpr)
    , blMore   :: (String, String) -> IO [(Bool, String, String, String)]
    }

-- | A fact line's fields: confirmed, the steps, and its reason.
factFields :: Bool -> DisjointFact -> (Bool, String, String, String)
factFields ok f = (ok, dfStep1 f, dfStep2 f, dfWhy f)

newBscLookup :: Design -> [Candidate] -> [DisjointFact] -> ([DisjointFact] -> IO [DisjointFact]) -> IO BscLookup
newBscLookup d cands facts more = do
    let confirmed = S.fromList [ (dfStep1 f, dfStep2 f, dfWhy f) | f <- facts ]
        isOk f = S.member (dfStep1 f, dfStep2 f, dfWhy f) confirmed
        ls = concat [ case c of
                        Single f -> [factFields (isOk f) f]
                        Chained f fbs -> factFields (isOk f) f : (if isOk f then [] else map (\g -> factFields (isOk g) g) fbs)
                    | c <- cands ]
        parsed = [ x | l <- ls, Just x <- [parseFact l] ]
        bad = S.fromList [ body | (False, body, _) <- parsed ]
        good = [ (body, k) | (True, body, k) <- parsed, not (S.member body bad) ]
        single = M.fromList [ ((a, p, b, q), text) | (_, Left (a, p, b, q, text)) <- good ]
        chain = M.fromListWith (flip (++)) [ ((b, q), [(p, a, text)]) | (_, Right (a, p, b, q, text)) <- good ]
        steps = dSteps d
    sr <- newIORef single
    br <- newIORef bad
    dr <- newIORef S.empty
    return BscLookup
        { blPos = M.fromList (zip (map stName steps) [0 ..])
        , blSingle = sr, blChain = chain, blBad = br, blDone = dr
        , blUses = M.fromListWith (flip (++)) ([ ((stName st, portLabel p), [c]) | st <- steps, (p, c) <- stReads st ]
                                              ++ [ ((stName st, portLabel (clPort c)), [clCond c]) | st <- steps, c <- stCalls st ])
        , blGuard = M.fromList [ (stName st, stGuard st) | st <- steps ]
        , blMore = \(yn, xn) -> do
            let fs = invertedFacts d yn xn
            ok <- more fs
            let oks = S.fromList [ (dfStep1 f, dfStep2 f, dfWhy f) | f <- ok ]
            return [ factFields (S.member (dfStep1 f, dfStep2 f, dfWhy f) oks) f | f <- fs ]
        }

-- | parse_fact_line: (confirmed, body, single (a, p, b, q, text) or chain (a0, K, b, q, text)).
parseFact :: (Bool, String, String, String) -> Maybe (Bool, String, Either (String, String, String, String, String) (String, String, String, String, String))
parseFact (ok, a, b, why)
    | any (any (== ' ')) [a, b] = Nothing
    | otherwise = case words why of
        [p, "then", q] | why == unwords [p, "then", q] -> Just (ok, body, Left (a, p, b, q, text))
        [k, "(any", "of", "the", n, "uses", "before)", "then", q] | all (`elem` "0123456789") n, not (null n)
                                                                  , why == unwords [k, "(any", "of", "the", n, "uses", "before)", "then", q]
            -> Just (ok, body, Right (a, k, b, q, text))
        _ -> Nothing
  where body = a ++ " / " ++ b ++ ": " ++ why
        text = (if ok then "confirmed " else "UNCONFIRMED ") ++ body

-- | BscFacts.covers: the confirmed fact covering use @pa@ of step @a@ and use @pb@ of @b@.
bscCovers :: BscLookup -> String -> String -> String -> String -> IO (Maybe String)
bscCovers bl a pa b pb = do
    r <- one a pa b pb
    case r of
        Just t -> return (Just t)
        Nothing -> one b pb a pa
  where
    pos = blPos bl
    one x px y py = do
        -- facts of the CF-swap orders, for a pair whose earlier step is the second
        case (M.lookup x pos, M.lookup y pos) of
            (Just i, Just j) | i > j -> do
                done <- readIORef (blDone bl)
                if S.member (x, y) done then return () else do
                    writeIORef (blDone bl) (S.insert (x, y) done)
                    ls <- blMore bl (x, y)
                    let parsed = [ v | l <- ls, Just v <- [parseFact l] ]
                    modifyIORef' (blBad bl) (S.union (S.fromList [ body | (False, body, _) <- parsed ]))
                    bad <- readIORef (blBad bl)
                    modifyIORef' (blSingle bl) (\m -> foldl (\m' (k, t) -> M.insert k t m') m
                        [ ((a', p', b', q'), t) | (True, body, Left (a', p', b', q', t)) <- parsed, not (S.member body bad) ])
            _ -> return ()
        single <- readIORef (blSingle bl)
        case M.lookup (x, px, y, py) single of
            Just t -> return (Just t)
            Nothing -> case (M.lookup x pos, M.lookup y pos) of
                (Just i, Just j) | i < j ->
                    return (case [ t | (k, a0, t) <- M.findWithDefault [] (y, py) (blChain bl)
                                     , px == k || stripCopy px == k, i <= M.findWithDefault (-1) a0 pos ] of
                              (t : _) -> Just t
                              [] -> Nothing)
                _ -> return Nothing
    stripCopy l = case reverse l of
        (']' : r1) | (ds@(_ : _), '[' : r2) <- span isDigit r1 -> reverse r2
        _ -> l

-- | BscFacts.use_cond and use_guard: bsc's conditions of a step's uses with a label, and its guard.
bscUseCond :: BscLookup -> String -> String -> [AExpr]
bscUseCond bl st lab = M.findWithDefault [] (st, lab) (blUses bl)

bscUseGuard :: BscLookup -> String -> Maybe AExpr
bscUseGuard bl st = M.findWithDefault Nothing st (blGuard bl)
