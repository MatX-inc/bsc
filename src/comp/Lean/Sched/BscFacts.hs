{-# LANGUAGE LambdaCase, MagicHash, BangPatterns, DeriveGeneric, DeriveAnyClass, StandaloneDeriving #-}
-- | Disjointness facts bsc relied on but did not record, confirmed by its solver.
module Lean.Sched.BscFacts where

import Control.Exception(evaluate)
import Control.Monad(foldM, forM, forM_, when)
import Control.Monad.State.Strict(State, evalState, gets, modify', runState)
import qualified Data.ByteString as BS
import Data.Char(isAsciiLower, isAsciiUpper, isDigit, toUpper)
import Data.Containers.ListUtils(nubOrd)
import Data.IORef(IORef, newIORef, readIORef, writeIORef)
import qualified Data.Foldable as F
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import GHC.Exts(isTrue#, reallyUnsafePtrEquality#)
import Data.List(intercalate, isPrefixOf, mapAccumL, partition, sortOn, tails, transpose)
import qualified Data.Map.Lazy as ML
import qualified Data.Map.Strict as M
import Data.Maybe(catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import qualified Data.Sequence as Sq
import qualified Data.Set as S
import System.Console.GetOpt(ArgDescr(..), ArgOrder(..), OptDescr(..), getOpt, usageInfo)
import System.Environment(getArgs)
import System.Exit(exitFailure, exitSuccess)
import System.FilePath(takeFileName)
import System.IO.Unsafe(unsafePerformIO)
import System.IO(IOMode(..), hPutStr, hPutStrLn, hSetEncoding, stderr,
                 stdout, utf8, withFile)

import Control.DeepSeq(NFData)
import GHC.Generics(Generic)
import ABin(ABin(..), ABinModInfo(..))
import ASchedule(AScheduleInfo(..), MethodId(..), UniqueUse(..))
import AScheduleInfo(SchedNode(..), erdbToList)
import AState(aState)
import AVerilogUtil(vNameToTask)
import ASyntax
import ASyntaxUtil(aType, aAnd, aOr, aOrs, aSubst, argInputPorts)
import AUses(Rule(..), buildUseMaps, extractCondition, rumToMethodUseMap,
             ruleName, rulePred, ruleAncestor)
import BackendNamingConventions(isRWire, isRWire0, isBypassWire)
import DisjointTest(initDisjointTestState, checkDisjointExprWithCtx)
import Error(ErrorHandle, initErrorHandle)
import Flags(Flags, unSpecTo, useDPI)
import GenABin(readABinFile)
import Id(getIdString, getIdBaseString, mkIdCanFire, mkIdWillFire, mkRdyId,
          isRdyId, getIdPosition, setIdBaseString, mk_homeless_id)
import IntegerUtil(aaaa)
import IntLit(IntLit(..))
import PPrint(ppString)
import Pragma(SchedulePragma(..))
import Prim(PrimOp(..))
import RSchedule(ratToNestedLists)
import SchedInfo(SchedInfo(..), MethodConflictInfo(..))
import VModInfo(vName, vFields, vSched, vPath, VFieldInfo(..), VPathInfo(..),
                getVNameString, mkNamedOutputs)

import Lean.Sched.Types
import Lean.Sched.Conflicts
import Lean.Sched.Extract
import Lean.Sched.Order

-- | A pair of uses whose conditions must be disjoint: of one step, and of
-- another or the same; or of a step, and of any of the earlier uses a chain
-- lists ('dfChain'): then the first condition is the OR of the chain's
-- first k members.
data DisjointFact = DisjointFact
    { dfStep1, dfStep2 :: String
    , dfCond1, dfCond2 :: AExpr        -- ^ each including its rule's predicate
    , dfWhy :: String
    , dfChain :: Maybe (Chain, Int)
    }

-- | The uses of one method (or one port) of a child, in the order of the
-- steps, each with its step and its condition (with the rule's predicate):
-- a later use that may not follow them is checked against the OR of those
-- before it, once, rather than against each.
data Chain = Chain { cnName :: String, cnMembers :: [(String, AExpr)] }

-- | What the solver is asked: a fact, or a chained fact with the facts to
-- ask instead if it is not confirmed (one for each earlier use).
data Candidate = Single DisjointFact | Chained DisjointFact [DisjointFact]

-- | The candidate facts, in a form the solver can check.
--
-- In the schedule's order, a use that may not follow some earlier uses of
-- a child's method (or port: two copies of a method are separate ports) is
-- checked against all of them at once when they come from two or more
-- steps: the earlier uses of each such method form a chain ('Chain'), and
-- the use's condition must be disjoint from the OR of the chain's members
-- before it, which a chain of definitions writes in a size linear in the
-- members.  So there is one fact per use, not per pair of uses.  Pairs of
-- uses in one step that may not share a rule are checked pair by pair.
--
-- An order other than the schedule's runs some steps before steps they come
-- after in the schedule ('orderBlock'); each such pair of steps is checked
-- once, however many orders run it so.
disjointCandidates :: Design -> [Candidate]
disjointCandidates d = seqCands ++ map Single (nubOn key (invFacts ++ within))
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
    key f = (dfStep1 f, dfStep2 f, dfWhy f)

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
    -- the facts of one later use and one bucket of earlier uses (sorted as
    -- pairing each step with each later one would sort them)
    -- (the earlier uses are listed only when needed: for the pairs, or the
    -- longest chain of a method)
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

    -- the orders other than the schedule's: each pair of steps one of them
    -- runs the other way round, once (for an order that moves one step, the
    -- steps it moves it past are those up to its range's end; only those
    -- after the furthest any earlier order moved it past are new)
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
emptyBucket = Bucket Sq.empty 0 (error "schedlean: an empty bucket")

addUse :: Bucket -> (Int, Int, Port, AExpr) -> Bucket
addUse bk u@(a, _, _, _) =
    let (lastStep, _, _, _) = bkLast bk
        newStep = Sq.null (bkUses bk) || lastStep /= a
    in bk { bkUses = bkUses bk Sq.|> u, bkSteps = bkSteps bk + (if newStep then 1 else 0), bkLast = u }

nubOn :: Ord k => (a -> k) -> [a] -> [a]
nubOn f = go S.empty
  where go _ [] = []
        go seen (x : xs) | S.member (f x) seen = go seen xs
                         | otherwise = x : go (S.insert (f x) seen) xs

-- | Re-ask bsc's disjointness solver (Yices or STP, as configured) about
-- each candidate ('disjointCandidates'); keep the confirmed facts.  A chain
-- is given to the solver as definitions, each the OR of the one before and
-- one more member, so that each fact on it is a query of one definition.
confirmDisjointness :: ErrorHandle -> Flags -> APackage -> Design -> [Candidate] -> IO [DisjointFact]
confirmDisjointness errh flags apkg d candidates =
    case candidates of
        [] -> return []
        cands -> do
            -- the definitions and rule predicates the scheduler gave it,
            -- including the use conditions 'buildUseMaps' defines
            let rules = [ (ruleName r, rulePred r, ruleAncestor r) | r <- scheduledRules apkg ]
            st0 <- initDisjointTestState "schedlean" errh flags
                       (M.elems (dSeqDefs d) ++ chainDefs) (apkg_state_instances apkg) rules
            (_, kept) <- foldM check (st0, []) cands
            return (reverse kept)
  where
    -- each chain once (by name, the longest: the later uses' chains extend
    -- the earlier ones'), its prefixes as definitions
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
            else foldM askOne (st', kept) fbs
    askOne (s1, ks) g = do
        (ok', s2) <- ask s1 g
        return (s2, if ok' then g : ks else ks)


deriving instance Generic DisjointFact
instance NFData DisjointFact
deriving instance Generic Chain
instance NFData Chain
deriving instance Generic Candidate
instance NFData Candidate
