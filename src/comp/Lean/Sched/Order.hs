{-# LANGUAGE LambdaCase, MagicHash #-}
-- | The logical execution orders to check: the schedule's and the CF swaps.
module Lean.Sched.Order where

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

-- | The orders to check: the schedule's, and one per exported CF pair.
--
-- A CF export lets a parent run the two methods in the other relative
-- order.  Everything else stays constrained as bsc's scheduler left it: the
-- swapped order is a topological sort of the scheduler's execution-order
-- edges ('asi_sched_graph') plus one edge putting the later method first,
-- keeping every other step as close to its original position as possible.
-- A direct edge between the two methods is an arbitrary choice the CF
-- export makes irrelevant, so it is dropped.  A pair whose swap would
-- break a constraint is left out (and noted).
ordersOf :: [String] -> [(String, String)] -> (String -> [String]) -> [(String, String)]
         -> Either String ([Order], [(String, String)])
ordersOf names edges stepsOf cfs = case backward of
    (x, y) : _ | distinct ->
        Left ("bsc's execution order runs " ++ y ++ " before " ++ x
              ++ ", against an edge of its scheduling graph")
    _ -> Right ( Order "" ("", "") names n [] ("", "") : [ o | Right o <- swaps ]
               , [ p | Left p <- swaps ] )
  where
    n = length names
    pos = M.fromList (zip names [0 :: Int ..])
    atPos = M.fromList (zip [0 :: Int ..] names)
    -- the schedule's steps from each position on, shared by the orders
    tailsAt = M.fromList (zip [0 :: Int ..] (tails names))
    tailAt k = M.findWithDefault [] k tailsAt
    -- the edges between steps, checked once rather than in every swap
    edges' = [ (x, y) | (x, y) <- edges, M.member x pos, M.member y pos ]
    -- Every edge goes forward in the schedule's order: bsc's execution order
    -- is the execution nodes of a topological sort ('flattenCombinedGraph')
    -- of a graph with all of 'asi_sched_graph''s edges but the arbitrary
    -- choices it adds afterwards, which it reads off that same order.  So the
    -- topological sort gives back the schedule's order, and a swapped one
    -- differs from it only in a window ('window'), which is all that has to
    -- be sorted.  (An edge going backward is reported as an error.)
    distinct = S.size (S.fromList names) == n
    backward = [ (x, y) | (x, y) <- edges', pos M.! x >= pos M.! y ]
    predsOf = M.fromListWith S.union [ (y, S.singleton x) | (x, y) <- edges' ]
    succsOf = M.fromListWith S.union [ (x, S.singleton y) | (x, y) <- edges' ]
    swaps = [ swapped a b sa sb | (a, b) <- cfs, let sa = stepsOf a, let sb = stepsOf b
                                , not (null sa), not (null sb) ]
    -- each method's steps (several when bsc split it): all of the later
    -- method's go before all of the earlier one's.  Rather than as an edge
    -- from each of the one to each of the other, that is counted once: an
    -- earlier method's step also waits until none of the later method's
    -- steps is left to run.
    swapped a b sa sb =
        let first ss = minimum (map (pos M.!) ss)
            aFirst = first sa < first sb
            (e, l) = if aFirst then (sa, sb) else (sb, sa)
            eS = S.fromList e
            lS = S.fromList l
            order o from win = Order ("CF_" ++ a ++ "_" ++ b) (a, b) o from win
                                        (if aFirst then (b, a) else (a, b))
        in if distinct
           then case window e l eS lS of
               Just (f, win, to) ->
                   -- (the window differs at its start; the comparison is
                   -- for safety, as the whole order's would be)
                   let k = length (takeWhile id (zipWith (\i x -> M.lookup i atPos == Just x) [f ..] win))
                       o = take f names ++ win ++ tailAt to
                   in Right (if k == length win then order o n []
                             else order o (f + k) (drop k win))
               Nothing -> Left (a, b)
           -- (with a step repeated, the sort never places every position)
           else Left (a, b)
    -- Kahn's algorithm, always taking the earliest available step, where a
    -- step of the earlier method (eS) also waits for the later method's
    -- (lS), from the first step of the two methods, f, up to the first
    -- position from which the swapped order is the schedule's again: (f, the steps in between, that position).  The
    -- steps before f come out as they are, all of them before any of the
    -- two methods' steps.  The steps are taken in the schedule's order (a
    -- step is available when its predecessors have run), except that one
    -- whose predecessors have not all run waits, and runs, before any later
    -- step, once they have: the earliest step available is always a waiting
    -- one, if any is.  Once nothing waits and both methods' steps have all
    -- run, every step so far has run, and the rest run in order.
    window e l eS lS = go f S.empty M.empty M.empty (S.size lS) S.empty []
      where
        f = minimum (map (pos M.!) (e ++ l))
        lastSpecial = maximum (map (pos M.!) (e ++ l))
        -- the swapped order's edges, other than those from each of the later
        -- method's steps to each of the earlier one's: no edge goes the
        -- other way between them
        ownPreds y | S.member y eS = filter (`S.notMember` lS) (S.toList (M.findWithDefault S.empty y predsOf))
                   | S.member y lS = filter (`S.notMember` eS) (S.toList (M.findWithDefault S.empty y predsOf))
                   | otherwise     = S.toList (M.findWithDefault S.empty y predsOf)
        ownSuccs x | S.member x lS = filter (`S.notMember` eS) (S.toList (M.findWithDefault S.empty x succsOf))
                   | S.member x eS = filter (`S.notMember` lS) (S.toList (M.findWithDefault S.empty x succsOf))
                   | otherwise     = S.toList (M.findWithDefault S.empty x succsOf)
        ran done y = pos M.! y < f || S.member y done
        -- the next position to look at, the waiting steps that can run (by
        -- position), the others with how many of their own predecessors
        -- they wait for (the earlier method's apart: they also wait for
        -- the later method's steps), how many of the later method's steps
        -- are left, the steps run, and the window so far (last first)
        go next ready waiting waitingE lLeft done acc = case S.minView ready of
            Just ((_, x), ready') -> run x next ready' waiting waitingE lLeft done acc
            Nothing
                | M.null waiting && M.null waitingE && next > lastSpecial -> Just (f, reverse acc, next)
                | next >= n -> Nothing
                | otherwise ->
                    let y = atPos M.! next
                        k = length (filter (not . ran done) (ownPreds y))
                        isE = S.member y eS
                    in if k == 0 && not (isE && lLeft > 0) then run y (next + 1) ready waiting waitingE lLeft done acc
                       else if isE then go (next + 1) ready waiting (M.insert y k waitingE) lLeft done acc
                       else go (next + 1) ready (M.insert y k waiting) waitingE lLeft done acc
        run x next ready waiting waitingE lLeft done acc =
            let ss = ownSuccs x
                wokenW = [ y | y <- ss, M.member y waiting ]
                waiting' = foldr (M.adjust (subtract 1)) waiting wokenW
                newlyW = [ y | y <- wokenW, waiting' M.! y == 0 ]
                wokenE = [ y | y <- ss, M.member y waitingE ]
                waitingE' = foldr (M.adjust (subtract 1)) waitingE wokenE
                lLeft' = if S.member x lS then lLeft - 1 else lLeft
                -- (when the later method's last step runs, every step of the
                -- earlier one waiting for nothing else can run)
                newlyE | lLeft' > 0 = []
                       | lLeft > 0  = [ y | (y, 0) <- M.toList waitingE' ]
                       | otherwise  = [ y | y <- wokenE, waitingE' M.! y == 0 ]
            in go next (foldr (\y -> S.insert (pos M.! y, y)) ready (newlyW ++ newlyE))
                  (foldr M.delete waiting' newlyW) (foldr M.delete waitingE' newlyE) lLeft'
                  (S.insert x done) (x : acc)

-- | An order other than the schedule's, as the schedule's with some steps of
-- a range moved after the others: the range's first position, the position
-- after its last, and the positions moved, in order (none when the order is
-- the schedule's).  'ordersOf' makes only orders of this form: the earlier
-- method's steps, and the steps that must follow them, wait for the later
-- method's last step and then run in their order, right after it (Kahn's
-- algorithm always takes the earliest step available, and the waiting ones
-- first).  'Nothing' says the order is not of this form.
orderBlock :: M.Map String Int -> Order -> Maybe (Int, Int, [Int])
orderBlock pos o
    | length ps /= length (orWindow o) = Nothing
    | null win = Just (orFrom o, orFrom o, [])
    | ascending stay && ascending moved && rest == stay = Just (f, t, moved)
    | otherwise = Nothing
  where
    ps = mapMaybe (`M.lookup` pos) (orWindow o)
    f = orFrom o
    -- (the window may end with steps where the schedule has them)
    win = reverse (dropWhile (uncurry (==)) (reverse (zip [f ..] ps)))
    t = f + length win
    (stay, moved) = break (== f) (map snd win)
    movedSet = IS.fromList moved
    rest = [ p | p <- [f .. t - 1], not (IS.member p movedSet) ]
    ascending xs = and (zipWith (<) xs (drop 1 xs))

-- | Do two steps commute: is each of their uses of a submodule conflict-free
-- with each of the other's?  Then their relative order cannot matter.  (Two
-- writes of one register may come in either order, but not to the same
-- effect.)  @usesOf@ gives each step's 'stepInstUses', made once per step.
commute :: Design -> (String -> Maybe (M.Map String (S.Set String))) -> String -> String -> Bool
commute d usesOf x y = case (usesOf x, usesOf y) of
    -- uses of different instances always commute, so only each instance's
    -- methods used by both steps are compared
    (Just ux, Just uy) -> and [ S.member (mu, mw) cf
                              | (i, (mus, mws)) <- M.toList (M.intersectionWith (,) ux uy)
                              , let cf = cfCF (chConf (dChildren d M.! i))
                              , mu <- S.toList mus, mw <- S.toList mws ]
    _ -> False

-- | The methods a step uses, by instance: every read its expressions make,
-- whatever bsc's use analysis says of it.
stepInstUses :: Step -> M.Map String (S.Set String)
stepInstUses st = M.fromListWith S.union [ (ptInst u, S.singleton (ptMeth u)) | u <- uses ]
  where uses = stReadSites st ++ map clPort (stCalls st)

-- | The steps of an interface method: one, or one per rule when bsc split it.
methodSteps :: Design -> String -> [String]
methodSteps d m = M.findWithDefault [] m (dMethodSteps d)
