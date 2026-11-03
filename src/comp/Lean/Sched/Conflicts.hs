{-# LANGUAGE LambdaCase, MagicHash #-}
-- | Which child method uses may share a cycle, and an index to find conflicts.
module Lean.Sched.Conflicts where

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

-- | The conflict tables of 'Conflicts', over the given methods of a child.
--
-- Each method's list is made when it is first wanted ('conflictingIn' asks
-- for it only when it may be the shorter walk).
conflictTables :: [String] -> Conflicts -> Conflicts
conflictTables methods c =
    c { cfNoFollow  = table NoFollow, cfNoPrecede = table NoPrecede, cfNoShare = table NoShare }
  where ms = nubOrd methods
        table k = ML.fromList [ (m, filter (kindBad k c m) ms) | m <- ms ]

emptyUseIndex :: UseIndex a
emptyUseIndex = UseIndex M.empty M.empty

useIndex :: (a -> Port) -> [a] -> UseIndex a
useIndex port = foldr (useIndexCons port) emptyUseIndex

-- | Add a use in front of all the others.
useIndexCons :: (a -> Port) -> a -> UseIndex a -> UseIndex a
useIndexCons port x (UseIndex byMeth byPort) =
    let p = port x
    in UseIndex (M.insertWith (++) (ptInst p, ptMeth p) [x] byMeth) (M.insertWith (++) (portKey p) [x] byPort)

-- | The buckets of an index that hold the uses conflicting with port @p@,
-- given one of the tables of 'Conflicts': of each method the table names,
-- every copy, except that only the same copy of @p@'s own method conflicts
-- (two copies of a method are separate ports).  Each bucket is in index
-- order; the buckets are disjoint.
--
-- With 'cfNoFollow', these are the uses that may not follow a use of @p@
-- in a later rule; with 'cfNoPrecede', those a use of @p@ may not follow;
-- with 'cfNoShare', those one rule may not make with a use of @p@.  Uses of
-- other instances never conflict.
conflicting :: Design -> ConflictKind -> Port -> UseIndex a -> [[a]]
conflicting d kind p (UseIndex byMeth byPort) = conflictingIn d kind p byMeth byPort

-- | 'conflicting' over any buckets, by instance and method, and by port.
--
-- Of the methods that conflict with @p@'s and the methods of its instance
-- the index holds, the fewer are walked: an index can hold few of a
-- child's many methods, or many of them when few conflict.
conflictingIn :: Design -> ConflictKind -> Port
              -> M.Map (String, String) b -> M.Map (String, String, Maybe Integer) b -> [b]
conflictingIn d kind p byMeth byPort
    | length (take (M.size held + 1) listed) <= M.size held =
        [ us | m <- listed, Just us <- [bucket m] ]
    | otherwise =
        [ us | ((_, m), _) <- M.toAscList held, kindBad kind conf (ptMeth p) m, Just us <- [bucket m] ]
  where
    conf = chConf (childOf d p)
    listed = M.findWithDefault [] (ptMeth p) (kindTable kind conf)
    -- the index's methods of p's instance
    held = M.takeWhileAntitone ((== ptInst p) . fst) (M.dropWhileAntitone ((< ptInst p) . fst) byMeth)
    bucket m = if m == ptMeth p then M.lookup (portKey p) byPort else M.lookup (ptInst p, m) byMeth

kindTable :: ConflictKind -> Conflicts -> M.Map String [String]
kindTable k = case k of { NoFollow -> cfNoFollow; NoPrecede -> cfNoPrecede; NoShare -> cfNoShare }

-- | Does method @m2@ conflict with @m@ in this way?
kindBad :: ConflictKind -> Conflicts -> String -> String -> Bool
kindBad k c m m2 = case k of
    NoFollow  -> not (mayFollow c m m2)
    NoPrecede -> not (mayFollow c m2 m)
    NoShare   -> not (mayShareRule c m m2)

-- | The buckets of 'conflicting' as one list, in the order of a key.
mergeOn :: Ord k => (a -> k) -> [[a]] -> [a]
mergeOn key bs = case bs of
    []  -> []
    [b] -> b
    _   -> sortOn key (concat bs)

-- | May @m1@, invoked by one rule, be followed in the same cycle by @m2@ in a
-- later rule?  (bsc's @ncSetSC@.)
mayFollow :: Conflicts -> String -> String -> Bool
mayFollow c m1 m2 = any (S.member (m1, m2)) [cfCF c, cfSB c, cfSBR c]

-- | May one rule invoke both @m1@ and @m2@?  (bsc's @ncSetPC@, either way.)
mayShareRule :: Conflicts -> String -> String -> Bool
mayShareRule c m1 m2 =
    S.member (m1, m2) (cfCF c) || S.member (m1, m2) (cfSB c) || S.member (m2, m1) (cfSB c)
