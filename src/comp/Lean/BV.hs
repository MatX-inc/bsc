{-# LANGUAGE LambdaCase, MagicHash #-}
-- | Bit-vector sorts of bsc values, as Lean names them.
module Lean.BV where

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


-- | The sort of a symbolic value.
data Sort = SBool | SBits Integer | SString | SUnit
          | SView [Sort]          -- ^ a step's view as a function of the calls it sees
          | SPos                  -- ^ a position in the cycle's system-task calls (concrete)
    deriving (Eq, Ord, Show)

sortOf :: AType -> Sort
sortOf t = case t of
    ATBit 0    -> SUnit
    ATBit 1    -> SBool
    ATBit n    -> SBits n
    ATString _ -> SString
    _          -> SBits (aSizeOf t)
  where aSizeOf (ATTuple ts) = sum (map aSizeOf ts)
        aSizeOf (ATArray n e) = n * aSizeOf e
        aSizeOf (ATBit n) = n
        aSizeOf _ = 0

sortName :: Sort -> String
sortName s = case s of
    SBits n  -> "Tok" ++ show n
    SString  -> "TokStr"
    SView as -> concatMap (\a -> sortName a ++ " → ") as ++ "View"
    SPos     -> "TaskPos"
    _        -> "Bool"
