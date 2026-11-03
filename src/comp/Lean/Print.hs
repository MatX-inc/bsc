{-# LANGUAGE LambdaCase, MagicHash #-}
-- | Building and printing Lean text: names, doc comments, wrapping.
module Lean.Print where

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


-- | The words Lean 4 (v4.35) does not accept as identifiers, and a few more
-- it does accept but that read badly as names.
leanKeywords :: [String]
leanKeywords = words
    "abbrev at attribute axiom break builtin_initialize by calc catch class \
    \coinductive coinductive_fixpoint continue decreasing_by declare_syntax_cat def \
    \deriving do elab elab_rules else end example export extends false finally for \
    \forall from fun have hiding if import in include inductive inductive_fixpoint \
    \infix infixl infixr initialize instance let local macro macro_rules match meta \
    \mod mut mutual namespace noncomputable nomatch nofun nonrec notation omit opaque \
    \open partial partial_fixpoint postfix prefix private protected public \
    \recommended_spelling renaming return run_cmd run_elab run_meta scoped section \
    \seal set_option show sorry structure suffices syntax termination_by then theorem \
    \this true try unif_hint unless universe unsafe unseal using variable where with \
    \Type Prop Sort"

leanKeywordSet :: S.Set String
leanKeywordSet = S.fromList leanKeywords

-- | Text that cannot open or close a Lean block comment.
commentSafe :: String -> String
commentSafe ('/' : '-' : rest) = '/' : ' ' : commentSafe ('-' : rest)
commentSafe ('-' : '/' : rest) = '-' : ' ' : commentSafe ('/' : rest)
commentSafe (c : rest) = c : commentSafe rest
commentSafe [] = []

-- | A Lean doc comment, wrapped.
doc :: String -> [String]
doc s0 = case wrap 92 (commentSafe s0) of
    [l]      -> ["/-- " ++ l ++ " -/"]
    (l : ls) -> ("/-- " ++ l) : map ("    " ++) (init ls) ++ ["    " ++ last ls ++ " -/"]
    []       -> []

chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs = case splitAt n xs of
    (a, []) -> [ a | not (null a) ]
    (a, b)  -> a : chunksOf n b

-- | Items joined by a separator, as many to a line as fit in a width.
wrapJoin :: Int -> String -> [String] -> [String]
wrapJoin w sep = go
  where go [] = []
        go (x : xs) = let (line, rest) = fill (length x) [x] xs in line : go rest
        -- the line so far: its length, and its pieces, last first
        fill n parts (y : ys) | n' <= w = fill n' (y : sep : parts) ys
          where n' = n + length sep + length y
        fill _ parts ys = (concat (reverse parts), ys)

wrap :: Int -> String -> [String]
wrap w = wrapJoin w " " . words
