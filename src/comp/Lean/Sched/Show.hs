{-# LANGUAGE LambdaCase, MagicHash #-}
-- | --show-model: the extracted model as text.
module Lean.Sched.Show where

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
import Lean.Sched.BscFacts
import Lean.Sched.Conflicts

-- | The extracted model, for checking phase 1 on its own (@-dump-design@),
-- with the candidate disjointness facts and those the solver confirmed.
-- The condition shown with each read ("hint: used when") is bsc's use
-- condition, only a hint for which facts to ask for; the reads out of order
-- are the sites, whatever the hints say.
dumpDesign :: Design -> [Candidate] -> [DisjointFact] -> [String]
dumpDesign d cands facts =
    [ "module " ++ dModule d ++ " (package " ++ dPackage d ++ "), from " ++ dSource d
    , "inputs:  " ++ unwords [ n ++ " :: " ++ ppString t | n <- dInputOrder d, Just t <- [M.lookup n (dInputs d)] ]
    , "outputs: " ++ unwords [ n ++ " :: " ++ ppString t | (n, t) <- dOutputs d ]
    , "", "children:" ]
    ++ concat [ ("  " ++ chInst c ++ " : " ++ chModule c ++ "   " ++ unwords (words (chSched c)))
                : [ "    " ++ mtName mt ++ " (" ++ kindName (mtKind mt) ++ ")"
                    ++ (if null (mtDeps mt) then "" else ", sees same-cycle " ++ intercalate ", " (mtDeps mt))
                    ++ concat [ "\n      copy " ++ maybe "-" show (ptCopy p) ++ ":"
                                ++ maybe "" (" enable " ++) (ptEnable p)
                                ++ concat [ " arg " ++ a ++ " :: " ++ ppString t | (a, t) <- ptArgs p ]
                                ++ concat [ " result " ++ r ++ " :: " ++ ppString t | (r, t) <- ptResults p ]
                              | p <- mtCopies mt ]
                  | mt <- map (chMethods c M.!) (chOrder c) ]
                ++ [ "    wire: " ++ o ++ " = " ++ i ++ " within the cycle" | (o, i) <- chWires c ]
              | c <- map (dChildren d M.!) (dChildOrder d) ]
    ++ [ "", "execution order:" ]
    ++ concat [ [ "  " ++ show k ++ ". " ++ stName st ++ "   [" ++ stDescr st ++ "]"
                , "       fires on " ++ stFire st ++ ", can fire " ++ stCanFire st ]
                ++ [ "       guard " ++ ppString g | Just g <- [stGuard st] ]
                ++ [ "       reads " ++ portLabel p ++ "  (hint: used when " ++ ppString c ++ ")" | (p, c) <- stReads st ]
                ++ [ "       reads " ++ portLabel (siPort s) ++ " out of order: " ++ siteText s | s <- stSites st ]
                ++ [ "       a system task (when " ++ ppString c ++ ") reads " ++ portLabel (siPort s)
                     ++ " out of order: " ++ siteText s
                   | (c, _, ss) <- stTaskSites st, s <- ss ]
                ++ [ "       calls " ++ portLabel (clPort c) ++ " when " ++ ppString (clCond c)
                     ++ " with " ++ intercalate ", " (map ppString (clArgs c)) | c <- stCalls st ]
                ++ [ "       check " ++ ppString c | c <- stChecks st ]
                ++ [ "       output " ++ ouName o ++ " = " ++ ppString (ouSeq o)
                     ++ (if null (ouMask o) then "" else "  (when " ++ unwords (map show (ouMask o)) ++ ")")
                   | o <- stOutputs st ]
                ++ [ "       task #" ++ show (tcTag tc) ++ " " ++ tcName tc ++ (if tcCheck tc then " (run-time check)" else "")
                     ++ " when " ++ ppString (tcCond tc)
                     ++ (if null (tcArgs tc) then "" else " with " ++ intercalate ", " (map ppString (tcArgs tc)))
                   | tc <- stTaskCalls st ]
              | (k, st) <- zip [1 :: Int ..] (dSteps d) ]
    ++ (if null (dHwTasks d) then [] else
        "" : "system-task calls of the hardware, in the order of its always block:"
        : [ "  " ++ maybe "matches none" (\k -> "task #" ++ show k) (htTag h) ++ ": " ++ htName h ++ " [" ++ htId h ++ "]"
            ++ " when " ++ ppString (htCond h)
          | h <- dHwTasks d ])
    ++ [ "", "use conditions (recomputed):" ]
    ++ [ "  " ++ getIdString (adef_objid u) ++ " = " ++ ppString (adef_expr u) | u <- dUseDefs d ]
    ++ [ "", "orders checked:" ]
    ++ [ "  " ++ (if null (orName o) then "schedule" else orName o) ++ ": " ++ unwords (orSteps o)
       | o <- dOrders d ]
    ++ [ "", "disjoint guards (bsc):     " ++ pairsText (dDisjoint d)
       , "urgency blocking (r, by):  " ++ pairsText (S.toList (dBlocks d))
       , "mutually_exclusive (user): " ++ pairsText (dUserME d)
       , "exported C:                " ++ pairsText (dExportedC d)
       , "exported CF:               " ++ pairsText (dExportedCF d)
       , "unrealizable CF swaps:     " ++ pairsText (dUnrealized d)
       , "exported SB/SBR:           " ++ unwords [ a ++ " " ++ r ++ " " ++ b | (a, r, b) <- dExportedSB d ]
       , "enables (method, EN):      " ++ pairsText (M.toList (dEnables d))
       , "ready (method, RDY):       " ++ pairsText (M.toList (dReady d))
       , "unspecified values:        " ++ dUnspec d
       , "", "disjointness bsc relied on but did not record:" ]
    ++ concatMap candLines cands
    ++ [ "warning: " ++ w | w <- dWarnings d ]
  where
    kindName k = case k of { ValueMethod -> "value"; ActionMethod -> "action"; ActionValueMethod -> "actionvalue" }
    siteText s
        | siWithin s = "with its own call of " ++ portLabel (siUse s)
        | ptKind (siUse s) == ValueMethod = "after a read of " ++ portLabel (siUse s)
        | otherwise = "after a call of " ++ portLabel (siUse s)
    pairsText ps = if null ps then "-" else unwords [ "(" ++ a ++ ", " ++ b ++ ")" | (a, b) <- ps ]
    factKey f = (dfStep1 f, dfStep2 f, dfWhy f)
    confirmed = S.fromList (map factKey facts)
    -- (a chained fact not confirmed is followed by the pairs asked instead)
    candLines c = case c of
        Single f -> [factLine "  " f]
        Chained f fbs -> factLine "  " f : (if S.member (factKey f) confirmed then [] else map (factLine "    ") fbs)
    factLine ind f = ind ++ (if S.member (factKey f) confirmed then "confirmed " else "UNCONFIRMED ")
                     ++ dfStep1 f ++ " / " ++ dfStep2 f ++ ": " ++ dfWhy f
