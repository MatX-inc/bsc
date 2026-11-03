{-# LANGUAGE LambdaCase, MagicHash #-}
-- | Reads the submodule annotations rule out (the out-of-order sites).
module Lean.Sched.OutOfOrder where

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
import Lean.Sched.Order

-- | Each step with its sites ('stSites', 'stTaskSites'), the copies with a
-- site, the value copies whose reads a later read may not follow
-- ('dReadFlags'), and the CAN_FIRE signals the sites rely on ('dRelied').
--
-- A read at step x is a site for a use u when a step making u comes before
-- x in the schedule's order or, when other orders are checked, in one of
-- them (a moved step runs after the steps of its range that stay,
-- 'orderBlock').  A use is a call of an action copy, or a read of a value
-- copy (in the step's own expressions or a system task's).  A step bsc
-- proved never to fire with x does not count: the theorem has that fact
-- ('disjoint_' in section 5) whenever both steps have a CAN_FIRE, and each
-- fires only when its CAN_FIRE holds ('firesOnlyWhenCan'), so whenever x
-- fires no step it leaves out made u.
--
-- The work is per read and use, not per pair of steps: the steps that make
-- each use are a set of positions, and whether one in a range of positions
-- is not left out is a few lookups, each meeting a left-out position once.
-- For a block of several moved steps, the positions of the steps of its
-- range that stay are made once per block and use, so that no moved step's
-- range walks past the other moved steps.
havocSites :: Design -> ([Step], S.Set Port, [Port], [String])
havocSites d = (sited, S.fromList [ siPort s | st <- sited, s <- stSites st ], flagPorts, relied)
  where
    steps = dSteps d
    stepArr = IM.fromList (zip [0 :: Int ..] steps)
    pos = M.fromList (zip (map stName steps) [0 :: Int ..])
    distinct = M.size pos == length steps
    callsAt = IM.map (\st -> S.fromList (map clPort (stCalls st))) stepArr
    taskReads st = [ p | (_, _, ps) <- stTaskReads st, p <- ps ]
    -- the uses of each step: the action copies it calls, and the value
    -- copies it reads
    usesAt = IM.map (\st -> S.unions [ S.fromList (map clPort (stCalls st)), S.fromList (stReadSites st)
                                     , S.fromList (taskReads st) ])
                    stepArr
    -- each use, with the positions of the steps that make it
    usePos = M.fromListWith IS.union [ (u, IS.singleton k) | (k, us) <- IM.toList usesAt, u <- S.toList us ]
    useIdx = useIndex id (M.keys usePos)
    -- the uses a read may not follow: action copies, and value copies of
    -- other methods (two reads of one method see the same value)
    readPorts = S.unions [ S.fromList (stReadSites st ++ taskReads st) | st <- steps ]
    candidates = ML.fromList [ (p, [ u | u <- concat (conflicting d NoPrecede p useIdx)
                                       , ptKind u /= ValueMethod || ptMeth u /= ptMeth p ])
                             | p <- S.toList readPorts ]
    candidatesOf p = ML.findWithDefault [] p candidates
    -- the blocks the other orders move: one step, or several
    blocks | not distinct = []
           | otherwise = [ b | o <- drop 1 (dOrders d), Just b@(_, _, _ : _) <- [orderBlock pos o] ]
    multi = zip [0 :: Int ..] [ b | b@(_, _, _ : _ : _) <- blocks ]
    -- of each block of several steps, the positions in its range of the
    -- steps that stay, by the use they make (each block's made when first
    -- asked for)
    stays = ML.fromList [ (b, M.fromListWith IS.union [ (u, IS.singleton k) | k <- [lo .. t - 1], IS.notMember k movedSet
                                                                          , u <- S.toList (usesAt IM.! k) ])
                        | (b, (lo, t, moved)) <- multi, let movedSet = IS.fromList moved ]
    -- the ranges of positions that come before each step in some order:
    -- the positions before it; those up to the end of its range, for a step
    -- moved alone (the longest of those); and for a step of a block of
    -- several, those of the block's staying steps after it
    extra = IM.fromListWith (++) ([ (x, [(x + 1, t, Nothing)]) | (_, t, [x]) <- blocks ]
                                  ++ [ (x, [(x + 1, t, Just b)]) | (b, (_, t, moved)) <- multi, x <- moved ])
    rangesOf k =
        let rs = IM.findWithDefault [] k extra
            singles = [ t | (_, t, Nothing) <- rs ]
        in (0, k, Nothing) : [ (k + 1, maximum singles, Nothing) | not (null singles) ]
           ++ [ r | r@(_, _, Just _) <- rs ]
    -- the steps bsc proved never fire with each, by the facts the theorem has
    same = sameAtHw d
    canFireDef st = M.member (stCanFire st) (dHwDefs d)
    reasonAt = IM.map (\st -> if canFireDef st then firesOnlyWhenCan d same st else Nothing) stepArr
    disjointPos
        | not distinct = []
        | otherwise = [ (ka, kb) | (a, b) <- dDisjoint d, Just ka <- [M.lookup a pos], Just kb <- [M.lookup b pos]
                                 , isJust (reasonAt IM.! ka), isJust (reasonAt IM.! kb) ]
    prunable = IM.fromListWith IS.union [ (x, IS.singleton y) | (ka, kb) <- disjointPos, (x, y) <- [(ka, kb), (kb, ka)] ]
    relied = S.toList (S.fromList [ stCanFire (stepArr IM.! k) | (ka, kb) <- disjointPos, k <- [ka, kb]
                                                              , reasonAt IM.! k == Just ViaSequential ])
    -- does a step at a position of a range, other than those left out, make u?
    madeIn u (lo, hi, src) out = go lo
      where ks = case src of
                Nothing -> M.findWithDefault IS.empty u usePos
                Just b  -> M.findWithDefault IS.empty u (stays ML.! b)
            go k = case IS.lookupGE k ks of
                Just r | r < hi -> not (out r) || go (r + 1)
                _ -> False
    sitesAt k ps =
        let px = IM.findWithDefault IS.empty k prunable
            rs = rangesOf k
            own = useIndex id (S.toList (callsAt IM.! k))
        in [ s | p <- ps
               , s <- [ Site p u False | u <- candidatesOf p
                                       , any (\r -> madeIn u r (`IS.member` px)) rs ]
                      ++ [ Site p u True | u <- concat (conflicting d NoShare p own) ] ]
    sited = [ st { stSites = sitesAt k (stReadSites st)
                 , stTaskSites = [ (c, args, sitesAt k ps) | (c, args, ps) <- stTaskReads st ] }
            | (k, st) <- zip [0 ..] steps ]
    flagPorts = S.toList (S.fromList [ siUse s | st <- sited
                                               , s <- stSites st ++ concat [ ss | (_, _, ss) <- stTaskSites st ]
                                               , ptKind (siUse s) == ValueMethod ])

-- | Does a step fire only when its CAN_FIRE holds, by the definitions the
-- theorem states, and why?  The facts that rely on it ('disjoint_' in
-- section 5) are about the hardware's CAN_FIRE signals.
--
-- A step whose hardware CAN_FIRE is constantly true qualifies.  So does an
-- action method whose WILL_FIRE is its enable, when the hardware computes
-- its ready output, which the environment's @ready_@ fact says the enable
-- implies, as its CAN_FIRE AND'ed with more: that is all about the
-- hardware's definitions ('ViaHardware').  Otherwise the step's firing is
-- its sequential WILL_FIRE (or, for a value method, its sequential ready
-- signal), read at the hardware's view, AND'ed with more: a rule's WILL_FIRE
-- is its CAN_FIRE AND'ed with what blocks it, and a value method's ready
-- signal is its CAN_FIRE AND'ed with more.  That CAN_FIRE is the sequential
-- one, so it must be one definition in both semantics: @same@ ('sameAtHw')
-- says the two packages define it alike, and the analysis checks that it
-- became one Lean definition ('ViaSequential').
firesOnlyWhenCan :: Design -> (String -> Bool) -> Step -> Maybe FireReason
firesOnlyWhenCan d same st
    | canTrue || any viaEnable fireConj = Just ViaHardware
    | same (stCanFire st) && (any isCan fireConj || valueReady) = Just ViaSequential
    | otherwise = Nothing
  where
    seqDef n = adef_expr <$> M.lookup n (dSeqDefs d)
    hwDef n = adef_expr <$> M.lookup n (dHwDefs d)
    conj e = case e of
        APrim _ _ op args | op == PrimBAnd || op == PrimAnd -> concatMap conj args
        _ -> [e]
    isCan e = case e of { ASDef _ i -> getIdString i == stCanFire st; _ -> False }
    canTrue = case hwDef (stCanFire st) of { Just (ASInt _ _ il) -> ilValue il /= 0; _ -> False }
    fireConj = maybe [] conj (seqDef (stFire st))
    -- is the CAN_FIRE among some conjuncts of a hardware signal, through
    -- the definitions they name (each looked into once)?
    canAmong _ [] = False
    canAmong seen (x : xs) = case x of
        _ | isCan x -> True
        ASDef _ i | let n = getIdString i, S.notMember n seen, Just e <- hwDef n -> canAmong (S.insert n seen) (conj e ++ xs)
        _ -> canAmong seen xs
    -- the method's ready output, which the environment's ready_ fact is
    -- about, as the hardware computes it
    readyOutIsCan = case stMethod st >>= (`M.lookup` dReady d) of
        Just r -> maybe False (canAmong S.empty . conj) (hwDef r)
        Nothing -> False
    viaEnable e = case (e, stMethod st) of
        (ASPort _ i, Just m) -> M.lookup m (dEnables d) == Just (getIdString i) && readyOutIsCan
        _ -> False
    -- the ready signal a value method's step fires on (only when it is an
    -- output): the sequential definition, read at the hardware's view
    valueReady = stKind st == ValueMethodStep && case stMethod st of
        Just m | M.member m (dReady d), Just r <- M.lookup m (dReadyDef d) -> maybe False (any isCan . conj) (seqDef r)
        _ -> False

-- | Why a step fires only when its CAN_FIRE holds ('firesOnlyWhenCan'): by
-- the hardware's definitions alone, or by its sequential firing, which
-- needs the CAN_FIRE to be one definition in both semantics.
data FireReason = ViaHardware | ViaSequential
    deriving Eq

-- | Is a definition of the 'APackage', read at the hardware's view, the
-- 'ASPackage''s definition of the same name?  The two are compared as
-- expressions, through the definitions they name (each pair once), where a
-- read of a value method without arguments in the one is the child's output
-- wire for the same copy in the other: 'aState' makes the one of the other,
-- and at the hardware's view such a read is that wire.  A definition that is
-- a mere name on one side only is looked through.  Anything else differing
-- makes them different.  (Partially applied, the table is made once.)
sameAtHw :: Design -> String -> Bool
sameAtHw d = \n -> ML.findWithDefault False n table
  where
    -- knot-tied through the definitions, so the table must be lazy
    table = ML.fromList [ (n, eqE (adef_expr s) (adef_expr h))
                        | (n, s) <- M.toList (dSeqDefs d), Just h <- [M.lookup n (dHwDefs d)] ]
    alias defs i = case adef_expr <$> M.lookup (getIdString i) defs of
        Just e@(ASDef {}) -> Just e
        Just e@(ASPort {}) -> Just e
        Just e@(ASInt {}) -> Just e
        _ -> Nothing
    eqE s h = case (s, h) of
        (ASAny _ (Just v), _) -> eqE v h
        (_, ASAny _ (Just w)) -> eqE s w
        (ASDef _ i, ASDef _ j) | getIdString i == getIdString j -> ML.findWithDefault False (getIdString i) table
        (ASDef _ i, _) | Just s' <- alias (dSeqDefs d) i -> eqE s' h
        (_, ASDef _ j) | Just h' <- alias (dHwDefs d) j -> eqE s h'
        (AMethCall {}, _) -> readIs s 0 h
        (ATupleSel _ r@(AMethCall {}) k, _) -> readIs r (fromInteger k - 1) h
        (APrim _ t op xs, APrim _ t' op' ys) -> op == op' && t == t' && all2 xs ys
        (ATupleSel _ x k, ATupleSel _ y k') -> k == k' && eqE x y
        (ATuple _ xs, ATuple _ ys) -> all2 xs ys
        (AFunCall _ _ f _ xs, AFunCall _ _ g _ ys) -> f == g && all2 xs ys
        _ -> leaf s && s == h
    all2 xs ys = length xs == length ys && and (zipWith eqE xs ys)
    leaf e = case e of
        ASInt {} -> True; ASStr {} -> True; ASPort {} -> True; ASParam {} -> True
        ASAny _ Nothing -> True
        _ -> False
    -- a read without arguments, and the output wire of its copy
    readIs r k h = case (r, h) of
        (AMethCall _ o m [], ASDef _ j) | not (M.member (getIdString j) (dHwDefs d)) -> wireOf o m r k j
        (AMethCall _ o m [], ASPort _ j) -> wireOf o m r k j
        _ -> False
    wireOf o m r k j =
        let inst = getIdString o
            port = lookupMeth d inst (getIdBaseString m) >>= \mt -> readPortFor d mt inst r
        in case (port, M.lookup (getIdString j) (dStateOuts d)) of
            (Just p, Just (p', k')) -> portKey p == portKey p' && k == k'
            _ -> False

-- ----------------------------------------------------------------------------
-- ** Disjointness facts bsc relied on but did not record
-- ----------------------------------------------------------------------------
--
-- When the execution order puts a use of one method after a use that must
-- not precede it, the scheduler accepted the order for one of three
-- reasons: the two rules block each other (visible in WILL_FIRE), their
-- CAN_FIRE conditions are disjoint (recorded in the ExclusiveRulesDB), or
-- the two uses' conditions are disjoint (not recorded).  For the last kind,
-- the solver bsc used is asked again, set up with the definitions and rule
-- predicates the scheduler gave it; only facts it confirms become hypotheses.
