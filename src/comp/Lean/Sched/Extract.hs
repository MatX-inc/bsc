{-# LANGUAGE LambdaCase, MagicHash, BangPatterns #-}
-- | Phase 1: the Design, from the .ba and aState.
module Lean.Sched.Extract where

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
import Lean.Sched.OutOfOrder

-- | Build the 'Design', or say why the module is out of scope.
extractDesign :: FilePath -> String -> ABinModInfo -> ASPackage -> Either String Design
extractDesign path version abmi aspkg = do
    let apkg = abmi_apkg abmi
        asi  = abmi_aschedinfo abmi
        name = getIdString (apkg_name apkg)
    when (apkgIsMCD apkg) $
        Left (name ++ ": modules with several clocks or resets are not supported")
    when (not (null (apkg_size_params apkg))) $
        Left (name ++ ": modules with size parameters are not supported")
    let children  = M.fromList [ (chInst c, c) | c <- map extractChild (aspkg_state_instances aspkg) ]
        ifc       = apkg_interface apkg
        defs0     = [ aif_value f | f@AIDef {} <- ifc ] ++ [ aif_value f | f@AIActionValue {} <- ifc ]
                    ++ apkg_local_defs apkg
        -- The .ba drops the conditions under which each method use matters
        -- (GenABin's Bin UseCond), so they are recomputed as the scheduler
        -- computed them, by AUses.buildUseMaps.  Its definitions are named
        -- apart from the package's ('renameSchedulerUses' made room).
        -- WILL_FIRE (and through it RDY) is scheduling logic the scheduler
        -- itself added; to the use analysis, as to the scheduler, it is opaque
        opaqueFires = [ if "WILL_FIRE_" `isPrefixOf` getIdString i then ADef i t (ASPort t i) ps else df
                      | df@(ADef i t _ ps) <- defs0 ]
        (ruleUses, _, newDefs) = buildUseMaps opaqueFires (scheduledRules apkg) (apkg_state_instances apkg)
        taken     = map (getIdString . adef_objid) defs0
        prefix    = freePrefix "use_cond_" taken
        newIds    = M.fromList [ (i, setIdBaseString i (prefix ++ show k))
                               | (k, i) <- zip [1 :: Int ..] (reverse (map adef_objid newDefs)) ]
        rename    = aSubst (M.fromList [ (i, ASDef t (newIds M.! i)) | ADef i t _ _ <- newDefs ])
        useDefs   = [ ADef (newIds M.! i) t (rename e) p | ADef i t e p <- newDefs ]
        -- each rule's value-method uses, with the condition each matters under
        useConds  = M.map (\(_, objs) -> [ (o, m, e, rename (extractCondition u))
                                         | (o, meths) <- M.toList objs, (m, uses) <- M.toList meths
                                         , u@(UUExpr e _) <- uses ])
                          (rumToMethodUseMap ruleUses)
        seqDefs   = M.fromList [ (getIdString (adef_objid d), d) | d <- defs0 ++ useDefs ]
        hwDefs    = M.fromList [ (getIdString (adef_objid d), d) | d <- aspkg_values aspkg ]
        clockish  = S.fromList (concatMap clockResetIds (apkg_inputs apkg))
        inputs    = [ (getIdString i, t) | (i, t) <- aspkg_inputs aspkg
                                         , not (getIdString i `S.member` clockish) ]
        -- the outputs of methods; exported clocks, gates and resets are wires,
        -- not values any step computes
        methodOuts = S.fromList [ getIdString n | f <- ifc, isMethodField f
                                                , n <- mkNamedOutputs (aif_fieldinfo f) ]
        outputs   = [ (getIdString i, t) | (i, t) <- aspkg_outputs aspkg, getIdString i `S.member` methodOuts ]
        otherOuts = [ getIdString i | (i, _) <- aspkg_outputs aspkg, not (getIdString i `S.member` methodOuts) ]
        stateOuts = M.fromList [ (sig, (p, k))
                               | c <- M.elems children, m <- M.elems (chMethods c)
                               , p <- mtCopies m, (k, (sig, _)) <- zip [0 ..] (ptResults p) ]
        useCopy   = valueCopyTable asi
        callCopy  = actionCopyTable asi
        userME    = nubOrd [ (getIdString a, getIdString b)
                           | SPMutuallyExclusive groups <- apkg_schedule_pragmas apkg
                           , (ga, k) <- zip groups [0 :: Int ..], gb <- drop (k + 1) groups
                           , a <- ga, b <- gb ]
        userMESet = S.fromList userME
        isUserME a b = S.member (a, b) userMESet || S.member (b, a) userMESet
        disjoint  = nubOrd [ (min a b, max a b)
                           | (r1, (ds, _)) <- erdbToList (asi_exclusive_rules_db asi)
                           , r2 <- ds
                           , let a = getIdString r1, let b = getIdString r2
                           , a /= b, not (isUserME a b) ]
        blocks    = S.fromList [ (getIdString r, getIdString b)
                               | ASchedEsposito rs <- asch_scheduler (asi_schedule asi)
                               , (r, bs) <- rs, b <- bs ]
        exported  = methodConflictInfo (asi_v_sched_info asi)
        names     = map (\(a, b) -> (getIdString a, getIdString b))
        order     = reverse (asch_rev_exec_order (asi_schedule asi))
        hwOutSet  = S.fromList (map fst outputs)
        -- the ports of each method, by name: (* enable = "..." *) and
        -- (* ready = "..." *) rename them, and always_ready drops the ready
        methodSigs = aspsi_methods (aspkg_signal_info aspkg)
        inputSet  = S.fromList (map fst inputs)
        enables   = M.fromList [ (getIdString (aspm_name m), en) | m <- methodSigs
                               , Just en <- [fmap getIdString (aspm_menableid m)]
                               , en `S.member` inputSet ]
        ready     = M.fromList [ (getIdString (aspm_name m), rdy) | m <- methodSigs
                               , Just rdy <- [fmap getIdString (aspm_mrdyid m)]
                               , rdy `S.member` hwOutSet ]
        exportedC = [ p | p@(a, b) <- names (sC exported), a /= b ]
        -- the RDY fields of the interface, by name, in interface order
        rdyIfcMap = M.fromListWith (++) [ (aif_name g, [g]) | g@AIDef {} <- reverse ifc ]
        -- what each step needs to find its rule or method
        ruleMap   = M.fromListWith (\_ old -> old) [ (arule_id r, r) | r <- apkg_rules apkg ]
        methodBySched = M.fromListWith (\_ old -> old)
                            [ (n, (f, body)) | f <- ifc, isMethodField f
                                             , let body = M.fromListWith (\_ old -> old)
                                                              [ (arule_id r, r) | r <- bodyOf f ]
                                             , n <- aif_name f : aIfaceSchedNames f ]
        partial   = Design
            { dModule = name, dSource = path, dVersion = version
            , dPackage = abmi_src_name abmi
            , dChildren = children
            , dChildOrder = map (getIdString . avi_vname) (aspkg_state_instances aspkg)
            , dSteps = [], dStepMap = M.empty, dMethodSteps = M.empty
            , dSeqDefs = seqDefs, dHwDefs = hwDefs
            , dInputs = M.fromList inputs, dInputOrder = map fst inputs
            , dOutputs = outputs, dStateOuts = stateOuts, dUseCopy = useCopy
            , dDisjoint = disjoint, dDisjointSet = S.fromList disjoint
            , dBlocks = blocks, dUserME = userME
            , dExportedC = exportedC
            -- (reversed, so that each prepended partner leaves them in order)
            , dExportedCBy = M.fromListWith (++) [ (k, [v]) | (a, b) <- reverse exportedC
                                                            , (k, v) <- [(b, a), (a, b)] ]
            , dExportedSB = [ (a, "SB", b) | (a, b) <- names (sSB exported), a /= b ]
                         ++ [ (a, "SBR", b) | (a, b) <- names (sSBR exported), a /= b ]
            , dEnables = enables, dReady = ready
            , dReadyDef = M.fromList [ (getIdString (aif_name f), getIdString (adef_objid (aif_value g)))
                                     | f <- ifc, isMethodField f, not (isRdyId (aif_name f))
                                     , g <- M.findWithDefault [] (mkRdyId (aif_name f)) rdyIfcMap ]
            , dOrders = [], dUnrealized = [], dExportedCF = [], dUseDefs = useDefs
            , dHavocPorts = S.empty, dReadFlags = [], dRelied = []
            , dUnspec = unSpecTo (abmi_flags abmi)
            , dHwTasks = [], dTaskTags = 0
            , dWarnings = [ "outputs not compared (not method results): " ++ unwords otherOuts | not (null otherOuts) ]
                          ++ [ "bsc reported a dynamic execution order (" ++ takeWhile (/= '\n') w ++ ")"
                          | (_, tag, w) <- asi_warnings asi
                          , tag `elem` ["G0096", "G0100", "G0101"] ]
            }
    -- (one partial application, so that its table of definitions is shared)
    let readsIn = exprReads partial
        lookups = StepLookups { slRule = ruleMap, slMethod = methodBySched, slRdy = rdyIfcMap
                              , slReads = readsIn, slDPI = useDPI (abmi_flags abmi) }
    steps0 <- concat <$> mapM (extractStep partial lookups callCopy useConds hwOutSet) order
    let (steps, nTags) = numberTasks steps0
        hwTasks = matchTasks steps (hardwareTasks aspkg)
        names' = map stName steps
        -- (reversed, so that each prepended step leaves them in order)
        stepsMap = M.fromListWith (++) [ (m, [stName st]) | st <- reverse steps, Just m <- [stMethod st] ]
        stepsOf m = M.findWithDefault [] m stepsMap
        cfs = nubOrd [ (min a b, max a b) | (a, b) <- names (sCF exported), a /= b
                                          , not (null (stepsOf a)), not (null (stepsOf b)) ]
        execEdges = nubOrd [ (getIdString p, getIdString r)
                           | (Exec r, preds) <- asi_sched_graph asi, Exec p <- preds ]
    (orders, unrealized) <- ordersOf names' execEdges stepsOf cfs
    let swapNotes = [ "the exported " ++ a ++ " CF " ++ b ++ " cannot be realized without breaking "
                      ++ "another scheduling constraint; that swap is not checked" | (a, b) <- unrealized ]
        stepMapOf ss = M.fromListWith (\_ old -> old) [ (stName st, st) | st <- ss ]
        ordered = partial { dSteps = steps, dStepMap = stepMapOf steps, dMethodSteps = stepsMap
                          , dOrders = orders, dUnrealized = unrealized, dExportedCF = cfs
                          , dHwTasks = hwTasks, dTaskTags = nTags }
        (sited, havoc, flags, relied) = havocSites ordered
    return ordered { dSteps = sited, dStepMap = stepMapOf sited
                   , dHavocPorts = havoc, dReadFlags = flags, dRelied = relied
                   , dWarnings = dWarnings partial ++ swapNotes }
  where
    clockResetIds (AAI_Clock o g) = getIdString o : maybe [] (pure . getIdString) g
    clockResetIds (AAI_Reset r)   = [getIdString r]
    clockResetIds _               = []

-- | bsc's scheduler leaves the definitions it made for use conditions
-- (@__duses1@, ...) in the package, and rules may refer to them (the checks
-- of @conflict_free@ do).  'buildUseMaps', run again, numbers its own from
-- @__duses1@ too, and reuses existing definitions of equal expressions, so
-- the scheduler's are renamed apart before it runs.
renameSchedulerUses :: APackage -> APackage
renameSchedulerUses apkg
    | null old  = apkg
    | otherwise = apkg { apkg_local_defs = map renameDef (apkg_local_defs apkg)
                       , apkg_rules      = aSubst sub (apkg_rules apkg)
                       , apkg_interface  = aSubst sub (apkg_interface apkg) }
  where
    isOld i = "__duses" `isPrefixOf` getIdString i
    old     = [ d | d <- apkg_local_defs apkg, isOld (adef_objid d) ]
    taken   = map (getIdString . adef_objid) (apkg_local_defs apkg)
    prefix  = freePrefix "bsc_uses_" taken
    newId i = setIdBaseString i (prefix ++ drop (length "__duses") (getIdString i))
    sub     = M.fromList [ (i, ASDef t (newId i)) | ADef i t _ _ <- old ]
    renameDef (ADef i t e p) = ADef (if isOld i then newId i else i) t (aSubst sub e) p

-- | The first of @base@, @_base@, @__base@, ... that no name starts with
-- (@base@ itself starting with something else than @_@).  A name blocks
-- the one with as many underscores as it has before @base@.
freePrefix :: String -> [String] -> String
freePrefix base names = replicate (until (`S.notMember` blocked) (+ 1) 0) '_' ++ base
  where blocked = S.fromList [ length us | n <- names, let (us, rest) = span (== '_') n
                                         , base `isPrefixOf` rest ]

-- | The rules of an action (or action-value) method's body.
bodyOf :: AIFace -> [ARule]
bodyOf f = case f of { AIAction { aif_body = rs } -> rs; AIActionValue { aif_body = rs } -> rs; _ -> [] }

-- | An interface field that is a method (not a clock, reset or inout).
isMethodField :: AIFace -> Bool
isMethodField f = case f of { AIDef {} -> True; AIAction {} -> True; AIActionValue {} -> True; _ -> False }

-- | Rules and methods as the scheduler sees them, for 'buildUseMaps'
-- (the same conversion as 'ASchedule.cvtARule' and 'ASchedule.cvtIfc').
-- The checks of assumptions are left out: the scheduler ignores a rule's
-- assumptions, which become actions only after it ran ('ARemoveAssumps').
scheduledRules :: APackage -> [Rule]
scheduledRules apkg =
    concatMap fromIfc (apkg_interface apkg)
    ++ [ Rule (arule_id r) (arule_parent r) [arule_pred r] [] (acts r) | r <- apkg_rules apkg ]
  where
    acts r = filter (not . isAssumptionCheck) (arule_actions r)
    fromIfc f = case f of
        AIAction { aif_pred = p, aif_body = rs } ->
            [ Rule (arule_id r) (arule_parent r) [p, arule_pred r] [p, arule_pred r] (acts r) | r <- rs ]
        AIActionValue { aif_pred = p, aif_body = rs, aif_value = ADef dId t _ _ } ->
            [ Rule (arule_id r) (arule_parent r) [p, arule_pred r] [p, arule_pred r, ASDef t dId] (acts r)
            | r <- rs ]
        AIDef { aif_name = m, aif_pred = p, aif_value = ADef dId t _ _ }
            | not (isRdyId m) -> [ Rule m Nothing [p] [p, ASDef t dId] [] ]
        _ -> []

-- | A @$error@ (or other task) that checks one of bsc's run-time
-- assumptions, made from an assumption by 'ARemoveAssumps'.
isAssumptionCheck :: AAction -> Bool
isAssumptionCheck a = case a of
    AFCall {}      -> aact_assump a
    ATaskAction {} -> aact_assump a
    _              -> False

-- | The methods of a child instance, from its 'VModInfo' and from the
-- per-method types and used-copy counts that 'aState' left on the 'AVInst'.
extractChild :: AVInst -> Child
extractChild avi = Child
    { chInst    = inst
    , chModule  = getVNameString (vName vmi)
    , chMethods = M.fromList [ (mtName m, m) | m <- meths ]
    , chOrder   = map mtName meths
    , chConf    = conf
    , chSched   = ppString (methodConflictInfo (vSched vmi))
    , chAlias   = M.fromList [ (sig, canon) | (port, sig) <- valueOuts
                                            , Just canon <- [M.lookup port firstSig] ]
    , chWires   = [ (o, i) | (vo, vi) <- wireEqs, Just o <- [signalOf vo], Just i <- [signalOf vi] ]
    }
  where
    -- the Verilog of bsc's wire primitives (the back end inlines it)
    wireEqs | isRWire avi      = [("WGET", "WVAL"), ("WHAS", "WSET")]
            | isRWire0 avi     = [("WHAS", "WSET")]
            | isBypassWire avi = [("WGET", "WVAL")]
            | otherwise        = []
    -- the signal on a Verilog port, for the first copy of its method in use
    signalOf vp = listToMaybe
        [ sig | (f, _) <- fields, Just mt <- [lookupM f], p <- take 1 (mtCopies mt)
              , (vp', sig) <- zip (map (getVNameString . fst) (vf_outputs f)) (map fst (ptResults p))
                              ++ zip (map (getVNameString . fst) (concat (vf_inputs f))) (map fst (ptArgs p))
                              ++ [ (getVNameString en, e) | Just (en, _) <- [vf_enable f], Just e <- [ptEnable p] ]
              , vp' == vp ]
    -- (Verilog output port, result wire) of each argument-free value method
    valueOuts = [ (getVNameString vp, sig)
                | (f, (argTys, Nothing, _)) <- fields, null argTys, Just mt <- [lookupM f]
                , p <- take 1 (mtCopies mt), ((vp, _), (sig, _)) <- zip (vf_outputs f) (ptResults p) ]
    -- the first wire on each Verilog port, as 'lookup' would find it
    firstSig = M.fromListWith (\_ old -> old) valueOuts
    lookupM f = M.lookup (getIdBaseString (vf_name f)) methMap
    methMap = M.fromListWith (\_ old -> old) [ (mtName m, m) | m <- meths ]
    inst  = getIdString (avi_vname avi)
    vmi   = avi_vmi avi
    mci   = methodConflictInfo (vSched vmi)
    -- (the tables cover only the methods in use: no use of another is ever
    -- in an index 'conflicting' looks in)
    conf  = conflictTables [ mtName m | m <- meths, not (null (mtCopies m)) ] Conflicts
        { cfCF  = S.fromList (concat [ [(a, b), (b, a)] | (a, b) <- pairs (sCF mci) ])
        , cfSB  = S.fromList (pairs (sSB mci))
        , cfSBR = S.fromList (pairs (sSBR mci))
        , cfNoFollow = M.empty, cfNoPrecede = M.empty, cfNoShare = M.empty }
    pairs = map (\(a, b) -> (getIdBaseString a, getIdBaseString b))
    used  = M.fromList [ (getIdBaseString m, n) | (m, n) <- avi_iarray avi ]
    -- 'avi_meth_types' has an entry per field of the VModInfo, as in 'aState'
    fields = [ (f, ty) | (f@Method {}, ty) <- zip (vFields vmi) (avi_meth_types avi) ]
    paths  = let VPathInfo ps = vPath vmi in ps
    -- the input ports with a combinational path to each output port
    pathsTo = M.fromListWith (++) [ (o, [i]) | (i, o) <- paths ]
    -- the methods each input port belongs to (all fields of a name)
    inputOwners = M.fromListWith (++) [ (i, [getIdBaseString (vf_name g)]) | (g, _) <- fields, i <- portsIn g ]
    -- the methods each method is SB or SBR of
    sbOf = M.fromListWith (++) [ (b, [a]) | (a, b) <- S.toList (cfSB conf) ++ S.toList (cfSBR conf) ]
    -- where each action method comes in 'actionNames'
    actionPos = M.fromListWith (++) [ (a, [k]) | (k, a) <- zip [0 :: Int ..] actionNames ]
    portsIn f  = map fst (concat (vf_inputs f)) ++ maybe [] (pure . fst) (vf_enable f)
    portsOut f = map fst (vf_outputs f)
    meths = [ mkMeth f ty | (f, ty) <- fields ]
    actionNames = [ getIdBaseString (vf_name f) | (f, (_, Just _, _)) <- fields ]
    mkMeth f (argTys, enTy, resTys) =
        let m     = getIdBaseString (vf_name f)
            kind | isJust enTy && null resTys = ActionMethod
                 | isJust enTy                = ActionValueMethod
                 | otherwise                  = ValueMethod
            nUsed = M.findWithDefault 0 m used
            ino k = if vf_mult f > 1 then Just k else Nothing
            sig k part = getIdString (mkMethId (avi_vname avi) (vf_name f) (ino k) part)
            port k = Port
                { ptInst = inst, ptMeth = m, ptCopy = ino k, ptKind = kind
                , ptEnable  = fmap (const (sig k MethodEnable)) enTy
                , ptArgs    = [ (sig k (MethodArg argN portM), t)
                              | (argN, grp) <- zip [1 ..] argTys
                              , (portM, t) <- zip (splitPortNums grp) grp ]
                , ptResults = [ (sig k (MethodResult pm), t)
                              | (pm, t) <- zip (splitPortNums resTys) resTys ] }
            -- the other action methods it sees: those SB or SBR of it, those
            -- with an input that has a path to one of its outputs, and those
            -- in use that may not follow it (a later call makes what a read
            -- saw arbitrary), in the order of 'actionNames'
            observed = S.delete m (S.fromList (M.findWithDefault [] m sbOf
                                               ++ [ a | o <- portsOut f, i <- M.findWithDefault [] o pathsTo
                                                      , a <- M.findWithDefault [] i inputOwners ]
                                               ++ [ a | kind == ValueMethod, a <- actionNames
                                                      , M.findWithDefault 0 a used > 0, not (mayFollow conf m a) ]))
        in Meth { mtName = m, mtKind = kind, mtCopies = map port [0 .. nUsed - 1]
                , mtDeps = map snd (sortOn fst [ (k, a) | a <- S.toList observed
                                                        , k <- M.findWithDefault [] a actionPos ]) }

-- | The copy of a multi-ported method serving each value-method use, numbered
-- as 'aState' numbers them: the resource allocation table assigns uses to
-- port numbers, and 'aState' renumbers the ports in use densely from 0.
valueCopyTable :: AScheduleInfo -> M.Map (String, String) (M.Map AExpr Integer)
valueCopyTable asi = M.fromList
    [ (k, M.fromList [ (e, c) | (UUExpr e _, c) <- uses ]) | (k, uses) <- denseRat asi ]

-- | The copies serving one method's action calls, by 'callKey': the first
-- copy of each call.
type CallCopies = M.Map CallKey Integer

actionCopyTable :: AScheduleInfo -> M.Map (String, String) CallCopies
actionCopyTable asi = M.fromList
    [ (k, M.fromListWith (\_ old -> old) [ (key, c) | (UUAction a, c) <- uses, Just key <- [callKey a] ])
    | (k, uses) <- denseRat asi ]

-- | The copy serving an action, as 'lookup' over the uses in order would
-- find it: the first use equal to it.
lookupCallCopy :: AAction -> CallCopies -> Maybe Integer
lookupCallCopy a t = callKey a >>= (`M.lookup` t)

-- | A key of a call, equal exactly when the calls are ('=='), so that a
-- table of calls is a map.  The derived 'Ord' of 'AExpr' cannot serve: its
-- 'Eq' ignores some fields (the ids of primitive applications and
-- literals, and a clock's type) that the derived 'Ord' compares.  A call
-- that is not equal to itself (a NaN argument), or not an 'ACall', equals
-- no use and has no key.
data CallKey = CallKey AId AId [ExprKey]
    deriving (Eq, Ord)

data ExprKey
    = EKPrim AType PrimOp [ExprKey] | EKMeth AType AId AId [ExprKey] | EKValue AType AId AId
    | EKTuple AType [ExprKey] | EKSel AType ExprKey Integer
    | EKNoInline AType AId ANoInlineFun [ExprKey] | EKFun AType AId String Bool [ExprKey]
    | EKTask AType AId String Bool Integer
    | EKPort AType AId | EKParam AType AId | EKDef AType AId
    | EKInt AType Integer | EKReal AType Double | EKStr AType String
    | EKAny AType (Maybe ExprKey) | EKClock ExprKey ExprKey | EKReset ExprKey | EKInout ExprKey
    | EKGate AType AId AId
    deriving (Eq, Ord)

callKey :: AAction -> Maybe CallKey
callKey a = case a of
    ACall o m args -> CallKey o m <$> mapM key args
    _ -> Nothing
  where key e = case e of
            APrim _ t op xs -> EKPrim t op <$> mapM key xs
            AMethCall t o m xs -> EKMeth t o m <$> mapM key xs
            AMethValue t o m -> Just (EKValue t o m)
            ATuple t xs -> EKTuple t <$> mapM key xs
            ATupleSel t x k -> (\kx -> EKSel t kx k) <$> key x
            ANoInlineFunCall t o f xs -> EKNoInline t o f <$> mapM key xs
            AFunCall t o f isC xs -> EKFun t o f isC <$> mapM key xs
            ATaskValue t o f isC n -> Just (EKTask t o f isC n)
            ASPort t i -> Just (EKPort t i)
            ASParam t i -> Just (EKParam t i)
            ASDef t i -> Just (EKDef t i)
            ASInt _ t il -> Just (EKInt t (ilValue il))
            ASReal _ t r | isNaN r -> Nothing
                         | otherwise -> Just (EKReal t r)
            ASStr _ t str -> Just (EKStr t str)
            ASAny t me -> EKAny t <$> mapM key me
            ASClock _ (AClock osc gate) -> EKClock <$> key osc <*> key gate
            ASReset _ (AReset w) -> EKReset <$> key w
            ASInout _ (AInout w) -> EKInout <$> key w
            AMGate t o c -> Just (EKGate t o c)

denseRat :: AScheduleInfo -> [((String, String), [(UniqueUse, Integer)])]
denseRat asi =
    [ ((getIdString o, getIdBaseString m), [ (u, rank M.! p) | (u, p) <- uses ])
    | (MethodId o m, uses) <- ratToNestedLists (asi_resource_alloc_table asi)
    , let rank = M.fromList (zip (S.toAscList (S.fromList (map snd uses))) [0 ..]) ]

-- | What 'extractStep' looks up, built once for all the steps.
data StepLookups = StepLookups
    { slRule   :: M.Map ARuleId ARule      -- ^ each rule by id (the first, if repeated)
    , slMethod :: M.Map ARuleId (AIFace, M.Map ARuleId ARule)
      -- ^ the method field each scheduled name belongs to (its own name, or
      -- a rule of its body), the first in interface order, and the rules of
      -- its body by id (the first, if repeated)
    , slRdy    :: M.Map AId [AIFace]       -- ^ the value fields of each name (RDY)
    , slReads  :: AExpr -> S.Set Port      -- ^ 'exprReads', its table shared
    , slDPI    :: Bool                     -- ^ bsc's @-use-dpi@: how Verilog names imported functions
    }

-- | One entry of the logical execution order, or nothing for names that are
-- not scheduled entities of their own (the RDY methods).
extractStep :: Design -> StepLookups -> M.Map (String, String) CallCopies
            -> M.Map ARuleId [(AId, AId, AExpr, AExpr)]
            -> S.Set String -> ARuleId -> Either String [Step]
extractStep d lks callCopy useConds hwOuts sid =
    case (lookupRule, lookupMethod) of
        (Just r, _) -> pure <$> ruleStep RuleStep Nothing r [] (ruleDescr r)
        (_, Just (f, _)) | isRdyId (aif_name f) -> Right []
        (_, Just (f@AIDef {}, _)) -> pure <$> valueStep f
        (_, Just (f, bodyMap)) -> do
            let body = maybe [] pure (M.lookup sid bodyMap)
                firstBody = [ arule_id r | r <- take 1 (aif_body f) ] == [sid]
                enMask = [ WhenInput en | Just en <- [M.lookup (getIdString (aif_name f)) (dEnables d)] ]
                outs = if firstBody then rdyOutputs f ++ valueOutputs f enMask else []
            case body of
                (r : _) -> pure <$> ruleStep ActionMethodStep (Just (getIdString (aif_name f))) r outs
                                                 ("method " ++ getIdString (aif_name f))
                []      -> Left ("cannot find the body of method " ++ getIdString (aif_name f))
        _ -> Left ("the schedule names an unknown rule or method: " ++ getIdString sid)
  where
    lookupRule   = M.lookup sid (slRule lks)
    lookupMethod = M.lookup sid (slMethod lks)
    ruleDescr r = "rule " ++ arule_descr r ++ posNote (arule_id r)
    rdyIfc f = M.findWithDefault [] (mkRdyId (aif_name f)) (slRdy lks)
    rdyName f = listToMaybe [ n | g <- rdyIfc f, n <- map getIdString (mkNamedOutputs (aif_fieldinfo g))
                                , n `S.member` hwOuts ]
    rdyOutputs f =
        [ Output { ouName = n, ouSeq = adef_expr (aif_value g), ouMask = [] }
        | g <- rdyIfc f, n <- map getIdString (mkNamedOutputs (aif_fieldinfo g)), n `S.member` hwOuts ]
    -- the results of a value or action-value method (an action method has none)
    valueOutputs f mask = case f of
        AIDef { aif_value = v }         -> resultsOf f v mask
        AIActionValue { aif_value = v } -> resultsOf f v mask
        _                               -> []
    resultsOf f (ADef _ t e _) mask =
        let ns = map getIdString (mkNamedOutputs (aif_fieldinfo f))
            es = case (t, e) of
                     (ATTuple ts, ATuple _ xs) -> zip ts xs
                     (ATBit 0, _)              -> []
                     _                         -> [(t, e)]
        in [ Output n ei mask | (n, (_, ei)) <- zip ns es, n `S.member` hwOuts ]
    valueStep f = do
        let n = getIdString (aif_name f)
            -- a value method's result matters when it is ready, and not in a
            -- cycle in which the parent enables a method it exports as C
            mask = maybe [] (pure . WhenOutput) (rdyName f)
                   ++ [ WhenNotInput en | other <- M.findWithDefault [] n (dExportedCBy d)
                                        , Just en <- [M.lookup other (dEnables d)] ]
            outs = rdyOutputs f ++ valueOutputs f mask
        return Step
            { stName = n, stMethod = Just n, stKind = ValueMethodStep
            , stDescr = "value method " ++ n ++ posNote (aif_name f)
            , stFire = getIdString (mkIdWillFire (aif_name f))
            , stCanFire = getIdString (mkIdCanFire (aif_name f))
            , stGuard = Nothing, stCalls = [], stChecks = []
            , stReads = readsOf (map ouSeq outs), stReadSites = found (map ouSeq outs)
            , stTaskReads = [], stSites = [], stTaskSites = [], stTaskCalls = []
            , stOutputs = outs }
    ruleStep kind meth r outs descr = do
        calls <- mapM mkCall [ a | a@ACall {} <- arule_actions r ]
        let n = getIdString (arule_id r)
            checks = [ c | a <- arule_actions r, isAssumptionCheck a, (c : _) <- [aact_args a] ]
            exprs  = arule_pred r : concat [ clCond c : clArgs c | c <- calls ] ++ map ouSeq outs
        return Step
            { stName = n, stMethod = meth, stKind = kind, stDescr = descr
            , stFire = getIdString (mkIdWillFire (arule_id r))
            , stCanFire = getIdString (mkIdCanFire (arule_id r))
            , stGuard = Just (arule_pred r), stCalls = calls, stChecks = checks
            , stReads = readsOf exprs, stReadSites = found exprs
            , stTaskReads = [ (c, args, found (c : args)) | a <- arule_actions r, isTask a, (c : args) <- [aact_args a] ]
            , stSites = [], stTaskSites = []
            , stTaskCalls = mapMaybe (taskCall (slDPI lks)) (arule_actions r)
            , stOutputs = outs }
    isTask a  = case a of { AFCall {} -> not (aact_assump a); ATaskAction {} -> not (aact_assump a); _ -> False }
    mkCall a@(ACall o m (cond : args)) = do
        let inst = getIdString o; meth = getIdBaseString m
        mt <- maybe (Left ("call of unknown method " ++ inst ++ "." ++ meth)) Right (lookupMeth d inst meth)
        port <- case mtCopies mt of
            [p] -> Right p
            ps  -> case maybe Nothing (lookupCallCopy a) (M.lookup (inst, meth) callCopy) of
                       Just k | fromInteger k < length ps -> Right (ps !! fromInteger k)
                       _ -> Left ("cannot tell which copy of " ++ inst ++ "." ++ meth ++ " a call uses")
        let argPorts = concatMap argInputPorts args
        when (length argPorts /= length (ptArgs port)) $
            Left ("a call of " ++ portLabel port ++ " has " ++ show (length argPorts)
                  ++ " argument ports, the method " ++ show (length (ptArgs port)))
        Right Call { clPort = port, clCond = cond, clArgs = argPorts }
    mkCall a = Left ("malformed call " ++ ppString a)
    -- the copies some expressions read, in order
    found exprs = S.toList (S.unions (map (slReads lks) exprs))
    -- the reads of a step, with the conditions bsc's use analysis gives them
    -- (hints only)
    readsOf exprs =
        let fromBsc = M.fromListWith (\a b -> aOrs [a, b])
                [ (p, c)
                | (o, m, e, c) <- M.findWithDefault [] sid useConds
                , let inst = getIdString o, Just mt <- [lookupMeth d inst (getIdBaseString m)]
                , Just p <- [readPortFor d mt inst e] ]
        -- where bsc's use analysis covered the step, a read it left out is one
        -- whose condition it found impossible; elsewhere every read counts
        in if M.member sid useConds then M.toList fromBsc
           else [ (p, aTrue) | p <- found exprs ]

-- | The value-method copies an expression reads, through definitions.
exprReads :: Design -> AExpr -> S.Set Port
exprReads d = fst (readTables d)

-- | 'exprReads', and the copies each definition reads, from one table
-- (made once per design, for every expression they are applied to).
readTables :: Design -> (AExpr -> S.Set Port, String -> S.Set Port)
readTables d = (\e0 -> into e0 S.empty, \n -> ML.findWithDefault S.empty n table)
  where
    -- knot-tied through the definitions, so the table must be lazy
    table = ML.map (\df -> into (adef_expr df) S.empty) (dSeqDefs d)
    -- (each read added to a set as it is met, rather than a set made of
    -- each subexpression)
    into e acc = case e of
        AMethCall _ o m args ->
            let inst = getIdString o
                here = lookupMeth d inst (getIdBaseString m) >>= \mt -> readPortFor d mt inst e
            in foldr into (maybe acc (`S.insert` acc) here) args
        ATupleSel _ x _   -> into x acc
        ATuple _ xs       -> foldr into acc xs
        ASAny _ (Just v)  -> into v acc
        APrim _ _ _ xs    -> foldr into acc xs
        AFunCall { ae_args = xs } -> foldr into acc xs
        ANoInlineFunCall { ae_args = xs } -> foldr into acc xs
        ASDef _ i
            -- a WILL_FIRE signal is read at the hardware's view, not by the step
            | "WILL_FIRE_" `isPrefixOf` getIdString i -> acc
            | otherwise -> S.union (M.findWithDefault S.empty (getIdString i) table) acc
        _                 -> acc

posNote :: AId -> String
posNote i = let p = ppString (getIdPosition i)
            in if null p || p == "Unknown position" then "" else " (" ++ p ++ ")"

-- | A rule's system-task or foreign-function call, named as 'aState' names
-- it for Verilog ('vNameToTask' for an imported C function).
taskCall :: Bool -> AAction -> Maybe TaskCall
taskCall dpi a = case a of
    AFCall { aact_objid = i, afcall_fun = f, afcall_isC = isC, aact_args = c : args, aact_assump = chk } ->
        Just (mk i f isC c args chk)
    ATaskAction { aact_objid = i, ataskact_fun = f, ataskact_isC = isC, aact_args = c : args, aact_assump = chk } ->
        Just (mk i f isC c args chk)
    _ -> Nothing
  where
    mk i f isC c args chk = TaskCall { tcName = if isC then vNameToTask dpi f else f
                                     , tcId = getIdString i ++ posNote i
                                     , tcCond = c, tcArgs = args, tcCheck = chk, tcTag = 0, tcPos = posNote i }

-- | Number the steps' task calls in the schedule's order, from 0.
numberTasks :: [Step] -> ([Step], Int)
numberTasks = go 0
  where
    go n [] = ([], n)
    go n (st : rest) =
        let k = length (stTaskCalls st)
            st' = st { stTaskCalls = [ tc { tcTag = n + j } | (j, tc) <- zip [0 ..] (stTaskCalls st) ] }
            (rest', n') = go (n + k) rest
        in (st' : rest', n')

-- | The 'ASPackage''s system-task calls in the order Verilog prints them
-- ('vForeignBlock'), each condition WILL_FIRE AND'ed with the call's own.
hardwareTasks :: ASPackage -> [HwTask]
hardwareTasks aspkg =
    [ HwTask { htName = afc_fun fc
             , htId = getIdString (afc_name fc) ++ posNote (afc_name fc)
             , htStep = stepOf c, htCond = c, htArgs = args, htTag = Nothing }
    | (_, fcs) <- aspkg_foreign_calls aspkg, fc <- fcs, (c : args) <- [afc_args fc] ]
  where
    stepOf c = case c of
        ASDef _ i | Just r <- fireOf i -> Just r
        APrim _ _ PrimBAnd (ASDef _ i : _) | Just r <- fireOf i -> Just r
        _ -> Nothing
    fireOf i = let n = getIdString i
               in if "WILL_FIRE_" `isPrefixOf` n then Just (drop (length "WILL_FIRE_") n) else Nothing

-- | Each hardware call's sequential call: the next unmatched one of the same
-- step, task and id.  The match only chooses what the theorem compares.
matchTasks :: [Step] -> [HwTask] -> [HwTask]
matchTasks steps hws = reverse (frth (F.foldl' one (S.empty, byStep0, byId0, []) hws))
  where
    -- (reversed, so that each prepended call leaves the lists in program order)
    calls = reverse [ (stName st, tc) | st <- steps, tc <- stTaskCalls st ]
    byStep0 = M.fromListWith (++) [ ((Just r, tcName tc, tcId tc), [tcTag tc]) | (r, tc) <- calls ]
    byId0 = M.fromListWith (++) [ ((tcName tc, tcId tc), [tcTag tc]) | (_, tc) <- calls ]
    frth (_, _, _, x) = x
    -- (each list keeps what is left of it, so each candidate is passed once)
    one (!used, !byStep, !byId, acc) h = case htStep h of
        Just _ ->
            let key = (htStep h, htName h, htId h)
            in case dropWhile (`S.member` used) (M.findWithDefault [] key byStep) of
                   (k : rest) -> (S.insert k used, M.insert key rest byStep, byId, h { htTag = Just k } : acc)
                   [] -> (used, M.delete key byStep, byId, h : acc)
        Nothing ->
            let key = (htName h, htId h)
            in case dropWhile (`S.member` used) (M.findWithDefault [] key byId) of
                   (k : rest) -> (S.insert k used, byStep, M.insert key rest byId, h { htTag = Just k } : acc)
                   [] -> (used, byStep, M.delete key byId, h : acc)

-- ----------------------------------------------------------------------------
-- ** Reads the annotations rule out
-- ----------------------------------------------------------------------------
--
-- A value method read after a call of the same submodule that it may not
-- follow, or in one rule with a call it may not share a rule with, sees no
-- value the submodule promises: in the sequential semantics it is
-- arbitrary, a universally quantified variable.  Which reads are such
-- ('Site') is decided here from the expressions the steps read and the
-- annotations alone, never from bsc's use conditions; when such a read
-- matters is left to the exact conditions of the calls and to the proof.
