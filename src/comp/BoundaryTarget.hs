-- | Support for "boundary-targeted synthesis": when an import-BVI names a
-- pure-BSV fallback module, the fallback is synthesized with the import's
-- declared VModInfo as a boundary target, so that the fallback's generated
-- boundary (port names, protocol) matches the import's by construction,
-- and anything that cannot be forced (schedule, paths) is checked.
module BoundaryTarget(
    resolveFallbackTargets,
    boundaryTargetPProps,
    targetMethodBetterInfo,
    targetMethodArgNames,
    chkBoundaryTarget
) where

import Data.List(find, sort, intercalate)
import Data.Either(partitionEithers)
import qualified Data.Map as M
import qualified Data.Set as S

import Util(fastNub)
import Id
import PreIds(idDefaultClock, idDefaultReset)
import Error(EMsg, ErrMsg(..))
import Position(getPosition)
import PFPrint(pfpString)
import VModInfo
import SchedInfo
import Pragma
import ISyntax
import IfcBetterInfo(BetterInfo(..), noMethodInfo)

-- ===============
-- Collecting fallback targets from the elaborated package

-- Find all import-BVI VModInfos in the package that declare a fallback,
-- returning a map from the fallback module (unqualified) to the target
-- VModInfos of the imports naming it.  (A fallback may legitimately be
-- named by more than one import only if the boundaries agree; the caller
-- reports the error otherwise.)
collectFallbackTargets :: IPackage a -> M.Map Id [VModInfo]
collectFallbackTargets (IPackage _ _ _ ds) =
    M.fromListWith (++)
        [ (unQualId fb, [vi])
          | IDef _ _ e _ <- ds,
            vi <- collectE e,
            Just fb <- [vFallback vi] ]
  where
    collectE :: IExpr a -> [VModInfo]
    collectE (ILam _ _ e) = collectE e
    collectE (ILAM _ _ e) = collectE e
    collectE (IAps f _ es) = collectE f ++ concatMap collectE es
    collectE (ICon _ (ICVerilog { vInfo = vi })) = [vi]
    collectE _ = []

-- Resolve the fallback targets of a package: each fallback module maps
-- to the boundary declared by the import(s) naming it.  Errors are
-- reported for a fallback named by imports with differing boundaries,
-- and for imports whose shape targeted synthesis does not support.
resolveFallbackTargets :: IPackage a -> (M.Map Id VModInfo, [EMsg])
resolveFallbackTargets ipkg =
    let groups = M.toList (collectFallbackTargets ipkg)
        -- the module name is not part of the prescribed boundary
        norm vi = vi { vName = VName "" }
        resolve (fb, vis) =
            case (fastNub (map norm vis)) of
              [vi] -> case (chkTargetShape vi) of
                        Nothing -> Right (fb, vi)
                        Just reason ->
                            Left (getPosition fb,
                                  EFallbackUnsupportedImport
                                      (pfpString fb) reason)
              _ -> Left (getPosition fb,
                         EFallbackConflictingImports (pfpString fb))
        (errs, oks) = partitionEithers (map resolve groups)
    in  (M.fromList oks, errs)

-- ===============
-- v1 shape restrictions

-- Returns a reason string if the import's boundary has a shape that
-- targeted synthesis does not (yet) support.
chkTargetShape :: VModInfo -> Maybe String
chkTargetShape vmi
    | length (input_clocks (vClk vmi)) > 1
    = Just "it has more than one input clock"
    | not (null (output_clocks (vClk vmi)))
    = Just "it has output clocks"
    | not (null (ancestorClocks (vClk vmi))) ||
      not (null (siblingClocks (vClk vmi)))
    = Just "it declares clock ancestry relationships"
    | length (input_resets (vRst vmi)) > 1
    = Just "it has more than one input reset"
    | not (null (output_resets (vRst vmi)))
    = Just "it has output resets"
    | any isInout (vArgs vmi)
    = Just "it has inout arguments"
    | not (null [ f | f <- vFields vmi, not (isMethodField f) ])
    = Just "its interface has clock, reset, or inout fields"
    | not (null [ f | Method { vf_mult = m } <- vFields vmi, f <- [m], f /= 1 ])
    = Just "it has multi-ported methods"
    | let mci = methodConflictInfo (vSched vmi)
      in not (null (sME mci)) || not (null (sEXT mci))
    = Just "it has ME or EXT scheduling annotations"
    | not (null (rulesBetweenMethods (vSched vmi))) ||
      not (null (rulesBeforeMethods (vSched vmi))) ||
      not (null (clockCrossingMethods (vSched vmi)))
    = Just "it has rule-ordering or clock-crossing scheduling annotations"
    | otherwise
    = Nothing
  where
    isMethodField (Method {}) = True
    isMethodField _ = False

-- ===============
-- Targeting: pragmas for the fallback's default clock and reset,
-- and always_ready/always_enabled shapes

-- These use the existing pragma forms, so that the evaluator's existing
-- boundary-naming machinery applies them; only method port names (which
-- have no module-level pragma form) are overridden directly, via
-- targetMethodBetterInfo and targetMethodArgNames below.
boundaryTargetPProps :: VModInfo -> [PProp]
boundaryTargetPProps vmi = clk_pps ++ rst_pps ++ ar_pps ++ ae_pps
  where
    clk_pps =
        case (input_clocks (vClk vmi)) of
          [] -> [PPclock_osc [(idDefaultClock, "")]]
          [(_, Nothing)] -> [PPclock_osc [(idDefaultClock, "")]]
          [(_, Just (VName osc, gateinfo))] ->
              [PPclock_osc [(idDefaultClock, osc)]] ++
              case gateinfo of
                Left True  -> [PPgate_inhigh [idDefaultClock]]
                Left False -> [PPgate_unused [idDefaultClock]]
                Right (VName g) ->
                    [PPclock_gate [(idDefaultClock, g)],
                     PPgate_input_clocks [idDefaultClock]]
          _ -> []  -- rejected by chkTargetShape
    rst_pps =
        case (input_resets (vRst vmi)) of
          [] -> [PPreset_port [(idDefaultReset, "")]]
          [(_, (Nothing, _))] -> [PPreset_port [(idDefaultReset, "")]]
          [(_, (Just (VName rstn), _))] ->
              [PPreset_port [(idDefaultReset, rstn)]]
          _ -> []  -- rejected by chkTargetShape
    meth_names = [ vf_name f | f@(Method {}) <- vFields vmi ]
    -- methods with no RDY_ method in the boundary are always_ready
    no_rdy_meths = [ m | m <- meth_names,
                         not (isRdyId m),
                         (mkRdyId m) `notElem` meth_names ]
    ar_pps = if null no_rdy_meths
             then []
             else [PPalwaysReady (map (:[]) no_rdy_meths)]
    -- methods whose enable is an inhigh port are always_enabled
    ae_meths = [ vf_name f
                 | f@(Method { vf_enable = Just (_, props) }) <- vFields vmi,
                   VPinhigh `elem` props ]
    ae_pps = if null ae_meths
             then []
             else [PPalwaysEnabled (map (:[]) ae_meths)]

-- ===============
-- Targeting: method port names

-- Build the BetterInfo (result/ready/enable port names) for an interface
-- field from the target's boundary, instead of from interface-decl pragmas.
targetMethodBetterInfo :: VModInfo -> Id -> BetterInfo
targetMethodBetterInfo vmi f =
    let uf = unQualId f in
    case (findMethod vmi f) of
      Nothing -> noMethodInfo uf
      Just m ->
          BetterMethodInfo {
              mi_id = uf,
              mi_result = maybe (id_to_vPort uf) id (vf_output m),
              mi_enable = maybe (id_to_vPort (mkEnableId uf)) id (vf_enable m),
              mi_ready = case (findMethod vmi (mkRdyId uf)) of
                           Just r | Just p <- vf_output r -> p
                           _ -> id_to_vPort (mkRdyId uf),
              mi_prefix = uf
          }

-- The target's argument port names for a method, or Nothing if the
-- method is not in the target's boundary.
targetMethodArgNames :: VModInfo -> Id -> Maybe [String]
targetMethodArgNames vmi f =
    fmap (map getVPortString . vf_inputs) (findMethod vmi f)

findMethod :: VModInfo -> Id -> Maybe VFieldInfo
findMethod vmi f =
    let isMeth m@(Method {}) = qualEq (vf_name m) (unQualId f)
        isMeth _ = False
    in  find isMeth (vFields vmi)

-- ===============
-- The final checks, in the post-schedule window of module generation:
-- 1. the produced boundary structures must equal the target's (modulo
--    module name), since the parent's shared instantiation is built
--    from the target and the fallback's own Verilog/Bluesim artifacts
--    are built from the produced structures;
-- 2. the produced schedule must refine the target's (the fallback may
--    conflict less, never more);
-- 3. the produced combinational paths must be a subset of the target's.

chkBoundaryTarget :: Id ->        -- the fallback module being generated
                     VModInfo ->  -- the target (the import's VModInfo)
                     VWireInfo -> [VFieldInfo] -> VSchedInfo -> VPathInfo ->
                     [EMsg]
chkBoundaryTarget mod_id target wireinfo fieldinfo schedinfo pathinfo =
    let pos = getPosition mod_id
        mod_str = pfpString (unQualId mod_id)
        boundary_diffs =
            chkClkBoundary (vClk target) (wClk wireinfo) ++
            chkRstBoundary (vRst target) (wRst wireinfo) ++
            chkArgBoundary (vArgs target) (wArgs wireinfo) ++
            chkFieldBoundary (vFields target) fieldinfo
        sched_diffs = chkSchedRefinement (vSched target) schedinfo
        path_diffs = chkPathRefinement (vPath target) pathinfo
    in  (if null boundary_diffs then []
         else [(pos, EFallbackBoundaryMismatch mod_str boundary_diffs)]) ++
        (if null sched_diffs then []
         else [(pos, EFallbackScheduleMismatch mod_str sched_diffs)]) ++
        (if null path_diffs then []
         else [(pos, EFallbackPathMismatch mod_str path_diffs)])

-- compare input clock ports (osc port name and gate shape), ignoring the
-- boundary-internal clock names
chkClkBoundary :: VClockInfo -> VClockInfo -> [String]
chkClkBoundary tgt gen =
    let ports ci = sort [ (getVNameString osc, fmap getVNameString g)
                          | (_, Just (osc, g)) <- input_clocks ci ]
        fmt (osc, mg) = osc ++
                        case mg of
                          Left True -> " (gate inhigh)"
                          Left False -> " (gate unused)"
                          Right g -> " (gate " ++ g ++ ")"
        tgt_ps = ports tgt
        gen_ps = ports gen
    in  if tgt_ps == gen_ps
        then []
        else ["clock ports: the import has " ++
              bracket (map fmt tgt_ps) ++
              " but the fallback has " ++ bracket (map fmt gen_ps)]

chkRstBoundary :: VResetInfo -> VResetInfo -> [String]
chkRstBoundary tgt gen =
    let ports ri = sort [ getVNameString p
                          | (_, (Just p, _)) <- input_resets ri ]
        tgt_ps = ports tgt
        gen_ps = ports gen
    in  if tgt_ps == gen_ps
        then []
        else ["reset ports: the import has " ++ bracket tgt_ps ++
              " but the fallback has " ++ bracket gen_ps]

-- compare the parameter/port argument sequences (relative order matters,
-- for positional parameter instantiation in Verilog-95 mode and for
-- positional argument binding in Bluesim)
chkArgBoundary :: [VArgInfo] -> [VArgInfo] -> [String]
chkArgBoundary tgt gen =
    let fmt (Param n) = "parameter " ++ getVNameString n
        fmt (Port (n,_) _ _) = "port " ++ getVNameString n
        fmt a = pfpString (getVArgInfoName a)
        args as = [ fmt a | a <- as, isParam a || isPort a ]
        tgt_as = args tgt
        gen_as = args gen
    in  if tgt_as == gen_as
        then []
        else ["arguments: the import has " ++ bracket tgt_as ++
              " but the fallback has " ++ bracket gen_as]

chkFieldBoundary :: [VFieldInfo] -> [VFieldInfo] -> [String]
chkFieldBoundary tgt gen =
    let tgt_map = M.fromList [ (unQualId (vf_name m), m)
                               | m@(Method {}) <- tgt ]
        -- the produced fields still contain RDY methods at this stage,
        -- even when the target declares the method always-ready (they are
        -- dropped later, and the always-ready property itself is enforced
        -- separately); ignore them here when the target has no RDY method
        -- but does have the base method
        alwaysRdyDropped r =
            isRdyId r &&
            (r `M.notMember` tgt_map) &&
            or [ mkRdyId b == r | b <- M.keys tgt_map ]
        gen_map = M.fromList [ (unQualId (vf_name m), m)
                               | m@(Method {}) <- gen,
                                 not (alwaysRdyDropped (unQualId (vf_name m))) ]
        missing = M.keys (tgt_map `M.difference` gen_map)
        extra   = M.keys (gen_map `M.difference` tgt_map)
        fmtPort = getVPortString
        -- compare the ports of a method, and the inhigh-ness of enables
        chkMeth (f, (t, g)) =
            let diffs =
                    (if map fmtPort (vf_inputs t) /= map fmtPort (vf_inputs g)
                     then ["argument ports " ++
                           bracket (map fmtPort (vf_inputs t)) ++ " vs " ++
                           bracket (map fmtPort (vf_inputs g))]
                     else []) ++
                    (chkMPort "output" (vf_output t) (vf_output g)) ++
                    (chkMPort "enable" (vf_enable t) (vf_enable g)) ++
                    (if (enInhigh t) /= (enInhigh g)
                     then ["enable inhigh-ness differs"]
                     else [])
            in  map (\ d -> "method " ++ pfpString f ++ ": " ++ d) diffs
        enInhigh m = case (vf_enable m) of
                       Just (_, props) -> VPinhigh `elem` props
                       Nothing -> False
        chkMPort what mt mg =
            if (fmap fmtPort mt) == (fmap fmtPort mg)
            then []
            else [what ++ " port " ++ fmtM mt ++ " vs " ++ fmtM mg]
        fmtM Nothing = "(none)"
        fmtM (Just p) = fmtPort p
        common = M.toList (M.intersectionWith (,) tgt_map gen_map)
    in  map (\ f -> "method " ++ pfpString f ++
                    " is missing from the fallback") missing ++
        map (\ f -> "the fallback has an extra method " ++ pfpString f)
            extra ++
        concatMap chkMeth common

-- The fallback's schedule refines the import's if every pair of methods
-- that the import declares to be usable together (CF, or ordered SB/SBR)
-- is at least as usable in the fallback.
chkSchedRefinement :: VSchedInfo -> VSchedInfo -> [String]
chkSchedRefinement tgt gen =
    let mkSets si =
            let mci = methodConflictInfo si
                unq (a, b) = (unQualId a, unQualId b)
                sym ps = ps ++ [ (b, a) | (a, b) <- ps ]
                cf_set = S.fromList (map unq (sym (sCF mci ++ sP mci)))
                sb_set = cf_set `S.union`
                         S.fromList (map unq (sSB mci))
                sbr_set = sb_set `S.union`
                          S.fromList (map unq (sSBR mci))
            in  (cf_set, sb_set, sbr_set)
        (tgt_cf, tgt_sb, tgt_sbr) = mkSets tgt
        (gen_cf, gen_sb, gen_sbr) = mkSets gen
        fmtPair rel (a, b) = pfpString a ++ " " ++ rel ++ " " ++ pfpString b
        -- self-pairs are not exercised across the boundary: the methods
        -- in scope are single-ported, so a parent cannot invoke one twice
        -- in a cycle regardless of the annotation
        missing rel t g = [ fmtPair rel p
                            | p@(a, b) <- S.toList (t `S.difference` g),
                              a /= b ]
    in  fastNub (missing "CF" tgt_cf gen_cf ++
                 missing "SB" tgt_sb gen_sb ++
                 missing "SBR" tgt_sbr gen_sbr)

-- The fallback must not have combinational input-to-output paths beyond
-- those the import declares (the parent's schedule assumed only those).
chkPathRefinement :: VPathInfo -> VPathInfo -> [String]
chkPathRefinement (VPathInfo tgt) (VPathInfo gen) =
    let mkSet ps = S.fromList [ (getVNameString a, getVNameString b)
                                | (a, b) <- ps ]
        extra = mkSet gen `S.difference` mkSet tgt
    in  [ a ++ " -> " ++ b | (a, b) <- S.toList extra ]

bracket :: [String] -> String
bracket ss = "[" ++ intercalate ", " ss ++ "]"
