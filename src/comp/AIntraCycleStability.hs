-- Intra-cycle stability of expression cones over a module's local
-- defs.  Two consumers share the stable-leaf notion (constants,
-- register reads, pure prim ops over stable operands), and keeping
-- them in one module keeps the definitions from drifting:
--
--   * the -verilog dynamic-module-argument warning
--     (WVDynamicInstArg): a state-instance Port argument is a
--     continuous wire in the generated Verilog, with no scheduling
--     relationship to the rules that compute it — child logic samples
--     the settled end-of-cycle value.  If the argument's cone is
--     intra-cycle stable, atomic semantics hold vacuously; if it can
--     change during the cycle, the submodule observes the value as if
--     all writers preceded all readers — silently outside the atomic
--     model.  (Bluesim sidesteps this by requiring constants,
--     EBSimDynamicArg; the trs backend's future dynamic-module-args
--     support will REFUSE what this warns about, with this same
--     predicate.)
--
--   * dynamic scheduling's guard inlining (aDynSchedGuard in
--     ASchedule.hs, on the -sched-dynamic branch): a guard usable for
--     per-edge alternative selection must inline to a cone of
--     constants, register reads, and pure prim ops —
--     aInlineStableCone below.  Callers there should migrate to this
--     module when that branch lands.

module AIntraCycleStability (
    aWarnDynamicInstArgs,
    aConeUnstableSources,
    aInlineStableCone
    ) where

import qualified Data.Map as M
import qualified Data.Set as S

import Error (EMsg, ErrMsg(..))
import Position (getPosition)
import Id (getIdString, getIdBaseString, isIdCanFire, isIdWillFire)
import VModInfo (vName, getVNameString, isPort, getVArgInfoName)
import ASyntax
import BackendNamingConventions (isRWire, isRWire0, isBypassWire,
                                 isBypassWire0)

-- ===============
-- The -verilog warning: one WVDynamicInstArg per state instance with
-- at least one Port argument whose cone contains a proven-unstable
-- source.  Parameters are elaboration constants and never checked;
-- clocks/resets/inouts are not Port args.
aWarnDynamicInstArgs :: APackage -> [EMsg]
aWarnDynamicInstArgs apkg =
    [ (getPosition (avi_vname avi),
       WVDynamicInstArg (getIdString (avi_vname avi)) bad_args)
    | avi <- apkg_state_instances apkg
    , let bad_args = [ getIdString (getVArgInfoName vai)
                     | (vai, e) <- getInstArgs avi
                     , isPort vai
                     , not (null (aConeUnstableSources apkg e)) ]
    , not (null bad_args) ]

-- ===============
-- Warn-tier classification (v1 policy: zero false positives).  The
-- cone — the argument expression inlined through the module's local
-- defs — is scanned for sources PROVEN to vary within a clock cycle:
--
--   * wire-primitive value reads (RWire / RWire0 / BypassWire /
--     BypassWire0 / CrossingBypassWire instances; PulseWire and
--     DWire reduce to these),
--   * CAN_FIRE / WILL_FIRE references,
--   * ActionValue method results (AMethValue) and ActionValue task
--     results (ATaskValue).
--
-- Cones containing only OPAQUE sources stay silent: value-method
-- calls on user submodules or non-register primitives (a
-- register-backed getter is common and perfectly stable — warning on
-- every such call would be noise), module input ports (the PARENT's
-- instantiation gets its own warning if it feeds them unstably), and
-- clock gates.  This is a known, deliberate v1 gap: completeness is
-- traded for zero false positives.
aConeUnstableSources :: APackage -> AExpr -> [String]
aConeUnstableSources apkg e0 =
    let defmap = M.fromList [ (adef_objid d, adef_expr d)
                            | d <- apkg_local_defs apkg ]
        wireset = S.fromList [ avi_vname avi
                             | avi <- apkg_state_instances apkg
                             , isRWire avi || isRWire0 avi ||
                               isBypassWire avi || isBypassWire0 avi ]

        -- visited defs make the walk linear in the def DAG
        go :: S.Set AId -> AExpr -> (S.Set AId, [String])
        go seen (ASDef _ i)
            | isIdCanFire i || isIdWillFire i =
                (seen, ["rule fire signal " ++ getIdBaseString i])
            | i `S.member` seen = (seen, [])
            | otherwise =
                case (M.lookup i defmap) of
                  Just e -> go (S.insert i seen) e
                  Nothing -> (S.insert i seen, [])
        go seen (AMethCall _ obj meth args)
            | obj `S.member` wireset =
                let (seen', srcs) = goList seen args
                in  (seen', ("wire read " ++ getIdBaseString obj ++ "." ++
                             getIdBaseString meth) : srcs)
            | otherwise = goList seen args  -- opaque call; args still count
        go seen (AMethValue _ obj meth) =
            (seen, ["ActionValue method result " ++ getIdBaseString obj ++
                    "." ++ getIdBaseString meth])
        go seen (ATaskValue { ae_funname = fn }) =
            (seen, ["ActionValue task result " ++ fn])
        go seen (APrim _ _ _ args) = goList seen args
        go seen (ATuple _ args) = goList seen args
        go seen (ATupleSel _ e _) = go seen e
        go seen (ANoInlineFunCall _ _ _ args) = goList seen args
        go seen (AFunCall _ _ _ _ args) = goList seen args
        -- stable or opaque leaves
        go seen (ASPort {}) = (seen, [])
        go seen (ASParam {}) = (seen, [])
        go seen (ASInt {}) = (seen, [])
        go seen (ASReal {}) = (seen, [])
        go seen (ASStr {}) = (seen, [])
        go seen (ASAny {}) = (seen, [])
        go seen (ASClock {}) = (seen, [])
        go seen (ASReset {}) = (seen, [])
        go seen (ASInout {}) = (seen, [])
        go seen (AMGate {}) = (seen, [])

        goList seen [] = (seen, [])
        goList seen (a:as) =
            let (seen', s1) = go seen a
                (seen'', s2) = goList seen' as
            in  (seen'', s1 ++ s2)
    in  snd (go S.empty e0)

-- ===============
-- Inline an expression through the module's local defs to a cone of
-- stable leaves only: integer constants, register reads (RegN /
-- RegUN / RegA), and pure prim ops over stable operands.  Nothing =
-- the cone is not provably stable (or exceeds the size cap: inlining
-- shares nothing, so a heavily shared def DAG could otherwise
-- explode).  This is dynamic scheduling's guard-inlining predicate
-- (aDynSchedGuard); it is intentionally STRICTER than the warning's
-- classification above — a merely opaque source fails it.
aInlineStableCone :: APackage -> AExpr -> Maybe AExpr
aInlineStableCone amod e0 =
    let defmap = M.fromList [ (adef_objid d, adef_expr d)
                            | d <- apkg_local_defs amod ]
        regset = S.fromList [ avi_vname avi
                            | avi <- apkg_state_instances amod
                            , getVNameString (vName (avi_vmi avi))
                                  `elem` ["RegN", "RegUN", "RegA"] ]
        limit = 4096 :: Int

        go :: Int -> AExpr -> Maybe (Int, AExpr)
        go n _ | n > limit = Nothing
        go n (ASDef _ i) =
            case (M.lookup i defmap) of
              Just e -> go (n+1) e
              Nothing -> Nothing
        go n e@(ASInt {}) = Just (n+1, e)
        go n e@(AMethCall _ obj meth [])
            | obj `S.member` regset && getIdBaseString meth == "read"
            = Just (n+1, e)
        go n (APrim aid t op args) = do
            (n', args') <- goList (n+1) args
            return (n', APrim aid t op args')
        go _ _ = Nothing

        goList n [] = Just (n, [])
        goList n (a:as) = do
            (n', a') <- go n a
            (n'', as') <- goList n' as
            return (n'', a' : as')
    in  snd <$> go 0 e0
