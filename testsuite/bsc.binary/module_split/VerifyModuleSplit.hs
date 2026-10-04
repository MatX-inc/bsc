-- Build in the Cabal environment after building bsc-core, then set
-- MODULE_PAIR_VERIFY to the resulting executable when running test.sh:
--   cabal exec -- ghc -package bsc-core -package bsc-ba VerifyModuleSplit.hs -o verify-module-split
--
-- This verifier compares encoded fields because AScheduleInfo has no Eq
-- instance and ASyntax's Eq deliberately ignores some source metadata.
module Main (main) where

import ABin
import AModule
import AScheduleInfo
    ( AScheduleInfo(..), AScheduleErrInfo(..), RuleRelationDB(..)
    , RuleRelationInfo(..), Conflicts(..), defaultRuleRelationship
    , areRulesExclusive, areRulesDisjoint, erdbFromList, erdbToList )
import ASchedulePatch
import AScheduleRelations (deriveExclusiveRulesDB, methodBeforeRuleEdges)
import ASimParams (aInlineSimParams)
import ASyntax
    ( APackage(..), ARule(..), AVInst(..), ADef(..), AExpr(..), AIFace(..)
    , AAction(..), aTrue, aTBool, getInstArgs )
import AUses (MethodId(..), RuleUses(..), UniqueUse(..), UseCond(..), ucTrue)
import BinData (Bin, encode, decode)
import Error (ErrorHandle, initErrorHandle, bsError, bsWarning)
import Flags (Flags, relaxMethodEarliness, unSpecTo, optUndet, stableVerilog)
import FlagsDecode (defaultFlags, decodeFlags)
import FStringCompat (mkFString)
import GenABin (readABinFile)
import GenModule
import Id (Id, getIdString, mkId, unQualId)
import IntLit (ilDec)
import Position (noPosition)
import qualified PhaseConfig as PC
import PreIds (idWSet)
import VModInfo (VFieldInfo(..), isParam, isPort)
import Wires (emptyWireProps)

import Control.Monad (forM_, unless, when)
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (find, sortOn)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs, lookupEnv)
import System.Exit (die)
import System.FilePath (replaceExtension)

sameEncoding :: Bin a => String -> a -> a -> IO ()
sameEncoding label left right = unless (encode left == encode right) $
    die ("different encoded " ++ label)

roundTrip :: Bin a => String -> a -> IO ()
roundTrip label value =
    sameEncoding label value (decode (B.pack (encode value)))

expectLeft :: String -> Either String a -> IO ()
expectLeft _ (Left _) = return ()
expectLeft label (Right _) = die (label ++ " unexpectedly accepted")

-- The artifact schema must not depend on the scheduler's legacy cache,
-- either on success or on the diagnostic-only failure path. A bottom here
-- catches accidental serialization without depending on binary byte tags.
checkScheduleProjection :: AScheduleInfo -> IO ()
checkScheduleProjection info = do
    let withoutCache = info { asi_exclusive_rules_db =
            error "success artifact forced ExclusiveRulesDB" }
        err = AScheduleErrInfo
            { asei_warnings = asi_warnings info
            , asei_errors = []
            , asei_method_uses_map = asi_method_uses_map info
            , asei_rule_uses_map = asi_rule_uses_map info
            , asei_resource_alloc_table = Just (asi_resource_alloc_table info)
            , asei_exclusive_rules_db =
                error "failure artifact forced ExclusiveRulesDB"
            , asei_sched_order = Just (asi_sched_order info)
            , asei_schedule = Just (asi_schedule info)
            , asei_sched_graph = Just (asi_sched_graph info)
            , asei_rule_relation_db = Just (asi_rule_relation_db info)
            , asei_v_sched_info = Just (asi_v_sched_info info)
            }
    sameEncoding "schedule projection without legacy cache"
        (toBSchedInfo info) (toBSchedInfo withoutCache)
    roundTrip "success schedule schema" (toBSchedInfo withoutCache)
    roundTrip "failure schedule schema" (toBSchedErrInfo err)
    case asei_exclusive_rules_db (fromBSchedErrInfo (toBSchedErrInfo err)) of
        Nothing -> return ()
        Just _ -> die "failure schedule restored a legacy cache"
    checkUseConditionEncoding info

-- Unlike the completed .ba representation, a schedule still needs expression
-- use conditions to build conflict-free checks. Exercise every condition
-- component, both predicate/read maps (including foreign functions), method
-- use lists, and RAT keys on the successful and failed schedule paths.
checkUseConditionEncoding :: AScheduleInfo -> IO ()
checkUseConditionEncoding info = do
    let ident = mkId noPosition . mkFString
        rule = ident "condition_rule"
        object = ident "condition_object"
        method = ident "condition_method"
        foreignFunction = ident "condition_function"
        expr name = ASPort aTBool (ident name)
        useExpr = AMethCall aTBool object method []
        condition = UseCond
            (S.singleton (expr "condition_true"))
            (S.singleton (expr "condition_false"))
            (M.singleton (expr "condition_equal") (ilDec 1))
            (M.singleton (expr "condition_unequal") (S.fromList [ilDec 0, ilDec 1]))
        expressionUses =
            ( M.singleton object (M.singleton method (M.singleton useExpr condition))
            , M.singleton foreignFunction (M.singleton useExpr condition) )
        methodId = MethodId object method
        methodUses = M.singleton methodId
            [(UUExpr useExpr condition, ([], [rule], []))]
        ruleUses = M.singleton rule
            (aTrue, RuleUses expressionUses expressionUses (M.empty, M.empty))
        -- These keys differ only in their use condition. A lossy decoder
        -- would collapse them into one resource-allocation entry.
        resources = M.singleton methodId (M.fromList
            [(UUExpr useExpr condition, 7), (UUExpr useExpr ucTrue, 9)])
        saved = toBSchedInfo (info
            { asi_method_uses_map = methodUses
            , asi_rule_uses_map = ruleUses
            , asi_resource_alloc_table = resources })
        restored = decode (B.pack (encode saved)) :: BSchedInfo
        failure = BSchedErrInfo
            { bsei_warnings = []
            , bsei_errors = []
            , bsei_method_uses_map = methodUses
            , bsei_rule_uses_map = ruleUses
            , bsei_resource_alloc_table = Just resources
            , bsei_sched_order = Nothing
            , bsei_schedule = Nothing
            , bsei_sched_graph = Nothing
            , bsei_rule_relation_db = Nothing
            , bsei_v_sched_info = Nothing }
        restoredFailure = decode (B.pack (encode failure)) :: BSchedErrInfo
        snapshotRules = M.map (\(predicate, RuleUses predicates reads writes) ->
            (predicate, predicates, reads, writes))
    unless (bsi_method_uses_map restored == methodUses &&
            snapshotRules (bsi_rule_uses_map restored) == snapshotRules ruleUses &&
            bsi_resource_alloc_table restored == resources) $
        die "successful schedule lost a method-use condition"
    unless (bsei_method_uses_map restoredFailure == methodUses &&
            snapshotRules (bsei_rule_uses_map restoredFailure) == snapshotRules ruleUses &&
            bsei_resource_alloc_table restoredFailure == Just resources) $
        die "failed schedule lost a method-use condition"

-- These small schedules distinguish CAN_FIRE disjointness from WILL_FIRE
-- exclusivity and cover the two kinds of edges absent from RuleRelationDB.
checkScheduleRelations :: APackage -> IO ()
checkScheduleRelations body = do
    let ident = mkId noPosition . mkFString
        ra = ident "relation_a"
        rb = ident "relation_b"
        rc = ident "relation_c"
        rd = ident "relation_d"
        unused = ident "relation_unused"
        obj = ident "relation_object"
        otherObj = ident "relation_other_object"
        method = ident "relation_method"
        uses objects =
            (aTrue, RuleUses
                (M.fromList
                    [ (o, M.singleton method
                        (M.singleton (AMethCall aTBool o method []) ucTrue))
                    | o <- objects ], M.empty)
                (M.empty, M.empty) (M.empty, M.empty))
        ruleUses = M.fromList
            [ (ra, uses [obj]), (rb, uses [obj])
            , (rc, uses [otherObj]), (rd, uses []), (unused, uses []) ]
        clean = body { apkg_interface = [], apkg_local_defs = [], apkg_rules = [] }
        initial = defaultRuleRelationship
        conflict = Just (CUse [])
        relations = RuleRelationDB
            (S.fromList [(ra,rb), (rb,ra), (ra,rc), (rc,ra)])
            (M.fromList
                [ ((ra,rb), initial { mSC = conflict })
                , ((rb,rc), initial { mCF = conflict })
                , ((rc,rb), initial { mArb = Just CArbitraryChoice })
                , ((rb,rd), initial { mRes = Just (CResource (MethodId obj method)) })
                , ((rd,rb), initial { mCycle = Just (CCycle [rd,rb]) })
                , ((rc,rd), initial { mPragma = Just (CUserEarliness noPosition) })
                , ((rd,rc), initial { mSC = conflict }) ])
        derived = deriveExclusiveRulesDB clean ruleUses relations []
        expected = erdbFromList
            [ (ra, ([rb], [])), (rb, ([ra], [rd]))
            , (rc, ([], [rd])), (rd, ([], [rb,rc])) ]
    sameEncoding "derived disjointness and exclusivity" expected derived
    unless (areRulesDisjoint derived ra rb &&
            not (areRulesDisjoint derived ra rc) &&
            not (areRulesExclusive derived rb rc) &&
            not (areRulesExclusive derived rc rb)) $
        die "incorrect disjointness, CF-only or arbitrary-order relation"

    let emptyRelations = RuleRelationDB S.empty M.empty
        field name = Method name Nothing Nothing 1 [] [] Nothing
        valueMethod = AIDef method [] emptyWireProps aTrue
            (ADef method aTBool aTrue []) (field method) []
        withMethod = clean { apkg_interface = [valueMethod] }
        -- ra is intentionally absent from apkg_rules: generated assertion
        -- monitors are still user rules for the methods-first option.
        methodUses = M.fromList [(ra, uses []), (method, uses [])]
        early = deriveExclusiveRulesDB withMethod methodUses emptyRelations
            (methodBeforeRuleEdges withMethod methodUses)
        late = deriveExclusiveRulesDB withMethod methodUses emptyRelations []
    sameEncoding "methods-before-rules relation"
        (erdbFromList [(ra, ([], [method]))]) early
    sameEncoding "relaxed method earliness" (erdbFromList []) late

    let arg = ident "relation_argument"
        alias = ident "relation_alias"
        alias2 = ident "relation_alias2"
        methodRule actions = ARule method [] "relation method"
            emptyWireProps aTrue actions [] Nothing
        definitions =
            [ ADef alias aTBool (ASPort aTBool arg) []
            , ADef alias2 aTBool (ASDef aTBool alias) [] ]
        argumentValue = AIActionValue [[(arg,aTBool)]] emptyWireProps aTrue
            method [methodRule []]
            (ADef method aTBool (ASDef aTBool alias2) []) (field method)
        selfUses = M.singleton method (uses [])
        deriveSelf iface = deriveExclusiveRulesDB
            (clean { apkg_interface = [iface], apkg_local_defs = definitions })
            selfUses emptyRelations []
        expectedSelf = erdbFromList [(method, ([], [method]))]
        conditionalAction = AIAction [[(arg,aTBool)]] emptyWireProps aTrue
            method [methodRule [ACall obj method [ASDef aTBool alias2]]]
            (field method)
    sameEncoding "method result argument self-conflict" expectedSelf
        (deriveSelf argumentValue)
    sameEncoding "method action-condition argument self-conflict" expectedSelf
        (deriveSelf conditionalAction)
    sameEncoding "method result independent of arguments" (erdbFromList [])
        (deriveSelf (argumentValue { aif_value = ADef method aTBool aTrue [] }))

    let splitId = ident "relation_split_method"
        splitMethod = AIAction [] emptyWireProps aTrue method
            [(methodRule []) { arule_id = splitId }] (field method)
        splitUses = M.fromList [(ra, uses []), (splitId, uses [])]
        splitBody = clean { apkg_interface = [splitMethod] }
        splitEarly = deriveExclusiveRulesDB splitBody splitUses emptyRelations
            (methodBeforeRuleEdges splitBody splitUses)
    sameEncoding "split interface rule earliness"
        (erdbFromList [(ra, ([], [splitId]))]) splitEarly

-- Force a parameter/port through a new local definition. Bluesim must
-- inline it on replay exactly as its compile-time validation does.
checkSimParamInlining :: APackage -> IO ()
checkSimParamInlining body =
    case [ (instanceIndex, argumentIndex, expr)
         | (instanceIndex, inst) <- zip [0 :: Int ..] (apkg_state_instances body)
         , (argumentIndex, (arg, expr)) <- zip [0 :: Int ..] (getInstArgs inst)
         , isParam arg || isPort arg ] of
        [] -> return ()
        ((instanceIndex, argumentIndex, expr):_) -> do
            let defId = mkId noPosition (mkFString "__parameter_replay_test__")
                def = ADef defId (ae_type expr) expr []
                alter inst = inst { avi_iargs =
                    [ if index == argumentIndex then ASDef (ae_type expr) defId else arg
                    | (index, arg) <- zip [0 :: Int ..] (avi_iargs inst) ] }
                modified = body
                    { apkg_local_defs = apkg_local_defs body ++ [def]
                    , apkg_state_instances =
                        [ if index == instanceIndex then alter inst else inst
                        | (index, inst) <- zip [0 :: Int ..] (apkg_state_instances body) ]
                    }
            sameEncoding "Bluesim parameter inlining"
                (apkg_state_instances (aInlineSimParams body))
                (apkg_state_instances (aInlineSimParams modified))

checkPair :: ErrorHandle -> Flags -> FilePath -> IO ABin
checkPair errh flags filename = do
    let moduleName = replaceExtension filename "bmod"
        scheduleName = replaceExtension filename "bsched"
    let config = PC.materializeConfig flags
    (canonical, reconstructed) <- readModulePair errh config moduleName
    unless (canonical == scheduleName) $ die "noncanonical pair filename"
    (_, fromSchedule) <- readModulePair errh config scheduleName
    sameEncoding "either-suffix reconstruction" reconstructed fromSchedule
    case reconstructed of
        ABinMod info _ -> checkScheduleProjection (abmi_aschedinfo info)
        _ -> die "expected a successful module artifact"
    moduleBytes <- B.readFile moduleName
    scheduleBytes <- B.readFile scheduleName
    let (amod, hash) = readBModFile errh moduleName moduleBytes
        schedule = readBSchedFile errh scheduleName scheduleBytes
    unless (hash == bs_module_hash schedule) $ die "unbound schedule"
    roundTrip "module" amod
    roundTrip "schedule" schedule
    checkScheduleRelations (amod_body amod)
    checkSimParamInlining (amod_body amod)
    case reconstructModule errh config amod schedule of
        Left reason -> die reason
        Right direct -> sameEncoding "direct reconstruction" reconstructed direct
    checkSavedMaterialization errh flags amod schedule reconstructed
    case schedule of
        BSchedError {} -> die "expected a successful schedule"
        BSched {} -> do
            let body = amod_body amod
                patch = bs_patch schedule
                unknownId = mkId noPosition (mkFString "__unknown_patch_id__")
            expectLeft "different module" $
                applyASchedulePatch body (patch { asp_module = unknownId })
            expectLeft "unknown removed rule" $
                applyASchedulePatch body
                    (patch { asp_removed_rules = [unknownId] })
            expectLeft "unknown removed definition" $
                applyASchedulePatch body
                    (patch { asp_removed_defs = [unknownId] })
            scheduled <- either die return (applyASchedulePatch body patch)
            recorded <- either die return (diffASchedulePatch body scheduled)
            replayed <- either die return (applyASchedulePatch body recorded)
            sameEncoding "recorded patch replay" scheduled replayed
            -- Scheduling may add rules, but it must not silently rewrite
            -- an existing rule's action body without recording that change.
            let originalIds = map arule_id (apkg_rules body)
                existing rule = arule_id rule `elem` originalIds &&
                                not (null (arule_actions rule))
            case find existing (apkg_rules scheduled) of
                Nothing -> return ()
                Just victim -> do
                    let change rule
                            | arule_id rule == arule_id victim =
                                rule { arule_actions = [] }
                            | otherwise = rule
                        mutated = scheduled
                            { apkg_rules = map change (apkg_rules scheduled) }
                    expectLeft "unrecorded original rule body change" $
                        diffASchedulePatch body mutated
    return reconstructed

-- Resolution of undefined values and noinline naming already happened when
-- the schedule artifact was written. A later invocation must preserve that
-- concrete IR, even with opposite lowering options. Only its compatibility
-- Flags field is invocation-local; compare all other encoded ABin fields.
checkSavedMaterialization :: ErrorHandle -> Flags -> AModule -> BSched -> ABin -> IO ()
checkSavedMaterialization errh flags amod schedule (ABinMod expected version) =
    forM_ ["0", "1", "A"] $ \choice -> do
        let changedFlags = flags
                { unSpecTo = choice
                , optUndet = not (optUndet flags)
                , stableVerilog = not (stableVerilog flags)
                }
        changed <- either die return $
            reconstructModule errh (PC.materializeConfig changedFlags) amod schedule
        case changed of
            ABinMod actual actualVersion ->
                sameEncoding "saved materialization under different invocation options"
                    (ABinMod expected version)
                    (ABinMod (actual { abmi_flags = abmi_flags expected }) actualVersion)
            _ -> die "saved materialization unexpectedly reconstructed an error"
checkSavedMaterialization _ _ _ _ _ = die "expected a successful materialized module"

-- Method-order edges describe the saved scheduling decision. Changing the
-- option for a later codegen invocation must not reinterpret that decision.
checkMethodOrder :: ErrorHandle -> Flags -> FilePath -> IO ()
checkMethodOrder errh flags filename = do
    reconstructed <- checkPair errh flags filename
    let scheduleName = replaceExtension filename "bsched"
    scheduleBytes <- B.readFile scheduleName
    case readBSchedFile errh scheduleName scheduleBytes of
        BSched { bs_method_order = edges } ->
            when (null edges) $ die "expected saved method-order edges"
        BSchedError {} -> die "expected a successful method-order schedule"
    let oppositeFlags = flags
            { relaxMethodEarliness = not (relaxMethodEarliness flags) }
    (_, opposite) <- readModulePair errh (PC.materializeConfig oppositeFlags) filename
    case (reconstructed, opposite) of
        (ABinMod actual _, ABinMod changed _) -> do
            sameEncoding "method-order body under opposite invocation option"
                (abmi_apkg actual) (abmi_apkg changed)
            let actualSchedule = abmi_aschedinfo actual
                changedSchedule = abmi_aschedinfo changed
            sameEncoding "method-order schedule under opposite invocation option"
                (toBSchedInfo actualSchedule) (toBSchedInfo changedSchedule)
            unless (erdbToList (asi_exclusive_rules_db actualSchedule) ==
                    erdbToList (asi_exclusive_rules_db changedSchedule)) $
                die "invocation option changed saved method-order relations"
        _ -> die "expected successful method-order reconstructions"

-- CUse lists explain conflicts; their method pairs may be enumerated in a
-- different order in a fresh scheduling process. Normalize only that list
-- order, preserving pair orientation, every value, and all other ordering.
normalizeScheduleDiagnostics :: BSched -> BSched
normalizeScheduleDiagnostics schedule =
    case schedule of
        BSched {} ->
            schedule
                { bs_schedule = normalizeInfo (bs_schedule schedule)
                , bs_final_schedule = normalizeInfo <$> bs_final_schedule schedule
                }
        BSchedError {} ->
            let info = bs_error schedule
            in schedule { bs_error = info
                { bsei_rule_relation_db = relations <$> bsei_rule_relation_db info } }
  where
    normalizeInfo info = info
        { bsi_rule_relation_db = relations (bsi_rule_relation_db info) }
    methodKey (MethodId object method) = (getIdString object, getIdString method)
    pairKey (first, second) = (methodKey first, methodKey second)
    conflict (CUse uses) = CUse (sortOn pairKey uses)
    conflict other = other
    relation info = info
        { mCF = conflict <$> mCF info
        , mSC = conflict <$> mSC info
        , mRes = conflict <$> mRes info
        , mCycle = conflict <$> mCycle info
        , mPragma = conflict <$> mPragma info
        , mArb = conflict <$> mArb info }
    relations (RuleRelationDB disjoint entries) =
        RuleRelationDB disjoint (M.map relation entries)

compareSchedules :: ErrorHandle -> FilePath -> FilePath -> IO ()
compareSchedules errh firstName secondName = do
    firstBytes <- B.readFile firstName
    secondBytes <- B.readFile secondName
    let first = readBSchedFile errh firstName firstBytes
        second = readBSchedFile errh secondName secondBytes
    sameEncoding "schedule with normalized conflict explanations"
        (normalizeScheduleDiagnostics first) (normalizeScheduleDiagnostics second)

-- The new materializer orders generated, independent CF wires by name so
-- that replay does not depend on string interning order. The old compiler
-- can emit the same wires and their writes in a different order. Identify
-- only added instances that exactly match the saved CF wire template, then
-- normalize their trailing instance list and contiguous runs of writes.
-- Every value, original instance, and other action ordering stays unchanged.
legacyCFWireIds :: AModule -> APackage -> S.Set Id
legacyCFWireIds amod actual =
    case amod_cf_template amod of
        Nothing -> S.empty
        Just template -> S.fromList
            [ avi_vname inst
            | inst <- apkg_state_instances actual
            , avi_vname inst `S.notMember` originals
            , encode inst == encode (template { avi_vname = avi_vname inst }) ]
  where
    originals = S.fromList
        (map avi_vname (apkg_state_instances (amod_body amod)))

normalizeLegacyCF :: S.Set Id -> APackage -> IO APackage
normalizeLegacyCF wireIds body = do
    let isWire inst = avi_vname inst `S.member` wireIds
        (originals, wires) = break isWire (apkg_state_instances body)
        isWrite (ACall object method _) =
            object `S.member` wireIds && method == unQualId idWSet
        isWrite _ = False
        normalizeWrites actions =
            let (unchanged, remaining) = span (not . isWrite) actions
            in case remaining of
                [] -> unchanged
                _ -> let (writes, rest) = span isWrite remaining
                     in unchanged ++ sortOn encode writes ++ normalizeWrites rest
    unless (all isWire wires) $
        die "generated CF wire instances are not a trailing group"
    return body
        { apkg_state_instances = originals ++ sortOn encode wires
        , apkg_rules =
            [ rule { arule_actions = normalizeWrites (arule_actions rule) }
            | rule <- apkg_rules body ] }

compareLegacy :: AModule -> ABin -> ABin -> IO ()
compareLegacy amod (ABinMod actual _) (ABinMod expected _) = do
    -- Version strings and invocation flags may differ between compiler
    -- builds. Compare every body/schedule/interface field consumed by a
    -- backend, including positions within those fields.
    let cfWireIds = legacyCFWireIds amod (abmi_apkg actual)
    actualBody <- normalizeLegacyCF cfWireIds (abmi_apkg actual)
    expectedBody <- normalizeLegacyCF cfWireIds (abmi_apkg expected)
    sameEncoding "module body" actualBody expectedBody
    let actualSchedule = abmi_aschedinfo actual
        expectedSchedule = abmi_aschedinfo expected
        -- The legacy .ba codec discards UseCond after materialization. Its
        -- decoded schedule cannot validate the richer .bsched input facts;
        -- compare only the representation that the baseline actually saves.
        -- checkUseConditionEncoding independently checks those full facts,
        -- and the module-body comparison above checks their materialized use.
        legacyActualSchedule =
            decode (B.pack (encode actualSchedule)) :: AScheduleInfo
    sameEncoding "legacy schedule representation"
        (toBSchedInfo legacyActualSchedule) (toBSchedInfo expectedSchedule)
    unless (erdbToList (asi_exclusive_rules_db actualSchedule) ==
            erdbToList (asi_exclusive_rules_db expectedSchedule)) $
        die "different derived rule exclusivity relations"
    sameEncoding "pragmas" (abmi_pps actual) (abmi_pps expected)
    sameEncoding "original type" (abmi_oqt actual) (abmi_oqt expected)
    sameEncoding "method information" (abmi_method_dump actual) (abmi_method_dump expected)
    sameEncoding "path information" (abmi_pathinfo actual) (abmi_pathinfo expected)
compareLegacy _ _ _ = die "expected two successful module artifacts"

-- Options belong to this invocation, not to either input artifact. Reuse
-- the compiler's parser for the compatibility Flags field; saved lowering
-- results must remain unchanged under different invocation options.
verifierFlags :: ErrorHandle -> [String] -> IO Flags
verifierFlags errh options = do
    libraryDir <- fromMaybe "" <$> lookupEnv "BLUESPECDIR"
    let (_, warnings, errors, flags, filenames) =
            decodeFlags options ([], [], [], defaultFlags libraryDir)
    unless (null filenames) $
        die ("unexpected verifier option arguments: " ++ unwords filenames)
    unless (null warnings) $ bsWarning errh warnings
    unless (null errors) $ bsError errh errors
    return flags

main :: IO ()
main = do
    errh <- initErrorHandle
    args <- getArgs
    case args of
        ["compare-schedules", first, second] ->
            compareSchedules errh first second
        ("pair" : filename : options) -> do
            flags <- verifierFlags errh options
            _ <- checkPair errh flags filename
            return ()
        ("method-order" : filename : options) -> do
            flags <- verifierFlags errh options
            checkMethodOrder errh flags filename
        ("compare" : filename : legacyName : options) -> do
            flags <- verifierFlags errh options
            reconstructed <- checkPair errh flags filename
            let moduleName = replaceExtension filename "bmod"
            moduleBytes <- B.readFile moduleName
            legacyBytes <- B.readFile legacyName
            let (amod, _) = readBModFile errh moduleName moduleBytes
                (legacy, _) = readABinFile errh legacyName legacyBytes
            compareLegacy amod reconstructed legacy
        _ -> die ("usage: verify-module-split pair ARTIFACT [BSC-OPTIONS] | " ++
                  "method-order ARTIFACT [BSC-OPTIONS] | " ++
                  "compare-schedules FIRST.bsched SECOND.bsched | " ++
                  "compare ARTIFACT LEGACY.ba [BSC-OPTIONS]")
