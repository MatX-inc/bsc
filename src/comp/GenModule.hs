-- | Versioned module and schedule artifacts. Their combination reconstructs
-- the module view formerly stored in a module .ba file.
module GenModule
    ( genBModFile
    , genBSchedFile
    , readBModFile
    , readBSchedFile
    , readBModFileMaybe
    , readBSchedFileMaybe
    , readModulePair
    , reconstructModule
    , diffAMaterializePatch
    ) where

import ABin
import AMaterializePatch
import AModule
import ASchedulePatch
import AScheduleRelations (deriveExclusiveRulesDB)
import ASyntax (APackage(..), ADef(..), ARule(..), AIFace(..), AVInst(..))
import AUses
    ( UseCond(..), UniqueUse(..), RuleUses(..), MethodUsesMap, RuleUsesMap )
import BinData
import Error (ErrorHandle, ErrMsg(..), bsError, bsErrorUnsafe, internalError,
              withErrorHandleFlags)
import Eval (rnf)
import FileIOUtil (readBinaryFileCatch, writeBinaryFileCatch)
import GenABin ()
import Id (unQualId)
import Position (Position, noPosition)
import qualified PhaseConfig as PC
import qualified PhaseConfigLegacy as PL
import Pragma (PProp(PPoptions))
import RSchedule (RAT)
import Util (hashInit, nextHashByte, showHash)
import Version (bscVersionStr)

import Control.Exception
    ( AsyncException, SomeException, catch, displayException, evaluate
    , fromException, throwIO )
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as M
import System.Exit (ExitCode)
import System.FilePath (replaceExtension, takeExtension)

bmodHeader, bschedHeader :: B.ByteString
bmodHeader = BC.pack "bsc-bmod-20261003-2"
bschedHeader = BC.pack "bsc-bsched-20261003-5"

compilerVersion :: String
compilerVersion = bscVersionStr True

-- The hash covers the exact payload written, including remapped positions
-- and the compiler version. It uses the same hash as BinData.decodeWithHash.
genBModFile :: ErrorHandle -> (Position -> Position) -> FilePath ->
               AModule -> IO String
genBModFile errh remapP filename amod = do
    let payload = B.pack (encodeWith remapP (compilerVersion, amod))
    writeBinaryFileCatch errh filename
        (B.unpack bmodHeader ++ B.unpack payload)
    let hash = showHash (B.foldl' nextHashByte hashInit payload)
    length hash `seq` return hash

genBSchedFile :: ErrorHandle -> (Position -> Position) -> FilePath ->
                 BSched -> IO ()
genBSchedFile errh remapP filename schedule =
    writeBinaryFileCatch errh filename
        (B.unpack bschedHeader ++ encodeWith remapP (compilerVersion, schedule))

readBModFile :: ErrorHandle -> FilePath -> B.ByteString -> (AModule, String)
readBModFile errh filename bytes =
    either (invalidFile errh filename) id (decodeBMod bytes)

readBSchedFile :: ErrorHandle -> FilePath -> B.ByteString -> BSched
readBSchedFile errh filename bytes =
    either (invalidFile errh filename) id (decodeBSched bytes)

-- Like readABinFileMaybe, these check the format/version. Malformed payloads
-- are rejected by BinData; the IO pair reader below turns those exceptions
-- into file diagnostics before returning any module to a backend.
readBModFileMaybe :: B.ByteString -> Maybe (AModule, String)
readBModFileMaybe = either (const Nothing) Just . decodeBMod

readBSchedFileMaybe :: B.ByteString -> Maybe BSched
readBSchedFileMaybe = either (const Nothing) Just . decodeBSched

decodeBMod :: B.ByteString -> Either String (AModule, String)
decodeBMod bytes
    | B.take (B.length bmodHeader) bytes /= bmodHeader =
        Left "module artifact format does not match this compiler"
    | otherwise =
        let ((version, amod), hash) =
                decodeWithHash (B.drop (B.length bmodHeader) bytes)
        in if version == compilerVersion
           then Right (amod, hash)
           else Left "module artifact was written by a different compiler version"

decodeBSched :: B.ByteString -> Either String BSched
decodeBSched bytes
    | B.take (B.length bschedHeader) bytes /= bschedHeader =
        Left "schedule artifact format does not match this compiler"
    | otherwise =
        let (version, schedule) = decode (B.drop (B.length bschedHeader) bytes)
        in if version == compilerVersion
           then Right schedule
           else Left "schedule artifact was written by a different compiler version"

invalidFile :: ErrorHandle -> FilePath -> String -> a
invalidFile errh filename reason =
    bsErrorUnsafe errh [(noPosition, EFileReadFailure filename reason)]

-- | Read both siblings, regardless of which one the caller named. Validate
-- their content binding before applying the schedule. The returned path is
-- always the schedule path, so callers use one identity for the pair.
-- Common lowering is recorded as concrete IR changes. The invocation's
-- configuration controls diagnostics and later backend generation only.
readModulePair :: ErrorHandle -> PC.PhaseConfig PC.MaterializeFlags ->
                  FilePath -> IO (FilePath, ABin)
readModulePair errh config filename =
  withErrorHandleFlags errh (PL.legacyFlags config) $ do
    if takeExtension filename `elem` [".bmod", ".bsched"]
       then return ()
       else bsError errh [(noPosition, EFileReadFailure filename
                              "expected a .bmod or .bsched artifact")]
    let moduleName = replaceExtension filename "bmod"
        scheduleName = replaceExtension filename "bsched"
    moduleBytes <- readBinaryFileCatch errh noPosition moduleName
    scheduleBytes <- readBinaryFileCatch errh noPosition scheduleName
    let reconstruct = do
            (amod, hash) <- decodeBMod moduleBytes
            schedule <- decodeBSched scheduleBytes
            if hash == bs_module_hash schedule
               then reconstructModule errh config amod schedule
               else Left "schedule does not match the contents of its .bmod artifact"
        forceResult result@(Left _) = result
        forceResult result@(Right (ABinMod info _)) =
            rnf (abmi_apkg info) `seq` rnf (abmi_aschedinfo info) `seq` result
        forceResult result@(Right (ABinModSchedErr info _)) =
            rnf (abmsei_apkg info) `seq` result
        forceResult result@(Right (ABinForeignFunc _ _)) = result
    result <- catchSynchronous (evaluate (forceResult reconstruct))
        (return . Left . ("invalid module/schedule artifact: " ++) . displayException)
    case result of
        Left reason -> bsError errh
            [(noPosition, EFileReadFailure scheduleName reason)]
        Right abin -> return (scheduleName, abin)

-- Preserve cancellation and already-reported compiler diagnostics. Only
-- malformed data and reconstruction failures become file-read diagnostics.
catchSynchronous :: IO a -> (SomeException -> IO a) -> IO a
catchSynchronous action handler = catch action $ \exception ->
    case fromException exception :: Maybe AsyncException of
        Just _ -> throwIO exception
        Nothing -> case fromException exception :: Maybe ExitCode of
            Just _ -> throwIO exception
            Nothing -> handler exception

-- | Reconstruct after the reader has checked the exact artifact hash.
-- Keeping this operation pure also permits in-memory round-trip checks.
reconstructModule :: ErrorHandle -> PC.PhaseConfig PC.MaterializeFlags ->
                     AModule -> BSched -> Either String ABin
reconstructModule _errh config amod schedule =
    let flags = PL.legacyFlags config
        info = amod_info amod
        body = amod_body amod
    in if unQualId (ami_name info) /= apkg_name body
       then Left "module identity does not match its elaborated body"
       else case schedule of
        BSchedError { bs_error = scheduleError } ->
            Right (ABinModSchedErr (ABinModSchedErrInfo
                { abmsei_path = ami_source_prefix info
                , abmsei_src_name = ami_source_package info
                , abmsei_apkg = body
                , abmsei_aschederrinfo = fromBSchedErrInfo scheduleError
                , abmsei_pps = ami_pragmas info
                , abmsei_oqt = ami_original_type info
                , abmsei_flags = flags
                }) compilerVersion)
        BSched {} -> do
            scheduledBody <- applyASchedulePatch body (bs_patch schedule)
            finalBody <- applyAMaterializePatch scheduledBody (bs_materialization schedule)
            if apkg_backend finalBody /= bs_backend schedule
               then Left "materialization backend does not match the schedule artifact"
               else return ()
            let savedInfo = bs_schedule schedule
                exclusive = deriveExclusiveRulesDB body
                    (bsi_rule_uses_map savedInfo) (bsi_rule_relation_db savedInfo)
                    (bs_method_order schedule)
                finalSchedule = fromBSchedInfo exclusive
                    (maybe savedInfo id (bs_final_schedule schedule))
            return (ABinMod (ABinModInfo
                { abmi_path = ami_source_prefix info
                , abmi_src_name = ami_source_package info
                , abmi_apkg = finalBody
                , abmi_aschedinfo = finalSchedule
                , abmi_pps = ami_pragmas info
                , abmi_oqt = ami_original_type info
                , abmi_method_dump = bs_method_dump schedule
                , abmi_pathinfo = bs_path_info schedule
                , abmi_flags = flags
                }) compilerVersion)

-- | Record common-lowering results without retaining the options which
-- chose them. Exact object encodings include source positions/properties
-- deliberately ignored by ASyntax's Eq instances. The full replay check
-- also rejects changes to package fields outside the four edited lists.
diffAMaterializePatch :: (Position -> Position) -> APackage -> APackage ->
                         Either String AMaterializePatch
diffAMaterializePatch remapP before after = do
    defs <- diffAOrderedPatch adef_objid same
                (apkg_local_defs before) (apkg_local_defs after)
    rules <- diffAOrderedPatch arule_id same
                (apkg_rules before) (apkg_rules after)
    ifc <- diffAOrderedPatch aif_name same
                (apkg_interface before) (apkg_interface after)
    insts <- diffAOrderedPatch avi_vname same
                (apkg_state_instances before) (apkg_state_instances after)
    let patch = AMaterializePatch
            { amp_module = apkg_name before
            , amp_backend = apkg_backend after
            , amp_defs = defs
            , amp_rules = rules
            , amp_interface = ifc
            , amp_instances = insts
            }
    replayed <- applyAMaterializePatch before patch
    if same replayed after
       then Right patch
       else Left "common lowering changed a module field not represented by its materialization patch"
  where
    same :: Bin a => a -> a -> Bool
    same left right = encodeWith remapP left == encodeWith remapP right

instance Bin AModuleInfo where
    writeBytes info = do
        toBin (ami_name info)
        toBin (ami_original_type info)
        -- Options pragmas are invocation arguments, not module declarations.
        -- Enforce this here as well as at the elaboration boundary.
        toBin (filter isDeclaration (ami_pragmas info))
        toBin (ami_is_function info)
        toBin (ami_source_prefix info)
        toBin (ami_source_package info)
      where
        isDeclaration (PPoptions _) = False
        isDeclaration _ = True
    readBytes = AModuleInfo <$> fromBin <*> fromBin <*> fromBin <*> fromBin
                            <*> fromBin <*> fromBin

instance Bin AModule where
    writeBytes amod = section "AModule" $ do
        toBin (amod_info amod)
        toBin (amod_body amod)
        toBin (amod_true_methods amod)
        toBin (amod_cf_template amod)
    readBytes = AModule <$> fromBin <*> fromBin <*> fromBin <*> fromBin

instance Bin ASchedulePatch where
    writeBytes patch = do
        toBin (asp_module patch)
        toBin (asp_backend patch)
        toBin (asp_removed_defs patch)
        toBin (asp_added_defs patch)
        toBin (asp_removed_rules patch)
        toBin (asp_added_rules patch)
        toBin (asp_ready_values patch)
    readBytes = ASchedulePatch <$> fromBin <*> fromBin <*> fromBin <*> fromBin
                              <*> fromBin <*> fromBin <*> fromBin

instance Bin a => Bin (AOrderedPatch a) where
    writeBytes patch = do
        toBin (aop_order patch)
        toBin (aop_updates patch)
    readBytes = AOrderedPatch <$> fromBin <*> fromBin

instance Bin AMaterializePatch where
    writeBytes patch = do
        toBin (amp_module patch)
        toBin (amp_backend patch)
        toBin (amp_defs patch)
        toBin (amp_rules patch)
        toBin (amp_interface patch)
        toBin (amp_instances patch)
    readBytes = AMaterializePatch <$> fromBin <*> fromBin <*> fromBin
                                 <*> fromBin <*> fromBin <*> fromBin

instance Bin BSched where
    writeBytes schedule@BSched {} = section "BSched" $ do
        putI 0
        toBin (bs_module_hash schedule)
        toBin (bs_patch schedule)
        toBin (bs_schedule schedule)
        toBin (bs_materialization schedule)
        toBin (bs_final_schedule schedule)
        toBin (bs_method_order schedule)
        toBin (bs_wrapper_schedule schedule)
        toBin (bs_method_dump schedule)
        toBin (bs_path_info schedule)
        toBin (bs_backend schedule)
    writeBytes (BSchedError hash scheduleError) = section "BSchedError" $ do
        putI 1
        toBin hash
        toBin scheduleError
    readBytes = do
        tag <- getI
        case tag of
            0 -> BSched <$> fromBin <*> fromBin <*> fromBin <*> fromBin
                        <*> fromBin <*> fromBin <*> fromBin <*> fromBin
                        <*> fromBin <*> fromBin
            1 -> BSchedError <$> fromBin <*> fromBin
            _ -> internalError ("GenModule.Bin(BSched): tag = " ++ show tag)

instance Bin BSchedInfo where
    writeBytes info = section "BSchedInfo" $ do
        toBin (bsi_warnings info)
        toBin (FullMethodUses (bsi_method_uses_map info))
        toBin (FullRuleUsesMap (bsi_rule_uses_map info))
        toBin (FullRAT (bsi_resource_alloc_table info))
        toBin (bsi_sched_order info)
        toBin (bsi_schedule info)
        toBin (bsi_sched_graph info)
        toBin (bsi_rule_relation_db info)
        toBin (bsi_v_sched_info info)
    readBytes = BSchedInfo <$> fromBin
                          <*> (unFullMethodUses <$> fromBin)
                          <*> (unFullRuleUsesMap <$> fromBin)
                          <*> (unFullRAT <$> fromBin)
                          <*> fromBin <*> fromBin <*> fromBin <*> fromBin
                          <*> fromBin

instance Bin BSchedErrInfo where
    writeBytes info = section "BSchedErrInfo" $ do
        toBin (bsei_warnings info)
        toBin (bsei_errors info)
        toBin (FullMethodUses (bsei_method_uses_map info))
        toBin (FullRuleUsesMap (bsei_rule_uses_map info))
        toBin (fmap FullRAT (bsei_resource_alloc_table info))
        toBin (bsei_sched_order info)
        toBin (bsei_schedule info)
        toBin (bsei_sched_graph info)
        toBin (bsei_rule_relation_db info)
        toBin (bsei_v_sched_info info)
    readBytes = BSchedErrInfo <$> fromBin <*> fromBin
                             <*> (unFullMethodUses <$> fromBin)
                             <*> (unFullRuleUsesMap <$> fromBin)
                             <*> (fmap unFullRAT <$> fromBin)
                             <*> fromBin <*> fromBin <*> fromBin
                             <*> fromBin <*> fromBin

-- Legacy .ba encoding deliberately discards UseCond: its module body has
-- already been instrumented. A .bsched precedes that transformation, so its
-- conditions are inputs to conflict-free checks and must survive exactly.
-- Keep this lossless encoding local to the new format rather than changing
-- the established GenABin instances or format.
newtype FullUseCond = FullUseCond { unFullUseCond :: UseCond }

instance Bin FullUseCond where
    writeBytes (FullUseCond (UseCond true false equal unequal)) = do
        toBin true
        toBin false
        toBin equal
        toBin unequal
    readBytes = FullUseCond <$>
        (UseCond <$> fromBin <*> fromBin <*> fromBin <*> fromBin)

newtype FullUniqueUse = FullUniqueUse { unFullUniqueUse :: UniqueUse }
    deriving (Eq, Ord)

instance Bin FullUniqueUse where
    writeBytes (FullUniqueUse (UUAction action)) = do
        putI 0
        toBin action
    writeBytes (FullUniqueUse (UUExpr expr condition)) = do
        putI 1
        toBin expr
        toBin (FullUseCond condition)
    readBytes = do
        tag <- getI
        case tag of
            0 -> FullUniqueUse . UUAction <$> fromBin
            1 -> do
                expr <- fromBin
                condition <- unFullUseCond <$> fromBin
                return (FullUniqueUse (UUExpr expr condition))
            _ -> internalError ("GenModule.Bin(FullUniqueUse): tag = " ++ show tag)

newtype FullRuleUses = FullRuleUses { unFullRuleUses :: RuleUses }

instance Bin FullRuleUses where
    writeBytes (FullRuleUses (RuleUses predicates reads writes)) = do
        let full (methods, functions) =
                ( M.map (M.map (M.map FullUseCond)) methods
                , M.map (M.map FullUseCond) functions )
        toBin (full predicates)
        toBin (full reads)
        toBin writes
    readBytes = do
        let restore (methods, functions) =
                ( M.map (M.map (M.map unFullUseCond)) methods
                , M.map (M.map unFullUseCond) functions )
        predicates <- restore <$> fromBin
        reads <- restore <$> fromBin
        writes <- fromBin
        return (FullRuleUses (RuleUses predicates reads writes))

newtype FullMethodUses = FullMethodUses { unFullMethodUses :: MethodUsesMap }

instance Bin FullMethodUses where
    writeBytes (FullMethodUses uses) = toBin $
        M.map (map (\(use, users) -> (FullUniqueUse use, users))) uses
    readBytes = FullMethodUses .
        M.map (map (\(use, users) -> (unFullUniqueUse use, users))) <$> fromBin

newtype FullRuleUsesMap = FullRuleUsesMap { unFullRuleUsesMap :: RuleUsesMap }

instance Bin FullRuleUsesMap where
    writeBytes (FullRuleUsesMap uses) = toBin $
        M.map (\(predicate, uses) -> (predicate, FullRuleUses uses)) uses
    readBytes = FullRuleUsesMap .
        M.map (\(predicate, uses) -> (predicate, unFullRuleUses uses)) <$> fromBin

newtype FullRAT = FullRAT { unFullRAT :: RAT }

instance Bin FullRAT where
    writeBytes (FullRAT table) = toBin $
        M.map (M.mapKeysMonotonic FullUniqueUse) table
    readBytes = FullRAT . M.map (M.mapKeysMonotonic unFullUniqueUse) <$> fromBin
