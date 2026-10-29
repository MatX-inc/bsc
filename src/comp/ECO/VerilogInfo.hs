-- Checked, version-locked Verilog information: the existing VProgram plus its
-- rendering context, typed backend metadata, and capture bindings. No RTL parser.
-- Native .ba has an optional ordinary legalized VProgram, retained by new ECO
-- captures. Legacy captures require an exact ordinary rendering control. The
-- companion also retains renderer context and final typed child metadata.
module ECO.VerilogInfo
    ( VerilogInfo(..), ASTEnvelope(..)
    , readVerilogInfo, writeVerilogInfo, renderVerilogText
    , emitVerilog, verifyVerilogInfo, verilogFilterIdentities, validateVerilogInfo
    , verilogRenderingIsExact
    ) where

import Control.Exception (evaluate)
import Control.Monad (forM_, unless)
import qualified Data.ByteString as BS
import Data.Char (isSpace)
import Data.List (isPrefixOf, sort)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (canonicalizePath, copyFile, createDirectoryIfMissing,
                         withCurrentDirectory)
import System.Environment (getExecutablePath)
import System.FilePath ((</>), takeDirectory)
import System.Process (callProcess)
import Text.Read (readMaybe)

import ABin (ABinModInfo(..))
import ASyntax
import BinData (Bin(..), encode, decode)
import ECO.Interface (fixedPorts)
import ECO.Options
import ECO.Replay
import Error (initErrorHandle)
import Eval (rnf)
import Id (getIdString)
import GenABin ()
import PPrint (pp80)
import Verilog
import VModInfo (vSched, vArgs, VArgInfo(..), VeriPortProp(..),
                 lookupInputClockWires, lookupInputResetWire)
import SchedInfo (SchedInfo(..), MethodConflictInfo(..))
import Version (bscVersionStr)

-- Every identity is exact bytes. Kept snapshots retain the source capture and
-- checked keep plan; they are proposals requiring ordinary-to-kept proof.
-- ASTEnvelope and its ae* fields are existing Read/Show wire-format names;
-- preserve them so the renamed in-memory API can still read prior captures.
data ASTEnvelope = ASTEnvelope
    { aeSchema :: Int
    , aeTargetRevision :: String
    , aeNativeVersion :: String
    , aeModule :: String
    , aeCapture :: FileIdentity
    , aeProducer :: FileIdentity
    , aePayload :: FileIdentity
    , aeOutput :: FileIdentity
    , aePlan :: Maybe FileIdentity
    } deriving (Read, Show)

data VerilogInfo = VerilogInfo
    { viProgram :: VProgram
    , viHeader :: String
    , viInstances :: [AVInst]
    , viUnboundInstances :: [String]
    , viStateOutputs :: [AStateOut]
    , viSignals :: [(AId,AType)]
    , viReplay :: ReplayManifest
    , viIdentity :: FileIdentity
    , viEnvelope :: ASTEnvelope
    , viBinding :: String
    }

-- ASTPayload is the existing binary wire-format type. Its name, field order,
-- codec, magic, and .eco-ast artifact filenames remain unchanged.
data ASTPayload = ASTPayload String VProgram [AVInst] [AStateOut] [(AId,AType)]
    deriving (Eq)
instance Bin ASTPayload where
    writeBytes (ASTPayload h p i o s) = do
        toBin h; toBin p; toBin i; toBin o; toBin s
    readBytes = do
        h <- fromBin; p <- fromBin; i <- fromBin; o <- fromBin; s <- fromBin
        return (ASTPayload h p i o s)

magic :: BS.ByteString
magic = BS.pack (map (fromIntegral . fromEnum) "bsc-eco-verilog-ast-1\n")

failVerilogInfo :: String -> IO a
failVerilogInfo = ioError . userError . ("ECO Verilog info: " ++)

-- BinData stores these five conflict relations as bit matrices. Decoding
-- preserves the relation but chooses list order using this process's intern
-- table. Other lists (including ME groups, ports and methods) retain order.
normalizeInstanceRelations :: AVInst -> AVInst
normalizeInstanceRelations inst = inst {avi_vmi = vmi {vSched = sched {methodConflictInfo = relations}}}
  where
    vmi = avi_vmi inst
    sched = vSched vmi
    m = methodConflictInfo sched
    relations = m {sCF=sort (sCF m),sSB=sort (sSB m),sP=sort (sP m),
                   sSBR=sort (sSBR m),sC=sort (sC m)}

normalizePayloadRelations :: ASTPayload -> ASTPayload
normalizePayloadRelations (ASTPayload h p i o s) =
    ASTPayload h p (map normalizeInstanceRelations i) o s

-- Reproduce only the construction-interface lowering in
-- AState.rewireClockResetInout. No final annotation supplies a direction or
-- width: ports and wire types come from the retained native AVInst. Any
-- unsupported shape remains unbound, including a changed method descriptor.
lowerConstructionInterface :: AVInst -> Maybe AVInst
lowerConstructionInterface inst = do
    let vi = avi_vmi inst
        args = vArgs vi
        values = avi_iargs inst
    if length args /= length values then Nothing else do
      pairs <- mapM (lower vi) (zip args values)
      return inst {avi_vmi = vi {vArgs = concatMap fst pairs},
                   avi_iargs = concatMap snd pairs}
  where
    lower vi (ClockArg i, ASClock _ AClock {aclock_osc=osc,aclock_gate=gate}) =
      Just $ case lookupInputClockWires i vi of
        Nothing -> ([],[])
        Just (p,Nothing) -> ([Port (p,[VPclock]) Nothing Nothing],[osc])
        Just (p,Just g) -> ([Port (p,[VPclock]) Nothing Nothing,
                            Port (g,[VPclockgate]) Nothing Nothing],[osc,gate])
    lower vi (ResetArg i, ASReset _ AReset {areset_wire=wire}) =
      Just $ case lookupInputResetWire i vi of
        Nothing -> ([],[])
        Just p -> ([Port (p,[VPreset]) Nothing Nothing],[wire])
    lower _ (InoutArg n clk rst, ASInout _ AInout {ainout_wire=wire}) =
      Just ([Port (n,[VPinout]) clk rst],[wire])
    lower _ (ClockArg _,_) = Nothing
    lower _ (ResetArg _,_) = Nothing
    lower _ (InoutArg {},_) = Nothing
    lower _ (arg,value) = Just ([arg],[value])

checkIdentity :: FileIdentity -> IO ()
checkIdentity expected = do
    actual <- fileIdentity (fiPath expected)
    same <- sameFileIdentity actual expected
    unless same $ failVerilogInfo ("file changed: " ++ fiPath expected)

readRecord :: Read a => FilePath -> IO a
readRecord path = do
    text <- readFile path
    _ <- evaluate (length text)
    maybe (failVerilogInfo ("invalid envelope: " ++ path)) return (readMaybe text)

-- Called after ordinary output and native capture, before any ECO rename.
-- The header is constructed by the existing native writer, never extracted
-- from emitted RTL. Copy the ordinary bytes before a later operation writes
-- the module's output path.
writeVerilogInfo :: ECOOptions -> FilePath -> String -> String -> ASPackage ->
                    VProgram -> FilePath -> IO ()
writeVerilogInfo opts directory moduleName header final program ordinary = do
    let capturePath = case ecoReplay opts of
            Just path -> Just path
            Nothing -> fmap (\d -> d </> moduleName ++ ".replay") (ecoCapture opts)
    case capturePath of
      Nothing -> return () -- graph-only observation has no checked native owner
      Just path -> do
        source <- fileIdentity path
        manifest <- readRecord (fiPath source) :: IO ReplayManifest
        producer <- getExecutablePath >>= fileIdentity
        let plan = Nothing
        let stem = directory </> moduleName ++ ".eco-ast"
            payloadPath = stem ++ ".bin"
            referencePath = stem ++ ".output.v"
            signals = aspkg_parameters final ++ aspkg_inputs final ++
                      aspkg_outputs final ++ aspkg_inouts final ++
                      [(adef_objid d,adef_type d) | d <- aspkg_values final ++ aspkg_inout_values final]
            payload = ASTPayload header program (aspkg_state_instances final)
                                 (aspkg_state_outputs final) signals
        createDirectoryIfMissing True directory
        BS.writeFile payloadPath (magic `BS.append` BS.pack (encode payload))
        copyFile ordinary referencePath
        payloadId <- fileIdentity payloadPath
        outputId <- fileIdentity referencePath
        -- Ordinary captures must continue to agree with the checked captured
        -- output. Kept variants have their separate ordinary control/proof.
        case plan of
          Nothing -> unless (map (fiSHA256 . snd) (rmOutputs manifest) == [fiSHA256 outputId]) $
                         failVerilogInfo "ordinary AST output differs from native capture"
          Just _ -> return ()
        checkIdentity source
        let envelope = ASTEnvelope 1 replayTargetRevision (bscVersionStr True)
                         moduleName source producer payloadId outputId plan
        writeFile stem (show envelope ++ "\n")

-- No snapshot restore is performed here: this is AST inspection, not backend
-- code generation. All binary strings are interned into the same live table.
readVerilogInfo :: FilePath -> IO VerilogInfo
readVerilogInfo path = do
    identity <- fileIdentity path
    envelope <- readRecord (fiPath identity)
    unless (aeSchema envelope == 1 && aeTargetRevision envelope == replayTargetRevision &&
            aeNativeVersion envelope == bscVersionStr True) $
        failVerilogInfo "snapshot schema/native library mismatch"
    mapM_ checkIdentity ([aeCapture envelope,aeProducer envelope,aePayload envelope,aeOutput envelope] ++
                        maybe [] (:[]) (aePlan envelope))
    errh <- initErrorHandle
    (manifest,info,_) <- inspectReplay errh (fiPath (aeCapture envelope))
    sameProducer <- sameFileIdentity (aeProducer envelope) (rmProducer manifest)
    unless sameProducer $
        failVerilogInfo "snapshot producer disagrees with its native capture"
    bytes <- BS.readFile (fiPath (aePayload envelope))
    unless (magic `BS.isPrefixOf` bytes) $ failVerilogInfo "invalid binary AST header"
    let payload@(ASTPayload header program instances outputs signals) = decode (BS.drop (BS.length magic) bytes)
    _ <- evaluate (rnf program `seq` rnf instances `seq` rnf outputs `seq` rnf signals `seq` rnf header)
    unless (all (\line -> all isSpace line || "//" `isPrefixOf` line) (lines header)) $
        failVerilogInfo "renderer header contains non-comment text"
    -- Native maps use interned Id ordering, so byte order may legitimately
    -- change when read in a different live intern table. Check the native
    -- value and serialization fixed point in this table, rather than imposing
    -- a false cross-process canonical-byte requirement. The original payload
    -- is still bound by its exact recorded SHA256.
    let encoded = BS.pack (encode payload)
        reread = decode encoded :: ASTPayload
    unless (normalizePayloadRelations reread == normalizePayloadRelations payload &&
            BS.pack (encode reread) == encoded) $
        failVerilogInfo "AST native codec roundtrip lost information"
    unless (vGetMainModName program == aeModule envelope &&
            aeModule envelope == rbModuleName (rmBackend manifest)) $
        failVerilogInfo "snapshot/native module disagreement"
    case aePlan envelope of
      Nothing -> do
        forM_ (abmi_vprogram info) $ \ordinary ->
          unless (program == ordinary && pp80 program == pp80 ordinary) $
            failVerilogInfo "ordinary snapshot differs from retained native Verilog AST"
        unless (map (fiSHA256 . snd) (rmOutputs manifest) == [fiSHA256 (aeOutput envelope)]) $
            failVerilogInfo "ordinary snapshot differs from retained native Verilog AST/output"
      Just _ -> return ()
    mapM_ checkIdentity ([aeCapture envelope,aeProducer envelope,aePayload envelope,aeOutput envelope] ++
                        maybe [] (:[]) (aePlan envelope))
    checkIdentity identity
    -- Final descriptors are useful bindings, not an independent authority to
    -- reinterpret child directions. Admit only descriptors tied to the native
    -- package; unmatched lowerings stay explicitly unknown to proof clients.
    let retainedInstances = apkg_state_instances (abmi_apkg info)
        nativeInstances = retainedInstances ++ mapMaybe lowerConstructionInterface retainedInstances
        sameInterface a b = avi_vname a == avi_vname b && avi_type a == avi_type b &&
            avi_user_import a == avi_user_import b && avi_meth_types a == avi_meth_types b &&
            avi_port_types a == avi_port_types b &&
            avi_vmi (normalizeInstanceRelations a) == avi_vmi (normalizeInstanceRelations b) &&
            map ae_type (avi_iargs a) == map ae_type (avi_iargs b)
        bound i = any (sameInterface i) nativeInstances
        snapshot = VerilogInfo
            { viProgram = program
            , viHeader = header
            , viInstances = filter bound instances
            , viUnboundInstances = [getIdString (avi_vname i) | i <- instances, not (bound i)]
            , viStateOutputs = outputs
            , viSignals = signals
            , viReplay = manifest
            , viIdentity = identity
            , viEnvelope = envelope
            , viBinding = binding
            }
        binding = case aePlan envelope of
          Just _ -> "checked-backend-variant"
          Nothing -> case abmi_vprogram info of
            Just _ -> "retained-native-ast"
            Nothing -> "legacy-regenerated-ast"
    ordinaryBytes <- BS.readFile (fiPath (aeOutput envelope))
    -- With no external filter, bind every AST node to the actual captured RTL
    -- bytes immediately. Filtered snapshots are ordinary-render controls only:
    -- verifyVerilogInfo must run their exact filters. A transformed candidate
    -- requires observed byte identity before and after its own filter run.
    if null (rmFilters manifest)
      then do
        unless (TE.encodeUtf8 (T.pack (renderVerilogText snapshot program)) == ordinaryBytes) $
          failVerilogInfo "AST rendering differs from captured ordinary output"
      else return ()
    validateVerilogInfo snapshot
    return snapshot

-- This is exactly writeVFileCatch's whitespace policy applied to the native
-- header and VProgram prettyprinter. No emitted RTL is parsed or rewritten.
renderVerilogText :: VerilogInfo -> VProgram -> String
renderVerilogText snapshot program = unlines $ map trim $ lines (viHeader snapshot ++ pp80 program)
  where
    trim = reverse . dropWhile isSpace . reverse

snapshotFiles :: VerilogInfo -> [FileIdentity]
snapshotFiles snapshot =
    let e = viEnvelope snapshot; m = viReplay snapshot
    in [viIdentity snapshot,aeCapture e,aeProducer e,aePayload e,aeOutput e,
        rmABin m,rmFlags m,rmIntern m] ++ maybe [] (:[]) (aePlan e) ++
       rmDependencies m ++ map snd (rmFilters m) ++ map snd (rmOutputs m)

validateVerilogInfo :: VerilogInfo -> IO ()
validateVerilogInfo = mapM_ checkIdentity . snapshotFiles

verilogFilterIdentities :: VerilogInfo -> [(String,FileIdentity)]
verilogFilterIdentities = rmFilters . viReplay

-- This is an observed byte property of one bound output, not a semantic
-- whitelist for a filter program. Every transformed candidate repeats it.
verilogRenderingIsExact :: VerilogInfo -> IO Bool
verilogRenderingIsExact snapshot = do
    validateVerilogInfo snapshot
    output <- BS.readFile (fiPath (aeOutput (viEnvelope snapshot)))
    let exact = TE.encodeUtf8 (T.pack (renderVerilogText snapshot (viProgram snapshot))) == output
    _ <- evaluate exact
    validateVerilogInfo snapshot
    return exact

emitVerilog :: VerilogInfo -> FilePath -> VProgram -> IO ()
emitVerilog snapshot path program = do
    unless (fixedPorts (viProgram snapshot) == fixedPorts program) $
        failVerilogInfo "candidate changed module ports or parameters"
    let changed = program /= viProgram snapshot
        filtered = not (null (verilogFilterIdentities snapshot))
    if changed && filtered
      then do
        exact <- verilogRenderingIsExact snapshot
        unless exact $ failVerilogInfo "ordinary Verilog filter changed AST rendering; post-filter proof required"
      else return ()
    validateVerilogInfo snapshot
    let manifest = viReplay snapshot
    createDirectoryIfMissing True (takeDirectory path)
    absolute <- canonicalizePath path
    overlap <- outputPathsOverlap (map fiPath (snapshotFiles snapshot)) [absolute]
    unless (not overlap) $
        failVerilogInfo "output would overwrite a bound capture or snapshot artifact"
    let renderedBytes = TE.encodeUtf8 (T.pack (renderVerilogText snapshot program))
    BS.writeFile absolute renderedBytes
    withCurrentDirectory (rmWorkingDirectory manifest) $
        forM_ (reverse (rmFilters manifest)) $ \(_,filterIdentity) -> do
            checkIdentity filterIdentity
            callProcess (fiPath filterIdentity) [absolute]
            checkIdentity filterIdentity
    afterFilter <- BS.readFile absolute
    if changed
      then unless (afterFilter == renderedBytes) $
             failVerilogInfo "candidate Verilog filter differs from bound typed AST rendering; post-filter proof required"
      else do
        expected <- BS.readFile (fiPath (aeOutput (viEnvelope snapshot)))
        unless (afterFilter == expected) $ failVerilogInfo "ordinary AST emission differs from captured output"
    validateVerilogInfo snapshot

-- The caller chooses a fresh control directory; preserve the original module
-- basename for filters which use it. Failure is explicit, never a relaxation.
verifyVerilogInfo :: VerilogInfo -> FilePath -> IO FileIdentity
verifyVerilogInfo snapshot directory = do
    let output = directory </> aeModule (viEnvelope snapshot) ++ ".v"
    emitVerilog snapshot output (viProgram snapshot)
    actual <- fileIdentity output
    unless (fiSHA256 actual == fiSHA256 (aeOutput (viEnvelope snapshot))) $
        failVerilogInfo "ordinary AST rendering/filter byte control failed"
    return actual
