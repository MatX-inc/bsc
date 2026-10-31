-- Checked, version-locked Verilog information: the existing VProgram plus its
-- rendering context, typed backend metadata, and capture bindings. No RTL parser.
-- Every ECO capture retains the ordinary legalized VProgram in its native .ba;
-- the payload must equal it and render to the captured bytes. The companion
-- also retains renderer context and final typed child metadata.
module ECO.VerilogInfo
    ( VerilogInfo(..), ASTEnvelope(..)
    , readVerilogInfo, writeVerilogInfo, renderVerilogText
    , emitVerilog, verifyVerilogInfo, verilogFilterIdentities, validateVerilogInfo
    , verilogRenderingIsExact, lexicallyEmittable, declaredBeforeUse
    ) where

import Control.Exception (evaluate)
import Control.Monad (foldM, foldM_, forM_, unless, when)
import qualified Data.ByteString as BS
import Data.Char (isAsciiLower, isAsciiUpper, isDigit, isSpace)
import Data.List (isPrefixOf, sort)
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (canonicalizePath, copyFile, createDirectoryIfMissing,
                         withCurrentDirectory)
import System.FilePath ((</>), takeDirectory)
import System.Process (callProcess)
import Text.Read (readMaybe)

import ABin (ABinModInfo(..))
import ASyntax
import BinData (Bin(..), encode, decodeStrict)
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
-- ASTEnvelope and its ae* fields are the Read/Show wire-format names.
data ASTEnvelope = ASTEnvelope
    { aeSchema :: Int
    , aeNativeVersion :: String
    , aeModule :: String
    , aeCapture :: FileIdentity
    , aeProducer :: FileIdentity
    , aePayload :: FileIdentity
    , aeOutput :: FileIdentity
    , aePlan :: Maybe FileIdentity
    } deriving (Read, Show)

-- viExactRendering: the payload renders to exactly the captured output bytes.
-- Without a filter this is required; with one it records whether the filter
-- is a byte identity on the ordinary output.
data VerilogInfo = VerilogInfo
    { viProgram :: VProgram
    , viHeader :: String
    , viInstances :: [AVInst]
    , viUnboundInstances :: [String]
    , viReplay :: ReplayManifest
    , viIdentity :: FileIdentity
    , viEnvelope :: ASTEnvelope
    , viExactRendering :: Bool
    }

-- ASTPayload is the binary wire-format type behind the .eco-ast.bin artifact.
data ASTPayload = ASTPayload String VProgram [AVInst]
    deriving (Eq)
instance Bin ASTPayload where
    writeBytes (ASTPayload h p i) = do
        toBin h; toBin p; toBin i
    readBytes = do
        h <- fromBin; p <- fromBin; i <- fromBin
        return (ASTPayload h p i)

magic :: BS.ByteString
magic = BS.pack (map (fromIntegral . fromEnum) "bsc-eco-verilog-ast-3\n")

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
normalizePayloadRelations (ASTPayload h p i) =
    ASTPayload h p (map normalizeInstanceRelations i)

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
        producer <- producerIdentity
        plan <- mapM fileIdentity (ecoBackendPlan opts)
        let stem = directory </> moduleName ++ ".eco-ast"
            payloadPath = stem ++ ".bin"
            referencePath = stem ++ ".output.v"
            payload = ASTPayload header program (aspkg_state_instances final)
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
        let envelope = ASTEnvelope 3 (bscVersionStr True)
                         moduleName source producer payloadId outputId plan
        writeFile stem (show envelope ++ "\n")

-- No snapshot restore is performed here: this is AST inspection, not backend
-- code generation. All binary strings are interned into the same live table.
-- One identity pass: the envelope's own artifacts here, the capture's inside
-- inspectReplay (which also binds the producer the manifest names).
readVerilogInfo :: FilePath -> IO VerilogInfo
readVerilogInfo path = do
    identity <- fileIdentity path
    envelope <- readRecord (fiPath identity)
    unless (aeSchema envelope == 3 && aeNativeVersion envelope == bscVersionStr True) $
        failVerilogInfo "snapshot schema/native library mismatch"
    checkIdentity (aeCapture envelope)
    errh <- initErrorHandle
    (manifest,info,_) <- inspectReplay errh (fiPath (aeCapture envelope))
    sameProducer <- sameFileIdentity (aeProducer envelope) (rmProducer manifest)
    unless sameProducer $
        failVerilogInfo "snapshot producer disagrees with its native capture"
    mapM_ checkIdentity ([aePayload envelope,aeOutput envelope] ++ maybe [] (:[]) (aePlan envelope))
    bytes <- BS.readFile (fiPath (aePayload envelope))
    unless (magic `BS.isPrefixOf` bytes) $ failVerilogInfo "invalid binary AST header"
    -- The payload is a BinData container with nothing marked lazy; decode it
    -- strictly so that every shared-table entry is validated here, before
    -- the fixed-point check below re-encodes it.
    let payload@(ASTPayload header program instances) = decodeStrict (BS.drop (BS.length magic) bytes)
    _ <- evaluate (rnf program `seq` rnf instances `seq` rnf header)
    unless (all (\line -> all isSpace line || "//" `isPrefixOf` line) (lines header)) $
        failVerilogInfo "renderer header contains non-comment text"
    -- Native maps use interned Id ordering, so byte order may legitimately
    -- change when read in a different live intern table. Check the native
    -- value and serialization fixed point in this table, rather than imposing
    -- a false cross-process canonical-byte requirement. The original payload
    -- is still bound by its exact recorded SHA256.
    let encoded = BS.pack (encode payload)
        reread = decodeStrict encoded :: ASTPayload
    unless (normalizePayloadRelations reread == normalizePayloadRelations payload &&
            BS.pack (encode reread) == encoded) $
        failVerilogInfo "AST native codec roundtrip lost information"
    unless (vGetMainModName program == aeModule envelope &&
            aeModule envelope == rbModuleName (rmBackend manifest)) $
        failVerilogInfo "snapshot/native module disagreement"
    retained <- maybe (failVerilogInfo "capture lacks the retained native Verilog AST")
                      return (abmi_vprogram info)
    case aePlan envelope of
      Nothing -> do
        unless (program == retained && pp80 program == pp80 retained) $
            failVerilogInfo "ordinary snapshot differs from retained native Verilog AST"
        unless (map (fiSHA256 . snd) (rmOutputs manifest) == [fiSHA256 (aeOutput envelope)]) $
            failVerilogInfo "ordinary snapshot differs from retained native Verilog AST/output"
      Just _ -> return ()
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
    ordinaryBytes <- BS.readFile (fiPath (aeOutput envelope))
    -- Bind every AST node to the actual captured RTL bytes once. A filtered
    -- snapshot whose rendering is not exact is an ordinary-render control
    -- only: a transformed candidate requires observed byte identity before
    -- and after its own filter run (emitVerilog).
    let snapshot = VerilogInfo
            { viProgram = program
            , viHeader = header
            , viInstances = filter bound instances
            , viUnboundInstances = [getIdString (avi_vname i) | i <- instances, not (bound i)]
            , viReplay = manifest
            , viIdentity = identity
            , viEnvelope = envelope
            , viExactRendering = exact
            }
        exact = TE.encodeUtf8 (T.pack (renderVerilogText snapshot program)) == ordinaryBytes
    _ <- evaluate exact
    unless (exact || not (null (rmFilters manifest))) $
        failVerilogInfo "AST rendering differs from captured ordinary output"
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
verilogRenderingIsExact = return . viExactRendering

-- Printed identifier positions, in print order. The annotation VIds of
-- VEOp/VEUnOp/VEWConst and the inlined item inside a VId are never printed;
-- constants, tri-constants, unknowns and quoted strings print no identifier.
data Printed = Ident String | Macro String

printedText :: Printed -> String
printedText (Ident s) = s
printedText (Macro s) = s

ident :: VId -> Printed
ident = Ident . getVIdString

exprP :: VExpr -> [Printed]
exprP e = case e of
    VEWConst {} -> []
    VEUnOp _ _ a -> exprP a
    VEOp _ a _ b -> exprP a ++ exprP b
    VEVar i -> [ident i]
    VEConcat es -> concatMap exprP es
    VEIndex i a -> ident i : exprP a
    VESelect a h l -> exprP a ++ exprP h ++ exprP l
    VESelect1 a p -> exprP a ++ exprP p
    VERepeat a b -> exprP a ++ exprP b
    VEIf c t f -> exprP c ++ exprP t ++ exprP f
    VEFctCall f es -> ident f : concatMap exprP es
    VEMacro s -> [Macro s]
    _ -> []

eventP :: VEventExpr -> [Printed]
eventP (VEEOr a b) = eventP a ++ eventP b
eventP (VEEposedge e) = exprP e
eventP (VEEnegedge e) = exprP e
eventP (VEE e) = exprP e
eventP (VEEMacro s e) = Macro s : exprP e

lvalueP :: VLValue -> [Printed]
lvalueP (VLId i) = [ident i]
lvalueP (VLConcat ls) = concatMap lvalueP ls
lvalueP (VLSub l e) = lvalueP l ++ exprP e

stmtP :: VStmt -> [Printed]
stmtP s = case s of
    VAt ev t -> eventP ev ++ stmtP t
    Valways t -> stmtP t
    Vinitial t -> stmtP t
    VSeq ts -> concatMap stmtP ts
    Vcasex e arms _ _ -> exprP e ++ concatMap armP arms
    Vcase e arms _ _ -> exprP e ++ concatMap armP arms
    VAssign l e -> lvalueP l ++ exprP e
    VAssignA l e -> lvalueP l ++ exprP e
    Vif e t -> exprP e ++ stmtP t
    Vifelse e t f -> exprP e ++ stmtP t ++ stmtP f
    Vdumpvars _ is -> map ident is
    VTask t es -> ident t : concatMap exprP es
    VAssert ev es -> eventP ev ++ concatMap exprP es
    VZeroDelay -> []
  where
    armP (VCaseArm es t) = concatMap exprP es ++ stmtP t
    armP (VDefault t) = stmtP t

rangeP :: Maybe VRange -> [Printed]
rangeP = maybe [] (\(h,l) -> exprP h ++ exprP l)

varP :: VVar -> [Printed]
varP (VVar i) = [ident i]
varP (VArray (l,h) i) = ident i : exprP l ++ exprP h

declP :: VVDecl -> [Printed]
declP (VVDecl _ r vs) = rangeP r ++ concatMap varP vs
declP (VVDWire r v e) = rangeP r ++ varP v ++ exprP e

connP :: (VId, Maybe VExpr) -> [Printed]
connP (i,e) = ident i : maybe [] exprP e

argP :: VArg -> [Printed]
argP (VAInput i r) = ident i : rangeP r
argP (VAInout i j r) = ident i : maybe [] (\k -> [ident k]) j ++ maybe [] rangeP r
argP (VAOutput i r) = ident i : rangeP r
argP (VAParameter i r e _) = ident i : rangeP r ++ exprP e

itemP :: VMItem -> [Printed]
itemP x = case x of
    VMDecl d -> declP d
    VMInst m i ps cs -> ident m : ident i : concatMap connP ps ++ concatMap connP cs
    VMAssign l e -> lvalueP l ++ exprP e
    VMStmt _ s -> stmtP s
    VMComment _ y -> itemP y
    VMRegGroup i _ _ y -> ident i : itemP y
    VMGroup _ yss -> concatMap (concatMap itemP) yss
    VMFunction (VFunction n r ds s) -> ident n : rangeP r ++ concatMap declP ds ++ stmtP s
    VMDPI d -> dpiP d

-- the C link name, when given, is printed verbatim before "="
dpiP :: VDPI -> [Printed]
dpiP (VDPI n clink _ _ args) = ident n : map Ident (maybe [] (:[]) clink) ++ [ident i | (i,_,_) <- args]

-- ^\$?[A-Za-z_][A-Za-z0-9_$.]*$
validIdentifier :: String -> Bool
validIdentifier ('$':s) = plain s
validIdentifier s = plain s

plain :: String -> Bool
plain (c:cs) = (letter c || c == '_') && all (\d -> letter d || isDigit d || d `elem` "_$.") cs
plain [] = False

letter :: Char -> Bool
letter c = isAsciiUpper c || isAsciiLower c

-- The printer emits every identifier and comment element verbatim: an element
-- containing a line break leaves its comment. Nothing else carries free text
-- (strings are quoted; instance parameters are named, with no comment).
lexicallyEmittable :: VProgram -> Either String ()
lexicallyEmittable (VProgram modules dpis header) = do
    forM_ (concatMap dpiP dpis ++ concatMap moduleP modules) $ \p ->
        unless (validIdentifier (printedText p)) (Left (printedText p))
    forM_ (header ++ concatMap commentP modules) $ \c ->
        when (any (`elem` "\r\n") c) (Left c)
  where
    moduleP m = ident (vm_name m) : concatMap argP (concatMap fst (vm_ports m)) ++
                concatMap itemP (vm_body m)
    commentP m = vm_comments m ++ concatMap snd (vm_ports m) ++ concatMap commentItem (vm_body m)
    commentItem (VMComment c y) = c ++ commentItem y
    commentItem (VMRegGroup _ _ c y) = c ++ commentItem y
    commentItem (VMGroup _ yss) = concatMap (concatMap commentItem) yss
    commentItem (VMStmt _ (VAssert _ es)) = [s | VEString s <- es] -- printed unquoted
    commentItem _ = []

-- Walking each body in print order, every identifier an expression, lvalue,
-- event, instance connection or parameter uses must already be declared by a
-- port, a parameter, an import-DPI function, or an earlier VMDecl/VVDWire,
-- VMFunction name or instance name. System names ($...) are never declared;
-- function bodies are opaque.
declaredBeforeUse :: VProgram -> Either String ()
declaredBeforeUse (VProgram modules dpis _) = mapM_ modul modules
  where
    imported = [getVIdString n | VDPI n _ _ _ _ <- dpis]
    modul m = foldM_ item (S.fromList (imported ++ concatMap portNames (concatMap fst (vm_ports m)))) (vm_body m)
    portNames (VAInout i j _) = getVIdString i : maybe [] ((:[]) . getVIdString) j
    portNames a = [getVIdString (vargName a)]
    name = getVIdString . vvName
    item declared x = case x of
        VMDecl (VVDecl _ r vs) -> used declared (rangeP r) >> return (foldr (S.insert . name) declared vs)
        VMDecl (VVDWire r v e) -> used declared (rangeP r ++ exprP e) >> return (S.insert (name v) declared)
        VMInst _ i ps cs -> used declared (concatMap conn ps ++ concatMap conn cs) >>
                            return (S.insert (getVIdString i) declared)
        VMAssign l e -> used declared (lvalueP l ++ exprP e) >> return declared
        VMStmt _ s -> used declared (stmtP s) >> return declared
        VMComment _ y -> item declared y
        VMRegGroup _ _ _ y -> item declared y
        VMGroup _ yss -> foldM item declared (concat yss)
        VMFunction (VFunction n _ _ _) -> return (S.insert (getVIdString n) declared)
        VMDPI (VDPI n _ _ _ _) -> return (S.insert (getVIdString n) declared)
    conn (_,e) = maybe [] exprP e
    used declared printed = case [s | Ident s <- printed, take 1 s /= "$", not (S.member s declared)] of
        [] -> Right ()
        s:_ -> Left s

-- The gates run before any byte is written; identities are validated once
-- before writing and once more after the external filters have run.
emitVerilog :: VerilogInfo -> FilePath -> VProgram -> IO ()
emitVerilog snapshot path program = do
    unless (fixedPorts (viProgram snapshot) == fixedPorts program) $
        failVerilogInfo "candidate changed module ports or parameters"
    either (failVerilogInfo . ("unprintable identifier or comment: " ++)) return (lexicallyEmittable program)
    either (failVerilogInfo . ("identifier used before its declaration: " ++)) return (declaredBeforeUse program)
    let manifest = viReplay snapshot
        changed = program /= viProgram snapshot
    when (changed && not (null (rmFilters manifest)) && not (viExactRendering snapshot)) $
        failVerilogInfo "ordinary Verilog filter changed AST rendering; post-filter proof required"
    validateVerilogInfo snapshot
    createDirectoryIfMissing True (takeDirectory path)
    absolute <- canonicalizePath path
    overlap <- outputPathsOverlap (map fiPath (snapshotFiles snapshot)) [absolute]
    when overlap $
        failVerilogInfo "output would overwrite a bound capture or snapshot artifact"
    let renderedBytes = TE.encodeUtf8 (T.pack (renderVerilogText snapshot program))
    BS.writeFile absolute renderedBytes
    withCurrentDirectory (rmWorkingDirectory manifest) $
        forM_ (reverse (rmFilters manifest)) $ \(_,filterIdentity) ->
            callProcess (fiPath filterIdentity) [absolute]
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
