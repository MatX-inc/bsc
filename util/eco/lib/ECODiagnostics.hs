-- Explicit diagnostic projections of typed artifacts. These reports do not
-- authorize transformations, replace replay, or supply proof assumptions.
module ECODiagnostics (writeBackendReport, writeAdapterReceiptJSON) where

import Control.Exception (evaluate)
import Control.Monad (unless)
import Data.Char (ord)
import Data.List (intercalate, nub)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Numeric (showHex)
import System.Directory (createDirectory)
import System.FilePath ((</>))
import Text.Read (readMaybe)

import ECO.Interface (fixedPorts)
import ECO.Replay
import ECO.Types
import ECO.Validate (validateGraph)
import ECO.VerilogInfo

need :: Bool -> String -> IO ()
need ok message = unless ok (ioError (userError ("ECO diagnostic: " ++ message)))

record :: Read a => FilePath -> IO a
record path = do
    source <- readFile path
    _ <- evaluate (length source)
    maybe (ioError (userError ("invalid typed artifact: " ++ path))) return (readMaybe source)

unchanged :: FileIdentity -> IO ()
unchanged expected = do
    actual <- fileIdentity (fiPath expected)
    same <- sameFileIdentity expected actual
    need same ("input changed: " ++ fiPath expected)

manifestFiles :: ReplayManifest -> [FileIdentity]
manifestFiles manifest =
    [rmProducer manifest,rmABin manifest,rmFlags manifest,rmIntern manifest] ++
    rmDependencies manifest ++ map snd (rmFilters manifest) ++ map snd (rmOutputs manifest)

evidenceFiles :: FilePath -> IO [FileIdentity]
evidenceFiles path = do
    evidence <- readReplayEvidence path
    case evidence of
      Nothing -> return []
      Just bound -> do
        identity <- fileIdentity (path ++ ".evidence")
        return [identity,reCapture bound,reABin bound,reSourceCapture bound,
                reObservedProducer bound,rePayload bound]

-- A newly created directory prevents replacing any existing file, including
-- aliases of inputs. Reports never reuse an existing output directory.
writeDiagnostic :: [FileIdentity] -> FilePath -> FilePath -> String -> IO FileIdentity
writeDiagnostic inputs directory name text = do
    let identities = nub inputs
        destination = directory </> name
    _ <- evaluate (length text)
    mapM_ unchanged identities
    overlap <- outputPathsOverlap (map fiPath identities) [destination]
    need (not overlap) "output would overwrite an input artifact"
    createDirectory directory
    writeFile destination text
    mapM_ unchanged identities
    fileIdentity destination

-- The snapshot authenticates the native capture, RTL and optional keep plan.
-- Graph sidecars are structurally checked and their exact identities reported;
-- their contents are not promoted to authenticated compiler facts or proofs.
writeBackendReport :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> IO FileIdentity
writeBackendReport snapshotPath prePath finalPath portsPath directory = do
    snapshot <- readVerilogInfo snapshotPath
    preIdentity <- fileIdentity prePath
    finalIdentity <- fileIdentity finalPath
    portsIdentity <- fileIdentity portsPath
    pre <- record prePath
    final <- record finalPath
    either (ioError . userError) return (validateGraph pre)
    either (ioError . userError) return (validateGraph final)
    let envelope = viEnvelope snapshot
        capture = aeCapture envelope
        moduleName = aeModule envelope
        manifest = viReplay snapshot
    need (egModule pre == moduleName && egModule final == moduleName &&
          egStage pre == "pre-aopt-typed-definitions" &&
          egStage final == "final-typed-definitions") "graph module or checkpoint differs"
    ports <- readFile portsPath
    need (ports == fixedPorts (viProgram snapshot)) "port sidecar differs from the checked Verilog information"
    plan <- case aePlan envelope of
      Nothing -> return Nothing
      Just identity -> do
        candidate <- record (fiPath identity)
        need (ebpSchema candidate == 1 && ebpModule candidate == moduleName &&
              ebpCaptureSHA256 candidate == fiSHA256 capture &&
              ebpPreOptSHA256 candidate == fiSHA256 preIdentity)
             "keep plan differs from capture or preoptimization graph"
        return (Just candidate)
    let requested = maybe [] (map (\(name,_,_) -> name) . ebpKeepDefinitions) plan
        retained = S.fromList [name | node <- egNodes final, enEligible node,
                              enOp node == "definition", Just name <- [enName node]]
    need (length requested == S.size (S.fromList requested) &&
          all (`S.member` retained) requested) "keep names are duplicated or absent from the supplied final graph"
    evidence <- evidenceFiles (fiPath capture)
    evidenceIdentity <- replayEvidenceIdentity (fiPath capture)
    let nodes = M.fromList [(enKey n,n) | n <- egNodes pre]
        uses = M.fromListWith (++) [(operand,[(enKey n,index)]) | n <- egNodes pre,
                                   (index,operand) <- zip [0 :: Int ..] (enOperands n)]
        finalNames = S.fromList [name | n <- egNodes final, Just name <- [enName n]]
        provenance = [object
            [("name",json name),("key",json (enKey n)),("type",json (enType n)),
             ("rhs",strings (enOperands n)),
             ("uses",array [object [("consumer",json consumer),("operand",show index)] |
                            (consumer,index) <- M.findWithDefault [] (enKey n) uses]),
             ("survivesByName",boolean (S.member name finalNames))] |
             n <- M.elems nodes, enOp n == "definition", Just name <- [enName n]]
        text = object
            [("schema","1"),("kind",json "bsc-eco-backend-diagnostic"),
             ("diagnosticOnly","true"),("proofAuthority","false"),
             ("graphBinding",json "supplied typed sidecars; identities and structure checked, no semantic proof"),
             ("module",json moduleName),("snapshotIdentity",identityJSON (viIdentity snapshot)),
             ("capture",identityJSON capture),("evidenceIdentity",maybe "null" identityJSON evidenceIdentity),
             ("planIdentity",maybe "null" identityJSON (aePlan envelope)),
             ("preoptIdentity",identityJSON preIdentity),("finalIdentity",identityJSON finalIdentity),
             ("portsIdentity",identityJSON portsIdentity),
             ("preopt",graphJSON pre),("final",graphJSON final),("keeps",strings requested),
             ("fixedPorts",json ports),("provenance",array provenance)] ++ "\n"
        inputs = [viIdentity snapshot,capture,aeProducer envelope,aePayload envelope,aeOutput envelope,
                  preIdentity,finalIdentity,portsIdentity] ++
                 maybe [] (:[]) (aePlan envelope) ++ manifestFiles manifest ++ evidence
    validateVerilogInfo snapshot
    result <- writeDiagnostic inputs directory "backend-report.json" text
    validateVerilogInfo snapshot
    return result

type AdapterReceipt = (String,FileIdentity,FileIdentity,FileIdentity,FileIdentity,
                       [FileIdentity],[FileIdentity],Maybe FileIdentity,String)

-- This checks the recorded identities and byte-control relationships. It does
-- not rerun either producer and cannot turn a receipt into a semantic proof.
writeAdapterReceiptJSON :: FilePath -> FilePath -> IO FileIdentity
writeAdapterReceiptJSON path directory = do
    receiptIdentity <- fileIdentity path
    (schema,sourceIdentity,originalProducer,adapterProducer,adaptedIdentity,
     originalOutputs,currentOutputs,relocation,scope) <- record path :: IO AdapterReceipt
    need (schema == "bsc-eco-native-adapter-1") "unsupported adapter receipt schema"
    source <- record (fiPath sourceIdentity)
    adapted <- record (fiPath adaptedIdentity)
    originalMatches <- sameFileIdentity originalProducer (rmProducer source)
    adapterMatches <- sameFileIdentity adapterProducer (rmProducer adapted)
    need (originalMatches && adapterMatches) "receipt producer differs from its capture"
    need (rmSchema source == replaySchema && rmSchema adapted == replaySchema &&
          rmTargetRevision source == rmTargetRevision adapted &&
          rmNativeVersion source == rmNativeVersion adapted &&
          rbModuleName (rmBackend source) == rbModuleName (rmBackend adapted) &&
          fiSHA256 (rmABin source) == fiSHA256 (rmABin adapted))
         "source and adapted native capture bindings differ"
    let expected = map (fiSHA256 . snd) (rmOutputs source)
        digests = map fiSHA256
    need (not (null expected) && expected == digests originalOutputs &&
          expected == digests currentOutputs &&
          expected == map (fiSHA256 . snd) (rmOutputs adapted))
         "recorded backend controls differ from captured output bytes"
    sourceEvidence <- evidenceFiles (fiPath sourceIdentity)
    adaptedEvidence <- evidenceFiles (fiPath adaptedIdentity)
    let inputs = [receiptIdentity,sourceIdentity,originalProducer,adapterProducer,adaptedIdentity] ++
                 manifestFiles source ++ manifestFiles adapted ++ originalOutputs ++ currentOutputs ++
                 maybe [] (:[]) relocation ++ sourceEvidence ++ adaptedEvidence
        text = object
            [("schema","1"),("kind",json "bsc-eco-adapter-diagnostic"),
             ("diagnosticOnly","true"),("proofAuthority","false"),
             ("receiptIdentity",identityJSON receiptIdentity),("source",identityJSON sourceIdentity),
             ("originalProducer",identityJSON originalProducer),("adapterProducer",identityJSON adapterProducer),
             ("adaptedCapture",identityJSON adaptedIdentity),
             ("originalControlRelocation",maybe "null" identityJSON relocation),
             ("originalOutputs",array (map identityJSON originalOutputs)),
             ("currentOutputs",array (map identityJSON currentOutputs)),
             ("ordinaryByteParity","true"),("producerControlsRerun","false"),
             ("recordedScope",json scope)] ++ "\n"
    writeDiagnostic inputs directory "adapter-receipt.json" text

graphJSON :: EcoGraph -> String
graphJSON graph = object
    [("schema",show (egSchema graph)),("module",json (egModule graph)),
     ("stage",json (egStage graph)),("reserved",strings (egReserved graph)),
     ("nodes",array (map nodeJSON (egNodes graph)))]
  where
    nodeJSON n = object
        [("key",json (enKey n)),("name",maybe "null" json (enName n)),
         ("type",json (enType n)),("op",json (enOp n)),("operands",strings (enOperands n)),
         ("boundary",maybe "null" json (enBoundary n)),("eligible",boolean (enEligible n)),
         ("contexts",strings (enContexts n)),("opaque",boolean (enOpaque n))]

identityJSON :: FileIdentity -> String
identityJSON i = object [("path",json (fiPath i)),("sha256",json (fiSHA256 i))]
object :: [(String,String)] -> String
object fields = "{" ++ intercalate "," [json key ++ ":" ++ value | (key,value) <- fields] ++ "}"
array :: [String] -> String
array values = "[" ++ intercalate "," values ++ "]"
strings :: [String] -> String
strings = array . map json
boolean :: Bool -> String
boolean True = "true"
boolean False = "false"
json :: String -> String
json source = '"' : concatMap escape source ++ "\""
  where
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape c | ord c < 32 = let h = showHex (ord c) ""
                            in "\\u" ++ replicate (4 - length h) '0' ++ h
             | otherwise = [c]
