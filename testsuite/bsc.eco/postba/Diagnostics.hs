module Main where

import Control.Exception (SomeException, finally, try)
import Control.Monad (unless)
import qualified Data.ByteString as BS
import ECO.Replay
import ECO.Types
import ECODiagnostics
import ECOASTInvariant (EvidenceJSON(..), parseEvidenceJSON)
import System.Directory (createDirectoryLink, doesDirectoryExist)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.Posix.Files (createLink)

check label ok = unless ok (fail label) >> putStrLn ("PASS: " ++ label)
reject label action = do
    result <- try (action >> return ()) :: IO (Either SomeException ())
    check label (case result of Left _ -> True; Right _ -> False)
field name source = case parseEvidenceJSON source of
    Right (JObject fields) -> lookup name fields
    _ -> Nothing
type Receipt = (String,FileIdentity,FileIdentity,FileIdentity,FileIdentity,
                [FileIdentity],[FileIdentity],Maybe FileIdentity,String)

main = do
    args <- getArgs
    case args of
      ["--kept",plan,snapshot,pre,final,ports,directory] ->
        kept plan snapshot pre final ports directory
      [snapshot,pre,final,ports,receipt,backendDirectory,adapterDirectory,work] ->
        ordinary snapshot pre final ports receipt backendDirectory adapterDirectory work
      _ -> fail "Diagnostics.hs INPUTS | --kept PLAN SNAPSHOT PREOPT FINAL PORTS FRESH_DIRECTORY"

ordinary snapshot pre final ports receipt backendDirectory adapterDirectory work = do
    let backendReport = backendDirectory </> "backend-report.json"
        adapterReport = adapterDirectory </> "adapter-receipt.json"
        backend preFile finalFile portsFile out = writeBackendReport snapshot preFile finalFile portsFile out
    original <- mapM fileIdentity [snapshot,pre,final,ports,receipt,backendReport,adapterReport]
    backendText <- readFile backendReport
    adapterText <- readFile adapterReport
    check "backend report preserves graphs, ports and diagnostic classification"
        (field "proofAuthority" backendText == Just (JBool False) &&
         field "diagnosticOnly" backendText == Just (JBool True) &&
         field "preopt" backendText /= Nothing && field "final" backendText /= Nothing &&
         field "fixedPorts" backendText /= Nothing && field "provenance" backendText /= Nothing)
    check "adapter report checks recorded byte parity without rerunning producers"
        (field "proofAuthority" adapterText == Just (JBool False) &&
         field "ordinaryByteParity" adapterText == Just (JBool True) &&
         field "producerControlsRerun" adapterText == Just (JBool False))

    reject "existing backend report is never replaced" (backend pre final ports backendDirectory)
    reject "existing adapter report is never replaced" (writeAdapterReceiptJSON receipt adapterDirectory)
    createDirectoryLink backendDirectory (work </> "directory-alias")
    reject "symlinked output directory cannot replace a report"
        (backend pre final ports (work </> "directory-alias"))
    createLink pre (work </> "input-hardlink")
    reject "hardlinked input cannot become an output directory"
        (backend pre final ports (work </> "input-hardlink"))
    reject "typed receipt cannot become an output directory" (writeAdapterReceiptJSON receipt receipt)

    graph <- read <$> readFile pre :: IO EcoGraph
    let invalidGraph = work </> "invalid-preopt"
        otherModule = work </> "other-module"
        badPorts = work </> "bad-ports"
    writeFile invalidGraph (show (graph {egNodes=egNodes graph ++ take 1 (egNodes graph)}))
    reject "malformed typed graphs are rejected"
        (backend invalidGraph final ports (work </> "invalid-graph-output"))
    writeFile otherModule (show (graph {egModule="anotherModule"}))
    reject "graphs from another module are rejected"
        (backend otherModule final ports (work </> "other-module-output"))
    writeFile badPorts "different ports\n"
    reject "port sidecar must agree with checked native information"
        (backend pre final badPorts (work </> "bad-ports-output"))

    (schema,source,producer,adapter,adapted,oldOutputs,newOutputs,relocation,scope) <-
        read <$> readFile receipt :: IO Receipt
    let badReceipt = work </> "bad-receipt"
    writeFile badReceipt (show (schema,source,producer {fiSHA256=replicate 64 '0'},
        adapter,adapted,oldOutputs,newOutputs,relocation,scope))
    reject "receipt producer must match the bound source capture"
        (writeAdapterReceiptJSON badReceipt (work </> "bad-receipt-output"))
    control <- case oldOutputs of
      first:_ -> return (fiPath first)
      [] -> fail "fixture has no original backend control"
    bytes <- BS.readFile control
    (do BS.appendFile control (BS.singleton 0)
        reject "changed backend control bytes invalidate receipt rendering"
            (writeAdapterReceiptJSON receipt (work </> "changed-control-output")))
      `finally` BS.writeFile control bytes
    invalidOutput <- doesDirectoryExist (work </> "changed-control-output")
    check "rejected inputs produce no diagnostic output" (not invalidOutput)
    after <- mapM (fileIdentity . fiPath) original
    check "reports and source artifacts remain byte-identical after rejected writes" (original == after)

kept planPath snapshot pre final ports directory = do
    plan <- read <$> readFile planPath :: IO EcoBackendPlan
    let requested = map (\(name,_,_) -> JString name) (ebpKeepDefinitions plan)
    check "kept diagnostic fixture requests a real helper" (not (null requested))
    result <- writeBackendReport snapshot pre final ports directory
    report <- readFile (fiPath result)
    check "kept report preserves requested helper names"
        (field "keeps" report == Just (JArray requested))
    planIdentity <- fileIdentity planPath
    check "kept report identifies its bound plan" (case field "planIdentity" report of
        Just (JObject fields) -> lookup "sha256" fields == Just (JString (fiSHA256 planIdentity))
        _ -> False)
    graph <- read <$> readFile pre :: IO EcoGraph
    let changed = directory </> "changed-preopt"
    writeFile changed (show (graph {egReserved="diagnostic_probe":egReserved graph}))
    reject "kept report rejects another structurally valid preoptimization graph"
        (writeBackendReport snapshot changed final ports (directory </> "rejected"))
