-- A fresh core capture has no PR244 observations. Optional export can still
-- complete an explicit unavailable report, without invoking a proof solver.
module Main where

import Control.Monad (unless)
import Data.IORef
import Data.List (isInfixOf)
import ECO.Replay
import ECO.VerilogInfo
import ECOASTInvariant
import ECOASTProof (readASTContracts)
import ECOInvariantExport
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (getArgs)
import System.FilePath ((</>))

check label condition = do
    unless condition (fail label)
    putStrLn ("PASS: " ++ label)

jsonField name source = case parseEvidenceJSON source of
    Right (JObject fields) -> lookup name fields
    _ -> Nothing

main = do
    [snapshotPath, directory] <- getArgs
    createDirectoryIfMissing True directory
    snapshot <- readVerilogInfo snapshotPath
    evidence <- readReplayEvidence (fiPath (aeCapture (viEnvelope snapshot)))
    check "core capture has no compiler observation companion"
        (case evidence of Nothing -> True; Just _ -> False)
    let solver = directory </> "solver-must-not-run"
    solverExists <- doesFileExist solver
    check "missing-evidence test has no available solver" (not solverExists)
    report <- consumeASTEvidence solver 1 (directory </> "inspection") snapshot []
    check "missing observations are explicitly unavailable"
        (irAvailability report == "unavailable-in-capture")
    check "missing observations supply no rows or contracts"
        (null (irRows report) && null (invariantContracts report))

    let exported = directory </> "exported"
    runOptionalInvariantExport ExportInvariants exported $ do
        writeInvariantOutputs exported report
        fileIdentity (exported </> "report.json")
    status <- readFile (exported </> "status.json")
    check "optional report production completes without claiming evidence"
        (jsonField "status" status == Just (JString "completed"))
    rendered <- readFile (exported </> "report.json")
    check "completed report records unavailable evidence and zero facts"
        (jsonField "availability" rendered == Just (JString "unavailable-in-capture") &&
         jsonField "assertions" rendered == Just (JNumber "0") &&
         jsonField "assumptionsExported" rendered == Just (JNumber "0"))
    contracts <- readASTContracts (exported </> "assertions.ast-contracts")
    assertions <- readFile (exported </> "assertions.svh")
    check "unavailable export writes no assertion contracts or statements"
        (null contracts && not ("assert (" `isInfixOf` assertions))

    ran <- newIORef False
    let deferred = directory </> "deferred"
    runOptionalInvariantExport DeferInvariants deferred
        (writeIORef ran True >> fail "deferred exporter must not run")
    didRun <- readIORef ran
    deferredStatus <- readFile (deferred </> "status.json")
    deferredReport <- doesFileExist (deferred </> "report.json")
    check "explicit deferral does not invoke the exporter or write a report"
        (not didRun && not deferredReport &&
         jsonField "status" deferredStatus == Just (JString "deferred"))
    validateVerilogInfo snapshot
    check "all native source identities remain intact" True
