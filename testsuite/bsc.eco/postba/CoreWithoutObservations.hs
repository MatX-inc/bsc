-- A core capture carries no observation companion, so optional export
-- completes an explicit unavailable report without invoking a proof solver.
-- A producer runtime writes a companion; without a solver nothing is proved,
-- so the same export still records zero facts. Deferral never runs it.
module Main where

import Control.Monad (unless)
import Data.IORef
import Data.List (isInfixOf)
import ECO.Replay
import ECO.VerilogInfo
import ECOASTInvariant
import ECOInvariantExport
import ECOJSON
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
    companion <- doesFileExist (fiPath (aeCapture (viEnvelope snapshot)) ++ ".evidence")
    let availability = if companion then "captured" else "unavailable-in-capture"
    putStrLn ("observation companion present: " ++ show companion)
    let solver = directory </> "solver-must-not-run"
    solverExists <- doesFileExist solver
    check "missing-evidence test has no available solver" (not solverExists)
    report <- consumeASTEvidence solver 1 (directory </> "inspection") snapshot []
    check "observation availability follows the capture's producer"
        (irAvailability report == availability)
    check "no observations or no solver supply no assertions"
        (null (invariantAssertions report) && (companion || null (irRows report)))

    let exported = directory </> "exported"
    runOptionalInvariantExport ExportInvariants exported $ do
        writeInvariantOutputs exported report
        fileIdentity (exported </> "report.json")
    status <- readFile (exported </> "status.json")
    check "optional report production completes without claiming evidence"
        (jsonField "status" status == Just (JString "completed"))
    rendered <- readFile (exported </> "report.json")
    check "completed report records the capture's availability and zero facts"
        (jsonField "availability" rendered == Just (JString availability) &&
         jsonField "assertions" rendered == Just (JNumber "0") &&
         jsonField "assumptionsExported" rendered == Just (JNumber "0"))
    assertions <- readFile (exported </> "assertions.svh")
    check "export without proved facts writes no assertion statements"
        (not ("assert (" `isInfixOf` assertions))

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
