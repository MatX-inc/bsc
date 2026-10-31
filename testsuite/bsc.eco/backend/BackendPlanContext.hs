module Main where

import Control.Exception (IOException, try)
import Control.Monad (forM_, unless)
import Data.List (isInfixOf)
import System.Directory (createDirectoryIfMissing, getModificationTime, setModificationTime)
import System.Posix.Files (createLink)
import Data.Time (UTCTime(..), fromGregorian)
import System.Environment (getArgs)
import System.FilePath ((</>))

import ASyntax
import ECO.Backend
import ECO.Options
import ECO.Replay (fileIdentity, fiSHA256)
import ECO.Types
import Id (mk_homeless_id)
import IntLit (ilDec)

reject :: String -> IO a -> IO ()
reject fragment action = do
    result <- try (action >> return ()) :: IO (Either IOException ())
    unless (either (isInfixOf fragment . show) (const False) result) $
        fail ("expected rejection: " ++ fragment)

-- Both invocations use the same module spelling, just as independent compiler
-- calls can. Their validated plan identities must remain independently owned.
graph :: EcoGraph
graph = EcoGraph 1 "Context" "pre-aopt-typed-definitions"
    [EcoNode "def:helper" (Just "helper") "Bit:1" "definition"
        ["rhs"] Nothing True [] False,
     EcoNode "rhs" Nothing "Bit:1" "integer:1" [] Nothing False [] False]
    ["helper"]

package :: ASPackage
package = ASPackage (mk_homeless_id "Context") False [] [] [] [] [] []
    [ADef helper (ATBit 1) (ASInt helper (ATBit 1) (ilDec 1)) []]
    [] [] [] (ASPSignalInfo [] [] [] [] [] [] [] [] [] []) (ASPCommentInfo [] [])
  where helper = mk_homeless_id "helper"

prepare :: FilePath -> IO (ECOOptions, Maybe CheckedBackendPlan)
prepare directory = do
    createDirectoryIfMissing True directory
    let capture = directory </> "capture"
        preopt = directory </> "Context.eco-preopt"
        path = directory </> "keep.plan"
        opts = defaultECOOptions {ecoReplay=Just capture, ecoBackendPlan=Just path}
    writeFile capture "independently owned backend context\n"
    writeFile preopt (show graph ++ "\n")
    ci <- fileIdentity capture
    pi <- fileIdentity preopt
    writeFile path (show (EcoBackendPlan 1 "Context" (fiSHA256 ci)
        (fiSHA256 pi) [("helper","Bit:1","rhs")]) ++ "\n")
    (_, checked) <- applyBackendPlan opts directory "Context" graph package
    return (opts, checked)

main :: IO ()
main = do
    [root] <- getArgs
    let first = root </> "first"
        second = root </> "second"
        plain = root </> "no-plan"
        finish directory (opts, checked) =
            writeBackendMetadata opts checked directory "Context" graph graph
    a@(optsA, checkedA) <- prepare first
    b@(optsB, checkedB) <- prepare second
    -- These fail with the old singleton: validating B replaces A's plan.
    forM_ [finish first a, finish second b, finish second b, finish first a] id

    let checkpoint = first </> "Context.eco-preopt"
        separate = root </> "separate-graph"
        aliased = root </> "aliased-graph"
        past = UTCTime (fromGregorian 2000 1 1) 0
    setModificationTime checkpoint past
    finish first a
    timestamp <- getModificationTime checkpoint
    unless (timestamp == past) $ fail "deferred observation rewrote the checked preopt checkpoint"
    -- The checked invocation must reuse its authenticated checkpoint, not
    -- reconstruct the preopt graph after optimization/native capture, and
    -- only from the directory holding it: no copy, link or relocation.
    writeBackendMetadata optsA checkedA first "Context"
        (error "checked preopt graph was forced again") graph
    original <- fileIdentity checkpoint
    createDirectoryIfMissing True separate
    reject "does not hold the checked preopt checkpoint" (finish separate a)
    createDirectoryIfMissing True aliased
    createLink checkpoint (aliased </> "Context.eco-preopt")
    reject "does not hold the checked preopt checkpoint" (finish aliased a)
    afterAlias <- fileIdentity checkpoint
    unless (afterAlias == original) $ fail "rejected checkpoint alias changed source bytes"

    checkpointBytes <- readFile checkpoint
    length checkpointBytes `seq` appendFile checkpoint "changed checkpoint\n"
    reject "preopt checkpoint changed after validation" (finish first a)
    damaged <- readFile checkpoint
    unless (damaged == checkpointBytes ++ "changed checkpoint\n") $
        fail "deferred observation silently replaced a changed checkpoint"
    writeFile checkpoint checkpointBytes
    reject "not checked before optimization" (finish first (optsA, Nothing))
    reject "changed after validation" (finish second (optsB, checkedA))
    reject "changed after validation" (finish first (optsA, checkedB))
    reject "changed after validation" $
        writeBackendMetadata optsA checkedA first "Other" graph graph
    reject "without a plan option" (finish first (defaultECOOptions, checkedA))
    let missing = graph {egNodes=[n {enEligible=False} | n <- egNodes graph]}
    reject "keep did not survive" $
        writeBackendMetadata optsA checkedA first "Context" graph missing

    (_, unchecked) <- applyBackendPlan defaultECOOptions plain "Context" graph package
    finish plain (defaultECOOptions, unchecked)
    finish first a

    -- A failed validation cannot poison another call's deferred observations.
    let Just pathB = ecoBackendPlan optsB
    writeFile pathB "invalid plan\n"
    reject "invalid structural plan" $
        applyBackendPlan optsB second "Context" graph package
    finish first a

    -- The owned token still rechecks its input at completion time.
    let Just pathA = ecoBackendPlan optsA
    appendFile pathA "\n"
    reject "changed after validation" (finish first a)
    putStrLn "PASS: independent backend contexts, failed-call isolation, stale inputs, checkpoint reuse and survival guards"
