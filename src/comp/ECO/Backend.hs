-- Checked post-native-package operations. The only structural operation is
-- the existing IdP_keep annotation; no old expression is copied into new IR.
module ECO.Backend (CheckedBackendPlan, applyBackendPlan, writeBackendMetadata, checkBackendPorts) where

import Control.Monad (foldM, forM_, unless)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import System.Directory (canonicalizePath, copyFile, createDirectoryIfMissing)
import System.FilePath ((</>))
import Text.Read (readMaybe)

import ASyntax
import Id (getIdString, setKeepId)
import ECO.Options
import ECO.Types
import ECO.Replay (fileIdentity, FileIdentity(..), outputPathsOverlap)
import ECO.Validate (validateGraph)
import ECO.Interface (fixedPorts)
import Verilog

-- A validated plan belongs to one backend invocation. Keep the constructor
-- private and carry only its authenticated input and requests, not the IR.
-- The deferred writer rechecks both the plan and its already-written preopt
-- checkpoint, then checks final survival. It never reconstructs that checkpoint.
data CheckedBackendPlan = CheckedBackendPlan FileIdentity FileIdentity EcoBackendPlan

failBackend :: String -> IO a
failBackend = ioError . userError . ("ECO backend: " ++)

writePreOpt :: FilePath -> String -> EcoGraph -> IO ()
writePreOpt directory moduleName graph = do
    either failBackend return (validateGraph graph)
    createDirectoryIfMissing True directory
    let path = directory </> moduleName ++ ".eco-preopt"
    writeFile path (show graph ++ "\n")

applyBackendPlan :: ECOOptions -> FilePath -> String -> EcoGraph -> ASPackage ->
                    IO (ASPackage, Maybe CheckedBackendPlan)
applyBackendPlan opts directory moduleName graph package = case ecoBackendPlan opts of
    Nothing -> return (package, Nothing)
    Just path -> do
        planIdentity <- fileIdentity path
        contents <- readFile path
        plan <- maybe (failBackend "invalid structural plan") return (readMaybe contents)
        readIdentity <- fileIdentity path
        unless (readIdentity == planIdentity) $ failBackend "structural plan changed while reading"
        capture <- maybe (failBackend "structural plan requires strict native replay")
                         fileIdentity (ecoReplay opts)
        overlap <- outputPathsOverlap [path, fiPath capture]
                       [directory </> moduleName ++ ".eco-preopt"]
        unless (not overlap) $ failBackend "preopt output would overwrite the plan or capture"
        writePreOpt directory moduleName graph
        graphIdentity <- fileIdentity (directory </> moduleName ++ ".eco-preopt")
        unless (ebpSchema plan == 1 && ebpModule plan == moduleName &&
                ebpCaptureSHA256 plan == fiSHA256 capture &&
                ebpPreOptSHA256 plan == fiSHA256 graphIdentity) $
            failBackend "structural plan capture/module/schema/pre-aOpt digest mismatch"
        let requested = ebpKeepDefinitions plan
            names = [n | (n,_,_) <- requested]
            wanted = S.fromList names
            definitions = M.fromList [(getIdString (adef_objid d),d) | d <- aspkg_values package]
            nodes = M.fromList [(n,node) | node <- egNodes graph, Just n <- [enName node]]
            byKey = M.fromList [(enKey node,node) | node <- egNodes graph]
            -- Keep annotations must not preserve an arbitrary don't-care,
            -- opaque foreign result, unresolved reference, or cyclic cone.
            pureCone key = case visit S.empty S.empty key of
                Just _ -> True
                Nothing -> False
            visit active done key
                | S.member key active = Nothing
                | S.member key done = Just done
                | otherwise = case M.lookup key byKey of
                    Nothing -> Nothing
                    Just node | enOpaque node || enOp node == "any" -> Nothing
                              | otherwise -> do
                        finished <- foldM (visit (S.insert key active)) done (enOperands node)
                        return (S.insert key finished)
        unless (length names == S.size wanted) $ failBackend "duplicate keep definition"
        forM_ requested $ \(name,ty,rhs) -> case (M.lookup name definitions, M.lookup name nodes) of
            (Just _,Just node) | enEligible node && not (enOpaque node) &&
                                enBoundary node == Nothing && enOp node == "definition" &&
                                enType node == ty && enOperands node == [rhs] &&
                                pureCone (enKey node) -> return ()
            _ -> failBackend ("keep request is unknown, protected, non-scalar, opaque or has stale typed RHS: " ++ name)
        let keep d | S.member (getIdString (adef_objid d)) wanted =
                        d { adef_objid = setKeepId (adef_objid d) }
                   | otherwise = d
        return (package { aspkg_values = map keep (aspkg_values package) },
                Just (CheckedBackendPlan planIdentity graphIdentity plan))

checkBackendPorts :: ECOOptions -> FilePath -> String -> VProgram -> IO ()
checkBackendPorts opts directory moduleName program = case ecoBackendPlan opts of
    Nothing -> return ()
    Just _ -> do
        baseline <- readFile (directory </> "ordinary-control" </> moduleName ++ ".eco-ports")
        unless (baseline == fixedPorts program) $ failBackend "structural operation changed module ports or parameters"

writeBackendMetadata :: ECOOptions -> Maybe CheckedBackendPlan -> FilePath -> String ->
                        EcoGraph -> EcoGraph -> VProgram -> IO ()
writeBackendMetadata opts checked directory moduleName pre final program = do
    either failBackend return (validateGraph final)
    plan <- case (ecoBackendPlan opts, checked) of
        (Nothing, Nothing) -> return Nothing
        (Just path, Just (CheckedBackendPlan expected _ plan)) -> do
            current <- fileIdentity path
            unless (current == expected && ebpModule plan == moduleName) $
                failBackend "structural plan changed after validation"
            return (Just plan)
        (Just _, Nothing) -> failBackend "structural plan was not checked before optimization"
        (Nothing, Just _) -> failBackend "checked structural plan supplied without a plan option"
    let requested = maybe [] (map (\(n,_,_) -> n) . ebpKeepDefinitions) plan
        retained = S.fromList [name | node <- egNodes final, enEligible node,
                                    enOp node == "definition", Just name <- [enName node]]
    unless (all (`S.member` retained) requested) $
        failBackend "requested keep did not survive as a final eligible scalar definition"
    case checked of
        Nothing -> writePreOpt directory moduleName pre
        Just (CheckedBackendPlan planIdentity checkpoint _) ->
            reusePreOptCheckpoint planIdentity checkpoint directory moduleName
    writeFile (directory </> moduleName ++ ".eco-ports") (fixedPorts program)

-- A keep request already serialized this exact graph before optimization. The
-- usual deferred destination is that same file; only a separate graph directory
-- needs a copy. Preserve the bytes and reject changed inputs or output aliases.
reusePreOptCheckpoint :: FileIdentity -> FileIdentity -> FilePath -> String -> IO ()
reusePreOptCheckpoint plan checkpoint directory moduleName = do
    current <- fileIdentity (fiPath checkpoint)
    unless (current == checkpoint) $ failBackend "preopt checkpoint changed after validation"
    let output = directory </> moduleName ++ ".eco-preopt"
    destination <- canonicalizePath output
    unless (destination == fiPath checkpoint) $ do
        overlap <- outputPathsOverlap [fiPath plan, fiPath checkpoint] [destination]
        unless (not overlap) $ failBackend "checkpoint output would overwrite the plan or preopt input"
        createDirectoryIfMissing True directory
        copyFile (fiPath checkpoint) destination
        copied <- fileIdentity destination
        after <- fileIdentity (fiPath checkpoint)
        unless (fiSHA256 copied == fiSHA256 checkpoint && after == checkpoint) $
            failBackend "preopt checkpoint changed while copying"
