-- Deferred typed checkpoint observation, invoked only after normal output and the
-- native capture have been forced.  Default compilation never calls this.
module ECO.Observe (observeECO, checkECOOutputPaths) where

import Control.Monad (unless)
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import ASyntax (APackage, ASPackage)
import ECO.Options
import ECO.Types
import ECO.Graph (extractFinalGraph)
import ECO.Backend (CheckedBackendPlan, writeBackendMetadata)
import ECO.VerilogInfo (writeVerilogInfo)
import ECO.Replay (ReplayManifest, checkReplayOutputPaths, outputPathsOverlap)
import ECO.Namespace
import Verilog (VProgram)

-- Companions live beside the capture being written (capture and adaptation)
-- and otherwise beside the RTL output (plan replay).
observationDirectory :: ECOOptions -> FilePath -> FilePath
observationDirectory opts outputDir = fromMaybe outputDir (ecoCapture opts)

-- Use the same directory selection as the deferred writer; a plan writes its
-- preopt checkpoint there before optimization. Check every path before that
-- first write.
checkECOOutputPaths :: ECOOptions -> FilePath -> ReplayManifest -> FilePath ->
                       String -> [FilePath] -> IO ()
checkECOOutputPaths opts manifestPath manifest outputDir moduleName ordinary = do
    let stem = observationDirectory opts outputDir </> moduleName
        observations = [stem ++ suffix | suffix <-
            [".eco-preopt", ".eco-graph", ".eco-ast", ".eco-ast.bin", ".eco-ast.output.v"]]
        outputs = ordinary ++ observations
    checkReplayOutputPaths manifestPath manifest outputs
    ordinaryOverlap <- outputPathsOverlap ordinary observations
    unless (not ordinaryOverlap) $
        ioError (userError "ECO replay: output would overwrite a bound capture or ordinary output artifact")

observeECO :: ECOOptions -> Maybe CheckedBackendPlan -> FilePath -> String -> APackage -> ASPackage ->
              EcoGraph -> ASPackage -> VProgram -> String -> FilePath -> IO ()
observeECO opts checkedPlan outputDir moduleName scheduled state preGraph final normal header outputPath = do
    let namespace = ecoNamespace normal
        raw = extractFinalGraph scheduled state final
        candidates = [s | n <- egNodes raw, enEligible n, Just s <- [enName n]]
        allowed = ecoEligible candidates normal
        allowedSet = S.fromList allowed
        graph = raw { egReserved = S.toAscList (S.fromList (namespace ++ egReserved raw)),
                      egNodes = [n { enEligible = enEligible n &&
                                    maybe False (`S.member` allowedSet) (enName n) }
                                | n <- egNodes raw] }
        dir = observationDirectory opts outputDir
        stem = dir </> moduleName
    createDirectoryIfMissing True dir
    writeFile (stem ++ ".eco-graph") (show graph ++ "\n")
    writeBackendMetadata opts checkedPlan dir moduleName preGraph graph
    writeVerilogInfo opts dir moduleName header final normal outputPath
