-- Replay accepted artifacts with fresh proofs and authenticated native context.
module Main where
import ECOAST
import ECOASTReplay
import ECO.Replay(FileIdentity(..))
import ECOASTFlow
import ECOASTTranslatedObjective
import ECOASTSearch
import ECOASTCost (CostEnvelope(..))
import System.Directory (doesFileExist)
import ECOASTProof
import ECOASTProofContext
import Control.Monad (unless)
import qualified Data.ByteString as BS
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath ((</>),takeDirectory)
import Text.Read (readMaybe)

main = do
  allArgs <- getArgs
  case allArgs of
    ["--flow",flow,output] -> do
      verified <- replaySelectedFlow flow output
      validateReplay verified
      putStrLn ("Reproved ordinary NEW through selected flow; output=" ++ fiPath (replayRTL verified))
    _ -> replayBranch allArgs

-- This explicitly lower-level operation proves only the supplied branch.
replayBranch allArgs = do
  (objectiveMode,oldPath,args) <- case allArgs of
    "--objective":mode:old:rest | mode `elem` ["translated-raw","translated-normalized"] -> return (Just mode,Just old,rest)
    _ -> return (Nothing,Nothing,allArgs)
  case args of
    [configuration,context,plan,output] -> do
      contents <- readFile configuration
      config <- maybe (die "invalid CaptureFlow configuration") return (readMaybe contents)
      objectiveExists <- doesFileExist (takeDirectory plan </> "objective.plan")
      case (objectiveMode,objectiveExists) of
        (Nothing,True) -> do
          envelopeText <- readFile (takeDirectory plan </> "objective.plan")
          envelope <- maybe (die "invalid objective envelope") return (readMaybe envelopeText)
          unless (costPolicyIdentity envelope=="whole-file-raw-v1")
            (die "this plan uses an explicit objective; pass --objective MODE OLD.eco-ast to verify it")
        _ -> return ()
      snapshot <- readVerilogInfo context
      exact <- verilogRenderingIsExact snapshot
      unless exact (die "branch snapshot does not exactly render its native AST")
      _ <- verifyVerilogInfo snapshot (output ++ ".context-control")
      let pins = childPinsFromInstances (viInstances snapshot)
          gate options directory alpha contracts = runASTProofWithContracts pins pins contracts
            (soYosys options) (soProofSeconds options) directory (soDomain options) alpha
      candidate <- case (objectiveMode,oldPath) of
        (Just mode,Just oldSource) -> do
          old <- readVerilogInfo oldSource
          oldExact <- verilogRenderingIsExact old
          unless oldExact (die "OLD snapshot does not exactly render its native AST")
          _ <- verifyVerilogInfo old (output ++ ".old-context-control")
          let objective=translatedCostProvider (if mode=="translated-raw" then TranslatedRaw else TranslatedNormalized)
                [cfImplementationManifest config] old snapshot
          (result,selected,ordinary) <- replayASTPlanWithCost objective (viProgram old) gate
            (cfSearch config) (renderVerilogText snapshot) plan output (viProgram snapshot)
          validateVerilogInfo old
          putStrLn ("Verified objective " ++ mode ++ ": " ++ show (changedLines selected) ++ " versus ordinary " ++ show (changedLines ordinary))
          return result
        _ -> replayASTPlanWith gate (cfSearch config) (renderVerilogText snapshot)
          plan output (viProgram snapshot)
      let actual = output </> aeModule (viEnvelope snapshot) ++ ".v"
      emitVerilog snapshot actual candidate
      expected <- BS.readFile (output </> "selected.v")
      emitted <- BS.readFile actual
      unless (expected == emitted) (die "branch replay emission differs from the proved renderer")
      validateVerilogInfo snapshot
      putStrLn ("Reproved saved branch plan; output=" ++ (output </> "selected.v"))
    _ -> die "usage: replay.hs --flow QUALIFIED_FLOW FRESH_OUTPUT | [--objective translated-raw|translated-normalized OLD.eco-ast] FLOW.config BRANCH.eco-ast search.plan FRESH_OUTPUT"
