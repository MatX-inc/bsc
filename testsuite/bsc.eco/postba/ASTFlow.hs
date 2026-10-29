module Main where
import Control.Monad (unless)
import ECOASTFlow
import ECOASTSearch
import ECOASTProof
import ECO.Replay (FileIdentity(..))
import System.Environment (getArgs)
import System.Directory (getCurrentDirectory, doesFileExist)
import System.FilePath ((</>))
main = do
  [compiler,old,new,library,manifest,yosys,output] <- getArgs
  working <- getCurrentDirectory
  let options = defaultSearchOptions {soDomain=StrictFourState,soFeatures=["F01","F02","F06"],soYosys=yosys}
      config = CaptureFlow compiler "mkFanoutLoss" old new working working
        (Just library) (Just library) output options 120 manifest
  result <- runCaptureFlow defaultFlowPolicy {fpInvariants=DeferInvariants} config
  unless (changedLines (frSelectedCost result) <= changedLines (frOrdinaryCost result))
    (fail "flow worsened ordinary output")
  unless (frSelected result /= "ordinary-rollback")
    (fail "fixture did not exercise a proved improving branch")
  unless (frSchema result == 5 && frObjectivePolicy result == "whole-file-raw-v1" &&
          frConfig result `elem` frInputs result)
    (fail "flow report does not bind its effective configuration")
  verified <- doesFileExist (output </> "verified" </> "upstream-qualification.json")
  unless verified (fail "flow did not finish with the shared fresh verifier")
  putStrLn ("PASS: capture flow selected " ++ frSelected result ++ " at " ++
    show (changedLines (frSelectedCost result)) ++ " lines versus ordinary " ++
    show (changedLines (frOrdinaryCost result)) ++ "; effective configuration " ++
    fiPath (frConfig result) ++ " bound and freshly verified")
