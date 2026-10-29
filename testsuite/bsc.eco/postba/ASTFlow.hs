module Main where
import Control.Monad (unless)
import ECOASTFlow
import ECOASTSearch
import ECOASTProof
import System.Environment (getArgs)
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
main = do
  [compiler,old,new,library,manifest,output] <- getArgs
  working <- getCurrentDirectory
  let options = defaultSearchOptions {soDomain=StrictFourState,soFeatures=["F01","F02","F06"]}
      config = CaptureFlow compiler "mkFanoutLoss" old new working working
        (Just library) (Just library) Nothing output options 120 manifest Nothing
  writeFile (output ++ ".config") (show config ++ "\n")
  result <- runCaptureFlowWithInvariants DeferInvariants config
  unless (changedLines (frSelectedCost result) <= changedLines (frOrdinaryCost result))
    (fail "flow worsened ordinary output")
  unless (frSelected result /= "ordinary-rollback")
    (fail "fixture did not exercise a proved improving branch")
  putStrLn ("PASS: capture flow selected " ++ frSelected result ++ " at " ++
    show (changedLines (frSelectedCost result)) ++ " lines versus ordinary " ++
    show (changedLines (frOrdinaryCost result)))
