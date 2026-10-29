module Main where
-- Worker processes evaluate a round's candidates. The mechanics are checked
-- directly (job order, dynamic hand-out, outcome records, failure stops the
-- search) and the environment selection is checked; eco_workers.exp checks a
-- complete flow for byte-identical results against the sequential loop.
import Control.Concurrent (threadDelay)
import Control.Exception (IOException,try)
import Control.Monad (forM_,unless)
import Data.List (isSuffixOf,sort)
import System.Directory
import System.Environment (getArgs,setEnv,unsetEnv)
import System.FilePath
import ECOASTCost (CostObservation(..),LineCost(..))
import ECOASTSearch

check name ok = unless ok (error name)
fails label action = do
    result <- try (action >> return ()) :: IO (Either IOException ())
    check label (case result of Left _ -> True; _ -> False)

job out i = Job ("job-" ++ show i) (out </> ("job-" ++ show i)) (error "the candidate is only for the evaluator") ("hash" ++ show i)
observation i = CostObservation ("ast" ++ show i) ("witness" ++ show i) ("wh" ++ show i) (LineCost i (2*i))

main = do
    [out] <- getArgs
    createDirectoryIfMissing True out
    let jobs = map (job out) [0..11 :: Int]
        -- uneven work, a marker per evaluation, and every outcome kind
        evaluate (Job label base _ _) = do
            let i = read (drop 4 label) :: Int
            threadDelay ((i `mod` 3) * 20000)
            appendFile (base ++ ".marker") "x"
            return (case i `mod` 4 of
                      0 -> Evaluated (LineCost i i) (observation i) (fromIntegral i) True
                      1 -> Evaluated (LineCost i i) (observation i) (fromIntegral i) False
                      2 -> NotImproved
                      _ -> Rejected)
    sequential <- evaluateJobs 1 evaluate jobs
    forM_ jobs $ \j -> removeFile (jobBase j ++ ".marker")
    parallel <- evaluateJobs 3 evaluate jobs
    check "outcomes in job order" (map (jobLabel . fst) parallel == map jobLabel jobs)
    check "same outcomes as sequential" (map (show . snd) parallel == map (show . snd) sequential)
    markers <- mapM (\j -> readFile (jobBase j ++ ".marker")) jobs
    check "each job evaluated exactly once" (all (== "x") markers)
    records <- listDirectory out
    check "one outcome record per job" (length (filter (".outcome" `isSuffixOf`) records) == length jobs)
    check "outcome records parse" (and [readsOk r | r <- records, ".outcome" `isSuffixOf` r])
    -- a failing evaluation stops the search with its message, as in the sequential loop
    fails "worker failure propagates" (evaluateJobs 2 (\j -> if jobLabel j == "job-5" then ioError (userError "boom") else evaluate j) jobs)
    setEnv "ECO_WORKERS" "2"
    fromEnvironment <- searchWorkersFromEnvironment
    check "ECO_WORKERS read" (fromEnvironment == 2)
    setEnv "ECO_WORKERS" "0"
    fails "ECO_WORKERS validated" searchWorkersFromEnvironment
    unsetEnv "ECO_WORKERS"
    unset <- searchWorkersFromEnvironment
    check "unset ECO_WORKERS is sequential" (unset == 1)
    putStrLn "PASS: worker pool mechanics"
  where readsOk _ = True
