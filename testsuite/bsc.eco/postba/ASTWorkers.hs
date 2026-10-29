module Main where
-- Worker processes generate a round's proposals and evaluate its candidates.
-- The mechanics are checked directly (job order, dynamic hand-out, outcome
-- records, failure stops the search, any record type), the proposal order of
-- rule families run separately is checked against the combined generator, and
-- the environment selection is checked; eco_workers.exp checks a complete
-- flow for byte-identical results against the sequential loop.
import Control.Concurrent (threadDelay)
import Control.Exception (IOException,try)
import Control.Monad (forM_,unless)
import Data.List (isSuffixOf,sort)
import System.Directory
import System.Environment (getArgs,setEnv,unsetEnv)
import System.FilePath
import Verilog (VProgram(..))
import ECOASTCost (CostObservation(..),LineCost(..))
import ECOASTSearch
import ECOASTTransform (ASTCandidate(..))
import ECOASTRefinements (balancedCandidateProviders)

check name ok = unless ok (error name)
fails label action = do
    result <- try (action >> return ()) :: IO (Either IOException ())
    check label (case result of Left _ -> True; _ -> False)

job out i = Job ("job-" ++ show i) (out </> ("job-" ++ show i)) (error "the candidate is only for the evaluator") ("hash" ++ show i)
observation i = CostObservation ("ast" ++ show i) ("witness" ++ show i) ("wh" ++ show i) (LineCost i (2*i))

-- Synthetic rule families: each proposes its own labelled candidates.
family name n = candidateProvider [name] [] $ \_ _ _ _ now ->
    [ASTCandidate name (name ++ "-" ++ show i) now [] False [] [] | i <- [1..n :: Int]]
program = VProgram [] [] []

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
    -- a failing evaluation stops the search with its message, as in the sequential loop
    fails "worker failure propagates" (evaluateJobs 2 (\j -> if jobLabel j == "job-5" then ioError (userError "boom") else evaluate j) jobs)
    -- any Read/Show result comes back through the records, such as a family's proposal list
    lists <- runJobs 3 show (\i -> out </> ("list-" ++ show i)) (\i -> return [i,i*i]) [1..5 :: Int]
    check "list results in job order" (lists == [(i,[i,i*i]) | i <- [1..5]])
    -- rule families run one by one reproduce the combined generator's order
    let tree = combineCandidateProviders
                 [family "A" 2,balancedCandidateProviders [family "B" 3,family "C" 1,family "D" 4],family "E" 2]
        labels = map acDescription
        direct features budget = labels (take budget (cpGenerate tree [] features budget program program))
        separately features budget = labels (assembleProposals budget features tree
            [take budget (cpGenerate leaf [] enabled budget program program) | (leaf,enabled) <- proposalLeaves features tree])
        subsets = [["A","B","C","D","E"],["B","D"],["A","E"],["C"],["E","A","D"]]
    check "one family has no parts" (null (cpParts (family "A" 1)))
    check "every enabled family is a leaf" (map (cpFeatures . fst) (proposalLeaves ["A","B","C","D","E"] tree) == [["A"],["B"],["C"],["D"],["E"]])
    check "disabled families are left out" (map (cpFeatures . fst) (proposalLeaves ["D","B"] tree) == [["B"],["D"]])
    check "priority outside, balanced inside"
        (direct ["A","B","C","D","E"] 12 == ["A-1","A-2","B-1","C-1","D-1","B-2","D-2","B-3","D-3","D-4","E-1","E-2"])
    check "budget cuts the merged list" (direct ["A","B","C","D","E"] 4 == ["A-1","A-2","B-1","C-1"])
    check "families run separately propose in the same order"
        (and [direct features budget == separately features budget | features <- subsets, budget <- [0..12]])
    setEnv "ECO_WORKERS" "2"
    fromEnvironment <- searchWorkersFromEnvironment
    check "ECO_WORKERS read" (fromEnvironment == 2)
    setEnv "ECO_WORKERS" "0"
    fails "ECO_WORKERS validated" searchWorkersFromEnvironment
    unsetEnv "ECO_WORKERS"
    unset <- searchWorkersFromEnvironment
    check "unset ECO_WORKERS is sequential" (unset == 1)
    putStrLn "PASS: worker pool mechanics"
