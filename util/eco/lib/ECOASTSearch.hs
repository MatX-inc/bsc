-- Bounded external search over retained Verilog ASTs. Candidate generation and
-- equivalence are separate modules. Printed RTL is used only for the objective
-- and by the external SAT tool, never parsed back into an ECO representation.
module ECOASTSearch
    ( SearchOptions(..), SearchResult(..), ASTPlan(..), ASTStep(..), LineCost(..)
    , ProofRunner, defaultSearchOptions, SearchInput(..), SearchRequest(..)
    , defaultSearchRequest, searchWorkersFromEnvironment, optimizeAST
    , Job(..), Evaluation(..), evaluateJobs
    , CandidateGenerator, CandidateProvider(..), defaultCandidateProvider
    , combineCandidateProviders, CostProvider(..), wholeFileCostProvider
    , replayASTPlanWithCost
    , replayASTPlan, replayASTPlanWith, lineCost, changedLines
    ) where

import Control.Exception (IOException, SomeException, try)
import Control.Monad (foldM, forM, forM_, unless, when)
import qualified Data.ByteString as BS
import Data.List (minimumBy, nub)
import qualified Data.Map as M
import Data.Ord (comparing)
import qualified Data.Set as S
import System.Directory
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))
import System.FilePath
import System.IO (BufferMode(..), hClose, hFlush, hGetLine, hIsEOF, hPutStrLn, hSetBuffering, stderr, stdout)
import System.Posix.IO (closeFd, createPipe, fdToHandle)
import System.Posix.Process (exitImmediately, forkProcess, getProcessStatus)
import Text.Read (readMaybe)
import BinData (encode,decode)
import GenABin ()
import Verilog
import ECORename (renameInternalSignals)
import ECO.Replay (fileIdentity,FileIdentity(..))
import ECOASTProof
import ECOASTTransform
import ECOASTCost

data SearchOptions = SearchOptions
    { soDomain :: ProofDomain, soFeatures :: [String], soRounds :: Int
    , soCandidates :: Int, soYosys :: FilePath, soProofSeconds :: Int
    } deriving (Eq,Read,Show)
defaultSearchOptions :: SearchOptions
defaultSearchOptions = SearchOptions Binary
    ["F" ++ (if n < 10 then "0" else "") ++ show n | n <- [1::Int ..20]]
    8 64 "yosys" 30

data ASTStep = ASTStep
    { stepFeature :: String, stepDescription :: String
    , stepBefore :: String, stepAfter :: String, stepArtifact :: FilePath
    , stepArtifactHash :: String, stepAlpha :: [(String,String)]
    , stepBeforeCost :: LineCost, stepAfterCost :: LineCost
    , stepOperations :: [(String,[(String,String)])]
    , stepContractsArtifact :: FilePath, stepContractsHash :: String
    } deriving (Eq,Read,Show)
data ASTPlan = ASTPlan
    { planSchema :: Int, planOptions :: SearchOptions
    , planInput :: String, planOutput :: String, planRenderer :: String
    , planSteps :: [ASTStep]
    } deriving (Eq,Read,Show)
data SearchResult = SearchResult
    { srProgram :: VProgram, srCost :: LineCost, srOrdinaryCost :: LineCost
    , srObjectiveCost :: LineCost, srOrdinaryObjectiveCost :: LineCost
    , srCostEnvelope :: CostEnvelope
    , srPlan :: ASTPlan, srStop :: String, srRolledBack :: Bool
    }

-- Each input carries its own renderer: OLD and CURRENT may have different
-- authenticated headers. The objective measures separate witnesses and never
-- replaces either complete RTL renderer.
data SearchInput = SearchInput
    { searchProgram :: VProgram, searchRender :: VProgram -> String }

data SearchRequest = SearchRequest
    { searchOptions :: SearchOptions, searchDirectory :: FilePath
    , searchOld :: SearchInput, searchCurrent :: SearchInput
    , searchProvider :: CandidateProvider, searchProof :: ProofRunner
    , searchContracts :: [ASTContract], searchRenames :: [(String,String)]
    , searchObjective :: CostProvider
    -- Worker processes that evaluate a round's candidates (1 = in this
    -- process). Never part of a receipt: the result does not depend on it.
    , searchWorkers :: Int
    }

-- Select the proof domain explicitly in options. Custom policy changes only
-- the named fields it supplies; all candidates still use the same proof gate.
defaultSearchRequest :: SearchOptions -> FilePath -> SearchInput -> SearchInput -> SearchRequest
defaultSearchRequest options directory old current = SearchRequest
    { searchOptions=options, searchDirectory=directory
    , searchOld=old, searchCurrent=current
    , searchProvider=defaultCandidateProvider, searchProof=defaultProof
    , searchContracts=[], searchRenames=[]
    , searchObjective=wholeFileCostProvider (searchRender old) (searchRender current)
    , searchWorkers=1
    }

-- ECO_WORKERS names how many worker processes evaluate candidates; a build
-- rule sets it from the CPU count its action declares. Unset or empty keeps
-- the sequential loop.
searchWorkersFromEnvironment :: IO Int
searchWorkersFromEnvironment = do
    value <- lookupEnv "ECO_WORKERS"
    case value of
      Nothing -> return 1
      Just "" -> return 1
      Just text -> case readMaybe text of
        Just n | n >= 1 && n <= 256 -> return n
        _ -> ioError (userError ("ECO AST search: ECO_WORKERS must be an integer from 1 to 256, not " ++ show text))

-- Context (including native child pin metadata) comes from the checked capture
-- supplied by the caller, never from an editable candidate or plan artifact.
type ProofRunner = SearchOptions -> FilePath -> [(String,String)] -> [ASTContract] ->
                   VProgram -> VProgram -> IO ProofResult
defaultProof :: ProofRunner
defaultProof options directory alpha contracts =
    runASTProofWithContracts [] [] contracts (soYosys options) (soProofSeconds options)
                            directory (soDomain options) alpha

-- Policy is supplied by bluehs callers, independently of the frozen compiler
-- and SDK. Sources identify caller-supplied rule modules/data; they are audited
-- inputs, not a claim that the runner can discover all Haskell dependencies.
-- The generator receives active contracts, enabled feature names, a proposal
-- budget, OLD, and CURRENT. It only proposes: the runner owns proof and replay.
type CandidateGenerator = [ASTContract] -> [String] -> Int -> VProgram -> VProgram -> [ASTCandidate]
data CandidateProvider = CandidateProvider
    { cpFeatures :: [String], cpSources :: [FilePath]
    , cpGenerate :: CandidateGenerator
    }

defaultCandidateProvider :: CandidateProvider
defaultCandidateProvider = CandidateProvider (soFeatures defaultSearchOptions) []
    generateASTCandidatesWithContracts

-- The caller chooses ordering (and therefore budget priority). Duplicate
-- feature registrations are rejected by search rather than silently shadowed.
combineCandidateProviders :: [CandidateProvider] -> CandidateProvider
combineCandidateProviders providers = CandidateProvider
    (concatMap cpFeatures providers) (nub (concatMap cpSources providers)) $ \contracts features budget old current ->
      take budget $ concat
        [cpGenerate provider contracts enabled budget old current |
         provider <- providers, let enabled=filter (`elem` cpFeatures provider) features,
         not (null enabled)]

require :: Bool -> String -> IO ()
require yes message = unless yes (ioError (userError ("ECO AST search: " ++ message)))

hashFile :: FilePath -> IO String
hashFile = fmap fiSHA256 . fileIdentity
writeAST :: FilePath -> VProgram -> IO String
writeAST path program = BS.writeFile path (BS.pack (encode program)) >> hashFile path
samePorts :: VProgram -> VProgram -> Bool
samePorts (VProgram a _ _) (VProgram b _ _) =
    [(vm_name m,vm_ports m) | m <- a] == [(vm_name m,vm_ports m) | m <- b]

-- One request names independent proposal, proof and scoring policies. Costs in
-- srCost/srOrdinaryCost and ASTPlan remain complete-file costs for saved-plan
-- compatibility; srObjectiveCost/srOrdinaryObjectiveCost drive selection.
optimizeAST :: SearchRequest -> IO SearchResult
optimizeAST request = do
    require (all (>0) [soRounds options,soCandidates options,soProofSeconds options]) "nonpositive budget"
    require (not (any null (cpFeatures provider)) &&
             length (cpFeatures provider) == length (nub (cpFeatures provider))) "duplicate or empty feature registration"
    require (all (`elem` cpFeatures provider) (soFeatures options)) "unknown feature"
    ruleInputs <- mapM fileIdentity (nub (cpSources provider))
    require (samePorts old ordinary) "old/new module ports differ"
    exists <- doesPathExist directory
    require (not exists) "output directory already exists"
    createDirectoryIfMissing True (directory </> "steps")
    writeFile (directory </> "candidate-provider.report")
        (show (cpFeatures provider,ruleInputs) ++ "\n")
    writeFile (directory </> "old.v") (renderOld old)
    writeFile (directory </> "ordinary-new.v") (render ordinary)
    originalHash <- writeAST (directory </> "ordinary-new.ast") ordinary
    rendererHash <- hashFile (directory </> "ordinary-new.v")
    originalCost <- lineCost (directory </> "old.v") (directory </> "ordinary-new.v")
    costContext <- prepareCost objective directory old
    ordinaryScore <- scoreCost costContext objective "ordinary" ordinary
    let initialPlan = ASTPlan 2 options originalHash originalHash rendererHash []
        prove label relation obligations a b = runner options (directory </> label) relation obligations a b
        save before after cost nextCost plan candidate relation = do
            let index = length (planSteps plan) + 1
                relative = "steps" </> show index ++ ".ast"
                contractPath = "steps" </> show index ++ ".contracts"
            h <- writeAST (directory </> relative) after
            writeASTContracts (directory </> contractPath) (acContracts candidate)
            contractHash <- hashFile (directory </> contractPath)
            let step = ASTStep (acFeature candidate) (acDescription candidate)
                       before h relative h relation cost nextCost (acOperations candidate) contractPath contractHash
            return plan { planOutput=h,planSteps=planSteps plan ++ [step] }
    (start,startCost,startScore,startPlan,startScores) <- if null renames || "F01" `notElem` soFeatures options
      then return (ordinary,originalCost,ordinaryScore,initialPlan,[ordinaryScore])
      else do
        named <- either (ioError . userError) return
                    (renameInternalSignals (map fst renames) renames ordinary)
        proof <- prove "proof-names" [(b,a) | (a,b) <- renames] [] ordinary named
        require (proofPassed proof) ("initial name proof failed: " ++ proofStatus proof)
        writeFile (directory </> "named.v") (render named)
        nextCost <- lineCost (directory </> "old.v") (directory </> "named.v")
        let candidate = ASTCandidate "F01" "External typed name correspondence" named
                          [("rename",renames)] False [] []
        plan <- save originalHash named originalCost nextCost initialPlan candidate [(b,a) | (a,b) <- renames]
        namedScore <- scoreCost costContext objective "named" named
        return (named,nextCost,namedScore,plan,[ordinaryScore,namedScore])
    (final,finalCost,finalScore,finalPlan,finalScores,stop) <- rounds costContext prove save 1 start startCost startScore startPlan startScores S.empty
    let rollback = changedLines (costValue finalScore) > changedLines (costValue ordinaryScore)
        chosen = if rollback then ordinary else final
        cost = if rollback then originalCost else finalCost
        plan = if rollback then initialPlan else finalPlan
        score = if rollback then ordinaryScore else finalScore
        scores = if rollback then [ordinaryScore] else finalScores
    mapM_ (\identity -> do
      actual <- fileIdentity (fiPath identity)
      require (actual == identity) "candidate provider input changed during search") ruleInputs
    when rollback $ writeFile (directory </> "explored.plan") (show finalPlan ++ "\n")
    writeFile (directory </> "selected.v") (render chosen)
    finalHash <- writeAST (directory </> "selected.ast") chosen
    require (finalHash == planOutput plan) "selected AST hash mismatch"
    writeFile (directory </> "search.plan") (show plan ++ "\n")
    envelope <- sealCost costContext objective (directory </> "search.plan") scores
    writeFile (directory </> "summary.txt") $ unlines
        ["objective_policy=" ++ costPolicy objective,
         "ordinary_changed_lines=" ++ show (changedLines (costValue ordinaryScore)),
         "selected_changed_lines=" ++ show (changedLines (costValue score)),
         "whole_file_ordinary_changed_lines=" ++ show (changedLines originalCost),
         "whole_file_selected_changed_lines=" ++ show (changedLines cost),
         "accepted_steps=" ++ show (length (planSteps plan)),
         "stop=" ++ stop,"rolled_back=" ++ show rollback,
         "global_minimum_proved=False"]
    return SearchResult
        { srProgram=chosen, srCost=cost, srOrdinaryCost=originalCost
        , srObjectiveCost=costValue score, srOrdinaryObjectiveCost=costValue ordinaryScore
        , srCostEnvelope=envelope, srPlan=plan, srStop=stop, srRolledBack=rollback
        }
  where
    options = searchOptions request
    directory = searchDirectory request
    SearchInput old renderOld = searchOld request
    SearchInput ordinary render = searchCurrent request
    provider = searchProvider request
    runner = searchProof request
    contracts = searchContracts request
    renames = searchRenames request
    objective = searchObjective request
    rounds costContext prove save n current cost score plan scores visited
      | n > soRounds options = return (current,cost,score,plan,scores,"round-limit")
      | otherwise = do
          let activeContracts = if "F01" `elem` soFeatures options then renameASTContracts renames contracts else contracts
              generated = take (soCandidates options+1) $
                cpGenerate provider activeContracts (soFeatures options) (soCandidates options+1) old current
              limited = length generated > soCandidates options
          -- Admission stays sequential: it writes each candidate's AST and
          -- dedupes proof keys in proposal order, so which candidates get
          -- evaluated never depends on how they are evaluated.
          (_,admitted,visited') <- foldM (admitCandidate n (planOutput plan))
              (0 :: Int,[],visited) (take (soCandidates options) generated)
          outcomes <- evaluateJobs (searchWorkers request) (evaluateCandidate costContext prove current score) (reverse admitted)
          let accepted = [((changedLines (costValue nextScore),size,jobHash job),jobCandidate job,nextCost,nextScore,jobHash job)
                         | (job,Evaluated nextCost nextScore size True) <- outcomes]
          case accepted of
            [] -> return (current,cost,score,plan,scores,if limited then "candidate-limit" else "no-improving-proved-candidate")
            _ -> do
              let (_,candidate,nextCost,nextScore,h) = minimumBy (comparing (\(rank,_,_,_,_) -> rank)) accepted
              plan' <- save (planOutput plan) (acProgram candidate) cost nextCost plan candidate (acStateMap candidate)
              require (planOutput plan' == h) "candidate AST changed during proof"
              rounds costContext prove save (n+1) (acProgram candidate) nextCost nextScore plan' (scores++[nextScore]) visited'
    admitCandidate roundNumber referenceHash (index,admitted,visited) candidate = do
        let label = "candidate-" ++ show roundNumber ++ "-" ++ show index
            base = directory </> label
            program = acProgram candidate
        require (acFeature candidate `elem` soFeatures options) "provider emitted a disabled or unregistered feature"
        if (soDomain options == StrictFourState && acBinaryOnly candidate) || not (samePorts ordinary program)
          then return (index+1,admitted,visited)
          else do
            h <- writeAST (base ++ ".ast") program
            -- Proof failures belong to a reference/candidate/certificate
            -- combination, not to the candidate AST alone. Another rule can
            -- propose the same AST with a valid state relation or obligation.
            let proofKey=(referenceHash,h,acStateMap candidate,show (acContracts candidate))
            if proofKey `S.member` visited then return (index+1,admitted,visited) else
              return (index+1,Job label base candidate h:admitted,S.insert proofKey visited)
    -- The expensive part of one candidate: render, score, prove. Everything it
    -- makes is a file under the search directory, so it runs unchanged in a
    -- worker process, and the outcome record is all that comes back.
    evaluateCandidate costContext prove current score job = do
        let Job label base candidate _ = job
            program = acProgram candidate
        writeFile (base ++ ".v") (render program)
        nextCost <- lineCost (directory </> "old.v") (base ++ ".v")
        nextScore <- scoreCost costContext objective label program
        if changedLines (costValue nextScore) >= changedLines (costValue score) then return NotImproved else do
          outcome <- try (prove (label ++ "-proof") (acStateMap candidate) (acContracts candidate) current program)
              :: IO (Either IOException ProofResult)
          case outcome of
            Left err -> writeFile (base ++ ".rejected") (show err) >> return Rejected
            Right proof -> do
              writeFile (base ++ ".proof.json") (renderProofResult proof)
              size <- getFileSize (directory </> costWitness nextScore)
              return (Evaluated nextCost nextScore size (proofPassed proof))

-- One admitted candidate of a round, in proposal order.
data Job = Job { jobLabel :: String, jobBase :: FilePath, jobCandidate :: ASTCandidate, jobHash :: String }

-- What evaluating a candidate produced: exactly the data the round's selection
-- needs. Proof details and witnesses stay on disk, where the sequential loop
-- also leaves them.
data Evaluation = NotImproved | Rejected | Evaluated LineCost CostObservation Integer Bool | Failed String
    deriving (Read, Show)

-- Evaluate a round's admitted candidates, in worker processes when asked for.
-- The workers are forked after admission, so each inherits the loaded programs
-- and cost context without reloading anything. A worker takes job indices from
-- its own pipe, writes the candidate's files and a one-line outcome record, and
-- reports the index on the shared completion pipe; the parent hands the next
-- job to whichever worker finished, so uneven proof times balance out.
-- Outcomes come back in job order and selection is unchanged, so the result
-- is byte-identical to the sequential loop. No AST crosses a process boundary.
evaluateJobs :: Int -> (Job -> IO Evaluation) -> [Job] -> IO [(Job,Evaluation)]
evaluateJobs workers evaluate jobs
  | workers <= 1 || null jobs = mapM (\job -> (,) job <$> evaluate job) jobs
  | otherwise = do
      hFlush stdout >> hFlush stderr
      let indexed = zip [0 :: Int ..] jobs
          count = min workers (length jobs)
          outcomePath job = jobBase job ++ ".outcome"
          -- Whatever happens to a job, its index is reported, so the parent
          -- never waits for a worker that failed: the failure becomes a Failed
          -- record and stops the search with its message, as an uncaught
          -- exception would have in the sequential loop.
          attempt job = do
              result <- try (evaluate job) :: IO (Either SomeException Evaluation)
              return (either (Failed . show) id result)
      (doneRead,doneWrite) <- createPipe
      pool <- forM [1..count] $ \_ -> do
          (jobRead,jobWrite) <- createPipe
          pid <- forkProcess $ do
              closeFd jobWrite >> closeFd doneRead
              input <- fdToHandle jobRead
              done <- fdToHandle doneWrite
              hSetBuffering done LineBuffering
              let loop = do
                    eof <- hIsEOF input
                    unless eof $ do
                      index <- read <$> hGetLine input
                      let job = jobs !! index
                      outcome <- attempt job
                      written <- try (writeFile (outcomePath job) (show outcome ++ "\n")) :: IO (Either SomeException ())
                      hPutStrLn done (either (const "-1") (const (show index)) written)
                      loop
              loop
              hClose done >> hFlush stdout >> hFlush stderr
              exitImmediately ExitSuccess
          closeFd jobRead
          feed <- fdToHandle jobWrite
          hSetBuffering feed LineBuffering
          return (pid,feed)
      closeFd doneWrite
      done <- fdToHandle doneRead
      let feedJob (_,feed) index = hPutStrLn feed (show index)
          (first,rest) = splitAt count indexed
      forM_ (zip pool first) $ \(w,(index,_)) -> feedJob w index
      let run owner pending inflight
            | inflight == (0 :: Int) = return ()
            | otherwise = do
                finished <- read <$> hGetLine done
                when (finished < 0) $ ioError (userError "ECO AST search: a worker could not record a candidate outcome")
                let w = owner M.! finished
                case pending of
                  (index,_):more -> feedJob w index >> run (M.insert index w (M.delete finished owner)) more inflight
                  [] -> run (M.delete finished owner) [] (inflight-1)
      run (M.fromList [(index,w) | (w,(index,_)) <- zip pool first]) rest (length first)
      forM_ pool $ \(_,feed) -> hClose feed
      forM_ pool $ \(pid,_) -> getProcessStatus True False pid
      hClose done
      forM indexed $ \(_,job) -> do
          text <- readFile (outcomePath job)
          outcome <- maybe (ioError (userError ("ECO AST search: unreadable outcome record for " ++ jobLabel job))) return (readMaybe text)
          case outcome of
            Failed message -> ioError (userError ("ECO AST search: " ++ jobLabel job ++ ": " ++ message))
            _ -> return (job,outcome)

-- Saved candidates are untrusted. Bind every artifact and rerun the complete
-- proof chain; a stored receipt never authorizes replay by itself.
replayASTPlan :: SearchOptions -> (VProgram -> String) -> FilePath -> FilePath -> VProgram -> IO VProgram
replayASTPlan = replayASTPlanWith defaultProof

replayASTPlanWith :: ProofRunner -> SearchOptions -> (VProgram -> String) -> FilePath -> FilePath -> VProgram -> IO VProgram
replayASTPlanWith runner expected render planPath directory ordinary = do
    contents <- readFile planPath
    plan <- maybe (ioError (userError "invalid ECO AST plan")) return (readMaybe contents)
    require (planSchema plan == 2) "unsupported plan schema"
    require (planOptions plan == expected) "plan proof domain/options differ from requested policy"
    exists <- doesPathExist directory; require (not exists) "replay output directory exists"
    createDirectoryIfMissing True directory
    h <- writeAST (directory </> "input.ast") ordinary
    require (h == planInput plan) "plan input AST changed"
    writeFile (directory </> "input.v") (render ordinary)
    rh <- hashFile (directory </> "input.v")
    require (rh == planRenderer plan) "plan renderer/header changed"
    bundle <- canonicalizePath (takeDirectory planPath)
    (output,outputHash) <- foldM (applyStep bundle (planOptions plan)) (ordinary,h)
                               (zip [1::Int ..] (planSteps plan))
    require (outputHash == planOutput plan) "plan final hash mismatch"
    _ <- writeAST (directory </> "selected.ast") output
    writeFile (directory </> "selected.v") (render output)
    return output
  where
    applyStep bundle options (reference,before) (index,step) = do
      require (before == stepBefore step) "broken plan hash chain"
      let relative = stepArtifact step
      require (isRelative relative && ".." `notElem` splitDirectories relative) "artifact escapes plan bundle"
      path <- canonicalizePath (bundle </> relative)
      require (take (length (addTrailingPathSeparator bundle)) path == addTrailingPathSeparator bundle)
              "artifact symlink escapes plan bundle"
      after <- hashFile path
      require (after == stepArtifactHash step && after == stepAfter step) "candidate artifact changed"
      bytes <- BS.readFile path
      let candidate = decode bytes
      require (samePorts ordinary candidate) "candidate changed fixed module ports"
      let contractRelative = stepContractsArtifact step
      require (isRelative contractRelative && ".." `notElem` splitDirectories contractRelative)
              "contracts artifact escapes plan bundle"
      contractPath <- canonicalizePath (bundle </> contractRelative)
      require (take (length (addTrailingPathSeparator bundle)) contractPath == addTrailingPathSeparator bundle)
              "contracts symlink escapes plan bundle"
      contractHash <- hashFile contractPath
      require (contractHash == stepContractsHash step) "contracts artifact changed"
      contracts <- readASTContracts contractPath
      proof <- runner options (directory </> "proof-" ++ show index)
                   (stepAlpha step) contracts reference candidate
      require (proofPassed proof) ("replayed proof failed: " ++ proofStatus proof)
      writeFile (directory </> "proof-" ++ show index ++ ".json") (renderProofResult proof)
      return (candidate,after)

-- Objective-aware replay preserves the existing complete semantic proof gate,
-- then independently checks source identities and every projected score witness.
replayASTPlanWithCost :: CostProvider -> VProgram -> ProofRunner -> SearchOptions ->
    (VProgram -> String) -> FilePath -> FilePath -> VProgram -> IO (VProgram,LineCost,LineCost)
replayASTPlanWithCost objective old runner expected render planPath directory ordinary = do
    planIdentity <- fileIdentity planPath
    output <- replayASTPlanWith runner expected render planPath directory ordinary
    plan <- readFile planPath >>= maybe (fail "invalid objective semantic plan") pure . readMaybe
    actual <- fileIdentity planPath
    require (actual==planIdentity) "semantic plan changed during objective replay"
    bundle <- canonicalizePath (takeDirectory planPath)
    inputHash <- hashFile (directory </> "input.ast")
    require (inputHash==planInput plan) "objective ordinary AST differs from semantic input"
    candidates <- mapM (\(index,step) -> do
      let relative=stepArtifact step
      require (isRelative relative && ".." `notElem` splitDirectories relative) "objective artifact escapes plan bundle"
      path <- canonicalizePath (bundle </> relative)
      require (take (length (addTrailingPathSeparator bundle)) path == addTrailingPathSeparator bundle)
              "objective artifact symlink escapes plan bundle"
      h <- hashFile path
      require (h==stepArtifactHash step && h==stepAfter step) "objective candidate changed after semantic replay"
      bytes <- BS.readFile path
      let program=decode bytes
      decodedHash <- writeAST (directory </> "objective-input-" ++ show index ++ ".ast") program
      require (decodedHash==stepAfter step) "objective candidate bytes changed while read"
      pure program) (zip [1::Int ..] (planSteps plan))
    values <- verifyCostEnvelope objective old planPath (takeDirectory planPath </> "objective.plan")
                  (directory </> "objective-validation") (ordinary:candidates)
    finalIdentity <- fileIdentity planPath
    require (finalIdentity==planIdentity) "semantic plan changed during objective validation"
    returnedHash <- writeAST (directory </> "objective-returned.ast") output
    selectedHash <- hashFile (directory </> "selected.ast")
    require (returnedHash==planOutput plan && selectedHash==returnedHash)
      "returned objective replay output differs from the authenticated final AST"
    writeFile (directory </> "objective-returned.v") (render output)
    returnedRTL <- hashFile (directory </> "objective-returned.v")
    selectedRTL <- hashFile (directory </> "selected.v")
    require (returnedRTL==selectedRTL) "objective replay selected RTL changed"
    return (output,last values,head values)
