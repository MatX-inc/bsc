-- Bounded external search over retained Verilog ASTs. Candidate generation and
-- equivalence are separate modules. Printed RTL is used only for the objective
-- and by the external SAT tool, never parsed back into an ECO representation.
module ECOASTSearch
    ( SearchOptions(..), SearchResult(..), ASTPlan(..), ASTStep(..), LineCost(..)
    , ProofRunner, defaultSearchOptions, SearchInput(..), SearchRequest(..)
    , defaultSearchRequest, searchWorkersFromEnvironment, optimizeAST
    , Job(..), Evaluation(..), evaluateJobs, runJobs
    , CandidateGenerator, CandidateProvider(..), ProposalOrder(..), candidateProvider
    , defaultCandidateProvider, combineCandidateProviders, combineCandidateProvidersWith
    , Proposal(..), proposalLeaves, assembleProposals
    , CostProvider(..), wholeFileCostProvider
    , replayASTPlanWithCost
    , replayASTPlan, replayASTPlanWith, lineCost, changedLines
    ) where

import Control.Exception (IOException, SomeException, try)
import Control.Monad (foldM, forM, forM_, unless, when)
import qualified Data.ByteString as BS
import Data.List (minimumBy, nub, transpose)
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
-- A provider is one rule family (no parts) or a combination that generates
-- through its parts and merges their proposals in cpOrder. Search may run the
-- families in worker processes and merge what they proposed the same way, so
-- where a family ran never changes what is proposed or in which order.
data CandidateProvider = CandidateProvider
    { cpFeatures :: [String], cpSources :: [FilePath]
    , cpGenerate :: CandidateGenerator
    , cpParts :: [CandidateProvider], cpOrder :: ProposalOrder
    }
-- Priority takes every proposal of a part before the next part's; balanced
-- gives each part a turn before taking any part's second proposal.
data ProposalOrder = PriorityOrder | BalancedOrder deriving (Eq,Show)

mergeProposals :: ProposalOrder -> [[a]] -> [a]
mergeProposals PriorityOrder = concat
mergeProposals BalancedOrder = concat . transpose

-- One rule family.
candidateProvider :: [String] -> [FilePath] -> CandidateGenerator -> CandidateProvider
candidateProvider features sources generate = CandidateProvider features sources generate [] PriorityOrder

defaultCandidateProvider :: CandidateProvider
defaultCandidateProvider = candidateProvider (soFeatures defaultSearchOptions) []
    generateASTCandidatesWithContracts

-- The caller chooses ordering (and therefore budget priority). Duplicate
-- feature registrations are rejected by search rather than silently shadowed.
combineCandidateProviders :: [CandidateProvider] -> CandidateProvider
combineCandidateProviders = combineCandidateProvidersWith PriorityOrder

combineCandidateProvidersWith :: ProposalOrder -> [CandidateProvider] -> CandidateProvider
combineCandidateProvidersWith order providers = combined
  where
    combined = CandidateProvider (concatMap cpFeatures providers) (nub (concatMap cpSources providers))
                   generate providers order
    generate contracts features budget old current = assembleProposals budget features combined
        [take budget (cpGenerate family contracts enabled budget old current)
        | (family,enabled) <- proposalLeaves features combined]

-- The parts that take part in a generation, each with the features it sees.
enabledParts :: [String] -> [CandidateProvider] -> [(CandidateProvider,[String])]
enabledParts features providers =
    [(part,enabled) | part <- providers, let enabled = filter (`elem` cpFeatures part) features, not (null enabled)]

-- The rule families a generation runs, in the provider's order, each with the
-- features it sees; and the merge that turns their proposal lists (each cut
-- to the budget) back into the provider's proposal list.
proposalLeaves :: [String] -> CandidateProvider -> [(CandidateProvider,[String])]
proposalLeaves features provider
  | null (cpParts provider) = [(provider,features)]
  | otherwise = concat [proposalLeaves enabled part | (part,enabled) <- enabledParts features (cpParts provider)]

assembleProposals :: Int -> [String] -> CandidateProvider -> [[a]] -> [a]
assembleProposals budget features provider lists = case assemble features provider lists of
    (proposals,[]) -> proposals
    _ -> error "ECO AST search: more proposal lists than rule families"
  where
    assemble fs p ls
      | null (cpParts p) = case ls of
          (l:rest) -> (take budget l,rest)
          [] -> error "ECO AST search: fewer proposal lists than rule families"
      | otherwise =
          let step (merged,rest) (part,enabled) = let (l,rest') = assemble enabled part rest in (merged ++ [l],rest')
              (parts,remaining) = foldl step ([],ls) (enabledParts fs (cpParts p))
          in (take budget (mergeProposals (cpOrder p) parts),remaining)

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
              budget = soCandidates options+1
          (proposals,cleanup) <- propose n activeContracts budget current
          let limited = length proposals > soCandidates options
          -- Admission stays sequential: it places each candidate's AST and
          -- dedupes proof keys in proposal order, so which candidates get
          -- evaluated never depends on how or where they were proposed.
          (_,admitted,visited') <- foldM (admitCandidate n (planOutput plan))
              (0 :: Int,[],visited) (take (soCandidates options) proposals)
          outcomes <- evaluateJobs (searchWorkers request) (evaluateCandidate costContext prove current score) (reverse admitted)
          let accepted = [((changedLines (costValue nextScore),size,jobHash job),job,nextCost,nextScore)
                         | (job,Evaluated nextCost nextScore size True) <- outcomes]
          chosen <- case accepted of
            [] -> return Nothing
            _ -> do
              let (_,job,nextCost,nextScore) = minimumBy (comparing (\(rank,_,_,_) -> rank)) accepted
              candidate <- jobCandidate job
              return (Just (job,candidate,nextCost,nextScore))
          cleanup
          case chosen of
            Nothing -> return (current,cost,score,plan,scores,if limited then "candidate-limit" else "no-improving-proved-candidate")
            Just (job,candidate,nextCost,nextScore) -> do
              plan' <- save (planOutput plan) (acProgram candidate) cost nextCost plan candidate (acStateMap candidate)
              require (planOutput plan' == jobHash job) "candidate AST changed during proof"
              rounds costContext prove save (n+1) (acProgram candidate) nextCost nextScore plan' (scores++[nextScore]) visited'
    -- A round's proposals: the provider's generator in this process, or each
    -- rule family in a worker process with the proposals merged in the
    -- provider's order. Either way the list is what the generator returns.
    -- A family's proposals come back as records; their programs stay in the
    -- artifacts the family wrote, which admission moves under the round's
    -- labels and whoever needs the program reads.
    propose roundNumber activeContracts budget current
      | searchWorkers request <= 1 = return
          ([Proposed (describe candidate) (const (return candidate))
           | candidate <- take budget (cpGenerate provider activeContracts (soFeatures options) budget old current)]
          ,return ())
      | otherwise = do
          let scratch = directory </> "proposals" </> ("round-" ++ show roundNumber)
              families = [Family index family enabled (scratch </> ("family-" ++ show index))
                         | (index,(family,enabled)) <- zip [0 :: Int ..] (proposalLeaves (soFeatures options) provider)]
          createDirectoryIfMissing True scratch
          results <- runJobs (searchWorkers request) familyLabel familyBase (proposeFamily activeContracts budget current) families
          let proposals = assembleProposals budget (soFeatures options) provider (map snd results)
              cleanup = do
                  -- The records stay as evidence of what each family proposed;
                  -- the artifacts were either moved under round labels or are
                  -- no longer needed.
                  leftovers <- listDirectory scratch
                  forM_ [scratch </> f | f <- leftovers, takeExtension f /= ".outcome"] removeFile
          return ([Proposed record (loadProposal record) | record <- proposals],cleanup)
    proposeFamily activeContracts budget current family =
        forM (zip [0 :: Int ..] (take budget (cpGenerate (familyProvider family) activeContracts (familyEnabled family) budget old current))) $
          \(index,candidate) -> do
            let base = familyBase family ++ "-" ++ show index
            h <- writeAST (base ++ ".ast") (acProgram candidate)
            writeASTContracts (base ++ ".contracts") (acContracts candidate)
            return (describe candidate) { ppArtifact = Just (base ++ ".ast",base ++ ".contracts",h) }
    describe candidate = Proposal (acFeature candidate) (acDescription candidate) (acOperations candidate)
        (acBinaryOnly candidate) (acStateMap candidate) (show (acContracts candidate))
        (samePorts ordinary (acProgram candidate)) Nothing
    loadProposal record path = case ppArtifact record of
        Nothing -> ioError (userError "ECO AST search: a proposal record names no artifact")
        Just (_,contractPath,_) -> do
            bytes <- BS.readFile path
            obligations <- readASTContracts contractPath
            return (ASTCandidate (ppFeature record) (ppDescription record) (decode bytes) (ppOperations record)
                                 (ppBinaryOnly record) (ppStateMap record) obligations)
    admitCandidate roundNumber referenceHash (index,admitted,visited) proposed = do
        let record = prRecord proposed
            label = "candidate-" ++ show roundNumber ++ "-" ++ show index
            base = directory </> label
        require (ppFeature record `elem` soFeatures options) "provider emitted a disabled or unregistered feature"
        if (soDomain options == StrictFourState && ppBinaryOnly record) || not (ppSamePorts record)
          then return (index+1,admitted,visited)
          else do
            h <- place proposed (base ++ ".ast")
            -- Proof failures belong to a reference/candidate/certificate
            -- combination, not to the candidate AST alone. Another rule can
            -- propose the same AST with a valid state relation or obligation.
            let proofKey=(referenceHash,h,ppStateMap record,ppContractKey record)
            if proofKey `S.member` visited then return (index+1,admitted,visited) else
              return (index+1,Job label base (prCandidate proposed (base ++ ".ast")) h:admitted,S.insert proofKey visited)
    -- The candidate's AST under its round label: written now, or moved from
    -- where its rule family wrote it (the same bytes, so the same hash).
    place proposed path = case ppArtifact (prRecord proposed) of
        Nothing -> prCandidate proposed path >>= writeAST path . acProgram
        Just (programPath,_,h) -> renameFile programPath path >> return h
    -- The expensive part of one candidate: render, score, prove. Everything it
    -- makes is a file under the search directory, so it runs unchanged in a
    -- worker process, and the outcome record is all that comes back.
    evaluateCandidate costContext prove current score job = do
        let Job label base _ _ = job
        candidate <- jobCandidate job
        let program = acProgram candidate
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

-- What a rule family reports about one proposal: everything admission and
-- selection need except the program. When the family ran in a worker process
-- the program and the obligations are in the artifacts it wrote (program,
-- contracts, program hash).
data Proposal = Proposal
    { ppFeature :: String, ppDescription :: String, ppOperations :: [(String,[(String,String)])]
    , ppBinaryOnly :: Bool, ppStateMap :: [(String,String)], ppContractKey :: String
    , ppSamePorts :: Bool, ppArtifact :: Maybe (FilePath,FilePath,String)
    } deriving (Read,Show)

-- A proposal awaiting admission, and the candidate itself given the path its
-- program ends up under.
data Proposed = Proposed { prRecord :: Proposal, prCandidate :: FilePath -> IO ASTCandidate }

-- One rule family's share of a round's generation.
data Family = Family { familyIndex :: Int, familyProvider :: CandidateProvider, familyEnabled :: [String], familyBase :: FilePath }
familyLabel :: Family -> String
familyLabel family = "family-" ++ show (familyIndex family)

-- One admitted candidate of a round, in proposal order. The candidate is read
-- by whoever evaluates it, or by the round when it is the one accepted.
data Job = Job { jobLabel :: String, jobBase :: FilePath, jobCandidate :: IO ASTCandidate, jobHash :: String }

-- What evaluating a candidate produced: exactly the data the round's selection
-- needs. Proof details and witnesses stay on disk, where the sequential loop
-- also leaves them.
data Evaluation = NotImproved | Rejected | Evaluated LineCost CostObservation Integer Bool
    deriving (Read, Show)

evaluateJobs :: Int -> (Job -> IO Evaluation) -> [Job] -> IO [(Job,Evaluation)]
evaluateJobs workers = runJobs workers jobLabel jobBase

-- Run a round's jobs, in worker processes when asked for. The workers are
-- forked when the jobs are known, so each inherits the loaded programs and
-- cost context without reloading anything. A worker takes job indices from
-- its own pipe, does the job, writes its result as a one-line record next to
-- the job's base path and reports the index on the shared completion pipe;
-- the parent hands the next job to whichever worker finished, so uneven job
-- times balance out. Results come back in job order, so a caller sees what
-- the sequential loop would have seen. No AST crosses a process boundary.
-- A job that throws stops the caller with its message, as it would have in
-- the sequential loop.
runJobs :: (Read r, Show r) => Int -> (job -> String) -> (job -> FilePath) -> (job -> IO r) -> [job] -> IO [(job,r)]
runJobs workers label base run jobs
  | workers <= 1 || null jobs = mapM (\job -> (,) job <$> run job) jobs
  | otherwise = do
      hFlush stdout >> hFlush stderr
      let indexed = zip [0 :: Int ..] jobs
          count = min workers (length jobs)
          recordPath job = base job ++ ".outcome"
          -- Whatever happens to a job, its index is reported, so the parent
          -- never waits for a worker that failed.
          attempt job = either (Left . message) Right <$> try (run job)
          message :: SomeException -> String
          message = show
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
                      written <- try (writeFile (recordPath job) (show outcome ++ "\n")) :: IO (Either SomeException ())
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
      let go owner pending inflight
            | inflight == (0 :: Int) = return ()
            | otherwise = do
                finished <- read <$> hGetLine done
                when (finished < 0) $ ioError (userError "ECO AST search: a worker could not record a job outcome")
                let w = owner M.! finished
                case pending of
                  (index,_):more -> feedJob w index >> go (M.insert index w (M.delete finished owner)) more inflight
                  [] -> go (M.delete finished owner) [] (inflight-1)
      go (M.fromList [(index,w) | (w,(index,_)) <- zip pool first]) rest (length first)
      forM_ pool $ \(_,feed) -> hClose feed
      forM_ pool $ \(pid,_) -> getProcessStatus True False pid
      hClose done
      forM indexed $ \(_,job) -> do
          text <- readFile (recordPath job)
          outcome <- maybe (ioError (userError ("ECO AST search: unreadable outcome record for " ++ label job))) return (readMaybe text)
          case outcome of
            Left message -> ioError (userError ("ECO AST search: " ++ label job ++ ": " ++ message))
            Right result -> return (job,result)

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
