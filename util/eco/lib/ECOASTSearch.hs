-- Bounded external search over retained Verilog ASTs. Candidate generation and
-- equivalence are separate modules. Printed RTL is used only for the objective
-- and by the external SAT tool, never parsed back into an ECO representation.
module ECOASTSearch
    ( SearchOptions(..), SearchResult(..), ASTPlan(..), ASTStep(..), LineCost(..)
    , ProofRunner, defaultSearchOptions, SearchInput(..), SearchRequest(..)
    , defaultSearchRequest, optimizeAST
    , CandidateGenerator, CandidateProvider(..), defaultCandidateProvider
    , combineCandidateProviders, CostProvider(..), wholeFileCostProvider
    , replayASTPlanWithCost
    , replayASTPlan, replayASTPlanWith, lineCost, changedLines
    ) where

import Control.Exception (IOException, try)
import Control.Monad (foldM, unless, when)
import qualified Data.ByteString as BS
import Data.List (minimumBy, nub)
import Data.Ord (comparing)
import qualified Data.Set as S
import System.Directory
import System.FilePath
import Text.Read (readMaybe)
import BinData (encode,decodeStrict)
import GenABin ()
import Verilog
import ECORename (renameInternalSignals)
import ECO.Replay (fileIdentity,FileIdentity(..))
import ECOASTProof
import ECOASTTransform
import ECOASTLexical
import ECOASTCost

data SearchOptions = SearchOptions
    { soDomain :: ProofDomain, soFeatures :: [String], soRounds :: Int
    , soCandidates :: Int, soYosys :: FilePath, soProofSeconds :: Int
    } deriving (Eq,Read,Show)
defaultSearchOptions :: SearchOptions
defaultSearchOptions = SearchOptions Binary (map fst featureTable) 8 64 "yosys" 30

data ASTStep = ASTStep
    { stepFeature :: String, stepDescription :: String
    , stepBefore :: String, stepAfter :: String, stepArtifact :: FilePath
    , stepArtifactHash :: String, stepAlpha :: [(String,String)]
    , stepBeforeCost :: LineCost, stepAfterCost :: LineCost
    , stepOperations :: [(String,[(String,String)])]
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

-- Each input carries its own renderer and the native child pin metadata of
-- its capture: OLD and CURRENT may have different authenticated headers. The
-- objective measures separate witnesses and never replaces either renderer.
data SearchInput = SearchInput
    { searchProgram :: VProgram, searchRender :: VProgram -> String
    , searchPins :: [ChildPin] }

data SearchRequest = SearchRequest
    { searchOptions :: SearchOptions, searchDirectory :: FilePath
    , searchOld :: SearchInput, searchCurrent :: SearchInput
    , searchProvider :: CandidateProvider, searchProof :: ProofRunner
    , searchRenames :: [(String,String)]
    , searchObjective :: CostProvider
    }

-- Select the proof domain explicitly in options. Custom policy changes only
-- the named fields it supplies; all candidates still use the same proof gate.
defaultSearchRequest :: SearchOptions -> FilePath -> SearchInput -> SearchInput -> SearchRequest
defaultSearchRequest options directory old current = SearchRequest
    { searchOptions=options, searchDirectory=directory
    , searchOld=old, searchCurrent=current
    , searchProvider=defaultCandidateProvider, searchProof=defaultProof (searchPins current)
    , searchRenames=[]
    , searchObjective=wholeFileCostProvider (searchRender old) (searchRender current)
    }

-- Context (including native child pin metadata) comes from the checked capture
-- supplied by the caller, never from an editable candidate or plan artifact.
-- Reference and candidate are both CURRENT-side programs with the same pins.
type ProofRunner = SearchOptions -> FilePath -> [(String,String)] -> VProgram -> VProgram -> IO ProofResult
defaultProof :: [ChildPin] -> ProofRunner
defaultProof pins options directory =
    runASTProofWithPins pins pins (soYosys options) (soProofSeconds options) directory (soDomain options)

-- Policy is supplied by bluehs callers, independently of the frozen compiler
-- and SDK. Sources identify caller-supplied rule modules/data; they are audited
-- inputs, not a claim that the runner can discover all Haskell dependencies.
-- The generator receives the enabled feature names, a proposal budget, OLD,
-- and CURRENT. It only proposes: the runner owns proof and replay.
type CandidateGenerator = [String] -> Int -> VProgram -> VProgram -> [ASTCandidate]
data CandidateProvider = CandidateProvider
    { cpFeatures :: [String], cpSources :: [FilePath]
    , cpGenerate :: CandidateGenerator
    }

defaultCandidateProvider :: CandidateProvider
defaultCandidateProvider = CandidateProvider (map fst featureTable) [] generateASTCandidates

-- The caller chooses ordering (and therefore budget priority). Duplicate
-- feature registrations are rejected by search rather than silently shadowed.
combineCandidateProviders :: [CandidateProvider] -> CandidateProvider
combineCandidateProviders providers = CandidateProvider
    (concatMap cpFeatures providers) (nub (concatMap cpSources providers)) $ \features budget old current ->
      take budget $ concat
        [cpGenerate provider enabled budget old current |
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
emittable :: VProgram -> IO ()
emittable program = either (\offender -> require False ("candidate is not emittable: " ++ offender)) return
    (lexicallyEmittable program >> declaredBeforeUse program)

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
    let initialPlan = ASTPlan 4 options originalHash originalHash rendererHash []
        prove label relation a b = runner options (directory </> label) relation a b
        save before after cost nextCost plan candidate relation = do
            let index = length (planSteps plan) + 1
                relative = "steps" </> show index ++ ".ast"
            h <- writeAST (directory </> relative) after
            let step = ASTStep (acFeature candidate) (acDescription candidate)
                       before h relative h relation cost nextCost (acOperations candidate)
            return plan { planOutput=h,planSteps=planSteps plan ++ [step] }
    let unchanged = (ordinary,originalCost,ordinaryScore,initialPlan,[ordinaryScore])
    -- The initial renaming is gated on the objective like every other step:
    -- a renamed program that does not improve it leaves the search at NEW.
    (start,startCost,startScore,startPlan,startScores) <- if null renames || "F01" `notElem` soFeatures options
      then return unchanged
      else do
        named <- either (ioError . userError) return
                    (renameInternalSignals (map fst renames) renames ordinary)
        emittable named
        writeFile (directory </> "named.v") (render named)
        namedScore <- scoreCost costContext objective "named" named
        if changedLines (costValue namedScore) >= changedLines (costValue ordinaryScore)
          then do
            writeFile (directory </> "named.rejected")
              ("no objective improvement: " ++ show (changedLines (costValue namedScore)) ++
               " >= " ++ show (changedLines (costValue ordinaryScore)) ++ "\n")
            return unchanged
          else do
            proof <- prove "proof-names" [(b,a) | (a,b) <- renames] ordinary named
            require (proofPassed proof) ("initial name proof failed: " ++ proofStatus proof)
            nextCost <- lineCost (directory </> "old.v") (directory </> "named.v")
            let candidate = ASTCandidate "F01" "External typed name correspondence" named
                              [("rename",renames)] False []
            plan <- save originalHash named originalCost nextCost initialPlan candidate [(b,a) | (a,b) <- renames]
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
    SearchInput old renderOld _ = searchOld request
    SearchInput ordinary render _ = searchCurrent request
    provider = searchProvider request
    runner = searchProof request
    renames = searchRenames request
    objective = searchObjective request
    rounds costContext prove save n current cost score plan scores visited
      | n > soRounds options = return (current,cost,score,plan,scores,"round-limit")
      | otherwise = do
          let generated = take (soCandidates options+1) $
                cpGenerate provider (soFeatures options) (soCandidates options+1) old current
              limited = length generated > soCandidates options
          (_,accepted,visited') <- foldM (tryCandidate costContext prove n (planOutput plan) current score)
              (0,[],visited) (take (soCandidates options) generated)
          case accepted of
            [] -> return (current,cost,score,plan,scores,if limited then "candidate-limit" else "no-improving-proved-candidate")
            _ -> do
              let (_,candidate,nextCost,nextScore,h) = minimumBy (comparing (\(rank,_,_,_,_) -> rank)) accepted
              plan' <- save (planOutput plan) (acProgram candidate) cost nextCost plan candidate (acStateMap candidate)
              require (planOutput plan' == h) "candidate AST changed during proof"
              rounds costContext prove save (n+1) (acProgram candidate) nextCost nextScore plan' (scores++[nextScore]) visited'
    tryCandidate costContext prove roundNumber referenceHash current score (index,accepted,visited) candidate = do
        let label = "candidate-" ++ show roundNumber ++ "-" ++ show index
            base = directory </> label
            program = acProgram candidate
            skip = return (index+1,accepted,visited)
        require (acFeature candidate `elem` soFeatures options) "provider emitted a disabled or unregistered feature"
        if (soDomain options == StrictFourState && acBinaryOnly candidate) || not (samePorts ordinary program)
          then skip
          else do
            h <- writeAST (base ++ ".ast") program
            case lexicallyEmittable program >> declaredBeforeUse program of
              Left offender -> writeFile (base ++ ".rejected") ("not emittable: " ++ offender ++ "\n") >> skip
              Right () -> do
                -- Proof failures belong to a reference/candidate/certificate
                -- combination, not to the candidate AST alone. Another rule can
                -- propose the same AST with a valid state relation.
                let proofKey=(referenceHash,h,acStateMap candidate)
                if proofKey `S.member` visited then skip else do
                  let visited' = S.insert proofKey visited
                  writeFile (base ++ ".v") (render program)
                  nextCost <- lineCost (directory </> "old.v") (base ++ ".v")
                  -- A scoring or proof failure rejects this candidate only.
                  outcome <- try (do
                      nextScore <- scoreCost costContext objective label program
                      if changedLines (costValue nextScore) >= changedLines (costValue score) then return Nothing else do
                        proof <- prove (label ++ "-proof") (acStateMap candidate) current program
                        writeFile (base ++ ".proof.json") (renderProofResult proof)
                        size <- getFileSize (directory </> costWitness nextScore)
                        return (Just (nextScore,proof,size)))
                      :: IO (Either IOException (Maybe (CostObservation,ProofResult,Integer)))
                  case outcome of
                    Left err -> do
                      writeFile (base ++ ".rejected") (show err)
                      return (index+1,accepted,visited')
                    Right Nothing -> return (index+1,accepted,visited')
                    Right (Just (nextScore,proof,size)) -> do
                      let scored = ((changedLines (costValue nextScore),size,h),candidate,nextCost,nextScore,h)
                      return (index+1,if proofPassed proof then scored:accepted else accepted,visited')

-- Saved candidates are untrusted. Bind every artifact and rerun the complete
-- proof chain; a stored receipt never authorizes replay by itself.
replayASTPlan :: [ChildPin] -> SearchOptions -> (VProgram -> String) -> FilePath -> FilePath -> VProgram -> IO VProgram
replayASTPlan = replayASTPlanWith . defaultProof

replayASTPlanWith :: ProofRunner -> SearchOptions -> (VProgram -> String) -> FilePath -> FilePath -> VProgram -> IO VProgram
replayASTPlanWith runner expected render planPath directory ordinary = do
    contents <- readFile planPath
    plan <- maybe (ioError (userError "invalid ECO AST plan")) return (readMaybe contents)
    require (planSchema plan == 4) "unsupported plan schema"
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
      let candidate = decodeStrict bytes
      require (samePorts ordinary candidate) "candidate changed fixed module ports"
      emittable candidate
      proof <- runner options (directory </> "proof-" ++ show index) (stepAlpha step) reference candidate
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
      let program=decodeStrict bytes
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
