-- External native-capture orchestration. No source frontend or RTL parser.
module ECOASTFlow (CaptureFlow(..), FlowReport(..), FlowBranch(..), StructurePolicy(..),
                   applyStructurePolicy, structurePolicyLabel,
                   FlowPolicy(..), defaultFlowPolicy, wholeFileObjective, runCaptureFlow,
                   InvariantExportPolicy(..), runOptionalInvariantExport) where

import Control.Exception (IOException, try, bracket)
import Control.Monad (unless, forM, forM_)
import Data.List (minimumBy)
import Data.Ord (comparing)
import System.Directory
import System.Environment (getEnvironment)
import System.Exit (ExitCode(..))
import System.FilePath
import System.IO
import System.Process
import System.Timeout (timeout)
import ECO.Types
import ECOMatchTypes
import ECOMatch
import ECO.VerilogInfo
import ECOBind (readRecord)
import ECOASTPlan
import ECOASTProof
import ECOASTProofContext
import ECOASTSearch
import ECOASTCost
import ECOStructurePolicy
import ECOASTInvariant
import ECOInvariantExport
import ECOASTReplay
import ECOJSON (EvidenceJSON(..), renderEvidenceJSON)
import ECO.Replay (FileIdentity(..), fileIdentity, scrubbedEnvironment)
import ECO.Interface (fixedPorts)
import Verilog (VProgram)

-- The external policy supplies typed proposals, the scoring objective and the
-- optional export choice. Native capture validation, proof context, saved-plan
-- binding and the final fresh verification stay in this runner.
data FlowPolicy = FlowPolicy
    { fpProvider :: CandidateProvider
    , fpObjective :: VerilogInfo -> VerilogInfo -> CostProvider
    , fpInvariants :: InvariantExportPolicy }
wholeFileObjective :: VerilogInfo -> VerilogInfo -> CostProvider
wholeFileObjective old new = wholeFileCostProvider (renderVerilogText old) (renderVerilogText new)
defaultFlowPolicy :: FlowPolicy
defaultFlowPolicy = FlowPolicy defaultCandidateProvider wholeFileObjective ExportInvariants

need :: Bool -> String -> IO ()
need ok message = unless ok (ioError (userError ("ECO AST flow: " ++ message)))
checked :: Either String a -> IO a
checked = either (ioError . userError) return

-- Pass arguments directly, use captured working context, and record exact logs.
-- Timeouts terminate and reap the compiler rather than leaking a backend job.
command :: Int -> FilePath -> FilePath -> Maybe FilePath -> FilePath -> [String] -> IO ()
command seconds logPath working library executable args = do
    inherited <- getEnvironment
    let clean = filter ((`notElem` scrubbedEnvironment) . fst) inherited
        environment = case library of
          Nothing -> clean
          Just lib -> [("BLUESPECDIR",lib),("LD_LIBRARY_PATH",lib </> "SAT" ++ ":" ++
                       maybe "" id (lookup "LD_LIBRARY_PATH" clean))] ++
                      filter ((`notElem` ["BLUESPECDIR","LD_LIBRARY_PATH"]) . fst) clean
    writeFile (logPath ++ ".command") (show (executable,args,working,library) ++ "\n")
    withFile logPath WriteMode $ \logHandle -> do
      let setup = (proc executable args) {cwd=Just working,env=Just environment,
                       std_out=UseHandle logHandle,std_err=UseHandle logHandle}
          stop (_,_,_,process) = do
              running <- getProcessExitCode process
              case running of Nothing -> terminateProcess process >> waitForProcess process >> return (); _ -> return ()
      bracket (createProcess setup) stop $ \(_,_,_,process) -> do
        outcome <- timeout (seconds*1000000) (waitForProcess process)
        need (outcome == Just ExitSuccess) ("command failed or timed out: " ++ logPath)

-- One explicit scoring policy drives naming, search, branch choice, rollback,
-- and final survival. Its witnesses never replace complete deployed RTL. The
-- result is final only after the shared verifier has freshly replayed this
-- flow's own output directory from roots derived from its configuration.
runCaptureFlow :: FlowPolicy -> CaptureFlow -> IO FlowReport
runCaptureFlow (FlowPolicy provider objectiveFactory invariantPolicy) config = do
    need (cfCommandSeconds config > 0) "nonpositive command timeout"
    need (not (null (cfModule config)) && takeFileName (cfModule config) == cfModule config &&
          cfModule config `notElem` [".",".."]) "invalid module name"
    exists <- doesPathExist (cfOutput config)
    need (not exists) "output directory exists"
    createDirectoryIfMissing True (cfOutput config)
    output <- canonicalizePath (cfOutput config)
    -- The effective configuration is the flow's first artifact: every verifier
    -- binds it and checks it against the caller's roots.
    let effective = config {cfOutput=output}
    writeFile (output </> "effective.config") (show effective ++ "\n")
    configID <- fileIdentity (output </> "effective.config")
    compiler <- canonicalizePath (cfCompiler config)
    inputs <- (configID :) <$> mapM fileIdentity ([compiler,cfOldCapture config,cfNewCapture config,
                                                   cfImplementationManifest config] ++ cpSources provider)
    let moduleName = cfModule config
        stem directory = directory </> moduleName
        native label side capture extra adapt = do
          let directory = output </> label
              working = if side then cfNewDirectory config else cfOldDirectory config
              library = if side then cfNewLibrary config else cfOldLibrary config
          command (cfCommandSeconds config) (output </> label ++ ".log") working library compiler
              ([if adapt then "-eco-adapt" else "-eco-replay",capture,"-eco-output",directory]
               ++ extra ++ ["+RTS","-K128m","-M8G","-RTS"])
          return directory
        -- Companions sit beside the recaptured capture after adaptation and
        -- beside the RTL after a plan replay; controls go beside the run.
        snapshot companions controls = do
          result <- readVerilogInfo (stem companions ++ ".eco-ast")
          need (aeModule (viEnvelope result) == moduleName) "module mismatch"
          _ <- verifyVerilogInfo result (controls </> "ast-control")
          return result
        graphs companions = do
          pre <- readRecord (stem companions ++ ".eco-preopt")
          final <- readRecord (stem companions ++ ".eco-graph")
          return (pre,final)
        matching label oldPath newPath = do
          old <- readRecord oldPath; new <- readRecord newPath
          matches <- checked (matchGraphs old new)
          renames <- checked (proposeRenames (egReserved new) old new matches)
          writeFile (output </> label ++ ".eco-plan") (show (EcoRenamePlan 1 moduleName renames) ++ "\n")
          return renames
        runner reference candidate options directory alpha =
          runASTProofWithPins (childPinsFromInstances (viInstances reference))
              (childPinsFromInstances (viInstances candidate)) (soYosys options)
              (soProofSeconds options) directory (soDomain options) alpha
        requireExactRendering context = do
          exact <- verilogRenderingIsExact context
          need exact "filtered ordinary bytes differ from the proof AST rendering"
    oldDir <- native "adapted-old" False (cfOldCapture config) [] True
    newDir <- native "adapted-new" True (cfNewCapture config) [] True
    let oldCompanions = oldDir </> "capture"; newCompanions = newDir </> "capture"
    old <- snapshot oldCompanions oldDir; new <- snapshot newCompanions newDir
    -- Score only a rendering authenticated to the actual ordinary RTL. This
    -- gate must precede the caught branch search and ordinary rollback.
    requireExactRendering old
    requireExactRendering new
    need (fixedPorts (viProgram old) == fixedPorts (viProgram new)) "fixed module ports differ"
    originalWholeCost <- lineCost (fiPath (aeOutput (viEnvelope old))) (fiPath (aeOutput (viEnvelope new)))
    let initialObjective=objectiveFactory old new
    objectiveContext <- prepareCost initialObjective output (viProgram old)
    initialScore <- scoreCost objectiveContext initialObjective "ordinary" (viProgram new)
    let originalCost=costValue initialScore
    (oldPre,oldFinal) <- graphs oldCompanions; (newPre,newFinal) <- graphs newCompanions
    proposal <- checked (proposeKeeps oldPre oldFinal newPre newFinal)
    writeFile (output </> "keep-proposal") (show proposal ++ "\n")
    let keepBranch = do
          let capture = newCompanions </> moduleName ++ ".replay"
              planPath = output </> "keep.plan"
          capId <- fileIdentity capture
          preId <- fileIdentity (stem newCompanions ++ ".eco-preopt")
          plan <- checked (makeBackendPlan proposal (fiSHA256 capId) (fiSHA256 preId))
          writeFile planPath (show plan ++ "\n")
          keptDir <- native "kept" True capture ["-eco-backend-plan",planPath] False
          kept <- snapshot keptDir keptDir
          requireExactRendering new
          requireExactRendering kept
          alpha <- matching "keep-control-match" (stem newCompanions ++ ".eco-graph") (stem keptDir ++ ".eco-graph")
          gate <- runner new kept (cfSearch config) (output </> "proof-keep") alpha (viProgram new) (viProgram kept)
          writeFile (output </> "proof-keep.json") (renderProofResult gate)
          need (proofPassed gate) ("keep control proof failed: " ++ proofStatus gate)
          return ("kept",keptDir,kept,[(b,a) | (a,b) <- alpha])
    kept <- if null (kpKeeps proposal) || "F06" `notElem` soFeatures (cfSearch config)
            then return (Right Nothing)
            else fmap (fmap Just) (try keepBranch :: IO (Either IOException (String,FilePath,VerilogInfo,[(String,String)])))
    let branches = ("ordinary",newCompanions,new,[]) : case kept of Right (Just b) -> [b]; _ -> []
        keepFailure = case kept of Left err -> [FlowBranch "kept" Nothing (Just (show err)) Nothing Nothing]; _ -> []
    objectiveInputs <- mapM fileIdentity (concat [costSources (objectiveFactory old c) | (_,_,c,_) <- branches])
    forM_ branches $ \(_,_,c,_) -> need
      (costPolicy (objectiveFactory old c)==costPolicy initialObjective) "branch objective policy differs"
    results <- forM branches $ \(name,companions,current,_) -> do
      result <- try (do
        requireExactRendering old
        requireExactRendering current
        alpha <- if "F01" `elem` soFeatures (cfSearch config)
                 then matching (name ++ "-match") (stem oldCompanions ++ ".eco-graph") (stem companions ++ ".eco-graph")
                 else return []
        let searchDir = output </> name ++ "-search"
            gate = runner current current
            branchObjective=objectiveFactory old current
        found <- optimizeAST (defaultSearchRequest (cfSearch config) searchDir
                     (SearchInput (viProgram old) (renderVerilogText old) (childPinsFromInstances (viInstances old)))
                     (SearchInput (viProgram current) (renderVerilogText current) (childPinsFromInstances (viInstances current))))
                     { searchObjective=branchObjective, searchProvider=provider
                     , searchProof=gate, searchRenames=alpha }
        let envelope=srCostEnvelope found
        need ((costOldASTHash envelope,costOldWitnessHash envelope)==costBaselineIdentity objectiveContext)
          "branch objective changed the OLD scoring baseline"
        planId <- fileIdentity (searchDir </> "search.plan")
        outputId <- fileIdentity (searchDir </> "selected.v")
        return (FlowBranch name (Just (srObjectiveCost found)) Nothing (Just planId) (Just outputId),current,srProgram found)
        ) :: IO (Either IOException (FlowBranch,VerilogInfo,VProgram))
      return (name,result)
    let good = [(name,row,context,program) | (name,Right (row,context,program)) <- results]
        scored = [(changedLines cost,fiSHA256 ident,(name,row,context,program)) |
                  (name,row,context,program) <- good,Just cost <- [fbCost row],Just ident <- [fbOutput row]]
        selected = if null scored then Nothing else Just (minimumBy (comparing (\(c,h,_) -> (c,h))) scored)
        (name,cost,context,program) = case selected of
          Just (count,_,(n,row,c,p)) | count < changedLines originalCost ->
            (n,maybe originalCost id (fbCost row),c,p)
          _ -> ("ordinary-rollback",originalCost,new,viProgram new)
        rows = keepFailure ++ [case result of Right (row,_,_) -> row
                                              Left err -> FlowBranch n Nothing (Just (show err)) Nothing Nothing |
                              (n,result) <- results]
    mapM_ (validateVerilogInfo . (\(_,_,c,_) -> c)) branches
    validateVerilogInfo old
    forM_ (inputs++objectiveInputs) $ \identity -> do
      actual <- fileIdentity (fiPath identity)
      need (actual == identity) "input/tool identity changed during run"
    let destination = output </> moduleName ++ ".v"
    emitVerilog context destination program
    identity <- fileIdentity destination
    finalWholeCost <- lineCost (fiPath (aeOutput (viEnvelope old))) destination
    selectedScore <- scoreCost objectiveContext (objectiveFactory old context) "selected" program
    need (cost == costValue selectedScore && changedLines cost <= changedLines originalCost) "selected result worsened ordinary objective"
    writeFile (output </> "flow-objective.report")
      (show (costPolicy initialObjective,initialScore,selectedScore,originalWholeCost,finalWholeCost,objectiveInputs) ++ "\n")
    objectiveReport <- fileIdentity (output </> "flow-objective.report")
    objectivePlans <- mapM fileIdentity [takeDirectory (fiPath ident) </> "objective.plan" |
        (_,Right (row,_,_)) <- results,Just ident <- [fbPlan row]]
    let report = FlowReport 5 moduleName name (cfSearch config) (costPolicy initialObjective) originalCost cost rows identity
                   (inputs++objectiveInputs++objectivePlans++[objectiveReport]) configID
    writeFile (output </> "flow.report") (show report ++ "\n")
    -- The written bundle is verified freshly through the shared verifier before
    -- the deliverable is final; nothing below rests on the search's own proofs.
    verified <- replaySelectedFlow (configRoots effective) output (output </> "verified")
    validateReplay verified
    need (fiSHA256 (replayRTL verified) == fiSHA256 identity) "fresh verification differs from the emitted RTL"
    -- Optional facts are a separate product: export against the selected AST,
    -- not merely against ordinary-new, and never let missing historical
    -- metadata or an unresolved fact invalidate an already proved ECO result.
    let invariantDirectory=output </> "selected-invariants"
        branchRelation=case [r | (n,_,_,r)<-branches,n==name] of [r]->r;_->[]
        acceptedPlan=case [ident | (n,Right (row,_,_))<-results,n==name,Just ident<-[fbPlan row]] of
          [ident]->Just ident
          _->Nothing
    createDirectoryIfMissing True invariantDirectory
    writeFile (invariantDirectory </> "status.json") (renderEvidenceJSON (JObject
      [("status",JString (if invariantPolicy==ExportInvariants then "pending" else "deferred")),
       ("assumptionsExported",JNumber "0")]) ++ "\n")
    writeFile (output </> "summary.txt") $ unlines
        ["ordinary_changed_lines=" ++ show (changedLines originalCost),
         "selected_changed_lines=" ++ show (changedLines cost),"selected_branch=" ++ name,
         "proof_domain=" ++ show (soDomain (cfSearch config)),
         "objective_policy=" ++ costPolicy initialObjective,
         "structure_policy=" ++ structurePolicyLabel (cfSearch config),
         "fresh_verification=" ++ fiPath (replayReceipt verified),
         "scope=ordinary-new to selected; no source intent, reachability, timing or physical QoR proof",
         "global_minimum_proved=False"]
    runOptionalInvariantExport invariantPolicy invariantDirectory $ do
      steps<-case acceptedPlan of
        Nothing->return []
        Just ident->do
          actual<-fileIdentity (fiPath ident)
          need (actual==ident) "accepted plan changed before invariant rebinding"
          plan<-readRecord (fiPath ident)
          return (planSteps plan)
      bindings<-checked (composeInvariantBindings (viProgram new)
        (branchRelation:[[(b,a)|(a,b)<-stepAlpha step]|step<-steps]))
      facts<-consumeSelectedASTEvidence (soYosys (cfSearch config)) (soProofSeconds (cfSearch config))
               invariantDirectory new context program identity bindings
      let bound=facts {irSources=irSources facts++maybe [] (\f->[("acceptedPlan",f)]) acceptedPlan}
      writeInvariantOutputs invariantDirectory bound
      fileIdentity (invariantDirectory </> "report.json")
    return report
