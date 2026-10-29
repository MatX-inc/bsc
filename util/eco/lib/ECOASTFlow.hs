-- External native-capture orchestration. No source frontend or RTL parser.
module ECOASTFlow (CaptureFlow(..), FlowReport(..), FlowBranch(..), StructurePolicy(..),
                   applyStructurePolicy, structurePolicyLabel, runCaptureFlow,
                   InvariantExportPolicy(..), runCaptureFlowWithInvariants, runCaptureFlowWithProvider, runCaptureFlowWithCost,
                   runOptionalInvariantExport) where

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
import Text.Read (readMaybe)
import ECO.Types
import ECOMatchTypes
import ECOMatch
import ECOAggressive (AssignmentAudit, auditJSON)
import ECOAudit
import ECOAST
import ECOASTPlan
import ECOASTProof
import ECOASTProofContext
import ECOASTSearch
import ECOASTCost
import ECOStructurePolicy
import ECOASTInvariant
import ECOInvariantExport
import ECO.Replay (FileIdentity(..), fileIdentity)
import ECO.Interface (fixedPorts)
import Verilog (VProgram)

data CaptureFlow = CaptureFlow
    { cfCompiler :: FilePath, cfModule :: String
    , cfOldCapture :: FilePath, cfNewCapture :: FilePath
    , cfOldDirectory :: FilePath, cfNewDirectory :: FilePath
    , cfOldLibrary :: Maybe FilePath, cfNewLibrary :: Maybe FilePath
    , cfMatcher :: Maybe FilePath, cfOutput :: FilePath
    , cfSearch :: SearchOptions, cfCommandSeconds :: Int
    , cfImplementationManifest :: FilePath
    , cfContracts :: Maybe FilePath
    } deriving (Eq,Read,Show)
data FlowBranch = FlowBranch
    { fbName :: String, fbCost :: Maybe LineCost, fbError :: Maybe String
    , fbPlan :: Maybe FileIdentity, fbOutput :: Maybe FileIdentity
    } deriving (Eq,Read,Show)
data FlowReport = FlowReport
    { frSchema :: Int, frModule :: String, frSelected :: String
    , frSearchPolicy :: SearchOptions
    , frRenderingPolicies :: [(String,ASTRenderingPolicy)]
    , frObjectivePolicy :: String
    , frOrdinaryCost :: LineCost, frSelectedCost :: LineCost
    , frBranches :: [FlowBranch], frOutput :: FileIdentity
    , frInputs :: [FileIdentity], frGlobalMinimumProved :: Bool
    } deriving (Eq,Read,Show)

need :: Bool -> String -> IO ()
need ok message = unless ok (ioError (userError ("ECO AST flow: " ++ message)))
record :: Read a => FilePath -> IO a
record path = readFile path >>= maybe (ioError (userError ("invalid record: " ++ path))) return . readMaybe
checked :: Either String a -> IO a
checked = either (ioError . userError) return

-- Pass arguments directly, use captured working context, and record exact logs.
-- Timeouts terminate and reap the compiler rather than leaking a backend job.
command :: Int -> FilePath -> FilePath -> Maybe FilePath -> FilePath -> [String] -> IO ()
command seconds logPath working library executable args = do
    inherited <- getEnvironment
    let clean = filter ((`notElem` ["BSC_OPTIONS","GHCRTS","BSC_DIAGNOSTIC_KEEP_ADefs"]) . fst) inherited
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

runCaptureFlow :: CaptureFlow -> IO FlowReport
runCaptureFlow = runCaptureFlowWithInvariants ExportInvariants

runCaptureFlowWithInvariants :: InvariantExportPolicy -> CaptureFlow -> IO FlowReport
runCaptureFlowWithInvariants = runCaptureFlowWithProvider defaultCandidateProvider

-- The external policy supplies typed proposals and source identities only.
-- Native capture validation, matching audit, proof context and saved-plan
-- replay stay in this runner. CaptureFlow's serialized format stays unchanged.
runCaptureFlowWithProvider :: CandidateProvider -> InvariantExportPolicy -> CaptureFlow -> IO FlowReport
runCaptureFlowWithProvider = runCaptureFlowWithCost (\old new -> wholeFileCostProvider (renderVerilogText old) (renderVerilogText new))

-- One explicit scoring policy drives naming, search, branch choice, rollback,
-- and final survival. Its witnesses never replace complete deployed RTL.
runCaptureFlowWithCost :: (VerilogInfo -> VerilogInfo -> CostProvider) -> CandidateProvider ->
    InvariantExportPolicy -> CaptureFlow -> IO FlowReport
runCaptureFlowWithCost objectiveFactory provider invariantPolicy config = do
    need (cfCommandSeconds config > 0) "nonpositive command timeout"
    need (not (null (cfModule config)) && takeFileName (cfModule config) == cfModule config &&
          cfModule config `notElem` [".",".."]) "invalid module name"
    exists <- doesPathExist (cfOutput config)
    need (not exists) "output directory exists"
    createDirectoryIfMissing True (cfOutput config)
    output <- canonicalizePath (cfOutput config)
    compiler <- canonicalizePath (cfCompiler config)
    inputs <- mapM fileIdentity ([compiler,cfOldCapture config,cfNewCapture config,
                                 cfImplementationManifest config] ++ maybe [] (:[]) (cfMatcher config) ++
                                 maybe [] (:[]) (cfContracts config) ++ cpSources provider)
    contracts <- maybe (return []) readASTContracts (cfContracts config)
    let moduleName = cfModule config
        stem directory = directory </> moduleName
        native label side capture extra adapt = do
          let directory = output </> label
              working = if side then cfNewDirectory config else cfOldDirectory config
              library = if side then cfNewLibrary config else cfOldLibrary config
          command (cfCommandSeconds config) (output </> label ++ ".log") working library compiler
              ([if adapt then "-eco-adapt" else "-eco-replay",capture,
                "-eco-output",directory,"-eco-graph",directory] ++ extra ++ ["+RTS","-K128m","-M8G","-RTS"])
          return directory
        snapshot directory = do
          result <- readVerilogInfo (stem directory ++ ".eco-ast")
          need (aeModule (viEnvelope result) == moduleName) "module mismatch"
          _ <- verifyVerilogInfo result (directory </> "ast-control")
          return result
        graphs directory = do
          pre <- record (stem directory ++ ".eco-preopt")
          final <- record (stem directory ++ ".eco-graph")
          return (pre,final)
        matching label oldPath newPath = do
          old <- record oldPath; new <- record newPath
          let target = output </> label
          (audit,renames) <- case cfMatcher config of
            Nothing -> do
              (matches,audit) <- checked (matchGraphsWithAudit Aggressive old new)
              a <- maybe (ioError (userError "missing aggressive audit")) return audit
              renames <- checked (proposeRenames (egReserved new) old new matches)
              writeFile (target ++ ".eco-plan") (show (EcoRenamePlan 1 moduleName renames) ++ "\n")
              writeFile (target ++ ".eco-audit") (show a ++ "\n")
              writeFile (target ++ ".eco-audit.json") (auditJSON renames a ++ "\n")
              return (a,renames)
            Just executable -> do
              command (cfCommandSeconds config) (target ++ ".log") (cfNewDirectory config)
                      (cfNewLibrary config) executable ["--policy","aggressive",oldPath,newPath,target]
              plan <- record (target ++ ".eco-plan")
              need (erpSchema plan == 1 && erpModule plan == moduleName) "matcher plan schema/module mismatch"
              audit <- record (target ++ ".eco-audit") :: IO AssignmentAudit
              return (audit,erpRenames plan)
          proof <- checked (checkAssignmentAudit old new audit renames)
          writeFile (target ++ ".audit-check") (show proof ++ "\n")
          return renames
        runner reference candidate options directory alpha obligations =
          runASTProofWithContracts (childPinsFromInstances (viInstances reference))
              (childPinsFromInstances (viInstances candidate)) obligations (soYosys options)
              (soProofSeconds options) directory (soDomain options) alpha
        requireExactRendering context = do
          exact <- verilogRenderingIsExact context
          need exact "filtered ordinary bytes differ from the proof AST rendering"
    oldDir <- native "adapted-old" False (cfOldCapture config) [] True
    newDir <- native "adapted-new" True (cfNewCapture config) [] True
    old <- snapshot oldDir; new <- snapshot newDir
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
    (oldPre,oldFinal) <- graphs oldDir; (newPre,newFinal) <- graphs newDir
    proposal <- checked (proposeKeeps oldPre oldFinal newPre newFinal)
    writeFile (output </> "keep-proposal") (show proposal ++ "\n")
    let keepBranch = do
          let capture = newDir </> "capture" </> moduleName ++ ".replay"
              planPath = output </> "keep.plan"
          capId <- fileIdentity capture
          preId <- fileIdentity (stem newDir ++ ".eco-preopt")
          plan <- checked (makeBackendPlan proposal (fiSHA256 capId) (fiSHA256 preId))
          writeFile planPath (show plan ++ "\n")
          keptDir <- native "kept" True capture ["-eco-backend-plan",planPath] False
          kept <- snapshot keptDir
          requireExactRendering new
          requireExactRendering kept
          alpha <- matching "keep-control-match" (stem newDir ++ ".eco-graph") (stem keptDir ++ ".eco-graph")
          gate <- runner new kept (cfSearch config) (output </> "proof-keep") alpha [] (viProgram new) (viProgram kept)
          writeFile (output </> "proof-keep.json") (renderProofResult gate)
          need (proofPassed gate) ("keep control proof failed: " ++ proofStatus gate)
          let forward=[(b,a) | (a,b) <- alpha]
          return ("kept",keptDir,kept,renameASTContracts forward contracts,forward)
    kept <- if null (kpKeeps proposal) || "F06" `notElem` soFeatures (cfSearch config)
            then return (Right Nothing)
            else fmap (fmap Just) (try keepBranch :: IO (Either IOException (String,FilePath,VerilogInfo,[ASTContract],[(String,String)])))
    let branches = ("ordinary",newDir,new,contracts,[]) : case kept of Right (Just b) -> [b]; _ -> []
        keepFailure = case kept of Left err -> [FlowBranch "kept" Nothing (Just (show err)) Nothing Nothing]; _ -> []
    objectiveInputs <- mapM fileIdentity (concat [costSources (objectiveFactory old c) | (_,_,c,_,_) <- branches])
    forM_ branches $ \(_,_,c,_,_) -> need
      (costPolicy (objectiveFactory old c)==costPolicy initialObjective) "branch objective policy differs"
    results <- forM branches $ \(name,directory,current,branchContracts,_) -> do
      result <- try (do
        requireExactRendering old
        requireExactRendering current
        alpha <- if "F01" `elem` soFeatures (cfSearch config)
                 then matching (name ++ "-match") (stem oldDir ++ ".eco-graph") (stem directory ++ ".eco-graph")
                 else return []
        let searchDir = output </> name ++ "-search"
            gate = runner current current
        let branchObjective=objectiveFactory old current
        workers <- searchWorkersFromEnvironment
        found <- optimizeAST (defaultSearchRequest (cfSearch config) searchDir
                     (SearchInput (viProgram old) (renderVerilogText old))
                     (SearchInput (viProgram current) (renderVerilogText current)))
                     { searchObjective=branchObjective, searchProvider=provider
                     , searchProof=gate, searchContracts=branchContracts, searchRenames=alpha
                     , searchWorkers=workers }
        let envelope=srCostEnvelope found
        need ((costOldASTHash envelope,costOldWitnessHash envelope)==costBaselineIdentity objectiveContext)
          "branch objective changed the OLD scoring baseline"
        -- Replay semantics and the separate scoring envelope before use.
        (replayed,cost,ordinaryScore) <- replayASTPlanWithCost branchObjective (viProgram old) gate (cfSearch config) (renderVerilogText current) (searchDir </> "search.plan")
                        (output </> name ++ "-replay") (viProgram current)
        need (replayed == srProgram found) "plan replay changed the selected AST"
        need (cost==srObjectiveCost found && ordinaryScore==srOrdinaryObjectiveCost found) "objective replay changed search costs"
        let destination = output </> name ++ "-selected.v"
        emitVerilog current destination replayed
        wholeCost <- lineCost (fiPath (aeOutput (viEnvelope old))) destination
        need (wholeCost == srCost found) "selected emission changed complete textual cost"
        planId <- fileIdentity (searchDir </> "search.plan")
        outputId <- fileIdentity destination
        return (FlowBranch name (Just cost) Nothing (Just planId) (Just outputId),current,replayed)
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
    mapM_ (validateVerilogInfo . (\(_,_,c,_,_) -> c)) branches
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
    -- Optional facts are a separate product: export against the selected AST,
    -- not merely against ordinary-new, and never let missing historical
    -- metadata or an unresolved fact invalidate an already proved ECO result.
    let invariantDirectory=output </> "selected-invariants"
        branchRelation=case [r | (n,_,_,_,r)<-branches,n==name] of [r]->r;_->[]
        acceptedPlan=case [ident | (n,Right (row,_,_))<-results,n==name,Just ident<-[fbPlan row]] of
          [ident]->Just ident
          _->Nothing
    let rendering = ("old",astRenderingPolicy old) :
                    [(n,astRenderingPolicy s) | (n,_,s,_,_) <- branches]
        objective = costPolicy initialObjective ++ ";preserve-new-compiler-scaffolding;" ++ structurePolicyLabel (cfSearch config)
        schema = if costPolicy initialObjective=="whole-file-raw-v1" then 3 else 4
        report = FlowReport schema moduleName name (cfSearch config) rendering objective originalCost cost rows identity
                   (inputs++objectiveInputs++objectivePlans++[objectiveReport]) False
    createDirectoryIfMissing True invariantDirectory
    writeFile (invariantDirectory </> "status.json") (renderEvidenceJSON (JObject
      [("status",JString (if invariantPolicy==ExportInvariants then "pending" else "deferred")),
       ("assumptionsExported",JNumber "0")]) ++ "\n")
    writeFile (output </> "flow.report") (show report ++ "\n")
    writeFile (output </> "summary.txt") $ unlines
        ["ordinary_changed_lines=" ++ show (changedLines originalCost),
         "selected_changed_lines=" ++ show (changedLines cost),"selected_branch=" ++ name,
         "proof_domain=" ++ show (soDomain (cfSearch config)),
         "objective_policy=" ++ objective,
         "scope=ordinary-new to selected; no source intent, reachability, timing or physical QoR proof",
         "global_minimum_proved=False"]
    runOptionalInvariantExport invariantPolicy invariantDirectory $ do
      steps<-case acceptedPlan of
        Nothing->return []
        Just ident->do
          actual<-fileIdentity (fiPath ident)
          need (actual==ident) "accepted plan changed before invariant rebinding"
          plan<-record (fiPath ident)
          return (planSteps plan)
      bindings<-checked (composeInvariantBindings (viProgram new)
        (branchRelation:[[(b,a)|(a,b)<-stepAlpha step]|step<-steps]))
      facts<-consumeSelectedASTEvidence (soYosys (cfSearch config)) (soProofSeconds (cfSearch config))
               invariantDirectory new context program identity bindings
      let bound=facts {irSources=irSources facts++maybe [] (\f->[("acceptedPlan",f)]) acceptedPlan}
      writeInvariantOutputs invariantDirectory bound
      fileIdentity (invariantDirectory </> "report.json")
    return report
