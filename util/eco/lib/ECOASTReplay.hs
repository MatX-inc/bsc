-- A fresh proof of ordinary NEW through the selected flow or an accepted prefix.
-- Every executable and capture the verifier uses comes from the caller's trust
-- roots; the directory under verification must agree with them and never
-- supplies a tool path. Stored receipts bind inputs; their previous pass/fail
-- fields grant no authority.
module ECOASTReplay
 ( CaptureFlow(..), FlowBranch(..), FlowReport(..)
 , TrustRoots(..), configRoots, rootsFromConfig
 , LoadedFlow(..), loadFlow, AdapterReceipt(..), adapterInputs
 , VerifiedFlow, replaySelectedFlow, replayFlowPrefix, validateReplay
 , replayRoots, replayOptions, replayOld, replayContext, replayProgram, replayAST, replayRTL
 , replayInputs, replayReceipt ) where
import ECO.VerilogInfo
import ECOBind
import ECOASTSearch
import ECOASTProof
import ECOASTProofContext
import ECOASTCost(CostObservation(..),CostEnvelope(..))
import ECOASTTranslatedObjective
import ECOJSON(EvidenceJSON(..),renderEvidenceJSON)
import ECO.Replay(FileIdentity(..),ReplayManifest(..),AdapterReceipt(..),adapterSchema,fileIdentity,sameFileIdentity)
import ECO.Interface(fixedPorts)
import ECOMatchTypes(EcoRenamePlan(..))
import Verilog(VProgram)
import BinData(encode)
import GenABin()
import qualified Data.ByteString as B
import Control.Monad
import System.Directory
import System.FilePath

data CaptureFlow=CaptureFlow
 { cfCompiler::FilePath,cfModule::String
 , cfOldCapture::FilePath,cfNewCapture::FilePath
 , cfOldDirectory::FilePath,cfNewDirectory::FilePath
 , cfOldLibrary::Maybe FilePath,cfNewLibrary::Maybe FilePath
 , cfOutput::FilePath,cfSearch::SearchOptions,cfCommandSeconds::Int
 , cfImplementationManifest::FilePath }deriving(Eq,Read,Show)
data FlowBranch=FlowBranch
 { fbName::String,fbCost::Maybe LineCost,fbError::Maybe String
 , fbPlan::Maybe FileIdentity,fbOutput::Maybe FileIdentity }deriving(Eq,Read,Show)
data FlowReport=FlowReport
 { frSchema::Int,frModule::String,frSelected::String
 , frSearchPolicy::SearchOptions,frObjectivePolicy::String
 , frOrdinaryCost::LineCost,frSelectedCost::LineCost
 , frBranches::[FlowBranch],frOutput::FileIdentity
 , frInputs::[FileIdentity],frConfig::FileIdentity }deriving(Eq,Read,Show)

-- Everything a verifier executes or authenticates against, chosen by the caller.
data TrustRoots=TrustRoots
 { trCompiler::FilePath,trOldCapture::FilePath,trNewCapture::FilePath
 , trSdkManifest::FilePath,trDomain::ProofDomain,trSolver::FilePath,trSeconds::Int }
 deriving(Eq,Read,Show)
configRoots :: CaptureFlow -> TrustRoots
configRoots c=TrustRoots(cfCompiler c)(cfOldCapture c)(cfNewCapture c)(cfImplementationManifest c)
 (soDomain(cfSearch c))(soYosys(cfSearch c))(soProofSeconds(cfSearch c))
-- The caller's own flow.config is the usual roots file. A record inside the
-- flow directory it names would let that directory anchor its own verification.
rootsFromConfig :: FilePath -> IO TrustRoots
rootsFromConfig path=do
 config<-readRecord path::IO CaptureFlow
 file<-canonicalizePath path;flow<-canonicalizePath(cfOutput config)
 need(not(under flow file))"trust roots must not come from inside the flow directory"
 pure(configRoots config)
under root path=let prefix=addTrailingPathSeparator root in take(length prefix)path==prefix

-- The constructor is private: callers obtain this value only by fresh replay.
-- File bindings still need revalidation after any subsequent work.
data VerifiedFlow=VerifiedFlow
 { vfRoots::TrustRoots,vfOptions::SearchOptions,vfOld::VerilogInfo,vfOrdinary::VerilogInfo,vfContext::VerilogInfo
 , vfProgram::VProgram,vfAST::FileIdentity,vfRTL::FileIdentity
 , vfInputs::[FileIdentity],vfReceipt::FileIdentity }
-- Export ordinary accessor functions, not record labels: exported labels would
-- permit callers to record-update an otherwise abstract verified value.
replayRoots :: VerifiedFlow -> TrustRoots
replayRoots=vfRoots
replayOptions :: VerifiedFlow -> SearchOptions
replayOptions=vfOptions
replayOld, replayContext :: VerifiedFlow -> VerilogInfo
replayOld=vfOld
replayContext=vfContext
replayProgram :: VerifiedFlow -> VProgram
replayProgram=vfProgram
replayAST, replayRTL, replayReceipt :: VerifiedFlow -> FileIdentity
replayAST=vfAST
replayRTL=vfRTL
replayReceipt=vfReceipt
replayInputs :: VerifiedFlow -> [FileIdentity]
replayInputs=vfInputs
data Target=SelectedFlow|AcceptedPrefix String Int FileIdentity

replaySelectedFlow :: TrustRoots -> FilePath -> FilePath -> IO VerifiedFlow
replaySelectedFlow roots=replayFlow roots SelectedFlow

-- An explicit prefix keeps its recorded plan options; the caller's roots supply
-- the solver, domain and timeout and it independently measures the result.
replayFlowPrefix :: TrustRoots -> FilePath -> String -> Int -> FileIdentity -> FilePath -> IO VerifiedFlow
replayFlowPrefix roots flow branch count planIdentity=replayFlow roots(AcceptedPrefix branch count planIdentity)flow

validateReplay :: VerifiedFlow -> IO ()
validateReplay result=do
 mapM_ unchanged(replayReceipt result:replayInputs result)
 mapM_ validateVerilogInfo[vfOld result,vfOrdinary result,vfContext result]

need b s=unless b(failWith"flow replay"s)
required s=maybe(failWith"flow replay"s)pure
identity i=JObject[("path",JString(fiPath i)),("sha256",JString(fiSHA256 i))]

-- A flow loaded against the roots: effective.config agrees with them by file
-- identity, both adapted snapshots link to the root captures through their
-- adapter receipts, and every recorded plan chain is intact and bound.
data LoadedFlow=LoadedFlow
 { lfFlow::FilePath,lfConfig::CaptureFlow,lfConfigID::FileIdentity
 , lfOld::VerilogInfo,lfOrdinary::VerilogInfo,lfKept::Maybe VerilogInfo
 , lfPlans::[(String,FileIdentity,ASTPlan,[FileIdentity])]
 , lfBindings::[FileIdentity] }
loadFlow :: TrustRoots -> FilePath -> FilePath -> IO LoadedFlow
loadFlow roots flowPath controls=do
 flow<-canonicalizePath flowPath
 configID<-fileIdentity(flow</>"effective.config")
 config<-readRecord(fiPath configID)::IO CaptureFlow
 expectedFlow<-canonicalizePath(cfOutput config)
 need(expectedFlow==flow)"flow directory differs from configuration"
 let opts=cfSearch config;modul=cfModule config
     rooted label path root=do
      a<-fileIdentity path;b<-fileIdentity root
      need(a==b)(label++" differs from the trust root");pure b
 compiler<-rooted"configured compiler"(cfCompiler config)(trCompiler roots)
 oldCapture<-rooted"configured OLD capture"(cfOldCapture config)(trOldCapture roots)
 newCapture<-rooted"configured NEW capture"(cfNewCapture config)(trNewCapture roots)
 manifest<-rooted"configured SDK manifest"(cfImplementationManifest config)(trSdkManifest roots)
 solver<-rooted"configured solver"(soYosys opts)(trSolver roots)
 need(soDomain opts==trDomain roots)"flow proof domain differs from the trust root"
 need(soProofSeconds opts==trSeconds roots&&trSeconds roots>0)"flow proof timeout differs from the trust root or is invalid"
 old<-readVerilogInfo(flow</>"adapted-old"</>"capture"</>modul<.>"eco-ast")
 ordinary<-readVerilogInfo(flow</>"adapted-new"</>"capture"</>modul<.>"eco-ast")
 oldAdapter<-adapterInputs(flow</>"adapted-old")oldCapture compiler old
 newAdapter<-adapterInputs(flow</>"adapted-new")newCapture compiler ordinary
 keptPresent<-doesFileExist(flow</>"kept"</>modul<.>"eco-ast")
 kept<-if keptPresent then Just<$>readVerilogInfo(flow</>"kept"</>modul<.>"eco-ast")else pure Nothing
 let snapshots=[old,ordinary]++maybe[]pure kept
 need(all((==fiSHA256 compiler).fiSHA256.aeProducer.viEnvelope)snapshots)"snapshot producer differs from configured compiler"
 forM_ snapshots$ \s->do
  need(aeModule(viEnvelope s)==modul)"snapshot module differs from configuration"
  need(fixedPorts(viProgram s)==fixedPorts(viProgram ordinary))"snapshot fixed module ports differ"
 forM_ kept$ \k->do
  need(aeCapture(viEnvelope k)==aeCapture(viEnvelope ordinary))"selected context capture differs from ordinary NEW"
  need(aePlan(viEnvelope k)/=Nothing)"kept context lacks native keep plan"
 _<-verifyVerilogInfo old(controls</>"old-control")
 _<-verifyVerilogInfo ordinary(controls</>"ordinary-control")
 forM_ kept$ \k->verifyVerilogInfo k(controls</>"kept-control")
 plans<-fmap concat$forM[("ordinary",Just ordinary),("kept",kept)]$ \(branch,context)->do
  let path=flow</>(branch++"-search")</>"search.plan"
  present<-doesFileExist path
  if not present then pure[]else do
   c<-required"recorded plan without its branch context"context
   planID<-fileIdentity path;plan<-readRecord path::IO ASTPlan
   need(planOptions plan==opts&&planSchema plan==3)"selected plan schema/options differ"
   need(length(planSteps plan)<=256)"accepted-prefix budget exceeds 256 steps per branch"
   let inputPath=controls</>(branch++"-input.ast")
   B.writeFile inputPath(B.pack(encode(viProgram c)))
   input<-fileIdentity inputPath;rendered<-fileIdentity(fiPath(aeOutput(viEnvelope c)))
   need(fiSHA256 input==planInput plan&&fiSHA256 rendered==planRenderer plan)"plan input or renderer identity differs from capture"
   let bundle=takeDirectory path
   (finalHash,artifactIDs)<-foldM(\(previous,ids)step->do
     need(stepBefore step==previous&&stepAfter step==stepArtifactHash step)"broken accepted-step hash chain"
     bound<-flip FileIdentity(stepArtifactHash step)<$>boundMember bundle(stepArtifact step)(stepArtifactHash step)
     pure(stepAfter step,ids++[bound]))(planInput plan,[])(planSteps plan)
   need(finalHash==planOutput plan)"plan final hash differs from accepted chain"
   pure[(branch,planID,plan,artifactIDs)]
 pure(LoadedFlow flow config configID old ordinary kept plans
  (configID:solver:manifest:oldAdapter++newAdapter++map viIdentity snapshots))

replayFlow roots target flowPath output=do
 exists<-doesPathExist output;need(not exists)"fresh output directory required"
 createDirectory output
 loaded<-loadFlow roots flowPath output
 let flow=lfFlow loaded;config=lfConfig loaded;modul=cfModule config;opts=cfSearch config
     old=lfOld loaded;ordinary=lfOrdinary loaded
 (branch,report,reportIDs)<-case target of
  SelectedFlow->do
   reportID<-fileIdentity(flow</>"flow.report");saved<-readRecord(fiPath reportID)::IO FlowReport
   need(frSchema saved==5&&frModule saved==modul&&frSearchPolicy saved==opts)"flow schema/module/proof options mismatch"
   need(frConfig saved==lfConfigID loaded)"flow report binds a different effective configuration"
   mapM_ unchanged(frInputs saved++[frOutput saved])
   pure(frSelected saved,Just saved,reportID:frOutput saved:frInputs saved)
  AcceptedPrefix name count planID->do
   need(count>=0&&name `elem`["ordinary","kept"])"invalid accepted prefix"
   unchanged planID
   pure(name,Nothing,[])
 need(branch `elem`["ordinary","kept","ordinary-rollback"])"unknown selected branch"
 context<-if branch/="kept"then pure ordinary else required"selected kept context is missing"(lfKept loaded)
 let domain=trDomain roots;solver=trSolver roots;seconds=trSeconds roots
 solverID<-fileIdentity solver
 let runner _ directory alpha=runASTProofWithPins
      (childPinsFromInstances(viInstances context))(childPinsFromInstances(viInstances context))
      solver seconds directory domain alpha
 bridgeIDs<-if branch/="kept"then pure[]else do
  let relationPath=flow</>"keep-control-match.eco-plan"
  relationID<-fileIdentity relationPath;relation<-readRecord relationPath::IO EcoRenamePlan
  need(erpSchema relation==1&&erpModule relation==modul)"kept relation schema/module differs"
  proof<-runASTProofWithPins(childPinsFromInstances(viInstances ordinary))(childPinsFromInstances(viInstances context))
    solver seconds(output</>"ordinary-to-kept")domain(erpRenames relation)(viProgram ordinary)(viProgram context)
  writeFile(output</>"ordinary-to-kept.json")(renderProofResult proof)
  need(proofPassed proof)"fresh ordinary NEW to kept proof failed"
  unchanged relationID
  proofID<-fileIdentity(output</>"ordinary-to-kept.json")
  pure[relationID,proofID]
 (current,planIDs)<-if branch=="ordinary-rollback"then pure(viProgram ordinary,[])else do
  (sourceID,sourcePlan,artifactIDs)<-required"selected branch has no recorded plan"
    (lookup branch[(b,(i,p,a))|(b,i,p,a)<-lfPlans loaded])
  let sourcePath=fiPath sourceID;sourceBundle=takeDirectory sourcePath
  forM_ report$ \saved->case[row|row<-frBranches saved,fbName row==branch]of
   [row]->need(fbPlan row==Just sourceID&&fbError row==Nothing)"selected plan differs from bound flow branch"
   _->need False"selected flow branch is missing or ambiguous"
  (planPath,plan)<-case target of
   SelectedFlow->pure(sourcePath,sourcePlan)
   AcceptedPrefix _ count expected->do
    need(sourceID==expected)"selected source plan changed"
    need(count<=length(planSteps sourcePlan))"accepted prefix exceeds source plan"
    let steps=take count(planSteps sourcePlan)
        final=case reverse steps of []->planInput sourcePlan;step:_->stepAfter step
        prefix=sourcePlan{planSteps=steps,planOutput=final}
        bundle=output</>"bundle"
    createDirectory bundle
    forM_ steps $ \step->do
     let relative=stepArtifact step
     source<-boundMember sourceBundle relative(stepArtifactHash step)
     createDirectoryIfMissing True(takeDirectory(bundle</>relative));copyFile source(bundle</>relative)
    let path=bundle</>"prefix.plan"
    writeFile path(show prefix++"\n")
    pure(path,prefix)
  let replayDirectory=output</>"replay"
  (current,objectiveIDs)<-case target of
   AcceptedPrefix{}->do
    result<-replayASTPlanWith runner opts(renderVerilogText context)planPath replayDirectory(viProgram context)
    pure(result,[])
   SelectedFlow->do
    let objectivePath=sourceBundle</>"objective.plan"
    available<-doesFileExist objectivePath
    if not available then do
     need(fmap frObjectivePolicy report==Just"whole-file-raw-v1")"selected objective envelope missing"
     result<-replayASTPlanWith runner opts(renderVerilogText context)planPath replayDirectory(viProgram context)
     pure(result,[])
    else do
     objectiveID<-fileIdentity objectivePath;envelope<-readRecord objectivePath::IO CostEnvelope
     need(fmap frObjectivePolicy report==Just(costPolicyIdentity envelope))"selected objective differs from flow report"
     witnessIDs<-forM(("objective/old.score.v",costOldWitnessHash envelope):
        [(costWitness observation,costWitnessHash observation)|observation<-costObservations envelope])$ \(relative,digest)->
       flip FileIdentity digest<$>boundMember sourceBundle relative digest
     mapM_ unchanged(costSourceIdentities envelope)
     provider<-case costPolicyIdentity envelope of
      "whole-file-raw-v1"->pure(wholeFileCostProvider(renderVerilogText old)(renderVerilogText context))
      "translated-raw-v1"->pure(translatedCostProvider TranslatedRaw[cfImplementationManifest config]old context)
      "translated-normalized-v1"->pure(translatedCostProvider TranslatedNormalized[cfImplementationManifest config]old context)
      _->failWith"flow replay""unsupported full-flow objective"
     (result,_,_)<-replayASTPlanWithCost provider(viProgram old)runner opts(renderVerilogText context)planPath replayDirectory(viProgram context)
     pure(result,objectiveID:witnessIDs++costSourceIdentities envelope)
  unchanged sourceID
  selectedID<-fileIdentity(replayDirectory</>"selected.ast")
  need(fiSHA256 selectedID==planOutput plan)"selected replay identity mismatch"
  pure(current,sourceID:selectedID:artifactIDs++objectiveIDs)
 let ast=output</>"qualified-input.ast";rtl=output</>modul<.>"v"
 B.writeFile ast(B.pack(encode current));emitVerilog context rtl current
 actual<-fileIdentity rtl
 forM_ report$ \saved->need(fiSHA256 actual==fiSHA256(frOutput saved))"fresh upstream replay differs from selected flow RTL"
 astID<-fileIdentity ast
 closureIDs<-closureIdentities output
 let inputs=[solverID,astID,actual]++lfBindings loaded++reportIDs++bridgeIDs++planIDs++closureIDs
 mapM_ unchanged inputs
 mapM_ validateVerilogInfo[old,ordinary,context]
 let receiptPath=output</>"upstream-qualification.json"
 writeFile receiptPath(renderEvidenceJSON(JObject
  [("schema",JNumber"2"),("status",JString"freshly-replayed"),("branch",JString branch),
   ("proofDomain",JString(show domain)),("solver",identity solverID),("proofSeconds",JNumber(show seconds)),
   ("inputAST",identity astID),
   ("scope",JString"Fresh ordinary NEW to selected CURRENT accepted-plan replay with caller-supplied roots; kept bridge proved separately when selected."),
   ("evidence",JArray(map identity inputs))])++"\n")
 receipt<-fileIdentity receiptPath
 pure(VerifiedFlow roots opts old ordinary context current astID actual inputs receipt)

-- Native adaptation links an original capture to the current compiler and
-- proves ordinary output parity. Paths may legitimately differ after adaptation,
-- and a capture that moved behind a compatibility symlink still links by the
-- native identity rule. The original producer is identity only: it is bound
-- inside the source manifest and never executed or re-hashed here.
adapterInputs :: FilePath -> FileIdentity -> FileIdentity -> VerilogInfo -> IO [FileIdentity]
adapterInputs directory source compiler snapshot=do
 receiptID<-fileIdentity(directory</>"adapter-receipt")
 receipt<-readRecord(fiPath receiptID)::IO AdapterReceipt
 sourceLinked<-sameFileIdentity(arSource receipt)source
 producerLinked<-sameFileIdentity(arAdapterProducer receipt)compiler
 need(arSchema receipt==adapterSchema&&sourceLinked&&producerLinked&&
      arAdapted receipt==aeCapture(viEnvelope snapshot))"native adapter source/capture/producer linkage differs"
 original<-readRecord(fiPath source)::IO ReplayManifest
 need(arOriginalProducer receipt==rmProducer original)"native adapter original producer differs"
 let expected=map snd(rmOutputs original)
 need(map fiSHA256 expected==map fiSHA256(arCurrentOutputs receipt))"native adapter ordinary output parity differs"
 case[i|(name,i)<-rmOutputs original,takeFileName name==aeModule(viEnvelope snapshot)<.>"v"]of
  [moduleOutput]->need(fiSHA256 moduleOutput==fiSHA256(aeOutput(viEnvelope snapshot)))"adapted snapshot differs from configured ordinary module output"
  _->failWith"flow replay""configured ordinary module output is missing or ambiguous"
 let ids=[receiptID,source,arAdapterProducer receipt,arAdapted receipt]++expected++arCurrentOutputs receipt
 mapM_ unchanged ids
 pure ids
