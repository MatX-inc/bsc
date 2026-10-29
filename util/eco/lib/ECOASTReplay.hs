{-# LANGUAGE CPP #-}
-- A fresh proof of ordinary NEW through the selected flow or an accepted prefix.
-- Stored receipts bind inputs; their previous pass/fail fields grant no authority.
module ECOASTReplay
 ( VerifiedFlow, replaySelectedFlow, replayFlowPrefix, validateReplay
 , replayOptions, replayOld, replayContext, replayProgram, replayAST, replayRTL
 , replayInputs, replayReceipt ) where
import ECOAST
import ECOASTFlow
import ECOASTSearch
import ECOASTProof
import ECOASTProofContext
import ECOASTCost(CostObservation(..),CostEnvelope(..),wholeFileCostProvider)
import ECOASTTranslatedObjective
import ECOASTInvariant(EvidenceJSON(..),renderEvidenceJSON)
import ECO.Replay(FileIdentity(..),ReplayManifest(..),fileIdentity)
import ECO.Interface(fixedPorts)
import ECOMatchTypes(EcoRenamePlan(..))
import Verilog(VProgram)
import BinData(encode)
import GenABin()
import qualified Data.ByteString as B
import Data.List(sort)
import Control.Monad
import System.Directory
import System.FilePath
import Text.Read(readMaybe)

-- The constructor is private: callers obtain this value only by fresh replay.
-- File bindings still need revalidation after any subsequent work.
data VerifiedFlow=VerifiedFlow
 { vfOptions::SearchOptions, vfOld::VerilogInfo, vfOrdinary::VerilogInfo, vfContext::VerilogInfo
 , vfProgram::VProgram, vfAST::FileIdentity, vfRTL::FileIdentity
 , vfInputs::[FileIdentity], vfReceipt::FileIdentity }
-- Export ordinary accessor functions, not record labels: exported labels would
-- permit callers to record-update an otherwise abstract verified value.
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

replaySelectedFlow :: FilePath -> FilePath -> IO VerifiedFlow
replaySelectedFlow=replayFlow SelectedFlow Nothing

-- An explicit prefix keeps its recorded proof domain and plan options. The
-- caller may select the solver/timeout, then independently measures its result.
replayFlowPrefix :: ProofDomain -> FilePath -> Int -> FilePath -> String -> Int -> FileIdentity -> FilePath -> IO VerifiedFlow
replayFlowPrefix domain solver seconds flow branch count planIdentity=
 replayFlow(AcceptedPrefix branch count planIdentity)(Just(domain,solver,seconds))flow

validateReplay :: VerifiedFlow -> IO ()
validateReplay result=do
 mapM_ unchanged(replayReceipt result:replayInputs result)
 mapM_ validateVerilogInfo[vfOld result,vfOrdinary result,vfContext result]

need b s=unless b(fail("flow replay: "++s))
record p=readFile p>>=maybe(fail("invalid typed record: "++p))pure.readMaybe
unchanged i=fileIdentity(fiPath i)>>= \actual->need(actual==i)("changed bound input: "++fiPath i)
identity i=JObject[("path",JString(fiPath i)),("sha256",JString(fiSHA256 i))]
closure root=do
 names<-fmap sort(listDirectory root)
 fmap concat $ forM names $ \name->do
  let path=root</>name
  isDirectory<-doesDirectoryExist path
  if isDirectory then closure path else pure[path]
member bundle relative expected=do
 need(isRelative relative&&".."`notElem`splitDirectories relative)"artifact path escapes bundle"
 root<-canonicalizePath bundle;path<-canonicalizePath(root</>relative)
 need(take(length(addTrailingPathSeparator root))path==addTrailingPathSeparator root)"artifact symlink escapes bundle"
 ident<-fileIdentity path
 need(fiSHA256 ident==expected)"candidate artifact changed"
 pure ident

replayFlow target proofTools flowPath output=do
 flow<-canonicalizePath flowPath
 exists<-doesPathExist output;need(not exists)"fresh output directory required"
 effective<-doesFileExist(flow</>"effective.config")
 let fallback=case target of SelectedFlow->takeDirectory flow</>"flow.config";_->flow++".config"
 configID<-fileIdentity(if effective then flow</>"effective.config"else fallback)
 config<-record(fiPath configID)::IO CaptureFlow
 expectedFlow<-canonicalizePath(cfOutput config)
 need(expectedFlow==flow)"flow directory differs from configuration"
 (branch,report,reportIDs)<-case target of
  SelectedFlow->do
   reportID<-fileIdentity(flow</>"flow.report");saved<-record(fiPath reportID)::IO FlowReport
   need(frSchema saved `elem`[3,4]&&frModule saved==cfModule config&&frSearchPolicy saved==cfSearch config)"flow schema/module/proof options mismatch"
   mapM_ unchanged(frInputs saved++[frOutput saved])
   pure(frSelected saved,Just saved,reportID:frOutput saved:frInputs saved)
  AcceptedPrefix name count planID->do
   need(count>=0&&name `elem`["ordinary","kept"])"invalid accepted prefix"
   unchanged planID
   pure(name,Nothing,[])
 need(branch `elem`["ordinary","kept","ordinary-rollback"])"unknown selected branch"
 let modul=cfModule config;opts=cfSearch config
     (domain,solver,seconds)=case proofTools of
       Nothing->(soDomain opts,soYosys opts,soProofSeconds opts)
       Just supplied->supplied
 need(domain==soDomain opts&&seconds>0)"flow proof domain differs from explicit requested domain or timeout is invalid"
 solverID<-fileIdentity solver;implementation<-fileIdentity __FILE__
 createDirectory output
 old<-readVerilogInfo(flow</>"adapted-old"</>modul<.>"eco-ast")
 ordinary<-readVerilogInfo(flow</>"adapted-new"</>modul<.>"eco-ast")
 oldCapture<-fileIdentity(cfOldCapture config);newCapture<-fileIdentity(cfNewCapture config);compiler<-fileIdentity(cfCompiler config)
 oldAdapter<-adapterInputs(flow</>"adapted-old")oldCapture compiler old
 newAdapter<-adapterInputs(flow</>"adapted-new")newCapture compiler ordinary
 context<-if branch=="kept"then readVerilogInfo(flow</>"kept"</>modul<.>"eco-ast")else pure ordinary
 need(all ((==fiSHA256 compiler).fiSHA256.aeProducer.viEnvelope)[old,ordinary,context])"snapshot producer differs from configured compiler"
 need(aeCapture(viEnvelope context)==aeCapture(viEnvelope ordinary))"selected context capture differs from ordinary NEW"
 when(branch=="kept")$need(aePlan(viEnvelope context)/=Nothing)"kept context lacks native keep plan"
 forM_[old,ordinary,context]$ \s->do
  need(aeModule(viEnvelope s)==modul)"snapshot module differs from configuration"
  need(fixedPorts(viProgram s)==fixedPorts(viProgram ordinary))"snapshot fixed module ports differ"
 _<-verifyVerilogInfo old(output</>"old-control")
 _<-verifyVerilogInfo ordinary(output</>"ordinary-control")
 when(branch=="kept")$void(verifyVerilogInfo context(output</>"kept-control"))
 let runner _ directory alpha contracts=runASTProofWithContracts
      (childPinsFromInstances(viInstances context))(childPinsFromInstances(viInstances context))
      contracts solver seconds directory domain alpha
 bridgeIDs<-if branch/="kept"then pure[]else do
  let relationPath=flow</>"keep-control-match.eco-plan"
  relationID<-fileIdentity relationPath;relation<-record relationPath::IO EcoRenamePlan
  need(erpSchema relation==1&&erpModule relation==modul)"kept relation schema/module differs"
  proof<-runASTProofWithPins(childPinsFromInstances(viInstances ordinary))(childPinsFromInstances(viInstances context))
    solver seconds(output</>"ordinary-to-kept")domain(erpRenames relation)(viProgram ordinary)(viProgram context)
  writeFile(output</>"ordinary-to-kept.json")(renderProofResult proof)
  need(proofPassed proof)"fresh ordinary NEW to kept proof failed"
  unchanged relationID
  proofID<-fileIdentity(output</>"ordinary-to-kept.json")
  pure[relationID,proofID]
 (current,planIDs)<-if branch=="ordinary-rollback"then pure(viProgram ordinary,[])else do
  let sourcePath=flow</>(branch++"-search")</>"search.plan"
  sourceID<-fileIdentity sourcePath;sourcePlan<-record sourcePath::IO ASTPlan
  case report of
   Nothing->pure()
   Just saved->case [row|row<-frBranches saved,fbName row==branch]of
    [row]->need(fbPlan row==Just sourceID&&fbError row==Nothing)"selected plan differs from bound flow branch"
    _->fail"selected flow branch is missing or ambiguous"
  need(planOptions sourcePlan==opts&&planSchema sourcePlan==2)"selected plan schema/options differ"
  let sourceBundle=takeDirectory sourcePath
  artifactIDs<-fmap concat $ forM(planSteps sourcePlan)$ \step->mapM(uncurry(member sourceBundle))
    [(stepArtifact step,stepArtifactHash step),(stepContractsArtifact step,stepContractsHash step)]
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
    forM_ steps $ \step->forM_[(stepArtifact step,stepArtifactHash step),(stepContractsArtifact step,stepContractsHash step)]$ \(relative,digest)->do
     source<-member sourceBundle relative digest
     createDirectoryIfMissing True(takeDirectory(bundle</>relative));copyFile(fiPath source)(bundle</>relative)
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
     need(maybe False ((=="whole-file-raw-v1").takeWhile(/=';').frObjectivePolicy)report)"selected objective envelope missing"
     result<-replayASTPlanWith runner opts(renderVerilogText context)planPath replayDirectory(viProgram context)
     pure(result,[])
    else do
     objectiveID<-fileIdentity objectivePath;envelope<-record objectivePath::IO CostEnvelope
     need(maybe False ((==costPolicyIdentity envelope).takeWhile(/=';').frObjectivePolicy)report)"selected objective differs from flow report"
     witnessIDs<-mapM(uncurry(member sourceBundle))
       (("objective/old.score.v",costOldWitnessHash envelope):
        [(costWitness observation,costWitnessHash observation)|observation<-costObservations envelope])
     mapM_ unchanged(costSourceIdentities envelope)
     provider<-case costPolicyIdentity envelope of
      "whole-file-raw-v1"->pure(wholeFileCostProvider(renderVerilogText old)(renderVerilogText context))
      "translated-raw-v1"->pure(translatedCostProvider TranslatedRaw[cfImplementationManifest config]old context)
      "translated-normalized-v1"->pure(translatedCostProvider TranslatedNormalized[cfImplementationManifest config]old context)
      _->fail"unsupported full-flow objective; use explicit branch objective replay"
     (result,_,_)<-replayASTPlanWithCost provider(viProgram old)runner opts(renderVerilogText context)planPath replayDirectory(viProgram context)
     pure(result,objectiveID:witnessIDs++costSourceIdentities envelope)
  unchanged sourceID
  selectedID<-fileIdentity(replayDirectory</>"selected.ast")
  need(fiSHA256 selectedID==planOutput plan)"selected replay identity mismatch"
  pure(current,sourceID:selectedID:artifactIDs++objectiveIDs)
 let ast=output</>"qualified-input.ast";rtl=output</>modul<.>"v"
 B.writeFile ast(B.pack(encode current));emitVerilog context rtl current
 actual<-fileIdentity rtl
 case report of
  Just saved->need(fiSHA256 actual==fiSHA256(frOutput saved))"fresh upstream replay differs from selected flow RTL"
  Nothing->pure()
 astID<-fileIdentity ast
 closureIDs<-closure output>>=mapM fileIdentity
 let inputs=[configID,implementation,solverID,astID,actual,viIdentity old,viIdentity ordinary,viIdentity context]++reportIDs++oldAdapter++newAdapter++bridgeIDs++planIDs++closureIDs
 mapM_ unchanged inputs
 mapM_ validateVerilogInfo[old,ordinary,context]
 let receiptPath=output</>"upstream-qualification.json"
 writeFile receiptPath(renderEvidenceJSON(JObject
  [("schema",JNumber"1"),("status",JString"freshly-replayed"),("branch",JString branch),
   ("proofDomain",JString(show domain)),("inputAST",identity astID),
   ("scope",JString"Fresh ordinary NEW to selected CURRENT accepted-plan replay; kept bridge proved separately when selected."),
   ("evidence",JArray(map identity inputs))])++"\n")
 receipt<-fileIdentity receiptPath
 pure(VerifiedFlow opts old ordinary context current astID actual inputs receipt)

-- Native adaptation links original captures to new compiler identities and
-- proves ordinary output parity. Paths may legitimately differ after adaptation.
adapterInputs directory source compiler snapshot=do
 let path=directory</>"adapter-receipt"
 receiptID<-fileIdentity path
 (schema,recordedSource,originalProducer,adapterProducer,adapted,originalOutputs,currentOutputs,relocation,_)<-
  record path::IO(String,FileIdentity,FileIdentity,FileIdentity,FileIdentity,[FileIdentity],[FileIdentity],Maybe FileIdentity,String)
 need(schema=="bsc-eco-native-adapter-1"&&recordedSource==source&&adapterProducer==compiler&&adapted==aeCapture(viEnvelope snapshot))"native adapter source/capture/producer linkage differs"
 original<-record(fiPath source)::IO ReplayManifest
 need(originalProducer==rmProducer original)"native adapter original producer differs"
 let expected=map snd(rmOutputs original);digests=map fiSHA256
 need(digests expected==digests originalOutputs&&digests expected==digests currentOutputs)"native adapter ordinary output parity differs"
 case[i|(name,i)<-rmOutputs original,takeFileName name==aeModule(viEnvelope snapshot)<.>"v"]of
  [moduleOutput]->need(fiSHA256 moduleOutput==fiSHA256(aeOutput(viEnvelope snapshot)))"adapted snapshot differs from configured ordinary module output"
  _->fail"configured ordinary module output is missing or ambiguous"
 let ids=[receiptID,source,originalProducer,adapterProducer,adapted]++expected++originalOutputs++currentOutputs++maybe[](:[])relocation
 mapM_ unchanged ids
 pure ids
