{-# LANGUAGE CPP #-}
-- A finishing stage over an already qualified CURRENT value. This API proves
-- CURRENT -> output only; callers establish and bind the upstream proof chain.
module ECOASTFinishOrder(FinishOrderReport(..),finishDeclarationOrderFrom,orderCostImproves,firstQualified) where
import ECOAST
import ECOASTDeclarationOrder
import ECOASTJointDeclarationOrder
import ECOASTTransform(ASTCandidate(..))
import ECOASTSearch
import ECOASTCost
import ECOASTTranslatedObjective
import ECOASTTranslatedMetric
import ECOASTMetric(MetricInput(..))
import ECOASTProof
import ECOASTProofContext
import ECOASTInvariant(EvidenceJSON(..),renderEvidenceJSON)
import ECO.Replay(FileIdentity(..),fileIdentity)
import ECO.Interface(fixedPorts)
import Verilog
import BinData(encode)
import GenABin()
import qualified Data.ByteString as B
import Data.List(sort,sortOn)
import Control.Monad
import System.Directory
import System.FilePath

data FinishOrderReport=FinishOrderReport
 { foStatus::String,foReason::String,foInputAST::FileIdentity,foOutputAST::FileIdentity
 , foOutputRTL::FileIdentity,foBeforeRaw::Int,foBeforeNormalized::Int
 , foAfterRaw::Int,foAfterNormalized::Int,foWireNames::[(String,String)],foEvidence::[FileIdentity] }deriving(Eq,Read,Show)
orderCostImproves (beforeRaw,beforeNormalized)(afterRaw,afterNormalized)=
 (afterNormalized,afterRaw)<(beforeNormalized,beforeRaw)
-- Try candidates in score order; a failed proof is not a veto on a later
-- independently proved improvement. Every attempt keeps its own artifacts.
firstQualified :: [a] -> (Int -> a -> IO (Maybe b)) -> IO (Maybe b)
firstQualified xs action=go 1 xs where
 go _ []=pure Nothing
 go i(x:rest)=action i x >>= maybe(go(i+1)rest)(pure . Just)
closure root=do
 names<-fmap sort(listDirectory root)
 fmap concat $ forM names $ \name->do
  let path=root</>name
  isDirectory<-doesDirectoryExist path
  if isDirectory then closure path else pure[path]
need b s=unless b(fail("ECO finish order: "++s))
unchanged i=fileIdentity(fiPath i)>>= \actual->need(actual==i)("changed input: "++fiPath i)
ident i=JObject[("path",JString(fiPath i)),("sha256",JString(fiSHA256 i))]
num=JNumber . show
costJSON(r,n)=JObject[("raw",num r),("normalized",num n)]

-- Inputs are authenticated rendering contexts and an explicitly supplied native
-- CURRENT AST. The result does not by itself assert OLD/NEW functional intent.
-- Upstream identities are checked and recorded, never treated as proof grants.
finishDeclarationOrderFrom :: FileIdentity -> [FileIdentity] -> SearchOptions -> VerilogInfo -> VerilogInfo ->
 VProgram -> FilePath -> IO FinishOrderReport
finishDeclarationOrderFrom manifest upstream options old context current output=do
 exists<-doesPathExist output;need(not exists)"fresh output directory required"
 need(soProofSeconds options>0)"positive proof timeout required"
 need(fixedPorts(viProgram old)==fixedPorts current&&fixedPorts(viProgram context)==fixedPorts current)"fixed module interface changed"
 mapM_ validateVerilogInfo[old,context]
 mapM_ unchanged(manifest:upstream)
 let sources=[__FILE__,takeDirectory __FILE__ </> "ECOASTDeclarationOrder.hs",
       takeDirectory __FILE__ </> "ECOASTPreservation.hs",
       takeDirectory __FILE__ </> "ECOASTJointDeclarationOrder.hs",
       takeDirectory __FILE__ </> "ECOASTValueNames.hs"]
 sourceIDs<-mapM fileIdentity sources
 solver<-fileIdentity(soYosys options)
 createDirectory output
 let input=output</>"input.ast";inputRTL=output</>"input"</>aeModule(viEnvelope context)<.>"v"
 B.writeFile input(B.pack(encode current));inputID<-fileIdentity input
 emitVerilog context inputRTL current
 forM_["score-raw","score-normalized"](createDirectory.(output</>))
 let raw=translatedCostProvider TranslatedRaw sources old context
     normalized=translatedCostProvider TranslatedNormalized sources old context
 rawContext<-prepareCost raw(output</>"score-raw")(viProgram old)
 normalizedContext<-prepareCost normalized(output</>"score-normalized")(viProgram old)
 let score label program=do
      r<-scoreCost rawContext raw label program
      n<-scoreCost normalizedContext normalized label program
      pure(changedLines(costValue r),changedLines(costValue n))
 before<-score "input" current
 let candidates=declarationOrderCandidates 1(viProgram old)current++
       jointDeclarationOrderCandidates 1(viProgram old)current
     forward c=[(n,o)|(o,n)<-acStateMap c]
     preserved c=if acFeature c=="DeclarationOrder" then declarationOrderOnly current(acProgram c)
       else jointDeclarationOrderOnly(forward c)current(acProgram c)
     runOptions=options{soRounds=1,soCandidates=1}
     runner opts directory alpha contracts=runASTProofWithContracts
       (childPinsFromInstances(viInstances context))(childPinsFromInstances(viInstances context))
       contracts(soYosys opts)(soProofSeconds opts)directory(soDomain opts)alpha
 scored<-forM candidates $ \candidate->do
  need(preserved candidate)"candidate changed more than permitted wire identities and declaration order"
  after<-score(acFeature candidate)(acProgram candidate)
  pure(after,candidate)
 let eligible=sortOn(\((r,n),c)->(n,r,acFeature c))[(cost,c)| (cost,c)<-scored,orderCostImproves before cost]
 qualified<-firstQualified eligible $ \attempt (after,candidate)->do
    let searchObjective=if snd after<snd before then normalized else raw
        selectedProvider=if acFeature candidate=="DeclarationOrder" then declarationOrderProvider else jointDeclarationOrderProvider
        provider=selectedProvider{cpSources=map fiPath(sourceIDs++upstream)}
        selectedOptions=runOptions{soFeatures=[acFeature candidate]}
        suffix=if attempt==1 then "" else "-"++show attempt
        searchDir=output</>"search"++suffix
        replayDir=output</>"replay"++suffix
    workers<-searchWorkersFromEnvironment
    result<-optimizeAST (defaultSearchRequest selectedOptions searchDir
      (SearchInput (viProgram old) (renderVerilogText old))
      (SearchInput current (renderVerilogText context)))
      { searchObjective=searchObjective, searchProvider=provider, searchProof=runner, searchWorkers=workers }
    (replayed,_,_)<-replayASTPlanWithCost searchObjective(viProgram old)runner selectedOptions(renderVerilogText context)
      (searchDir</>"search.plan")replayDir current
    need(encode replayed==encode(srProgram result))"independent saved-plan replay differs"
    let changed=encode replayed/=encode current
    need(not changed||encode replayed==encode(acProgram candidate))"search selected a different unscored proposal"
    writeFile(output</>"attempt-"++show attempt++".json")(renderEvidenceJSON(JObject
      [("feature",JString(acFeature candidate)),("qualified",JBool changed),
       ("proposalCost",costJSON after),("searchDirectory",JString searchDir),("replayDirectory",JString replayDir)])++"\n")
    pure(if changed then Just(replayed,forward candidate)else Nothing)
 let (selected,wireNames,reason)=case qualified of
      Just(program,pairs)->(program,pairs,"lexicographic substantive/raw improvement; independent proof and saved-plan replay passed")
      Nothing->(current,[],if null candidates then declarationOrderReason(viProgram old)current++"; "++jointDeclarationOrderReason(viProgram old)current
         else if null eligible then "no lexicographic substantive/raw improvement; regressive isolated steps rejected"
         else "all improving proposals rejected by independent proof")
     unchangedProgram=encode selected==encode current
 need(declarationOrderOnly current selected||jointDeclarationOrderOnly wireNames current selected||encode selected==encode current)
   "selected output changed declaration payload or nondeclaration structure beyond wire names"
 let ast=output</>"selected.ast";rtl=output</>aeModule(viEnvelope context)<.>"v"
 B.writeFile ast(B.pack(encode selected));emitVerilog context rtl selected
 after<-score "selected" selected
 need(unchangedProgram||orderCostImproves before after)"selected result failed final dual-metric acceptance"
 metric<-measureTranslatedAST(output</>"metric")(Snapshot(fiPath(viIdentity old)))
  (ContextAST(fiPath(viIdentity context))ast rtl)
 need(after==(changedLines(translatedRaw metric),changedLines(translatedNormalized metric)))"emitted metric differs from score"
 outputID<-fileIdentity ast;rtlID<-fileIdentity rtl;inputRTLID<-fileIdentity inputRTL
 allArtifacts<-closure output
 artifactIDs<-mapM fileIdentity allArtifacts
 mapM_ validateVerilogInfo[old,context]
 mapM_ unchanged(manifest:upstream++sourceIDs++[solver,inputID])
 need(fiSHA256 inputID/=fiSHA256 outputID||unchangedProgram)"changed flag and serialized AST identity disagree"
 when(unchangedProgram)$do
  need(fiSHA256 inputID==fiSHA256 outputID)"no-op changed serialized CURRENT metadata"
  need(fiSHA256 inputRTLID==fiSHA256 rtlID)"no-op changed rendered CURRENT bytes"
 let status=if unchangedProgram then "unchanged" else "changed"
     evidence=manifest:upstream++sourceIDs++[solver,viIdentity old,viIdentity context,inputID,outputID,rtlID]++artifactIDs
     report=FinishOrderReport status reason inputID outputID rtlID(fst before)(snd before)(fst after)(snd after)wireNames evidence
 writeFile(output</>"finish-order.report")(show report++"\n")
 writeFile(output</>"receipt.json")(renderEvidenceJSON(JObject
  [("schema",num 2),("status",JString status),("reason",JString reason),("proofDomain",JString(show(soDomain options))),
   ("scope",JString"CURRENT to finished output only. Upstream proof-chain qualification is the caller's responsibility; upstream identities are bindings, not semantic proof grants."),
   ("implementationManifest",ident manifest),("oldContext",ident(viIdentity old)),("currentContext",ident(viIdentity context)),
   ("inputAST",ident inputID),("inputRTL",ident inputRTLID),("selectedAST",ident outputID),("selectedRTL",ident rtlID),
   ("before",costJSON before),("after",costJSON after),("preservesDeclarationObjectsAndNondeclarationStructure",JBool(null wireNames)),
   ("preservesDeclarationObjectsAndNondeclarationStructureModuloWireNames",JBool True),
   ("forwardWireNames",JArray[JArray[JString n,JString o]|(n,o)<-wireNames]),
   ("freshSavedPlanReplay",JBool(not unchangedProgram)),("inputEqualsOutput",JBool(unchangedProgram)),
   ("selectedObjectivePolicy",JString(if unchangedProgram then "none" else if snd after<snd before then "translated-normalized-v1" else "translated-raw-v1")),("rawTradeoff",JBool(fst after>fst before)),("beforeVariantPreserved",JBool True),("acceptance",JString"lexicographic normalized then raw decrease; declaration partitions and wrapper objects fixed modulo recorded wire identities; input variant retained"),
   ("evidence",JArray(map ident evidence))])++"\n")
 pure report
