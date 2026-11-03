-- A flow whose search policy carries no layout feature: reclaiming the vacant
-- historical state group leaves the register groups in NEW's order, and the
-- public finishing stage must restore OLD's order under its own fresh proof
-- and saved-plan replay, binding every artifact in its receipt.
module Main where
import ECOASTFlow
import ECOASTSearch
import ECOASTProof
import ECOASTFinishOrder(FinishOrderReport(..))
import ECOASTVacancy(vacancyProvider)
import ECOBind(readRecord)
import ECOJSON
import ECO.Replay(FileIdentity(..),fileIdentity)
import Control.Monad(forM,forM_,unless)
import Data.List(findIndex,isInfixOf,isPrefixOf,isSuffixOf)
import Data.Maybe(fromMaybe)
import System.Directory
import System.Environment
import System.Exit(ExitCode(..))
import System.FilePath
import System.Process(readProcessWithExitCode)

check ok why=unless ok(fail why)
json path=readFile path>>=either(\e->fail(path++": "++e))pure.parseEvidenceJSON
field k(JObject kvs)=lookup k kvs
field _ _=Nothing
str k v=case field k v of Just(JString s)->Just s;_->Nothing

main=do
 [bluehs,sdk,compiler,oldCapture,newCapture,oldDir,newDir,library,manifest,yosys,rootArg]<-getArgs
 createDirectory rootArg;root<-canonicalizePath rootArg
 let flowDir=root</>"flow";roots=root</>"flow.config"
     options=defaultSearchOptions{soDomain=StrictFourState,soFeatures=["F01","F21"],soYosys=yosys}
     config=CaptureFlow compiler"mkFinishChanged"oldCapture newCapture oldDir newDir(Just library)(Just library)flowDir options 120 manifest
     provider=combineCandidateProviders[vacancyProvider,defaultCandidateProvider]
 writeFile roots(show config++"\n")
 report<-runCaptureFlow(FlowPolicy provider wholeFileObjective DeferInvariants)config
 check(frSelected report=="ordinary")"fixture must select the ordinary branch"
 plan<-readRecord(flowDir</>"ordinary-search"</>"search.plan")::IO ASTPlan
 check("F21"`elem`map stepFeature(planSteps plan))"fixture must reclaim the vacant historical state group"
 -- The public finishing command over the caller's roots.
 let finished=root</>"finished";finish=finished</>"finish"
 (code,out,err)<-readProcessWithExitCode bluehs["-i","-i"++sdk</>"lib",sdk</>"scripts/finish-order.hs",roots,flowDir,finished]""
 writeFile(root</>"finish-order.log")(out++err)
 check(code==ExitSuccess&&"changed:"`isPrefixOf`out)("finish-order.hs must report a changed declaration order: "++out++err)
 result<-readRecord(finish</>"finish-order.report")::IO FinishOrderReport
 check(foStatus result=="changed"&&fiSHA256(foInputAST result)/=fiSHA256(foOutputAST result))"finishing must change the serialized AST"
 check(foAfterRaw result<foBeforeRaw result&&foAfterNormalized result<=foBeforeNormalized result)"finishing must improve the dual metric"
 -- One declaration-order step, proved at search time and again by the
 -- independent saved-plan replay, each with its reference and candidate.
 finishPlan<-readRecord(finish</>"search"</>"search.plan")::IO ASTPlan
 step<-case planSteps finishPlan of [s]|stepFeature s=="DeclarationOrder"->pure s;_->fail"finishing must accept exactly one declaration-order step"
 names<-listDirectory(finish</>"search")
 passed<-fmap concat$forM[f|f<-names,"candidate-"`isPrefixOf`f,".proof.json"`isSuffixOf`f]$ \f->do
  v<-json(finish</>"search"</>f)
  pure[finish</>"search"</>takeWhile(/='.')f++"-proof"|str"candidate"(fromMaybe(JObject[])(field"subject"v))==Just(stepArtifactHash step),str"status"v==Just"passed"]
 proofDir<-case passed of [d]->pure d;_->fail"finishing search must hold one passed receipt for the accepted step"
 let replayDir=finish</>"replay"</>"proof-1"
 forM_[proofDir,replayDir]$ \d->forM_["reference.ast","candidate.ast"]$ \n->doesFileExist(d</>n)>>=flip check(d</>n++" missing")
 replayed<-json(replayDir<.>"json")
 check(str"status"replayed==Just"passed")"fresh saved-plan replay proof must pass"
 reference<-fileIdentity(replayDir</>"reference.ast");candidate<-fileIdentity(replayDir</>"candidate.ast")
 check(fiSHA256 reference==fiSHA256(foInputAST result)&&fiSHA256 candidate==fiSHA256(foOutputAST result))"replay proof subject must be the finishing input and output"
 -- The receipt binds the upstream verification and every finishing artifact.
 receipt<-json(finish</>"receipt.json")
 check(str"status"receipt==Just"changed"&&field"freshSavedPlanReplay"receipt==Just(JBool True)&&field"inputEqualsOutput"receipt==Just(JBool False))"receipt must record the changed status and the fresh replay"
 let bound=[(p,s)|Just(JArray xs)<-[field"evidence"receipt],x<-xs,Just p<-[str"path"x],Just s<-[str"sha256"x]]
 forM_ bound$ \(p,s)->do i<-fileIdentity p;check(fiSHA256 i==s)("receipt evidence changed: "++p)
 forM_[finished</>"upstream-qualification.json",replayDir<.>"json",replayDir</>"reference.ast",replayDir</>"candidate.ast",
       proofDir</>"candidate.ast",finish</>"selected.ast",finish</>"mkFinishChanged.v"]$ \p->check(p`elem`map fst bound)("receipt does not bind "++p)
 -- The finished RTL declares the reclaimed group in OLD's position.
 let groups text=(findIndex("// register alpha"`isInfixOf`)(lines text),findIndex("// register beta"`isInfixOf`)(lines text))
 before<-groups<$>readFile(finish</>"input"</>"mkFinishChanged.v");after<-groups<$>readFile(finish</>"mkFinishChanged.v")
 check(fst before>snd before&&fst after<snd after&&fst after/=Nothing)"finishing must move the reclaimed register group before beta"
 putStrLn("PASS: vacancy-only flow, changed public finishing stage (raw "++show(foBeforeRaw result)++" -> "++show(foAfterRaw result)++"), fresh proof and replay with bound receipt")
