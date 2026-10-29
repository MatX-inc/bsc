module Main where
import ECOASTReplay
import ECOASTSearch
import ECOASTProof(ProofDomain(..))
import ECOBind(readRecord)
import ECOJSON(EvidenceJSON(..),parseEvidenceJSON)
import ECO.Replay(FileIdentity(..),fileIdentity)
import Control.Exception(try,SomeException)
import Control.Monad(forM,forM_,unless,when)
import Data.List(isInfixOf,isPrefixOf,isSuffixOf,intersect)
import Data.Maybe(mapMaybe)
import System.Directory
import System.Environment
import System.FilePath
import System.Process(readProcessWithExitCode,callProcess)
import System.Exit(ExitCode(..))
field k(JObject kvs)=lookup k kvs
field _ _=Nothing
str(Just(JString s))=Just s
str _=Nothing
arr(Just(JArray xs))=xs
arr _=[]
receipt p=readFile p>>=either(fail . ("unreadable receipt "++))pure . parseEvidenceJSON
receiptsIn dir=do
 names<-listDirectory dir
 mapM(receipt . (dir</>))[n|n<-names,"candidate-" `isPrefixOf` n,".proof.json" `isSuffixOf` n]
subjectKey r=(str(field"reference"=<<field"subject"r),str(field"candidate"=<<field"subject"r))
need b s=unless b(fail s)
main=do
 [bluehs,sdk,rootsPath,flowArg,twinRootsPath,twinArg,fakeArg,fakeSolver,calls,out]<-getArgs
 flow<-canonicalizePath flowArg;twin<-canonicalizePath twinArg;fake<-canonicalizePath fakeArg
 createDirectory out
 roots<-rootsFromConfig rootsPath
 need(trDomain roots==Binary)"fixture roots must select the Binary domain"
 solverID<-fileIdentity(trSolver roots)
 let cli label args=do
      (code,stdout,stderr)<-readProcessWithExitCode bluehs(["-i","-i"++sdk</>"lib",sdk</>"scripts/replay.hs","--flow"]++args)""
      writeFile(out</>label<.>"log")(stdout++stderr)
      pure(code,stdout++stderr)
     reject label marker action=do
      result<-try action::IO(Either SomeException VerifiedFlow)
      case result of
       Left err|marker `isInfixOf`show err->pure()
       _->fail("not refused: "++label++": "++either show(const"accepted")result)
     bindsSolver r=str(field"solver"=<<field"subject"r)==Just(fiPath solverID)&&
                   str(field"solver-sha256"=<<field"subject"r)==Just(fiSHA256 solverID)
 -- The SAT-decided flow: every fresh proof passed, the accepted step's proof
 -- has a residual root the solver decided, and its artifacts are in place.
 report<-readRecord(flow</>"flow.report")::IO FlowReport
 let branch=frSelected report
 need(branch `elem`["ordinary","kept"])"fixture flow rolled back to ordinary NEW"
 plan<-readRecord(flow</>(branch++"-search")</>"search.plan")::IO ASTPlan
 need(not(null(planSteps plan)))"fixture flow accepted no step"
 verified<-receipt(flow</>"verified"</>"upstream-qualification.json")
 need(str(field"status"verified)==Just"freshly-replayed"&&str(field"proofDomain"verified)==Just"Binary")"verified receipt is not a fresh Binary replay"
 need(str(field"path"=<<field"solver"verified)==Just(fiPath solverID)&&str(field"sha256"=<<field"solver"verified)==Just(fiSHA256 solverID))"verified receipt does not bind the root solver"
 proofs<-forM[1..length(planSteps plan)]$ \i->do
  let dir=flow</>"verified"</>"replay"</>("proof-"++show i)
  r<-receipt(dir<.>"json")
  need(str(field"status"r)==Just"passed")("fresh proof "++show i++" did not pass")
  pure(dir,r)
 let residual=[(dir,r)|(dir,r)<-proofs,not(null(arr(field"residualRoots"r)))]
 need(not(null residual))"no fresh proof needed the solver"
 forM_ residual$ \(dir,r)->do
  forM_["prove.ys","proof.log","miter.v","reference.ast","candidate.ast"]$ \name->do
   present<-doesFileExist(dir</>name)
   need present(dir</>name++" is missing")
  logText<-readFile(dir</>"proof.log")
  need("SAT proof finished - no model found: SUCCESS!" `isInfixOf` logText)"proof log records no solver success"
  need(bindsSolver r)"residual proof receipt does not name the root solver"
  logID<-fileIdentity(dir</>"proof.log")
  need(str(field"proof-log"=<<field"subject"r)==Just(fiSHA256 logID))"proof log identity differs from the receipt"
  script<-readFile(dir</>"prove.ys")
  need(("sat -timeout "++show(trSeconds roots)) `isInfixOf` script&&"-prove mismatch 0" `isInfixOf` script)"prove.ys does not carry the root timeout"
 searchReceipts<-receiptsIn(flow</>(branch++"-search"))
 let accepted=stepArtifactHash(last(planSteps plan))
 case[r|r<-searchReceipts,snd(subjectKey r)==Just accepted]of
  [r]->need(str(field"status"r)==Just"passed"&&not(null(arr(field"residualRoots"r)))&&bindsSolver r)"the selected step's receipt is not a SAT-decided pass"
  _->fail"the selected step has no unique search receipt"
 -- The behaviour-changing twin: every solver-decided candidate is rejected
 -- with a model, so only structurally proved steps survive into its plan.
 twinReport<-readRecord(twin</>"flow.report")::IO FlowReport
 twinReceipts<-concat<$>mapM(\b->do present<-doesDirectoryExist(twin</>(b++"-search"));if present then receiptsIn(twin</>(b++"-search"))else pure[])["ordinary","kept"]
 let rejected=[r|r<-twinReceipts,str(field"status"r)==Just"rejected"]
     passed=[r|r<-twinReceipts,str(field"status"r)==Just"passed"]
 need(not(null rejected))"twin flow produced no solver-rejected candidate"
 forM_ rejected$ \r->do
  let issues=arr(field"issues"r)
  need(mapMaybe(str . field"code")issues==["inequivalent"])"rejected twin candidate is not an inequivalence verdict"
  need(not(null(arr(field"residualRoots"r)))&&bindsSolver r)"rejected twin candidate was not decided by the root solver"
  logPath<-maybe(fail"rejected receipt names no solver log")pure(str(field"detail"(head issues)))
  logText<-readFile logPath
  need("SAT proof finished - model found: FAIL!" `isInfixOf` logText)"rejected candidate log records no model"
 need(null(map subjectKey rejected `intersect` map subjectKey passed))"a rejected twin candidate also passed"
 need(null[r|r<-twinReceipts,not(null(arr(field"residualRoots"r))),str(field"status"r)/=Just"rejected"])"a solver-decided twin candidate was not rejected"
 when(frSelected twinReport `elem`["ordinary","kept"])$do
  twinPlan<-readRecord(twin</>(frSelected twinReport++"-search")</>"search.plan")::IO ASTPlan
  need(not(any(("Boolean form" `isInfixOf`) . stepDescription)(planSteps twinPlan)))"a historical Boolean form survived into the twin's plan"
  forM_(zip[1::Int ..](planSteps twinPlan))$ \(i,_)->do
   r<-receipt(twin</>"verified"</>"replay"</>("proof-"++show i)<.>"json")
   need(str(field"status"r)==Just"passed"&&null(arr(field"residualRoots"r)))("twin step "++show i++" was not proved structurally")
 -- The forged solver was invoked by the flow configured with it (so the
 -- marker works) and it granted passes; the real roots refuse that flow at
 -- the solver binding, before any proof, so the marker never reappears.
 invoked<-doesFileExist calls
 need invoked"the forged solver named by fake.config was never invoked"
 fakeReport<-readRecord(fake</>"flow.report")::IO FlowReport
 need(soYosys(frSearchPolicy fakeReport)==fakeSolver)"fake flow report does not name the forged solver"
 fakeReceipts<-concat<$>mapM(\b->do present<-doesDirectoryExist(fake</>(b++"-search"));if present then receiptsIn(fake</>(b++"-search"))else pure[])["ordinary","kept"]
 need(not(null[r|r<-fakeReceipts,str(field"status"r)==Just"passed",str(field"solver"=<<field"subject"r)==Just fakeSolver]))"forged solver flow recorded no forged pass"
 removeFile calls
 twinRoots<-rootsFromConfig twinRootsPath
 reject"forged-solver flow""configured solver differs from the trust root"(replaySelectedFlow twinRoots fake(out</>"fake-library"))
 (fakeCode,fakeLog)<-cli"fake-cli"[twinRootsPath,fake,out</>"fake-cli"]
 need(fakeCode/=ExitSuccess&&"configured solver differs from the trust root" `isInfixOf` fakeLog)"CLI accepted the flow run with the forged solver"
 -- A consistently relocated copy of the SAT-decided flow still verifies; the
 -- same copy whose effective.config names the forged solver is refused.
 let relocated label mutate=do
      let copy=out</>label
      callProcess"cp"["-a",flow,copy]
      cfg<-readRecord(copy</>"effective.config")::IO CaptureFlow
      writeFile(copy</>"effective.config")(show(mutate cfg{cfOutput=copy})++"\n")
      configID<-fileIdentity(copy</>"effective.config")
      let move i|(flow++"/") `isPrefixOf` fiPath i=i{fiPath=copy++drop(length flow)(fiPath i)}
                |otherwise=i
          rebind i=if fiPath i==fiPath configID then configID else i
          moved=report{frConfig=rebind(move(frConfig report)),frInputs=map(rebind . move)(frInputs report),frOutput=move(frOutput report),
                       frBranches=[row{fbPlan=fmap move(fbPlan row),fbOutput=fmap move(fbOutput row)}|row<-frBranches report]}
      writeFile(copy</>"flow.report")(show moved++"\n")
      pure copy
 relocatedFlow<-relocated"relocated"id
 relocatedResult<-replaySelectedFlow roots relocatedFlow(out</>"relocated-replay")
 need(fiSHA256(replayRTL relocatedResult)==fiSHA256(frOutput report))"consistently relocated copy did not reproduce the selected output"
 forgedFlow<-relocated"forged"(\c->c{cfSearch=(cfSearch c){soYosys=fakeSolver}})
 reject"forged-solver copy""configured solver differs from the trust root"(replaySelectedFlow roots forgedFlow(out</>"forged-replay"))
 (forgedCode,forgedLog)<-cli"forged-cli"[rootsPath,forgedFlow,out</>"forged-cli"]
 need(forgedCode/=ExitSuccess&&"configured solver differs from the trust root" `isInfixOf` forgedLog)"CLI accepted a copy whose configuration names a forged solver"
 invokedAgain<-doesFileExist calls
 need(not invokedAgain)"a verifier with real roots invoked the forged solver"
 putStrLn("PASS: "++show(length residual)++" SAT-decided fresh proofs with prove.ys/proof.log, selected step decided by the root solver, "++
  show(length rejected)++" twin candidates rejected with a model and only structural twin steps kept, forged-solver flow and forged copy refused at the solver binding without invoking the solver")
