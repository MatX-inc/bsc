module Main where
import ECO.VerilogInfo(VerilogInfo(..),readVerilogInfo)
import ECOASTReplay
import ECOASTSearch
import ECOASTProof
import ECOASTProofContext(childPinsFromInstances)
import ECOASTLexical(lexicallyEmittable,declaredBeforeUse)
import ECOBind(readRecord)
import ECOJSON(EvidenceJSON(..),parseEvidenceJSON)
import ECO.Replay(FileIdentity(..),fileIdentity)
import Verilog
import BinData(encode,decode)
import GenABin()
import qualified Data.ByteString as B
import qualified Data.Generics as G
import Control.Exception(try,SomeException)
import Control.Monad(forM_,unless)
import Data.Either(isLeft,isRight)
import Data.List(isInfixOf,isPrefixOf)
import Data.Maybe(mapMaybe)
import System.Directory
import System.Environment
import System.FilePath
import System.Process(readProcessWithExitCode,callProcess)
import System.Exit(ExitCode(..))
block(VMDecl(VVDecl t _ vs))=t `elem`[VDWire,VDReg]&&not(null vs)&&all scalar vs where scalar VVar{}=True;scalar _=False
block(VMComment _ x)=block x
block(VMRegGroup _ _ _ x)=block x
block(VMGroup False xss)=not(null(concat xss))&&all block(concat xss)
block _=False
flatten=concatMap go where
 go(VMComment _ x)=go x
 go(VMRegGroup _ _ _ x)=go x
 go(VMGroup _ xss)=flatten(concat xss)
 go x=[x]
field k(JObject kvs)=lookup k kvs
field _ _=Nothing
str(Just(JString s))=Just s
str _=Nothing
arr(Just(JArray xs))=xs
arr _=[]
need b s=unless b(fail s)
main=do
 [bluehs,sdk,rootsPath,flowArg,out]<-getArgs
 flow<-canonicalizePath flowArg
 createDirectory out
 roots<-rootsFromConfig rootsPath
 report<-readRecord(flow</>"flow.report")::IO FlowReport
 let branch=frSelected report;modul=frModule report;bundle=flow</>(branch++"-search")
 need(branch `elem`["ordinary","kept"])"fixture flow rolled back to ordinary NEW"
 plan<-readRecord(bundle</>"search.plan")::IO ASTPlan
 let steps=planSteps plan;index=length steps
 need(index>0)"fixture plan has no accepted step"
 let step=last steps
 context<-readVerilogInfo(if branch=="kept"then flow</>"kept"</>modul<.>"eco-ast"else flow</>"adapted-new"</>"capture"</>modul<.>"eco-ast")
 reference<-if index==1 then pure(viProgram context)else decode<$>B.readFile(bundle</>stepArtifact(steps!!(index-2)))
 accepted<-decode<$>B.readFile(bundle</>stepArtifact step)
 need(isRight(lexicallyEmittable accepted>>declaredBeforeUse accepted))"the accepted step is not emittable"
 let VProgram[m]dpis comments=accepted
     ports=[getVIdString(vargName a)|a<-concatMap fst(vm_ports m)]
     output=head[getVIdString i|VAOutput i _<-concatMap fst(vm_ports m)]
     payload="\nassign "++output++" = 1'b0;"
     victim=head[getVIdString(vvName v)|VMDecl(VVDecl _ _ vs)<-flatten(vm_body m),v<-vs,getVIdString(vvName v) `notElem` ports]
     injected=victim++"; assign "++output++" = 1'b0; //"
     rename(VId s i info)=if s==victim then VId injected i info else VId s i info
     withBody body=VProgram[m{vm_body=body}]dpis comments
     (run,rest)=span block(vm_body m)
     -- Three plan-step artifacts a saved plan could carry: a comment element
     -- that escapes its line, an identifier that ends its statement, and a
     -- declaration block moved after the first use of its names.
     commented=withBody(case vm_body m of x:xs->VMComment["ECO note",payload]x:xs;[]->[])
     renamed=withBody(G.everywhere(G.mkT rename)(vm_body m))
     late=withBody(init run++rest++[last run])
     pins=childPinsFromInstances(viInstances context)
     tampers=[("comment",commented,"lexical"),("identifier",renamed,"lexical"),("declaration-order",late,"declaration-order")]
 need(not(null run))"the accepted step has no leading declaration block"
 let relocated label=do
      let copy=out</>label
      callProcess"cp"["-a",flow,copy]
      cfg<-readRecord(copy</>"effective.config")::IO CaptureFlow
      writeFile(copy</>"effective.config")(show cfg{cfOutput=copy}++"\n")
      configID<-fileIdentity(copy</>"effective.config")
      let move i|(flow++"/") `isPrefixOf` fiPath i=i{fiPath=copy++drop(length flow)(fiPath i)}
                |otherwise=i
          rebind i=if fiPath i==fiPath configID then configID else i
          moved=report{frConfig=rebind(move(frConfig report)),frInputs=map(rebind . move)(frInputs report),frOutput=move(frOutput report),
                       frBranches=[row{fbPlan=fmap move(fbPlan row),fbOutput=fmap move(fbOutput row)}|row<-frBranches report]}
      writeFile(copy</>"flow.report")(show moved++"\n")
      pure copy
     -- Replace the last accepted artifact and rebind the whole chain the way
     -- an untrusted plan could: step hash, plan output, branch plan identity.
     tampered label program=do
      copy<-relocated label
      let artifact=copy</>(branch++"-search")</>stepArtifact step
      B.writeFile artifact(B.pack(encode program))
      h<-fiSHA256<$>fileIdentity artifact
      let planPath=copy</>(branch++"-search")</>"search.plan"
      writeFile planPath(show plan{planSteps=init steps++[step{stepArtifactHash=h,stepAfter=h}],planOutput=h}++"\n")
      planID<-fileIdentity planPath
      moved<-readRecord(copy</>"flow.report")::IO FlowReport
      writeFile(copy</>"flow.report")(show moved{frBranches=[if fbName row==branch then row{fbPlan=Just planID}else row|row<-frBranches moved]}++"\n")
      pure copy
     reject label marker action=do
      result<-try action::IO(Either SomeException VerifiedFlow)
      case result of
       Left err|marker `isInfixOf`show err->pure()
       _->fail("not refused: "++label++": "++either show(const"accepted")result)
     cli label args=do
      (code,stdout,stderr)<-readProcessWithExitCode bluehs(["-i","-i"++sdk</>"lib",sdk</>"scripts/replay.hs","--flow"]++args)""
      writeFile(out</>label<.>"log")(stdout++stderr)
      pure(code,stdout++stderr)
 -- A consistently relocated copy of the untouched flow verifies: refusals
 -- below come from the gates, not from the rebinding.
 control<-relocated"relocated"
 verified<-replaySelectedFlow roots control(out</>"relocated-replay")
 need(fiSHA256(replayRTL verified)==fiSHA256(frOutput report))"relocated copy did not reproduce the selected output"
 forM_ tampers$ \(label,program,code)->do
  offender<-either pure(const(fail(label++": tampered program is emittable")))(lexicallyEmittable program>>declaredBeforeUse program)
  need(code/="lexical"||isLeft(lexicallyEmittable program))(label++": lexical gate did not fire")
  need(code/="declaration-order"||isRight(lexicallyEmittable program)&&isLeft(declaredBeforeUse program))(label++": declaration gate did not fire")
  need(encode program/=encode accepted&&fixedPortsSame program accepted)(label++": tamper changed nothing or the ports")
  -- The proof gate refuses the artifact with the issue code before any proof.
  proof<-runASTProofWithPins pins pins(trSolver roots)(trSeconds roots)(out</>label++"-proof")(trDomain roots)(stepAlpha step)reference program
  writeFile(out</>label<.>"json")(renderProofResult proof)
  need(not(proofPassed proof)&&proofStatus proof=="unresolved"&&map issueCode(proofIssues proof)==[code]&&map issueDetail(proofIssues proof)==[offender])(label++": proof gate did not report "++code)
  rendered<-readFile(out</>label<.>"json")>>=either fail pure . parseEvidenceJSON
  need(mapMaybe(str . field"code")(arr(field"issues"rendered))==[code])(label++": receipt does not carry the issue code")
  -- The fresh full-flow verifier refuses the consistently rebound copy.
  copy<-tampered label program
  let marker="candidate is not emittable: "++offender
  reject label marker(replaySelectedFlow roots copy(out</>label++"-replay"))
  (exit,logText)<-cli(label++"-cli")[rootsPath,copy,out</>label++"-cli-replay"]
  need(exit/=ExitSuccess&&marker `isInfixOf` logText)(label++": CLI accepted the tampered plan")
 putStrLn"PASS: relocated control verified; comment-newline, identifier and declaration-order plan tampers refused by the proof gate (issue codes lexical, lexical, declaration-order) and by replay.hs --flow on a consistently rebound chain"
 where fixedPortsSame(VProgram a _ _)(VProgram b _ _)=[(vm_name x,vm_ports x)|x<-a]==[(vm_name x,vm_ports x)|x<-b]
