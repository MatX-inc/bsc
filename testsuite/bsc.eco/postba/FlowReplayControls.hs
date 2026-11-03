module Main where
import ECO.VerilogInfo
import ECOASTFlow
import ECOASTReplay
import ECOASTSearch
import ECOASTCost(CostObservation(..),CostEnvelope(..))
import ECO.Replay(FileIdentity(..),fileIdentity)
import qualified Data.ByteString as B
import Control.Exception(bracket,try,SomeException)
import Control.Monad(forM_,unless)
import Data.List(isInfixOf,isPrefixOf)
import System.Environment
import System.Directory
import System.FilePath
import System.Process(readProcessWithExitCode,callProcess)
import System.Exit(ExitCode(..))
bytes=B.pack.map(fromIntegral.fromEnum)
record p=fmap(read.map(toEnum.fromIntegral).B.unpack)(B.readFile p)
changed path content action=bracket(B.readFile path)(B.writeFile path)(\_->B.writeFile path content>>action)
main=do
 [bluehs,sdk,rootsPath,flowArg,out]<-getArgs
 flow<-canonicalizePath flowArg
 createDirectory out
 roots<-rootsFromConfig rootsPath
 -- Accessors must not expose Haskell record updates on the verified value.
 let forge=out</>"ForgeVerified.hs"
 writeFile forge "module Main where\nimport ECOASTReplay\nforge x=x{replayProgram=replayProgram x}\nmain=pure ()\n"
 (forgeCode,forgeOut,forgeErr)<-readProcessWithExitCode bluehs["-i","-i"++sdk</>"lib",forge]""
 unless(forgeCode/=ExitSuccess&&"record field" `isInfixOf`(forgeOut++forgeErr)&&"replayProgram" `isInfixOf`(forgeOut++forgeErr))(fail"verified value permits record updates")
 let reportPath=flow</>"flow.report"
 report<-record reportPath::IO FlowReport
 let withReport r=changed reportPath(bytes(show r++"\n"))
     check r label=withReport r $ do
      verified<-replaySelectedFlow roots flow(out</>label)
      validateReplay verified
      unless(fiSHA256(replayRTL verified)==fiSHA256(frOutput r))(fail"full replay differs from selected output")
     reject label marker action=do
      result<-try action::IO(Either SomeException VerifiedFlow)
      case result of
       Left err|marker `isInfixOf`show err->pure()
       _->fail("full-flow tamper not rejected: "++label++": "++either show(const"accepted")result)
     cli label args=do
      (code,stdout,stderr)<-readProcessWithExitCode bluehs(["-i","-i"++sdk</>"lib",sdk</>"scripts/replay.hs","--flow"]++args)""
      writeFile(out</>label<.>"log")(stdout++stderr)
      pure(code,stdout++stderr)
 -- The public CLI proves the full selected chain, not just its last branch.
 (code,_)<-cli"cli"[rootsPath,flow,out</>"cli"]
 unless(code==ExitSuccess)(fail"full-flow public replay command failed")
 cliOutput<-fileIdentity(out</>"cli"</>frModule report<.>"v")
 unless(fiSHA256 cliOutput==fiSHA256(frOutput report))(fail"full-flow CLI output mismatch")
 -- Trust roots never come from inside the directory under verification.
 inside<-try(rootsFromConfig(flow</>"effective.config"))::IO(Either SomeException TrustRoots)
 case inside of
  Left err|"inside the flow directory" `isInfixOf`show err->pure()
  _->fail"roots file inside the flow directory was accepted"
 -- Force both valid branch selections while retaining their recorded plan/output
 -- bindings; the kept path must freshly prove its ordinary-to-kept bridge.
 forM_["ordinary","kept"] $ \branch->case[row|row<-frBranches report,fbName row==branch]of
  [row]|fbError row==Nothing,Just output<-fbOutput row->check(report{frSelected=branch,frOutput=output})branch
  _->fail("fixture must have a qualified "++branch++" branch")
 ordinary<-readVerilogInfo(flow</>"adapted-new"</>"capture"</>frModule report<.>"eco-ast")
 check(report{frSelected="ordinary-rollback",frOutput=aeOutput(viEnvelope ordinary)})"rollback"
 -- Rebind the changed objective into the report so the negative control reaches
 -- fresh objective validation rather than only its earlier file binding gate.
 let objectivePath=flow</>(frSelected report++"-search")</>"objective.plan"
 envelope<-record objectivePath::IO CostEnvelope
 objectiveID<-fileIdentity objectivePath
 let wrong=envelope{costOldASTHash=replicate 64 '0'}
 changed objectivePath(bytes(show wrong++"\n"))$do
  rebound<-fileIdentity objectivePath
  let updated=report{frInputs=[if ident==objectiveID then rebound else ident|ident<-frInputs report]}
  withReport updated(reject"objective""objective OLD AST changed"(replaySelectedFlow roots flow(out</>"bad-objective")))
 -- A returned proof cannot authorize later-mutated outputs.
 verified<-replaySelectedFlow roots flow(out</>"revalidation")
 let rtl=fiPath(replayRTL verified)
 original<-B.readFile rtl
 changed rtl(original<>bytes"// changed after verification\n")$do
  result<-try(validateReplay verified)::IO(Either SomeException())
  case result of
   Left err|"changed bound input" `isInfixOf`show err->pure()
   _->fail"verified output mutation was not rejected"
 -- Objective witnesses are also retained inputs, including the OLD baseline
 -- and accepted score text, rather than only the envelope that names them.
 forM_["objective/old.score.v",costWitness(last(costObservations envelope))]$ \relative->do
  let witness=takeDirectory objectivePath</>relative
  content<-B.readFile witness
  changed witness(content<>bytes"// changed score witness\n")$do
   result<-try(validateReplay verified)::IO(Either SomeException())
   case result of
    Left err|"changed bound input" `isInfixOf`show err->pure()
    _->fail"objective witness mutation escaped full-flow revalidation"
 -- In a kept flow, ordinary NEW remains part of the proof chain even though
 -- it is not the final rendering context. Its payload must stay authenticated.
 let payload=fiPath(aePayload(viEnvelope ordinary))
 savedPayload<-B.readFile payload
 changed payload(savedPayload<>B.singleton 0)$do
  result<-try(validateReplay verified)::IO(Either SomeException())
  case result of
   Left _->pure()
   _->fail"ordinary NEW payload mutation escaped kept-chain revalidation"
 -- The roots, not the directory under verification, name the solver. A copy of
 -- the flow whose unbound files are rewritten consistently still verifies, but
 -- one whose effective.config names a forged solver is refused before any proof
 -- runs; the forged solver is never invoked.
 let relocated label mutate=do
      let copy=out</>label
      callProcess"cp"["-a",flow,copy]
      cfg<-record(copy</>"effective.config")::IO CaptureFlow
      writeFile(copy</>"effective.config")(show(mutate cfg{cfOutput=copy})++"\n")
      configID<-fileIdentity(copy</>"effective.config")
      let move i|(flow++"/") `isPrefixOf` fiPath i=i{fiPath=copy++drop(length flow)(fiPath i)}
                |otherwise=i
          rebind i=if fiPath i==fiPath configID then configID else i
          moved=report{frConfig=rebind(move(frConfig report)),frInputs=map(rebind.move)(frInputs report),frOutput=move(frOutput report),
                       frBranches=[row{fbPlan=fmap move(fbPlan row),fbOutput=fmap move(fbOutput row)}|row<-frBranches report]}
      writeFile(copy</>"flow.report")(show moved++"\n")
      pure copy
 relocatedFlow<-relocated"relocated"id
 relocatedResult<-replaySelectedFlow roots relocatedFlow(out</>"relocated-replay")
 unless(fiSHA256(replayRTL relocatedResult)==fiSHA256(frOutput report))(fail"consistently relocated copy did not reproduce the selected output")
 let forged=out</>"forged-solver.sh";calls=out</>"forged-solver.calls"
 writeFile forged("#!/bin/sh\necho invoked >> "++calls++"\necho 'SAT proof finished - no model found: SUCCESS!'\nexit 0\n")
 permissions<-getPermissions forged
 setPermissions forged permissions{executable=True}
 forgedFlow<-relocated"forged"(\c->c{cfSearch=(cfSearch c){soYosys=forged}})
 reject"forged solver""configured solver differs from the trust root"(replaySelectedFlow roots forgedFlow(out</>"forged-replay"))
 (forgedCode,forgedLog)<-cli"forged-cli"[rootsPath,forgedFlow,out</>"forged-cli"]
 unless(forgedCode/=ExitSuccess&&"configured solver differs from the trust root" `isInfixOf` forgedLog)(fail"CLI accepted a flow whose configuration names a forged solver")
 invoked<-doesFileExist calls
 unless(not invoked)(fail"forged solver named by the flow directory was invoked")
 putStrLn"PASS: opaque result, full-flow CLI, roots outside the flow, ordinary/kept/rollback chains, rebound objective tamper, post-verification output/payload/objective bindings, relocated copy and forged-solver refusal"
