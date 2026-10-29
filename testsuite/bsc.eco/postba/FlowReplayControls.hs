module Main where
import ECOAST
import ECOASTFlow
import ECOASTReplay
import ECOASTSearch
import ECOASTCost(CostObservation(..),CostEnvelope(..))
import ECO.Replay(FileIdentity(..),fileIdentity)
import qualified Data.ByteString as B
import Control.Exception(bracket,try,SomeException)
import Control.Monad(forM_,unless)
import Data.List(isInfixOf)
import System.Environment
import System.Directory
import System.FilePath
import System.Process(readProcessWithExitCode)
import System.Exit(ExitCode(..))
bytes=B.pack.map(fromIntegral.fromEnum)
record p=fmap(read.map(toEnum.fromIntegral).B.unpack)(B.readFile p)
changed path content action=bracket(B.readFile path)(B.writeFile path)(\_->B.writeFile path content>>action)
main=do
 [bluehs,sdk,flow,out]<-getArgs
 createDirectory out
 -- Accessors must not expose Haskell record updates on the verified value.
 let forge=out</>"ForgeVerified.hs"
 writeFile forge "module Main where\nimport ECOASTReplay\nforge x=x{replayProgram=replayProgram x}\nmain=pure ()\n"
 (forgeCode,forgeOut,forgeErr)<-readProcessWithExitCode bluehs["-i"++sdk</>"lib",forge]""
 unless(forgeCode/=ExitSuccess&&"record field" `isInfixOf`(forgeOut++forgeErr)&&"replayProgram" `isInfixOf`(forgeOut++forgeErr))(fail"verified value permits record updates")
 let reportPath=flow</>"flow.report"
 report<-record reportPath::IO FlowReport
 let withReport r=changed reportPath(bytes(show r++"\n"))
     check r label=withReport r $ do
      verified<-replaySelectedFlow flow(out</>label)
      validateReplay verified
      unless(fiSHA256(replayRTL verified)==fiSHA256(frOutput r))(fail"full replay differs from selected output")
     reject label marker action=do
      result<-try action::IO(Either SomeException VerifiedFlow)
      case result of
       Left err|marker `isInfixOf`show err->pure()
       _->fail("full-flow tamper not rejected: "++label)
 -- The public CLI proves the full selected chain, not just its last branch.
 (code,stdout,stderr)<-readProcessWithExitCode bluehs["-i"++sdk</>"lib",sdk</>"scripts/replay.hs","--flow",flow,out</>"cli"]""
 writeFile(out</>"cli.log")(stdout++stderr)
 unless(code==ExitSuccess)(fail"full-flow public replay command failed")
 cli<-fileIdentity(out</>"cli"</>frModule report<.>"v")
 unless(fiSHA256 cli==fiSHA256(frOutput report))(fail"full-flow CLI output mismatch")
 -- Force both valid branch selections while retaining their recorded plan/output
 -- bindings; the kept path must freshly prove its ordinary-to-kept bridge.
 forM_["ordinary","kept"] $ \branch->case[row|row<-frBranches report,fbName row==branch]of
  [row]|fbError row==Nothing,Just output<-fbOutput row->check(report{frSelected=branch,frOutput=output})branch
  _->fail("fixture must have a qualified "++branch++" branch")
 ordinary<-readVerilogInfo(flow</>"adapted-new"</>frModule report<.>"eco-ast")
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
  withReport updated(reject"objective""objective OLD AST changed"(replaySelectedFlow flow(out</>"bad-objective")))
 -- A returned proof cannot authorize later-mutated outputs.
 verified<-replaySelectedFlow flow(out</>"revalidation")
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
 putStrLn"PASS: opaque result, full-flow CLI, ordinary/kept/rollback chains, rebound objective tamper and post-verification output/payload/objective bindings"
