module Main where
import ECOASTFlow
import ECOASTFinishOrder
import ECOASTSearch
import ECOASTProof
import ECO.Replay(FileIdentity(..),fileIdentity)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Control.Exception(bracket)
import Control.Monad(unless)
import Data.List(isInfixOf)
import System.Environment
import System.Directory
import System.FilePath
import System.Process(readProcessWithExitCode)
import System.Exit(ExitCode(..))
bytes=B.pack.map(fromIntegral.fromEnum)
record p=fmap (read . map(toEnum.fromIntegral) . B.unpack)(B.readFile p)
main=do
 [bluehs,sdk,roots,flow,out]<-getArgs
 createDirectory out
 let reportPath=flow</>"flow.report"
 report<-record reportPath::IO FlowReport
 config<-record(flow</>"effective.config")::IO CaptureFlow
 finished<-record(takeDirectory flow</>"finished/finish/finish-order.report")::IO FinishOrderReport
 unless(foStatus finished=="unchanged"&&fiSHA256(foInputAST finished)==fiSHA256(foOutputAST finished))
  (fail"guide fixture must exercise byte-identical finishing no-op")
 let run label marker=do
      (code,stdout,stderr)<-readProcessWithExitCode bluehs["-i","-i"++sdk</>"lib",sdk</>"scripts/finish-order.hs",roots,flow,out</>label]""
      writeFile(out</>label<.>"log")(stdout++stderr)
      unless(code/=ExitSuccess&&marker `isInfixOf`(stdout++stderr))(fail("tamper not rejected: "++label))
     changed path content action=bracket(B.readFile path)(B.writeFile path)(\_->B.writeFile path content>>action)
     badDomain=let opts=frSearchPolicy report;domain=if soDomain opts==Binary then StrictFourState else Binary
               in report{frSearchPolicy=opts{soDomain=domain}}
     corruptOutput=report{frOutput=(frOutput report){fiSHA256=replicate 64 '0'}}
 changed reportPath(bytes(show badDomain++"\n"))(run"domain""flow schema/module/proof options mismatch")
 changed reportPath(bytes(show corruptOutput++"\n"))(run"selected-output""changed bound input")
 let branch=frSelected report
 unless(branch=="ordinary-rollback")$do
  let corruptPlan=report{frBranches=[if fbName row==branch then row{fbPlan=fmap(\i->i{fiSHA256=replicate 64 '0'})(fbPlan row)}else row|row<-frBranches report]}
  changed reportPath(bytes(show corruptPlan++"\n"))(run"selected-plan""selected plan differs from bound flow branch")
  let planPath=flow</>(branch++"-search")</>"search.plan"
  plan<-record planPath::IO ASTPlan
  case planSteps plan of
   []->fail"CLI fixture must contain an accepted step"
   step:_->do
    let ast=takeDirectory planPath</>stepArtifact step
    content<-B.readFile ast
    changed ast(content<>B.singleton 0)(run"candidate-bytes""candidate artifact changed")
 -- The adapter receipt names the original capture by identity; rewrite that
 -- digest in place so the receipt no longer links the adapted snapshot to the
 -- configured OLD capture.
 let adapterPath=flow</>"adapted-old/adapter-receipt"
 source<-fileIdentity(cfOldCapture config)
 receipt<-B.readFile adapterPath
 let (before,after)=B.breakSubstring(BC.pack(fiSHA256 source))receipt
 unless(not(B.null after))(fail"adapter receipt does not name the configured OLD capture")
 changed adapterPath(before<>BC.pack(replicate 64 '0')<>B.drop 64 after)(run"wrong-capture""native adapter source/capture/producer linkage differs")
 putStrLn"PASS: byte-exact no-op and 5 public finish-order domain/output/plan/candidate/capture tamper controls"
