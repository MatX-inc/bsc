module Main where
import ECOASTFlow
import ECOASTFinishOrder
import ECOASTSearch
import ECOASTProof
import ECO.Replay(FileIdentity(..))
import qualified Data.ByteString as B
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
 [bluehs,sdk,flow,out]<-getArgs
 createDirectory out
 let reportPath=flow</>"flow.report"
 report<-record reportPath::IO FlowReport
 finished<-record(takeDirectory flow</>"finished/finish/finish-order.report")::IO FinishOrderReport
 unless(foStatus finished=="unchanged"&&fiSHA256(foInputAST finished)==fiSHA256(foOutputAST finished))
  (fail"guide fixture must exercise byte-identical finishing no-op")
 let run label marker=do
      (code,stdout,stderr)<-readProcessWithExitCode bluehs["-i"++sdk</>"lib",sdk</>"scripts/finish-order.hs",sdk</>"build.manifest",flow,out</>label]""
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
 let adapterPath=flow</>"adapted-old/adapter-receipt"
 (schema,source,op,np,adapted,oo,no,relocation,note)<-record adapterPath::IO(String,FileIdentity,FileIdentity,FileIdentity,FileIdentity,[FileIdentity],[FileIdentity],Maybe FileIdentity,String)
 let wrong=(schema,source{fiSHA256=replicate 64 '0'},op,np,adapted,oo,no,relocation,note)
 changed adapterPath(bytes(show wrong++"\n"))(run"wrong-capture""native adapter source/capture/producer linkage differs")
 putStrLn"PASS: byte-exact no-op and 5 public finish-order domain/output/plan/candidate/capture tamper controls"
