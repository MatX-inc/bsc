module Main where
import Control.Monad(unless,forM_)
import Control.Exception(IOException,try)
import Data.List(isInfixOf)
import System.Directory(getCurrentDirectory)
import System.Environment(getArgs)
import System.FilePath
import ECOAST
import ECOASTCost
import ECOASTFlow
import ECOASTSearch
import ECOASTProof
import ECOASTMetric(MetricInput(..))
import ECOASTTranslatedMetric
import ECOASTTranslatedObjective
import ECO.Replay(FileIdentity(..))
check s b=unless b(fail s)>>putStrLn("PASS: "++s)
main=do
 [compiler,old,new,filtered,library,manifest,output]<-getArgs
 working<-getCurrentDirectory
 let options=defaultSearchOptions{soDomain=StrictFourState,soFeatures=["F01","F02","F06"]}
     configuration dir=CaptureFlow compiler "mkObjectiveFlow" old new working working
       (Just library)(Just library)Nothing dir options 120 manifest Nothing
 writeFile(output++"-cli.config")(show(configuration(output++"-cli"))++"\n")
 forM_ [TranslatedRaw,TranslatedNormalized] $ \mode->do
  let dir=output++"-"++show mode;config=configuration dir
  writeFile(dir++".config")(show config++"\n")
  result<-runCaptureFlowWithCost (translatedCostProvider mode [manifest]) defaultCandidateProvider DeferInvariants config
  let context=dir</>(if frSelected result=="kept" then "kept" else "adapted-new")</>"mkObjectiveFlow.eco-ast"
      ast=dir</>(if frSelected result=="ordinary-rollback" then "adapted-new/capture/mkObjectiveFlow.eco-ast.bin" else frSelected result++"-replay/selected.ast")
  check("translated flow chooses an improved branch "++show mode)(frSelected result/="ordinary-rollback")
  metric<-measureTranslatedAST(dir++"-metric")
    (Snapshot(dir</>"adapted-old/mkObjectiveFlow.eco-ast"))
    (ContextAST context ast(fiPath(frOutput result)))
  let measured=case mode of TranslatedRaw->translatedRaw metric;TranslatedNormalized->translatedNormalized metric
  check("branch and final emission use the same translated metric "++show mode)(frSelectedCost result==measured)
  check("translated flow reports explicit schema and policy "++show mode)(frSchema result==4&&frSelectedCost result/=frOrdinaryCost result)
 -- This deliberately simple callback makes the authenticated kept branch the
 -- preferred one even if ordinary whole-file costs would suggest another tie.
 let branchObjective _ context=CostProvider "test-kept-branch-v1" [manifest]
       (const(Right"baseline\n"))
       (const(Right(if takeFileName(takeDirectory(fiPath(aeOutput(viEnvelope context))))=="kept"
         then "baseline\n" else "ordinary\n")))
     config=configuration(output++"-branches")
 result<-runCaptureFlowWithCost branchObjective defaultCandidateProvider DeferInvariants config
 check"cross-branch choice uses supplied objective"(frSelected result=="kept"&&changedLines(frSelectedCost result)==0&&changedLines(frOrdinaryCost result)==2)
 let inconsistent old context=
       let p=branchObjective old context
           kept=takeFileName(takeDirectory(fiPath(aeOutput(viEnvelope context))))=="kept"
       in if kept then p{costOldText=const(Right"different-old\n")} else p
 rejected<-runCaptureFlowWithCost inconsistent defaultCandidateProvider DeferInvariants
   (configuration(output++"-inconsistent-baseline"))
 check"branches with a different OLD scoring baseline are rejected"
   (frSelected rejected/="kept"&&"branch objective changed the OLD scoring baseline" `isInfixOf` show rejected)
 -- A reproducible external filter can alter ordinary bytes while leaving the
 -- retained native AST untouched. It cannot authorize a translated rollback.
 let filteredConfig=(configuration(output++"-filtered")){cfNewCapture=filtered}
 attempt<-try(runCaptureFlowWithCost (translatedCostProvider TranslatedNormalized [manifest])
   defaultCandidateProvider DeferInvariants filteredConfig)::IO(Either IOException FlowReport)
 check"nonidentity ordinary filter is rejected before objective fallback"
   (case attempt of Left e->"filtered ordinary bytes differ" `isInfixOf` show e;_->False)
 putStrLn"PASS: objective flow controls complete"
