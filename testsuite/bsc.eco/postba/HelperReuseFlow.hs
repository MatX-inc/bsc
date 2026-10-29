module Main where
import Control.Monad
import qualified Data.ByteString as B
import ECOAST
import ECOASTFlow
import ECOASTSearch
import ECOASTProof
import ECOASTProofContext
import ECOASTRefinements
import ECOASTTranslatedObjective
import ECOASTCost
import System.Environment
import System.Directory
import System.FilePath
main=do
 [compiler,oldCapture,newCapture,library,manifest,root,ys]<-getArgs
 working<-getCurrentDirectory
 let base=defaultSearchOptions{soDomain=StrictFourState,soFeatures=["F01","F04"],soYosys=ys}
     options=enableStandardRefinements base
     config=CaptureFlow compiler "mkHelperPipeline" oldCapture newCapture working working
              (Just library)(Just library)Nothing root options 120 manifest Nothing
 unless(all (`elem`soFeatures options)["F24WidthRole","F24ExactLeaf","F24Layout","ValueNames","ValueNamesLayout","F25","F26LiveHelpers"])(fail"missing standard provider")
 unless(all (`notElem`soFeatures(enableStandardRefinements base{soFeatures=["F22","F22Inactive"]}))["F22","F22Inactive"])(fail"retention policy inherited implicitly")
 unless(all (`notElem`soFeatures(enableStandardRefinements base{soFeatures=[]}))["F24Layout","ValueNamesLayout"])(fail"layout enabled without F04")
 writeFile(root++"-cli.config")(show config{cfOutput=root++"-cli",cfSearch=base}++"\n")
 result<-runCaptureFlowWithCost (translatedCostProvider TranslatedNormalized [manifest]) standardRefinementProvider DeferInvariants config
 forM_ ["ordinary","kept"] $ \branch->do
  let contextPath=root</>(if branch=="ordinary" then "adapted-new" else "kept")</>"mkHelperPipeline.eco-ast"
      search=root</>branch++"-search/search.plan"
  exists<-doesFileExist search
  when exists $ do
   old<-readVerilogInfo(root</>"adapted-old/mkHelperPipeline.eco-ast")
   context<-readVerilogInfo contextPath
   let provider=translatedCostProvider TranslatedNormalized [manifest] old context
   let pins=childPinsFromInstances(viInstances context)
       gate opts directory alpha contracts=runASTProofWithContracts pins pins contracts (soYosys opts)(soProofSeconds opts)directory(soDomain opts)alpha
   _<-replayASTPlanWithCost provider(viProgram old)gate options(renderVerilogText context)search(root</>branch++"-fresh-replay")(viProgram context)
   a<-B.readFile(root</>branch++"-replay/selected.ast")
   b<-B.readFile(root</>branch++"-fresh-replay/selected.ast")
   unless(a==b)(fail"fresh objective-aware replay differs")
 print result
 putStrLn"PASS standard provider registration, explicit non-retention, fresh captures, full proof and objective-aware replay"
