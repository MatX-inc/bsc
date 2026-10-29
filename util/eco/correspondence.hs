-- Explicit optional refinement flow; no direct-reference input.
module Main where
import ECOASTFlow
import ECOASTSearch
import ECOASTCorrespondence
import ECOASTValueNames
import ECOASTVacancy(vacancyProvider)
import ECOASTStateView(stateViewProvider)
import ECOASTShell(shellProvider)
import ECOASTInactive(inactiveProvider)
import System.Environment
import System.FilePath
import Text.Read
main=do
 args<-getArgs
 case args of
  [source,config]->do
   c<-readFile config >>= maybe(fail "invalid native CaptureFlowConfig")pure.readMaybe
   let provider=combineCandidateProviders[correspondenceProvider{cpSources=[source]},valueNamesProvider{cpSources=[takeDirectory source</>"ECOASTValueNames.hs"]},vacancyProvider,stateViewProvider,shellProvider,inactiveProvider,defaultCandidateProvider]
       s=cfSearch c
       fs=soFeatures s
       extra=["F24WidthRole","F24ExactLeaf","ValueNames"]++[f|"F04" `elem` fs,f<-["F24Layout","ValueNamesLayout"]]
       enabled=fs++[f|f<-extra,f `notElem` fs]
       configured=c{cfSearch=s{soFeatures=enabled}}
   result<-runCaptureFlowWithProvider provider DeferInvariants configured
   writeFile(cfOutput configured</>"effective.config")(show configured++"\n")
   print result
  _->fail "usage: correspondence.hs RULE_SOURCE.hs CONFIG (RULE_SOURCE is the hash-bound ECOASTCorrespondence module)"
