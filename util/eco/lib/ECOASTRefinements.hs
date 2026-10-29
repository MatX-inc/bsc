-- Recommended non-retention rules, implemented entirely in the external SDK.
module ECOASTRefinements(standardRefinementProvider,enableStandardRefinements,balancedCandidateProviders) where
import ECOASTSearch
import ECOASTCorrespondence(correspondenceProvider)
import ECOASTValueNames(valueNamesProvider)
import ECOASTPipelineChain(pipelineChainProvider)
import ECOASTHelperReuse(helperReuseProvider)
import ECOASTLiveAliases(liveAliasProvider)
import ECOASTLiveShell(liveShellProvider)
import ECOASTAliasOrientation(aliasOrientationProvider)
import Data.List(transpose)
import ECOASTVacancy(vacancyProvider)
import ECOASTStateView(stateViewProvider)

-- SDK modules are bound by the packaged manifest; cpSources name only
-- caller-supplied rule inputs.
standardRefinementProvider :: CandidateProvider
standardRefinementProvider=balancedCandidateProviders
  [correspondenceProvider,valueNamesProvider,pipelineChainProvider,helperReuseProvider,
   liveAliasProvider [],aliasOrientationProvider,liveShellProvider,vacancyProvider,
   stateViewProvider,defaultCandidateProvider]

enableStandardRefinements :: SearchOptions -> SearchOptions
enableStandardRefinements options=options{soFeatures=prior++[f|f<-extras,f `notElem` prior]}
 where
 -- Historical unused-helper shells are a separate explicit CLI policy.
 prior=filter (`notElem` ["F22","F22Inactive"]) (soFeatures options)
 extras=["F21","F23","F24WidthRole","F24ExactLeaf","ValueNames","F25","F26LiveHelpers","LiveAliases","LiveShell","AliasOrientation"]++
   [f|"F04" `elem` prior,f<-["F24Layout","ValueNamesLayout"]]

-- Give each enabled family an opportunity before taking its second proposal.
-- This affects only the standard preset; generic combineCandidateProviders
-- retains its explicit caller-priority behavior. The total budget still bounds
-- emitted proposals, and an undersized budget cannot cover every family.
balancedCandidateProviders :: [CandidateProvider] -> CandidateProvider
balancedCandidateProviders providers=combined{cpGenerate=generate}
 where
 combined=combineCandidateProviders providers
 generate features budget old current=take (max 0 budget) $ concat $ transpose
  [cpGenerate provider enabled budget old current | provider<-providers,
   let enabled=filter (`elem`cpFeatures provider) features,not(null enabled)]
