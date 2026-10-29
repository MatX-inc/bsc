-- Optional selection from authenticated accepted plans; source searches are unchanged.
module Main where
import ECOASTSelection
import ECOASTReplay (TrustRoots(..), rootsFromConfig)
import ECOASTTranslatedMetric
import System.Environment (getArgs)
import System.Exit (die)
main = do
  allArgs <- getArgs
  let (translated,args)=case allArgs of "--translated":rest->(True,rest);_->(False,allArgs)
  case args of
    rootsPath:output:flows@(_:_) -> do
      roots <- rootsFromConfig rootsPath
      if translated then selectPrefixesWithMetric "synthesis-translated" [trSdkManifest roots]
        (\dir a b -> do r<-measureTranslatedAST dir a b;return(translatedRaw r,translatedNormalized r))
        roots output flows
      else selectPrefixes roots output flows
    _ -> die "usage: select.hs [--translated] ROOTS.config FRESH_OUTPUT FLOW..."
