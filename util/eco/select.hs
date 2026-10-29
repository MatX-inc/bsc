{-# LANGUAGE CPP #-}
-- Optional selection from authenticated accepted plans; source searches are unchanged.
module Main where
import ECOASTSelection
import ECOASTTranslatedMetric
import ECOASTProof
import System.Environment (getArgs)
import System.FilePath ((</>),takeDirectory)
import System.Directory (doesDirectoryExist)
import System.Exit (die)
import Text.Read (readMaybe)
main = do
  beside <- doesDirectoryExist (takeDirectory __FILE__ </> "lib")
  let library=takeDirectory __FILE__ </> (if beside then "lib" else "../lib")
  allArgs <- getArgs
  let (translated,args)=case allArgs of "--translated":rest->(True,rest);_->(False,allArgs)
  case args of
    mode:yosys:seconds:output:flows -> do
      domain <- case mode of
        "strict" -> return StrictFourState
        "binary" -> return Binary
        _ -> die "proof domain must be strict or binary"
      limit <- maybe (die "invalid proof timeout") return (readMaybe seconds)
      if translated then selectPrefixesWithMetric "synthesis-translated" [__FILE__,library </> "ECOASTSelection.hs",library </> "ECOASTTranslatedMetric.hs",library </> "ECOASTMetric.hs"]
        (\dir a b -> do r<-measureTranslatedAST dir a b;return(translatedRaw r,translatedNormalized r))
        domain yosys limit output flows
      else selectPrefixes domain yosys limit output flows
    _ -> die "usage: select.hs [--translated] strict|binary YOSYS PROOF_SECONDS FRESH_OUTPUT FLOW..."
