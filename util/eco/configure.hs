-- Write an explicit, typed flow configuration without constructing it by hand.
-- The written record is also the trust-roots file for every later verifier.
module Main where
import ECOASTFlow
import ECOASTSearch
import ECOASTProof
import Control.Monad (when)
import System.Directory (canonicalizePath, doesPathExist)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath (takeDirectory, takeFileName, (</>))

main = do
  args <- getArgs
  case args of
    [domain,bsc,modName,old,new,oldCwd,newCwd,library,manifest,yosys,out,path] -> do
      proofDomain <- case domain of
        "strict" -> return StrictFourState
        "binary" -> return Binary
        _ -> die "proof domain must be strict or binary"
      exists <- doesPathExist path
      when exists (die "configuration already exists; use a fresh path")
      paths <- mapM canonicalizePath [bsc,old,new,oldCwd,newCwd,library,manifest,yosys]
      outputParent <- canonicalizePath (takeDirectory out)
      let [compiler,oldCapture,newCapture,oldDirectory,newDirectory,lib,implementation,solver] = paths
          options = defaultSearchOptions {soDomain=proofDomain,soYosys=solver}
          config = CaptureFlow compiler modName oldCapture newCapture oldDirectory newDirectory
                     (Just lib) (Just lib) (outputParent </> takeFileName out) options 120 implementation
      writeFile path (show config ++ "\n")
      putStrLn ("Wrote " ++ path ++ "; proof domain=" ++ show proofDomain)
    _ -> die "usage: configure.hs strict|binary BSC MODULE OLD.replay NEW.replay OLD_CWD NEW_CWD LIB SDK_MANIFEST YOSYS OUTPUT CONFIG"
