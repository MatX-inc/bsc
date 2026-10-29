-- Freshly verify a selected flow with trust roots the caller supplies.
module Main where
import ECOASTReplay
import ECO.Replay (FileIdentity(..))
import System.Environment (getArgs)
import System.Exit (die)

main = do
  args <- getArgs
  case args of
    ["--flow",rootsPath,flow,output] -> do
      roots <- rootsFromConfig rootsPath
      verified <- replaySelectedFlow roots flow output
      validateReplay verified
      putStrLn ("Reproved ordinary NEW through selected flow; output=" ++ fiPath (replayRTL verified))
    _ -> die "usage: replay.hs --flow ROOTS.config QUALIFIED_FLOW FRESH_OUTPUT"
