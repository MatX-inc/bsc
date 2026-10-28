-- Optional JSON projection; no compiler output or proof policy depends on it.
module Main where
import ECODiagnostics (writeBackendReport)
import ECO.Replay (fiPath)
import System.Environment (getArgs)
import System.Exit (die)

main = do
    args <- getArgs
    case args of
      [snapshot,pre,final,ports,directory] ->
        writeBackendReport snapshot pre final ports directory >>= putStrLn . fiPath
      _ -> die "usage: bluehs backend-report.hs SNAPSHOT.eco-ast PREOPT.eco-preopt FINAL.eco-graph PORTS.eco-ports FRESH_OUTPUT_DIRECTORY"
