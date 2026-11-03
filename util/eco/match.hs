-- Run with bluehs util/eco/match.hs OLD NEW OUTPUT_STEM.
-- This process is deliberately independent of compiler/native intern state.
module Main where

import ECO.Types
import ECOMatchTypes
import ECOMatch
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath (takeDirectory)
import Text.Read (readMaybe)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [oldPath,newPath,stem] -> run oldPath newPath stem
        _ -> die "usage: eco-match OLD.eco-graph NEW.eco-graph OUTPUT_STEM"

run :: FilePath -> FilePath -> FilePath -> IO ()
run oldPath newPath stem = do
    old <- readGraph oldPath
    new <- readGraph newPath
    result <- either die return (matchGraphs old new)
    renames <- either die return (proposeRenames (egReserved new) old new result)
    createDirectoryIfMissing True (takeDirectory stem)
    writeFile (stem ++ ".eco-plan") (show (EcoRenamePlan ecoGraphSchema (egModule new) renames) ++ "\n")
    putStrLn $ egModule new ++ ": " ++ show (length (emCorrespondences result)) ++ " correspondences, " ++
        show (length (emUnmatchedOld result)) ++ " unmatched old, " ++
        show (length (emUnmatchedNew result)) ++ " unmatched new, " ++
        show (length renames) ++ " renames"

readGraph :: FilePath -> IO EcoGraph
readGraph path = do
    contents <- readFile path
    maybe (die ("invalid ECO graph: " ++ path)) return (readMaybe contents)
