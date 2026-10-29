-- Run with bluehs util/eco/match.hs OLD NEW OUTPUT_STEM.
-- This process is deliberately independent of compiler/native intern state.
module Main where

import ECO.Types
import ECOMatchTypes
import ECOMatch
import ECOAggressive (auditJSON)
import Data.Char (ord)
import Data.List (intercalate, sort)
import Numeric (showHex)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath (takeDirectory)
import Text.Read (readMaybe)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [oldPath,newPath,stem] -> run Baseline oldPath newPath stem
        ["--policy","baseline",oldPath,newPath,stem] -> run Baseline oldPath newPath stem
        ["--policy","local-roles",oldPath,newPath,stem] -> run LocalRoles oldPath newPath stem
        ["--policy","aggressive",oldPath,newPath,stem] -> run Aggressive oldPath newPath stem
        _ -> die "usage: eco-match [--policy baseline|local-roles|aggressive] OLD.eco-graph NEW.eco-graph OUTPUT_STEM"

run :: MatchPolicy -> FilePath -> FilePath -> FilePath -> IO ()
run policy oldPath newPath stem = do
    old <- readGraph oldPath
    new <- readGraph newPath
    (result,audit) <- either die return (matchGraphsWithAudit policy old new)
    renames <- either die return (proposeRenames (egReserved new) old new result)
    let plan = EcoRenamePlan ecoGraphSchema (egModule new) renames
        eligibleNames = [s | n <- egNodes new, enEligible n, Just s <- [enName n]]
        eligible = if policy == Aggressive then sort eligibleNames else eligibleNames
    createDirectoryIfMissing True (takeDirectory stem)
    writeFile (stem ++ ".eco-plan") (show plan ++ "\n")
    writeFile (stem ++ ".eco-match") (show result ++ "\n")
    case audit of
        Nothing -> return ()
        Just a -> do
            writeFile (stem ++ ".eco-audit") (show a ++ "\n")
            writeFile (stem ++ ".eco-audit.json") (auditJSON renames a ++ "\n")
    writeFile (stem ++ ".eco-map.json") $
        "{\"schema\":1,\"module\":" ++ json (egModule new) ++
        ",\"renames\":{" ++ intercalate "," [json a ++ ":" ++ json b | (a,b) <- renames] ++
        "},\"eligible\":[" ++ intercalate "," (map json eligible) ++ "]}\n"
    putStrLn $ "{\"policy\":" ++ json (case policy of Baseline -> "baseline"; LocalRoles -> "local-roles"; Aggressive -> "aggressive") ++
        ",\"module\":" ++ json (egModule new) ++
        ",\"old_nodes\":" ++ show (length (egNodes old)) ++
        ",\"new_nodes\":" ++ show (length (egNodes new)) ++
        ",\"exact_content\":" ++ show (length (filter ((== ExactContent) . ecKind) (emCorrespondences result))) ++
        ",\"changed_roles\":" ++ show (length (filter ((== ChangedRole) . ecKind) (emCorrespondences result))) ++
        ",\"local_roles\":" ++ show (length (filter ((== "typed-ordered-operand-role") . ecReason) (emCorrespondences result))) ++
        ",\"unmatched_old\":" ++ show (length (emUnmatchedOld result)) ++
        ",\"unmatched_new\":" ++ show (length (emUnmatchedNew result)) ++
        ",\"ambiguous_buckets\":" ++ show (emAmbiguousBuckets result) ++
        ",\"duplicate_content_buckets\":" ++ show (emAmbiguousBuckets result) ++
        ",\"ambiguous_buckets_scope\":\"legacy count of duplicate fingerprint buckets, not unresolved eligible outcomes\"" ++
        (case audit of Nothing -> ""; Just _ -> ",\"unresolved_eligible\":0") ++
        ",\"renamed_symbols\":" ++ show (length renames) ++ "}"

readGraph :: FilePath -> IO EcoGraph
readGraph path = do
    contents <- readFile path
    maybe (die ("invalid ECO graph: " ++ path)) return (readMaybe contents)

json :: String -> String
json str = '"' : concatMap escape str ++ "\""
  where
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape c | ord c < 32 = let h = showHex (ord c) ""
                           in "\\u" ++ replicate (4 - length h) '0' ++ h
             | otherwise = [c]
