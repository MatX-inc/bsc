module Bloogle(bloogleTypeMatches, bloogleNameMatches) where

import Control.Exception(SomeException, try)
import Data.Char(isSpace)
import Data.List(isPrefixOf, isSuffixOf)
import System.Exit(ExitCode(..))
import System.Process(readProcessWithExitCode)

-- Queries against bloogle, a Hoogle-like search over the Bluespec
-- libraries, used to decorate diagnostics with suggestions when the
-- user has supplied a database with -bloogle-db.  A missing or failing
-- bloogle executable just yields no suggestions, never an error.
--
-- The queries use the "bloogle search <db> <query>" CLI: a bare word is
-- a name search and a query starting with "::" is a type search, with
-- one plain-text result per output line.
--
-- This module deliberately does not import Error or Flags, so that the
-- error-reporting code in Error.hs can call it without an import cycle.

-- cap on the number of suggestions appended to one diagnostic
maxMatches :: Int
maxMatches = 5

-- approximate matches from a type search on the given type
bloogleTypeMatches :: String -> String -> IO [String]
bloogleTypeMatches db t = do
    ls <- bloogleSearch db (":: " ++ t)
    return (take maxMatches ls)

-- results of a name search (a substring match), filtered down to the
-- lines whose identifier is exactly the given name
bloogleNameMatches :: String -> String -> IO [String]
bloogleNameMatches db v = do
    ls <- bloogleSearch db v
    return (take maxMatches (filter (isNamed v) ls))

-- run "bloogle search <db> <query>" and return the result lines
bloogleSearch :: String -> String -> IO [String]
bloogleSearch db query = do
    r <- try (readProcessWithExitCode "bloogle" ["search", db, query] "")
    case (r :: Either SomeException (ExitCode, String, String)) of
      Right (ExitSuccess, out, _) -> return (filter isResultLine (lines out))
      _ -> return []

-- keep the "Module name :: type" lines, dropping blank lines, notes
-- like "-- plus more results not shown", and "No results found"
isResultLine :: String -> Bool
isResultLine l =
    let l' = dropWhile isSpace l
    in  not (null l') &&
        not ("--" `isPrefixOf` l') &&
        not ("No results" `isPrefixOf` l')

-- whether a result line is for an identifier named exactly "v",
-- accepting both the "Module name :: type" and "Module.name :: type"
-- output shapes (and "Module data Name a b" style lines for types)
isNamed :: String -> String -> Bool
isNamed v l = any matches (words (takeWhile (/= ':') l))
  where matches w = (w == v) || (('.':v) `isSuffixOf` w)
