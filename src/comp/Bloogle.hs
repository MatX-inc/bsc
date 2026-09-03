module Bloogle(bloogleAnnotate) where

import Control.Exception(SomeException, try)
import Data.Char(isSpace)
import Data.List(isPrefixOf, isSuffixOf)
import System.Exit(ExitCode(..))
import System.Process(readProcessWithExitCode)

import Error(EMsg, ErrMsg(..))
import Flags(Flags(..))

-- Decorate diagnostics with suggestions from bloogle, a Hoogle-like
-- search over the Bluespec libraries.  This only happens when the user
-- has supplied a database with -bloogle-db, and a missing or failing
-- bloogle executable just leaves the diagnostic without suggestions.
--
-- The queries use the "bloogle search <db> <query>" CLI: a bare word is
-- a name search and a query starting with "::" is a type search, with
-- one plain-text result per output line.

-- cap on the number of suggestions appended to one diagnostic
maxMatches :: Int
maxMatches = 5

bloogleAnnotate :: Flags -> EMsg -> IO EMsg
bloogleAnnotate flags msg@(pos, emsg) =
    case (bloogleDb flags, emsg) of
      (Just db, WTypedHole t []) -> do
          -- a type search already returns approximate matches
          ls <- bloogleSearch db (":: " ++ t)
          return (pos, WTypedHole t (take maxMatches ls))
      (Just db, EUnboundVar v []) -> do
          -- a name search is a substring match, so keep only the
          -- results whose identifier is exactly the unbound name
          ls <- bloogleSearch db v
          return (pos, EUnboundVar v (take maxMatches (filter (isNamed v) ls)))
      _ -> return msg

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
-- output shapes
isNamed :: String -> String -> Bool
isNamed v l = any matches (words (takeWhile (/= ':') l))
  where matches w = (w == v) || (('.':v) `isSuffixOf` w)
