-- | An intentionally non-semantic census. Recognising Tcl syntax does not
-- mean a test has been lowered: every command is reported as unsupported.
module BscTestsuite.Census
  ( CensusSite(..), CensusReport(..), censusText, censusFile, censusTree
  , censusTreeIncludingTemplates, renderCensusJson, renderCensusText
  ) where

import Prelude hiding (Word)
import BscTestsuite.Tcl
import Control.Monad (filterM, forM)
import Data.Char (isDigit, ord)
import Data.List (group, intercalate, isPrefixOf, isSuffixOf, sort)
import Data.Maybe (fromMaybe)
import Numeric (showHex)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, pathIsSymbolicLink)
import System.FilePath ((</>), dropExtension, dropTrailingPathSeparator, normalise, takeFileName)

data CensusSite = CensusSite
  { sitePosition :: SourcePos, siteConstruct :: String, siteCategory :: String
  , siteReason :: String
  } deriving (Eq, Show)

data CensusReport = CensusReport
  { censusFiles :: [FilePath]
  , censusInactiveTemplates :: [FilePath]
  , censusSites :: [CensusSite]
  , censusErrors :: [TclError]
  } deriving (Eq, Show)

emptyReport :: CensusReport
emptyReport = CensusReport [] [] [] []

combine :: [CensusReport] -> CensusReport
combine rs = CensusReport (concatMap censusFiles rs)
  (concatMap censusInactiveTemplates rs) (concatMap censusSites rs) (concatMap censusErrors rs)

censusText :: FilePath -> String -> CensusReport
censusText path input = (inspectParse (parseScript path input)) { censusFiles = [path] }

censusFile :: FilePath -> IO CensusReport
censusFile path = censusText path <$> readFile path

-- | Active .exp paths under bsc.* only. A symlink is read at its lexical
-- path: fullparallel activates long tests by linking .exp to .exp.golden.
-- Configuration infrastructure is intentionally outside this population.
censusTree :: FilePath -> IO CensusReport
censusTree = censusTreeWith False

-- | Explicitly add inactive templates; this is a source census, not the
-- active DejaGNU population. Templates are still identified in the report.
censusTreeIncludingTemplates :: FilePath -> IO CensusReport
censusTreeIncludingTemplates = censusTreeWith True

censusTreeWith :: Bool -> FilePath -> IO CensusReport
censusTreeWith includeTemplates inputRoot = do
  let root = dropTrailingPathSeparator (normalise inputRoot)
  roots <- if "bsc." `isPrefixOf` takeFileName root then pure [root] else do
    entries <- sort <$> listDirectory root
    filterM doesDirectoryExist [root </> e | e <- entries, "bsc." `isPrefixOf` e]
  paths <- sort . concat <$> mapM filesBelow roots
  let active = filter (".exp" `isSuffixOf`) paths
      templates = filter (\p -> ".exp.golden" `isSuffixOf` p || ".exp.in" `isSuffixOf` p) paths
  inactive <- filterM (fmap not . doesFileExist . dropExtension) templates
  rs <- mapM censusFile (sort (active ++ if includeTemplates then inactive else []))
  pure (combine rs) { censusInactiveTemplates = inactive }
  where
    filesBelow dir = do
      entries <- sort <$> listDirectory dir
      concat <$> forM entries (\e -> do
        let path = dir </> e
        isDir <- doesDirectoryExist path
        if not isDir then pure [path] else do
          symlink <- pathIsSymbolicLink path
          if symlink then pure [] else filesBelow path)

inspectParse :: Either TclError Script -> CensusReport
inspectParse (Left err) = emptyReport { censusErrors = [err] }
inspectParse (Right s) = inspect s

inspect :: Script -> CensusReport
inspect = combine . map inspectCommand . scriptCommands

inspectCommand :: Command -> CensusReport
inspectCommand (Command _ []) = emptyReport
inspectCommand (Command p ws@(first:args)) = combine
  [ emptyReport { censusSites = [CensusSite p name category "not-yet-lowered"] }
  , combine [inspect sub | w <- ws, sub <- wordSubstitutions w]
  , bodies (dropWhile (== ':') name) args
  ]
  where
    name = fromMaybe ("<dynamic-command: " ++ wordSource first ++ ">") (staticWord first)
    category = case staticWord first of
      Nothing -> "dynamic-command"
      Just n | dropWhile (== ':') n `elem` tclCommands -> "tcl-command"
      _ -> "harness-or-unknown-command"

tclCommands :: [String]
tclCommands = words $ "after append apply array break catch cd clock close concat continue " ++
  "dict encoding eof error eval exec exit expr fblocked fconfigure fcopy file fileevent flush " ++
  "for foreach format gets glob global if incr info interp join lappend lassign lindex linsert " ++
  "list llength lmap lrange lrepeat lreplace lreverse lsearch lset lsort namespace open package " ++
  "pid proc puts pwd read regexp regsub rename return scan seek set socket source split string " ++
  "subst switch tailcall tell time trace try unset update uplevel upvar variable vwait while yield"

diagnostic :: String -> Word -> String -> CensusReport
diagnostic construct w reason = emptyReport
  { censusSites = [CensusSite (wordPosition w) construct "uninspected-script" reason] }

body :: String -> Word -> CensusReport
body construct w
  | wordKind w == Braced = inspectParse (parseScriptAt (wordBodyPosition w) (wordText w))
  | staticWord w == Just (wordText w) = inspectParse (parseScriptAt (wordBodyPosition w) (wordText w))
  | otherwise = diagnostic construct w "not-yet-lowered: script requires substitution or escape decoding"

expression :: String -> Word -> CensusReport
expression construct w = diagnostic construct w
  "not-yet-lowered: expression evaluation (including substitutions) is not interpreted"

bodies :: String -> [Word] -> CensusReport
bodies "proc" [_, _, w] = body "proc body" w
bodies "if" args = inspectIf args
bodies "foreach" args = lastBody "foreach body" args
bodies "lmap" args = lastBody "lmap body" args
bodies "for" [start, condition, next, action] = combine
  [body "for initialization" start, expression "for expression" condition
  , body "for iteration" next, body "for body" action]
bodies "while" [condition, action] = combine [expression "while expression" condition, body "while body" action]
bodies "catch" (w:_) = body "catch body" w
bodies "try" (w:rest) = combine [body "try body" w, handlers rest]
  where
    handlers (kind:_:_:action:more) | staticWord kind `elem` [Just "on", Just "trap"] =
      combine [body "try handler" action, handlers more]
    handlers [kind, action] | staticWord kind == Just "finally" = body "try finally" action
    handlers [] = emptyReport
    handlers (x:_) = diagnostic "try handlers" x "not-yet-lowered: unrecognized handler structure"
bodies "namespace" (subcommand:_:args)
  | staticWord subcommand `elem` [Just "eval", Just "inscope"] = concatenated "namespace script" args
bodies "eval" args = concatenated "eval script" args
bodies "uplevel" (level:args)
  | maybe False isLevel (staticWord level) = concatenated "uplevel script" args
  where
    isLevel ('#':s) = not (null s) && all isDigit s
    isLevel s = not (null s) && all isDigit s
bodies "uplevel" args = concatenated "uplevel script" args
bodies "switch" args = inspectSwitch args
bodies "expr" args = combine (map (expression "expr expression") args)
bodies "subst" args = lastDiagnostic "subst input" args
bodies _ _ = emptyReport

lastBody :: String -> [Word] -> CensusReport
lastBody _ [] = emptyReport
lastBody construct args = body construct (last args)

lastDiagnostic :: String -> [Word] -> CensusReport
lastDiagnostic _ [] = emptyReport
lastDiagnostic construct args = diagnostic construct (last args)
  "not-yet-lowered: runtime substitution is not interpreted"

concatenated :: String -> [Word] -> CensusReport
concatenated _ [] = emptyReport
concatenated construct [w] = body construct w
concatenated construct (w:_) = diagnostic construct w
  "not-yet-lowered: concatenated scripts require Tcl evaluation"

inspectIf :: [Word] -> CensusReport
inspectIf [] = emptyReport
inspectIf (condition:rest) = combine [expression "if expression" condition, branch afterThen]
  where
    afterThen = case rest of
      w:xs | staticWord w == Just "then" -> xs
      xs -> xs
    branch [] = diagnostic "if body" condition "not-yet-lowered: missing static body"
    branch (action:more) = combine [body "if body" action, following more]
    following [] = emptyReport
    following (w:xs) | staticWord w == Just "elseif" = inspectIf xs
    following [w, action] | staticWord w == Just "else" = body "else body" action
    following [action] = body "else body" action
    following (w:_) = diagnostic "if branches" w "not-yet-lowered: dynamic or malformed branch structure"

inspectSwitch :: [Word] -> CensusReport
inspectSwitch = options
  where
    options [] = emptyReport
    options (w:ws)
      | staticWord w == Just "--" = cases ws
      | staticWord w `elem` map Just ["-exact", "-glob", "-regexp", "-nocase"] = options ws
      | staticWord w `elem` map Just ["-matchvar", "-indexvar"] = options (drop 1 ws)
      | maybe False ("-" `isPrefixOf`) (staticWord w) =
          diagnostic "switch options" w "not-yet-lowered: unrecognized option"
      | otherwise = cases (w:ws)
    cases (_value:[w])
      | Just p <- firstContinuation (wordBodyPosition w) (wordText w) =
          diagnostic "switch cases" (w { wordPosition = p })
            "not-yet-lowered: switch list contains backslash-newline; source-preserving preprocessing is not implemented"
      | wordKind w == Braced || staticWord w == Just (wordText w) =
          case parseListAt (wordBodyPosition w) (wordText w) of
            Left err -> emptyReport { censusErrors = [err] }
            Right pairs -> actions pairs
      | otherwise = diagnostic "switch cases" w "not-yet-lowered: dynamic case list"
    cases (_value:pairs) = actions pairs
    cases [] = emptyReport
    actions (_pattern:action:more) = combine
      [if staticWord action == Just "-" then emptyReport else body "switch body" action, actions more]
    actions [] = emptyReport
    actions [w] = diagnostic "switch cases" w "not-yet-lowered: unmatched pattern"

-- The script parser performs continuation substitution in an outer braced
-- word, but the Tcl list parser preserves it in an inner braced list element.
-- Until the preprocessing step has a source map, do not inspect these lists
-- using raw source as if it were the runtime list value. Report the source
-- location of the sequence rather than silently changing its interpretation.
firstContinuation :: SourcePos -> String -> Maybe SourcePos
firstContinuation p ('\\':'\n':_) = Just p
firstContinuation p (c:cs) = firstContinuation p
  { sourceLine = sourceLine p + if c == '\n' then 1 else 0
  , sourceColumn = if c == '\n' then 1 else sourceColumn p + 1
  , sourceOffset = sourceOffset p + 1
  } cs
firstContinuation _ [] = Nothing

renderCensusText :: CensusReport -> String
renderCensusText r = unlines $
  [ "Lexical census only; no semantic lowering is implemented."
  , "Files: " ++ show (length (censusFiles r))
  , "Inactive templates: " ++ show (length (censusInactiveTemplates r))
  , "Unlowered command sites: " ++ show (length [s | s <- censusSites r, siteCategory s /= "uninspected-script"])
  , "Uninspected script/expression sites: " ++ show (length [s | s <- censusSites r, siteCategory s == "uninspected-script"])
  , "Lexical errors: " ++ show (length (censusErrors r))
  , "These are source sites, not test/check counts or PASS/XFAIL/FAIL results."
  , "Counts by construct:"
  ] ++ ["  " ++ construct ++ ": " ++ show count | (construct, count) <- constructCounts r]
    ++ [location (sitePosition s) ++ ": " ++ siteConstruct s ++ ": " ++ siteReason s | s <- censusSites r]
    ++ [location (errorPosition e) ++ ": lexical-error: " ++ errorMessage e | e <- censusErrors r]

renderCensusJson :: CensusReport -> String
renderCensusJson r = object
  [ ("schema_version", "1"), ("semantic_lowering_implemented", "false")
  , ("count_unit", quoted "source-sites-not-tests")
  , ("command_site_count", show (length [s | s <- censusSites r, siteCategory s /= "uninspected-script"]))
  , ("uninspected_script_site_count", show (length [s | s <- censusSites r, siteCategory s == "uninspected-script"]))
  , ("files", array (map quoted (censusFiles r)))
  , ("inactive_templates", array (map quoted (censusInactiveTemplates r)))
  , ("construct_counts", array [object [("construct", quoted c), ("count", show n)] | (c,n) <- constructCounts r])
  , ("sites", array (map site (censusSites r)))
  , ("lexical_errors", array (map err (censusErrors r)))
  ] ++ "\n"
  where
    site s = object (pos (sitePosition s) ++
      [("construct", quoted (siteConstruct s)), ("category", quoted (siteCategory s))
      , ("status", quoted "not-yet-lowered"), ("reason", quoted (siteReason s))])
    err e = object (pos (errorPosition e) ++ [("message", quoted (errorMessage e))])
    pos p = [("file", quoted (sourceFile p)), ("line", show (sourceLine p)), ("column", show (sourceColumn p))]

constructCounts :: CensusReport -> [(String, Int)]
constructCounts r = [(x, length xs) | xs@(x:_) <- group (sort (map siteConstruct (censusSites r)))]

location :: SourcePos -> String
location p = sourceFile p ++ ":" ++ show (sourceLine p) ++ ":" ++ show (sourceColumn p)

object :: [(String, String)] -> String
object fields = "{" ++ intercalate "," [quoted k ++ ":" ++ v | (k,v) <- fields] ++ "}"

array :: [String] -> String
array xs = "[" ++ intercalate "," xs ++ "]"

quoted :: String -> String
quoted s = '"' : concatMap escape s ++ "\""
  where
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape c | ord c < 32 || (ord c >= 0xd800 && ord c <= 0xdfff) =
      let h = showHex (ord c) "" in "\\u" ++ replicate (4 - length h) '0' ++ h
    escape c = [c]
