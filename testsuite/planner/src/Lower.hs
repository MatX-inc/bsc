-- | Adapt testsuite Tcl to semantic test procedures.
--
-- Tcl provides syntax and source positions; Procedures defines the tests.
-- This adapter resolves a small scalar/foreach vocabulary and records either
-- a semantic test or a located issue. Unsupported items never discard other
-- scripts or already planned tests. Counts describe inventory, not verdicts.
--
-- Unknown values are not guessed. Unsupported assignments invalidate their
-- destination, and opaque setup may leave later tests unresolved. Recognized
-- test procedures with unimplemented test kinds are individual unsupported
-- items. No Tcl, compiler, or filesystem command is executed here.
module Lower
  ( lowerTest, lowerPlan, lowerInvocation, renderIssue, supportedCompilerOptions
  ) where

import Prelude hiding (Word)
import Control.Monad (unless, when)
import Data.Char (isControl, ord)
import Data.List (foldl', isInfixOf)
import qualified Data.Map.Strict as Map
import Procedures
import Tcl
import TestPlan

-- The environment is local to a source script. Nothing from an unimplemented
-- assignment is allowed to leave an old value in place. setupProblem records
-- an opaque effect (such as exec or a file mutation), not ordinary test order.
data LowerState = LowerState
  { variables :: Map.Map String String
  , setupProblem :: Maybe SourcePos
  , itemsReversed :: [PlanItem]
  , testCount :: Int
  , expansionCount :: Int
  , expansionStopped :: Bool
  }

data Failure = Failure SourcePos String IssueKind String

renderIssue :: PlanIssue -> String
renderIssue issue = sourceFile p ++ ":" ++ show (sourceLine p) ++ ":" ++
  show (sourceColumn p) ++ ": " ++ issueConstruct issue ++ ": " ++
  category ++ issueReason issue
  where
    p = issuePosition issue
    category = case issueKind issue of
      UnsupportedConstruct -> "unsupported: "
      UnresolvedDependency -> "unresolved: "

-- | Invalid global configuration is an invocation error. Unsupported source
-- constructs are data in a valid, possibly incomplete plan.
lowerPlan :: PlanConfig -> [(FilePath, String)] -> Either String TestPlan
lowerPlan config sources = do
  validateProcedureConfig config
  let plan = TestPlan config [lowerTest config path source | (path, source) <- sources]
  validatePlan plan
  pure plan

-- | Retain each discovered script, including empty and malformed scripts.
-- Full-script syntax errors cannot yet be recovered past: one issue covers the
-- script, while independently parsed scripts remain available in the plan.
lowerTest :: PlanConfig -> FilePath -> String -> ScriptPlan
lowerTest config path source = case parseScript path source of
  Left err -> ScriptPlan path [Unplanned (PlanIssue Nothing
    (errorPosition err) "Tcl syntax" UnsupportedConstruct (diagnosticText (errorMessage err)))]
  Right script -> ScriptPlan path (reverse (itemsReversed result))
    where result = lowerScript config path (LowerState Map.empty Nothing [] 0 0 False) script

-- Reserve a number before resolving a recognized invocation's arguments.
-- Ordinary commands and unimplemented helpers are never counted as tests.
lowerScript :: PlanConfig -> FilePath -> LowerState -> Script -> LowerState
lowerScript config path initial script =
  foldl' lower initial (scriptCommands script)
  where
    lower state command
      | expansionStopped state = state
      | otherwise =
          let advanced = budget (commandPosition command) state
              recognized = case commandWords command of
                nameWord:_ -> maybe False (`elem` supportedProcedures) (staticWord nameWord)
                [] -> False
              identifier = Identifier path (testCount advanced + 1)
              numbered = if recognized then advanced { testCount = testCount advanced + 1 }
                         else advanced
              identity = if recognized then Just identifier else Nothing
          in if expansionStopped advanced then advanced else
             case lowerCommand config path identifier numbered command of
               Right next -> next
               Left failure -> recover identity numbered command failure

-- Count both commands and loop iterations so nested empty bodies cannot evade
-- the expansion limit. The final issue explicitly says that expansion stopped;
-- it is not a claim to have counted the unexpanded remainder.
budget :: SourcePos -> LowerState -> LowerState
budget position state
  | expansionCount state >= 100000 =
      addIssue Nothing (Failure position "static expansion" UnsupportedConstruct
        "expansion limit reached; remaining commands and iterations were not inspected")
        state { expansionStopped = True }
  | otherwise = state { expansionCount = expansionCount state + 1 }

lowerCommand :: PlanConfig -> FilePath -> Identifier -> LowerState -> Command
             -> Either Failure LowerState
lowerCommand _ _ _ state (Command _ []) = Right state
lowerCommand config path identifier state (Command position (nameWord:args)) = do
  name <- maybe (unsupported position "dynamic command" "command names must be literal")
    Right (staticWord nameWord)
  let value = resolve state name
  case name of
    "set" -> do
      supplied <- mapM value args
      case supplied of
        [variable, contents] -> do
          checkVariable position name variable
          pure state { variables = Map.insert variable contents (variables state) }
        [variable] -> do
          checkVariable position name variable
          unless (Map.member variable (variables state)) $
            unresolved position name ("unbound scalar variable: " ++ variable)
          pure state
        _ -> unsupported position name "expected a scalar name and optional value"
    "foreach" -> case args of
      [variableWord, listWord, bodyWord] -> do
        variable <- value variableWord
        checkVariable position name variable
        contents <- value listWord
        wordsInList <- fromTcl name (parseListAt (wordBodyPosition listWord) contents)
        items <- mapM (maybe (unsupported position name "list item could not be decoded") Right . staticWord) wordsInList
        unless (wordKind bodyWord == Braced) $
          unsupported (wordPosition bodyWord) name "only a literal braced loop body is supported"
        body <- fromTcl name (parseScriptAt (wordBodyPosition bodyWord) (wordText bodyWord))
        when (length items > 100000) $
          unsupported position name "static expansion exceeds 100000 iterations"
        let iteration current item
              | expansionStopped current = current
              | otherwise =
                  let advanced = budget position current
                  in if expansionStopped advanced then advanced else
                    lowerScript config path
                      advanced { variables = Map.insert variable item (variables advanced) } body
        pure (foldl' iteration state items)
      _ -> unsupported position name "only one scalar variable, one list, and a braced body are supported"
    _ | name `elem` supportedProcedures -> compile name =<< mapM value args
    _ | name `elem` unsupportedTestProcedures ->
          unsupported position name "test kind is not implemented"
      | otherwise -> unsupported position name "procedure or setup semantics are not implemented"
  where
    compile name supplied = do
      test <- either (unsupported position name) Right
        (lowerInvocation config identifier position name supplied)
      case setupProblem state of
        Just prerequisite -> unresolved position name
          ("setup depends on unsupported item at " ++ sourceFile prerequisite ++ ":" ++
            show (sourceLine prerequisite) ++ ":" ++ show (sourceColumn prerequisite))
        Nothing -> pure state { itemsReversed = Planned test : itemsReversed state }

-- | Adapt an invocation whose Tcl arguments have already been resolved. This
-- is also the semantic boundary used to compare inert Tcl provenance records.
lowerInvocation :: PlanConfig -> Identifier -> SourcePos -> String -> [String]
                -> Either String Test
lowerInvocation config identifier position name args = case name of
  "compile_pass" -> ordinary compilePass
  "compile_fail" -> ordinary compileFail
  "compile_fail_error" -> do
    (compilation, expected) <- arguments (compileErrorArguments position name args)
    compileFailError config identifier position compilation expected
  _ -> Left ("unsupported test procedure: " ++ name)
  where
    ordinary procedure = do
      compilation <- arguments (compileArguments position name args)
      procedure config identifier position compilation
    arguments (Left (Failure _ _ _ reason)) = Left reason
    arguments (Right value) = Right value

resolve :: LowerState -> String -> Word -> Either Failure String
resolve state name word = case resolveScalarWord (`Map.lookup` variables state) word of
  Right value
    | length (take (maxExpandedValueLength + 1) value) > maxExpandedValueLength ->
        unsupported (wordPosition word) name "expanded scalar exceeds 1048576 characters"
    | otherwise -> Right value
  Left err
    | "unbound or unsupported scalar variable" `isInfixOf` errorMessage err ->
        unresolved (errorPosition err) name (errorMessage err)
    | otherwise -> fromTcl name (Left err)

-- Command counts alone do not bound work: repeated scalar concatenation can
-- grow a list exponentially before foreach sees it. Check a bounded prefix
-- before storing or parsing any expanded value.
maxExpandedValueLength :: Int
maxExpandedValueLength = 1048576

-- Test adapters only decode Tcl's calling convention here. Procedures owns
-- source/option restrictions and the meaning of pass, fail, and internal checks.
compileArguments :: SourcePos -> String -> [String] -> Either Failure Compilation
compileArguments position name args = do
  (source, rawOptions, nodeps) <- case args of
    [source] -> Right (source, "", "0")
    [source, options] -> Right (source, options, "0")
    [source, options, nodeps] -> Right (source, options, nodeps)
    _ -> unsupported position name "expected source, optional options, and optional nodeps"
  unless (nodeps `elem` ["0", "1"]) $ unsupported position name "nodeps must be 0 or 1"
  options <- parseOptions position name rawOptions
  pure (Compilation source options (nodeps == "0"))

compileErrorArguments :: SourcePos -> String -> [String]
                      -> Either Failure (Compilation, ExpectedError)
compileErrorArguments position name args = do
  (source, tag, rawCount, options, nodeps) <- case args of
    [source, tag] -> Right (source, tag, "1", "", "0")
    [source, tag, count] -> Right (source, tag, count, "", "0")
    [source, tag, count, options] -> Right (source, tag, count, options, "0")
    [source, tag, count, options, nodeps] -> Right (source, tag, count, options, nodeps)
    _ -> unsupported position name "expected source, error tag, optional error count, optional options, and optional nodeps"
  -- Tcl interprets leading-zero numbers as octal (and rejects some spellings).
  -- Accept only canonical decimal counts rather than changing their meaning.
  let canonical = case rawCount of
        "0" -> True
        first:rest -> first >= '1' && first <= '9' && all (\c -> c >= '0' && c <= '9') rest
        [] -> False
      bounded = length rawCount <= length (show (maxBound :: Int))
      countError = "error count must be a canonical nonnegative decimal Int (no leading zeros)"
  unless (canonical && bounded) $ unsupported position name countError
  let count = read rawCount :: Integer
  when (count > toInteger (maxBound :: Int)) $
    unsupported position name countError
  compilation <- compileArguments position name [source, options, nodeps]
  pure (compilation, ExpectedError tag (fromInteger count))

parseOptions :: SourcePos -> String -> String -> Either Failure [String]
parseOptions position name contents = do
  -- The harness parses option strings a second time as Tcl, then appends its
  -- own flags/source. Shell splitting or ignoring a trailing separator differs.
  when (any (`elem` ";\r\n") contents) $
    unsupported position name "option strings containing command separators are unsupported"
  Script commands <- fromTcl name (parseScriptAt position ("options " ++ contents))
  case commands of
    [Command _ (_:args)] -> mapM literal args
    _ -> unsupported position name "option string contains Tcl command separators"
  where
    literal word = case staticWord word of
      Just item | wordKind word /= Expanded -> Right item
      _ -> unsupported (wordPosition word) name "option string requires substitution or argument expansion"

addIssue :: Maybe Identifier -> Failure -> LowerState -> LowerState
addIssue identifier (Failure position construct kind reason) state = state
  { itemsReversed = Unplanned (PlanIssue identifier position
      (diagnosticText construct) kind (diagnosticText reason))
      : itemsReversed state }

-- Even an empty command name or control characters are valid unsupported
-- input. Escape them for the diagnostic rather than rejecting the whole plan.
diagnosticText :: String -> String
diagnosticText value
  | null value || any invalid value = show value
  | otherwise = value
  where invalid c = isControl c || (ord c >= 0xd800 && ord c <= 0xdfff)

-- Keep a failed item without silently retaining values it would have changed.
-- Known test calls do not alter our Tcl scalar environment. They still need
-- their artifact dependencies bound before an executor can run the plan.
-- Opaque calls may change setup or redefine procedures, so subsequent tests
-- remain inventoried as unresolved rather than being presented as executable.
recover :: Maybe Identifier -> LowerState -> Command -> Failure -> LowerState
recover identifier state command failure = case commandWords command of
  nameWord:args ->
    let recorded = addIssue identifier failure state
        block = recorded { variables = Map.empty, setupProblem = Just (commandPosition command) }
        hasSubstitutions = any (not . null . wordSubstitutions) args
    in case staticWord nameWord of
      Just "set" | not hasSubstitutions -> case args of
        variableWord:_ -> case resolve state "set" variableWord of
          Right variable | variable `elem` ordinaryScalars ->
            recorded { variables = Map.delete variable (variables recorded) }
          _ -> block
        _ -> block
      Just name | not hasSubstitutions &&
        name `elem` (supportedProcedures ++ unsupportedTestProcedures) ->
          if secondParseMayChangeSetup state name args then block else recorded
      _ -> block
  [] -> addIssue identifier failure state

-- Compiler option strings are evaluated again by the harness. Braces protect
-- substitutions only during the first parse: {[set ::source Changed.bs]} is
-- still effectful when exec_with_log evaluates the assembled command. Unknown
-- flags that were proven literal do not have this problem. The recognized
-- backend/object helpers also splice their source and module into that command.
secondParseMayChangeSetup :: LowerState -> String -> [Word] -> Bool
secondParseMayChangeSetup state name args = case mapM (resolve state name) args of
  -- The harness may bind names that this adapter does not know. Failure to
  -- resolve here does not prove the real Tcl call stops before its second parse.
  Left _ -> True
  Right values -> any unsafe (secondParsed values)
  where
    -- The diagnostic tag and count are data passed to find_n_error, not part
    -- of the compiler command's second Tcl parse.
    secondParsed (source:_:_:options:_) | name == "compile_fail_error" = [source, options]
    secondParsed (source:_:_) | name == "compile_fail_error" = [source]
    secondParsed values = values
    unsafe value = case parseOptions (SourcePos "option" 1 1 0) name value of
      Left _ -> True
      Right _ -> False

-- These recognized harness procedures denote tests whose semantic kinds have
-- not been implemented. This list is not a prefix heuristic: arbitrary unknown
-- procedures can mutate setup. Adding a name requires checking its harness role.
unsupportedTestProcedures :: [String]
unsupportedTestProcedures =
  [ "compile_verilog_pass", "compile_object_pass", "compile_pass_no_warning"
  , "compile_backend_pass"
  , "compile_verilog_fail_error", "compile_verilog_fail"
  , "compile_verilog_schedule_pass", "compile_verilog_pass_warning"
  , "compile_object_fail_error", "compile_pass_warning"
  , "compile_verilog_pass_no_warning", "compile_object_fail"
  , "compile_verilog_schedule_fail", "compile_verilog_fail_no_internal_error"
  , "compile_object_pass_warning", "compile_no_source_fail_error"
  , "compile_fail_error_warnings"
  ]

-- DejaGNU sources scripts at global scope. Names are admitted only when they
-- are ordinary test-local scalar values, rather than harness control state.
ordinaryScalars :: [String]
ordinaryScalars =
  [ "source", "sources", "src", "file", "files", "filename", "flags", "flag"
  , "opts", "options", "name", "stem", "extension", "suffix", "variants"
  , "variant", "unused", "x", "y", "i", "j", "command"
  ]

checkVariable :: SourcePos -> String -> String -> Either Failure ()
checkVariable position name variable = unless (variable `elem` ordinaryScalars) $
  unsupported position name ("unsupported scalar name " ++ show variable ++ "; harness state cannot be assigned")

fromTcl :: String -> Either TclError a -> Either Failure a
fromTcl name = either (\err -> unsupported (errorPosition err) name (errorMessage err)) Right

unsupported, unresolved :: SourcePos -> String -> String -> Either Failure a
unsupported position name = Left . Failure position name UnsupportedConstruct
unresolved position name = Left . Failure position name UnresolvedDependency
