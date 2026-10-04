-- | A deliberately closed, pure first lowering of testsuite Tcl. Unsupported
-- constructs reject the whole scenario; no prefix is returned as a plan.
module BscTestsuite.Lower
  ( PlanIssue(..), lowerTest, lowerPlan, renderIssue, supportedCompilerOptions
  ) where

import Prelude hiding (Word)
import BscTestsuite.Tcl
import BscTestsuite.TestPlan
import Control.Monad (foldM, unless, when)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import qualified Data.Map.Strict as Map
import System.FilePath ((</>), dropExtension, normalise, takeDirectory, takeExtension)

data PlanIssue = PlanIssue
  { issuePosition :: SourcePos, issueConstruct :: String, issueReason :: String
  } deriving (Eq, Show)

data LowerState = LowerState
  { variables :: Map.Map String String
  , stepsReversed :: [Step]
  , checksReversed :: [Check]
  , evaluatedCommands :: Int
  }

supportedCompilerOptions :: [String]
supportedCompilerOptions = ["-let-gen", "-no-let-gen", "-dinternal", "-v"]

renderIssue :: PlanIssue -> String
renderIssue (PlanIssue p construct reason) = sourceFile p ++ ":" ++
  show (sourceLine p) ++ ":" ++ show (sourceColumn p) ++ ": " ++
  construct ++ ": " ++ reason

lowerPlan :: PlanConfig -> [(FilePath, String)] -> Either [PlanIssue] TestPlan
lowerPlan config sources = case partitionResults (map (uncurry (lowerTest config)) sources) of
  ([], scenarios) ->
    let plan = TestPlan config scenarios
    in case validatePlan plan of
      Left message -> Left [PlanIssue (SourcePos "<plan>" 1 1 0) "plan" message]
      Right () -> Right plan
  (issues, _) -> Left issues
  where
    partitionResults = foldr collect ([], [])
    collect (Left issue) (issues, scenarios) = (issue:issues, scenarios)
    collect (Right scenario) (issues, scenarios) = (issues, scenario:scenarios)

lowerTest :: PlanConfig -> FilePath -> String -> Either PlanIssue Scenario
lowerTest config test source = do
  let start = SourcePos test 1 1 0
  mapM_ (checkOption start "configuration") (configCompilerOptions config)
  parsed <- fromTcl "Tcl syntax" (parseScript test source)
  result <- lowerScript config test [] (LowerState Map.empty [] [] 0) parsed
  let ordered = reverse (stepsReversed result)
      compileCount = length [() | step <- ordered, BscCompile {} <- [stepOperation step]]
      -- Retained workspace state may be what a subsequent invocation tests.
      -- Conservatively forbid reuse for every multi-compile scenario.
      policy = if compileCount > 1 then Never else LocalOnly
      scenario = Scenario test (map (\step -> step {stepCacheability = policy}) ordered)
        (reverse (checksReversed result))
  case validatePlan (TestPlan config [scenario]) of
    Left message -> Left (PlanIssue start "plan" message)
    Right () -> Right scenario

lowerScript :: PlanConfig -> FilePath -> [Int] -> LowerState -> Script
            -> Either PlanIssue LowerState
lowerScript config test prefix initial script =
  foldM lower initial (zip [1..] (scriptCommands script))
  where
    lower state (ordinal, command) = do
      when (evaluatedCommands state >= 100000) $
        problem (commandPosition command) "Tcl" "static expansion exceeds 100000 commands"
      lowerCommand config test (prefix ++ [ordinal])
        state {evaluatedCommands = evaluatedCommands state + 1} command

lowerCommand :: PlanConfig -> FilePath -> [Int] -> LowerState -> Command
             -> Either PlanIssue LowerState
lowerCommand _ _ _ state (Command _ []) = Right state
lowerCommand config test address state (Command p (nameWord:args)) = do
  name <- maybe (problem p "dynamic command" "command names must be literal") Right (staticWord nameWord)
  let value = fromTcl name . resolveScalarWord (`Map.lookup` variables state)
      values = mapM value args
  case name of
    "set" -> do
      supplied <- values
      case supplied of
        [variable, contents] -> do
          checkVariable p name variable
          Right state {variables = Map.insert variable contents (variables state)}
        [variable] -> do
          checkVariable p name variable
          unless (Map.member variable (variables state)) $
            problem p name ("unbound scalar variable: " ++ variable)
          Right state
        _ -> problem p name "expected a scalar name and optional value"
    "foreach" -> case args of
      [variableWord, listWord, bodyWord] -> do
        variable <- value variableWord
        checkVariable p name variable
        contents <- value listWord
        wordsInList <- fromTcl name (parseListAt (wordBodyPosition listWord) contents)
        items <- mapM (\w -> maybe (problem p name "list item could not be decoded") Right (staticWord w)) wordsInList
        unless (wordKind bodyWord == Braced) $
          problem (wordPosition bodyWord) name "only a literal braced loop body is supported"
        body <- fromTcl name (parseScriptAt (wordBodyPosition bodyWord) (wordText bodyWord))
        when (length items > 100000) $ problem p name "static expansion exceeds 100000 iterations"
        foldM (\current (iteration, item) ->
          lowerScript config test (address ++ [iteration])
            current {variables = Map.insert variable item (variables current)} body)
          state (zip [1..] items)
      _ -> problem p name "only one scalar variable, one list, and a braced body are supported"
    "compile_pass" -> values >>= lowerCompile config test address p True state
    "compile_fail" -> values >>= lowerCompile config test address p False state
    _ -> problem p name "unsupported construct; the entire scenario is rejected"

lowerCompile :: PlanConfig -> FilePath -> [Int] -> SourcePos -> Bool -> LowerState
             -> [String] -> Either PlanIssue LowerState
lowerCompile config test address p succeeds state args = do
  (source, rawOptions, nodeps) <- case args of
    [source] -> Right (source, "", "0")
    [source, options] -> Right (source, options, "0")
    [source, options, nodeps] -> Right (source, options, nodeps)
    _ -> problem p helper "expected source, optional options, and optional nodeps"
  unless (validSource source) $ problem p helper
    "only a .bs or .bsv source basename containing letters, digits, '.', '_' or '-' is supported"
  unless (nodeps `elem` ["0", "1"]) $ problem p helper "nodeps must be 0 or 1"
  options <- parseOptions p helper rawOptions
  let directory = takeDirectory test
      identifier role = Identifier test address role
      compileId = identifier "compile"
      workspaceInput = case stepsReversed state of
        [] -> [SuiteDirectory directory]
        previous:_ -> [Produced (stepId previous) "workspace" Nothing]
      dependencies = case stepsReversed state of
        [] -> []
        previous:_ -> [stepId previous]
      compile = Step compileId p
        (BscCompile source (configCompilerOptions config ++ options) (nodeps == "0"))
        (SuiteFile (normalise (directory </> source)) : workspaceInput)
        (outputs (source ++ ".bsc-out")) ["bsc"] dependencies LocalOnly
      assertion = Check (identifier "compile.result") p compileId
        (if succeeds then ToolSucceeds else ToolFails) Ordinary
      object = dropExtension source ++ ".bo"
      loadId = identifier "object-load"
      load = Step loadId p (InternalLoad object)
        [Produced compileId "workspace" Nothing, Produced compileId "workspace" (Just object)]
        (outputs (object ++ ".dumpbo-out")) ["dumpbo"] [compileId] LocalOnly
      internal = Check (identifier "object-load.result") p loadId ToolSucceeds Internal
      includeInternal = succeeds && configInternalChecks config
  Right state
    { stepsReversed = (if includeInternal then [load, compile] else [compile]) ++ stepsReversed state
    , checksReversed = (if includeInternal then [internal, assertion] else [assertion]) ++ checksReversed state
    }
  where
    helper = if succeeds then "compile_pass" else "compile_fail"
    outputs transcript =
      [ Output "workspace" DirectoryArtifact (Just ".")
      , Output "status" ProcessStatus Nothing
      , Output "transcript" Transcript (Just transcript)
      ]

parseOptions :: SourcePos -> String -> String -> Either PlanIssue [String]
parseOptions p helper contents = do
  -- The legacy helper parses its option string again as Tcl. In particular,
  -- shell-word splitting is not equivalent and would miss substitutions.
  -- A trailing separator can disappear when a fragment is parsed alone, but
  -- would split off the flags/source which the harness appends afterward.
  when (any (`elem` ";\r\n") contents) $
    problem p helper "option strings containing command separators are unsupported"
  Script commands <- fromTcl helper (parseScriptAt p ("options " ++ contents))
  options <- case commands of
    [Command _ (_:args)] -> mapM literal args
    _ -> problem p helper "option string contains Tcl command separators"
  mapM_ (checkOption p helper) options
  Right options
  where
    literal w = case staticWord w of
      Just item | wordKind w /= Expanded -> Right item
      _ -> problem (wordPosition w) helper "option string requires substitution or argument expansion"

checkOption :: SourcePos -> String -> String -> Either PlanIssue ()
checkOption p helper option = unless (option `elem` supportedCompilerOptions) $
  problem p helper ("unsupported compiler option: " ++ show option)

checkVariable :: SourcePos -> String -> String -> Either PlanIssue ()
checkVariable p command name = unless (name `elem` ordinaryScalars) $
  problem p command ("unsupported scalar name " ++ show name ++
    "; only audited ordinary scalar variables are supported; harness state cannot be assigned")
  where
    -- DejaGNU sources scripts at global scope. These names were checked against
    -- harness/framework globals; all other names need an explicit audit before
    -- admission, even if their Tcl spelling looks like an ordinary scalar.
    ordinaryScalars =
      [ "source", "sources", "src", "file", "files", "filename", "flags", "flag"
      , "opts", "options", "name", "stem", "extension", "suffix", "variants"
      , "variant", "unused", "x", "y", "i", "j", "command"
      ]

validSource :: String -> Bool
validSource source = not (null source) && head source /= '-' &&
  takeExtension source `elem` [".bs", ".bsv"] && all allowed source &&
  source /= ".bs" && source /= ".bsv"
  where
    allowed c = isAsciiLower c || isAsciiUpper c || isDigit c || c `elem` "._-"

fromTcl :: String -> Either TclError a -> Either PlanIssue a
fromTcl construct = either (\e -> Left (PlanIssue (errorPosition e) construct (errorMessage e))) Right

problem :: SourcePos -> String -> String -> Either PlanIssue a
problem p construct reason = Left (PlanIssue p construct reason)
