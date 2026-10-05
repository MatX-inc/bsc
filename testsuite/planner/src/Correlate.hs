-- | Match the supported part of a semantic plan to observed DejaGnu calls.
--
-- Both sides count only the agreed semantic procedures. The number is a join
-- key within one script, not proof of identity: source position and resolved
-- arguments must agree as well. Unsupported numbered calls remain visible as
-- skips. This establishes correspondence for the current vocabulary, not full
-- suite coverage, tool/input equivalence, or a Buck2 execution backend.
module Correlate
  ( Provenance(..), Invocation(..), Observation(..), Correlation(..)
  , decodeTestLog, correlatePlan, renderCorrelation
  ) where

import Control.Monad (foldM, unless)
import Data.List (intercalate, isPrefixOf, sort, stripPrefix)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing, mapMaybe)
import Text.Read (readMaybe)
import Lower (lowerInvocation)
import Procedures (internalChecksAfter, supportedProcedures)
import Tcl (SourcePos(..), TclError(..), parseListAt, staticWord)
import TestPlan

data Observation = Observation
  { observationRole :: String, observationDisposition :: String
  , observationMessage :: String
  } deriving (Eq, Show)
data Invocation = Invocation
  { invocationNumber :: Int, invocationProcedure :: String
  , invocationFile :: FilePath, invocationLine :: Int
  , invocationArguments :: [String], invocationResults :: [Observation]
  } deriving (Eq, Show)
data Provenance = Provenance
  { provenanceTest :: FilePath, provenanceInternalChecks :: Bool
  , provenanceProcedures :: [String], provenanceInvocations :: [Invocation]
  } deriving (Eq, Show)
data Correlation = Correlation
  { correlationMatches :: [(Test, [Observation])]
  , correlationSkips :: [PlanIssue]
  , correlationProblems :: [String]
  } deriving (Eq, Show)

ensure :: Bool -> String -> Either String ()
ensure condition message = unless condition (Left message)

-- The ordinary log has one active script and at most one active invocation.
-- Completed calls/scripts are accumulated in reverse to preserve linear work.
data ScriptCapture = ScriptCapture
  { captureTest :: FilePath, captureInternal :: Bool
  , captureCount :: Int, captureCalls :: [Invocation]
  , captureActive :: Maybe (Invocation, String)
  }
data LogCapture = LogCapture
  { captureScripts :: [Provenance], captureScript :: Maybe ScriptCapture
  , captureSawMarker :: Bool
  }

-- | Read ordinary testrun.log output. Only BSC-TEST metadata is parsed as Tcl
-- list data; no substitutions or commands are evaluated. The current protocol
-- supports single-line verdict messages: each recognized disposition line is
-- one complete observation, and ordinary following lines are not continuations.
decodeTestLog :: FilePath -> String -> Either String [Provenance]
decodeTestLog path input = do
  final <- foldM line (LogCapture [] Nothing False) (zip [1..] (lines input))
  ensure (captureSawMarker final) (path ++ ": unobserved test log: no BSC-TEST markers")
  case captureScript final of
    Just script -> Left (path ++ ": incomplete test log: " ++
      if isNothing (captureActive script) then "missing script finish" else "missing invocation end and script finish")
    Nothing -> pure (reverse (captureScripts final))
  where
    line state (lineNumber, text) = atLine lineNumber $ case stripPrefix "BSC-TEST: " text of
      Just metadata -> do
        values <- list lineNumber metadata
        marker lineNumber state { captureSawMarker = True } values
      Nothing -> case captureScript state of
        Just _ | "ERROR:" `isPrefixOf` text -> Left ("test script reported " ++ text)
        Just script | Just (call, role) <- captureActive script,
                      Just (disposition, message) <- verdict text ->
          let result = Observation role disposition message
              updated = call { invocationResults = result : invocationResults call }
          in pure state { captureScript = Just script { captureActive = Just (updated, role) } }
        _ -> pure state
    atLine lineNumber = either
      (Left . ((path ++ ":" ++ show lineNumber ++ ": ") ++)) Right
    list lineNumber value = case parseListAt (SourcePos path lineNumber 1 0) value of
      Left err -> Left (errorMessage err)
      Right wordsInList -> mapM (maybe (Left "nonliteral BSC-TEST list value") Right . staticWord) wordsInList
    marker _ _ ("unsupported":reason) =
      Left ("unsupported BSC-TEST metadata: " ++ unwords reason)
    marker _ state ["script", version, test, internalText] = do
      ensure (version == "1") ("unsupported BSC-TEST version: " ++ version)
      ensure (isNothing (captureScript state)) "new script before previous script finish"
      internal <- boolean internalText
      validatePlan (TestPlan (PlanConfig "test-log" internal []) [ScriptPlan test []])
      pure state { captureScript = Just (ScriptCapture test internal 0 [] Nothing) }
    marker lineNumber state ["begin", numberText, procedure, file, lineText, argsText] = do
      script <- activeScript state
      ensure (isNothing (captureActive script)) "nested invocation begin"
      number <- natural numberText
      ensure (number == captureCount script + 1) "noncontiguous test invocation numbers"
      ensure (procedure `elem` supportedProcedures) "uncounted procedure appears as a test invocation"
      originLine <- natural lineText
      ensure (originLine > 0) "invalid invocation source line"
      ensure (not (null file)) "empty invocation source file"
      arguments <- list lineNumber argsText
      ensure (all (not . any (`elem` "\r\n")) (file : arguments)) "unsupported multiline metadata"
      let call = Invocation number procedure file originLine arguments []
      pure state { captureScript = Just script { captureActive = Just (call, "compilation") } }
    marker _ state ["role", numberText, nextRole]
      | nextRole `elem` ["object-load", "diagnostic-count"] = do
          script <- activeScript state
          (call, role) <- activeInvocation script
          number <- natural numberText
          ensure (number == invocationNumber call) "role number differs from active invocation"
          ensure (role == "compilation") "repeated or conflicting role marker"
          whenDiagnostic nextRole $ do
            ensure (invocationProcedure call == "compile_fail_error")
              "diagnostic-count role requires compile_fail_error"
            ensure (null (invocationResults call)) "diagnostic-count follows a compilation verdict"
          pure state { captureScript = Just script { captureActive = Just (call, nextRole) } }
    marker _ state ["end", numberText] = do
      script <- activeScript state
      (call, _) <- activeInvocation script
      number <- natural numberText
      ensure (number == invocationNumber call) "end number differs from active invocation"
      let completed = call { invocationResults = reverse (invocationResults call) }
      pure state { captureScript = Just script
        { captureCount = number, captureCalls = completed : captureCalls script
        , captureActive = Nothing } }
    marker _ state ["finish", countText] = do
      script <- activeScript state
      ensure (isNothing (captureActive script)) "script finish before invocation end"
      count <- natural countText
      ensure (count == captureCount script) "script finish count disagrees with invocations"
      let completed = Provenance (captureTest script) (captureInternal script)
                        supportedProcedures (reverse (captureCalls script))
      pure state { captureScripts = completed : captureScripts state, captureScript = Nothing }
    marker _ _ values = Left ("unknown or malformed BSC-TEST marker: " ++ show values)
    activeScript state = maybe (Left "BSC-TEST marker outside an active script") Right (captureScript state)
    activeInvocation script = maybe (Left "BSC-TEST marker outside an active invocation") Right (captureActive script)
    whenDiagnostic role action = if role == "diagnostic-count" then action else Right ()

-- Remove the disposition's separator, preserving the rest of the message
-- exactly, including further leading spaces and trailing spaces.
verdict :: String -> Maybe (String, String)
verdict text = case [(disposition, message) | disposition <- dispositions,
                      Just message <- [stripPrefix (disposition ++ ":") text]] of
  (disposition, message):_ -> Just (disposition, case message of ' ':rest -> rest; _ -> message)
  [] -> Nothing
  where
    dispositions = ["PASS", "FAIL", "XPASS", "XFAIL", "KPASS", "KFAIL",
                    "UNRESOLVED", "UNTESTED", "UNSUPPORTED"]

natural :: String -> Either String Int
natural value = case readMaybe value :: Maybe Integer of
  Just number | not (null value), all (`elem` ['0'..'9']) value,
                number >= 0, number <= toInteger (maxBound :: Int) -> Right (fromInteger number)
  _ -> Left ("invalid provenance integer: " ++ show value)

boolean :: String -> Either String Bool
boolean "0" = Right False
boolean "1" = Right True
boolean value = Left ("invalid provenance boolean: " ++ show value)

-- Incomplete or malformed test logs are rejected by decodeTestLog.
-- Correspondence problems within complete files accumulate, so one mismatch
-- cannot hide the rest of the selected population.
correlatePlan :: TestPlan -> [Provenance] -> Either String Correlation
correlatePlan plan traces = do
  validatePlan plan
  ensure (null (configCompilerOptions (planConfig plan)))
    "cannot correlate global compiler options: current provenance does not record global compiler options"
  ensure (Map.size byScript == length traces) "multiple provenance logs for one script"
  let selected = map scriptPath (planScripts plan)
      extra = ["unselected provenance script: " ++ show (provenanceTest trace)
              | trace <- traces, provenanceTest trace `notElem` selected]
  pieces <- mapM correlateScript (planScripts plan)
  pure (foldl combine (Correlation [] [] extra) pieces)
  where
    config = planConfig plan
    byScript = Map.fromList [(provenanceTest trace, trace) | trace <- traces]
    combine a b = Correlation
      (correlationMatches a ++ correlationMatches b)
      (correlationSkips a ++ correlationSkips b)
      (correlationProblems a ++ correlationProblems b)
    numbered (Planned test) = Just (testId test)
    numbered (Unplanned issue) = issueId issue
    correlateScript script = case Map.lookup (scriptPath script) byScript of
      Nothing -> pure (Correlation [] []
        ["missing provenance for " ++ renderIdentifier identifier
        | identifier <- mapMaybe numbered (scriptItems script)])
      Just trace -> do
        let calls = Map.fromList [(invocationNumber call, call) | call <- provenanceInvocations trace]
            identifiers = mapMaybe numbered (scriptItems script)
            expectedNumbers = map identifierNumber identifiers
            headerProblems =
              [scriptPath script ++ ": internal-check configuration differs"
               | provenanceInternalChecks trace /= configInternalChecks config] ++
              [scriptPath script ++ ": supported-procedure set differs"
               | provenanceProcedures trace /= supportedProcedures]
            extraCalls = ["extra invocation " ++ renderIdentifier (Identifier (scriptPath script) number)
                         | number <- Map.keys calls, number `notElem` expectedNumbers]
        if not (null headerProblems)
          then pure (Correlation [] [] headerProblems)
          else do
            pieces <- mapM (correlateItem calls) (scriptItems script)
            pure (foldl combine (Correlation [] [] extraCalls) pieces)
    correlateItem _ (Unplanned issue) | Nothing <- issueId issue = pure (Correlation [] [] [])
    correlateItem calls item = case numbered item of
      Nothing -> pure (Correlation [] [] [])
      Just identifier -> case Map.lookup (identifierNumber identifier) calls of
        Nothing -> pure (Correlation [] [] ["missing invocation " ++ renderIdentifier identifier])
        Just call -> case item of
          Unplanned issue -> pure (Correlation [] [issue] [])
          Planned test -> correlateTest test call
    correlateTest test call = do
      let results = invocationResults call
          unexpectedSuccess = case (testKind test, results) of
            (CompilationErrorTest _ _, result:_) -> observationRole result == "compilation"
            _ -> False
      checks <- internalChecksAfter config (testKind test) unexpectedSuccess
      let identifier = testId test
          label = renderIdentifier identifier
          expectedProcedure = case testKind test of
            CompilationTest _ CompileSucceeds -> "compile_pass"
            CompilationTest _ CompileFails -> "compile_fail"
            CompilationErrorTest _ _ -> "compile_fail_error"
          primaryRole = case testKind test of
            CompilationErrorTest _ _ | not unexpectedSuccess -> "diagnostic-count"
            _ -> "compilation"
          expectedRoles = primaryRole : ["object-load" | _ <- checks]
          roles = map observationRole results
          actual = lowerInvocation config identifier (testOrigin test)
                     (invocationProcedure call) (invocationArguments call)
          anchorProblems =
            [label ++ ": source location differs (observed " ++ show (invocationFile call) ++
              ":" ++ show (invocationLine call) ++ ")"
             | invocationFile call /= sourceFile (testOrigin test) ||
               invocationLine call /= sourceLine (testOrigin test)] ++
            [label ++ ": procedure differs: " ++ invocationProcedure call
             | invocationProcedure call /= expectedProcedure] ++
            (case actual of
              Left problem -> [label ++ ": observed arguments cannot be interpreted: " ++ problem]
              Right translated -> [label ++ ": resolved arguments differ from the plan"
                                  | testKind translated /= testKind test]) ++
            [label ++ ": result roles differ: expected " ++ show expectedRoles ++ ", observed " ++ show roles
             | roles /= expectedRoles] ++
            [label ++ ": unexpected compilation success must report FAIL"
             | unexpectedSuccess, result:_ <- [results], observationDisposition result /= "FAIL"]
          resultProblems = [label ++ ": " ++ observationRole result ++ " reported " ++
              observationDisposition result ++ ": " ++ show (observationMessage result)
              | result <- results, observationDisposition result /= "PASS"]
      pure (Correlation [(test, results) | null anchorProblems] [] (anchorProblems ++ resultProblems))

renderCorrelation :: Correlation -> String
renderCorrelation report = unlines $
  ["Correspondence: " ++ show (length matches) ++ " matched tests, " ++
   show (length skips) ++ " skipped numbered tests, " ++ show (length problems) ++ " problems."] ++
  ["MATCH " ++ renderIdentifier (testId test) ++ " " ++
     intercalate ", " [observationRole result ++ "=" ++ observationDisposition result | result <- results]
   | (test, results) <- matches] ++
  ["SKIP " ++ renderIdentifier identifier ++ ": " ++ issueReason issue
   | issue <- skips, Just identifier <- [issueId issue]] ++
  ["PROBLEM " ++ problem | problem <- sort problems]
  where
    matches = correlationMatches report
    skips = correlationSkips report
    problems = correlationProblems report
