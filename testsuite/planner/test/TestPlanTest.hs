module TestPlanTest (runTests) where

import BscTestsuite.Tcl (SourcePos(..))
import BscTestsuite.TestPlan
import Control.Exception (evaluate)
import Control.Monad (forM_, unless)
import Data.Either (isLeft)
import Data.List (isInfixOf, isPrefixOf)
import System.Timeout (timeout)

runTests :: IO ()
runTests = do
  check "valid plan" (validatePlan fixture == Right ())
  check "strict JSON round trip" (decodePlan (encodePlan fixture) == Right fixture)
  check "stable serialization" (fmap encodePlan (decodePlan (encodePlan fixture)) == Right (encodePlan fixture))
  check "empty source scenario is retained"
    (decodePlan (encodePlan (withScenario (Scenario test [] []))) == Right (withScenario (Scenario test [] [])))
  let c = head (scenarioChecks scenario)
      changedResult = scenario { scenarioChecks = [c { checkExpectation = ToolFails }, last (scenarioChecks scenario)] }
  check "expected outcome does not identify check"
    (map checkId (scenarioChecks changedResult) == map checkId (scenarioChecks scenario))
  check "failed compile expectation does not gate dependent internal check"
    (validatePlan (withScenario changedResult) == Right ())
  let rootId = Identifier "example.exp" [1] "compile"
      rootOrigin = SourcePos "example.exp" 1 1 0
      rootStep = compileStep { stepId = rootId, stepOrigin = rootOrigin,
        stepInputs = [SuiteDirectory ".", SuiteFile "Example.bs"] }
      rootCheck = c { checkId = Identifier "example.exp" [1] "compile-result",
                      checkOrigin = rootOrigin, checkProducer = rootId }
  check "suite-root script uses a canonical source input"
    (validatePlan (withScenario (Scenario "example.exp" [rootStep] [rootCheck])) == Right ())
  let a = Identifier "bsc.plan/a:b.exp" [1, 1] "compile"
      b = Identifier "bsc.plan/a.exp" [1, 1, 1] "compile"
  check "delimited identifiers distinguish paths and coordinates" (renderIdentifier a /= renderIdentifier b)
  check "distinct duplicate loop iteration coordinates"
    (renderIdentifier (Identifier test [2, 1, 1] "compile") /= renderIdentifier (Identifier test [2, 2, 1] "compile"))
  let shifted = scenario { scenarioSteps = map (\s -> s { stepOrigin = shiftedOrigin }) (scenarioSteps scenario),
                          scenarioChecks = map (\x -> x { checkOrigin = shiftedOrigin }) (scenarioChecks scenario) }
  check "source location changes preserve identities"
    (map stepId (scenarioSteps shifted) == map stepId (scenarioSteps scenario)
     && map checkId (scenarioChecks shifted) == map checkId (scenarioChecks scenario))
  check "updated locations remain valid" (validatePlan (withScenario shifted) == Right ())
  forM_ malformed $ \(label, content) -> check label (isLeft (decodePlan content))
  forM_ invalidPlans $ \(label, plan) -> check label (isLeft (validatePlan plan))
  let weak = withScenario scenario { scenarioSteps = [compileStep { stepCacheability = Never },
                                                     internalStep { stepCacheability = Cacheable }] }
  case normalizeCacheability weak of
    Left err -> error err
    Right plan -> do
      check "never propagates" (map stepCacheability (scenarioSteps (head (planScenarios plan))) == [Never, Never])
      check "normalized graph validates" (validatePlan plan == Right ())
  let local = withScenario scenario { scenarioSteps = [compileStep, internalStep { stepCacheability = Cacheable }] }
  check "local-only propagates" (fmap (map stepCacheability . scenarioSteps . head . planScenarios)
    (normalizeCacheability local) == Right [LocalOnly, LocalOnly])
  let third = compileStep { stepId = Identifier test [2] "compile",
        stepInputs = [Produced internalId "workspace" Nothing, SuiteFile "bsc.plan/Example.bs"],
        stepDependsOn = [internalId], stepCacheability = Cacheable }
      transitive = withScenario scenario { scenarioSteps =
        [compileStep { stepCacheability = Never }, internalStep { stepCacheability = Cacheable }, third] }
  check "never propagates through multiple dependency levels"
    (fmap (map stepCacheability . scenarioSteps . head . planScenarios) (normalizeCacheability transitive)
      == Right [Never, Never, Never])
  case explainCheck fixture (renderIdentifier (checkId (last (scenarioChecks scenario)))) of
    Left err -> error err
    Right explanation -> forM_ ["dumpbo", "bsc", "local-only", "tool-succeeds", "internal", "Input:",
                               "Output:", "regardless of exit status", "bsc.plan/example.exp:3:1"] $ \fragment ->
      check ("explanation includes " ++ fragment) (fragment `isInfixOf` explanation)
  check "unknown explanation selector fails" (isLeft (explainCheck fixture "unknown"))
  sharedPrerequisiteTest
  putStrLn "TestPlan strict codec, identities, workspace graph, and cache-policy tests passed."

-- Repeatedly traversing each path in this DAG would take exponential time.
-- The explanation must visit each prerequisite once, in source order.
sharedPrerequisiteTest :: IO ()
sharedPrerequisiteTest = do
  let count = 40
      identifier n = Identifier test [n] "compile"
      makeStep n = compileStep
        { stepId = identifier n
        , stepDependsOn = map identifier (filter (> 0) [n - 1, n - 2])
        , stepInputs = SuiteFile "bsc.plan/Example.bs" :
            if n == 1 then [SuiteDirectory "bsc.plan"]
            else [Produced (identifier (n - 1)) "workspace" Nothing]
        , stepCacheability = Never
        }
      steps = map makeStep [1 .. count]
      finalCheck = Check (Identifier test [count] "compile-result") origin
        (identifier count) ToolSucceeds Ordinary
      plan = withScenario (Scenario test steps [finalCheck])
  result <- timeout 2000000 $ case explainCheck plan (renderIdentifier (checkId finalCheck)) of
    Left err -> ioError (userError err)
    Right explanation -> evaluate (length explanation) >> pure explanation
  case result of
    Nothing -> check "shared-prerequisite explanation finishes within two seconds" False
    Just explanation -> check "shared prerequisites appear exactly once in source order"
      (filter ("Step " `isPrefixOf`) (lines explanation)
       == ["Step " ++ renderIdentifier (identifier n) | n <- [1 .. count]])

check :: String -> Bool -> IO ()
check label ok = unless ok (ioError (userError ("TestPlanTest: " ++ label)))

test :: FilePath
test = "bsc.plan/example.exp"
origin, shiftedOrigin :: SourcePos
origin = SourcePos test 3 1 12
shiftedOrigin = SourcePos test 7 3 52
compileId, internalId :: Identifier
compileId = Identifier test [1] "compile"
internalId = Identifier test [1] "load-object"
outputs :: FilePath -> [Output]
outputs transcript = [Output "workspace" DirectoryArtifact (Just "."),
  Output "status" ProcessStatus Nothing, Output "transcript" Transcript (Just transcript)]
compileStep, internalStep :: Step
compileStep = Step compileId origin (BscCompile "Example.bs" [] True)
  [SuiteDirectory "bsc.plan", SuiteFile "bsc.plan/Example.bs"] (outputs "Example.bs.bsc-out") ["bsc"] [] LocalOnly
internalStep = Step internalId origin (InternalLoad "Example.bo")
  [Produced compileId "workspace" Nothing, Produced compileId "workspace" (Just "Example.bo")]
  (outputs "Example.bo.dumpbo-out") ["dumpbo"] [compileId] LocalOnly
scenario :: Scenario
scenario = Scenario test [compileStep, internalStep]
  [Check (Identifier test [1] "compile-result") origin compileId ToolSucceeds Ordinary,
   Check (Identifier test [1] "load-result") origin internalId ToolSucceeds Internal]
fixture :: TestPlan
fixture = withScenario scenario
withScenario :: Scenario -> TestPlan
withScenario s = TestPlan (PlanConfig "test" True []) [s]

malformed :: [(String, String)]
malformed =
  [ ("unknown root field", replaceFirst "{\"schema\":" "{\"unknown\":0,\"schema\":" encoded)
  , ("duplicate root field", replaceFirst "{\"schema\":" "{\"version\":1,\"schema\":" encoded)
  , ("unknown nested field", replaceFirst "\"name\":\"test\"" "\"name\":\"test\",\"unknown\":0" encoded)
  , ("duplicate nested field", replaceFirst "\"name\":\"test\"" "\"name\":\"test\",\"name\":\"test\"" encoded)
  , ("unsupported version", replaceFirst "\"version\":1" "\"version\":2" encoded)
  , ("unsupported identity", replaceFirst "source-site-check-v1" "future-id" encoded)
  , ("conditional dependency policy rejected", replaceFirst "\"completion\"" "\"success\"" encoded)
  , ("unsupported operation", replaceFirst "bsc-compile" "execute-shell" encoded)
  , ("wrong field type", replaceFirst "\"internal_checks\":true" "\"internal_checks\":1" encoded)
  , ("integer overflow", replaceFirst "\"line\":3" "\"line\":9999999999999999999999999" encoded)
  , ("negative coordinate", replaceFirst "\"site\":[1]" "\"site\":[-1]" encoded)
  , ("zero coordinate", replaceFirst "\"site\":[1]" "\"site\":[0]" encoded)
  , ("trailing JSON", encoded ++ "{}")
  , ("invalid Unicode escape", replaceFirst "\"name\":\"test\"" "\"name\":\"\\ud800\"" encoded)
  ]
  where encoded = encodePlan fixture

invalidPlans :: [(String, TestPlan)]
invalidPlans =
  [ ("empty selection", fixture { planScenarios = [] })
  , ("duplicate scenario", fixture { planScenarios = [scenario, scenario] })
  , ("duplicate check ID", withScenario scenario { scenarioChecks = replicate 2 (head (scenarioChecks scenario)) })
  , ("step/check ID collision", checks [firstCheck { checkId = compileId }])
  , ("missing check producer", checks [firstCheck { checkProducer = Identifier test [99] "missing" }])
  , ("forward dependency", steps [compileStep { stepDependsOn = [internalId] }, internalStep])
  , ("self dependency", steps [compileStep { stepDependsOn = [compileId] }, internalStep])
  , ("missing dependency", steps [compileStep, internalStep { stepDependsOn = [] }])
  , ("missing output reference", steps [compileStep, internalStep { stepInputs =
      [Produced compileId "missing" Nothing] }])
  , ("invalid output reference kind", steps [compileStep, internalStep { stepInputs =
      stepInputs internalStep ++ [Produced compileId "transcript" (Just "nested")] }])
  , ("duplicate output", steps [compileStep { stepOutputs = head (stepOutputs compileStep) : stepOutputs compileStep }, internalStep])
  , ("missing workspace snapshot", steps [compileStep, internalStep { stepInputs = [Produced compileId "workspace" (Just "Example.bo")] }])
  , ("wrong initial workspace", steps [compileStep { stepInputs = [SuiteDirectory "another", SuiteFile "bsc.plan/Example.bs"] }, internalStep])
  , ("input traversal", steps [compileStep { stepInputs = stepInputs compileStep ++ [SuiteFile "../outside"] }, internalStep])
  , ("output traversal", steps [compileStep { stepOutputs = stepOutputs compileStep ++ [Output "extra" FileArtifact (Just "../outside")] }, internalStep])
  , ("absolute input", steps [compileStep { stepInputs = stepInputs compileStep ++ [SuiteFile "/outside"] }, internalStep])
  , ("origin mismatch", checks [firstCheck { checkOrigin = SourcePos "elsewhere.exp" 1 1 0 }])
  , ("site mismatch", checks [firstCheck { checkId = Identifier test [9] "compile-result" }])
  , ("wrong assertion class", checks [firstCheck { checkClass = Internal }])
  , ("disabled internal checks", fixture { planConfig = PlanConfig "test" False [] })
  , ("cache restriction weakened", steps [compileStep { stepCacheability = Never }, internalStep])
  , ("invalid compiler configuration", fixture { planConfig = PlanConfig "test" True ["bad\NULoption"] })
  , ("configuration missing from effective compile options", fixture { planConfig = PlanConfig "test" True ["-v"] })
  ]
  where
    steps xs = withScenario scenario { scenarioSteps = xs }
    checks xs = withScenario scenario { scenarioChecks = xs }
    firstCheck = head (scenarioChecks scenario)

replaceFirst :: String -> String -> String -> String
replaceFirst old new input
  | old `isPrefixOf` input = new ++ drop (length old) input
  | c : rest <- input = c : replaceFirst old new rest
  | otherwise = error ("test replacement did not match: " ++ old)
