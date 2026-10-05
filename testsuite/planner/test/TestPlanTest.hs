module TestPlanTest (runTests) where

import Tcl (SourcePos(..))
import TestPlan
import Control.Monad (forM_, unless)
import Data.Either (isLeft)
import Data.List (isPrefixOf)

runTests :: IO ()
runTests = do
  check "valid mixed plan" (validatePlan fixture == Right ())
  check "version 3 wire contract" (encodePlan simplePlan == simpleEncoding)
  check "decode specified wire contract" (decodePlan simpleEncoding == Right simplePlan)
  check "strict JSON round trip" (decodePlan (encodePlan fixture) == Right fixture)
  check "stable serialization" (fmap encodePlan (decodePlan (encodePlan fixture)) == Right (encodePlan fixture))
  check "planned tests retain source order" (plannedTests fixture == [compilationTest])
  check "issues retain source order" (planIssues fixture == [unsupported, unresolved])
  check "mixed counts" (planCounts fixture == (1, 1, 1))
  let allUnplanned = withItems [Unplanned unsupported,
        Unplanned unresolved { issueId = Just (Identifier script 1) }]
  check "all-unplanned selection is valid" (validatePlan allUnplanned == Right ())
  check "all-unplanned round trip" (decodePlan (encodePlan allUnplanned) == Right allUnplanned)
  check "all-unplanned counts" (planCounts allUnplanned == (0, 1, 1))
  let emptyScript = withItems []
  check "empty script is retained" (decodePlan (encodePlan emptyScript) == Right emptyScript)
  check "empty script counts" (planCounts emptyScript == (0, 0, 0))
  let unnumbered = withItems [Unplanned unsupported, Unplanned unsupported]
  check "unnumbered diagnostics do not collide"
    (validatePlan unnumbered == Right () && decodePlan (encodePlan unnumbered) == Right unnumbered)
  let changedExpectation = compilationTest { testKind = CompilationTest compilation CompileFails }
      shifted = compilationTest { testOrigin = SourcePos script 7 3 52 }
  check "expectation does not identify a test" (testId changedExpectation == testId compilationTest)
  check "source position does not identify a test" (testId shifted == testId compilationTest)
  check "expected failure and shifted origin are valid"
    (validatePlan (withItems [Planned changedExpectation]) == Right ()
     && validatePlan (withItems [Planned shifted]) == Right ())
  check "identity uses version, path length, and invocation number"
    (renderIdentifier (Identifier "a:b.exp" 23) == "v3:7:a:b.exp:23")
  check "loop iterations have distinct identities"
    (renderIdentifier (Identifier script 1) /= renderIdentifier (Identifier script 2))
  let prefixed = fixture { planConfig = PlanConfig "options" False ["-v"] }
      withPrefix = prefixed { planScripts = [ScriptPlan script [Planned compilationTest
        { testKind = CompilationTest compilation { compilationOptions = ["-v", "-p", ".:+"] } CompileSucceeds }]] }
  check "effective options begin with the configuration options" (validatePlan withPrefix == Right ())
  check "missing configuration prefix fails" (isLeft (validatePlan prefixed))
  check "internal checks do not change the semantic test population"
    (plannedTests fixture == plannedTests fixture { planConfig = PlanConfig "test" False [] })
  let absent = compilationTest
        { testKind = CompilationTest compilation { compilationSource = "absent/Negative.bs" } CompileFails }
  check "absent sources remain representable" (validatePlan (withItems [Planned absent]) == Right ())
  let errorKind = CompilationErrorTest compilation (ExpectedError "T0001" 1)
      errorPlan = withItems [Planned compilationTest { testKind = errorKind }]
      errorEncoding = replaceFirst "\"kind\":\"compilation\"" "\"kind\":\"compilation-error\""
        (replaceFirst "\"expectation\":\"succeeds\"" "\"error_tag\":\"T0001\",\"error_count\":1" simpleEncoding)
  check "diagnostic kind has an explicit version 3 wire contract"
    (encodePlan errorPlan == errorEncoding && decodePlan errorEncoding == Right errorPlan)
  check "both test kinds expose their compilation"
    (testCompilation errorKind == compilation && testCompilation (testKind compilationTest) == compilation)
  forM_ [0, 1, maxBound] $ \count -> do
    let plan = withItems [Planned compilationTest
          { testKind = CompilationErrorTest compilation (ExpectedError "a_B-09" count) }]
    check "nonnegative Int error counts round trip" (decodePlan (encodePlan plan) == Right plan)
  forM_ ["", "T.*", "T[0-9]", "1T", "_T", "Té", "éT", "T 1"] $ \tag ->
    check "diagnostic tags reject regex and non-ASCII identifiers"
      (isLeft (validatePlan (withItems [Planned compilationTest
        { testKind = CompilationErrorTest compilation (ExpectedError tag 1) }])))
  check "negative error counts are invalid"
    (isLeft (validatePlan (withItems [Planned compilationTest
      { testKind = CompilationErrorTest compilation (ExpectedError "T0001" (-1)) }])))
  forM_ [("\"error_count\":1", "\"error_count\":-1"),
         ("\"error_count\":1", "\"error_count\":999999999999999999999999"),
         ("\"error_count\":1", "\"error_count\":\"1\""),
         ("\"error_count\":1", "\"error_count\":1,\"expectation\":\"fails\""),
         ("\"error_tag\":\"T0001\"", "\"error_tag\":\"T.*\"")]
    $ \(old, new) -> check "invalid diagnostic wire fields are rejected"
      (isLeft (decodePlan (replaceFirst old new errorEncoding)))
  forM_ malformed $ \(label, content) -> check label (isLeft (decodePlan content))
  forM_ invalidPlans $ \(label, plan) -> check label (isLeft (validatePlan plan))
  forM_ invalidConfigs $ \config -> check "invalid configuration" (isLeft (validateConfig config))
  putStrLn "TestPlan semantic model, strict version 3 codec, identities, and accounting tests passed."

check :: String -> Bool -> IO ()
check label ok = unless ok (ioError (userError ("TestPlanTest: " ++ label)))

script :: FilePath
script = "bsc.plan/example.exp"
origin :: SourcePos
origin = SourcePos script 3 1 12
compilation :: Compilation
compilation = Compilation "Example.bs" [] True
compilationTest :: Test
compilationTest = Test (Identifier script 1) origin (CompilationTest compilation CompileSucceeds)
unsupported, unresolved :: PlanIssue
unsupported = PlanIssue Nothing origin "exec" UnsupportedConstruct "unsupported command"
unresolved = PlanIssue (Just (Identifier script 2)) origin "compile_pass" UnresolvedDependency "source argument is not static"
fixture, simplePlan :: TestPlan
fixture = (withItems [Planned compilationTest, Unplanned unsupported, Unplanned unresolved])
  { planScripts = [ScriptPlan script [Planned compilationTest, Unplanned unsupported, Unplanned unresolved],
                   ScriptPlan "bsc.plan/empty.exp" []] }
simplePlan = withItems [Planned compilationTest]
withItems :: [PlanItem] -> TestPlan
withItems items = TestPlan (PlanConfig "test" True []) [ScriptPlan script items]

simpleEncoding :: String
simpleEncoding = "{\"schema\":\"bsc-testsuite-test-plan\",\"version\":3,\"identity\":\"file-test-number-v1\","
  ++ "\"configuration\":{\"name\":\"test\",\"internal_checks\":true,\"compiler_options\":[]},"
  ++ "\"scripts\":[{\"path\":\"bsc.plan/example.exp\",\"items\":[{\"status\":\"planned\","
  ++ "\"id\":{\"test\":\"bsc.plan/example.exp\",\"number\":1},"
  ++ "\"origin\":{\"file\":\"bsc.plan/example.exp\",\"line\":3,\"column\":1,\"offset\":12},"
  ++ "\"kind\":{\"kind\":\"compilation\",\"source\":\"Example.bs\",\"options\":[],"
  ++ "\"compile_dependencies\":true,\"expectation\":\"succeeds\"}}]}]}\n"

malformed :: [(String, String)]
malformed =
  [ ("unknown root field", replace "{\"schema\":" "{\"unknown\":0,\"schema\":")
  , ("duplicate root field", replace "{\"schema\":" "{\"version\":3,\"schema\":")
  , ("unknown config field", replace "\"name\":\"test\"" "\"name\":\"test\",\"unknown\":0")
  , ("duplicate config field", replace "\"name\":\"test\"" "\"name\":\"test\",\"name\":\"test\"")
  , ("unknown script field", replace "\"items\":[" "\"unknown\":0,\"items\":[")
  , ("unknown item field", replace "\"status\":\"planned\"" "\"status\":\"planned\",\"unknown\":0")
  , ("duplicate item discriminator", replace "\"status\":\"planned\"" "\"status\":\"planned\",\"status\":\"planned\"")
  , ("unknown identity field", replace "\"number\":1" "\"number\":1,\"role\":\"compile\"")
  , ("duplicate identity field", replace "\"number\":1" "\"number\":1,\"number\":1")
  , ("unknown origin field", replace "\"offset\":12" "\"offset\":12,\"unknown\":0")
  , ("unknown compilation field", replace "\"compile_dependencies\":true" "\"compile_dependencies\":true,\"unknown\":0")
  , ("duplicate compilation discriminator", replace "\"kind\":\"compilation\"" "\"kind\":\"compilation\",\"kind\":\"compilation\"")
  , ("unknown issue field", replace "\"construct\":\"exec\"" "\"construct\":\"exec\",\"unknown\":0")
  , ("missing required field", replace "\"compile_dependencies\":true," "")
  , ("old version rejected", replace "\"version\":3" "\"version\":1")
  , ("source-site version rejected", replace "\"version\":3" "\"version\":2")
  , ("future version rejected", replace "\"version\":3" "\"version\":4")
  , ("wrong schema rejected", replace "bsc-testsuite-test-plan" "other-schema")
  , ("unsupported identity", replace "file-test-number-v1" "source-site-check-v1")
  , ("unsupported test kind", replace "\"kind\":\"compilation\"" "\"kind\":\"execute-shell\"")
  , ("unsupported item status", replace "\"status\":\"planned\"" "\"status\":\"ignored\"")
  , ("unsupported expectation", replace "\"expectation\":\"succeeds\"" "\"expectation\":\"anything\"")
  , ("wrong field type", replace "\"internal_checks\":true" "\"internal_checks\":1")
  , ("integer overflow", replace "\"line\":3" "\"line\":9999999999999999999999999")
  , ("negative invocation number", replace "\"number\":1" "\"number\":-1")
  , ("zero invocation number", replace "\"number\":1" "\"number\":0")
  , ("noninteger invocation number", replace "\"number\":1" "\"number\":[]")
  , ("null planned identifier", replace "\"id\":{\"test\":\"bsc.plan/example.exp\",\"number\":1}" "\"id\":null")
  , ("fractional integer", replace "\"line\":3" "\"line\":3.0")
  , ("leading zero integer", replace "\"line\":3" "\"line\":03")
  , ("trailing JSON", encoded ++ "{}")
  , ("trailing comma", replace "\"compiler_options\":[]}" "\"compiler_options\":[],}")
  , ("invalid Unicode escape", replace "\"name\":\"test\"" "\"name\":\"\\ud800\"")
  , ("control character option", replace "\"options\":[]" "\"options\":[\"bad\\u0000option\"]")
  ]
  where
    encoded = encodePlan fixture
    replace old new = replaceFirst old new encoded

invalidPlans :: [(String, TestPlan)]
invalidPlans =
  [ ("empty selection", fixture { planScripts = [] })
  , ("duplicate script", fixture { planScripts = replicate 2 (ScriptPlan script []) })
  , ("duplicate test ID", withItems (replicate 2 (Planned compilationTest)))
  , ("duplicate issue ID", withItems (replicate 2 (Unplanned unresolved)))
  , ("test and issue ID collision", withItems [Planned compilationTest,
       Unplanned unsupported { issueId = Just (testId compilationTest) }])
  , ("first invocation must be one", testPlan compilationTest { testId = Identifier script 2 })
  , ("gaps across numbered issues are rejected", withItems [Planned compilationTest,
       Unplanned unresolved { issueId = Just (Identifier script 3) }])
  , ("invocation order must match source order", withItems [Unplanned unresolved, Planned compilationTest])
  , ("script traversal", fixture { planScripts = [ScriptPlan "../example.exp" []] })
  , ("absolute script", fixture { planScripts = [ScriptPlan "/example.exp" []] })
  , ("noncanonical script", fixture { planScripts = [ScriptPlan "./example.exp" []] })
  , ("wrong script suffix", fixture { planScripts = [ScriptPlan "example.bs" []] })
  , ("backslash script", fixture { planScripts = [ScriptPlan "a\\example.exp" []] })
  , ("different script identity", testPlan compilationTest { testId = Identifier "another.exp" 1 })
  , ("negative invocation number", testPlan compilationTest { testId = Identifier script (-1) })
  , ("zero invocation number", testPlan compilationTest { testId = Identifier script 0 })
  , ("origin mismatch", testPlan compilationTest { testOrigin = SourcePos "another.exp" 1 1 0 })
  , ("zero origin line", testPlan compilationTest { testOrigin = SourcePos script 0 1 0 })
  , ("zero origin column", testPlan compilationTest { testOrigin = SourcePos script 1 0 0 })
  , ("negative origin offset", testPlan compilationTest { testOrigin = SourcePos script 1 1 (-1) })
  , ("invalid issue origin", withItems [Unplanned unsupported { issuePosition = SourcePos script 0 1 0 }])
  , ("empty issue reason", withItems [Unplanned unsupported { issueReason = "" }])
  , ("empty issue construct", withItems [Unplanned unsupported { issueConstruct = "" }])
  , ("source traversal", source "../Outside.bs")
  , ("noncanonical source", source "./Example.bs")
  , ("absolute source", source "/Example.bs")
  , ("source suffix", source "Example.bo")
  , ("empty compilation option", testPlan compilationTest
        { testKind = CompilationTest compilation { compilationOptions = [""] } CompileSucceeds })
  , ("configuration prefix appears out of order", (testPlan compilationTest
        { testKind = CompilationTest compilation { compilationOptions = ["-g", "-v"] } CompileSucceeds })
        { planConfig = PlanConfig "options" True ["-v"] })
  ]
  where
    testPlan test = withItems [Planned test]
    source path = testPlan compilationTest
      { testKind = CompilationTest compilation { compilationSource = path } CompileSucceeds }

invalidConfigs :: [PlanConfig]
invalidConfigs = [PlanConfig "" True [], PlanConfig "bad\nname" True [],
  PlanConfig "test" True [""], PlanConfig "test" True ["bad\NULoption"]]

replaceFirst :: String -> String -> String -> String
replaceFirst old new input
  | old `isPrefixOf` input = new ++ drop (length old) input
  | c : rest <- input = c : replaceFirst old new rest
  | otherwise = error ("test replacement did not match: " ++ old)
