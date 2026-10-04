module LowerTest (runTests) where

import BscTestsuite.Lower
import BscTestsuite.Tcl (SourcePos(..))
import BscTestsuite.TestPlan
import Control.Monad (forM_, unless)
import Data.List (intercalate, isInfixOf, nub)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

runTests :: IO ()
runTests = do
  compileTests
  evaluationTests
  differentialTests
  identityTests
  rejectionTests
  putStrLn "Test-plan lowering, identity, and rejection tests passed."

check :: String -> Bool -> IO ()
check label ok = unless ok (ioError (userError ("LowerTest: " ++ label)))

config :: PlanConfig
config = PlanConfig
  { configName = "lowering-test"
  , configInternalChecks = True
  , configCompilerOptions = []
  }

testPath :: FilePath
testPath = "bsc.plan/example.exp"

loweredWith :: PlanConfig -> String -> Scenario
loweredWith options input = case lowerTest options testPath input of
  Left issue -> error ("LowerTest fixture did not lower: " ++ renderIssue issue)
  Right scenario -> scenario

lowered :: String -> Scenario
lowered = loweredWith config

compileSteps :: Scenario -> [Step]
compileSteps scenario =
  [step | step <- scenarioSteps scenario, BscCompile _ _ _ <- [stepOperation step]]

compilations :: Scenario -> [(FilePath, [String])]
compilations scenario =
  [(source, flags) | step <- compileSteps scenario,
                    BscCompile source flags _ <- [stepOperation step]]

ordinaryChecks :: Scenario -> [Check]
ordinaryChecks = filter ((== Ordinary) . checkClass) . scenarioChecks

internalChecks :: Scenario -> [Check]
internalChecks = filter ((== Internal) . checkClass) . scenarioChecks

compileTests :: IO ()
compileTests = do
  let success = lowered "compile_pass Good.bs\n"
      failure = lowered "compile_fail Bad.bs\n"
      noInternal = loweredWith (config { configInternalChecks = False })
        "compile_pass Good.bs\n"
  check "successful compilation has its source and flags"
    (compilations success == [("Good.bs", [])])
  check "compile_pass expects compiler success"
    (map checkExpectation (ordinaryChecks success) == [ToolSucceeds])
  check "compile_fail expects compiler failure"
    (map checkExpectation (ordinaryChecks failure) == [ToolFails])
  check "internal loading is an explicit operation following compilation"
    (map stepOperation (scenarioSteps success) ==
      [stepOperation (head (compileSteps success)), InternalLoad "Good.bo"])
  check "internal loading has its own success assertion"
    (map checkExpectation (internalChecks success) == [ToolSucceeds])
  check "expected compiler failures do not acquire an internal load"
    (length (scenarioSteps failure) == 1 && null (internalChecks failure))
  check "configuration can disable internal loading and assertions"
    (length (scenarioSteps noInternal) == 1 && null (internalChecks noInternal))
  check "every assertion names a producer in the scenario"
    (all (\assertion -> checkProducer assertion `elem` map stepId (scenarioSteps success))
      (scenarioChecks success))
  check "single-compile scenarios require local execution"
    (all ((== LocalOnly) . stepCacheability) (scenarioSteps success))
  check "operations declare the source or produced workspace they consume"
    (all (not . null . stepInputs) (scenarioSteps success))
  check "legacy nodeps defaults to compiling dependencies"
    ([dependencies | step <- compileSteps success,
                     BscCompile _ _ dependencies <- [stepOperation step]] == [True])
  let dependencyModes = lowered $ unlines
        [ "compile_pass WithDependencies.bs {} 0"
        , "compile_pass WithoutDependencies.bs {} 1"
        ]
  check "legacy nodeps 0 and 1 explicitly select dependency compilation"
    ([dependencies | step <- compileSteps dependencyModes,
                     BscCompile _ _ dependencies <- [stepOperation step]] == [True, False])
  let several = lowered "compile_pass First.bs\ncompile_fail Second.bs\n"
      steps = scenarioSteps several
  check "sequences preserve compiler invocation order"
    (map fst (compilations several) == ["First.bs", "Second.bs"])
  check "multi-compile sequences disable caching for every operation"
    (all ((== Never) . stepCacheability) steps)
  check "sequence operations depend on their immediate predecessor"
    (and [stepId previous `elem` stepDependsOn next
         | (previous, next) <- zip steps (drop 1 steps)])
  check "the first operation has no predecessor"
    (null (stepDependsOn (head steps)))
  let configured = loweredWith
        (config { configCompilerOptions = ["-v", "-let-gen"] })
        "compile_pass Configured.bs {-no-let-gen -dinternal}\n"
  check "configuration flags precede invocation flags without deduplication"
    (compilations configured ==
      [("Configured.bs", ["-v", "-let-gen", "-no-let-gen", "-dinternal"])])
  check "legacy BSV source names are accepted"
    (compilations (lowered "compile_pass Existing.bsv") == [("Existing.bsv", [])])

evaluationTests :: IO ()
evaluationTests = do
  let interpolated = lowered $ unlines
        [ "set stem Example"
        , "set extension bs"
        , "set flags -v"
        , "compile_pass ${stem}.$extension \"$flags -let-gen\""
        ]
  check "scalar variables interpolate in bare and quoted words"
    (compilations interpolated == [("Example.bs", ["-v", "-let-gen"])])
  let variants = lowered $ unlines
        [ "foreach flags {{} {-v} {-v}} {"
        , "  compile_pass Example.bs $flags"
        , "}"
        ]
  check "foreach list parsing preserves empty and repeated flag variants"
    (compilations variants ==
      [("Example.bs", []), ("Example.bs", ["-v"]), ("Example.bs", ["-v"])])
  let nested = lowered $ unlines
        [ "set variants {{} {-v}}"
        , "foreach source {One.bs Two.bs} {"
        , "  foreach flags $variants {compile_pass $source $flags}"
        , "}"
        ]
  check "nested loops expand in Tcl execution order"
    (compilations nested ==
      [("One.bs", []), ("One.bs", ["-v"]),
       ("Two.bs", []), ("Two.bs", ["-v"])])
  let empty = lowered $ unlines
        [ "foreach source {} {compile_pass $source}"
        , "compile_pass After.bs"
        ]
  check "an empty foreach performs no body invocation"
    (compilations empty == [("After.bs", [])])
  let assignment = lowered $ unlines
        [ "set source Before.bs"
        , "foreach unused {first second} {set source After.bs}"
        , "compile_pass $source"
        ]
  check "foreach scalar assignments remain visible after the loop"
    (compilations assignment == [("After.bs", [])])
  let lastValue = lowered $ unlines
        [ "foreach source {One.bs Two.bs} {}"
        , "compile_pass $source"
        ]
  check "foreach leaves its variable bound to the final element"
    (compilations lastValue == [("Two.bs", [])])

-- These fixed scripts run only against inert stubs. Never evaluate repository
-- test scripts: the oracle here checks language semantics, not compiler results.
differentialTests :: IO ()
differentialTests = forM_ fixtures $ \input -> do
  let prelude = unlines
        [ "proc capture {args} {puts [join $args |]}"
        , "proc compile_pass {source {flags {}} {nodeps 0}} {"
        , "  set command \"capture $flags -no-show-timestamps -no-show-version\""
        , "  if {$nodeps == 0} {append command \" -u\"}"
        , "  append command \" $source\""
        , "  uplevel 1 $command"
        , "}"
        , "proc compile_fail {source {flags {}} {nodeps 0}} {"
        , "  uplevel 1 [list compile_pass $source $flags $nodeps]"
        , "}"
        ]
      expected =
        [intercalate "|" (flags ++ ["-no-show-timestamps", "-no-show-version"] ++
           (if dependencies then ["-u"] else []) ++ [source])
        | step <- compileSteps (lowered input)
        , BscCompile source flags dependencies <- [stepOperation step]]
  (exitCode, output, errors) <- readProcessWithExitCode "tclsh" [] (prelude ++ input ++ "\n")
  check ("inert Tcl oracle succeeds: " ++ show input)
    (exitCode == ExitSuccess && null errors)
  check ("lowered invocation trace agrees with Tcl: " ++ show input)
    (lines output == expected)
  where
    fixtures =
      [ "set name Example; set flags -v; compile_pass ${name}.bs \"$flags -let-gen\""
      , "set stem Good; set unused \"\\$notDefined${stem}\"; compile_pass \"$stem\\.bs\" {-v}"
      , "set stem Good; set suffix \"\\x2ebs\"; compile_pass \"$stem$suffix\" \"\\x2dv\""
      , "foreach flags {{} {-v} {-v}} {compile_pass Example.bs $flags}"
      , "set variants {{} {-v}}; foreach source {One.bs Two.bs} {foreach flags $variants {compile_fail $source $flags 1}}"
      , "set source Before.bs; foreach unused {first second} {set source After.bs}; compile_pass $source"
      , "foreach source {} {compile_pass $source}; foreach source {One.bs Two.bs} {}; compile_pass $source"
      ]

identityTests :: IO ()
identityTests = do
  let original = lowered $ unlines
        [ "set flags {-v}"
        , "foreach source {One.bs Two.bs} {compile_pass $source $flags}"
        ]
      spaced = lowered $ unlines
        [ "# A comment before the same program."
        , ""
        , "  set flags {-v}; # A comment after the assignment."
        , "foreach source {One.bs Two.bs} {"
        , "  # The body still has one command."
        , "  compile_pass   $source   $flags"
        , "}"
        ]
  check "harmless whitespace and comments preserve operation identities"
    (map stepId (scenarioSteps original) == map stepId (scenarioSteps spaced))
  check "harmless whitespace and comments preserve assertion identities"
    (map checkId (scenarioChecks original) == map checkId (scenarioChecks spaced))
  let success = lowered "compile_pass Same.bs\n"
      failure = lowered "compile_fail Same.bs\n"
      changed = lowered "compile_pass Changed.bs {-v}\n"
      noInternal = loweredWith (config { configInternalChecks = False })
        "compile_pass Same.bs\n"
  check "expected pass versus fail does not rename the compiler operation"
    (map stepId (compileSteps success) == map stepId (compileSteps failure))
  check "expected pass versus fail does not rename the ordinary assertion"
    (map checkId (ordinaryChecks success) == map checkId (ordinaryChecks failure))
  check "source and flag edits retain source-address identities"
    (map stepId (compileSteps success) == map stepId (compileSteps changed) &&
     map checkId (ordinaryChecks success) == map checkId (ordinaryChecks changed))
  check "internal-check policy does not rename the ordinary work"
    (map stepId (compileSteps success) == map stepId (compileSteps noInternal) &&
     map checkId (ordinaryChecks success) == map checkId (ordinaryChecks noInternal))
  let duplicate = lowered "foreach source {Same.bs Same.bs} {compile_pass $source}\n"
      operationIds = map stepId (scenarioSteps duplicate)
      assertionIds = map checkId (scenarioChecks duplicate)
  check "repeated loop values produce distinct compiler invocations"
    (length (compileSteps duplicate) == 2 &&
     length (nub (map stepId (compileSteps duplicate))) == 2)
  check "all operation and assertion identities are unique across loop iterations"
    (length (nub (operationIds ++ assertionIds)) == length operationIds + length assertionIds)
  check "automatic internal work stays associated with its compiler source address"
    (and [identifierSite (stepId compiler) == identifierSite (checkId assertion)
         | (compiler, assertion) <- zip (compileSteps duplicate) (internalChecks duplicate)])
  let other = case lowerTest config "bsc.plan/other.exp" "compile_pass Same.bs\n" of
        Left issue -> error (renderIssue issue)
        Right scenario -> scenario
  check "test paths distinguish otherwise identical source addresses"
    (map stepId (compileSteps success) /= map stepId (compileSteps other))

rejectionTests :: IO ()
rejectionTests = do
  forM_ unsupported $ \(label, input) -> do
    let positioned = "# The unsupported construct begins below.\n" ++ input ++ "\n"
    case lowerTest config testPath positioned of
      Right _ -> check (label ++ " must not produce a plan") False
      Left issue -> do
        let position = issuePosition issue
        check (label ++ " has the logical test path") (sourceFile position == testPath)
        check (label ++ " has a source line and column")
          (sourceLine position >= 2 && sourceColumn position >= 1)
        check (label ++ " names its construct and reason")
          (not (null (issueConstruct issue)) && not (null (issueReason issue)))
        check (label ++ " renders a located diagnostic")
          (testPath `isInfixOf` renderIssue issue && issueReason issue `isInfixOf` renderIssue issue)
  case lowerPlan config
    [("bsc.plan/good.exp", "compile_pass Good.bs"),
     ("bsc.plan/bad.exp", "exec forbidden")] of
    Right _ -> check "one unsupported test rejects the whole requested plan" False
    Left issues -> check "whole-plan failure identifies the unsupported test"
      (any ((== "bsc.plan/bad.exp") . sourceFile . issuePosition) issues)
  case lowerPlan config
    [("bsc.plan/a.exp", "compile_pass A.bs"),
     ("bsc.plan/b.exp", "compile_fail B.bs")] of
    Left issues -> ioError (userError (unlines (map renderIssue issues)))
    Right plan -> check "all supported inputs appear in the complete plan"
      (length (planScenarios plan) == 2)
  forM_ [config { configName = "" }, config { configCompilerOptions = ["-unknown"] }] $ \badConfig ->
    check "invalid configuration is rejected before producing a scenario"
      (case lowerTest badConfig testPath "compile_pass Good.bs" of
        Left _ -> True
        Right _ -> False)
  where
    unsupported =
      [ ("command substitution", "compile_pass [exec forbidden]")
      , ("array variable", "compile_pass $sources(first)")
      , ("undefined variable", "compile_pass $missing")
      , ("braced words do not interpolate", "set source Good.bs; compile_pass {$source}")
      , ("argument expansion", "compile_pass {*}{Good.bs}")
      , ("source", "source other.tcl")
      , ("exec", "exec forbidden")
      , ("file mutation", "file copy old new")
      , ("copy helper", "copy old new")
      , ("touch helper", "touch Good.bs")
      , ("after", "after 1")
      , ("procedure", "proc helper {} {compile_pass Good.bs}")
      , ("conditional", "if {1} {compile_pass Good.bs}")
      , ("expected-failure helper", "setup_xfail *-*-*")
      , ("unknown helper", "unknown_helper Good.bs")
      , ("dynamic command", "set command compile_pass; $command Good.bs")
      , ("multiple foreach variables", "foreach {one two} {A.bs B.bs} {compile_pass $one}")
      , ("multiple foreach lists", "foreach one {A.bs} two {B.bs} {compile_pass $one}")
      , ("dynamic foreach body", "set body {compile_pass Good.bs}; foreach x {1} $body")
      , ("invalid foreach list", "foreach x {\"unterminated} {compile_pass Good.bs}")
      , ("array assignment", "set values(first) Good.bs")
      , ("wrong compile arity", "compile_pass")
      , ("extra compile arguments", "compile_pass Good.bs {} 0 extra")
      , ("unknown option", "compile_pass Good.bs {-unknown}")
      , ("option argument", "compile_pass Good.bs {-v extra}")
      , ("trailing option separator", "compile_pass Good.bs {-v;}")
      , ("option separator then comment", "compile_pass Good.bs {-v;# comment}")
      , ("trailing option newline", "compile_pass Good.bs {-v\n}")
      , ("harness output directory", "set outdir elsewhere; compile_pass Good.bs")
      , ("known-failure state", "set kfail_flag 1; compile_pass Good.bs")
      , ("unresolved state", "set errcnt 1; compile_pass Good.bs")
      , ("invalid nodeps", "compile_pass Good.bs {} true")
      , ("parent source", "compile_pass ../Good.bs")
      , ("absolute source", "compile_pass /tmp/Good.bs")
      , ("nested source", "compile_pass nested/Good.bs")
      , ("wrong source extension", "compile_pass Good.txt")
      , ("malformed Tcl", "compile_pass {Good.bs")
      ]
