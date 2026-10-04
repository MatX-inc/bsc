module LowerTest (runTests) where

import Lower
import Procedures (compilePass, compileFail, internalChecksFor, InternalCheck(..), explainTest, supportedProcedures)
import Tcl (SourcePos(..))
import TestPlan
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
  partialPlanTests
  unsupportedTests
  putStrLn "Semantic procedures, Tcl adaptation, identity, and partial-plan tests passed."

check :: String -> Bool -> IO ()
check label ok = unless ok (ioError (userError ("LowerTest: " ++ label)))

config :: PlanConfig
config = PlanConfig "lowering-test" True []

testPath :: FilePath
testPath = "bsc.plan/example.exp"

loweredWith :: PlanConfig -> String -> TestPlan
loweredWith options input = case lowerPlan options [(testPath, input)] of
  Left err -> error ("LowerTest fixture did not lower: " ++ err)
  Right plan -> plan

lowered :: String -> TestPlan
lowered = loweredWith config

compilations :: TestPlan -> [Compilation]
compilations plan = [compilation | test <- plannedTests plan,
                                  CompilationTest compilation _ <- [testKind test]]

sourceOptions :: TestPlan -> [(FilePath, [String])]
sourceOptions = map (\c -> (compilationSource c, compilationOptions c)) . compilations

compileTests :: IO ()
compileTests = do
  let success = lowered "compile_pass Good.bs\n"
      failure = lowered "compile_fail Bad.bs\n"
      noInternalConfig = config { configInternalChecks = False }
      noInternal = loweredWith noInternalConfig "compile_pass Good.bs\n"
      successKind = testKind (head (plannedTests success))
      failureKind = testKind (head (plannedTests failure))
  check "compile_pass describes an expected-success package compilation"
    (successKind == CompilationTest (Compilation "Good.bs" [] True) CompileSucceeds)
  check "compile_fail describes an expected-failure package compilation"
    (failureKind == CompilationTest (Compilation "Bad.bs" [] True) CompileFails)
  check "internal object inspection belongs to the expected-success test"
    (internalChecksFor config successKind == Right [ObjectLoads "Good.bo"])
  check "expected compiler failures have no automatic object inspection"
    (internalChecksFor config failureKind == Right [])
  check "internal-check policy changes obligations without changing tests"
    (plannedTests noInternal == plannedTests success &&
     internalChecksFor noInternalConfig successKind == Right [])
  let origin = SourcePos testPath 1 1 0
      identifier = Identifier testPath 1
      invocation = Compilation "Good.bs" [] True
  check "Tcl compile_pass uses the shared semantic procedure"
    (compilePass config identifier origin invocation == Right (head (plannedTests success)))
  check "semantic compileFail does not need a Tcl source adapter"
    (fmap testKind (compileFail config identifier origin invocation) ==
      Right (CompilationTest invocation CompileFails))
  check "only implemented semantic procedures reserve invocation numbers"
    (supportedProcedures == ["compile_pass", "compile_fail"])
  check "resolved invocation adapter agrees with source lowering"
    (lowerInvocation config identifier origin "compile_pass" ["Good.bs"] ==
      Right (head (plannedTests success)))
  let later = Identifier testPath 8
  check "resolved invocations retain later numbers, options, dependencies and expectation"
    (lowerInvocation config later origin "compile_fail" ["Bad.bs", "-v", "1"] ==
      Right (Test later origin (CompilationTest (Compilation "Bad.bs" ["-v"] False) CompileFails)))
  forM_ [("compile_pass", ["Good.bs", "-unknown"]),
         ("compile_pass", []), ("compile_verilog_pass", ["Good.bs"])] $ \(name, args) ->
    check "resolved invocation adapter rejects unsupported calls"
      (case lowerInvocation config identifier origin name args of
        Left _ -> True
        Right _ -> False)
  check "semantic constructors reject invalid identities and origins"
    (case compilePass config (Identifier "../invalid.exp" 0)
            (SourcePos "other.exp" 0 0 (-1)) invocation of
      Left _ -> True
      Right _ -> False)
  check "deliberately missing sources remain valid negative tests"
    (planCounts (lowered "compile_fail DoesNotExist.bs") == (1,0,0))
  let dependencyModes = lowered $ unlines
        ["compile_pass WithDependencies.bs {} 0", "compile_pass WithoutDependencies.bs {} 1"]
  check "nodeps is inverted into a positive dependency-compilation choice"
    (map compilationDependencies (compilations dependencyModes) == [True, False])
  let several = lowered "compile_pass First.bs\ncompile_fail Second.bs\n"
  check "multiple declarations retain their source order and test kinds"
    (map compilationSource (compilations several) == ["First.bs", "Second.bs"] &&
     planCounts several == (2,0,0))
  let configured = loweredWith
        (config { configCompilerOptions = ["-v", "-let-gen"] })
        "compile_pass Configured.bs {-v -no-let-gen -dinternal}\n"
  check "configuration flags precede local flags with duplicates preserved"
    (sourceOptions configured ==
      [("Configured.bs", ["-v", "-let-gen", "-v", "-no-let-gen", "-dinternal"])])
  check "legacy BSV sources are accepted"
    (sourceOptions (lowered "compile_pass Existing.bsv") == [("Existing.bsv", [])])
  let unsupportedOptions = success { planScripts = [ScriptPlan testPath
        [Planned (head (plannedTests success)) { testKind = CompilationTest
          (Compilation "Good.bs" ["-bdir", "elsewhere"] True) CompileSucceeds }]] }
  check "explanation rejects semantics not supported by the current procedures"
    (validatePlan unsupportedOptions == Right () &&
     case explainTest unsupportedOptions (renderIdentifier identifier) of
       Left _ -> True
       Right _ -> False)
  check "internal-check derivation also rejects unsupported artifact redirection"
    (case internalChecksFor config (testKind (head (plannedTests unsupportedOptions))) of
       Left _ -> True
       Right _ -> False)

evaluationTests :: IO ()
evaluationTests = do
  let interpolated = lowered $ unlines
        [ "set stem Example"
        , "set extension bs"
        , "set flags -v"
        , "compile_pass ${stem}.$extension \"$flags -let-gen\""
        ]
  check "scalar variables interpolate in bare and quoted words"
    (sourceOptions interpolated == [("Example.bs", ["-v", "-let-gen"])])
  let variants = lowered $ unlines
        [ "foreach flags {{} {-v} {-v}} {"
        , "  compile_pass Example.bs $flags"
        , "}"
        ]
  check "foreach list parsing preserves empty and repeated flag variants"
    (sourceOptions variants ==
      [("Example.bs", []), ("Example.bs", ["-v"]), ("Example.bs", ["-v"])])
  let nested = lowered $ unlines
        [ "set variants {{} {-v}}"
        , "foreach source {One.bs Two.bs} {"
        , "  foreach flags $variants {compile_pass $source $flags}"
        , "}"
        ]
  check "nested loops expand in Tcl execution order"
    (sourceOptions nested ==
      [("One.bs", []), ("One.bs", ["-v"]),
       ("Two.bs", []), ("Two.bs", ["-v"])])
  let empty = lowered $ unlines
        [ "foreach source {} {compile_pass $source}"
        , "compile_pass After.bs"
        ]
  check "an empty foreach performs no body invocation"
    (sourceOptions empty == [("After.bs", [])])
  let assignment = lowered $ unlines
        [ "set source Before.bs"
        , "foreach unused {first second} {set source After.bs}"
        , "compile_pass $source"
        ]
  check "foreach scalar assignments remain visible after the loop"
    (sourceOptions assignment == [("After.bs", [])])
  let lastValue = lowered $ unlines
        [ "foreach source {One.bs Two.bs} {}"
        , "compile_pass $source"
        ]
  check "foreach leaves its variable bound to the final element"
    (sourceOptions lastValue == [("Two.bs", [])])

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
        | Compilation source flags dependencies <- compilations (lowered input)]
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
        ["set flags {-v}", "foreach source {One.bs Two.bs} {compile_pass $source $flags}"]
      spaced = lowered $ unlines
        [ "# The same program with comments."
        , "  set flags {-v}; # comment"
        , "foreach source {One.bs Two.bs} {"
        , "  # one command"
        , "  compile_pass   $source   $flags"
        , "}"
        ]
      ids = map testId . plannedTests
  check "comments and whitespace preserve test identities" (ids original == ids spaced)
  let unrelated = lowered $ unlines
        [ "set unused ignored", "set flags {-v}"
        , "compile_verilog_pass Ignored.bs"
        , "foreach source {One.bs Two.bs} {set unused ignored; compile_pass $source $flags}"
        ]
  check "unrelated assignments and unsupported helpers do not renumber tests"
    (ids original == ids unrelated && map issueId (planIssues unrelated) == [Nothing])
  let opaqueHelper = lowered "compile_pass First.bs; unknown_helper; compile_fail Second.bs"
      counted plan = [identifier | script <- planScripts plan, item <- scriptItems script,
                        Just identifier <- [case item of
                          Planned test -> Just (testId test)
                          Unplanned issue -> issueId issue]]
  check "arbitrary unsupported helpers do not consume a number when they block later tests"
    (map identifierNumber (counted opaqueHelper) == [1,2] &&
     map (fmap identifierNumber . issueId) (planIssues opaqueHelper) == [Nothing, Just 2])
  let success = lowered "compile_pass Same.bs"
      failure = lowered "compile_fail Same.bs"
      changed = lowered "compile_pass Changed.bs {-v}"
  check "expected outcome, source and flags do not rename the invocation"
    (ids success == ids failure && ids success == ids changed)
  let duplicate = lowered "foreach source {Same.bs Same.bs} {compile_pass $source}"
  check "repeated loop values describe distinct tests"
    (length (nub (ids duplicate)) == 2 &&
     map identifierNumber (ids duplicate) == [1,2])
  let other = either error id (lowerPlan config [("bsc.plan/other.exp", "compile_pass Same.bs")])
  check "script paths distinguish identical invocation numbers" (ids success /= ids other)
  let files = either error id (lowerPlan config
        [(testPath, "compile_pass One.bs; compile_fail Two.bs"),
         ("bsc.plan/other.exp", "compile_pass Three.bs")])
  check "invocation counters restart in every script"
    (map (identifierNumber . testId) (plannedTests files) == [1,2,1])

partialPlanTests :: IO ()
partialPlanTests = do
  let mixed = lowered $ unlines
        ["compile_pass Before.bs", "compile_verilog_pass Other.bs", "compile_pass After.bs"]
  check "unsupported test kinds preserve independent tests on both sides"
    (planCounts mixed == (2,1,0) &&
     map compilationSource (compilations mixed) == ["Before.bs", "After.bs"])
  check "known diagnostic tests do not poison subsequent independent tests"
    (planCounts (lowered "compile_fail_error Bad.bs T0001; compile_pass Good.bs") == (1,1,0))
  let opaque = lowered $ unlines
        ["compile_pass Before.bs", "exec forbidden", "compile_pass After.bs"]
  check "opaque effects preserve the prefix and mark subsequent tests unresolved"
    (planCounts opaque == (1,1,1) &&
     "unsupported item" `isInfixOf` issueReason (last (planIssues opaque)))
  let stale = lowered $ unlines
        [ "set source Before.bs", "set source $missing"
        , "compile_pass Independent.bs", "compile_pass $source"
        ]
  check "unresolved assignments invalidate old values without poisoning independent literals"
    (planCounts stale == (1,0,2) &&
     map compilationSource (compilations stale) == ["Independent.bs"])
  let recovered = lowered $ unlines
        ["set source $missing", "set source Good.bs", "compile_pass $source"]
  check "a later known assignment restores that scalar"
    (planCounts recovered == (1,0,1))
  let reserved = lowered "set outdir elsewhere; compile_pass Good.bs"
      substitution = lowered "compile_pass [exec forbidden]; compile_pass Good.bs"
  check "reserved state and command substitutions leave setup unknown"
    (planCounts reserved == (0,1,1) && planCounts substitution == (0,1,1))
  forM_ ["compile_pass A.bs {[set ::source New.bs]}",
         "compile_pass $srcdir {[set ::source New.bs]}",
         "compile_verilog_pass A.bs {} {[set ::source New.bs]}",
         "compile_pass A.bs {-v; set ::source New.bs}"] $ \invocation -> do
    let secondParse = lowered ("set source Old.bs\n" ++ invocation ++ "\ncompile_pass $source")
    check "second-pass Tcl effects invalidate later declarations"
      (null (plannedTests secondParse) && length (planIssues secondParse) == 2 &&
       issueKind (last (planIssues secondParse)) == UnresolvedDependency)
  let loop = lowered $ unlines
        [ "foreach source {A.bs B.bs} {"
        , "  compile_pass $source {-unknown}"
        , "  compile_pass $source"
        , "}"
        ]
  check "unsupported invocations are counted separately in every loop iteration"
    (planCounts loop == (2,2,0) &&
     map (fmap identifierNumber . issueId) (planIssues loop) == [Just 1, Just 3])
  check "loop successes follow numbers reserved for unsupported invocations"
    (map (identifierNumber . testId) (plannedTests loop) == [2,4])
  let failedCalls = lowered "compile_pass; compile_fail $missing; compile_pass Later.bs"
  check "wrong arity and unresolved arguments both reserve invocation numbers"
    (map (fmap identifierNumber . issueId) (planIssues failedCalls) ==
      [Just 1, Just 2, Just 3])
  let numberedIssue = head (planIssues loop)
  check "numbered issues can be explained"
    (case issueId numberedIssue >>= either (const Nothing) Just . explainTest loop . renderIdentifier of
      Just description -> "unsupported" `isInfixOf` description
      Nothing -> False)
  let independent = either error id (lowerPlan config
        [(testPath, "compile_pass {broken"), ("bsc.plan/good.exp", "compile_pass Good.bs")])
  check "a syntax error retains its script and permits other scripts to lower"
    (length (planScripts independent) == 2 && planCounts independent == (1,1,0) &&
     map issueId (planIssues independent) == [Nothing])
  -- Empty loop bodies must still consume the finite static expansion budget.
  let values = unwords (replicate 320 "x")
      large = lowered ("foreach x {" ++ values ++ "} {foreach y {" ++ values ++ "} {}}")
  check "nested empty loops cannot evade the expansion bound"
    (planCounts large == (0,1,0) &&
     "remaining commands and iterations were not inspected" `isInfixOf`
       issueReason (head (planIssues large)))
  let doubling = lowered ("set x x; foreach i {" ++ unwords (replicate 24 "1") ++
        "} {set x \"$x $x\"}; foreach i $x {}; compile_pass Independent.bs")
  check "scalar expansion is bounded before parsing the resulting list"
    (any (isInfixOf "expanded scalar exceeds" . issueReason) (planIssues doubling))
  forM_ ["{}", "{odd\ncommand}", "{odd\tcommand}"] $ \command -> do
    let unusual = lowered (command ++ "; compile_pass Good.bs")
    check "unusual command names remain representable issues"
      (planCounts unusual == (0,1,1) && decodePlan (encodePlan unusual) == Right unusual)
  forM_ [config { configName = "" }, config { configCompilerOptions = ["-unknown"] }] $ \bad ->
    check "invalid global configuration remains a fatal invocation error"
      (case lowerPlan bad [(testPath, "compile_pass Good.bs")] of
        Left _ -> True
        Right _ -> False)

unsupportedTests :: IO ()
unsupportedTests = forM_ unsupported $ \(label, input) -> do
  let positioned = "# The unsupported construct begins below.\n" ++ input ++ "\n"
      plan = lowered positioned
  check (label ++ " is retained as an issue") (not (null (planIssues plan)))
  forM_ (planIssues plan) $ \issue -> do
    let position = issuePosition issue
    check (label ++ " has the logical test path") (sourceFile position == testPath)
    check (label ++ " has a source line and column")
      (sourceLine position >= 2 && sourceColumn position >= 1)
    check (label ++ " names its construct and reason")
      (not (null (issueConstruct issue)) && not (null (issueReason issue)))
    check (label ++ " renders a located diagnostic")
      (testPath `isInfixOf` renderIssue issue && issueReason issue `isInfixOf` renderIssue issue)
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
