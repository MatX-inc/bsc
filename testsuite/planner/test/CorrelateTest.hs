module CorrelateTest (runTests) where

import Control.Monad (forM_, unless)
import Data.Either (isLeft)
import Data.List (isInfixOf)
import Correlate
import Lower (lowerPlan)
import TestPlan

runTests :: IO ()
runTests = do
  decoderTests
  errorTests
  traces <- either fail pure (decodeTestLog "example.log" logText)
  trace <- case traces of
    [value] -> pure value
    _ -> fail "expected one script in example log"
  let report = correlate plan trace
  check "supported invocations match across an uncounted helper"
    (length (correlationMatches report) == 2 && null (correlationProblems report))
  let calls = provenanceInvocations trace
      first = head calls
      second = calls !! 1
      withFirst value = trace { provenanceInvocations = [value, second] }
      problem fragment changed = any (isInfixOf fragment) (correlationProblems (correlate plan changed))
  check "number alone cannot conceal a source mismatch"
    (problem "source location differs" (withFirst first { invocationLine = 2 }))
  check "number alone cannot conceal changed flags"
    (problem "resolved arguments differ" (withFirst first { invocationArguments = ["Example.bs"] }))
  check "number alone cannot conceal changed procedure"
    (problem "procedure differs" (withFirst first { invocationProcedure = "compile_fail" }))
  check "missing internal check is visible"
    (problem "result roles differ" (withFirst first { invocationResults = take 1 (invocationResults first) }))
  check "an extra internal check is visible"
    (problem "result roles differ" (withFirst first { invocationResults = invocationResults first ++ take 1 (invocationResults first) }))
  check "missing invocation is visible"
    (problem "missing invocation" trace { provenanceInvocations = [first] })
  check "extra invocation is visible"
    (problem "extra invocation" trace { provenanceInvocations = calls ++ [second { invocationNumber = 3 }] })
  check "wrong internal-check policy is rejected"
    (problem "internal-check configuration differs" trace { provenanceInternalChecks = False })
  let configuredPlan = either error id (lowerPlan config { configCompilerOptions = ["-v"] }
        [(script, source)])
  check "unrecorded global compiler options cannot be silently added to observed calls"
    (case correlatePlan configuredPlan [trace] of
       Left message -> "provenance does not record global compiler options" `isInfixOf` message
       Right _ -> False)
  let failed = first { invocationResults = (head (invocationResults first))
                       { observationDisposition = "FAIL" } : tail (invocationResults first) }
      failedReport = correlate plan (withFirst failed)
  check "a matched failure remains a failure"
    (length (correlationMatches failedReport) == 2 &&
     any (isInfixOf "reported FAIL") (correlationProblems failedReport))
  let partial = either error id (lowerPlan config
        [(script, "compile_pass Example.bs {-unknown}\ncompile_fail Missing.bs")])
      partialTrace = trace { provenanceInvocations = [first, second { invocationLine = 2 }] }
      partialReport = correlate partial partialTrace
  check "unsupported numbered invocation is explicitly skipped without shifting later tests"
    (length (correlationSkips partialReport) == 1 && length (correlationMatches partialReport) == 1 &&
     null (correlationProblems partialReport))
  let noInternalPlan = either error id (lowerPlan config { configInternalChecks = False }
        [(script, source)])
      noInternalTrace = trace { provenanceInternalChecks = False,
        provenanceInvocations = [first { invocationResults = take 1 (invocationResults first) }, second] }
  check "internal policy does not renumber semantic tests"
    (map testId (plannedTests noInternalPlan) == map testId (plannedTests plan) &&
     null (correlationProblems (correlate noInternalPlan noInternalTrace)))
  check "absent capture does not count as a complete match"
    (case correlatePlan plan [] of
       Right result -> length (correlationProblems result) == 2
       Left _ -> False)
  check "duplicate captures cannot double-count a script" (isLeft (correlatePlan plan [trace,trace]))
  putStrLn "Ordinary-log decoding, supported-test correspondence, and mismatch tests passed."

errorTests :: IO ()
errorTests = do
  let errorSource = "compile_fail_error Broken.bs T0001 2\ncompile_pass Good.bs\ncompile_fail Plain.bs\n"
      errorPlan internal = either error id (lowerPlan config { configInternalChecks = internal }
        [(script, errorSource)])
      text = unlines
        [ "BSC-TEST: script 1 bsc.example/example.exp 1"
        , "BSC-TEST: begin 1 compile_fail_error bsc.example/example.exp 1 {Broken.bs T0001 2 {} 0}"
        , "BSC-TEST: role 1 diagnostic-count"
        , "PASS: found two matching errors"
        , "BSC-TEST: end 1"
        , "BSC-TEST: begin 2 compile_pass bsc.example/example.exp 2 Good.bs"
        , "PASS: compilation succeeds"
        , "BSC-TEST: role 2 object-load"
        , "PASS: object loads"
        , "BSC-TEST: end 2"
        , "BSC-TEST: begin 3 compile_fail bsc.example/example.exp 3 Plain.bs"
        , "PASS: compilation fails"
        , "BSC-TEST: end 3"
        , "BSC-TEST: finish 3"
        ]
      observed = case decodeTestLog "error.log" text of
        Right [trace] -> trace
        result -> error (show result)
      calls = provenanceInvocations observed
      first = head calls
      withResults results = observed { provenanceInvocations =
        first { invocationResults = results } : tail calls }
      diagnostic disposition = Observation "diagnostic-count" disposition "error count"
      primary disposition = Observation "compilation" disposition "unexpected success"
      loaded = Observation "object-load" "PASS" "object loads"
      problems trace = correlationProblems (correlate (errorPlan True) trace)
      hasProblem fragment trace = any (isInfixOf fragment) (problems trace)
  check "error-tag test has one diagnostic result and interleaves with existing kinds"
    (length (correlationMatches (correlate (errorPlan True) observed)) == 3 &&
     null (problems observed))
  check "wrong error count remains a matched failing observation"
    (hasProblem "diagnostic-count reported FAIL" (withResults [diagnostic "FAIL"]) &&
     length (correlationMatches (correlate (errorPlan True) (withResults [diagnostic "FAIL"]))) == 3)
  check "unexpected successful compilation requires its conditional object check"
    (hasProblem "result roles differ" (withResults [primary "FAIL"]))
  check "unexpected successful compilation is a matched failure with an internal check"
    (hasProblem "compilation reported FAIL" (withResults [primary "FAIL", loaded]) &&
     length (correlationMatches (correlate (errorPlan True) (withResults [primary "FAIL", loaded]))) == 3)
  check "a fabricated PASS cannot conceal unexpected compilation success"
    (hasProblem "unexpected compilation success must report FAIL" (withResults [primary "PASS", loaded]))
  check "diagnostic branch must not grow an unconditional object-load check"
    (hasProblem "result roles differ" (withResults [diagnostic "PASS", loaded]))
  check "duplicate diagnostic results are rejected"
    (hasProblem "result roles differ" (withResults [diagnostic "PASS", diagnostic "PASS"]))
  let changedTag = observed { provenanceInvocations = first
        { invocationArguments = ["Broken.bs", "T9999", "2", "", "0"] } : tail calls }
  check "observed diagnostic arguments are checked against the saved expectation"
    (hasProblem "resolved arguments differ" changedTag)
  let withoutInternal = observed { provenanceInternalChecks = False,
        provenanceInvocations = first { invocationResults = [primary "FAIL"] } :
          [call { invocationResults = filter ((/= "object-load") . observationRole)
                   (invocationResults call) } | call <- tail calls] }
      withoutReport = correlate (errorPlan False) withoutInternal
  check "unexpected success with internal checks disabled needs no object result"
    (length (correlationMatches withoutReport) == 3 &&
     length (correlationProblems withoutReport) == 1 &&
     any (isInfixOf "compilation reported FAIL") (correlationProblems withoutReport))
  forM_ [ replace "BSC-TEST: role 1 diagnostic-count"
            "PASS: spurious compilation verdict\nBSC-TEST: role 1 diagnostic-count" text
        , replace "BSC-TEST: role 1 diagnostic-count"
            "BSC-TEST: role 1 diagnostic-count\nBSC-TEST: role 1 diagnostic-count" text
        , replace "BSC-TEST: role 1 diagnostic-count"
            "BSC-TEST: role 1 diagnostic-count\nBSC-TEST: role 1 object-load" text
        , replace "begin 1 compile_fail_error" "begin 1 compile_fail" text
        ] $ \malformed -> check "invalid diagnostic branch marker sequence is rejected"
          (isLeft (decodeTestLog "error.log" malformed))

decoderTests :: IO ()
decoderTests = do
  let decode = decodeTestLog "example.log"
      one text = case decode text of
        Right [decoded] -> decoded
        result -> error ("expected one decoded script: " ++ show result)
      trace = one logText
      calls = provenanceInvocations trace
  check "ordinary output and verdicts outside invocations are ignored"
    (map (map observationMessage . invocationResults) calls ==
      [["Example.bs compiles", "Example.bo loads"], ["Missing.bs does not compile"]])
  check "role marker attaches the internal check to its parent invocation"
    (map (map observationRole . invocationResults) calls ==
      [["compilation", "object-load"], ["compilation"]])
  let extra = unlines
        [ "BSC-TEST: script 1 bsc.example/other.exp 0"
        , "BSC-TEST: begin 1 compile_fail bsc.example/other.exp 8 Other.bs"
        , "PASS: Other.bs does not compile"
        , "BSC-TEST: end 1"
        , "BSC-TEST: finish 1"
        , "BSC-TEST: script 1 bsc.example/empty.exp 1"
        , "BSC-TEST: finish 0"
        ]
  check "multiple scripts preserve order and reset the invocation counter"
    (case decode (logText ++ extra) of
      Right scripts -> map provenanceTest scripts ==
        [script, "bsc.example/other.exp", "bsc.example/empty.exp"] &&
        map (map invocationNumber . provenanceInvocations) scripts == [[1,2],[1],[]] &&
        map provenanceInternalChecks scripts == [True,False,True]
      Left _ -> False)
  forM_ ["PASS", "FAIL", "XPASS", "XFAIL", "KPASS", "KFAIL",
         "UNRESOLVED", "UNTESTED", "UNSUPPORTED"] $ \disposition -> do
    let changed = one (replace "PASS: Missing.bs does not compile"
          (disposition ++ ":  exact message {with braces} $literal  ") logText)
        result = head (invocationResults (last (provenanceInvocations changed)))
    check ("single-line disposition and exact message: " ++ disposition)
      (observationDisposition result == disposition &&
       observationMessage result == " exact message {with braces} $literal  ")
  let literal = one (replace "{Example.bs -v}" "{{[exec forbidden]} {$missing}}" logText)
  check "metadata arguments are Tcl list data without command or variable evaluation"
    (invocationArguments (head (provenanceInvocations literal)) == ["[exec forbidden]", "$missing"])
  forM_ malformed $ \(label, input) -> check label (isLeft (decode input))
  check "protocol failures identify the log and line"
    (case decode (replace "BSC-TEST: end 1" "BSC-TEST: end 2" logText) of
      Left problem -> "example.log:" `isInfixOf` problem && "end number differs" `isInfixOf` problem
      Right _ -> False)
  where
    malformed =
      [ ("logs without markers are unobserved", "PASS: ordinary test\n")
      , ("unsupported multiline metadata is rejected",
          "BSC-TEST: unsupported multiline metadata\n")
      , ("missing finish marks an incomplete capture", replace "BSC-TEST: finish 2\n" "" logText)
      , ("missing end marks an incomplete invocation", replace "BSC-TEST: end 2\n" "" logText)
      , ("end outside a script is rejected", logText ++ "BSC-TEST: end 2\n")
      , ("begin outside a script is rejected", "BSC-TEST: begin 1 compile_pass a.exp 1 A.bs\n")
      , ("noncontiguous invocation numbers are rejected", replace "begin 2" "begin 3" logText)
      , ("mismatched invocation end is rejected", replace "end 1" "end 2" logText)
      , ("mismatched role number is rejected", replace "role 1" "role 2" logText)
      , ("unknown result role is rejected", replace "role 1 object-load" "role 1 other" logText)
      , ("repeated role markers are rejected", replace "BSC-TEST: role 1 object-load"
          "BSC-TEST: role 1 object-load\nBSC-TEST: role 1 object-load" logText)
      , ("mismatched finish count is rejected", replace "finish 2" "finish 3" logText)
      , ("nested invocation begin is rejected", replace "BSC-TEST: role 1 object-load"
          "BSC-TEST: begin 2 compile_pass bsc.example/example.exp 2 Nested.bs" logText)
      , ("nested script is rejected", replace "BSC-TEST: role 1 object-load"
          "BSC-TEST: script 1 bsc.example/nested.exp 1" logText)
      , ("error in an invocation is rejected", replace "PASS: Example.bs compiles" "ERROR: compiler crashed" logText)
      , ("error between invocations is rejected", replace "PASS: unsupported helper result"
          "ERROR: script aborted" logText)
      , ("unsupported protocol version is rejected", replace "script 1" "script 2" logText)
      , ("invalid internal policy is rejected", replace "example.exp 1\n" "example.exp true\n" logText)
      , ("uncounted procedure marker is rejected", replace "begin 1 compile_pass" "begin 1 other_helper" logText)
      , ("invalid source line is rejected", replace "example.exp 1 {Example.bs" "example.exp 0 {Example.bs" logText)
      , ("malformed list metadata is rejected", replace "{Example.bs -v}" "{Example.bs -v" logText)
      , ("extra marker arguments are rejected", replace "BSC-TEST: end 1" "BSC-TEST: end 1 extra" logText)
      , ("unknown markers are rejected", logText ++ "BSC-TEST: unknown\n")
      ]

check :: String -> Bool -> IO ()
check label value = unless value (fail ("CorrelateTest: " ++ label))

config :: PlanConfig
config = PlanConfig "correlation" True []
script :: FilePath
script = "bsc.example/example.exp"
source :: String
source = "compile_pass Example.bs {-v}\ncompile_verilog_pass Ignored.bs\ncompile_fail Missing.bs\n"
plan :: TestPlan
plan = either error id (lowerPlan config [(script, source)])
correlate :: TestPlan -> Provenance -> Correlation
correlate p trace = either error id (correlatePlan p [trace])
logText :: String
logText = unlines
  [ "Test run by example on Sunday"
  , "PASS: unrelated setup"
  , "BSC-TEST: script 1 bsc.example/example.exp 1"
  , "Running bsc.example/example.exp ..."
  , "BSC-TEST: begin 1 compile_pass bsc.example/example.exp 1 {Example.bs -v}"
  , "Executing compiler and collecting ordinary output"
  , "PASS: Example.bs compiles"
  , "BSC-TEST: role 1 object-load"
  , "PASS: Example.bo loads"
  , "BSC-TEST: end 1"
  , "PASS: unsupported helper result"
  , "BSC-TEST: begin 2 compile_fail bsc.example/example.exp 3 Missing.bs"
  , "PASS: Missing.bs does not compile"
  , "BSC-TEST: end 2"
  , "BSC-TEST: finish 2"
  , "=== bsc Summary ==="
  , "ERROR: outside the captured script"
  ]

replace :: String -> String -> String -> String
replace old new input
  | take (length old) input == old = new ++ drop (length old) input
  | c:rest <- input = c : replace old new rest
  | otherwise = error "CorrelateTest replacement did not match"
