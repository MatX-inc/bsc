module VerdictTest (runTests) where

import BscTestsuite.Verdict
import Control.Monad (unless)
import Data.List (isInfixOf)
import System.Exit (exitFailure)

runTests :: IO ()
runTests = do
  let simple = parse (summary ["Running one.exp ...", "PASS: alpha"] [("expected passes", 1)])
  one <- right "minimal complete summary" simple
  assert "suite-relative discovery" (manifestTests one == ["bsc.test/one.exp"])
  assert "legacy key" (manifestVerdicts one == [Verdict (CheckId "bsc.test/one.exp" "alpha" 1) PASS])
  right "identical manifests" (compareManifests one one) >>= assert "no differences" . null

  duplicates <- right "duplicate labels" (parse (summary
    ["Running one.exp ...", "PASS: same", "PASS: independent", "XFAIL: same"]
    [("expected passes", 2), ("expected failures", 1)]))
  assert "duplicates preserved and independently numbered"
    (map (checkOccurrence . verdictId) (manifestVerdicts duplicates) == [1, 1, 2])
  duplicateReorder <- right "independent label can move" (parse (summary
    ["Running one.exp ...", "PASS: independent", "PASS: same", "XFAIL: same"]
    [("expected passes", 2), ("expected failures", 1)]))
  right "label-local IDs survive unrelated order" (compareManifests duplicates duplicateReorder) >>= assert "reordering is irrelevant" . null

  fewer <- right "fewer checks" (parse (summary ["Running one.exp ...", "PASS: same"] [("expected passes", 1)]))
  diff <- right "compare duplicate population" (compareManifests duplicates fewer)
  assert "second identical occurrence is missing"
    (MissingCheck (CheckId "bsc.test/one.exp" "same" 2) XFAIL `elem` diff)

  sameCount <- right "different label, equal count" (parse (summary ["Running one.exp ...", "PASS: beta"] [("expected passes", 1)]))
  right "equal totals still compare identities" (compareManifests one sameCount) >>= assert "equal totals do not hide missing checks" . ((== 2) . length)

  withEmpty <- right "zero-check test retained" (parse (summary
    ["Running one.exp ...", "PASS: alpha", "Running empty.exp ..."] [("expected passes", 1)]))
  emptyDiff <- right "zero-check discovery difference" (compareManifests withEmpty one)
  assert "zero-check test disappearance detected" (MissingTest "bsc.test/empty.exp" `elem` emptyDiff)
  left "independent expected manifest catches shared omission" (validateDiscovery ["bsc.test/one.exp", "bsc.test/empty.exp"] one)
  right "matching expected discovery" (validateDiscovery ["bsc.test/one.exp"] one)
  left "duplicate expected discovery" (validateDiscovery ["bsc.test/one.exp", "bsc.test/one.exp"] one)

  absolute <- right "absolute .exp name" (parse (summary ["Running /checkout/testsuite/bsc.test/one.exp ...", "PASS: alpha"] [("expected passes", 1)]))
  assert "absolute .exp normalized" (absolute == one)
  dotted <- right "dot-dot inside root normalized" (parse (summary ["Running ../bsc.test/./one.exp ...", "PASS: alpha"] [("expected passes", 1)]))
  assert "lexical normalization" (dotted == one)
  left "path escapes suite root" (parse (summary ["Running ../../elsewhere.exp ...", "PASS: alpha"] [("expected passes", 1)]))
  left "absolute root sibling not accepted" (parse (summary ["Running /checkout/testsuite-other/one.exp ...", "PASS: alpha"] [("expected passes", 1)]))
  left "relative root rejected" (parseSummary "linux-iverilog" "testsuite" "/checkout/testsuite/bsc.test/testrun.sum" "anything\n")

  multiline <- right "multiline result" (parse (summary
    ["Running one.exp ...", "PASS: matched {first", "", "  second} exactly  "] [("expected passes", 1)]))
  assert "internal newline and whitespace preserved"
    (map (checkLabel . verdictId) (manifestVerdicts multiline) == ["matched {first\n\n  second} exactly  "])
  carriage <- right "CRLF" (parse (concatMap (\c -> if c == '\n' then "\r\n" else [c])
    (summary ["Running one.exp ...", "PASS: alpha"] [("expected passes", 1)])))
  assert "CRLF normalized" (carriage == one)

  categories <- right "all categories" (parse (summary
    ("Running one.exp ..." : [show disposition ++ ": " ++ show disposition | disposition <- [minBound .. maxBound :: Disposition]])
    [("expected passes", 1), ("unexpected failures", 1), ("expected failures", 1), ("unexpected successes", 1)
    ,("known failures", 1), ("unknown successes", 1), ("unresolved testcases", 1), ("untested testcases", 1), ("unsupported tests", 1)]))
  assert "all categories kept" (map verdictDisposition (manifestVerdicts categories) == [minBound .. maxBound])
  xfail <- right "xfail" (parse (summary ["Running one.exp ...", "XFAIL: alpha"] [("expected failures", 1)]))
  xpass <- right "xpass" (parse (summary ["Running one.exp ...", "XPASS: alpha"] [("unexpected successes", 1)]))
  right "expected failure transition" (compareManifests xfail xpass) >>=
    assert "XFAIL to XPASS remains visible" . (== [ChangedDisposition (CheckId "bsc.test/one.exp" "alpha" 1) XFAIL XPASS])

  diagnostics <- right "diagnostics" (parse (summary
    ["WARNING: before discovery", "Running one.exp ...", "PASS: alpha", "NOTE: detail", "ERROR: first", "  continued"]
    [("expected passes", 1)]))
  assert "diagnostic context and continuation preserved"
    (manifestDiagnostics diagnostics ==
      [Diagnostic Nothing Warning "before discovery", Diagnostic (Just "bsc.test/one.exp") Note "detail"
      ,Diagnostic (Just "bsc.test/one.exp") Error "first\n  continued"])
  right "diagnostic differences" (compareManifests one diagnostics) >>= assert "diagnostics compared" . ((== 3) . length)
  let duplicateDiagnostic = diagnostics { manifestDiagnostics = manifestDiagnostics diagnostics ++ take 1 (manifestDiagnostics diagnostics) }
  right "diagnostic multiplicity" (compareManifests diagnostics duplicateDiagnostic) >>= assert "duplicate diagnostic not deduplicated" . ((== 1) . length)

  left "empty summary" (parse "")
  left "no discoveries" (parse (summary [] []))
  left "missing Summary" (parse "Running one.exp ...\nPASS: alpha\n")
  left "truncated final line" (parse "Running one.exp ...\nPASS: alpha\n=== bsc Summary ===\n# of expected passes\t1")
  left "missing footer totals" (parse (summary ["Running one.exp ...", "PASS: alpha"] []))
  left "wrong footer totals" (parse (summary ["Running one.exp ...", "PASS: alpha"] [("expected passes", 2)]))
  left "unknown category" (parse (summary ["Running one.exp ...", "NEWRESULT: alpha"] []))
  left "verdict without discovery" (parse (summary ["PASS: alpha"] [("expected passes", 1)]))
  left "duplicate discovery" (parse (summary ["Running one.exp ...", "Running one.exp ..."] []))
  left "duplicate footer counter" (parse (summary ["Running one.exp ...", "PASS: alpha"] [("expected passes", 1), ("expected passes", 1)]))
  left "unknown footer counter" (parse (summary ["Running one.exp ..."] [("mystery results", 1)]))
  left "malformed Running while label pending" (parse (summary ["Running one.exp ...", "PASS: alpha", "Running two.exp"] [("expected passes", 1)]))
  left "multiple summaries" (parse (summary ["Running one.exp ...", "PASS: alpha"] [("expected passes", 1)] ++ "=== bsc Summary ===\n"))
  left "result after summary" (parse (summary ["Running one.exp ..."] [] ++ "PASS: late\n"))
  left "counter before summary" (parse "Running one.exp ...\n# of expected passes 1\n=== bsc Summary ===\n")
  left "empty configuration" (parseSummary " " "/checkout/testsuite" "/checkout/testsuite/bsc.test/testrun.sum" (summary ["Running one.exp ..."] []))
  left "non-ASCII counter digit rejected without exception" (parse ("Running one.exp ...\n=== bsc Summary ===\n# of expected passes\t\x0661\n"))

  left "configuration mismatch" (compareManifests one one { manifestConfiguration = "macos-iverilog" })
  left "duplicate structured IDs" (validateManifest one { manifestVerdicts = manifestVerdicts one ++ manifestVerdicts one })
  left "undiscovered check" (validateManifest one { manifestTests = ["other.exp"] })
  left "noncanonical structured path" (validateManifest one { manifestTests = ["bsc.test/../other.exp"] })
  left "empty merge" (mergeManifests [])
  left "overlapping summaries" (mergeManifests [one, one])
  other <- right "second directory" (parseSummary "linux-iverilog" "/checkout/testsuite" "/checkout/testsuite/bsc.other/testrun.sum"
    (summary ["Running one.exp ...", "PASS: alpha"] [("expected passes", 1)]))
  combined <- right "disjoint summaries merge" (mergeManifests [one, other])
  assert "same basename in two directories distinct" (length (manifestTests combined) == 2)
  left "merge configuration mismatch" (mergeManifests [one, other { manifestConfiguration = "different" }])

  roundTrip "JSON baseline" one
  roundTrip "JSON all dispositions" categories
  roundTrip "JSON duplicate labels" duplicates
  roundTrip "JSON diagnostics" diagnostics
  roundTrip "JSON multiline" multiline
  let unicode = one { manifestVerdicts = [Verdict (CheckId "bsc.test/one.exp" "quote\" backslash\\ \x1f642\n\t\0" 1) PASS] }
  roundTrip "JSON escaping and Unicode" unicode
  left "malformed JSON" (decodeManifest "{")
  left "non-JSON whitespace" (decodeManifest ('\v' : encodeManifest one))
  left "trailing JSON garbage" (decodeManifest (encodeManifest one ++ "null"))
  left "JSON leading-zero integer" (decodeManifest (replace "\"version\":1" "\"version\":01" (encodeManifest one)))
  left "JSON trailing object comma" (decodeManifest (replace "\"version\":1" "\"version\":1," (encodeManifest one)))
  left "JSON trailing array comma" (decodeManifest (replace "\"tests\":[\"bsc.test/one.exp\"]" "\"tests\":[\"bsc.test/one.exp\",]" (encodeManifest one)))
  left "unknown schema version" (decodeManifest (replace "\"version\":1" "\"version\":2" (encodeManifest one)))
  left "unknown identity version" (decodeManifest (replace "dejagnu-label-v1" "semantic-v2" (encodeManifest one)))
  left "duplicate JSON fields" (decodeManifest (replace "\"version\":1" "\"version\":1,\"version\":1" (encodeManifest one)))
  left "unknown JSON field" (decodeManifest (replace "\"version\":1" "\"version\":1,\"extra\":null" (encodeManifest one)))
  left "malformed JSON occurrence" (decodeManifest (replace "\"occurrence\":1" "\"occurrence\":0" (encodeManifest one)))
  left "unknown JSON disposition" (decodeManifest (replace "\"disposition\":\"PASS\"" "\"disposition\":\"SKIP\"" (encodeManifest one)))
  left "JSON duplicate check IDs" (decodeManifest (encodeManifest one { manifestVerdicts = manifestVerdicts one ++ manifestVerdicts one }))
  escaped <- right "JSON UTF-16 surrogate pair" (decodeManifest (replace "\"label\":\"alpha\"" "\"label\":\"\\ud83d\\ude42\"" (encodeManifest one)))
  assert "surrogate pair decoded" (map (checkLabel . verdictId) (manifestVerdicts escaped) == ["\x1f642"])
  left "unpaired surrogate rejected" (decodeManifest (replace "\"label\":\"alpha\"" "\"label\":\"\\ud83d\"" (encodeManifest one)))
  assert "JSON schema is explicit" ("\"identity\":\"dejagnu-label-v1\"" `isInfixOf` encodeManifest one)
  let suiteScale = Manifest "suite-scale" ["bsc.test/scale.exp"]
        [Verdict (CheckId "bsc.test/scale.exp" ("check " ++ show index ++ "\nquoted \"value\" \\ \x1f642") 1)
                 (if even index then PASS else XFAIL)
        | index <- [1 .. 20000 :: Int]] []
  roundTrip "suite-scale JSON round trip" suiteScale
  putStrLn "Verdict tests passed"

parse :: String -> Either String Manifest
parse = parseSummary "linux-iverilog" "/checkout/testsuite" "/checkout/testsuite/bsc.test/testrun.sum"

summary :: [String] -> [(String, Int)] -> String
summary body totals = unlines
  (["Test run by test on fixed timestamp", "Native configuration is x86_64-test-linux", ""] ++ body ++
   ["", "\t\t=== bsc Summary ===", ""] ++ ["# of " ++ name ++ "\t" ++ show n | (name, n) <- totals])

assert :: String -> Bool -> IO ()
assert label condition = unless condition (putStrLn ("FAIL: " ++ label) >> exitFailure)

right :: String -> Either String a -> IO a
right _ (Right value) = pure value
right label (Left problem) = putStrLn ("FAIL: " ++ label ++ ": " ++ problem) >> exitFailure

left :: String -> Either String a -> IO ()
left _ (Left _) = pure ()
left label (Right _) = putStrLn ("FAIL: expected rejection: " ++ label) >> exitFailure

roundTrip :: String -> Manifest -> IO ()
roundTrip label manifest = do
  decoded <- right label (decodeManifest (encodeManifest manifest))
  differences <- right label (compareManifests manifest decoded)
  assert label (null differences)

replace :: String -> String -> String -> String
replace needle replacement = walk
  where
    walk [] = []
    walk input@(c : rest)
      | take (length needle) input == needle = replacement ++ walk (drop (length needle) input)
      | otherwise = c : walk rest
