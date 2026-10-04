module PlanGoldenTest (runTests) where

import BscTestsuite.Lower
import BscTestsuite.TestPlan
import Control.Monad (forM_, unless)

runTests :: IO ()
runTests = do
  forM_ ["basic", "negative", "variants", "empty"] $ \name -> do
    source <- readFile (fixture (name ++ ".tcl"))
    golden <- readFile (fixture (name ++ ".plan.json"))
    expected <- either (fail . ("invalid plan golden: " ++)) pure (decodePlan golden)
    actual <- either (fail . unlines . map renderIssue) pure
      (lowerPlan (PlanConfig "golden" True []) [("bsc.plan/" ++ name ++ ".exp", source)])
    unless (actual == expected) $ fail (name ++ " plan differs from its golden")
    unless (decodePlan (encodePlan actual) == Right expected) $
      fail (name ++ " plan does not round-trip")
    if name /= "basic" then pure () else do
      let identifier = checkId (last (scenarioChecks (head (planScenarios actual))))
      explanation <- either fail pure (explainCheck actual (renderIdentifier identifier))
      expectedExplanation <- readFile (fixture "basic.explain.txt")
      unless (explanation == expectedExplanation) $ fail "explanation differs from its golden"
  putStrLn "Plan and explanation golden tests passed."
  where fixture name = "test/fixtures/plan/" ++ name
