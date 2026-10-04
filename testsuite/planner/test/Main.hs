module Main (main) where

import qualified CensusTest
import qualified VerdictTest
import qualified TestPlanTest
import qualified LowerTest
import qualified PlanGoldenTest
import qualified CorrelateTest

main :: IO ()
main = do
  VerdictTest.runTests
  CensusTest.runTests
  TestPlanTest.runTests
  LowerTest.runTests
  PlanGoldenTest.runTests
  CorrelateTest.runTests
