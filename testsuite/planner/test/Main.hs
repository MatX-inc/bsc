module Main (main) where

import qualified CensusTest
import qualified VerdictTest

main :: IO ()
main = do
  VerdictTest.runTests
  CensusTest.runTests
