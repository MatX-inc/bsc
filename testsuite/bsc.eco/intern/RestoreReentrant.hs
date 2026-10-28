module Main(main) where

import Control.Exception(evaluate)
import InternTest(assert)
import qualified SpeedyString as S

main :: IO ()
main = do
    spelling <- readFile "work_intern/spelling"
    captured <- read <$> readFile "work_intern/reentrant"
    assert "reentrant capture contains duplicate spellings"
        (captured == S.InternSnapshot 1 [(0,spelling)])
    S.restoreInternState captured
    prior <- evaluate (S.fromString spelling)
    actual <- S.snapshotInternState
    assert "unique reentrant state was not preserved" (actual == captured)
    next <- evaluate (S.fromString "after-reentrant-restore")
    assert "counter did not follow the restored ID" (next > prior)
    after <- S.snapshotInternState
    assert "restored reentrant allocation did not continue at the saved counter"
        (after == S.InternSnapshot 2 [(0,spelling),(1,"after-reentrant-restore")])
