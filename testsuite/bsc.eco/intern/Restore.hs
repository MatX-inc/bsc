module Main(main) where

import Control.Exception(evaluate)
import InternTest
import qualified SpeedyString as S

main :: IO ()
main = do
    captured <- read <$> readFile "work_intern/state"
    S.restoreInternState captured
    -- Query in reverse order, so fresh insertion cannot fake the order.
    restored <- forceStrings (reverse spellings)
    assert "relative interned ID order was not restored" (and (zipWith (>) restored (tail restored)))
    assert "unusual spellings changed" (map S.toString restored == reverse spellings)
    next <- evaluate (S.fromString "allocated-after-restore")
    assert "allocation did not continue after saved counter" (all (< next) restored)
    after <- S.snapshotInternState
    assert "allocation continuation/table mismatch" (after == S.InternSnapshot (length spellings + 1) (zip [0..] (spellings ++ ["allocated-after-restore"])))
    mustReject captured
