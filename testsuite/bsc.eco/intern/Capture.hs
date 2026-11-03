module Main(main) where

import InternTest
import qualified SpeedyString as S

main :: IO ()
main = do
    empty <- S.snapshotInternState
    assert "initial table is not empty" (empty == S.InternSnapshot 0 [])
    _ <- forceStrings spellings
    captured <- S.snapshotInternState
    assert "snapshot omitted entries/counter" (captured == S.InternSnapshot (length spellings) (zip [0..] spellings))
    writeFile "work_intern/state" (show captured)
