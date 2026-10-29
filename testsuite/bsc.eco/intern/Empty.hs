module Main(main) where

import InternTest(assert, forceStrings, mustReject)
import qualified SpeedyString as S

main :: IO ()
main = do
    S.restoreInternState (S.InternSnapshot 0 [])
    actual <- S.snapshotInternState
    assert "empty round trip failed" (actual == S.InternSnapshot 0 [])
    mustReject actual
    _ <- forceStrings ["first-after-empty-restore", "second-after-empty-restore"]
    allocated <- S.snapshotInternState
    assert "allocation after empty restore did not start at zero"
        (allocated == S.InternSnapshot 2
            [(0,"first-after-empty-restore"),(1,"second-after-empty-restore")])
