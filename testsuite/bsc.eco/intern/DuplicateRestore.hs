module Main(main) where

import InternTest(assert, mustReject)
import qualified SpeedyString as S

main :: IO ()
main = do
    mapM_ (\snapshot -> do
        mustReject snapshot
        actual <- S.snapshotInternState
        assert "rejected duplicate spellings changed the empty intern table"
            (actual == S.InternSnapshot 0 []))
        [ S.InternSnapshot 2 [(0,"duplicate"),(1,"duplicate")]
        , S.InternSnapshot 3 [(0,"duplicate"),(1,"between"),(2,"duplicate")]]
    let valid = S.InternSnapshot 2 [(0,"duplicate"),(1,"between")]
    S.restoreInternState valid
    actual <- S.snapshotInternState
    assert "duplicate-spelling rejection prevented a valid restore" (actual == valid)
