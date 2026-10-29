module Main(main) where

import Control.Exception(evaluate)
import InternTest(assert)
import qualified SpeedyString as S

-- Keep the inner ID available without forcing it before the outer lookup.
-- The runtime fixture and NOINLINE prevent folding away the nested lookup.
{-# NOINLINE nested #-}
nested :: S.SString -> String
nested = S.toString

main :: IO ()
main = do
    spelling <- readFile "work_intern/spelling"
    let inner = S.fromString spelling
    outer <- evaluate (S.fromString (nested inner))
    original <- evaluate inner
    assert "reentrant interning assigned different IDs to one spelling" (outer == original)
    snapshot <- S.snapshotInternState
    assert "reentrant interning allocated more than one entry"
        (snapshot == S.InternSnapshot 1 [(0,spelling)])
    writeFile "work_intern/reentrant" (show snapshot)
