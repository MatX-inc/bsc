module InternTest(assert, spellings, forceStrings, mustReject) where

import Control.Exception(evaluate, try, IOException)
import Control.Monad(unless)
import qualified SpeedyString as S

assert :: String -> Bool -> IO ()
assert msg ok = unless ok (ioError (userError msg))

-- aR and bA intentionally collide under the interner's hashStr.
spellings :: [String]
spellings = ["unused-but-must-survive", "z", "a", "", "space /\\\"\n\t", "lambda-\955", "a\NULb", "aR", "bA"]

forceStrings :: [String] -> IO [S.SString]
forceStrings = mapM (evaluate . S.fromString)

mustReject :: S.InternSnapshot -> IO ()
mustReject snapshot = do
    result <- try (S.restoreInternState snapshot) :: IO (Either IOException ())
    assert "unsafe/invalid restore was accepted" (either (const True) (const False) result)
