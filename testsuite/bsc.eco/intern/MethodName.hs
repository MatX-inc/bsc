module Main(main) where

import Control.Exception(evaluate)
import qualified Data.Set as Set
import ASyntax(mkMethStr, MethodPart(..))
import FStringCompat(getFString, mkFString)
import Id(mk_homeless_id)
import InternTest(assert)
import qualified SpeedyString as S

main :: IO ()
main = do
    spelling <- readFile "work_intern/spelling"
    let objectName = spelling ++ "_object"
        methodName = spelling ++ "_method"
    object <- evaluate (mk_homeless_id objectName)
    method <- evaluate (mk_homeless_id methodName)
    result <- evaluate (mkMethStr object method (Just 7) (MethodResult Nothing))
    let expected = objectName ++ "$" ++ methodName ++ "_7"
    assert "method result name changed" (getFString result == expected)
    regenerated <- evaluate (mkFString expected)
    assert "method result name has inconsistent intern IDs" (result == regenerated)
    snapshot <- S.snapshotInternState
    let names = map snd (S.internEntries snapshot)
    assert "method name construction duplicated its intermediate name"
        (length (filter (== methodName ++ "_7") names) == 1)
    assert "method name construction duplicated an interned spelling"
        (Set.size (Set.fromList names) == length names)
