-- RestoreRoundTrip.hs TABLE OUT: restore TABLE in this fresh process,
-- check that its spellings intern to themselves in table order, and write
-- the table snapshotted after restoring to OUT (the .exp compares it with
-- TABLE byte for byte).
import Control.Monad(unless)
import qualified Data.ByteString.Lazy as BL
import System.Environment(getArgs)
import System.Exit(exitFailure)

import InternRestore(readInternTable, withRestoredInterns)
import SpeedyString(InternSnapshot(..), fromString, toString, internCount,
                    snapshotInternState, renderInternTable)

main :: IO ()
main = do
    [table, out] <- getArgs
    Right expected <- readInternTable table
    withRestoredInterns table $ do
        snap <- snapshotInternState
        check "the snapshot after restoring is the table" (snap == expected)
        let spellings = map snd (internEntries expected)
            ss = map fromString spellings
        check "restored spellings compare in table order"
              (and (zipWith (<) ss (drop 1 ss)))
        check "restored spellings spell themselves" (map toString ss == spellings)
        n <- internCount
        check "interning restored spellings adds nothing"
              (n == internNextId expected)
        either fail (BL.writeFile out) (renderInternTable snap)

check :: String -> Bool -> IO ()
check what ok = unless ok $ do putStrLn ("FAIL: " ++ what)
                               exitFailure
