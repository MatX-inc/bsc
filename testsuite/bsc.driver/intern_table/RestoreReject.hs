-- RestoreReject.hs MODE: one restore scenario per fresh process.
--   invalid    invalid snapshots are rejected and change nothing; a valid
--              one then restores, and a second restore is rejected
--   late       a restore after a string is interned is rejected
--   empty      an empty snapshot restores and changes nothing, so another
--              restore may follow it
--   generated  generated spellings restore as plain entries, and a name
--              noted after the restore is placed after every restored entry
--   lazycount  a snapshot whose internNextId interns a string when it is
--              evaluated is rejected, and none of its entries is restored
import Control.Exception(ErrorCall, evaluate, try)
import Control.Monad(forM_, unless)
import Data.Either(isLeft, isRight)
import System.Environment(getArgs)
import System.Exit(exitFailure)

import SpeedyString(InternSnapshot(..), fromString, internCount,
                    restoreInternState, snapshotInternState)

main :: IO ()
main = do
    [mode] <- getArgs
    case mode of
      "invalid" -> do
        forM_ invalid $ \snap -> do
          r <- restoreInternState snap
          report "invalid snapshot" r
          check ("rejected: " ++ show snap) (isLeft r)
          untouched
        let throwing = InternSnapshot 2 [(0, "a"), (1, 'b' : error "boom")]
        r <- try (restoreInternState throwing)
        check "a snapshot holding an error throws"
              (isLeft (r :: Either ErrorCall (Either String ())))
        untouched
        ok <- restoreInternState sample
        check "the table restores" (ok == Right ())
        let ss = map (fromString . snd) (internEntries sample)
        check "in snapshot order, not spelling order"
              (and (zipWith (<) ss (drop 1 ss)))
        again <- restoreInternState sample
        report "second restore" again
        check "a second restore is rejected" (isLeft again)
        after <- snapshotInternState
        check "the second restore changes nothing" (after == sample)
      "late" -> do
        _ <- evaluate (fromString "early")
        r <- restoreInternState sample
        report "late restore" r
        check "a restore after interning is rejected" (isLeft r)
        after <- snapshotInternState
        check "the table still holds only that string"
              (after == InternSnapshot 1 [(0, "early")])
      "empty" -> do
        r <- restoreInternState (InternSnapshot 0 [])
        check "an empty snapshot restores" (isRight r)
        untouched
        again <- restoreInternState (InternSnapshot 0 [])
        check "a second empty snapshot restores" (isRight again)
        untouched
        full <- restoreInternState sample
        check "a snapshot restores after an empty one" (isRight full)
        after <- snapshotInternState
        check "the table is that snapshot" (after == sample)
      "generated" -> do
        let spellings = ["a", "_tctyvar5", "_tcdict2", "_tctyvar05", "b"]
            snap = InternSnapshot 5 (zip [0 ..] spellings)
        r <- restoreInternState snap
        check "generated spellings restore" (isRight r)
        let ss = map fromString spellings
        check "in snapshot order" (and (zipWith (<) ss (drop 1 ss)))
        c <- evaluate (fromString "c")
        v9 <- evaluate (fromString "_tctyvar9")
        check "_tctyvar5 is its restored entry"
              (case drop 1 ss of (s1:_) -> fromString "_tctyvar5" == s1; [] -> False)
        check "strings interned after the restore follow every restored entry"
              (case reverse ss of (sLast:_) -> sLast < c && c < v9; [] -> False)
        after <- snapshotInternState
        check "the snapshot appends newly interned names in order"
              (after == InternSnapshot 7
                          (zip [0 ..] (spellings ++ ["c", "_tctyvar9"])))
      "lazycount" -> do
        let snap = sample { internNextId = fromString "count" `seq` 4 }
        r <- restoreInternState snap
        report "snapshot whose count interns" r
        check "a snapshot whose count interns a string is rejected" (isLeft r)
        after <- snapshotInternState
        check "the table holds only the string the count interned"
              (after == InternSnapshot 1 [(0, "count")])
      _ -> fail ("unknown mode " ++ mode)

sample :: InternSnapshot
sample = InternSnapshot 4 (zip [0 ..] ["z", "_tctyvar3", "a/b", "\x2218"])

invalid :: [InternSnapshot]
invalid =
    [ InternSnapshot (-1) []
    , InternSnapshot 2 [(0, "a")]
    , InternSnapshot 1 [(0, "a"), (1, "b")]
    , InternSnapshot 2 [(1, "a"), (0, "b")]
    , InternSnapshot 2 [(0, "a"), (1, "a")]
    , InternSnapshot 3 [(0, "a"), (2, "b"), (1, "c")]
    , InternSnapshot 2 [(0, "_tctyvar1"), (1, "_tctyvar1")]
    ]

-- nothing interned (a noted name never spelled is not in a snapshot)
untouched :: IO ()
untouched = do
    n <- internCount
    snap <- snapshotInternState
    check "the table is unchanged" (n == 0 && snap == InternSnapshot 0 [])

report :: String -> Either String () -> IO ()
report what r = putStrLn (what ++ ": " ++ either id (const "accepted") r)

check :: String -> Bool -> IO ()
check what ok = unless ok $ do putStrLn ("FAIL: " ++ what)
                               exitFailure
