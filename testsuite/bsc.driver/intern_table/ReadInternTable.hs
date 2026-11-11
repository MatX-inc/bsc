-- ReadInternTable.hs TABLE SYNTHETIC: readInternTable accepts TABLE and
-- exactly the bytes renderInternTable writes, and rejects every other form
-- of a table; writes a table of awkward spellings to SYNTHETIC for
-- RestoreRoundTrip.hs.
import Control.Monad(forM_, unless)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Either(isLeft)
import Data.List(isPrefixOf)
import System.Environment(getArgs)
import System.Exit(exitFailure)

import InternRestore(decodeInternTable, readInternTable)
import SpeedyString(InternSnapshot(..), renderInternTable)

main :: IO ()
main = do
    [table, synthetic] <- getArgs
    r <- readInternTable table
    case r of
      Left problem -> do putStrLn ("FAIL: " ++ table ++ ": " ++ problem)
                         exitFailure
      Right snap -> check "the table is not empty" (internNextId snap > 0)
    missing <- readInternTable "no-such.intern.json"
    report "missing file" missing
    check "a missing file is rejected" (isLeft missing)

    let Right bytes = renderInternTable awkward
    check "awkward spellings round-trip"
          (decodeInternTable bytes == Right awkward)
    BLC.writeFile synthetic bytes

    let text = BLC.unpack bytes
    forM_ (variants text) $ \(what, t) -> do
      let v = decodeInternTable (BB.toLazyByteString (BB.stringUtf8 t))
      report what v
      check ("rejected: " ++ what) (isLeft v)

    -- renderInternTable writes a surrogate Char as a lone escape, which
    -- JSON text cannot hold
    forM_ ["x\xDC80y", "\xD800", "\xDBFFz"] $ \s -> do
      let Right lone = renderInternTable (InternSnapshot 2 [(0, "a"), (1, s)])
          v = decodeInternTable lone
      report ("lone surrogate in " ++ show s) v
      check "a lone surrogate escape is rejected, naming its line"
            (either ("line 3 escapes a surrogate" `isPrefixOf`) (const False) v)

-- Control characters, quote and backslash, DEL, Latin-1, a BMP symbol,
-- U+FFFF, a character outside the BMP (a surrogate pair in JSON), and
-- generated spellings (the typechecker's names and ITransform's CSE
-- names), canonical and not.
awkward :: InternSnapshot
awkward = InternSnapshot (length ss) (zip [0 ..] ss)
  where ss = [ "", "plain", "a/b", "q\"b\\s", "\n\r\t\x01\x1F", "\DEL", "\xE9\xFF"
             , "\x2218", "\xFFFF", "\x1F600", "_tctyvar3", "_tcdict0"
             , "_tctemp12", "_tctyvar03", "__i7", "__j0", "__i07", "__j"
             , "___i3", "__j999999999999999", "__i1000000000000000", " " ]

-- Other forms of the same table, or valid JSON that is not a valid table.
variants :: String -> [(String, String)]
variants t =
    [ ("an extra key", sub "{\"internNextId\"" "{\"x\":1,\"internNextId\"" t)
    , ("no internNextId", sub "\"internNextId\":22," "" t)
    , ("keys reordered", reordered)
    , ("a space after a colon", sub "\"internNextId\":" "\"internNextId\": " t)
    , ("no final newline", init t)
    , ("trailing text", t ++ "x")
    , ("an id written 1.0", sub "\n[1," "\n[1.0," t)
    , ("an id written 1e0", sub "\n[1," "\n[1e0," t)
    , ("a count written 22.0", sub ":22," ":22.0," t)
    , ("an escaped letter", sub "\"plain\"" "\"pl\\u0061in\"" t)
    , ("an escaped slash", sub "\"a/b\"" "\"a\\/b\"" t)
    , ("a control character in hex case", sub "\\u001f" "\\u001F" t)
    , ("a raw character outside the BMP", sub "\\ud83d\\ude00" "\x1F600" t)
    , ("an entry of three elements", sub "\n[1,\"plain\"]" "\n[1,\"plain\",0]" t)
    , ("an id that is a string", sub "\n[1," "\n[\"1\"," t)
    , ("a spelling that is a number", sub "\n[1,\"plain\"]" "\n[1,7]" t)
    , ("a repeated spelling", sub "\"plain\"" "\"a/b\"" t)
    , ("ids out of order", sub "\n[2,\"a/b\"]" "\n[3,\"a/b\"]" t)
    , ("a count too large", sub ":22," ":23," t)
    , ("an entry missing", sub ",\n[21,\" \"]" "" t)
    , ("a negative count", sub ":22," ":-1," t)
    ]
  where
    reordered =
        let rest = drop (length "{\"internNextId\":22,") t
            body = take (length rest - 2) rest  -- the entries, without "}\n"
        in  "{" ++ body ++ ",\"internNextId\":22}\n"

-- replace the first occurrence, which must exist
sub :: String -> String -> String -> String
sub old new s = go s
  where go [] = error ("no " ++ show old)
        go r@(c : cs) | take (length old) r == old = new ++ drop (length old) r
                      | otherwise = c : go cs

report :: String -> Either String a -> IO ()
report what r = putStrLn (what ++ ": " ++ either id (const "accepted") r)

check :: String -> Bool -> IO ()
check what ok = unless ok $ do putStrLn ("FAIL: " ++ what)
                               exitFailure
