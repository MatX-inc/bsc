{-# LANGUAGE OverloadedStrings #-}
-- | Reading the intern table that bsc -trace-intern-table writes, and
-- restoring it in a program run against the bsc library (util/bluehs):
--
-- > main = withRestoredInterns "Top.intern.json" $ do
-- >     ... -- decode the .ba, run passes: SStrings compare as they did in bsc
--
-- SString's Ord compares interning ids, not spellings, so a program that
-- decodes a compile's .bo or .ba files and runs passes over them orders
-- names as that compile did only if it restores the compile's table first.
module InternRestore(
    readInternTable, decodeInternTable,
    restoreInternTable, withRestoredInterns
  ) where

import Control.Exception(IOException, try)
import Control.Monad(unless, zipWithM)
import Data.Foldable(toList)
import Data.List(sort)
import Numeric(readHex)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types(Parser, JSONPathElement(Index), (<?>),
                        explicitParseField)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC

import SpeedyString(InternSnapshot(..), snapshotProblem, restoreInternState,
                    renderInternTable)

-- | The object renderInternTable writes: exactly the keys internNextId and
-- internEntries, each entry an [id, spelling] pair, and the snapshot valid
-- (SpeedyString.validSnapshot).  A spelling is JSON text, so it cannot hold a
-- surrogate Char, which renderInternTable writes as a lone \\u escape; aeson
-- rejects that escape.  An orphan, so that SpeedyString needs no aeson.
instance A.FromJSON InternSnapshot where
    parseJSON = A.withObject "intern table" $ \o -> do
        let keys = sort (map AK.toString (KM.keys o))
        unless (keys == ["internEntries", "internNextId"]) $
            fail ("the keys are " <> show keys <>
                  ", not internNextId and internEntries")
        n <- o A..: "internNextId"
        es <- explicitParseField (A.withArray "internEntries" entries) o
                                 "internEntries"
        let snap = InternSnapshot n es
        maybe (return snap) fail (snapshotProblem snap)
      where
        entries a = zipWithM (\i v -> entry v <?> Index i) [0 ..] (toList a)
        entry :: A.Value -> Parser (Int, String)
        entry = A.withArray "entry" $ \a ->
            case toList a of
              [i, s] -> (,) <$> A.parseJSON i <*> A.parseJSON s
              _ -> fail ("an entry of " <> show (length a) <>
                         " elements, not [id, spelling]")

-- | The snapshot a table holds, if the bytes are exactly those
-- renderInternTable writes for it (and so for no other snapshot).
decodeInternTable :: BL.ByteString -> Either String InternSnapshot
decodeInternTable bytes = do
    snap <- either (Left . explain) Right (A.eitherDecode' bytes)
    canonical <- renderInternTable snap
    unless (canonical == bytes) $ Left (difference canonical)
    return snap
  where
    explain problem = maybe problem loneSurrogate (lineOfLoneSurrogate bytes)
    loneSurrogate line =
        "line " <> show line <> " escapes a surrogate on its own, which " <>
        "renderInternTable writes for a surrogate Char and JSON text cannot hold"
    difference canonical =
        let k = commonPrefix canonical bytes
            line = 1 + BLC.count '\n' (BL.take k bytes)
        in  "line " <> show line <> " (byte " <> show k <>
            ") differs from the table's form as -trace-intern-table writes it"
    commonPrefix a b = fromIntegral (length (takeWhile id (BL.zipWith (==) a b)))

-- The first line with a \u escape of a surrogate that is not half of a
-- high-low pair.
lineOfLoneSurrogate :: BL.ByteString -> Maybe Int
lineOfLoneSurrogate = go 1 . BLC.unpack
  where
    go :: Int -> String -> Maybe Int
    go _ [] = Nothing
    go l ('\n' : r) = go (l + 1) r
    go l ('\\' : 'u' : r)
        | Just u <- hex4 r =
            case drop 4 r of
              '\\' : 'u' : r'
                | isHigh u, Just w <- hex4 r', isLow w -> go l (drop 4 r')
              r' | isHigh u || isLow u -> Just l
                 | otherwise -> go l r'
    go l ('\\' : _ : r) = go l r
    go l (_ : r) = go l r
    hex4 r = let h = take 4 r
             in  case readHex h of
                   [(v, "")] | length h == 4 -> Just (v :: Int)
                   _ -> Nothing
    isHigh u = u >= 0xD800 && u < 0xDC00
    isLow u = u >= 0xDC00 && u < 0xE000

-- | decodeInternTable of a file; an unreadable file is a Left.
readInternTable :: FilePath -> IO (Either String InternSnapshot)
readInternTable path = do
    r <- try (BS.readFile path)
    return $ case r of
      Left e -> Left (show (e :: IOException))
      Right bytes -> decodeInternTable (BL.fromStrict bytes)

-- | Read a table and restore it (SpeedyString.restoreInternState), which must
-- come before anything in the process is interned.
restoreInternTable :: FilePath -> IO (Either String ())
restoreInternTable path =
    readInternTable path >>= either (return . Left) restoreInternState

-- | Restore the table at the path, then run the action; a table that cannot
-- be read or restored is an IOError naming the path.  Call it first in main.
withRestoredInterns :: FilePath -> IO a -> IO a
withRestoredInterns path act = do
    r <- restoreInternTable path
    case r of
      Left problem -> ioError (userError (path <> ": " <> problem))
      Right () -> act
