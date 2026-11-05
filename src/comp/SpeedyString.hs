{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns #-}
module SpeedyString(SString, toString, fromString, (++), concat, filter,
                    InternSnapshot(..), snapshotInternState, validSnapshot,
                    renderInternTable) where

import Prelude hiding((++), concat, filter)
import qualified Prelude((++), filter)
import IOMutVar(MutableVar, newVar, readVar, writeVar)
import System.IO.Unsafe(unsafePerformIO)
import qualified Data.IntMap.Strict as M
import qualified Data.Set as Set
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import Data.Char(chr, ord, toUpper)
import Data.List(intersperse)
import Numeric(showHex)
-- import qualified NotSoSpeedyString
import ErrorUtil (internalError)
import qualified Data.Generics as Generic


data SString = SString !Int -- unique id
   deriving (Generic.Data, Generic.Typeable)

instance Eq SString where
    (SString i) == (SString i') = i == i'

-- note that Ord is not the usual string ordering
instance Ord SString where
    compare (SString i) (SString i') = compare i i'

instance Show SString where
    show = show . toString

-- public

toString :: SString -> String
toString (SString id) = unsafePerformIO $
                        do m <- readVar strings
                           return $ M.findWithDefault err id m

fromString :: String -> SString
fromString s = unsafePerformIO $
               do m <- readVar sstrings
                  return $ maybe (newSString s) id $ M.lookup (hashStr s) m >>= lookup s

(++) :: SString -> SString -> SString
s ++ s' = fromString $ (toString s) Prelude.++ (toString s')

concat :: [SString] -> SString
concat = fromString . concatMap toString

filter :: (Char -> Bool) -> SString -> SString
filter pred s = fromString $ Prelude.filter pred (toString s)

-- private

newSString :: String -> SString
newSString s = unsafePerformIO $
               do id <- freshInt
                  let ss = SString id
                  sm <- readVar strings
                  ssm <- readVar sstrings
                  let !sm'  = M.insert id s sm
                      !ssm' = M.insertWith (Prelude.++)
                                          (hashStr s) [(s,ss)] ssm
                  writeVar strings sm'
                  writeVar sstrings ssm'
                  return ss

err :: a
err = internalError "SpeedyString: inconsistent representation"

--toNotSoSpeedyString :: SString -> NotSoSpeedyString.SString
--toNotSoSpeedyString speedy = NotSoSpeedyString.fromString (toString speedy)

--fromNotSoSpeedyString :: NotSoSpeedyString.SString -> SString
--fromNotSoSpeedyString not_so_speedy =
--    fromString (NotSoSpeedyString.toString not_so_speedy)

-- internal representation

strings :: MutableVar (M.IntMap String)
strings = unsafePerformIO $ newVar (M.empty)

sstrings :: MutableVar (M.IntMap [(String, SString)])
sstrings = unsafePerformIO $ newVar (M.empty)


-- string hash function, stolen from FString

hashStr :: String -> Int
hashStr s = f s 0
        where f "" r = r
              f (c:cs) r = f cs (r*16+r+fromEnum c)


-- unique id factory

nextInt :: MutableVar Int
nextInt = unsafePerformIO $ (newVar 0)

freshInt :: IO Int
freshInt = do fresh <- readVar nextInt
              writeVar nextInt (fresh + 1)
              return fresh


-- A capture contains every spelling, including entries no longer reachable
-- from the scheduled package. Numeric IDs must be preserved: SString's Ord
-- instance compares interned IDs, not string contents.
data InternSnapshot = InternSnapshot
    { internNextId :: Int
    , internEntries :: [(Int, String)]
    } deriving (Eq, Read, Show)

-- Call only once ordinary output has been fully written.  Like the existing
-- interner, this requires a single compiler thread.
snapshotInternState :: IO InternSnapshot
snapshotInternState = do
    n <- readVar nextInt
    sm <- readVar strings
    return (InternSnapshot n (M.toAscList sm))

validSnapshot :: InternSnapshot -> Bool
validSnapshot (InternSnapshot n entries) =
    n >= 0 && map fst entries == [0 .. n - 1] &&
    Set.size (Set.fromList (map snd entries)) == n

-- Shaped like aeson's derived encoding, one entry per line, ASCII so that no
-- locale applies.  JSON reads a high then a low surrogate as one code point.
renderInternTable :: InternSnapshot -> Either String BL.ByteString
renderInternTable (InternSnapshot n entries) =
    case [ (i, h, l) | (i, s) <- entries, (h, l) <- zip s (drop 1 s),
                       h >= '\xD800' && h <= '\xDBFF',
                       l >= '\xDC00' && l <= '\xDFFF' ] of
      (i, h, l) : _ ->
          Left ("entry " <> show i <> " holds " <> codePoint h <> " then " <>
                codePoint l <> ", which JSON would read as " <>
                codePoint (chr (0x10000 + (ord h - 0xD800) * 0x400 +
                                (ord l - 0xDC00))))
      [] -> Right (BB.toLazyByteString document)
  where
    document = BB.string7 "{\"internNextId\":" <> BB.intDec n <>
               BB.string7 ",\"internEntries\":[" <>
               mconcat (intersperse (BB.char7 ',') (map entry entries)) <>
               BB.string7 "\n]}\n"
    entry (i, s) = BB.string7 "\n[" <> BB.intDec i <> BB.char7 ',' <>
                   jsonString s <> BB.char7 ']'
    codePoint c = "U+" <> map toUpper (showHex (ord c) "")

-- Control characters as aeson writes them.  A surrogate Char (from a non-UTF-8
-- file name or a \x escape) gets its own escape, which aeson's decoder rejects.
jsonString :: String -> BB.Builder
jsonString s = BB.char7 '"' <> foldMap escape s <> BB.char7 '"'
  where
    escape '"' = BB.string7 "\\\""
    escape '\\' = BB.string7 "\\\\"
    escape '\n' = BB.string7 "\\n"
    escape '\r' = BB.string7 "\\r"
    escape '\t' = BB.string7 "\\t"
    escape c
      | c >= ' ' && c < '\DEL' = BB.char7 c
      | n > 0xFFFF =
          unit (0xD800 + m `div` 0x400) <> unit (0xDC00 + m `mod` 0x400)
      | otherwise = unit n
      where n = ord c
            m = n - 0x10000
    unit u = BB.string7 "\\u" <> BB.word16HexFixed (fromIntegral u)
