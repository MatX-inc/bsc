{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns #-}
module SpeedyString(SString, toString, fromString, fromEnumName,
                    (++), concat, filter,
                    InternSnapshot(..), snapshotInternState, validSnapshot,
                    renderInternTable, internCount, internedSince,
                    isInterned) where

import Prelude hiding((++), concat, filter)
import qualified Prelude((++), filter)
import System.IO.Unsafe(unsafePerformIO)
import Control.Exception(evaluate)
import Control.Monad(forM_)
import Data.Array.Base(unsafeRead, unsafeWrite)
import Data.Array.IO(IOArray, IOUArray, newArray)
import Data.Bits((.&.), shiftL, shiftR)
import Data.IORef(IORef, newIORef, readIORef, writeIORef)
import qualified Data.Set as Set
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import Data.Char(chr, ord, toUpper, isDigit)
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
toString (SString i) = unsafePerformIO $
                       do t <- readIORef table
                          if i < 0 || i >= tNext t then err else spelling t i

-- s is hashed (forcing all of it) before the table is read, because
-- forcing s can intern other strings, even one equal to s
fromString :: String -> SString
fromString s = unsafePerformIO $
               do h <- evaluate (hashStr s)
                  case genKey s of
                    Just k  -> internGen k
                    Nothing -> internPlain h s

-- The spelling "_" ++ str ++ show n, as Id.enumId makes it, without
-- building the String when it is a generated spelling.  n is forced
-- before the table is read, as fromString forces its argument.
fromEnumName :: String -> Int -> SString
fromEnumName str n =
    case enumTag str of
      Just tag | n >= 0 && n < genLimit ->
                   unsafePerformIO (internGen (packGen tag n))
      _ -> fromString ('_' : str Prelude.++ show n)

(++) :: SString -> SString -> SString
s ++ s' = fromString $ (toString s) Prelude.++ (toString s')

concat :: [SString] -> SString
concat = fromString . concatMap toString

filter :: (Char -> Bool) -> SString -> SString
filter pred s = fromString $ Prelude.filter pred (toString s)

-- private

err :: a
err = internalError "SpeedyString: inconsistent representation"

--toNotSoSpeedyString :: SString -> NotSoSpeedyString.SString
--toNotSoSpeedyString speedy = NotSoSpeedyString.fromString (toString speedy)

--fromNotSoSpeedyString :: NotSoSpeedyString.SString -> SString
--fromNotSoSpeedyString not_so_speedy =
--    fromString (NotSoSpeedyString.toString not_so_speedy)

-- Generated spellings: the typechecker's fresh names (_tctyvarN, _tcdictN,
-- _tctempN, N in canonical decimal) are kept as a key, tag + 4 * N, and
-- spelled out only when asked.  Every intern of such a spelling, from enumId
-- or from any String, goes through the key, so ids are exactly those of
-- interning the String.

genLimit :: Int
genLimit = 10 ^ (15 :: Int)

packGen :: Int -> Int -> Int
packGen tag n = tag + n `shiftL` 2

enumTag :: String -> Maybe Int
enumTag "tctyvar" = Just 1
enumTag "tcdict"  = Just 2
enumTag "tctemp"  = Just 3
enumTag _         = Nothing

genPrefix :: Int -> String
genPrefix 1 = "_tctyvar"
genPrefix 2 = "_tcdict"
genPrefix 3 = "_tctemp"
genPrefix _ = err

genSpelling :: Int -> String
genSpelling k = genPrefix (k .&. 3) Prelude.++ show (k `shiftR` 2)

genKey :: String -> Maybe Int
genKey ('_':'t':'c':r) =
    case r of
      't':'y':'v':'a':'r':ds -> num 1 ds
      'd':'i':'c':'t':ds     -> num 2 ds
      't':'e':'m':'p':ds     -> num 3 ds
      _                      -> Nothing
  where num tag "0" = Just (packGen tag 0)
        num tag ds@(d:_)
          | d /= '0' && length ds <= 15 && all isDigit ds =
              Just (packGen tag (foldl (\a c -> a * 10 + (ord c - ord '0')) 0 ds))
        num _ _ = Nothing
genKey _ = Nothing

-- internal representation
--
-- Ids are dense, 0 .. tNext - 1.  tKey holds 0 for a plain spelling (kept
-- in tStr) and the key of a generated one.  tSlots is an open-addressed
-- table of (hash or key, id + 1) pairs, 0 marking an empty slot.

data Table = Table
    { tNext  :: !Int
    , tCap   :: !Int
    , tStr   :: !(IOArray Int String)
    , tKey   :: !(IOUArray Int Int)
    , tSlots :: !(IOUArray Int Int)
    , tMask  :: !Int
    }

{-# NOINLINE table #-}
table :: IORef Table
table = unsafePerformIO $
        do let cap = 65536
           ss <- newArray (0, cap - 1) []
           ks <- newArray (0, cap - 1) 0
           sl <- newArray (0, 4 * cap - 1) 0
           newIORef (Table 0 cap ss ks sl (2 * cap - 1))

spelling :: Table -> Int -> IO String
spelling t i = do k <- unsafeRead (tKey t) i
                  if k == 0 then unsafeRead (tStr t) i
                            else return (genSpelling k)

slotOf :: Int -> Int -> Int
slotOf mask h =
    fromIntegral ((fromIntegral h * 0x9E3779B97F4A7C15 :: Word) `shiftR` 29)
      .&. mask

-- the id holding the spelling, or the empty slot where it would go
probe :: Table -> Int -> (Int -> IO Bool) -> IO (Either Int Int)
probe t h same = go (slotOf (tMask t) h)
  where go j = do idp <- unsafeRead (tSlots t) (2 * j + 1)
                  if idp == 0 then return (Left j) else do
                    hv <- unsafeRead (tSlots t) (2 * j)
                    hit <- if hv == h then same (idp - 1) else return False
                    if hit then return (Right (idp - 1))
                           else go ((j + 1) .&. tMask t)

findPlain :: Table -> Int -> String -> IO (Either Int Int)
findPlain t h s = probe t h (\i -> do k <- unsafeRead (tKey t) i
                                      if k /= 0 then return False else do
                                        s' <- unsafeRead (tStr t) i
                                        return (s' == s))

findGen :: Table -> Int -> IO (Either Int Int)
findGen t k = probe t k (\i -> do k' <- unsafeRead (tKey t) i
                                  return (k' == k))

internPlain :: Int -> String -> IO SString
internPlain h s =
    do t <- readIORef table
       r <- findPlain t h s
       case r of
         Right i -> return (SString i)
         Left j -> insert t j h 0 s

internGen :: Int -> IO SString
internGen !k =
    do t <- readIORef table
       r <- findGen t k
       case r of
         Right i -> return (SString i)
         Left j -> insert t j k k []

insert :: Table -> Int -> Int -> Int -> String -> IO SString
insert t0 j h k s =
    do let i = tNext t0
       unsafeWrite (tSlots t0) (2 * j) h
       unsafeWrite (tSlots t0) (2 * j + 1) (i + 1)
       t1 <- if i < tCap t0 then return t0 else growIds t0
       unsafeWrite (tStr t1) i s
       unsafeWrite (tKey t1) i k
       let t2 = t1 { tNext = i + 1 }
       t3 <- if 2 * (i + 1) > tMask t2 then growSlots t2 else return t2
       writeIORef table t3
       return (SString i)

growIds :: Table -> IO Table
growIds t =
    do let cap = tCap t
           cap' = 2 * cap
       ss <- newArray (0, cap' - 1) []
       ks <- newArray (0, cap' - 1) 0
       forM_ [0 .. cap - 1] $ \i ->
           do unsafeRead (tStr t) i >>= unsafeWrite ss i
              unsafeRead (tKey t) i >>= unsafeWrite ks i
       return t { tCap = cap', tStr = ss, tKey = ks }

growSlots :: Table -> IO Table
growSlots t =
    do let n = tMask t + 1
           mask' = 2 * n - 1
       sl <- newArray (0, 2 * (mask' + 1) - 1) 0
       let t' = t { tSlots = sl, tMask = mask' }
       forM_ [0 .. n - 1] $ \j ->
           do idp <- unsafeRead (tSlots t) (2 * j + 1)
              if idp == 0 then return () else do
                hv <- unsafeRead (tSlots t) (2 * j)
                r <- probe t' hv (\_ -> return False)
                case r of
                  Left j' -> do unsafeWrite sl (2 * j') hv
                                unsafeWrite sl (2 * j' + 1) idp
                  Right _ -> err
       return t'


-- string hash function, stolen from FString

hashStr :: String -> Int
hashStr s = f s 0
        where f "" r = r
              f (c:cs) r = f cs (r*16+r+fromEnum c)


-- Whether s has been interned so far (interning nothing).
isInterned :: String -> IO Bool
isInterned s =
    do h <- evaluate (hashStr s)
       t <- readIORef table
       r <- case genKey s of
              Just k  -> findGen t k
              Nothing -> findPlain t h s
       return (either (const False) (const True) r)

-- The number of strings interned so far.
internCount :: IO Int
internCount = fmap tNext (readIORef table)

-- The strings interned with ids >= n, in id order (for the same
-- witness: naming what an operation interned).
internedSince :: Int -> IO [String]
internedSince n = do t <- readIORef table
                     spellingsFrom t (max 0 n)

-- Spellings of ids lo .. tNext - 1, in id order.  Built back to front in
-- constant stack: the dump runs in an exit handler under the default -K10m,
-- where a stack overflow cannot be delivered.
spellingsFrom :: Table -> Int -> IO [String]
spellingsFrom t lo = go (tNext t - 1) []
  where go i acc | i < lo = return acc
                 | otherwise = do s <- spelling t i
                                  go (i - 1) (s : acc)


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
    t <- readIORef table
    let n = tNext t
    ss <- spellingsFrom t 0
    return (InternSnapshot n (zip [0 ..] ss))

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
