{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns #-}
module SpeedyString(SString, toString, fromString, fromEnumName,
                    noteGenName,
                    (++), concat, filter,
                    InternSnapshot(..), snapshotInternState, validSnapshot,
                    snapshotProblem, restoreInternState,
                    renderInternTable, internCount, internedSince,
                    isInterned, sstringId) where

import Prelude hiding((++), concat, filter)
import qualified Prelude((++), filter)
import System.IO.Unsafe(unsafePerformIO)
import Control.DeepSeq(rnf)
import Control.Exception(evaluate)
import Control.Monad(forM_, when)
import Data.Array(Array, listArray)
import Data.Array.Base(unsafeAt, unsafeRead, unsafeWrite)
import Data.Array.IO(IOArray, IOUArray, newArray)
import Data.Bits((.&.), (.|.), shiftL, shiftR)
import Data.Int(Int32)
import Data.IORef(IORef, newIORef, readIORef, writeIORef)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Set as Set
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Short.Internal as SBS(unsafeIndex)
import Data.Char(chr, ord, toUpper, isDigit)
import Data.Word(Word8)
import Data.List(foldl', intersperse)
import Data.Maybe(isNothing)
import Numeric(showHex)
-- import qualified NotSoSpeedyString
import ErrorUtil (internalError)
import qualified Data.Generics as Generic


data SString = SString !Int -- unique id
   deriving (Generic.Data, Generic.Typeable)

-- The string's order key (see Reserved ids): equal ids are equal strings.
sstringId :: SString -> Int
sstringId (SString i) = i

instance Eq SString where
    (SString i) == (SString i') = i == i'

-- note that Ord is not the usual string ordering
instance Ord SString where
    compare (SString i) (SString i') = compare i i'

instance Show SString where
    show = show . toString

-- public

toString :: SString -> String
toString (SString d) = unsafePerformIO $
                       do t <- readIORef table
                          i <- indexOf d
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
s ++ s' = concat [s, s']

-- fromString . concatMap toString, on the packed spellings (see Byte
-- operations)
concat :: [SString] -> SString
concat ss = unsafePerformIO $
            do ps <- partBytes ss
               let !p = SBS.concat ps
               case genKeyBytes p of
                 Just k  -> internGen k
                 Nothing -> internBytes (hashBytes p) p

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
-- Entries are numbered 0 .. tNext - 1 in arrival order; an entry's id is
-- not its number (see Reserved ids).  tKey holds 0 for a plain spelling
-- (kept packed in tStr, see Packed spellings) and the key of a generated
-- one.  tSlots is an open-addressed table of (hash or key, entry + 1)
-- pairs, 0 marking an empty slot.

data Table = Table
    { tNext  :: !Int
    , tCap   :: !Int
    , tStr   :: !(IOArray Int SBS.ShortByteString)
    , tKey   :: !(IOUArray Int Int)
    , tSlots :: !(IOUArray Int Int)
    , tMask  :: !Int
    }

{-# NOINLINE table #-}
table :: IORef Table
table = unsafePerformIO $
        do let cap = 65536
           ss <- newArray (0, cap - 1) SBS.empty
           ks <- newArray (0, cap - 1) 0
           sl <- newArray (0, 4 * cap - 1) 0
           newIORef (Table 0 cap ss ks sl (2 * cap - 1))

spelling :: Table -> Int -> IO String
spelling t i = do k <- unsafeRead (tKey t) i
                  if k == 0 then fmap unpackSpelling (unsafeRead (tStr t) i)
                            else return (genSpelling k)

slotOf :: Int -> Int -> Int
slotOf mask h =
    fromIntegral ((fromIntegral h * 0x9E3779B97F4A7C15 :: Word) `shiftR` 29)
      .&. mask

-- the entry holding the spelling, or the empty slot where it would go
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
                                        p <- unsafeRead (tStr t) i
                                        return (packs p s))

findGen :: Table -> Int -> IO (Either Int Int)
findGen t k = probe t k (\i -> do k' <- unsafeRead (tKey t) i
                                  return (k' == k))

internPlain :: Int -> String -> IO SString
internPlain h s =
    do t <- readIORef table
       r <- findPlain t h s
       case r of
         Right i -> return (SString (idOf i))
         Left j -> fmap (SString . idOf) (insert t j h 0 (packSpelling s))

internGen :: Int -> IO SString
internGen !k =
    do t <- readIORef table
       r <- findGen t k
       v <- if k .&. 3 == 1 then reservedId (k `shiftR` 2) else return 0
       case r of
         Right i -> return (SString (if v /= 0 then v else idOf i))
         Left j -> do i <- insert t j k k SBS.empty
                      if v /= 0 then do materialize v i
                                        return (SString v)
                                else return (SString (idOf i))

insert :: Table -> Int -> Int -> Int -> SBS.ShortByteString -> IO Int
insert t0 j h k !s =
    do let i = tNext t0
       unsafeWrite (tSlots t0) (2 * j) h
       unsafeWrite (tSlots t0) (2 * j + 1) (i + 1)
       t1 <- if i < tCap t0 then return t0 else growIds t0
       unsafeWrite (tStr t1) i s
       unsafeWrite (tKey t1) i k
       let t2 = t1 { tNext = i + 1 }
       t3 <- if 2 * (i + 1) > tMask t2 then growSlots t2 else return t2
       writeIORef table t3
       return i

growIds :: Table -> IO Table
growIds t =
    do let cap = tCap t
           cap' = 2 * cap
       ss <- newArray (0, cap' - 1) SBS.empty
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


-- Packed spellings
--
-- A plain spelling is kept as its Chars' code points in UTF-8's bit
-- layout, surrogates included (three bytes, like any code point below
-- 0x10000) and NUL as a zero byte.  Every String round-trips, and equal
-- bytes are equal Strings.  The GC copies the bytes, not a list; toString
-- decodes a new String each time.

-- Written in one pass of the known length.  The state is the Chars left
-- and how many continuation bytes of the first are still to come.
packSpelling :: String -> SBS.ShortByteString
packSpelling s = fst (SBS.unfoldrN (foldl' (\a c -> a + width (ord c)) 0 s) step (s, 0))
  where
    step :: (String, Int) -> Maybe (Word8, (String, Int))
    step ([], _) = Nothing
    step (cs@(c : rest), k)
      | k > 0 = Just (byte (0x80 .|. (shiftR n (6 * (k - 1)) .&. 0x3F)),
                      (if k == 1 then rest else cs, k - 1))
      | n < 0x80 = Just (byte n, (rest, 0))
      | n < 0x800 = Just (byte (0xC0 .|. shiftR n 6), (cs, 1))
      | n < 0x10000 = Just (byte (0xE0 .|. shiftR n 12), (cs, 2))
      | otherwise = Just (byte (0xF0 .|. shiftR n 18), (cs, 3))
      where n = ord c
    width n | n < 0x80 = 1
            | n < 0x800 = 2
            | n < 0x10000 = 3
            | otherwise = 4 :: Int
    byte :: Int -> Word8
    byte = fromIntegral

-- Built back to front: the whole String, in constant stack.
unpackSpelling :: SBS.ShortByteString -> String
unpackSpelling p = go (SBS.length p) []
  where
    go 0 acc = acc
    go e acc = let s = start (e - 1)
                   !c = toChar (codeAt p s)
               in  go s (c : acc)
    start i = if byteAt p i .&. 0xC0 == 0x80 then start (i - 1) else i
    toChar n = if n < 0x80 then unsafeAt asciiChars n else chr n

-- Whether p packs s, decoding as it goes.
packs :: SBS.ShortByteString -> String -> Bool
packs p = go 0
  where
    n = SBS.length p
    go i [] = i == n
    go i (c : cs) = i < n && codeAt p i == ord c &&
                    go (i + leadWidth (byteAt p i)) cs

-- the length of the code point whose first byte is b
leadWidth :: Int -> Int
leadWidth b | b < 0x80 = 1
            | b < 0xE0 = 2
            | b < 0xF0 = 3
            | otherwise = 4

-- the code point whose bytes start at i
codeAt :: SBS.ShortByteString -> Int -> Int
codeAt p i
    | b < 0x80 = b
    | b < 0xE0 = shiftL (b .&. 0x1F) 6 .|. cont 1
    | b < 0xF0 = shiftL (b .&. 0x0F) 12 .|. shiftL (cont 1) 6 .|. cont 2
    | otherwise = shiftL (b .&. 0x07) 18 .|. shiftL (cont 1) 12 .|.
                  shiftL (cont 2) 6 .|. cont 3
  where b = byteAt p i
        cont k = byteAt p (i + k) .&. 0x3F

-- unchecked: every index read here is inside p, by the encoding
byteAt :: SBS.ShortByteString -> Int -> Int
byteAt p i = fromIntegral (SBS.unsafeIndex p i)

-- shared, so that decoding an ASCII Char allocates no box
asciiChars :: Array Int Char
asciiChars = listArray (0, 0x7F) ['\0' .. '\x7F']


-- Byte operations
--
-- concat (and so ++) joins the parts' packed spellings and interns the
-- result from its bytes, building no String.  Packing is per code point,
-- so the joined bytes pack the joined String, hashBytes gives its hashStr
-- and genKeyBytes its genKey: the result gets the id, slot and stored
-- bytes that fromString of the joined String gives it.
--
-- Order: fromString (concatMap toString ss) forces, inside its hash, each
-- spine cell of ss and then that element (either may intern), and reads
-- the element's spelling before the next cell; only then does it read the
-- table and intern the result.  partBytes forces the cells and elements in
-- that order and reads each spelling right after its element, before the
-- result's lookup.  Reading a spelling interns nothing: toString and
-- partBytes both find the entry with indexOf, and neither materializes a
-- reserved id (an SString holds one only after internGen materialized it).

-- Each part's bytes, in order.  Constant stack, as hashStr forced them.
partBytes :: [SString] -> IO [SBS.ShortByteString]
partBytes = go []
  where go acc l0 =
          do l <- evaluate l0
             case l of
               [] -> return (reverse acc)
               s : ss -> do SString d <- evaluate s
                            t <- readIORef table
                            i <- indexOf d
                            p <- if i < 0 || i >= tNext t then err
                                                           else bytesOf t i
                            go (p : acc) ss

bytesOf :: Table -> Int -> IO SBS.ShortByteString
bytesOf t i = do k <- unsafeRead (tKey t) i
                 if k == 0 then unsafeRead (tStr t) i
                           else return (packSpelling (genSpelling k))

-- hashStr of the String p packs
hashBytes :: SBS.ShortByteString -> Int
hashBytes p = go 0 0
  where n = SBS.length p
        go i !r | i >= n = r
                | otherwise = go (i + leadWidth (byteAt p i))
                                 (r*16+r+codeAt p i)

-- genKey of the String p packs.  A generated spelling is ASCII, and no
-- other code point packs to an ASCII byte.
genKeyBytes :: SBS.ShortByteString -> Maybe Int
genKeyBytes p
    | n < 8 || byteAt p 0 /= ord '_' || byteAt p 1 /= ord 't' ||
      byteAt p 2 /= ord 'c' = Nothing
    | prefix "tyvar" = num 1 8
    | prefix "dict"  = num 2 7
    | prefix "temp"  = num 3 7
    | otherwise      = Nothing
  where n = SBS.length p
        prefix w = and (zipWith (\j c -> byteAt p j == ord c) [3 ..] w)
        num tag o
          | n - o == 1 && byteAt p o == ord '0' = Just (packGen tag 0)
          | n > o && n - o <= 15 && byteAt p o /= ord '0' && all isD ds =
              Just (packGen tag (foldl' (\a j -> a * 10 + digit j) 0 ds))
          | otherwise = Nothing
          where ds = [o .. n - 1]
        digit j = byteAt p j - ord '0'
        isD j = digit j >= 0 && digit j <= 9

internBytes :: Int -> SBS.ShortByteString -> IO SString
internBytes h p =
    do t <- readIORef table
       r <- probe t h (\i -> do k <- unsafeRead (tKey t) i
                                if k /= 0 then return False else
                                  fmap (== p) (unsafeRead (tStr t) i))
       case r of
         Right i -> return (SString (idOf i))
         Left j -> fmap (SString . idOf) (insert t j h 0 p)


-- Reserved ids
--
-- The typechecker builds the name _tctyvarN of a variable it draws only
-- where the name is used (CType.TyVarG), and calls noteGenName N wherever
-- forcing the name used to intern it.  The first note of a spelling not
-- yet interned reserves the place that interning it there would have
-- given it: after every string interned and every place reserved so far,
-- before everything interned or reserved later.  Interning the spelling
-- later gives it the reserved id, so Ord SString answers as if every
-- noted name had been interned at its note.
--
-- So an id is an order key: entry i has id i * 2^32, and the q-th place,
-- reserved while the table held k entries, has id (k - 1) * 2^32 + 1 + q.
-- rMat maps a reserved id whose spelling is interned to its entry.

data Reserves = Reserves
    { rNum   :: !(IOUArray Int Int32)  -- N -> q + 1, inTable or 0
    , rCap   :: !Int
    , rSpill :: !(IM.IntMap Int)       -- N -> q + 1, for N >= rCap
    , rPlace :: !(IOUArray Int Int32)  -- q -> k
    , rPCap  :: !Int
    , rSeq   :: !Int                   -- places reserved so far
    , rMat   :: !(IM.IntMap Int)
    }

{-# NOINLINE reserves #-}
reserves :: IORef Reserves
reserves = unsafePerformIO $
           do a <- newArray (0, -1) 0
              p <- newArray (0, -1) 0
              newIORef (Reserves a 0 IM.empty p 0 0 IM.empty)

idOf :: Int -> Int
idOf i = i `shiftL` 32

-- the entry with id d, or -1
indexOf :: Int -> IO Int
indexOf d
    | d .&. 0xFFFFFFFF == 0 = return (d `shiftR` 32)
    | otherwise = do r <- readIORef reserves
                     return (IM.findWithDefault (-1) d (rMat r))

-- noted while _tctyvarN was already interned, so later notes need no probe
inTable :: Int
inTable = -1

-- q and k must fit rNum and rPlace
maxReserve :: Int
maxReserve = 0x7FFFFFFE

lookupRes :: Reserves -> Int -> IO Int
lookupRes r n
    | n < rCap r = fmap fromIntegral (unsafeRead (rNum r) n)
    | otherwise  = return (IM.findWithDefault 0 n (rSpill r))

-- the id reserved for _tctyvarN, or 0
reservedId :: Int -> IO Int
reservedId n =
    do r <- readIORef reserves
       v <- lookupRes r n
       if v <= 0 then return 0 else do
         let q = v - 1
         k <- unsafeRead (rPlace r) q
         return (idOf (fromIntegral k - 1) + 1 + q)

-- | Where forcing enumId "tctyvar" _ n used to intern its spelling: mark
-- that point without interning it (see Reserved ids).
noteGenName :: Int -> IO ()
noteGenName n
    | n < 0 || n >= genLimit = evaluate (fromEnumName "tctyvar" n) >> return ()
    | otherwise =
        do r <- readIORef reserves
           v <- lookupRes r n
           when (v == 0) $ do
             t <- readIORef table
             f <- findGen t (packGen 1 n)
             case f of
               Right _ -> when (n < rCap r) $
                            unsafeWrite (rNum r) n (fromIntegral inTable)
               Left _
                 | rSeq r >= maxReserve || tNext t >= maxReserve ->
                     internGen (packGen 1 n) >> return ()
                 | otherwise -> reserve r n (tNext t) >>= writeIORef reserves

reserve :: Reserves -> Int -> Int -> IO Reserves
reserve r0 n k =
    do let q = rSeq r0
       r1 <- if q < rPCap r0 then return r0 else
               do let c = max 1024 (2 * rPCap r0)
                  a <- newArray (0, c - 1) 0
                  forM_ [0 .. rPCap r0 - 1] $ \i ->
                      unsafeRead (rPlace r0) i >>= unsafeWrite a i
                  return r0 { rPlace = a, rPCap = c }
       unsafeWrite (rPlace r1) q (fromIntegral k)
       setNum r1 { rSeq = q + 1 } n (q + 1)

-- rNum covers 0 .. rCap - 1.  It grows by half, to cover N, when it would
-- then hold a reservation in every 16 entries: its 4-byte entries, which
-- the GC does not scan, then cost at most 64 bytes a reservation, about an
-- IntMap entry.  Other reservations go to rSpill.
setNum :: Reserves -> Int -> Int -> IO Reserves
setNum r n v
    | n < rCap r =
        do unsafeWrite (rNum r) n (fromIntegral v)
           return r
    | 16 * rSeq r >= cap' =
        do a <- newArray (0, cap' - 1) 0
           forM_ [0 .. rCap r - 1] $ \i -> unsafeRead (rNum r) i >>= unsafeWrite a i
           let (lo, hi) = IM.partitionWithKey (\m _ -> m < cap') (rSpill r)
           forM_ (IM.toList lo) $ \(m, w) -> unsafeWrite a m (fromIntegral w)
           unsafeWrite a n (fromIntegral v)
           return r { rNum = a, rCap = cap', rSpill = hi }
    | otherwise = return r { rSpill = IM.insert n v (rSpill r) }
  where cap' = 4096 * ((max (n + 1) (rCap r + rCap r `div` 2) + 4095) `div` 4096)

materialize :: Int -> Int -> IO ()
materialize v i = do r <- readIORef reserves
                     writeIORef reserves r { rMat = IM.insert v i (rMat r) }


-- string hash function, stolen from FString

hashStr :: String -> Int
hashStr s = f s 0
        where f "" r = r
              f (c:cs) r = f cs (r*16+r+fromEnum c)


-- Whether s has been interned so far (interning nothing); a noted
-- _tctyvarN counts as interned.
isInterned :: String -> IO Bool
isInterned s =
    do h <- evaluate (hashStr s)
       t <- readIORef table
       case genKey s of
         Just k -> do v <- if k .&. 3 == 1 then reservedId (k `shiftR` 2)
                                           else return 0
                      if v /= 0 then return True else found (findGen t k)
         Nothing -> found (findPlain t h s)
  where found = fmap (either (const False) (const True))

-- The number of strings interned so far.
internCount :: IO Int
internCount = fmap tNext (readIORef table)

-- The strings interned since internCount returned n, in arrival order
-- (for the same witness: naming what an operation interned).
internedSince :: Int -> IO [String]
internedSince n = do t <- readIORef table
                     spellingsFrom t (max 0 n)

-- Spellings of entries lo .. tNext - 1.  Built back to front in constant
-- stack: the dump runs in an exit handler under the default -K10m, where a
-- stack overflow cannot be delivered.
spellingsFrom :: Table -> Int -> IO [String]
spellingsFrom t lo = go (tNext t - 1) []
  where go i acc | i < lo = return acc
                 | otherwise = do s <- spelling t i
                                  go (i - 1) (s : acc)

-- Every spelling in id order: the entries in arrival order, each interned
-- reserved id moved to its place.  Built back to front in constant stack.
spellingsById :: Table -> Reserves -> IO [String]
spellingsById t r = go (tNext t - 1) (IM.toDescList (rMat r)) []
  where moved = IS.fromList (IM.elems (rMat r))
        go i ms acc
          | i >= 0 && IS.member i moved = go (i - 1) ms acc
          | otherwise =
              case ms of
                (d, m) : ms' | i < 0 || d > idOf i ->
                  do s <- spelling t m
                     go i ms' (s : acc)
                _ | i < 0 -> return acc
                  | otherwise -> do s <- spelling t i
                                    go (i - 1) ms (s : acc)


-- A capture contains every spelling, including entries no longer reachable
-- from the scheduled package, numbered in id order: SString's Ord instance
-- compares interned IDs, not string contents.
data InternSnapshot = InternSnapshot
    { internNextId :: Int
    , internEntries :: [(Int, String)]
    } deriving (Eq, Read, Show)

-- Call only once ordinary output has been fully written.  Like the existing
-- interner, this requires a single compiler thread.
snapshotInternState :: IO InternSnapshot
snapshotInternState = do
    t <- readIORef table
    r <- readIORef reserves
    ss <- spellingsById t r
    return (InternSnapshot (tNext t) (zip [0 ..] ss))

validSnapshot :: InternSnapshot -> Bool
validSnapshot = isNothing . snapshotProblem

-- Why a snapshot is not valid, if it is not: internNextId must be some
-- n >= 0, the ids 0 .. n - 1 in order, and the spellings distinct.
snapshotProblem :: InternSnapshot -> Maybe String
snapshotProblem (InternSnapshot n entries)
    | n < 0 = Just ("internNextId is " <> show n)
    | otherwise = go 0 Set.empty entries
  where
    go i _ [] | i == n = Nothing
              | otherwise = Just (show i <> " entries, but internNextId is " <>
                                  show n)
    go i seen ((j, s) : es)
        | i >= n = Just ("more than internNextId (" <> show n <> ") entries")
        | j /= i = Just ("entry " <> show i <> " has id " <> show j)
        | Set.member s seen = Just ("entry " <> show i <> " repeats " <> show s)
        | otherwise = go (i + 1) (Set.insert s seen) es

-- | Rebuild the table from a snapshot (snapshotInternState) taken in another
-- process, so that SStrings made here compare as they did there.  Snapshot
-- entry i becomes entry i, with id i * 2^32; interning its spelling again
-- returns it.  The snapshot holds no place reserved for a name that was
-- never spelled (see Reserved ids), and no reserve is restored: no SString
-- held one, and noting or interning such a name here puts it after every
-- restored entry.
--
-- Only into an untouched table: before flags are parsed, a .bo or .ba is
-- decoded, or any Id or FString (a CAF included) is forced.  Otherwise, or
-- if the snapshot is not valid, the result is Left and the table is
-- unchanged; so a second restore is refused unless the first was empty,
-- which changed nothing.  Single compiler thread, as for the interner.
restoreInternState :: InternSnapshot -> IO (Either String ())
restoreInternState snap@(InternSnapshot n entries) = do
    -- an exception in the snapshot's own values, or a string their
    -- evaluation interns, comes before the table is checked or changed
    evaluate (rnf (n, entries))
    t <- readIORef table
    r <- readIORef reserves
    case () of
      _ | tNext t /= 0 || rSeq r /= 0 ->
            return (Left ("restore must come before anything is interned, " <>
                          "but " <> show (tNext t) <> " strings are interned " <>
                          "and " <> show (rSeq r) <> " names noted"))
        | Just problem <- snapshotProblem snap -> return (Left problem)
        | otherwise -> do
            forM_ entries $ \(i, s) -> do
              h <- evaluate (hashStr s)
              SString d <- case genKey s of
                             Just k  -> internGen k
                             Nothing -> internPlain h s
              when (d /= idOf i) err
            return (Right ())

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
