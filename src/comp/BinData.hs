{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
{-# LANGUAGE PatternSynonyms, OverloadedLists, TypeFamilies #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# OPTIONS_GHC -Werror -fwarn-incomplete-patterns #-}
module BinData ( Byte
               , putBs, putB, putI
               , getN, getB, getI
               , Bin(..)
               , section
               , Lazy(..), toBinLazy, fromBinLazy
               , encode, encodeWith, decode, decodeStrict, decodeWithHash
               ) where

{- Routines for converting structures to/from byte strings.

   These routines are carefully crafted to avoid three problem scenarios:
   stack overflows due to thunks within a monad, requiring the
   entire binary stream to be built in memory before it can begin to be
   consumed, and losing structure sharing in the generated binary stream.

   Because of these requirements, this module is designed for
   full laziness and includes a facility for tracking shared structures.
   This allows the conversion to run in bounded space because the byte
   stream is generated lazily and the portion of it
   which has been consumed can be reclaimed by the garbage collector
   before the remainder of the stream has been generated.

   CONTAINER LAYOUT

   An encoded value is a region container.  All offsets are relative to
   the start of the encoding (an artifact's format tag precedes it and
   is not part of the container):

     [bodies]        the root value, written sequentially.  A shared
                     value (FString, Id, Position, Type, IType) occurs
                     in the stream as a plain index into its table; a
                     lazy sub-value (see Lazy / toBinLazy) occurs as a
                     chunk number followed inline by the chunk's bytes.
     [tables]        for each of Id, Position, Type and IType: the
                     entries (each entry is the value's writeBytes
                     payload, with nested shared occurrences as indices)
                     concatenated in index order, then an index of
                     count+1 u32 offsets (entry i spans [off_i, off_i+1)).
     [strtab]        u32 count, then every distinct FString in
                     first-occurrence order, in the Bin String encoding.
     [chunk index]   u32 count, then (u32 offset, u32 length) per chunk,
                     in chunk-number order; offsets are into [bodies].
     [footer]        fixed width (see footerSize): u64 offsets and counts
                     locating every region above, then an 8-byte magic.

   Indices are assigned per kind in first-occurrence order of a single
   left-to-right walk of the element stream (a parent's index precedes
   its children's), exactly as the previous in-stream sharing scheme
   assigned them, so every table and the string table are a pure
   function of the content (after the -remap-path-prefix remap applied
   in that walk).  The string table is the walk-ordered
   projection of the intern table: the reader interns it eagerly in
   table order, so FString ids come out exactly as a sequential decode
   would have produced them.  The other tables are decoded on demand,
   one memoized thunk per entry (forward references are allowed), and a
   lazy chunk is decoded on first force of the value that stands for it.

   The bodies region streams out as it is produced; only the tables and
   the two indices are buffered until the end.

   When reconstructing a data structure from its byte stream, we do
   not yet meet the goal of bounded space.  Care must be taken to make
   all of the reader routines sufficiently strict to avoid building
   large trees of thunks that are never evaluated.
-}

import ErrorUtil(internalError)
import System.IO.Unsafe(unsafePerformIO, unsafeInterleaveIO)

import FStringCompat
import PreIds(idDefaultClock)
import Id
import Position
import VModInfo
import SchedInfo(SchedInfo(..), MethodConflictInfo(..),
                 extractFromMethodConflictInfo)
import Pragma
import DefProp
import ASyntax
import ISyntax
import IType(iTypeNodeId)
import Wires
import CType
import IntLit
import Undefined
import Prim hiding(PrimArg(..))

import Util(hashInit, nextHashByte, showHash)

import Data.List(sort, intercalate, mapAccumL)
import Control.Monad(liftM, ap, when)
import Control.Exception(evaluate)
import Foreign.ForeignPtr(mallocForeignPtrBytes, withForeignPtr)
import Foreign.Ptr(Ptr)
import Foreign.Storable(peekByteOff, pokeByteOff)
import Data.Array.IArray()
import Data.Array.Unboxed
import GHC.Arr(Array(..))
import GHC.Exts(IsList(..), Int(I#), indexArray#)
import Data.Bits
import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as TE
import qualified Data.Text as TS
import qualified Data.Text.Encoding as TSE
import qualified Data.Map as M
import qualified Data.IntMap.Strict as IM
import qualified Data.Set as S
import Numeric(showHex)

import Debug.Trace

type Byte = Word8

{- To reduce the size of the binary stream and preserve shared
   structures, we want to avoid repeatedly emitting identical
   structures into the binary stream.  But we need to do it in
   a way that still allows full laziness.  So examining the entire
   structure to identify all shared structures before writing out
   the data is not a viable option.  Instead, we take the approach
   of converting (lazily) into this intermediate structure which
   distinguishes shared values from the other converted data.  This
   structure has the advantage of being compositional and easy to
   generate in a fully lazy monad with constant stack usage.

   When the final result is extracted from the monad, each shared
   element is replaced by its index; the first time a value is
   encountered its payload is routed to the table for its kind
   (strings to the string table).  Lazy sub-values are delimited so
   that their bytes can be located later without decoding them.

   This process can be done piecewise and lazily, which is what we
   want.
 -}

-- -------------------------------------------------------------
-- Intermediate structure with distinguished shared structures

{- HOW TO ADD A NEW TYPE TO THE SET OF STRUCTURES THAT GET SHARED

   1. Add a constructor for it to the BinElem definition
      (Don't forget to add it to the Show instance)
   2. Add a cache for it to the BinCache record along with
      a set function to update the field of the record
      (Don't forget to update unknownCache)
   3. Add a TableKind for it, a table for it to the Tables record, and
      a case to every per-kind function (tableFooter, finish, mkTables)
   4. Add a Shared instance to provide access to the cache and table
      added in steps 2 & 3
   5. In the Bin instance, define toBin and fromBin functions
      These will typically be simply:
        toBin a = Out [Constructor a] ()
        fromBin = readShared
   6. Add a case to the assemble function to handle the constructor
      added in Step 1
-}

data BinElem = B [Byte]
             -- shared types
             | S FString
             | I Id
             | P Position
             | T Type
             -- | ASL AStateLocPathComponent
             | IT IType
             -- | AExp AExpr
             -- accounting markers
             | Start String
             | End
             -- a lazily decoded sub-value (toBinLazy)
             | LazyStart
             | LazyEnd
             -- internal to assemble: the payload of a first occurrence
             | EntryStart TableKind Int
             | EntryEnd

instance Show BinElem where
  show (B bs)    = "[" ++
                   (intercalate "," [showHex b "" | b <- bs]) ++
                   "]"
  show (S s)     = getFString s
  show (I i)     = show i
  show (P p)     = show p
  show (T t)     = show t
  show (IT t)    = show t
  -- show (ASL l)   = show l
  -- show (AExp e)  = show e
  show (Start s) = "<BEGIN " ++ s ++ ">"
  show End       = "<END>"
  show LazyStart = "<LAZY"
  show LazyEnd   = "LAZY>"
  show (EntryStart k n) = "<ENTRY " ++ show k ++ " " ++ show n
  show EntryEnd  = "ENTRY>"

-- The kinds of shared value that live in offset-indexed tables
-- (strings have the eager string table instead)
data TableKind = KId | KPosition | KType | KIType
  deriving (Eq, Ord, Show, Enum, Bounded)

allKinds :: [TableKind]
allKinds = [minBound .. maxBound]

-- Simple difference list implementation for O(1) append.
newtype DList a = DL ([a] -> [a])

instance IsList (DList a) where
  type Item (DList a) = a

  {-# INLINE toList #-}
  toList (DL f) = f []

  {-# INLINE fromList #-}
  fromList = DL . (++)

instance Semigroup (DList a) where
  {-# INLINE (<>) #-}
  (DL a) <> (DL b) = DL (a . b)

type BinData = DList BinElem

-- -------------------------------------------------------------
-- Structures used to track shared values during writing

data Known k v = Known Int (M.Map k v)
  deriving Show

unknown :: Known k v
unknown = Known 0 M.empty

-- Cache of known keys and the index assigned to them
type Cache k = Known k Int

data BinCache = BinCache { string_cache   :: Cache FString
                         , id_cache       :: Cache IdKey
                         , position_cache :: Cache Position
                         , type_cache     :: Cache TypeKey
                         , itype_cache    :: Cache ITypeKey
                         }

-- Set functions
set_string_cache :: BinCache -> Cache FString -> BinCache
set_string_cache   bc c = bc { string_cache = c }

set_id_cache :: BinCache -> Cache IdKey -> BinCache
set_id_cache       bc c = bc { id_cache = c }

set_position_cache :: BinCache -> Cache Position -> BinCache
set_position_cache bc c = bc { position_cache = c }

set_type_cache :: BinCache -> Cache TypeKey -> BinCache
set_type_cache     bc c = bc { type_cache = c }

set_itype_cache :: BinCache -> Cache ITypeKey -> BinCache
set_itype_cache    bc c = bc { itype_cache = c }

unknownCache :: BinCache
unknownCache = BinCache unknown unknown unknown unknown unknown

-- We don't use Id as a key since the Eq instance for Id only
-- compares the strings, but we want to distinguish Ids with
-- different properties, positions, etc.
data IdKey = IK FString FString Position [IdProp]
  deriving (Eq, Ord, Show)

id_key :: Id -> IdKey
id_key i = IK (getIdBase i) (getIdQual i) (getIdPosition i) (getIdProps i)

-- We don't use the Type as a key since the Eq instance for some
-- elements of Type (like TyVar) ignore Kinds in the equaliy test.
data TypeKey = TKV IdKey Int Kind
             | TKC IdKey (Maybe Kind) TISort
             | TKN Integer Position
             | TKS FString Position
             | TKA TypeKey TypeKey
             | TKG Position Int
  deriving (Eq, Ord, Show)

type_key :: Type -> TypeKey
type_key (TVar (TyVar i n k))  = TKV (id_key i) n k
type_key (TCon (TyCon i mk s)) = TKC (id_key i) mk s
type_key (TCon (TyNum n p))    = TKN n p
type_key (TCon (TyStr s p))    = TKS s p
type_key (TAp t1 t2)           = TKA (type_key t1) (type_key t2)
type_key (TGen p n)            = TKG p n
type_key (TDefMonad _) = internalError $ "BinData.type_key: TDefMonad"

-- We don't use the IType as a key since the Eq instance for some
-- variants (like ITForAll) ignore Kinds in the equality test.
--
-- Interior nodes (ITAp/ITForAll) are hash-consed (see IType): the
-- intern table guarantees one node per exact-serialization-granularity
-- key, so the intern unique IS the node's structural identity, and the
-- deep structural key that used to be built here (which re-walked
-- shared subtrees once per path, exponentially many times on
-- DAG-shaped types) is unnecessary.  Leaves are not interned and keep
-- their exact keys.  The unique never enters the byte stream: it only
-- keys the writer-local sharing map, and the emitted bytes remain
-- structure plus first-occurrence LOCAL indices, fully
-- content-determined.
data ITypeKey = ITKI {-# UNPACK #-} !Int
              | ITKV IdKey
              | ITKC IdKey IKind TISort
              | ITKN Integer
              | ITKS FString
  deriving (Eq, Ord, Show)

itype_key :: IType -> ITypeKey
itype_key t@(ITForAll _ _ _) = ITKI (iTypeNodeId t)
itype_key t@(ITAp _ _)       = ITKI (iTypeNodeId t)
itype_key (ITVar i)          = ITKV (id_key i)
itype_key (ITCon i k s)      = ITKC (id_key i) k s
itype_key (ITNum n)          = ITKN n
itype_key (ITStr s)          = ITKS s

-- -------------------------------------------------------------
-- The Out monad makes it easy to generate composite BinData

data Out a = Out BinData a

out_data :: Out a -> [BinElem]
out_data (Out bd _) = toList bd

instance Monad Out where
  return = pure
  (Out bs x) >>= f = let (Out bs' x') = f x in Out (bs <> bs') x'

instance Functor Out where
  fmap = liftM

instance Applicative Out where
  pure x = Out [] x
  (<*>) = ap

-- Insert some bytes into the byte stream
putBs :: [Byte] -> Out ()
putBs bs = Out [B bs] ()

-- Insert one byte into the byte stream
putB :: Byte -> Out ()
putB b = putBs [b]

-- Insert an Int value (between 0 and 255) into the byte stream
putI :: Int -> Out ()
putI n = if (n >= 0) && (n < 256)
         then putB $ toEnum n
         else internalError $ "BinData.putI value out of range: " ++ (show n)

-- Insert accounting marks into the byte stream

beginSection :: String -> Out ()
beginSection s = Out [Start s] ()

endSection :: Out ()
endSection = Out [End] ()

section :: String -> Out () -> Out ()
section s action = do { beginSection s; action; endSection }

-- Write a sub-value as a lazy chunk: the reader skips its bytes and
-- decodes them on first force of the returned value (see fromBinLazy).
-- A lazy chunk may not occur inside a shared value's payload.
toBinLazy :: (Bin a) => a -> Out ()
toBinLazy x = do { Out [LazyStart] (); toBin x; Out [LazyEnd] () }

-- A wrapper whose Bin instance is the lazy-chunk encoding of the
-- wrapped value
newtype Lazy a = Lazy { unLazy :: a }

instance (Bin a) => Bin (Lazy a) where
  writeBytes (Lazy x) = toBinLazy x
  readBytes = Lazy <$> fromBinLazy

-- -------------------------------------------------------------
-- The In monad makes it easy to parse a byte stream

-- The reader's tables for one container: the whole encoding, the
-- eagerly interned string table, one lazily decoded (memoized per
-- entry) table per other shared kind, and the chunk index.
data Tables = Tables { tBytes      :: !BS.ByteString
                     , tStrings    :: !(Array Int FString)
                     , tIds        :: Array Int Id
                     , tPositions  :: Array Int Position
                     , tTypes      :: Array Int Type
                     , tITypes     :: Array Int IType
                     , tChunks     :: !(UArray Int Int) -- offset, length pairs
                     , tChunkCount :: !Int
                     }

-- The In monad reads from a fixed environment: the byte string, a small
-- mutable block holding the read offset and its limit, and the tables.
-- Keeping the per-byte state mutable means reading a byte allocates
-- nothing.  Every decode (the root, a table entry, a lazy chunk) runs
-- with its own state block over the shared tables (see runAt).  In a
-- strict run (decodeStrict) a lazy chunk is decoded in place instead of
-- being deferred.

data Env = Env { envBytes  :: !BS.ByteString
               , envState  :: !(Ptr Word8) -- see stateOffset .. stateSize
               , envTables :: !Tables
               , envStrict :: !Bool
               }

-- Layout of the mutable state block: the read offset (Int), the offset
-- this run may not read past (Int), and the number of chunks decoded in
-- place (Int, strict runs only).
stateOffset, stateLimit, stateChunks, stateSize :: Int
stateOffset = 0
stateLimit  = 8
stateChunks = 16
stateSize   = 24

newtype In a = In (Env -> IO a)

instance Monad In where
  return = pure
  (In v) >>= f = In $ \env -> do x <- v env
                                 let (In fn) = f x
                                 fn env

instance Functor In where
  fmap = liftM

instance Applicative In where
  pure x = In $ \_ -> return x
  (<*>) = ap

runInEnv :: In a -> Env -> IO a
runInEnv (In f) env = f env

getN :: Int -> In [Byte]
getN n = readN n getB

-- Read n values in order. In runs in IO, where replicateM and a recursive
-- (x:) <$> ... hold one stack frame per element until the list ends; a long
-- list overflows the stack. Accumulating and reversing runs in constant stack.
{-# INLINE readN #-}
readN :: Int -> In a -> In [a]
readN n0 r = go n0 []
  where go n acc | n <= 0    = return (reverse acc)
                 | otherwise = do { x <- r; go (n - 1) (x:acc) }

-- Advance the read offset past n bytes and return where they start.
{-# INLINE advance #-}
advance :: String -> Env -> Int -> IO Int
advance who env n = do
  let st = envState env
  off <- peekByteOff st stateOffset :: IO Int
  lim <- peekByteOff st stateLimit :: IO Int
  if n < 0 || off + n > lim
    then internalError ("BinData." ++ who ++ ": unexpected end of byte stream")
    else do pokeByteOff st stateOffset (off + n)
            return off

{-# INLINE getB #-}
getB :: In Byte
getB = In $ \env -> do
  off <- advance "getB" env 1
  let !b = BSU.unsafeIndex (envBytes env) off
  return b

-- Read n bytes as a slice of the input, without building a list.
getBS :: Int -> In BS.ByteString
getBS n = In $ \env -> do
  off <- advance "getBS" env n
  let !s = BSU.unsafeTake n (BSU.unsafeDrop off (envBytes env))
  return s

-- get an Int value (between 0 and 255)
getI :: In Int
getI = do { b <- getB; return (fromEnum b) }

-- ------------------------------------------------------------------
-- The Shared typeclass simplifies the process of creating shared
-- structures in the output byte stream.

class Shared k v | v -> k where
  -- query if a key is already known (value is used only for its type)
  knownAs   :: k -> v -> BinCache -> Maybe Int
  -- add a (key,value) association, returning the index assigned to it
  addKey    :: k -> v -> BinCache -> (Int, BinCache)
  -- get the value associated with a given index
  lookupIdx :: Int -> In v

-- Templates for instances. These take get/set functions for the
-- particular cache fields in a BinCache and generate a function
-- appropriate for defining an instance of Shared for the correct key
-- and value type.

-- get the right cache, lookup the key and return the result
mkKnownAs :: (Ord k) =>
             (BinCache -> (Cache k)) ->
             (k -> v -> BinCache -> Maybe Int)
mkKnownAs get k _ bc = let (Known _ m) = get bc
                       in M.lookup k m

-- get the right cache, add an mapping from key to the next index, and
-- put back the updated cache
mkAddKey :: (Ord k) =>
            (BinCache -> (Cache k)) ->
            (BinCache -> (Cache k) -> BinCache) ->
            (k -> v -> BinCache -> (Int, BinCache))
mkAddKey get set k _ bc = let (Known n m) = get bc
                          in (n, set bc (Known (n+1) (M.insert k n m)))

-- Index a boxed array without forcing the element: a table entry stays
-- the memoized thunk it is until a consumer needs the value.
{-# INLINE lazyIndex #-}
lazyIndex :: String -> Array Int v -> Int -> IO v
lazyIndex who arr idx =
  case arr of
    Array lo hi _ arr# ->
      if idx < lo || idx > hi
      then internalError $ "BinData.lookupIdx: invalid " ++ who ++
                           " index " ++ show idx
      else case indexArray# arr# (case idx - lo of I# i# -> i#) of
             (# v #) -> return v

mkLookupIdx :: String -> (Tables -> Array Int v) -> Int -> In v
mkLookupIdx who get idx = In $ \env -> lazyIndex who (get (envTables env)) idx

-- A shared value occurs in the stream as its index into the table for
-- its kind.  The table entry (or string) is already there; nothing is
-- assigned or recorded while reading.
readShared :: (Bin a, Shared k a) => In a
readShared = do idx <- readBytes
                lookupIdx idx

-- actual Shared instance definitions

instance Shared FString FString where
  knownAs   = mkKnownAs string_cache
  addKey    = mkAddKey string_cache set_string_cache
  lookupIdx = mkLookupIdx "FString" tStrings

instance Shared IdKey Id where
  knownAs   = mkKnownAs id_cache
  addKey    = mkAddKey id_cache set_id_cache
  lookupIdx = mkLookupIdx "Id" tIds

instance Shared Position Position where
  knownAs   = mkKnownAs position_cache
  addKey    = mkAddKey position_cache set_position_cache
  lookupIdx = mkLookupIdx "Position" tPositions

instance Shared TypeKey Type where
  knownAs   = mkKnownAs type_cache
  addKey    = mkAddKey type_cache set_type_cache
  lookupIdx = mkLookupIdx "Type" tTypes

instance Shared ITypeKey IType where
  knownAs   = mkKnownAs itype_cache
  addKey    = mkAddKey itype_cache set_itype_cache
  lookupIdx = mkLookupIdx "IType" tITypes

-- ------------------------------------------------------------------
-- Lazy chunks

-- Read a sub-value written by toBinLazy.  The stream holds the chunk
-- number; the chunk index gives the extent of its bytes, which start
-- right here.  Normally the bytes are skipped and the value is a thunk
-- that decodes them on first force (with its own state block over the
-- shared tables, and an end-of-chunk check); in a strict run they are
-- decoded now, in place.
fromBinLazy :: (Bin a) => In a
fromBinLazy = In $ \env -> do
  c <- runInEnv readBytes env
  let tbl = envTables env
      st  = envState env
  when (c < 0 || c >= tChunkCount tbl) $
    internalError $ "BinData.fromBinLazy: invalid chunk number " ++ show c
  let off = tChunks tbl ! (2*c)
      len = tChunks tbl ! (2*c+1)
  cur <- peekByteOff st stateOffset :: IO Int
  when (cur /= off) $
    internalError $ "BinData.fromBinLazy: chunk " ++ show c ++
                    " expected at " ++ show off ++ ", found at " ++ show cur
  if envStrict env
    then do x <- runInEnv fromBin env
            end <- peekByteOff st stateOffset :: IO Int
            when (end /= off + len) $
              internalError $ "BinData.fromBinLazy: chunk " ++ show c ++
                              " decoded " ++ show (end - off) ++ " of " ++
                              show len ++ " bytes"
            n <- peekByteOff st stateChunks :: IO Int
            pokeByteOff st stateChunks (n + 1)
            return x
    else do _ <- advance "fromBinLazy" env len
            unsafeInterleaveIO $
              runAt tbl False off (off + len) ("chunk " ++ show c) fromBin

-- Run a reader over [start, end) of the container with a fresh state
-- block, requiring it to consume exactly those bytes.
runAt :: Tables -> Bool -> Int -> Int -> String -> In a -> IO a
runAt tbl strict start end what (In f) = do
  fp <- mallocForeignPtrBytes stateSize
  withForeignPtr fp $ \st -> do
    pokeByteOff st stateOffset start
    pokeByteOff st stateLimit end
    pokeByteOff st stateChunks (0 :: Int)
    x <- f (Env (tBytes tbl) st tbl strict)
    off <- peekByteOff st stateOffset :: IO Int
    when (off /= end) $
      internalError $ "BinData.decode: " ++ what ++ " decoded " ++
                      show (off - start) ++ " of " ++ show (end - start) ++
                      " bytes"
    when strict $ do
      seen <- peekByteOff st stateChunks :: IO Int
      when (seen /= tChunkCount tbl) $
        internalError $ "BinData.decodeStrict: " ++ show seen ++ " of " ++
                        show (tChunkCount tbl) ++ " chunks reached"
    return x

-- ------------------------------------------------------------------
-- The Bin typeclass defines automatic conversions to/from a
-- byte stream.  We include instances for common types.

class Bin a where
  -- these support sharing for some data types
  toBin :: a -> Out ()
  fromBin :: In a

  -- these are direct binary write/read with no sharing
  -- (although sub-structures may be shared)
  writeBytes :: a -> Out ()
  readBytes :: In a

  -- default definitions for toBin & fromBin provide no sharing
  toBin   = writeBytes
  fromBin = readBytes


instance Bin Word8 where
  writeBytes c = putB c
  readBytes    = getB

instance Bin Bool where
  writeBytes False = putI 0
  writeBytes True  = putI 1
  readBytes = do i <- getI
                 case i of
                   0 -> return False
                   1 -> return True
                   n -> internalError $ "BinData.Bin(Bool).readBytes: " ++ show n

instance Bin Double where
  writeBytes i = do let (m,n) = decodeFloat i
                    toBin m; toBin n
  readBytes = do m <- fromBin; n <- fromBin; return (encodeFloat m n)

instance Bin Int where
  writeBytes i = toBin (toInteger i)
  -- The Integer encoding, read in Int arithmetic.  Int arithmetic wraps
  -- modulo the word size, as fromInteger of the exact value would.
  readBytes = loop 0
    where loop :: Int -> In Int
          loop !n = do b <- getB
                       if b == endNeg
                        then return (negate n)
                        else if b == endPos
                              then return n
                              else loop (n * 254 + fromIntegral b)

instance Bin Integer where
  writeBytes i = if i < 0
                 then putBs (emit (-i) [endNeg])
                 else putBs (emit   i  [endPos])
    where emit 0 bs = bs
          emit n bs = emit (n `div` baseI) (fromInteger (n `mod` baseI) : bs)
  -- Accumulate in Int while the value cannot overflow, so that the
  -- small values that make up most of a file allocate no Integer per byte.
  readBytes = small 0
    where small :: Int -> In Integer
          small !n = do b <- getB
                        if b == endNeg
                         then return (negate (toInteger n))
                         else if b == endPos
                               then return (toInteger n)
                               else if n <= smallLimit
                                     then small (n * 254 + fromIntegral b)
                                     else loop (toInteger n * baseI + toInteger b)
          loop n = do b <- getB
                      if b == endNeg
                       then return (-n)
                       else if b == endPos
                             then return n
                             else loop (n * baseI + toInteger b)
          -- the largest n for which n * 254 + 253 still fits in an Int
          smallLimit = (maxBound - 253) `div` 254 :: Int

endNeg, endPos :: Word8
endNeg = 255
endPos = 254
baseI :: Integer
baseI = 254

-- Write a Word32 value as 4 bytes with LSB first
instance Bin Word32 where
  writeBytes x = putBs $ [ toEnum (fromEnum ((shiftR x sh) .&. 255))
                         | sh <- [0,8..31]
                         ]
  readBytes  = do bs <- getN 4
                  return $ foldl1 (.|.) [ shiftL (toEnum (fromEnum x)) sh
                                        | (sh,x) <- zip [0,8..31] bs
                                        ]

-- For list types, we use a representation
-- where each non-nil element is given a 1 prefix and the
-- nil element is represented by 0.  This is not as
-- space-efficient as a length + bytes representation, but
-- it has better laziness properties.
instance {-# OVERLAPPABLE #-} (Bin a) => Bin [a] where
  writeBytes []     = putI 0
  writeBytes (x:xs) = do { putI 1; toBin x; toBin xs }
  -- Accumulated and reversed so a long list does not grow the stack (see readN).
  readBytes = go []
    where go acc = do i <- getI
                      case i of
                        0 -> return (reverse acc)
                        1 -> do x <- fromBin
                                go (x:acc)
                        n -> internalError $ "BinData.Bin([a]).readBytes: " ++ show n

-- For array types, we use a length + values representation
instance {-# OVERLAPPABLE #-} (IArray arr a, Ix i, Num i, Integral i, Bin a) => Bin (arr i a) where
  writeBytes x = let (lo,hi) = bounds x
                 in if (lo /= 0)
                    then internalError $ "BinData.Bin(Array).writeBytes: not 0-indexed"
                    else do toBin (toInteger (hi+1))
                            mapM_ toBin (elems x)
  readBytes = do len <- fromBin
                 xs <- readN (fromInteger len) fromBin
                 return $ listArray (0,fromInteger (len-1)) xs

instance {-# OVERLAPPING #-} (Bin a, Bin b) => Bin (a,b) where
  writeBytes (x,y) = do { toBin x; toBin y }
  readBytes = do x <- fromBin
                 y <- fromBin
                 return (x,y)

instance (Bin a, Bin b, Bin c) => Bin (a,b,c) where
  writeBytes (x,y,z) = do { toBin x; toBin y; toBin z }
  readBytes = do x <- fromBin
                 y <- fromBin
                 z <- fromBin
                 return (x,y,z)

instance (Bin a, Bin b, Bin c, Bin d) => Bin (a,b,c,d) where
  writeBytes (w,x,y,z) = do { toBin w; toBin x; toBin y; toBin z }
  readBytes = do w <- fromBin
                 x <- fromBin
                 y <- fromBin
                 z <- fromBin
                 return (w,x,y,z)

instance {-# OVERLAPPABLE #-} (Bin a) => Bin (Maybe a) where
  writeBytes Nothing  = putI 0
  writeBytes (Just x) = do { putI 1; toBin x }
  readBytes = do i <- getI
                 case i of
                   0 -> return Nothing
                   1 -> do { x <- fromBin; return (Just x) }
                   n -> internalError $ "BinData.Bin(Maybe a).readBytes: " ++ show n
instance Bin () where
  writeBytes () = return ()
  readBytes     = return ()

instance (Bin a, Bin b) => Bin (Either a b) where
  writeBytes (Left x)  = do { putI 0; toBin x }
  writeBytes (Right y) = do { putI 1; toBin y }
  readBytes = do i <- getI
                 case i of
                   0 -> do { x <- fromBin; return (Left x) }
                   1 -> do { y <- fromBin; return (Right y) }
                   n -> internalError $ "BinData.Bin(Either a b).readBytes: " ++ show n

instance (Ord a, Bin a, Bin b) => Bin (M.Map a b) where
    writeBytes omap = toBin (M.toList omap)
    readBytes       = do { ms <- fromBin; return (M.fromList ms) }

instance (Ord a, Bin a) => Bin (S.Set a) where
    writeBytes set = toBin (S.toList set)
    readBytes      = do { ss <- fromBin; return (S.fromList ss) }

-- ---------
-- Bin Ids, Positions, etc.

instance Bin FString where
  writeBytes = writeBytes . getFString
  readBytes = mkFString <$> readBytes
  -- FStrings are shared
  toBin s = Out [S s] ()
  fromBin = readShared

instance {-# OVERLAPPABLE #-} Bin String where
    writeBytes fs = do let bs = TE.encodeUtf8 $ T.pack fs
                       toBin $ toInteger $ B.length bs
                       putBs $ B.unpack bs
    readBytes     = do len <- fromBin
                       bs <- getBS len
                       let !t = TSE.decodeUtf8 bs
                       return (TS.unpack t)

instance Bin Position where
    writeBytes (Position fs l c is_stdlib) = section "Position" $ do
        toBin fs
        toBin l
        toBin c
        toBin is_stdlib
    readBytes = do
        fs <- fromBin
        l <- fromBin
        c <- fromBin
        is_stdlib <- fromBin
        return (mkPositionFull fs l c is_stdlib)

    -- Positions are shared
    toBin p = Out [P p] ()
    fromBin = readShared

instance Bin Id where
    writeBytes i = section "Id" $
                   do let (mfs,fs) = getIdFStrings i
                      toBin (getIdPosition i)
                      toBin mfs
                      toBin fs
                      toBin (getIdProps i)
    readBytes = do pos <- fromBin
                   mfs <- fromBin
                   fs <- fromBin
                   props <- fromBin
                   return (setIdProps (mkQId pos mfs fs) props)

    -- Ids are shared
    toBin i = Out [I i] ()
    fromBin = readShared

instance Bin IdProp where
    writeBytes IdPCanFire         = putI 0
    writeBytes IdPWillFire        = putI 1
    writeBytes IdPProbe           = putI 2
    writeBytes IdPInternal        = putI 4
    writeBytes IdPReady           = putI 5
    writeBytes IdPGeneratedIfc    = putI 7
    writeBytes IdPMeth            = putI 9
    writeBytes IdPCommutativeTCon = putI 14
    writeBytes IdP_enable         = putI 15
    writeBytes IdP_keep           = putI 17
    writeBytes IdPRule            = putI 18
    writeBytes IdPSplitRule       = putI 19
    writeBytes IdPDict            = putI 20
    writeBytes IdPRenaming        = putI 21
    writeBytes IdP_hide           = putI 22
    writeBytes IdP_hide_all       = putI 23
    writeBytes IdP_suffixed       = putI 24
    writeBytes (IdP_SuffixCount count)
                                  = do putI 25 ; toBin count
    writeBytes IdP_bad_name       = putI 26
    writeBytes IdP_from_rhs       = putI 27
    writeBytes IdP_signed         = putI 28
    writeBytes IdP_NakedInst      = putI 30
    writeBytes (IdPDisplayName name)
                                  = do putI 31 ; toBin name
    writeBytes (IdP_TypeJoin a b) = do putI 32 ; toBin a ; toBin b
    writeBytes IdP_keepEvenUnused = putI 33
    writeBytes IdPMethodPredicate = putI 34
    writeBytes (IdPInlinedPositions poss)
                                  = do putI 35 ; toBin poss
    writeBytes IdPParserGenerated = putI 36
    writeBytes IdPIncoherent      = putI 37
    writeBytes IdPCAF             = putI 38
    readBytes = do
        i <- getI
        case i of
          0  -> return IdPCanFire
          1  -> return IdPWillFire
          2  -> return IdPProbe
          4  -> return IdPInternal
          5  -> return IdPReady
          7  -> return IdPGeneratedIfc
          9  -> return IdPMeth
          14 -> return IdPCommutativeTCon
          15 -> return IdP_enable
          17 -> return IdP_keep
          18 -> return IdPRule
          19 -> return IdPSplitRule
          20 -> return IdPDict
          21 -> return IdPRenaming
          22 -> return IdP_hide
          23 -> return IdP_hide_all
          24 -> return IdP_suffixed
          25 -> do k <- fromBin; return (IdP_SuffixCount k)
          26 -> return IdP_bad_name
          27 -> return IdP_from_rhs
          28 -> return IdP_signed
          30 -> return IdP_NakedInst
          31 -> do k <- fromBin; return (IdPDisplayName k)
          32 -> do a <- fromBin; b <- fromBin; return $ IdP_TypeJoin a b
          33 -> return IdP_keepEvenUnused
          34 -> return IdPMethodPredicate
          35 -> do poss <- fromBin; return (IdPInlinedPositions poss)
          36 -> return IdPParserGenerated
          37 -> return IdPIncoherent
          38 -> return IdPCAF
          n  -> internalError $ "BinData.Bin(IdProp).readBytes: " ++ show n


-- ---------
-- Bin PProp, Pragmas, etc.

instance Bin PProp where
    writeBytes (PPverilog)              = putI 0
    writeBytes (PPalwaysReady ms)       = do putI 1; toBin ms
    writeBytes (PPalwaysEnabled ms)     = do putI 2; toBin ms
    writeBytes (PPscanInsert i)         = do putI 4; toBin i
    writeBytes (PPbitBlast)             = putI 5
    writeBytes (PPCLK s)                = do putI 6; toBin s
    writeBytes (PPRSTN s)               = do putI 7; toBin s
    writeBytes (PPoptions os)           = do putI 8; toBin os
    writeBytes (PPmethod_scheduling si) = do putI  9; toBin si
    -- 11 is obsolete (used to be PPgate_default_clock)
    writeBytes (PPgate_input_clocks cs) = do putI 12; toBin cs
    writeBytes (PPdoc txt)              = do putI 13; toBin txt
    writeBytes (PPperf_spec ranks)      = do putI 14; toBin ranks
    writeBytes (PPenabledWhenReady ms)  = do putI 15; toBin ms
    writeBytes (PPparam is)             = do putI 16; toBin is
    writeBytes (PPforeignImport i)      = do putI 17; toBin i
    writeBytes (PPinst_name i)          = do putI 18; toBin i
    writeBytes (PPclock_family is)      = do putI 19; toBin is
    writeBytes (PPclock_ancestors ils)  = do putI 20; toBin ils
    writeBytes (PPGATE s)               = do putI 21; toBin s
    writeBytes (PPclock_osc ps)         = do putI 22; toBin ps
    writeBytes (PPclock_gate ps)        = do putI 23; toBin ps
    writeBytes (PPgate_inhigh is)       = do putI 24; toBin is
    writeBytes (PPgate_unused is)       = do putI 25; toBin is
    writeBytes (PPreset_port ps)        = do putI 26; toBin ps
    writeBytes (PParg_param ps)         = do putI 27; toBin ps
    writeBytes (PParg_port ps)          = do putI 28; toBin ps
    writeBytes (PParg_clocked_by ps)    = do putI 29; toBin ps
    writeBytes (PParg_reset_by ps)      = do putI 30; toBin ps
    writeBytes (PPdeprecate txt)        = do putI 31; toBin txt
    writeBytes (PPinst_hide)            = putI 32
    writeBytes (PPinst_hide_all)        = putI 33
    writeBytes (PPdefault_clock_arg i)  = do putI 34; toBin i
    writeBytes (PPdefault_reset_arg i)  = do putI 35; toBin i
    readBytes = do
        i <- getI
        case i of
          0  -> return PPverilog
          1  -> do ms <- fromBin; return (PPalwaysReady ms)
          2  -> do ms <- fromBin; return (PPalwaysEnabled ms)
          4  -> do i <- fromBin; return (PPscanInsert i)
          5  -> return PPbitBlast
          6  -> do i <- fromBin; return (PPCLK i)
          7  -> do i <- fromBin; return (PPRSTN i)
          8  -> do os <- fromBin; return (PPoptions os)
          9  -> do si <- fromBin; return (PPmethod_scheduling si)
          -- for backward compatibility (used to be PPgate_default_clock)
          11 -> return (PPgate_input_clocks [idDefaultClock])
          12 -> do cs <- fromBin; return (PPgate_input_clocks cs)
          13 -> do txt <- fromBin; return (PPdoc txt)
          14 -> do ranks <- fromBin; return (PPperf_spec ranks)
          15 -> do ms <- fromBin; return (PPenabledWhenReady ms)
          16 -> do is <- fromBin; return (PPparam is)
          17 -> do i <- fromBin; return (PPforeignImport i)
          18 -> do i <- fromBin; return (PPinst_name i)
          19 -> do is <- fromBin; return (PPclock_family is)
          20 -> do ils <- fromBin; return (PPclock_ancestors ils)
          21 -> do i <- fromBin; return (PPGATE i)
          22 -> do ps <- fromBin; return (PPclock_osc ps)
          23 -> do ps <- fromBin; return (PPclock_gate ps)
          24 -> do is <- fromBin; return (PPgate_inhigh is)
          25 -> do is <- fromBin; return (PPgate_unused is)
          26 -> do ps <- fromBin; return (PPreset_port ps)
          27 -> do ps <- fromBin; return (PParg_param ps)
          28 -> do ps <- fromBin; return (PParg_port ps)
          29 -> do ps <- fromBin; return (PParg_clocked_by ps)
          30 -> do ps <- fromBin; return (PParg_reset_by ps)
          31 -> do txt <- fromBin; return (PPdeprecate txt)
          32 -> return PPinst_hide
          33 -> return PPinst_hide_all
          34 -> do i <- fromBin; return (PPdefault_clock_arg i)
          35 -> do i <- fromBin; return (PPdefault_reset_arg i)
          n  -> internalError $ "BinData.Bin(PProp).readBytes: " ++ show n

instance Bin PPnm where
    writeBytes (PPnmOne i) = do putI 0; toBin i
    writeBytes (PPnmArray i h l) = do putI 1; toBin i; toBin h; toBin l
    readBytes = do
        i <- getI
        case i of
          0 -> do i <- fromBin; return (PPnmOne i)
          1 -> do i <- fromBin; h <- fromBin; l <- fromBin; return (PPnmArray i h l)
          n -> internalError $ "BinData.Bin(PPnm).readBytes: " ++ show n

instance Bin IfcPragma where
    writeBytes (PIArgNames ids)     = do putI 0 ; toBin ids
    writeBytes (PIPrefixStr fs)     = do putI 1 ; toBin fs
    writeBytes (PIRdySignalName fs) = do putI 2 ; toBin fs
    writeBytes (PIEnSignalName fs)  = do putI 3 ; toBin fs
    writeBytes (PIResultName fs)    = do putI 4 ; toBin fs
    writeBytes (PIAlwaysRdy )       = putI 5
    writeBytes (PIAlwaysEnabled )   = putI 6
    readBytes = do
        i <- getI
        case i of
          0 -> do ids <- fromBin; return (PIArgNames ids )
          1 -> do fs <- fromBin;  return (PIPrefixStr fs)
          2 -> do fs <- fromBin;  return (PIRdySignalName fs)
          3 -> do fs <- fromBin;  return (PIEnSignalName fs)
          4 -> do fs <- fromBin;  return (PIResultName fs)
          5 -> do                 return (PIAlwaysRdy )
          6 -> do                 return (PIAlwaysEnabled)
          n -> internalError $ "BinData.Bin(IfcPragms).readBytes: " ++ show n

instance Bin RulePragma where
    writeBytes RPfireWhenEnabled                = putI 0
    writeBytes RPnoImplicitConditions           = putI 1
    writeBytes RPaggressiveImplicitConditions   = putI 2
    writeBytes RPconservativeImplicitConditions = putI 3
    writeBytes RPnoWarn                         = putI 4
    writeBytes RPwarnAllConflicts                  = putI 5
    writeBytes RPcanScheduleFirst               = putI 6
    writeBytes RPclockCrossingRule              = putI 7
    writeBytes (RPdoc txt)                      = do putI 8; toBin txt
    writeBytes RPhide                           = putI 9
    readBytes = do tag <- getI
                   case tag of
                     0 -> return RPfireWhenEnabled
                     1 -> return RPnoImplicitConditions
                     2 -> return RPaggressiveImplicitConditions
                     3 -> return RPconservativeImplicitConditions
                     4 -> return RPnoWarn
                     5 -> return RPwarnAllConflicts
                     6 -> return RPcanScheduleFirst
                     7 -> return RPclockCrossingRule
                     8 -> do txt <- fromBin; return (RPdoc txt)
                     9 -> return RPhide
                     n -> internalError $ "BinData.Bin(RulePragma).readBytes: " ++ show n

instance (Bin t, Ord t) => Bin (SchedulePragma t) where
    writeBytes (SPUrgency i)           = do putI 0; toBin i
    writeBytes (SPExecutionOrder i)    = do putI 1; toBin i
    writeBytes (SPPreempt ids1 ids2)   = do putI 2; toBin ids1; toBin ids2
    writeBytes (SPSchedule si)         = do putI 3; toBin si
    writeBytes (SPMutuallyExclusive i) = do putI 4; toBin i
    writeBytes (SPConflictFree i)      = do putI 5; toBin i
    readBytes = do tag <- getI
                   case tag of
                     0 -> do i <- fromBin; return (SPUrgency i)
                     1 -> do i <- fromBin; return (SPExecutionOrder i)
                     2 -> do ids1 <- fromBin; ids2 <- fromBin
                             return (SPPreempt ids1 ids2)
                     3 -> do si <- fromBin; return (SPSchedule si)
                     4 -> do i <- fromBin; return (SPMutuallyExclusive i)
                     5 -> do i <- fromBin; return (SPConflictFree i)
                     n -> internalError $ "BinData.Bin(SchedulePragma).readBytes: " ++ show n

-- ----------
-- Bin AClockDomain

instance Bin ClockDomain where
    writeBytes cd = toBin (writeClockDomain cd)
    readBytes     = do { i <- fromBin; return (readClockDomain i) }

instance Bin AClock where
    writeBytes (AClock osc gate) = do toBin osc; toBin gate
    readBytes = do osc <- fromBin
                   gate <- fromBin
                   return (AClock osc gate)

-- ----------
-- Bin AReset

instance Bin ResetId where
    writeBytes cd = toBin (writeResetId cd)
    readBytes = do { i <- fromBin; return (readResetId i) }

instance Bin AReset where
    writeBytes (AReset wire) = toBin wire
    readBytes = do { wire <- fromBin; return (AReset wire) }

-- ----------
-- Bin AInout

instance Bin AInout where
    writeBytes (AInout wire) = toBin wire
    readBytes = do { wire <- fromBin; return (AInout wire) }

-- ----------
-- Bin VWireInfo

instance Bin VWireInfo where
    writeBytes (WireInfo clk rst args) = do toBin clk; toBin rst; toBin args
    readBytes = do clk <- fromBin
                   rst <- fromBin
                   args <- fromBin
                   return (WireInfo clk rst args)

instance Bin VClockInfo where
    writeBytes (ClockInfo in_cs out_cs as ss) =
        do toBin in_cs; toBin out_cs; toBin as; toBin ss
    readBytes = do in_cs <- fromBin
                   out_cs <- fromBin
                   as <- fromBin
                   ss <- fromBin;
                   return (ClockInfo in_cs out_cs as ss)

instance Bin VResetInfo where
    writeBytes (ResetInfo in_rs out_rs) = do toBin in_rs; toBin out_rs
    readBytes = do in_rs <- fromBin
                   out_rs <- fromBin
                   return (ResetInfo in_rs out_rs)

instance Bin VArgInfo where
    writeBytes (Param a)    = do putI 0; toBin a
    writeBytes (Port a b c) = do putI 1; toBin a; toBin b; toBin c
    writeBytes (ClockArg a) = do putI 2; toBin a
    writeBytes (ResetArg a) = do putI 3; toBin a
    writeBytes (InoutArg a b c) = do putI 4; toBin a; toBin b; toBin c
    readBytes = do
        i <- getI
        case i of
          0 -> do a <- fromBin; return (Param a)
          1 -> do a <- fromBin; b <- fromBin; c <- fromBin; return (Port a b c)
          2 -> do a <- fromBin; return (ClockArg a)
          3 -> do a <- fromBin; return (ResetArg a)
          4 -> do a <- fromBin; b <- fromBin; c <- fromBin;
                  return (InoutArg a b c)
          n -> internalError $ "BinData.Bin(VArgInfo).readBytes: " ++ show n

instance Bin VName where
    writeBytes (VName s) = toBin s
    readBytes = do { s <- fromBin; return (VName s) }

-- ----------
-- Bin AType

instance Bin AType where
    writeBytes (ATBit sz)         = do putI 0; toBin sz
    writeBytes (ATString sz)      = do putI 1; toBin sz
    writeBytes (ATReal)           = do putI 2;
    writeBytes (ATArray sz t)     = do putI 3; toBin sz; toBin t
    writeBytes (ATAbstract i szs) = do putI 4; toBin i; toBin szs
    writeBytes (ATTuple ts)       = do putI 5; toBin ts
    readBytes = do
        i <- getI
        case i of
          0 -> do sz <- fromBin; return (ATBit sz)
          1 -> do sz <- fromBin; return (ATString sz)
          2 -> do return ATReal
          3 -> do sz <- fromBin; t <- fromBin; return (ATArray sz t)
          4 -> do i <- fromBin; szs <- fromBin; return (ATAbstract i szs)
          5 -> do ts <- fromBin; return (ATTuple ts)
          n -> internalError $ "GenABin.Bin(AType).readBytes: " ++ show n

-- ----------
-- Bin AExpr

instance Bin AExpr where
    writeBytes (APrim i t op args) = section "AExpr" $
        do putI 0; toBin i; toBin t; toBin op; toBin args
    writeBytes (AMethCall t obj meth args) = section "AExpr" $
        do putI 1; toBin t; toBin obj; toBin meth; toBin args
    writeBytes (AMethValue t obj meth) = section "AExpr" $
        do putI 2; toBin t; toBin obj; toBin meth
    writeBytes (ANoInlineFunCall t obj fun args) = section "AExpr" $
        do putI 3; toBin t; toBin obj; toBin fun; toBin args
    writeBytes (AFunCall t obj fun isC args) = section "AExpr" $
        do putI 4; toBin t; toBin obj; toBin fun; toBin isC; toBin args
    writeBytes (ATaskValue t obj fun isC cookie) = section "AExpr" $
        do putI 5; toBin t; toBin obj; toBin fun; toBin isC; toBin cookie
    writeBytes (ASPort t i)    = section "AExpr" $ do putI  6; toBin t; toBin i
    writeBytes (ASParam t i)   = section "AExpr" $ do putI  7; toBin t; toBin i
    writeBytes (ASDef t i)     = section "AExpr" $ do putI  8; toBin t; toBin i
    writeBytes (ASInt i t val) = section "AExpr" $ do putI  9; toBin i; toBin t; toBin val
    writeBytes (ASStr i t str) = section "AExpr" $ do putI 10; toBin i; toBin t; toBin str
    writeBytes (ASAny t me)       = section "AExpr" $ do putI 11; toBin t; toBin me
    writeBytes (ASClock t clk) = section "AExpr" $ do putI 12; toBin t; toBin clk
    writeBytes (ASReset t rst) = section "AExpr" $ do putI 13; toBin t; toBin rst
    writeBytes (AMGate t obj clk) = section "AExpr" $ do putI 14; toBin t; toBin obj; toBin clk
    writeBytes (ASInout t iot) = section "AExpr" $ do putI 15; toBin t; toBin iot
    writeBytes (ASReal i t val) = section "AExpr" $ do putI 16; toBin i; toBin t; toBin val
    writeBytes (ATupleSel t e idx) = section "AExpr" $ do putI 17; toBin t; toBin e; toBin idx
    writeBytes (ATuple t es) = section "AExpr" $ do putI 18; toBin t; toBin es
    readBytes = do
        i <- getI
        case i of
          0  -> do { i <- fromBin; t <- fromBin; op <- fromBin;
                     args <- fromBin; return (APrim i t op args); }
          1  -> do { t <- fromBin; obj <- fromBin; meth <- fromBin;
                     args <- fromBin; return (AMethCall t obj meth args); }
          2  -> do { t <- fromBin; obj <- fromBin; meth <- fromBin;
                     return (AMethValue t obj meth); }
          3  -> do { t <- fromBin; obj <- fromBin; fun <- fromBin;
                     args <- fromBin;
                     return (ANoInlineFunCall t obj fun args); }
          4  -> do { t <- fromBin; obj <- fromBin; fun <- fromBin;
                     isC <- fromBin; args <- fromBin;
                     return (AFunCall t obj fun isC args); }
          5  -> do { t <- fromBin; obj <- fromBin; fun <- fromBin;
                     isC <- fromBin; cookie <- fromBin;
                     return (ATaskValue t obj fun isC cookie); }
          6  -> do t <- fromBin; i <- fromBin; return (ASPort t i)
          7  -> do t <- fromBin; i <- fromBin; return (ASParam t i)
          8  -> do t <- fromBin; i <- fromBin; return (ASDef t i)
          9  -> do { i <- fromBin; t <- fromBin; val <- fromBin;
                     return (ASInt i t val) }
          10 -> do { i <- fromBin; t <- fromBin; str <- fromBin;
                     return (ASStr i t str) }
          11 -> do t <- fromBin; me <- fromBin; return (ASAny t me)
          12 -> do t <- fromBin; clk <- fromBin; return (ASClock t clk)
          13 -> do t <- fromBin; rst <- fromBin; return (ASReset t rst)
          14 -> do { t <- fromBin; obj <- fromBin; clk <- fromBin;
                     return (AMGate t obj clk); }
          15 -> do t <- fromBin; iot <- fromBin; return (ASInout t iot)
          16 -> do { i <- fromBin; t <- fromBin; val <- fromBin;
                     return (ASReal i t val) }
          17 -> do { t <- fromBin; e <- fromBin; idx <- fromBin;
                     return (ATupleSel t e idx) }
          18 -> do { t <- fromBin; es <- fromBin;
                     return (ATuple t es) }
          n  -> internalError $ "GenABin.Bin(IExpr).readBytes: " ++ show n
    -- toBin e = Out [AExp e] ()
    -- fromBin = readShared

instance Bin IntLit where
    writeBytes (IntLit w b i) = do toBin w; toBin b; toBin i
    readBytes = do w <- fromBin
                   b <- fromBin
                   i <- fromBin
                   return (IntLit w b i)

instance Bin ANoInlineFun where
    writeBytes (ANoInlineFun s ts ps mi) = do toBin s; toBin ts; toBin ps; toBin mi
    readBytes = do s <- fromBin
                   ts <- fromBin
                   ps <- fromBin
                   mi <- fromBin
                   return (ANoInlineFun s ts ps mi)

instance Bin PrimOp where
    writeBytes p = toBin (writePrimOp p)
    readBytes = do n <- fromBin; return (readPrimOp n)

-- ----------
-- Bin ADef

instance Bin ADef where
    writeBytes (ADef i t e p) = do toBin i; toBin t; toBin e; toBin p
    readBytes = do i <- fromBin
                   t <- fromBin
                   e <- fromBin
                   p <- fromBin
                   return (ADef i t e p)

instance Bin DefProp where
    writeBytes (DefP_Rule i) = do putI 0 ; toBin i
    writeBytes (DefP_Method i) = do putI 1 ; toBin i
    writeBytes (DefP_Instance i) = do putI 2 ; toBin i
    writeBytes DefP_NoCSE = putI 3
    writeBytes (DefP_DictRendering s) = do putI 4 ; toBin s
    writeBytes (DefP_DictKids ks) = do putI 5 ; toBin ks
    writeBytes (DefP_DictTypes ts) = do putI 6 ; toBin ts
    readBytes = do
      select <- getI
      case select of
        0 -> do i <- fromBin ; return $ DefP_Rule i
        1 -> do i <- fromBin ; return $ DefP_Method i
        2 -> do i <- fromBin ; return $ DefP_Instance i
        3 -> return DefP_NoCSE
        4 -> do s <- fromBin ; return $ DefP_DictRendering s
        5 -> do ks <- fromBin ; return $ DefP_DictKids ks
        6 -> do ts <- fromBin ; return $ DefP_DictTypes ts
        n -> internalError $ "BinData.Bin(DefProp).readBytes: " ++ show n

-- ------------
-- Bin VModInfo

instance Bin VModInfo where
    writeBytes x = section "VModInfo" $
                   do { toBin (vName x); toBin (vClk x); toBin (vRst x);
                        toBin (vArgs x); toBin (vFields x); toBin (vSched x);
                        toBin (vPath x); }
    readBytes = do { name <- fromBin; clk <- fromBin; rst <- fromBin;
                     args <- fromBin; meths <- fromBin; sch <- fromBin;
                     p <- fromBin;
                     return (mkVModInfo name clk rst args meths sch p); }

instance Bin VFieldInfo where
    writeBytes (Method a b c d e f g) =
        do { putI 0; toBin a; toBin b; toBin c; toBin d; toBin e; toBin f;
             toBin g; }
    writeBytes (Clock i) = do putI 1; toBin i
    writeBytes (Reset i) = do putI 2; toBin i
    writeBytes (Inout i p c r) = do putI 3; toBin i; toBin p; toBin c; toBin r
    readBytes = do
        i <- getI
        case i of
          0 -> do a <- fromBin; b <- fromBin; c <- fromBin; d <- fromBin;
                  e <- fromBin; f <- fromBin; g <- fromBin;
                  return (Method a b c d e f g)
          1 -> do a <- fromBin; return (Clock a)
          2 -> do a <- fromBin; return (Reset a)
          3 -> do i <- fromBin; p <- fromBin; c <- fromBin; r <- fromBin;
                  return (Inout i p c r)
          n -> internalError $ "GenABin.Bin(VFieldInfo).readBytes: " ++ show n

instance (Bin a, Ord a) => Bin (SchedInfo a) where
    writeBytes (SchedInfo mci rms rbm ccm) = do toBin mci
                                                toBin rms
                                                toBin rbm
                                                toBin ccm
    readBytes = do mci <- fromBin
                   rms <- fromBin
                   rbm <- fromBin
                   ccm <- fromBin
                   return (SchedInfo mci rms rbm ccm)

instance (Bin a, Ord a) => Bin (MethodConflictInfo a) where
    writeBytes mci@(MethodConflictInfo cfs sbs mes ps sbrs cs exts) =
        section "MethodConflictInfo" $
        do let meths = extractFromMethodConflictInfo mci
               arr :: Array Int a
               arr = listArray (0,length(meths)-1) meths
               arr_map = M.fromList (zip meths [0..])
           toBin arr -- write array of methods
           -- write list of pairs using compact bit matrix representations
           toBin (toBitMatrix arr_map cfs)
           toBin (toBitMatrix arr_map sbs)
           toBin mes
           toBin (toBitMatrix arr_map ps)
           toBin (toBitMatrix arr_map sbrs)
           toBin (toBitMatrix arr_map cs)
           toBin exts
    readBytes =
        do -- read method array
           arr <- (fromBin :: In (Array Int a))
           -- read lists of pairs from bit-matrix representations
           cfs0  <- fromBin
           let cfs = fromBitMatrix arr cfs0
           sbs0  <- fromBin
           let sbs = fromBitMatrix arr sbs0
           mes   <- fromBin
           ps0   <- fromBin
           let ps = fromBitMatrix arr ps0
           sbrs0 <- fromBin
           let sbrs = fromBitMatrix arr sbrs0
           cs0   <- fromBin
           let cs = fromBitMatrix arr cs0
           exts  <- fromBin;
           return (MethodConflictInfo cfs sbs mes ps sbrs cs exts)

type BitMatrix = UArray Int Word32

toBitMatrix :: (Ord a) => (M.Map a Int) -> [(a,a)] -> BitMatrix
toBitMatrix m ps =
    let n           = M.size m
        total_bits  = n * n
        total_words = (total_bits + 31) `quot` 32
        getIndex i = M.findWithDefault (internalError $ "toBitMatrix: bad index") i m
        idxs = [(getIndex a) * n + (getIndex b) | (a,b) <- ps]
    in accumArray (.|.) 0 (0,total_words-1) [ (i `quot` 32, bit (i `rem` 32))
                                            | i <- idxs
                                            ]

fromBitMatrix :: (Array Int a) -> BitMatrix -> [(a,a)]
fromBitMatrix marr mtx =
    let (_,n) = bounds marr
        len = n+1
        bits_of x = filter (testBit x) [0..31]
        ps = concat [ zip (repeat i) (bits_of w)
                    | (i,w) <- assocs mtx
                    , w /= 0
                    ]
    in [ (marr!row, marr!col)
       | (wi,bi) <- ps
       , let idx = wi*32 + bi
       , let row = idx `quot` len
       , let col = idx `rem` len
       ]

instance Bin VPathInfo where
    writeBytes (VPathInfo v) = section "VPathInfo" $ do toBin v
    readBytes = do v <- fromBin; return (VPathInfo v)

instance Bin VeriPortProp where
    writeBytes VPreg = putI 0
    writeBytes VPclock = putI 1
    writeBytes VPconst = putI 2
    writeBytes VPunused = putI 3
    writeBytes VPinhigh = putI 4
    writeBytes VPouthigh = putI 5
    writeBytes VPreset = putI 6
    writeBytes VPclockgate = putI 7
    writeBytes VPinout = putI 8
    readBytes = do
        i <- getI
        case i of
          0 -> return VPreg
          1 -> return VPclock
          2 -> return VPconst
          3 -> return VPunused
          4 -> return VPinhigh
          5 -> return VPouthigh
          6 -> return VPreset
          7 -> return VPclockgate
          8 -> return VPinout
          n -> internalError $ "GenABin.Bin(VeriPortProp).readBytes: " ++ show n

{-
instance Bin AStateLocPathComponent where
    writeBytes loc@(AStateLocPathComponent id1 id2 t) =
        section "AStateLocPathComponent" $
        do toBin id1; toBin id2; (section "CType" (toBin t))
    readBytes = do id1 <- fromBin; id2 <- fromBin; t <- fromBin;
                   return (AStateLocPathComponent id1 id2 t)

    -- AStateLocPathComponent is shared
    toBin l = Out [ASL l] ()
    fromBin = readShared
-}

-- ------------
-- Bin CType / CQType

-- AStateLocPathComponent contains CType
-- ABinModInfo also contains a CQType

instance Bin Type where
    writeBytes (TVar tvar) = do putI 0; toBin tvar
    writeBytes (TCon tcon) = do putI 1; toBin tcon
    writeBytes (TAp t1 t2) = do putI 2; toBin t1; toBin t2;
    writeBytes (TGen pos i) = do putI 3; toBin pos; toBin i
    writeBytes (TDefMonad pos) = internalError $ "BinData.write: TDefMonad"
    readBytes =
        do i <- getI
           case i of
             0 -> do tvar <- fromBin; return (TVar tvar)
             1 -> do tcon <- fromBin; return (TCon tcon)
             2 -> do t1 <- fromBin; t2 <- fromBin; return (TAp t1 t2)
             3 -> do pos <- fromBin; i <- fromBin; return (TGen pos i)
             n -> internalError $ "BinData.Bin(Type).readBytes: " ++ show n

    -- Type is shared
    toBin t = Out [T t] ()
    fromBin = readShared

instance Bin TyVar where
    writeBytes (TyVar i n k) = do toBin i; toBin n; toBin k
    readBytes = do i <- fromBin; n <- fromBin; k <- fromBin;
                   return (TyVar i n k)

instance Bin TyCon where
    writeBytes (TyCon i mk s) = do putI 0; toBin i; toBin mk; toBin s
    writeBytes (TyNum i pos) = do putI 1; toBin i; toBin pos
    writeBytes (TyStr s pos) = do putI 2; toBin s; toBin pos
    readBytes =
        do i <- getI
           case i of
             0 -> do i <- fromBin; mk <- fromBin; s <- fromBin;
                     return (TyCon i mk s)
             1 -> do i <- fromBin; pos <- fromBin; return (TyNum i pos)
             2 -> do s <- fromBin; pos <- fromBin; return (TyStr s pos)
             n -> internalError $ "BinData.Bin(TyCon).readBytes: " ++ show n

instance Bin TISort where
    writeBytes (TItype i t)     = do putI 0; toBin i; toBin t
    writeBytes (TIdata is enum) = do putI 1; toBin is; toBin enum
    writeBytes (TIstruct su is) = do putI 2; toBin su; toBin is
    writeBytes (TIabstract)     = do putI 3
    writeBytes (TIatf c ps t)   = do putI 4; toBin c; toBin ps; toBin t
    readBytes = do
        i <- getI
        case i of
          0 -> do i <- fromBin; t <- fromBin; return (TItype i t)
          1 -> do is <- fromBin; enum <- fromBin; return (TIdata is enum)
          2 -> do su <- fromBin; is <- fromBin; return (TIstruct su is)
          3 -> return TIabstract
          4 -> do c <- fromBin; ps <- fromBin; t <- fromBin;
                  return (TIatf c ps t)
          n -> internalError $ "BinData.Bin(TISort).readBytes: " ++ show n

instance Bin StructSubType where
    writeBytes SStruct           = putI 0
    writeBytes SClass            = putI 1
    writeBytes (SDataCon i nm)   = do putI 2 ; toBin i; toBin nm
    writeBytes (SInterface ps)   = do putI 3 ; toBin ps
    writeBytes (SPolyWrap i c f) = do putI 4; toBin i; toBin c; toBin f
    readBytes = do
        i <- getI
        case i of
          0 -> return SStruct
          1 -> return SClass
          2 -> do i  <- fromBin ; nm <- fromBin; return (SDataCon i nm)
          3 -> do ps <- fromBin ; return (SInterface ps)
          4 -> do i <- fromBin; c <- fromBin; f <- fromBin; return (SPolyWrap i c f)
          n -> internalError $ "BinData.Bin(StructSubType).readBytes: " ++ show n

instance Bin Kind where
  writeBytes KStar = putI 0
  writeBytes KNum = putI 1
  writeBytes (Kfun k1 k2) = do putI 2; toBin k1; toBin k2
  writeBytes (KVar i) = do putI 3; toBin i
  writeBytes KStr = putI 4
  readBytes =
      do i <- getI
         case i of
           0 -> return KStar
           1 -> return KNum
           2 -> do k1 <- fromBin; k2 <- fromBin; return (Kfun k1 k2)
           3 -> do i <- fromBin; return (KVar i)
           4 -> return KStr
           n -> internalError $ "BinData.Bin(Kind).readBytes: " ++ show n

instance Bin CQType where
    writeBytes (CQType ps t) = do toBin ps; toBin t
    readBytes = do ps <- fromBin; t <- fromBin; return (CQType ps t)

instance Bin CPred where
    writeBytes (CPred c ts) = do toBin c; toBin ts
    readBytes = do c <- fromBin; ts <- fromBin; return (CPred c ts)

instance Bin CTypeclass where
    writeBytes (CTypeclass i) = toBin i
    readBytes = do i <- fromBin; return (CTypeclass i)


-- ------------
-- Bin IType, IKind, etc.

instance Bin IType where
    writeBytes (ITForAll i k t) = do putI 0; toBin i; toBin k; toBin t
    writeBytes (ITAp t1 t2)     = do putI 1; toBin t1; toBin t2
    writeBytes (ITVar i)        = do putI 2; toBin i
    writeBytes (ITCon i k s)    = do putI 3; toBin i; toBin k; toBin s
    writeBytes (ITNum n)        = do putI 4; toBin n
    writeBytes (ITStr s)        = do putI 5; toBin s
    readBytes = do tag <- getI
                   case tag of
                     0 -> do i <- fromBin; k <- fromBin; t <- fromBin
                             return $ ITForAll i k t
                     1 -> do t1 <- fromBin; t2 <- fromBin
                             return $ ITAp t1 t2
                     2 -> do i <- fromBin; return $ ITVar i
                     3 -> do i <- fromBin; k <- fromBin; s <- fromBin
                             return $ ITCon i k s
                     4 -> do n <- fromBin; return $ ITNum n
                     5 -> do s <- fromBin; return $ ITStr s
                     n -> internalError $ "BinData.Bin(IType).readBytes: " ++ show n

    -- IType is shared
    toBin t = Out [IT t] ()
    fromBin = readShared

instance Bin IKind where
    writeBytes IKStar        = do putI 0
    writeBytes IKNum         = do putI 1
    writeBytes (IKFun k1 k2) = do putI 2; toBin k1; toBin k2
    writeBytes IKStr         = do putI 3
    readBytes = do tag <- getI
                   case tag of
                     0 -> return IKStar
                     1 -> return IKNum
                     2 -> do k1 <- fromBin; k2 <- fromBin; return (IKFun k1 k2)
                     3 -> return IKStr
                     n -> internalError $ "BinData.Bin(IKind).readBytes: " ++ show n

instance Bin UndefKind where
    writeBytes UNotUsed  = putI 0
    writeBytes UDontCare = putI 1
    writeBytes UNoMatch  = putI 2
    readBytes = do tag <- getI
                   case tag of
                     0 -> return UNotUsed
                     1 -> return UDontCare
                     2 -> return UNoMatch
                     n -> internalError $ "BinData.Bin(UndefKind).readBytes: " ++ show n


-- ------------------------------------------------------------------
-- This is a debugging facility to help in determining what structures
-- contribute the most bytes to the output byte stream.

-- XXX This should be a trace flag, but tracing can overflow the stack
-- when the data is too large, so I am leaving it as a compile-time
-- setting for now.
trace_bindata :: Bool
trace_bindata = False

type Histogram = M.Map String (Integer, Integer, Integer)

incr :: (Integer, Integer, Integer ) -> (Integer, Integer, Integer)
     -> (Integer, Integer, Integer)
incr (x1,y1,z1) (x2,y2,z2) = (x1+x2, y1+y2, z1+z2)

updateBytes :: [String] -> Integer -> Histogram -> Histogram
updateBytes [] _ h = h
-- updateBytes (s:ss) n h = let upds = (s, (n,0,n)):[(p, (0,0,n)) | p <- ss]
--                          in map_insertManyWith incr upds h
updateBytes (s:_) n h = M.insertWith incr s (n,0,n) h

incrSection :: String -> Histogram -> Histogram
incrSection s h = M.insertWith incr s (0,1,0) h

showHist :: Histogram -> String
showHist hist = let buckets = sort [ (n1,n2,n3,s)
                                   | (s,(n1,n2,n3)) <- (M.toList hist) ]
                    ls = [ (show n1) ++ " (" ++ (show n3) ++ ") count = " ++
                           (show n2) ++ " : " ++ s
                         | (n1,n2,n3,s) <- buckets ]
                    total = [ "----------------------------"
                            , (show (sum [ n | (n,_,_,_) <- buckets]))
                            ]
                in unlines (ls ++ total)

-- The histogram is taken over the element stream before sharing:
-- every occurrence of a shared value is charged its full payload.
buildHistogram :: [BinElem] -> Histogram
buildHistogram bes = snd (foldl build (["<UNCLAIMED>"], M.empty) (expand bes))
  where expand [] = []
        expand (x@(B _):xs) = x : expand xs
        expand (x@(Start _):xs) = x : expand xs
        expand (End:xs) = End : expand xs
        expand (S s:xs) = expand (out_data (writeBytes s) ++ xs)
        expand (I i:xs) = expand (out_data (writeBytes i) ++ xs)
        expand (P p:xs) = expand (out_data (writeBytes p) ++ xs)
        expand (T t:xs) = expand (out_data (writeBytes t) ++ xs)
        expand (IT t:xs) = expand (out_data (writeBytes t) ++ xs)
        expand (_:xs) = expand xs
        build ([], _) _ = internalError "unbalanced accounting marks"
        build (sec, hist) (B b) =
          (sec, updateBytes sec (toInteger (length b)) hist)
        build (sec, hist) (Start s) = ((s:sec), incrSection s hist)
        build ((_:sec), hist) End   = (sec, hist)
        build _ x = internalError $ "unexpected BinElem: " ++ (show x)

-- ------------------------------------------------------------------
-- Fixed-width integers and the footer

u32Bytes :: Int -> [Byte]
u32Bytes n
  | n < 0 || n > 0xffffffff = internalError $ "BinData: u32 out of range: " ++ show n
  | otherwise = [ fromIntegral (n `shiftR` sh) | sh <- [0, 8, 16, 24] ]

u64Bytes :: Int -> [Byte]
u64Bytes n
  | n < 0 = internalError $ "BinData: u64 out of range: " ++ show n
  | otherwise = [ fromIntegral (n `shiftR` sh) | sh <- [0, 8, 16, 24, 32, 40, 48, 56] ]

readUnsigned :: Int -> BS.ByteString -> Int -> Int
readUnsigned width s o
  | o < 0 || o + width > BS.length s =
      internalError "BinData.decode: index outside the container"
  | otherwise = foldr (\k acc -> (acc `shiftL` 8) .|.
                                 fromIntegral (BSU.unsafeIndex s (o + k)))
                      0 (enumFromTo 0 (width - 1))

readU32 :: BS.ByteString -> Int -> Int
readU32 = readUnsigned 4

readU64 :: BS.ByteString -> Int -> Int
readU64 = readUnsigned 8

-- Where the regions are.  Every field is derivable from the regions'
-- sizes, which lets the reader cross-check the whole layout at load.
data Footer = Footer { fBodiesEnd  :: !Int
                     , fTables     :: ![(Int, Int, Int)] -- per Kind: entries start, index start, count
                     , fStrStart   :: !Int
                     , fStrCount   :: !Int
                     , fChunkStart :: !Int
                     , fChunkCount :: !Int
                     }

footerMagic :: [Byte]
footerMagic = map (fromIntegral . fromEnum) "BSCBIN01"

footerFields :: Int
footerFields = 1 + 3 * length allKinds + 4

footerSize :: Int
footerSize = 8 * footerFields + length footerMagic

footerBytes :: Footer -> [Byte]
footerBytes ft =
    concatMap u64Bytes ([fBodiesEnd ft] ++
                        concat [ [a, b, c] | (a, b, c) <- fTables ft ] ++
                        [fStrStart ft, fStrCount ft, fChunkStart ft, fChunkCount ft])
    ++ footerMagic

parseFooter :: BS.ByteString -> Footer
parseFooter s =
    let len  = BS.length s
        base = len - footerSize
        field i = readU64 s (base + 8 * i)
        nk = length allKinds
        ft = Footer { fBodiesEnd  = field 0
                    , fTables     = [ (field (1 + 3*k), field (2 + 3*k), field (3 + 3*k))
                                    | k <- [0 .. nk - 1] ]
                    , fStrStart   = field (1 + 3*nk)
                    , fStrCount   = field (2 + 3*nk)
                    , fChunkStart = field (3 + 3*nk)
                    , fChunkCount = field (4 + 3*nk)
                    }
        bad what = internalError $ "BinData.decode: malformed container (" ++ what ++ ")"
        -- the regions are contiguous: each starts where the previous ends
        checkTables start [] = start
        checkTables start ((es, xs, n) : rest)
          | es /= start || xs < es || n < 0 = bad "table"
          | otherwise = checkTables (xs + 4 * (n + 1)) rest
    in if len < footerSize || BS.unpack (BS.drop (len - length footerMagic) s) /= footerMagic
       then bad "footer"
       else if fBodiesEnd ft < 0
            then bad "bodies"
            else let tend = checkTables (fBodiesEnd ft) (fTables ft)
                 in if fStrStart ft /= tend || fStrCount ft < 0
                    then bad "string table"
                    else if fChunkStart ft < fStrStart ft || fChunkCount ft < 0
                            || fChunkStart ft + 4 + 8 * fChunkCount ft /= base
                         then bad "chunk index"
                         else ft

-- ------------------------------------------------------------------
-- Assembling the container from the element stream

-- An open table entry: its kind and index, and its byte runs so far
-- (most recent first)
data Frame = Frame !TableKind !Int [[Byte]]

-- Closed table entries by kind, in completion order (most recent
-- first, as (index, bytes)); they are put into index order once, at
-- the end, instead of being kept sorted as they close.
data Entries = Entries { eIds       :: [(Int, BS.ByteString)]
                       , ePositions :: [(Int, BS.ByteString)]
                       , eTypes     :: [(Int, BS.ByteString)]
                       , eITypes    :: [(Int, BS.ByteString)]
                       }

noEntries :: Entries
noEntries = Entries [] [] [] []

addEntry :: TableKind -> Int -> BS.ByteString -> Entries -> Entries
addEntry KId       n b e = e { eIds       = (n, b) : eIds e }
addEntry KPosition n b e = e { ePositions = (n, b) : ePositions e }
addEntry KType     n b e = e { eTypes     = (n, b) : eTypes e }
addEntry KIType    n b e = e { eITypes    = (n, b) : eITypes e }

getEntries :: TableKind -> Entries -> [(Int, BS.ByteString)]
getEntries KId       = eIds
getEntries KPosition = ePositions
getEntries KType     = eTypes
getEntries KIType    = eITypes

cacheCount :: TableKind -> BinCache -> Int
cacheCount KId       bc = let (Known n _) = id_cache bc       in n
cacheCount KPosition bc = let (Known n _) = position_cache bc in n
cacheCount KType     bc = let (Known n _) = type_cache bc     in n
cacheCount KIType    bc = let (Known n _) = itype_cache bc    in n

-- The cold part of the writer's state.  The hot part (the open table
-- entries and the byte count of the bodies region) is passed as loop
-- arguments, so a plain byte run costs no record copy.
data WState = WState
  { wsRemap   :: Position -> Position     -- -remap-path-prefix
  , wsCache   :: !BinCache
  , wsStrings :: [FString]                -- first occurrences, most recent first
  , wsEntries :: !Entries                -- closed entries
  , wsChunks  :: !(IM.IntMap (Int, Int))  -- closed chunks: offset, length
  , wsOpen    :: [(Int, Int)]             -- open chunks: number, start offset
  , wsNChunks :: !Int
  }

-- The bytes of a value with no shared sub-structure (indices, strings)
plainBytes :: (Bin a) => a -> [Byte]
plainBytes x = concatMap plain (out_data (writeBytes x))
  where plain (B bs) = bs
        plain (Start _) = []
        plain End = []
        plain e = internalError $ "BinData.plainBytes: " ++ show e

-- Walk the element stream once, left to right.  Bytes go to the
-- innermost open table entry, or else to the bodies region, which is
-- produced lazily as the walk proceeds; the state is strict.  Index
-- assignment (per kind, first occurrence, parent before children) is
-- the same as the previous in-stream scheme's, so the output remains a
-- pure function of the content.
assemble :: (Position -> Position) -> [BinElem] -> [Byte]
assemble remapP = assembleGo st0 [] 0
  where st0 = WState { wsRemap = remapP, wsCache = unknownCache
                     , wsStrings = []
                     , wsEntries = noEntries, wsChunks = IM.empty
                     , wsOpen = [], wsNChunks = 0 }

-- arguments: the cold state, the open entries (innermost first), the
-- number of bytes emitted to the bodies region so far, the elements
assembleGo :: WState -> [Frame] -> Int -> [BinElem] -> [Byte]
assembleGo !st frames !main [] = finish st frames main
assembleGo !st frames !main (B bs : xs) = emit bs st frames main xs
assembleGo !st frames !main (Start _ : xs) = assembleGo st frames main xs
assembleGo !st frames !main (End : xs) = assembleGo st frames main xs
assembleGo !st frames !main (S s : xs) =
  case knownAs s s (wsCache st) of
    Just idx -> emit (plainBytes idx) st frames main xs
    Nothing  -> let (idx, cache') = addKey s s (wsCache st)
                in emit (plainBytes idx)
                        st { wsCache = cache', wsStrings = s : wsStrings st }
                        frames main xs
-- The Position transform (-remap-path-prefix) is applied before
-- sharing, so the cache is keyed on the stored (remapped) value:
-- positions that remap equal share a single entry.
assembleGo !st frames !main (P p : xs) =
  let p' = wsRemap st p in shared KPosition p' p' st frames main xs
assembleGo !st frames !main (I i : xs)  = shared KId (id_key i) i st frames main xs
assembleGo !st frames !main (T t : xs)  = shared KType (type_key t) t st frames main xs
assembleGo !st frames !main (IT t : xs) = shared KIType (itype_key t) t st frames main xs
assembleGo !st frames !main (EntryStart k n : xs) =
  assembleGo st (Frame k n [] : frames) main xs
assembleGo !st frames !main (EntryEnd : xs) =
  case frames of
    (Frame k n runs : fs) ->
      let !bytes = BS.pack (concat (reverse runs))
      in assembleGo st { wsEntries = addEntry k n bytes (wsEntries st) } fs main xs
    [] -> internalError "BinData.assemble: unbalanced table entry"
assembleGo !st frames !main (LazyStart : xs) =
  case frames of
    [] -> let c = wsNChunks st
              bs = plainBytes c
              !start = main + length bs
          in bs ++ assembleGo st { wsNChunks = c + 1, wsOpen = (c, start) : wsOpen st }
                              frames start xs
    _  -> internalError "BinData.assemble: lazy value inside a shared value"
assembleGo !st frames !main (LazyEnd : xs) =
  case wsOpen st of
    ((c, start) : rest) ->
      assembleGo st { wsOpen = rest
                    , wsChunks = IM.insert c (start, main - start) (wsChunks st) }
                 frames main xs
    [] -> internalError "BinData.assemble: unbalanced lazy value"

emit :: [Byte] -> WState -> [Frame] -> Int -> [BinElem] -> [Byte]
emit bs !st frames !main xs =
  case frames of
    [] -> bs ++ assembleGo st frames (main + length bs) xs
    (Frame k n runs : fs) -> assembleGo st (Frame k n (bs : runs) : fs) main xs

-- A shared occurrence: its index, and on first occurrence its payload,
-- routed to a new table entry (nested shared values inside it get
-- their own indices and entries as the walk reaches them).
shared :: (Bin v, Shared k v) =>
          TableKind -> k -> v -> WState -> [Frame] -> Int -> [BinElem] -> [Byte]
shared kind key v !st frames !main xs =
  case knownAs key v (wsCache st) of
    Just idx -> emit (plainBytes idx) st frames main xs
    Nothing  -> let (idx, cache') = addKey key v (wsCache st)
                in emit (plainBytes idx) st { wsCache = cache' } frames main
                        (EntryStart kind idx : out_data (writeBytes v) ++ (EntryEnd : xs))

-- The bodies region is complete: emit the tables, the string table,
-- the chunk index and the footer.
finish :: WState -> [Frame] -> Int -> [Byte]
finish st frames bodiesEnd
  | not (null frames) = internalError "BinData.assemble: unclosed table entry"
  | not (null (wsOpen st)) = internalError "BinData.assemble: unclosed lazy value"
  | IM.size (wsChunks st) /= wsNChunks st = internalError "BinData.assemble: chunks"
  | otherwise =
      let tableOf k = let es = getEntries k (wsEntries st)
                          n  = cacheCount k (wsCache st)
                          arr = array (0, n - 1) es :: Array Int BS.ByteString
                      in if length es /= n
                         then internalError $ "BinData.assemble: " ++ show k ++ " table"
                         else (elems arr, n)
          -- entries in index order, then count+1 u32 offsets
          tableRegion start k =
            let (es, n) = tableOf k
                lens = map BS.length es
                offs = scanl (+) 0 lens
                xs = start + sum lens
                bytes = concatMap BS.unpack es ++ concatMap u32Bytes offs
            in (xs + 4 * (n + 1), ((start, xs, n), bytes))
          (strStart, tableParts) = mapAccumL tableRegion bodiesEnd allKinds
          tableFields = map fst tableParts
          tableBytes = concatMap snd tableParts
          strs = reverse (wsStrings st)
          nStr = length strs
          strBytes = u32Bytes nStr ++ concatMap (plainBytes . getFString) strs
          chunkStart = strStart + length strBytes
          nChunks = wsNChunks st
          chunkBytes = u32Bytes nChunks ++
                       concat [ u32Bytes off ++ u32Bytes len
                              | (off, len) <- IM.elems (wsChunks st) ]
          ft = Footer { fBodiesEnd = bodiesEnd, fTables = tableFields
                      , fStrStart = strStart, fStrCount = nStr
                      , fChunkStart = chunkStart, fChunkCount = nChunks }
      in if nStr /= (let (Known n _) = string_cache (wsCache st) in n)
         then internalError "BinData.assemble: string table"
         else tableBytes ++ strBytes ++ chunkBytes ++ footerBytes ft

-- Run the Out monad and extract a byte stream.  The resulting
-- byte stream is generated lazily through the monad and preserves
-- sharing.

runOutWith :: (Position -> Position) -> Out () -> [Byte]
runOutWith remapP (Out xs _) =
    let bes   = toList xs
        bytes = assemble remapP bes
    in -- trace ("xs = " ++ (show xs)) $
       -- trace ("bytes = " ++ (show bytes)) $
       if trace_bindata
       then trace (showHist (buildHistogram bes)) $ bytes
       else bytes

-- Convenience function for encoding structures in the Bin typeclass
encode :: (Bin a) => a -> [Byte]
encode = encodeWith id

-- encode with a Position transform applied to stored positions
-- (-remap-path-prefix; see assemble)
encodeWith :: (Bin a) => (Position -> Position) -> a -> [Byte]
encodeWith remapP x = runOutWith remapP (toBin x)

-- ------------------------------------------------------------------
-- Reading the container

getU32 :: In Int
getU32 = do b0 <- getI; b1 <- getI; b2 <- getI; b3 <- getI
            return (b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 24))

inIO :: IO a -> In a
inIO act = In $ \_ -> act

-- Validate the indices, intern the string table in order, and build
-- the lazy tables.  A table entry is a thunk that decodes its bytes on
-- first force (memoized by the array; unsafePerformIO applies
-- noDuplicate, so two threads cannot both decode it); a parent entry
-- may reference entries with higher indices.
mkTables :: BS.ByteString -> Footer -> IO Tables
mkTables s ft = do
  let bodiesEnd = fBodiesEnd ft
      bad what = internalError $ "BinData.decode: malformed container (" ++ what ++ ")"
      -- entry offsets must be non-decreasing and end at the index
      checkTable k (es, xs, n) =
        let go i prev
              | i > n     = when (prev /= xs - es) $ bad (show k ++ " table extent")
              | otherwise = let o = readU32 s (xs + 4 * i)
                            in if o < prev then bad (show k ++ " table offsets")
                               else go (i + 1) o
        in go (0 :: Int) 0
  mapM_ (\(k, t) -> checkTable k t) (zip allKinds (fTables ft))
  let emptyBoxed :: Array Int v
      emptyBoxed = listArray (0, -1) []
      emptyChunks :: UArray Int Int
      emptyChunks = listArray (0, -1) []
      tbl0 = Tables { tBytes = s, tStrings = emptyBoxed
                    , tIds = emptyBoxed, tPositions = emptyBoxed
                    , tTypes = emptyBoxed, tITypes = emptyBoxed
                    , tChunks = emptyChunks, tChunkCount = 0 }
  -- the string table, interned now and in table order: FString ids
  -- are the Ord of every Id-keyed map, so this must happen before and
  -- in the same order as a sequential decode would have interned them
  strs <- runAt tbl0 False (fStrStart ft) (fChunkStart ft) "string table" $ do
            n <- getU32
            when (n /= fStrCount ft) $ bad "string count"
            let loop i acc
                  | i >= n    = return (reverse acc)
                  | otherwise = do str <- readBytes
                                   fs <- inIO (evaluate (mkFString str))
                                   loop (i + 1) (fs : acc)
            loop (0 :: Int) []
  let strArr = listArray (0, fStrCount ft - 1) strs :: Array Int FString
  -- the chunk index: every chunk lies within the bodies region
  chunks <- runAt tbl0 False (fChunkStart ft) (BS.length s - footerSize) "chunk index" $ do
              n <- getU32
              when (n /= fChunkCount ft) $ bad "chunk count"
              let loop i acc
                    | i >= n    = return (reverse acc)
                    | otherwise = do off <- getU32
                                     len <- getU32
                                     when (off + len > bodiesEnd) $ bad ("chunk " ++ show i)
                                     loop (i + 1) (len : off : acc)
              loop (0 :: Int) []
  let nChunks = fChunkCount ft
      chunkArr = listArray (0, 2 * nChunks - 1) chunks :: UArray Int Int
      entryBounds (es, xs, _) i = (es + readU32 s (xs + 4 * i), es + readU32 s (xs + 4 * (i + 1)))
      lazyTable :: (Bin v) => TableKind -> Array Int v
      lazyTable k =
        let t@(_, _, n) = fTables ft !! fromEnum k
            entry i = let (a, b) = entryBounds t i
                      in unsafePerformIO
                           (runAt tables False a b (show k ++ " entry " ++ show i) readBytes)
        in listArray (0, n - 1) [ entry i | i <- [0 .. n - 1] ]
      tables = Tables { tBytes = s, tStrings = strArr
                      , tIds = lazyTable KId, tPositions = lazyTable KPosition
                      , tTypes = lazyTable KType, tITypes = lazyTable KIType
                      , tChunks = chunkArr, tChunkCount = nChunks }
  return tables

decodeContainer :: (Bin a) => Bool -> BS.ByteString -> a
decodeContainer strict s = unsafePerformIO $ do
  let ft = parseFooter s
  tbl <- mkTables s ft
  x <- runAt tbl strict 0 (fBodiesEnd ft) "root" fromBin
  when strict $ do
    let force :: forall v . Array Int v -> IO ()
        force arr = mapM_ evaluate (elems arr)
    force (tIds tbl)
    force (tPositions tbl)
    force (tTypes tbl)
    force (tITypes tbl)
  return x

-- Decode a container.  The root is read now; shared table entries and
-- lazy chunks are decoded on demand.
decode :: (Bin a) => BS.ByteString -> a
decode = decodeContainer False

-- Decode a container completely: every lazy chunk is decoded in place
-- and every table entry is forced, so any malformed byte is reported
-- here rather than on first use.
decodeStrict :: (Bin a) => BS.ByteString -> a
decodeStrict = decodeContainer True

-- The hash authenticates the whole encoding: a fold over every byte,
-- independent of what the consumer later forces.
decodeWithHash :: (Bin a) => BS.ByteString -> (a,String)
decodeWithHash s = (decode s, showHash (BS.foldl' nextHashByte hashInit s))
