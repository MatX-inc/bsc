-- Buffered ASCII and UTF-8 record writer for TypeLinks (.tl) output.
--
-- Records are poked straight into a 1 MiB ForeignPtr buffer and flushed to
-- the file handle when full so each record allocates nothing beyond its
-- builder closure.  When no output file is open (in-memory provenance
-- capture without BSC_TYPELINKS=1), OutNothing discards writes in O(1).
module TypeLinksOut(
        Out, noOut, newOut, closeOut, write,
        Builder, char7, string7, stringUtf8, intDec
    ) where

import Control.Monad(when)
import Data.Array.Base(unsafeRead, unsafeWrite)
import Data.Array.IO(IOUArray)
import Data.Array.MArray(newArray)
import Data.Bits((.&.), (.|.), shiftR)
import Data.Char(ord)
import Data.Word(Word8)
import Foreign.ForeignPtr(ForeignPtr, mallocForeignPtrBytes, touchForeignPtr)
import Foreign.ForeignPtr.Unsafe(unsafeForeignPtrToPtr)
import Foreign.Ptr(Ptr)
import Foreign.Storable(pokeByteOff)
import System.IO(Handle, hPutBuf, hClose)

data Out
  = OutNone
  | Out { out_h   :: Handle,
          out_buf :: !(ForeignPtr Word8),
          out_ptr :: !(Ptr Word8),
          out_pos :: !(IOUArray Int Int) }

noOut :: Out
noOut = OutNone

outSize :: Int
outSize = 1048576

newOut :: Handle -> IO Out
newOut h = do
  buf <- mallocForeignPtrBytes outSize
  pos <- newArray (0, 0) 0
  return (Out { out_h = h, out_buf = buf, out_ptr = unsafeForeignPtrToPtr buf,
                out_pos = pos })

flush :: Out -> IO ()
flush OutNone = return ()
flush o = do
  n <- unsafeRead (out_pos o) 0
  unsafeWrite (out_pos o) 0 0
  when (n > 0) $ hPutBuf (out_h o) (out_ptr o) n
  touchForeignPtr (out_buf o)

closeOut :: Out -> IO ()
closeOut OutNone = return ()
closeOut o = do
  flush o
  hClose (out_h o)

{-# INLINE room #-}
room :: Out -> Int -> IO Int
room OutNone _ = return 0
room o k = do
  p <- unsafeRead (out_pos o) 0
  if p + k <= outSize then return p
    else do flush o
            return 0

{-# INLINE byteAt #-}
byteAt :: Out -> Int -> Int -> IO ()
byteAt OutNone _ _ = return ()
byteAt o p b = pokeByteOff (out_ptr o) p (fromIntegral b :: Word8)

newtype Builder = Builder (Out -> IO ())

instance Semigroup Builder where
  {-# INLINE (<>) #-}
  Builder a <> Builder b = Builder (\ o -> a o >> b o)

instance Monoid Builder where
  {-# INLINE mempty #-}
  mempty = Builder (\ _ -> return ())

{-# INLINE write #-}
write :: Out -> Builder -> IO ()
write OutNone _ = return ()
write o (Builder f) = do
  f o
  touchForeignPtr (out_buf o)

{-# INLINE char7 #-}
char7 :: Char -> Builder
char7 c = Builder (\ o -> putByte o (ord c .&. 0x7f))

{-# INLINE putByte #-}
putByte :: Out -> Int -> IO ()
putByte OutNone _ = return ()
putByte o b = do
  p <- room o 1
  byteAt o p b
  unsafeWrite (out_pos o) 0 (p + 1)

{-# INLINE string7 #-}
string7 :: String -> Builder
string7 s = Builder (\ o -> putString7 o s)

putString7 :: Out -> String -> IO ()
putString7 OutNone _ = return ()
putString7 _ [] = return ()
putString7 o (c : cs) = do putByte o (ord c .&. 0x7f)
                           putString7 o cs

{-# INLINE stringUtf8 #-}
stringUtf8 :: String -> Builder
stringUtf8 s = Builder (\ o -> putStringUtf8 o s)

putStringUtf8 :: Out -> String -> IO ()
putStringUtf8 OutNone _ = return ()
putStringUtf8 _ [] = return ()
putStringUtf8 o (c : cs) = do
  let x = ord c
  if x < 0x80 then putByte o x
    else do
      p <- room o 4
      if x < 0x800
        then do byteAt o p (0xc0 .|. (x `shiftR` 6))
                byteAt o (p + 1) (0x80 .|. (x .&. 0x3f))
                unsafeWrite (out_pos o) 0 (p + 2)
        else if x < 0x10000
        then do byteAt o p (0xe0 .|. (x `shiftR` 12))
                byteAt o (p + 1) (0x80 .|. ((x `shiftR` 6) .&. 0x3f))
                byteAt o (p + 2) (0x80 .|. (x .&. 0x3f))
                unsafeWrite (out_pos o) 0 (p + 3)
        else do byteAt o p (0xf0 .|. (x `shiftR` 18))
                byteAt o (p + 1) (0x80 .|. ((x `shiftR` 12) .&. 0x3f))
                byteAt o (p + 2) (0x80 .|. ((x `shiftR` 6) .&. 0x3f))
                byteAt o (p + 3) (0x80 .|. (x .&. 0x3f))
                unsafeWrite (out_pos o) 0 (p + 4)
  putStringUtf8 o cs

{-# INLINE intDec #-}
intDec :: Int -> Builder
intDec n = Builder (\ o -> putIntDec o n)

putIntDec :: Out -> Int -> IO ()
putIntDec OutNone _ = return ()
putIntDec o n
  | n >= 0 = do p <- room o 20
                digits p n
  | n == minBound = putString7 o (show n)
  | otherwise = do p <- room o 21
                   byteAt o p (ord '-')
                   digits (p + 1) (negate n)
  where
    digits p m = do
      let len = count 1 m
          end = p + len
          go q x | q < p = return ()
                 | otherwise = do byteAt o q (ord '0' + x `rem` 10)
                                  go (q - 1) (x `quot` 10)
      go (end - 1) m
      unsafeWrite (out_pos o) 0 end
    count :: Int -> Int -> Int
    count k x = if x < 10 then k else count (k + 1) (x `quot` 10)
