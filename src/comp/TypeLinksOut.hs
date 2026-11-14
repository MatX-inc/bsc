-- Type links (prototype): the writer of the .tl file (TypeLinks).
--
-- A record is a few short ASCII pieces and decimal numbers, and a module
-- writes millions of them.  They are poked straight into one buffer that
-- is handed to the file when it is full, so that a record costs no
-- allocation beyond the closure that describes it.  The names are those
-- of Data.ByteString.Builder, whose bytes these functions reproduce.
module TypeLinksOut(
        Out, newOut, closeOut, write,
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

-- The file, the buffer and the number of bytes waiting in it.  out_ptr is
-- the address of out_buf's memory, which every write keeps alive (write).
data Out = Out { out_h   :: Handle,
                 out_buf :: !(ForeignPtr Word8),
                 out_ptr :: !(Ptr Word8),
                 out_pos :: !(IOUArray Int Int) }

-- the buffer's size
outSize :: Int
outSize = 1048576

newOut :: Handle -> IO Out
newOut h = do
  buf <- mallocForeignPtrBytes outSize
  pos <- newArray (0, 0) 0
  return (Out { out_h = h, out_buf = buf, out_ptr = unsafeForeignPtrToPtr buf,
                out_pos = pos })

-- hand the waiting bytes to the file
flush :: Out -> IO ()
flush o = do
  n <- unsafeRead (out_pos o) 0
  -- the buffer is empty again whether or not the write succeeds
  unsafeWrite (out_pos o) 0 0
  when (n > 0) $ hPutBuf (out_h o) (out_ptr o) n
  touchForeignPtr (out_buf o)

-- write what is waiting and close the file
closeOut :: Out -> IO ()
closeOut o = do
  flush o
  hClose (out_h o)

-- The offset at which k more bytes fit (k is far below the buffer's
-- size): the bytes waiting are handed to the file first when they do not.
{-# INLINE room #-}
room :: Out -> Int -> IO Int
room o k = do
  p <- unsafeRead (out_pos o) 0
  if p + k <= outSize then return p
    else do flush o
            return 0

{-# INLINE byteAt #-}
byteAt :: Out -> Int -> Int -> IO ()
byteAt o p b = pokeByteOff (out_ptr o) p (fromIntegral b :: Word8)

-- A piece of a record: an action that appends its bytes.
newtype Builder = Builder (Out -> IO ())

instance Semigroup Builder where
  {-# INLINE (<>) #-}
  Builder a <> Builder b = Builder (\ o -> a o >> b o)

instance Monoid Builder where
  {-# INLINE mempty #-}
  mempty = Builder (\ _ -> return ())

-- append a record's pieces
{-# INLINE write #-}
write :: Out -> Builder -> IO ()
write o (Builder f) = do
  f o
  touchForeignPtr (out_buf o)

-- a character's low seven bits
{-# INLINE char7 #-}
char7 :: Char -> Builder
char7 c = Builder (\ o -> putByte o (ord c .&. 0x7f))

{-# INLINE putByte #-}
putByte :: Out -> Int -> IO ()
putByte o b = do
  p <- room o 1
  byteAt o p b
  unsafeWrite (out_pos o) 0 (p + 1)

-- each character's low seven bits
{-# INLINE string7 #-}
string7 :: String -> Builder
string7 s = Builder (\ o -> putString7 o s)

putString7 :: Out -> String -> IO ()
putString7 _ [] = return ()
putString7 o (c : cs) = do putByte o (ord c .&. 0x7f)
                           putString7 o cs

-- each character in UTF-8
{-# INLINE stringUtf8 #-}
stringUtf8 :: String -> Builder
stringUtf8 s = Builder (\ o -> putStringUtf8 o s)

putStringUtf8 :: Out -> String -> IO ()
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

-- a number in decimal
{-# INLINE intDec #-}
intDec :: Int -> Builder
intDec n = Builder (\ o -> putIntDec o n)

putIntDec :: Out -> Int -> IO ()
putIntDec o n
  | n >= 0 = do p <- room o 20
                digits p n
  -- the least Int has no negation
  | n == minBound = putString7 o (show n)
  | otherwise = do p <- room o 21
                   byteAt o p (ord '-')
                   digits (p + 1) (negate n)
  where
    -- the digits of a number that is not negative, from the offset p
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
