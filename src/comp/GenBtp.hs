{-# OPTIONS_GHC -Werror -fwarn-incomplete-patterns #-}
module GenBtp(BtpFile(..), genBtpFile, readBtpFile, readBtpFileStrict,
              readBtpStatus) where

-- The .btp file: the typed package that -check-only writes after
-- typecheck.  See bsc.hs (writeCheck).

import qualified Control.Exception as CE
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.IO(IOMode(ReadMode), withBinaryFile)

import FStringCompat(FString)
import Position(noPosition)
import Id(Id)
import Error(ErrMsg(..), ErrorHandle, bsError, Diagnostic(..))
import BinData
import GenBin(CPackageBin(..))
import FileIOUtil(writeFileBuilderCatch)

-- .btp file tag -- change this whenever the .btp format changes
header :: [Byte]
header = B.unpack $ TE.encodeUtf8 $ T.pack "bsc-btp-20261111-1"

headerBS :: B.ByteString
headerBS = B.pack header

data BtpFile = BtpFile
    { btp_name        :: Id
      -- the files the check read: the import closure with each
      -- package's .bi hash (as in a .bi file), or, for a check of a .bup,
      -- the package with the hash of its .bup
    , btp_imports     :: [(Id, String)]
      -- whether the check passed: False when typecheck reported errors
      -- and continued (-continue-after-errors)
    , btp_ok          :: Bool
      -- everything reported, in order
    , btp_diagnostics :: [Diagnostic]
      -- the typechecked package (before simplification and dictionary
      -- lifting), with poisoned definitions where typecheck failed and
      -- continued; imported signatures are references (CPackageBin)
    , btp_package     :: CPackageBin
    }

instance Bin Diagnostic where
    writeBytes (Diagnostic tag sev pos txt) = do toBin tag; toBin sev; toBin pos; toBin txt
    readBytes = do tag <- fromBin; sev <- fromBin; pos <- fromBin; txt <- fromBin
                   return (Diagnostic tag sev pos txt)

instance Bin BtpFile where
    writeBytes (BtpFile n is ok ds pkg) =
        section "BtpFile" $ do toBin n; toBin is; toBin ok; toBin ds; toBin pkg
    readBytes = do n <- fromBin; is <- fromBin; ok <- fromBin; ds <- fromBin
                   pkg <- fromBin
                   return (BtpFile n is ok ds pkg)

-- The tag, a status byte (btp_ok, so that an importer can read it
-- without decoding the rest), then one format 2 BinData container
genBtpFile :: ErrorHandle -> (Bool -> FString -> FString) -> String -> BtpFile -> IO ()
genBtpFile errh remapF fn btp = do
    bs <- encodeV2With remapF btp
    writeFileBuilderCatch errh fn
        (BB.byteString headerBS <> BB.word8 (if btp_ok btp then 1 else 0) <>
         BB.lazyByteString bs)

readBtpFile :: ErrorHandle -> String -> B.ByteString -> IO BtpFile
readBtpFile = readBtpWith False

-- Everything decoded and validated at once (for tools that print it)
readBtpFileStrict :: ErrorHandle -> String -> B.ByteString -> IO BtpFile
readBtpFileStrict = readBtpWith True

readBtpWith :: Bool -> ErrorHandle -> String -> B.ByteString -> IO BtpFile
readBtpWith strict errh nm s =
    let hlen = B.length headerBS
        payload = B.drop (hlen + 1) s
    in if B.take hlen s == headerBS && B.length s > hlen
       then return (if strict then decodeStrict payload else decode payload)
       else bsError errh [(noPosition, EBinFileVerMismatch nm)]

-- btp_ok of a .btp file, from its first bytes only; Nothing when the
-- file cannot be read or is not a .btp of this format
readBtpStatus :: FilePath -> IO (Maybe Bool)
readBtpStatus fn = CE.handle ioErr $ withBinaryFile fn ReadMode $ \h -> do
    let hlen = B.length headerBS
    s <- B.hGet h (hlen + 1)
    return $ if B.length s == hlen + 1 && B.take hlen s == headerBS
             then Just (B.last s /= 0)
             else Nothing
  where ioErr :: CE.IOException -> IO (Maybe Bool)
        ioErr _ = return Nothing
