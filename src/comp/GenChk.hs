{-# OPTIONS_GHC -Werror -fwarn-incomplete-patterns #-}
module GenChk(ChkFile(..), genChkFile, readChkFile, readChkFileStrict,
              readChkStatus) where

-- The .chk file: what -check-only writes after typecheck.  See bsc.hs
-- (writeCheck).

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

-- .chk file tag -- change this whenever the .chk format changes
header :: [Byte]
header = B.unpack $ TE.encodeUtf8 $ T.pack "bsc-chk-20261108-3"

headerBS :: B.ByteString
headerBS = B.pack header

data ChkFile = ChkFile
    { chk_name        :: Id
      -- the .bi files the check read: the import closure with each
      -- package's .bi hash (as in a .bi file)
    , chk_imports     :: [(Id, String)]
      -- whether the check passed: False when typecheck reported errors
      -- and continued (-continue-after-errors)
    , chk_ok          :: Bool
      -- everything reported, in order
    , chk_diagnostics :: [Diagnostic]
      -- the typechecked package (before simplification and dictionary
      -- lifting), with poisoned definitions where typecheck failed and
      -- continued; imported signatures are references (CPackageBin)
    , chk_package     :: CPackageBin
    }

instance Bin Diagnostic where
    writeBytes (Diagnostic tag sev pos txt) = do toBin tag; toBin sev; toBin pos; toBin txt
    readBytes = do tag <- fromBin; sev <- fromBin; pos <- fromBin; txt <- fromBin
                   return (Diagnostic tag sev pos txt)

instance Bin ChkFile where
    writeBytes (ChkFile n is ok ds pkg) =
        section "ChkFile" $ do toBin n; toBin is; toBin ok; toBin ds; toBin pkg
    readBytes = do n <- fromBin; is <- fromBin; ok <- fromBin; ds <- fromBin
                   pkg <- fromBin
                   return (ChkFile n is ok ds pkg)

-- The tag, a status byte (chk_ok, so that an importer can read it
-- without decoding the rest), then one format 2 BinData container
genChkFile :: ErrorHandle -> (Bool -> FString -> FString) -> String -> ChkFile -> IO ()
genChkFile errh remapF fn chk = do
    bs <- encodeV2With remapF chk
    writeFileBuilderCatch errh fn
        (BB.byteString headerBS <> BB.word8 (if chk_ok chk then 1 else 0) <>
         BB.lazyByteString bs)

readChkFile :: ErrorHandle -> String -> B.ByteString -> IO ChkFile
readChkFile = readChkWith False

-- Everything decoded and validated at once (for tools that print it)
readChkFileStrict :: ErrorHandle -> String -> B.ByteString -> IO ChkFile
readChkFileStrict = readChkWith True

readChkWith :: Bool -> ErrorHandle -> String -> B.ByteString -> IO ChkFile
readChkWith strict errh nm s =
    let hlen = B.length headerBS
        payload = B.drop (hlen + 1) s
    in if B.take hlen s == headerBS && B.length s > hlen
       then return (if strict then decodeStrict payload else decode payload)
       else bsError errh [(noPosition, EBinFileVerMismatch nm)]

-- chk_ok of a .chk file, from its first bytes only; Nothing when the
-- file cannot be read or is not a .chk of this format
readChkStatus :: FilePath -> IO (Maybe Bool)
readChkStatus fn = CE.handle ioErr $ withBinaryFile fn ReadMode $ \h -> do
    let hlen = B.length headerBS
    s <- B.hGet h (hlen + 1)
    return $ if B.length s == hlen + 1 && B.take hlen s == headerBS
             then Just (B.last s /= 0)
             else Nothing
  where ioErr :: CE.IOException -> IO (Maybe Bool)
        ioErr _ = return Nothing
