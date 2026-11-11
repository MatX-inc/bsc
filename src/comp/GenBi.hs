{-# OPTIONS_GHC -Werror -fwarn-incomplete-patterns #-}
module GenBi(BiFile(..), genBiFile, readBiFile, readBiFileStrict) where

-- The .bi file: a package's user-visible signature, as -signature-only
-- writes it after context reduction, with the import closure it was
-- computed against.  See bsc.hs (writeSignatureOnly) and
-- BinUtil.readImportsBi.

import Control.Exception(evaluate)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import FStringCompat(FString)
import Position(noPosition)
import Id(Id)
import CSyntax(CSignature)
import Error(ErrMsg(..), ErrorHandle, bsError)
import BinData
import GenBin()
import FileIOUtil(writeFileBuilderCatch)

-- .bi file tag -- change this whenever the .bi format changes
header :: [Byte]
header = B.unpack $ TE.encodeUtf8 $ T.pack "bsc-bi-20261108-1"

headerBS :: B.ByteString
headerBS = B.pack header

data BiFile = BiFile
    { bi_name    :: Id
      -- every package in the import closure (the packages whose
      -- signatures were loaded), with the hash of its .bi file, in the
      -- order of the importer's imported signatures
    , bi_imports :: [(Id, String)]
    , bi_sig     :: CSignature
    }

instance Bin BiFile where
    writeBytes (BiFile n is sig) = section "BiFile" $ do toBin n; toBin is; toBin sig
    readBytes = do n <- fromBin; is <- fromBin; sig <- fromBin
                   return (BiFile n is sig)

-- The tag followed by one format 2 BinData container
genBiFile :: ErrorHandle -> (Bool -> FString -> FString) -> String -> BiFile -> IO ()
genBiFile errh remapF fn bi = do
    bs <- encodeV2With remapF bi
    writeFileBuilderCatch errh fn (BB.byteString headerBS <> BB.lazyByteString bs)

-- The file and its hash.  The hash covers every byte after the tag:
-- the package name, the import closure with the imports' hashes, and
-- the signature.  So it changes when the package's interface changes or
-- when any interface in its closure does, and an importer that finds
-- two different hashes for one package (BinUtil.mergeHashes) has loaded
-- interfaces from inconsistent builds.
readBiFile :: ErrorHandle -> String -> B.ByteString -> IO (BiFile, String)
readBiFile = readBiWith False

-- Everything decoded and validated at once (for tools that print it)
readBiFileStrict :: ErrorHandle -> String -> B.ByteString -> IO (BiFile, String)
readBiFileStrict = readBiWith True

readBiWith :: Bool -> ErrorHandle -> String -> B.ByteString -> IO (BiFile, String)
readBiWith strict errh nm s =
    let hlen = B.length headerBS
    in if B.take hlen s == headerBS
       then do let payload = B.drop hlen s
                   bi = if strict then decodeStrict payload else decode payload
                   -- only the hash of the pair is used
                   (_, hash) = decodeWithHash payload :: ((), String)
               _ <- evaluate hash
               return (bi, hash)
       else bsError errh [(noPosition, EBinFileVerMismatch nm)]
