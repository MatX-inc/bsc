{-# OPTIONS_GHC -Werror -fwarn-incomplete-patterns #-}
-- | Foreign-function metadata shared by Bluesim, Verilog DPI and Verilog VPI.
-- This format contains no synthesized-module or scheduling representation.
module GenBDPI
    ( BDPI(..)
    , genBDPIFile
    , readBDPIFile
    , readBDPIFileMaybe
    ) where

import Warmup ()
import BinData
import Error (ErrorHandle, ErrMsg(..), bsErrorUnsafe, internalError)
import Eval (NFData(..))
import FileIOUtil (writeBinaryFileCatch)
import ForeignFunctions (ForeignFunction(..), ForeignType(..))
import Id (Id)
import Position (Position, noPosition)
import Version (bscVersionStr)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

data BDPI = BDPI
    { bdpi_src_name :: Id
    , bdpi_foreign_func :: ForeignFunction
    , bdpi_version :: String
    } deriving (Eq, Show)

instance NFData BDPI where
    rnf (BDPI srcName foreignFunction version) =
        rnf srcName `seq` rnf foreignFunction `seq` rnf version

-- Change this tag whenever the foreign metadata encoding changes.
header :: B.ByteString
header = BC.pack "bsc-bdpi-20261003-1"

genBDPIFile :: ErrorHandle -> (Position -> Position) -> FilePath -> BDPI -> IO ()
genBDPIFile errh remapP filename info =
    writeBinaryFileCatch errh filename
        (B.unpack header ++ encodeWith remapP info)

readBDPIFile :: ErrorHandle -> FilePath -> B.ByteString -> BDPI
readBDPIFile errh filename bytes =
    case readBDPIFileMaybe bytes of
        Just info -> info
        Nothing -> bsErrorUnsafe errh [(noPosition, EBinFileVerMismatch filename)]

-- Like the legacy binary reader, malformed payloads raise decoding errors.
-- The IO loader must force the result inside its file-error handler; decode
-- checks both truncated payloads and unused trailing bytes.
readBDPIFileMaybe :: B.ByteString -> Maybe BDPI
readBDPIFileMaybe bytes
    | B.take (B.length header) bytes /= header = Nothing
    | otherwise =
        let info = decode (B.drop (B.length header) bytes)
        in if bdpi_version info == bscVersionStr True
           then Just info
           else Nothing

instance Bin BDPI where
    writeBytes info = section "BDPI" $ do
        toBin (bdpi_version info)
        toBin (bdpi_src_name info)
        toBin (bdpi_foreign_func info)
    readBytes = do
        version <- fromBin
        srcName <- fromBin
        foreignFunction <- fromBin
        return (BDPI srcName foreignFunction version)

-- Shared with the legacy .ba codec. These instances were moved without
-- changing their encoding so legacy foreign and module artifacts still read.
instance Bin ForeignType where
    writeBytes Void        = do putI 0
    writeBytes (Narrow n)  = do putI 1; toBin n
    writeBytes (Wide n)    = do putI 2; toBin n
    writeBytes Polymorphic = do putI 3
    writeBytes StringPtr   = do putI 4
    readBytes = do i <- getI
                   case i of
                     0 -> return Void
                     1 -> do n <- fromBin; return (Narrow n)
                     2 -> do n <- fromBin; return (Wide n)
                     3 -> return Polymorphic
                     4 -> return StringPtr
                     n -> internalError $ "GenABin.Bin(ForeignType).readBytes: " ++ show n

instance Bin ForeignFunction where
    writeBytes (FF name rt args) = do toBin name; toBin rt; toBin args
    readBytes = do name <- fromBin
                   rt   <- fromBin
                   args <- fromBin
                   return (FF name rt args)
