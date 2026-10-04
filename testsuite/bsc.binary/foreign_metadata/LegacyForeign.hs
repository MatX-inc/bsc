-- Build in the Cabal environment after building bsc-core and bsc-ba:
--   cabal exec -- ghc -package bsc-core -package bsc-ba LegacyForeign.hs -o legacy-foreign
-- Set FOREIGN_METADATA_VERIFY to that executable when running test.sh.
-- This uses the retained legacy writer, rather than relabeling a new file.
module Main (main) where

import ABin (ABin(..), ABinForeignFuncInfo(..))
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import qualified Data.ByteString as B
import Error (initErrorHandle)
import GenABin (genABinFile)
import GenBDPI (BDPI(..), readBDPIFile)
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [source, destination] -> do
            errh <- initErrorHandle
            bytes <- B.readFile source
            info <- evaluate $ force $ readBDPIFile errh source bytes
            let legacy = ABinForeignFunc
                    (ABinForeignFuncInfo (bdpi_src_name info) (bdpi_foreign_func info))
                    (bdpi_version info)
            genABinFile errh id destination legacy
        _ -> die "usage: legacy-foreign INPUT.bdpi OUTPUT.ba"
