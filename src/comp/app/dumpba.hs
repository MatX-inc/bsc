{-# LANGUAGE CPP #-}
module Main_dumpba(main) where

import System.Environment(getArgs)

import GenABin
import ABin
import Capture(Capture, captureSummary, captureDump)
import PPrint
import Error(initErrorHandle)
import System.IO
import qualified Data.ByteString as BS

main :: IO ()
main = do
    errh <- initErrorHandle
    as <- getArgs
    case as of
     [mi] -> do
        file <- BS.readFile mi
        let (abi, hash) = readABinFile errh mi file
        hSetEncoding stdout utf8
        putStr (ppReadable abi)
        mapM_ (\c -> captureSummary c >>= mapM_ putStrLn) (abCapture abi)
        putStrLn ("Hash: " ++ hash)
     ["--capture", mi] -> do
        file <- BS.readFile mi
        let (abi, _) = readABinFile errh mi file
        hSetEncoding stdout utf8
        mapM_ (\c -> do captureSummary c >>= mapM_ putStrLn
                        mapM_ putStrLn (captureDump c)) (abCapture abi)
     _ -> do
        putStr ("Usage: dumpba [--capture] mod-id\n")

abCapture :: ABin -> [Capture]
abCapture (ABinMod { ab_modinfo = mi }) = [abmi_capture mi]
abCapture (ABinModSchedErr { ab_modschederr_info = mi }) = [abmsei_capture mi]
abCapture _ = []
