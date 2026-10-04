{-# LANGUAGE CPP #-}
module Main_dumpba(main) where

import Warmup ()
import System.Environment(getArgs)

import GenModule(readModulePair)
import ABinUtil(readAndCheckABin)
import FlagsDecode(defaultFlags)
import qualified PhaseConfig as PC
import FileNameUtil(hasDotSuf, bmodSuffix, bschedSuffix, bdpiSuffix)
import GenABin
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
        (abi, hash) <-
            if hasDotSuf bmodSuffix mi || hasDotSuf bschedSuffix mi
            then do -- Module pairs contain no saved flags; use tool defaults.
                    (_, result) <- readModulePair errh
                                       (PC.materializeConfig (defaultFlags "")) mi
                    return (result, "")
            else if hasDotSuf bdpiSuffix mi
            then do (_, result) <- readAndCheckABin errh (defaultFlags "") Nothing mi
                    return (result, "")
            else do file <- BS.readFile mi
                    return (readABinFile errh mi file)
        hSetEncoding stdout utf8
        putStr (ppReadable abi)
        putStrLn ("Hash: " ++ hash)
     _ -> do
        putStr ("Usage: dumpba module.bmod|module.bsched|foreign.bdpi|legacy.ba\n")
