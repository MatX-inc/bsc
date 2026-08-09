#!/usr/bin/env runghc
-- Script version of dumpba: runs against the compiled `bsc` cabal library.
-- Invoke via `cabal exec -v0 -- runghc util/bluehs/dumpba.hs ...` (see README.md),
-- or directly as ./dumpba.hs once a GHC package environment is set up.
{-# LANGUAGE CPP #-}
module Main(main) where

import System.Environment(getArgs)

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
        file <- BS.readFile mi
        let (abi, hash) = readABinFile errh mi file
        hSetEncoding stdout utf8
        putStr (ppReadable abi)
        putStrLn ("Hash: " ++ hash)
     _ -> do
        putStr ("Usage: dumpba mod-id\n")
