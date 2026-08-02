#!/usr/bin/env runghc
-- Script version of dumpbo: runs against the compiled `bsc` cabal library.
-- Invoke via `cabal exec -v0 -- runghc util/bluehs/dumpbo.hs ...` (see README.md),
-- or directly as ./dumpbo.hs once a GHC package environment is set up.
{-# LANGUAGE CPP #-}
module Main(main) where

import System.Environment(getArgs)
import System.Exit(exitWith, ExitCode(..))

import PPrint
import GenBin
import ISyntax
import Error(initErrorHandle)
import System.IO
import qualified Data.ByteString as BS

main :: IO ()
main = do
    errh <- initErrorHandle
    as <- getArgs
    (isBI, fname) <- case as of
                       ["-bi", mi]             -> return (True, mi)
                       [mi@(c:_)] | (c /= '-') -> return (False, mi)
                       _ -> do putStr ("Usage: dumpbo [-bi] mod-id\n")
                               exitWith (ExitFailure 1)
    file <- BS.readFile fname
    (bi_sig, bo_sig, ipkg, hash) <- readBinFile errh fname file
    hSetEncoding stdout utf8
    if (isBI)
       then do putStr (ppReadable bi_sig)
       else do putStrLn ("Internal Symbols (export): ")
               putStr (ppReadable bi_sig)
               putStrLn ("Internal Symbols (all): ")
               putStr (ppReadable bo_sig)
               putStr (ppReadable (ipkg :: IPackage ()))
               putStrLn ("Hash: " ++ hash)
    exitWith ExitSuccess
