{-# LANGUAGE CPP #-}
module Main_dumpbo(main) where

import System.Environment(getArgs)
import System.Exit(exitWith, ExitCode(..))

import Control.Monad(forM_, unless)
import PPrint
import GenBin
import ISyntax
import Prim(PrimOp, primOpCode, primOpFromCode, allPrimOps)
import Error(initErrorHandle)
import System.IO
import qualified Data.ByteString as BS

main :: IO ()
main = do
    errh <- initErrorHandle
    as <- getArgs
    (isBI, fname) <- case as of
                       ["-bi", mi]             -> return (True, mi)
                       ["-prim-codes"]         -> dumpPrimCodes
                       [mi@(c:_)] | (c /= '-') -> return (False, mi)
                       _ -> do putStr ("Usage: dumpbo [-bi] mod-id\n" ++
                                       "       dumpbo -prim-codes\n")
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
               putStr (ppReadable (ipkg :: IPackage PreElab))
               putStrLn ("Hash: " ++ hash)
    exitWith ExitSuccess

-- The code of every primitive, as the .bo writes it (one line per
-- primitive, in code order), after checking that each code reads back
-- as the primitive it was written from.  The testsuite compares the
-- listing with its expected copy, so a change to the encoding -- a
-- reordered table, a shifted code -- fails there.
dumpPrimCodes :: IO a
dumpPrimCodes = do
    hSetEncoding stdout utf8
    forM_ allPrimOps $ \ p -> do
        let n = primOpCode p
            p' = primOpFromCode n :: PrimOp PreElab
        unless (p' == p && show p' == show p) $ do
            hPutStrLn stderr ("dumpbo -prim-codes: code " ++ show n ++
                              " of " ++ show p ++ " reads back as " ++ show p')
            exitWith (ExitFailure 1)
        putStrLn (show n ++ "\t" ++ show p)
    exitWith ExitSuccess
