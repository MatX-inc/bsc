{-# LANGUAGE CPP #-}
module Main_dumpbo(main) where

import System.Environment(getArgs)
import System.Exit(exitWith, ExitCode(..))
import Data.List(isSuffixOf)

import PPrint
import Id(getIdString)
import GenBin
import GenBi(BiFile(..), readBiFileStrict)
import GenChk(ChkFile(..), readChkFileStrict)
import ISyntax
import Error(initErrorHandle, Diagnostic(..))
import System.IO
import qualified Data.ByteString as BS

main :: IO ()
main = do
    errh <- initErrorHandle
    as <- getArgs
    (isBI, fname) <- case as of
                       ["-bi", mi]             -> return (True, mi)
                       [mi@(c:_)] | (c /= '-') -> return (False, mi)
                       _ -> do putStr ("Usage: dumpbo [-bi] file.bo|file.bi|file.chk\n")
                               exitWith (ExitFailure 1)
    file <- BS.readFile fname
    hSetEncoding stdout utf8
    if ".bi" `isSuffixOf` fname
      then do (BiFile name imps sig, hash) <- readBiFileStrict errh fname file
              if (isBI)
                 then putStr (ppReadable sig)
                 else do putStrLn ("Package: " ++ getIdString name)
                         putStrLn ("Imports: " ++ unwords [ getIdString i | (i, _) <- imps ])
                         putStr (ppReadable sig)
                         putStrLn ("Hash: " ++ hash)
      else if ".chk" `isSuffixOf` fname then do
        ChkFile name imps ok diags (CPackageBin pkg) <- readChkFileStrict errh fname file
        putStrLn ("Package: " ++ getIdString name)
        putStrLn ("Imports: " ++ unwords [ getIdString i | (i, _) <- imps ])
        putStrLn ("Status: " ++ (if ok then "ok" else "failed"))
        mapM_ (putStr . diag_text) diags
        putStr (ppReadable pkg)
      else do
        -- everything is printed, so decode and validate the whole file at once
        (bi_sig, bo_sig, ipkg, hash) <- readBinFileStrict errh fname file
        if (isBI)
           then do putStr (ppReadable bi_sig)
           else do putStrLn ("Internal Symbols (export): ")
                   putStr (ppReadable bi_sig)
                   putStrLn ("Internal Symbols (all): ")
                   putStr (ppReadable bo_sig)
                   putStr (ppReadable (ipkg :: IPackage ()))
                   putStrLn ("Hash: " ++ hash)
    exitWith ExitSuccess
