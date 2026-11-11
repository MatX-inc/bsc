{-# LANGUAGE CPP #-}
module Main_dumpbo(main) where

import System.Environment(getArgs)
import System.Exit(exitWith, ExitCode(..))
import Data.List(isSuffixOf)
import qualified Data.Map as M
import qualified Data.Set as S

import PPrint
import Id(getIdString, getIdPosition)
import Position(Position(..), prPosition)
import CSyntax(CPackage(..), CImportedSignature(..), CSignature(..))
import BinData(encodeV2, encodeV2Strip)
import Util(hashInit, nextHashByte, showHash)
import GenBin
import GenBi(BiFile(..), readBiFileStrict)
import GenChk(ChkFile(..), readChkFileStrict)
import GenTci(TciFile(..), readTciFile)
import ISyntax
import Error(initErrorHandle, Diagnostic(..))
import System.IO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
    errh <- initErrorHandle
    as <- getArgs
    (isBI, isCanon, fname) <-
        case as of
          ["-bi", mi]             -> return (True, False, mi)
          ["-canon", mi]          -> return (False, True, mi)
          [mi@(c:_)] | (c /= '-') -> return (False, False, mi)
          _ -> do putStr ("Usage: dumpbo [-bi | -canon] file.bo|file.bi|file.chk|file.tci\n")
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
        if isCanon
          then do
            -- hashes of the package as encoded, of the whole file without
            -- its imports record, and of the package with the positions
            -- outside the package's own file reduced to their file (as a
            -- .tci stores imported positions)
            let hashOf bs = showHash (BL.foldl' nextHashByte hashInit bs)
                own = pos_file (getIdPosition name)
                sel p = not (pos_is_stdlib p) && pos_line p >= 0 && pos_file p /= own
            whole <- encodeV2 (ChkFile name [] ok diags (CPackageBin pkg))
            pk <- encodeV2 (CPackageBin pkg)
            (pks, _) <- encodeV2Strip (Just sel) (CPackageBin pkg)
            putStrLn ("Package encoding: " ++ hashOf pk)
            putStrLn ("Encoding without imports: " ++ hashOf whole)
            putStrLn ("Package encoding, imported positions stripped: " ++ hashOf pks)
            mapM_ (\d -> putStr (diag_tag d ++ " " ++ diag_severity d ++ " " ++
                                 prPosition (diag_position d) ++ "\n" ++ diag_text d))
                  diags
          else do
            mapM_ (putStr . diag_text) diags
            putStr (ppReadable pkg)
      else if ".tci" `isSuffixOf` fname then do
        (tci, hash) <- readTciFile errh fname file
        let pkg@(CPackage name _ _ sigs _ ds _) = tci_package tci
            showSet s = unwords (map getIdString (S.toList s))
        putStrLn ("Package: " ++ getIdString name)
        putStrLn ("Source: " ++ tci_source tci)
        putStrLn ("Flags: " ++ tci_flags tci)
        putStrLn ("Hash: " ++ hash)
        putStrLn ("Definitions: " ++ show (length ds))
        putStrLn ("Used in types: " ++ showSet (tci_used_types tci))
        putStrLn ("Used in context reduction: " ++ showSet (tci_used_ctx tci))
        putStrLn ("Used in exports: " ++ showSet (tci_used_exports tci))
        putStrLn ("ATF cache entries: " ++ show (M.size (tci_atf tci)))
        putStrLn "Signatures:"
        mapM_ (\(lib, CImpSign _ q (CSignature n _ fs es)) ->
                   putStrLn ("  " ++ getIdString n ++ (if q then " qualified" else "") ++
                             case lib of
                               Just h -> ": library, .bi hash " ++ h
                               Nothing -> ": " ++ show (length es) ++ " entries, " ++
                                          show (length fs) ++ " fixities"))
              (zip (tci_libs tci) sigs)
        -- -bi: the non-library signatures as kept; otherwise the package
        if isBI
          then mapM_ (\(lib, CImpSign _ _ sig) ->
                          maybe (putStr (ppReadable sig)) (const (return ())) lib)
                     (zip (tci_libs tci) sigs)
          else putStr (ppReadable pkg)
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
