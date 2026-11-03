{-# LANGUAGE CPP #-}
module Main_dumpbxref(main) where

import System.Environment(getArgs)
import System.Exit(exitWith, ExitCode(..))
import System.IO
import qualified Data.ByteString as BS

import Error(initErrorHandle)
import GenBXref
import IExpandMeta(MetaLedger(..), renderSideFile, renderLedger)

-- dumpbxref [--table | --events] <module>.bxref
--
-- Print a module's cross-reference file (GenBXref): the compiler that
-- wrote it, the module and the stage, then the metadata table as the
-- side file renders it (IExpandMeta.renderSideFile, so the text is the
-- side file -metadata-out writes for the same compile) and the ledger
-- as -dmetadata prints it (renderLedger: the sizes line and the
-- events, oldest first; the file carries no final ids to resolve).
-- --table prints the table alone, --events the ledger alone.
main :: IO ()
main = do
    errh <- initErrorHandle
    as <- getArgs
    (part, fname) <- case as of
                       ["--table", fn]  -> return (Table, fn)
                       ["--events", fn] -> return (Events, fn)
                       [fn@(c:_)] | (c /= '-') -> return (Whole, fn)
                       _ -> do hPutStr stderr "Usage: dumpbxref [--table | --events] <module>.bxref\n"
                               exitWith (ExitFailure 1)
    file <- BS.readFile fname
    let bx = readBXrefFile errh fname file
        stage = bxrefStageName (bx_stage bx)
        table = renderSideFile (bx_module bx) (bx_meta bx)
        events = renderLedger stage [] (MetaLedger (bx_meta bx))
    hSetEncoding stdout utf8
    case part of
      Table -> putStr table
      Events -> putStr events
      Whole -> do putStrLn ("version: " ++ bx_version bx)
                  putStrLn ("module: " ++ bx_module bx)
                  putStrLn ("stage: " ++ stage)
                  putStr table
                  putStr events
    exitWith ExitSuccess

data Part = Whole | Table | Events
