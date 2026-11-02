{-# LANGUAGE CPP #-}
module Main_dumpbxref(main) where

import System.Environment(getArgs)
import System.Exit(exitWith, ExitCode(..))
import System.IO
import qualified Data.ByteString as BS

import Error(initErrorHandle)
import GenBXref
import IExpandMeta(MetaLedger(..), IdentRecord(..), renderSideFile, renderLedger,
                   renderIdents, renderIdentSet)

-- dumpbxref [--table | --events | --idents | --resolve <identifier>] <module>.bxref
--
-- Print a module's cross-reference file (GenBXref): the compiler that
-- wrote it, the module and the stage, then the metadata table as the
-- side file renders it (IExpandMeta.renderSideFile, so the text is the
-- side file -metadata-out writes for the same compile), the ledger as
-- -dmetadata prints it (renderLedger: the sizes line and the events,
-- oldest first) and the identifier records of the Verilog backend's
-- join, one line per identifier with what it stands for and a summary
-- (renderIdents).  --table prints the table alone, --events the ledger
-- alone, --idents the identifier lines alone, and --resolve one
-- identifier's record set: its line, each record it stands for in the
-- positional and the position-free rendering, and the events on the
-- way (renderIdentSet).  A file with another tag, or written by
-- another compiler build, is rejected through GenBXref.readBXrefFile
-- as a .ba is.
main :: IO ()
main = do
    errh <- initErrorHandle
    as <- getArgs
    (part, fname) <- case as of
                       ["--table", fn]  -> return (Table, fn)
                       ["--events", fn] -> return (Events, fn)
                       ["--idents", fn] -> return (Idents, fn)
                       ["--resolve", ident, fn] -> return (Resolve ident, fn)
                       [fn@(c:_)] | (c /= '-') -> return (Whole, fn)
                       _ -> usage
    file <- BS.readFile fname
    let bx = readBXrefFile errh fname file
        stage = bxrefStageName (bx_stage bx)
        table = renderSideFile (bx_module bx) (bx_meta bx)
        events = renderLedger stage [] (MetaLedger (bx_meta bx))
        idents = renderIdents (bx_meta bx) (bx_idents bx)
    hSetEncoding stdout utf8
    case part of
      Table -> putStr table
      Events -> putStr events
      Idents -> putStr idents
      Resolve ident ->
          case [ r | r <- bx_idents bx, ir_ident r == ident || ir_id r == ident ] of
            [] -> do hPutStr stderr ("dumpbxref: no identifier " ++ ident ++ " in " ++ fname ++ "\n")
                     exitWith (ExitFailure 1)
            rs -> mapM_ (putStr . renderIdentSet (bx_module bx) (bx_meta bx)) rs
      Whole -> do putStrLn ("version: " ++ bx_version bx)
                  putStrLn ("module: " ++ bx_module bx)
                  putStrLn ("stage: " ++ stage)
                  putStr table
                  putStr events
                  putStr idents
    exitWith ExitSuccess
  where
    usage = do hPutStr stderr ("Usage: dumpbxref [--table | --events | --idents" ++
                               " | --resolve <identifier>] <module>.bxref\n")
               exitWith (ExitFailure 1)

data Part = Whole | Table | Events | Idents | Resolve String
