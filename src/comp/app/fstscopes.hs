{-# LANGUAGE ForeignFunctionInterface #-}
module Main_fstscopes(main) where

-- Dump an FST waveform file's hierarchy -- scopes with their
-- component (module type) names, and variables -- as stable text.
-- This is used by the testsuite to check the hierarchy that Bluesim's
-- FST dumper records; fst2vcd cannot be used for that, because its
-- reader path discards the scope component field.
--
-- The FST reading is done by libfst (the src/vendor/libfst
-- submodule), which is compiled into this executable;
-- fstscopes_hier.c provides flat accessors for libfst's fstHier
-- record, which contains a union that the FFI cannot express.

import Control.Monad(when)
import qualified Data.Map as M
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import System.Environment(getArgs, getProgName)
import System.Exit
import System.IO

data FstReader
data FstHier

foreign import ccall unsafe "fstReaderOpen"
    fstReaderOpen :: CString -> IO (Ptr FstReader)
foreign import ccall unsafe "fstReaderClose"
    fstReaderClose :: Ptr FstReader -> IO ()
foreign import ccall unsafe "fstReaderIterateHier"
    fstReaderIterateHier :: Ptr FstReader -> IO (Ptr FstHier)

-- flat accessors from fstscopes_hier.c
foreign import ccall unsafe "bsc_fsthier_kind"
    hierKind :: Ptr FstHier -> IO CInt
foreign import ccall unsafe "bsc_fsthier_scope_name"
    hierScopeName :: Ptr FstHier -> IO CString
foreign import ccall unsafe "bsc_fsthier_scope_component"
    hierScopeComponent :: Ptr FstHier -> IO CString
foreign import ccall unsafe "bsc_fsthier_var_name"
    hierVarName :: Ptr FstHier -> IO CString
foreign import ccall unsafe "bsc_fsthier_var_length"
    hierVarLength :: Ptr FstHier -> IO CUInt
foreign import ccall unsafe "bsc_fsthier_var_is_alias"
    hierVarIsAlias :: Ptr FstHier -> IO CInt
foreign import ccall unsafe "bsc_fsthier_var_type_name"
    hierVarTypeName :: Ptr FstHier -> IO CString
foreign import ccall unsafe "bsc_fsthier_var_direction_name"
    hierVarDirectionName :: Ptr FstHier -> IO CString
foreign import ccall unsafe "bsc_fsthier_attr_name"
    hierAttrName :: Ptr FstHier -> IO CString
foreign import ccall unsafe "bsc_fsthier_attr_arg"
    hierAttrArg :: Ptr FstHier -> IO CULLong
foreign import ccall unsafe "bsc_fsthier_attr_path"
    hierAttrPath :: Ptr FstHier -> IO CULLong

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fname] -> dumpHierarchy fname
    _       -> do prog <- getProgName
                  hPutStrLn stderr ("usage: " ++ prog ++ " <file.fst>")
                  exitWith (ExitFailure 2)

dumpHierarchy :: String -> IO ()
dumpHierarchy fname = do
  ctx <- withCString fname fstReaderOpen
  when (ctx == nullPtr) $ do
    hPutStrLn stderr ("cannot open '" ++ fname ++ "'")
    exitWith (ExitFailure 1)
  printEntries ctx
  fstReaderClose ctx

-- Each scope prints as
--   scope <name> <component|-> [defined <file>:<line>] [instantiated <file>:<line>]
-- and each variable as
--   var <width> <name> [(alias)] <reg|wire|...> [<direction>] [: <type>]
-- where the type is the name recorded by the writer, when there is one.
-- A variable's type and a scope's source stems arrive as attribute
-- entries just ahead of the entry they describe.
printEntries :: Ptr FstReader -> IO ()
printEntries ctx = loop M.empty Nothing Nothing Nothing
  where
    loop paths pending_type pending_src pending_inst = do
      h <- fstReaderIterateHier ctx
      when (h /= nullPtr) $ do
        kind <- hierKind h
        let stem label (Just (path, line)) =
                " " ++ label ++ " " ++ M.findWithDefault "?" path paths ++ ":" ++ show line
            stem _ Nothing = ""
            continue = loop paths Nothing Nothing Nothing
        case kind of
          0 -> do name <- hierScopeName h >>= peekCString
                  compPtr <- hierScopeComponent h
                  comp <- if compPtr == nullPtr
                          then return "-"
                          else peekCString compPtr
                  putStrLn ("scope " ++ name ++ " " ++ comp ++
                            stem "defined" pending_src ++
                            stem "instantiated" pending_inst)
                  continue
          1 -> do putStrLn "upscope"
                  continue
          2 -> do name <- hierVarName h >>= peekCString
                  len <- hierVarLength h
                  alias <- hierVarIsAlias h
                  vt <- hierVarTypeName h >>= peekCString
                  dirPtr <- hierVarDirectionName h
                  dir <- if dirPtr == nullPtr
                         then return ""
                         else fmap (' ' :) (peekCString dirPtr)
                  putStrLn ("var " ++ show len ++ " " ++ name ++
                            (if alias /= 0 then " (alias)" else "") ++
                            " " ++ vt ++ dir ++
                            maybe "" (" : " ++) pending_type)
                  continue
          4 -> do t <- hierAttrName h >>= peekCString
                  loop paths (Just t) pending_src pending_inst
          5 -> do p <- hierAttrName h >>= peekCString
                  n <- hierAttrArg h
                  loop (M.insert (fromIntegral n) p paths) pending_type pending_src pending_inst
          6 -> do st <- stemOf h
                  loop paths pending_type (Just st) pending_inst
          7 -> do st <- stemOf h
                  loop paths pending_type pending_src (Just st)
          _ -> loop paths pending_type pending_src pending_inst
    stemOf :: Ptr FstHier -> IO (Integer, Integer)
    stemOf h = do
      path <- hierAttrPath h
      line <- hierAttrArg h
      return (fromIntegral path, fromIntegral line)
