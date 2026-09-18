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

-- Each variable prints as
--   var <width> <name> [(alias)] <reg|wire|...> [<direction>] [: <type>]
-- where the type is the name recorded by the writer, when there is one
-- (it arrives as an attribute entry just ahead of its variable).
printEntries :: Ptr FstReader -> IO ()
printEntries ctx = loop Nothing
  where
    loop pending_type = do
      h <- fstReaderIterateHier ctx
      when (h /= nullPtr) $ do
        kind <- hierKind h
        case kind of
          0 -> do name <- hierScopeName h >>= peekCString
                  compPtr <- hierScopeComponent h
                  comp <- if compPtr == nullPtr
                          then return "-"
                          else peekCString compPtr
                  putStrLn ("scope " ++ name ++ " " ++ comp)
                  loop Nothing
          1 -> do putStrLn "upscope"
                  loop Nothing
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
                  loop Nothing
          4 -> do t <- hierAttrName h >>= peekCString
                  loop (Just t)
          _ -> loop pending_type
