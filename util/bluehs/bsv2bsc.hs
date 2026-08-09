#!/usr/bin/env runghc
-- Script version of bsv2bsc: runs against the compiled `bsc` cabal library.
-- Invoke via `cabal exec -v0 -- runghc util/bluehs/bsv2bsc.hs ...` (see README.md),
-- or directly as ./bsv2bsc.hs once a GHC package environment is set up.
module Main(main) where

import System.Environment

import Parser.BSV(bsvParseString)
import PPrint
import FlagsDecode(defaultFlags)
import Error(initErrorHandle)

main :: IO ()
main =
    do args <- getArgs
       case args of
         [] -> getContents >>= bsv2bsc "-"
         [fn] -> readFile fn >>= bsv2bsc fn
         _ -> error "usage: bsv2bsc filename"

bsv2bsc :: String -> String -> IO ()
bsv2bsc filename text =
    do errh <- initErrorHandle
       (pkg, _, _) <- bsvParseString errh (defaultFlags "") filename (stripExt filename) text
       putStrLn (ppReadable pkg)

stripExt :: String -> String
stripExt filename =
    case reverse filename of
    ('v':'s':'b':'.':revBase) -> reverse revBase
    _ -> filename
