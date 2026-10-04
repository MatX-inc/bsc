-- Parser-only companion to native-tcl-boundaries.c; never evaluates Tcl.
module Main (main) where

import BscTestsuite.Tcl
import Control.Exception (evaluate)
import Control.Monad (forM_)
import Data.Array (listArray, (!))
import Data.Char (ord)
import Numeric (showHex)
import System.Environment (getArgs)
import System.IO (IOMode(ReadMode), hGetContents, hSetEncoding, utf8, withFile)

utf8Bytes :: Char -> [Int]
utf8Bytes character
  | n < 0x80 = [n]
  | n < 0x800 = [0xc0 + n `div` 64, 0x80 + n `mod` 64]
  | n < 0x10000 =
      [0xe0 + n `div` 4096, 0x80 + (n `div` 64) `mod` 64, 0x80 + n `mod` 64]
  | otherwise =
      [0xf0 + n `div` 262144, 0x80 + (n `div` 4096) `mod` 64,
       0x80 + (n `div` 64) `mod` 64, 0x80 + n `mod` 64]
  where n = ord character

hex :: String -> String
hex = concatMap (concatMap byteHex . utf8Bytes)
  where
    byteHex n = let digits = showHex n ""
                in replicate (2 - length digits) '0' ++ digits

readUtf8 :: FilePath -> IO String
readUtf8 path = withFile path ReadMode $ \handle -> do
  hSetEncoding handle utf8
  input <- hGetContents handle
  _ <- evaluate (length input)
  pure input

inspectFile :: FilePath -> IO ()
inspectFile path = do
  input <- readUtf8 path
  let offsets = listArray (0, length input)
        (scanl (+) 0 (map (length . utf8Bytes) input))
      byteOffset position = offsets ! sourceOffset position
  putStrLn ("F\t" ++ hex path)
  case parseScript path input of
    Left err -> putStrLn ("E\t0\t" ++ show (byteOffset (errorPosition err)) ++
                         "\t" ++ hex (errorMessage err))
    Right (Script commands) -> forM_ commands $ \command -> do
      putStrLn ("C\t" ++ show (byteOffset (commandPosition command)))
      forM_ (commandWords command) $ \word ->
        putStrLn ("W\t" ++ show (byteOffset (wordPosition word)) ++
                  "\t" ++ hex (wordSource word))
  putStrLn "Z"

main :: IO ()
main = getArgs >>= mapM_ inspectFile
