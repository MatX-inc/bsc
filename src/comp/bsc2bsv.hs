module Main_bsc2bsv(main) where

import System.Environment
import qualified Control.Exception as CE
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import FStringCompat
import Parse
import Parser.Classic(pPackage, errSyntax)
import PVPrint
import CVPrint()
import Lex
import Error(internalError, showErrorList)

main :: IO ()
main =
    do args <- getArgs
       case args of
         [] -> B.getContents >>= bsc2bsv "-" . decodeUtf8orDie "<stdin>"
         [fn] -> B.readFile fn >>= bsc2bsv fn . decodeUtf8orDie fn
         _ -> error "usage: bsc2bsv filename"

-- source files are UTF-8 (independent of the locale), as in bsc itself
decodeUtf8orDie :: String -> B.ByteString -> T.Text
decodeUtf8orDie name bs =
    case TE.decodeUtf8' bs of
      Right txt -> txt
      Left _ -> error (name ++ ": not a UTF-8 encoded file")

bsc2bsv :: String -> T.Text -> IO ()
bsc2bsv filename text =
    do let lflags = LFlags { lf_is_stdlib = False,
                             lf_allow_sv_kws = True }
           tokens = lexStart lflags (mkFString filename) text
       case parse pPackage tokens of
         Left  (ss, tokens') -> let es = [errSyntax [s | s@(_:_) <- ss] tokens']
                                in  CE.throw $ CE.ErrorCall (showErrorList es)
         Right ((package,_):_) ->
           putStrLn $ pvpReadable package
         Right [] -> internalError "bsc2bsv: parse succeeded with no packages"
