{-# LANGUAGE ScopedTypeVariables #-}
-- | @bsc-classic-parse@: bsc's Bluespec Classic (Bluespec Haskell) lexer and
-- parser -- @Lex.hs@ and @Parser/Classic/CParser.hs@, unmodified -- built
-- without the rest of the compiler, as an accept/reject oracle for
-- differential testing of other BH parsers.
--
-- > bsc-classic-parse [--defns | --type] [--quiet] [--bsc-message] [FILE...]
--
-- With no FILEs (or a FILE of @-@) the source is read from stdin.  One line
-- is printed per input, in the style of @bh-parse@:
--
-- > NAME: ok
-- > NAME:LINE:COL: P0005 Unexpected `x'; expected `='
--
-- The default runs 'pPackage' on the whole input, which is exactly what
-- @bsc@ does with a @.bs@ file ('Depend.parseSrc').  @--defns@ instead
-- parses a bare list of top-level definitions ('pDefnsAndEOF'), and
-- @--type@ a single type ('pType', as bluetcl does).  @--bsc-message@ prints
-- bsc's full multi-line error text instead of the one-line summary.  The bsc
-- flags @-no-use-layout@ and @-outlaw-sv-kws-as-classic-ids@ are accepted
-- with their usual meaning (the parser also honours @$BSC_OPTIONS@, as bsc
-- does).
--
-- Exit status: 0 if every input was accepted, 1 if any was rejected, 3 if the
-- parser itself crashed on some input (an internal error, or a stack overflow
-- under bsc's own @-K10m@ limit -- real bsc would die on such an input too,
-- rather than report a syntax error), and 2 for a usage error.
module Main(main) where

import qualified Control.Exception as CE
import Control.Monad(unless, when)
import GHC.IO.Exception(IOErrorType(InvalidArgument))
import System.Environment(getArgs)
import System.Exit(ExitCode(..), exitWith)
import System.IO
import System.IO.Error(ioeGetErrorType)

import Error(EMsg, ErrMsg(ENotUTF8), getErrMsgTag, showErrorList)
import ErrorUtil(internalError)
import FStringCompat(mkFString)
import Lex(LFlags(..), Token, lexStart)
import Parse(Parser, parse, (+..))
import Parser.Classic(errSyntax, pDefnsAndEOF, pPackage)
import Parser.Classic.CParser(eof, pType)
import Position(filePosition, getPositionColumn, getPositionLine)

data Mode = Package | Defns | Type

data Opts = Opts
  { optMode :: Mode
  , optQuiet :: Bool
  , optBscMessage :: Bool
  , optAllowSvKws :: Bool
  , optFiles :: [FilePath]
  }

usage :: IO a
usage = do
  hPutStrLn stderr $ "usage: bsc-classic-parse [--defns | --type] [--quiet] [--bsc-message]"
                  ++ " [-no-use-layout] [-outlaw-sv-kws-as-classic-ids] [FILE...]"
  exitWith (ExitFailure 2)

parseArgs :: [String] -> IO Opts
parseArgs = go (Opts Package False False True [])
  where
    go o [] = return o { optFiles = reverse (optFiles o) }
    go o (a : as) = case a of
      "--defns" -> go o { optMode = Defns } as
      "--type" -> go o { optMode = Type } as
      "--quiet" -> go o { optQuiet = True } as
      "-q" -> go o { optQuiet = True } as
      "--bsc-message" -> go o { optBscMessage = True } as
      -- Read straight from the argument list by CParser (via IOUtil.progArgs).
      "-no-use-layout" -> go o as
      -- Depend.outlaw_sv_kws_as_classic_ids
      "-outlaw-sv-kws-as-classic-ids" -> go o { optAllowSvKws = False } as
      "-h" -> usage
      "--help" -> usage
      "-" -> file
      '-' : _ -> usage
      _ -> file
      where file = go o { optFiles = a : optFiles o } as

data Verdict = Accept | Reject EMsg | Crash String

-- | Run a parser the way bsc's driver does ('Depend.chkParse'), then force
-- the whole result, so that an error raised lazily from a semantic action
-- counts as well.  Every Classic AST type derives 'Show', so 'show' doubles
-- as a deepseq.
run :: Show a => Parser [Token] a -> [Token] -> Either EMsg ()
run p ts = case parse p ts of
  Left (ss, ts') -> Left (errSyntax (filter (not . null) ss) ts')
  Right ((x, _) : _) -> length (show x) `seq` Right ()
  Right [] -> internalError "bsc-classic-parse: parse succeeded with no result"

check :: Mode -> [Token] -> Either EMsg ()
check Package = run pPackage
check Defns = run pDefnsAndEOF
check Type = run (pType +.. eof) -- as bluetcl's TclParseUtils.classicStringWrapper

-- | Read like 'FileIOUtil.readFileCompat': lazily, as UTF-8.  A malformed
-- byte sequence therefore surfaces as an 'InvalidArgument' IOException from
-- inside the parse, which 'judge' turns into ENotUTF8 as 'Depend.parseSrc'
-- does.
readInput :: FilePath -> IO String
readInput "-" = hSetEncoding stdin utf8 >> hGetContents stdin
readInput f = do
  h <- openFile f ReadMode
  hSetEncoding h utf8
  hGetContents h

judge :: Opts -> String -> IO Verdict
judge o name = do
  r <- CE.try $ do
    text <- readInput (if name == "<stdin>" then "-" else name)
    let lflags = LFlags { lf_is_stdlib = False, lf_allow_sv_kws = optAllowSvKws o }
    v <- CE.evaluate (check (optMode o) (lexStart lflags (mkFString name) text))
    -- The message may still hold lazy lexer thunks (a `\x` escape with no
    -- digits makes bsc's lexer call foldl1 on an empty list, for instance),
    -- so force it here, where a crash is still caught and reported as one.
    case v of
      Left e -> CE.evaluate (length (showErrorList [e])) >> return v
      Right () -> return v
  return $ case r of
    Right (Right ()) -> Accept
    Right (Left e) -> Reject e
    Left (ex :: CE.SomeException)
      | Just ioe <- CE.fromException ex, ioeGetErrorType ioe == InvalidArgument
      -> Reject (filePosition (mkFString name), ENotUTF8)
      | otherwise -> Crash (show ex)

report :: Opts -> String -> Verdict -> IO ()
report o name v = case v of
  Accept -> unless (optQuiet o) $ putStrLn (name ++ ": ok")
  Reject e@(p, m)
    | optBscMessage o -> putStr (showErrorList [e])
    | otherwise ->
        putStrLn $ name ++ ":" ++ show (getPositionLine p) ++ ":" ++ show (getPositionColumn p)
                ++ ": " ++ getErrMsgTag m ++ " " ++ oneLine (drop 1 (lines (showErrorList [e])))
  Crash s -> putStrLn (name ++ ": CRASH " ++ oneLine (lines s))
  where oneLine = unwords . concatMap words

main :: IO ()
main = do
  o <- parseArgs =<< getArgs
  mapM_ (`hSetEncoding` utf8) [stdout, stderr]
  hSetBuffering stdout (BlockBuffering Nothing)
  let names = case optFiles o of
        [] -> ["<stdin>"]
        fs -> [if f == "-" then "<stdin>" else f | f <- fs]
  vs <- mapM (\n -> do v <- judge o n; report o n v; return v) names
  let nRej = length [() | Reject _ <- vs]
      nCrash = length [() | Crash _ <- vs]
  when (length names > 1) $
    putStrLn $ show (length names) ++ " inputs, " ++ show nRej ++ " rejected, " ++ show nCrash ++ " crashed"
  hFlush stdout
  exitWith $ case () of
    _ | nCrash > 0 -> ExitFailure 3
      | nRej > 0 -> ExitFailure 1
      | otherwise -> ExitSuccess
