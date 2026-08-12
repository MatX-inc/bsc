{-# LANGUAGE BangPatterns #-}
-- -fno-full-laziness so repeated lexing in the iteration loop is not
-- floated out and computed once.
{-# OPTIONS_GHC -fno-full-laziness #-}
module Main(main) where

import System.Environment(getArgs)
import System.IO
import System.Exit
import Control.Exception(evaluate, try, SomeException)
import Control.Monad(forM_, when)
import Data.Char(ord)
import Data.List(foldl')
import Data.Ratio(numerator, denominator)
import Data.IORef
import GHC.Clock(getMonotonicTimeNSec)
import System.Mem(getAllocationCounter, setAllocationCounter)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE
import Text.Printf(printf)

import Lex
import Position
import FStringCompat(mkFString, FString)
import PreStrings(fsEmpty)
import qualified LexAlexString as AS
import qualified LexAlexSBS as ASB
import qualified LexAlexLBS as ALB
import qualified LexAlexLT as ALT
import qualified LexAlexST as AST
import qualified LexAlexSTF as ASTF
import qualified LexAlexSTFI as ASTFI
import qualified LexAlexSTFW as ASTFW

lflags :: LFlags
lflags = LFlags { lf_is_stdlib = False, lf_allow_sv_kws = True }

-- ---------------------------------------------------------------------------
-- Forcing the token stream: strict left fold computing (count, order-dependent
-- hash).  Cuts at the first L_eof or L_error (the hand lexer's error streams
-- are infinite).  The hash forces positions, FString interning, full Integer/
-- Rational values, and every character of string/char literals.

forceTokens :: [Token] -> (Int, Int)
forceTokens = go 0 0
  where
    go :: Int -> Int -> [Token] -> (Int, Int)
    go !n !h [] = (n, h)
    go !n !h (Token p li : ts) =
      let !hp = pos_line p * 7919 + pos_column p * 31
          !hi = itemHash li
          !h' = h * 31 + hp + hi
          !n' = n + 1
      in  case li of
            L_eof     -> (n', h')
            L_error _ -> (n', h')
            _         -> go n' h' ts

-- forces the FString (i.e. performs the interning) without depending on the
-- intern id, so hashes are comparable across engines and processes
forceFS :: FString -> Int
forceFS fs = case compare fs fsEmpty of { LT -> 10; EQ -> 20; GT -> 30 }

strHash :: String -> Int
strHash = foldl' (\a ch -> a * 33 + ord ch) 5381

itemHash :: LexItem -> Int
itemHash li = case li of
  L_varid fs        -> 101 + forceFS fs
  L_conid fs        -> 202 + forceFS fs
  L_varsym fs       -> 303 + forceFS fs
  L_consym fs       -> 404 + forceFS fs
  L_integer msz b v -> 505 + maybe 17 (fromInteger) msz * 7
                           + fromInteger b * 13
                           + fromInteger (v `mod` 1000000007) * 3
  L_float r         -> 606 + fromInteger (numerator r `mod` 1000003) * 5
                           + fromInteger (denominator r `mod` 1000003)
  L_char c          -> 707 + ord c
  L_string s        -> 808 + strHash s
  L_lpar -> 1; L_rpar -> 2; L_semi -> 3; L_uscore -> 4; L_bquote -> 5
  L_lcurl -> 6; L_rcurl -> 7; L_lbra -> 8; L_rbra -> 9
  L_action -> 10; L_case -> 11; L_class -> 12; L_data -> 13; L_deriving -> 14
  L_do -> 15; L_else -> 16; L_foreign -> 17; L_if -> 18; L_import -> 19
  L_in -> 20; L_infix -> 21; L_infixl -> 22; L_infixr -> 23
  L_interface -> 24; L_instance -> 25; L_let -> 26; L_letseq -> 27
  L_package -> 28; L_of -> 29; L_primitive -> 30; L_qualified -> 31
  L_rules -> 32; L_signature -> 33; L_struct -> 34; L_then -> 35
  L_module -> 36; L_type -> 37; L_valueOf -> 38; L_stringOf -> 39
  L_verilog -> 40; L_synthesize -> 41; L_when -> 42; L_where -> 43
  L_coherent -> 44; L_incoherent -> 45
  L_dcolon -> 46; L_colon -> 47; L_eq -> 48; L_at -> 49; L_lam -> 50
  L_bar -> 51; L_rarrow -> 52; L_larrow -> 53; L_dot -> 54; L_comma -> 55
  L_drarrow -> 56; L_irarrow -> 57
  L_lcurl_o -> 58; L_rcurl_o -> 59; L_semi_o -> 60
  L_lpragma -> 61; L_rpragma -> 62
  L_eof -> 63
  L_error e -> 900 + errHash e

errHash :: LexError -> Int
errHash LexBadCharLit      = 1
errHash LexBadStringLit    = 2
errHash (LexBadLexChar c)  = 3 + ord c
errHash (LexUntermComm p)  = 4 + pos_line p * 7 + pos_column p
errHash LexMissingNL       = 5

-- cut at first eof/error inclusive, for dump/compare
cutTokens :: [Token] -> [Token]
cutTokens [] = []
cutTokens (t@(Token _ li) : ts) = case li of
  L_eof     -> [t]
  L_error _ -> [t]
  _         -> t : cutTokens ts

-- ---------------------------------------------------------------------------
-- Engines.  NOINLINE so the loop body stays an out-of-line call.

{-# NOINLINE runHand #-}
runHand :: String -> String -> (Int, Int)
runHand fname str = forceTokens (lexStart lflags (mkFString fname) str)

{-# NOINLINE runAlexString #-}
runAlexString :: String -> String -> (Int, Int)
runAlexString fname str = forceTokens (AS.lexAlexStart lflags (mkFString fname) str)

{-# NOINLINE runAlexSBS #-}
runAlexSBS :: String -> SB.ByteString -> (Int, Int)
runAlexSBS fname bs = forceTokens (ASB.lexAlexStart lflags (mkFString fname) bs)

{-# NOINLINE runAlexLBS #-}
runAlexLBS :: String -> LB.ByteString -> (Int, Int)
runAlexLBS fname bs = forceTokens (ALB.lexAlexStart lflags (mkFString fname) bs)

{-# NOINLINE runAlexLT #-}
runAlexLT :: String -> TL.Text -> (Int, Int)
runAlexLT fname t = forceTokens (ALT.lexAlexStart lflags (mkFString fname) t)

{-# NOINLINE runAlexST #-}
runAlexST :: String -> T.Text -> (Int, Int)
runAlexST fname t = forceTokens (AST.lexAlexStart lflags (mkFString fname) t)

{-# NOINLINE runAlexSTF #-}
runAlexSTF :: String -> T.Text -> (Int, Int)
runAlexSTF fname t = forceTokens (ASTF.lexAlexStart lflags (mkFString fname) t)

{-# NOINLINE runAlexSTFI #-}
runAlexSTFI :: String -> T.Text -> (Int, Int)
runAlexSTFI fname t = forceTokens (ASTFI.lexAlexStart lflags (mkFString fname) t)

{-# NOINLINE runAlexSTFW #-}
runAlexSTFW :: String -> T.Text -> (Int, Int)
runAlexSTFW fname t = forceTokens (ASTFW.lexAlexStart lflags (mkFString fname) t)

-- ---------------------------------------------------------------------------

decodeOrDie :: FilePath -> SB.ByteString -> IO String
decodeOrDie fp bs = case TE.decodeUtf8' bs of
  Left _ -> hPutStrLn stderr ("NONUTF8 " ++ fp) >> exitWith (ExitFailure 3)
  Right t -> return (T.unpack t)

timeIt :: IO (Int, Int) -> IO (Double, Int, Int, Int)
timeIt act = do
  setAllocationCounter maxBound
  t0 <- getMonotonicTimeNSec
  (n, h) <- act
  _ <- evaluate n
  _ <- evaluate h
  t1 <- getMonotonicTimeNSec
  a1 <- getAllocationCounter
  let alloc = fromIntegral (maxBound - a1) :: Int
  return (fromIntegral (t1 - t0) / 1e9, n, h, alloc)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  case args of
    ["bench", engine, file, iters] -> benchMain engine file (read iters)
    ["dump", engine, file]         -> dumpMain engine file
    ["compare", file]              -> compareMain file
    _ -> do
      hPutStrLn stderr "usage: harness bench ENGINE FILE ITERS | dump ENGINE FILE | compare FILE"
      hPutStrLn stderr "  ENGINE = hand | alex-string | alex-sbs | alex-lbs | alex-lt | alex-st | alex-stf | alex-stfi | alex-stfw"
      exitWith (ExitFailure 2)

benchMain :: String -> FilePath -> Int -> IO ()
benchMain engine file iters = do
  bs <- SB.readFile file
  let nbytes = SB.length bs
  -- input prep, outside the timed region
  runIter <- case engine of
    "hand" -> do
      str <- decodeOrDie file bs
      _ <- evaluate (length str)
      return (\i -> evaluate (runHand ("it" ++ show i) str))
    "alex-string" -> do
      str <- decodeOrDie file bs
      _ <- evaluate (length str)
      return (\i -> evaluate (runAlexString ("it" ++ show i) str))
    "alex-sbs" ->
      return (\i -> evaluate (runAlexSBS ("it" ++ show i) bs))
    "alex-lbs" -> do
      let lbs = LB.fromStrict bs
      return (\i -> evaluate (runAlexLBS ("it" ++ show i) lbs))
    "alex-lt" -> case TE.decodeUtf8' bs of
      Left _ -> hPutStrLn stderr ("NONUTF8 " ++ file) >> exitWith (ExitFailure 3)
      Right t -> do
        let lt = TL.fromStrict t
        _ <- evaluate (TL.length lt)
        return (\i -> evaluate (runAlexLT ("it" ++ show i) lt))
    "alex-st" -> case TE.decodeUtf8' bs of
      Left _ -> hPutStrLn stderr ("NONUTF8 " ++ file) >> exitWith (ExitFailure 3)
      Right t -> do
        _ <- evaluate (T.length t)
        return (\i -> evaluate (runAlexST ("it" ++ show i) t))
    "alex-stf" -> case TE.decodeUtf8' bs of
      Left _ -> hPutStrLn stderr ("NONUTF8 " ++ file) >> exitWith (ExitFailure 3)
      Right t -> do
        _ <- evaluate (T.length t)
        return (\i -> evaluate (runAlexSTF ("it" ++ show i) t))
    "alex-stfi" -> case TE.decodeUtf8' bs of
      Left _ -> hPutStrLn stderr ("NONUTF8 " ++ file) >> exitWith (ExitFailure 3)
      Right t -> do
        _ <- evaluate (T.length t)
        return (\i -> evaluate (runAlexSTFI ("it" ++ show i) t))
    "alex-stfw" -> case TE.decodeUtf8' bs of
      Left _ -> hPutStrLn stderr ("NONUTF8 " ++ file) >> exitWith (ExitFailure 3)
      Right t -> do
        _ <- evaluate (T.length t)
        return (\i -> evaluate (runAlexSTFW ("it" ++ show i) t))
    _ -> hPutStrLn stderr ("bad engine " ++ engine) >> exitWith (ExitFailure 2)
  times <- newIORef []
  forM_ [1..iters] $ \i -> do
    (dt, n, h, alloc) <- timeIt (runIter i)
    modifyIORef' times (dt:)
    when (i == 1) $
      printf "TOKENS %d HASH %d BYTES %d ALLOC_LEX %d\n" n h nbytes alloc
  ts <- reverse <$> readIORef times
  let srt = sortD ts
      med = srt !! (length srt `div` 2)
      mn  = head srt
      tot = sum ts
  printf "ENGINE %s ITERS %d MEDIAN_S %.6f MIN_S %.6f TOTAL_S %.6f MEDIAN_MBPS %.2f\n"
         engine iters med mn tot (fromIntegral nbytes / 1e6 / med)
  forM_ (zip [1::Int ..] ts) $ \(i, dt) -> printf "ITER %d %.9f\n" i dt

sortD :: [Double] -> [Double]
sortD = foldr ins [] where
  ins x [] = [x]
  ins x (y:ys) | x <= y = x:y:ys
               | otherwise = y : ins x ys

dumpMain :: String -> FilePath -> IO ()
dumpMain engine file = do
  bs <- SB.readFile file
  toks <- case engine of
    "hand" -> do s <- decodeOrDie file bs
                 return (lexStart lflags (mkFString file) s)
    "alex-string" -> do s <- decodeOrDie file bs
                        return (AS.lexAlexStart lflags (mkFString file) s)
    "alex-sbs" -> return (ASB.lexAlexStart lflags (mkFString file) bs)
    "alex-lbs" -> return (ALB.lexAlexStart lflags (mkFString file) (LB.fromStrict bs))
    "alex-lt" -> do s <- decodeOrDie file bs
                    return (ALT.lexAlexStart lflags (mkFString file) (TL.pack s))
    "alex-st" -> do s <- decodeOrDie file bs
                    return (AST.lexAlexStart lflags (mkFString file) (T.pack s))
    "alex-stf" -> do s <- decodeOrDie file bs
                     return (ASTF.lexAlexStart lflags (mkFString file) (T.pack s))
    "alex-stfi" -> do s <- decodeOrDie file bs
                      return (ASTFI.lexAlexStart lflags (mkFString file) (T.pack s))
    "alex-stfw" -> do s <- decodeOrDie file bs
                      return (ASTFW.lexAlexStart lflags (mkFString file) (T.pack s))
    _ -> hPutStrLn stderr ("bad engine " ++ engine) >> exitWith (ExitFailure 2)
  mapM_ print (cutTokens toks)

compareMain :: FilePath -> IO ()
compareMain file = do
  bs <- SB.readFile file
  case TE.decodeUtf8' bs of
    Left _ -> putStrLn ("NONUTF8 " ++ file)
    Right t -> do
      let str = T.unpack t
          hand = cutTokens (lexStart lflags (mkFString file) str)
          variants = [ ("alex-string", cutTokens (AS.lexAlexStart lflags (mkFString file) str))
                     , ("alex-sbs",    cutTokens (ASB.lexAlexStart lflags (mkFString file) bs))
                     , ("alex-lbs",    cutTokens (ALB.lexAlexStart lflags (mkFString file) (LB.fromStrict bs)))
                     , ("alex-lt",     cutTokens (ALT.lexAlexStart lflags (mkFString file) (TL.fromStrict t)))
                     , ("alex-st",     cutTokens (AST.lexAlexStart lflags (mkFString file) t))
                     , ("alex-stf",    cutTokens (ASTF.lexAlexStart lflags (mkFString file) t))
                     , ("alex-stfi",   cutTokens (ASTFI.lexAlexStart lflags (mkFString file) t))
                     , ("alex-stfw",   cutTokens (ASTFW.lexAlexStart lflags (mkFString file) t)) ]
      results <- mapM (\(nm, toks) -> do
                          r <- try (evaluate (firstDiff 0 hand toks)) :: IO (Either SomeException (Maybe (Int, Maybe Token, Maybe Token)))
                          return (nm, r))
                      variants
      let report (nm, Left e) = putStrLn ("EXC " ++ file ++ " " ++ nm ++ " " ++ takeWhile (/= '\n') (show e))
          report (nm, Right Nothing) = return ()
          report (nm, Right (Just (i, a, b))) =
            putStrLn ("MISMATCH " ++ file ++ " " ++ nm ++ " at token " ++ show i
                      ++ "\n  hand: " ++ maybe "<end>" show a
                      ++ "\n  alex: " ++ maybe "<end>" show b)
      mapM_ report results
      let allOK = all (\(_, r) -> case r of Right Nothing -> True; _ -> False) results
      when allOK $ putStrLn ("OK " ++ file ++ " " ++ show (length hand) ++ " tokens")

-- first index where the two token streams differ (deep comparison via Eq;
-- also compares the is_stdlib-insensitive Position Eq plus payloads)
firstDiff :: Int -> [Token] -> [Token] -> Maybe (Int, Maybe Token, Maybe Token)
firstDiff _ [] [] = Nothing
firstDiff i (a:as) (b:bs)
  | tokEq a b = firstDiff (i+1) as bs
  | otherwise = Just (i, Just a, Just b)
firstDiff i as bs = Just (i, headM as, headM bs)
  where headM [] = Nothing
        headM (x:_) = Just x

-- Token Eq uses Position Eq which ignores is_stdlib; that matches what the
-- parser can observe.  Additionally compare the printed form to catch
-- FString content differences.
tokEq :: Token -> Token -> Bool
tokEq a b = a == b && show a == show b
