{-# LANGUAGE BangPatterns #-}
-- Shared support code for the Alex-generated Bluespec Classic lexers.
-- Contains: the input-stream class (String / strict BS / lazy BS with
-- inline UTF-8 decoding), the byte classification used by the DFA
-- (non-ASCII chars are mapped to pseudo-bytes, like GHC's own lexer),
-- and verbatim ports of Lex.hs's private helpers (lexLitChar', readN,
-- nextTab, the SV keyword sets) so behavior matches bug-for-bug.
module LexAlexShared where

import Data.Char
import Data.Word(Word8)
import Data.Bits((.&.), shiftL)
import qualified Data.Set as S
import qualified Data.ByteString as SB
import qualified Data.ByteString.Unsafe as BU
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Numeric(readFloat)

import Lex(LexItem(..), isIdChar, isSym)
import FStringCompat(FString, mkFString)
import ErrorUtil(internalError)
import SystemVerilogKeywords

-- ---------------------------------------------------------------------------
-- Input streams: yield one Char at a time (UTF-8 decode for ByteStrings).
-- bsc validates UTF-8 up front (FileIOUtil.decodeUtf8orError), so the
-- ByteString decoders assume well-formed UTF-8.

class LexStream s where
  unconsChar :: s -> Maybe (Char, s)

instance LexStream [Char] where
  unconsChar []     = Nothing
  unconsChar (c:cs) = Just (c, cs)
  {-# INLINE unconsChar #-}

mb2 :: Int -> Int -> Char
mb2 w0 w1 = chr (((w0 - 0xC0) `shiftL` 6) + (w1 - 0x80))
{-# INLINE mb2 #-}

mb3 :: Int -> Int -> Int -> Char
mb3 w0 w1 w2 = chr (((w0 - 0xE0) `shiftL` 12) + ((w1 - 0x80) `shiftL` 6) + (w2 - 0x80))
{-# INLINE mb3 #-}

mb4 :: Int -> Int -> Int -> Int -> Char
mb4 w0 w1 w2 w3 = chr (((w0 - 0xF0) `shiftL` 18) + ((w1 - 0x80) `shiftL` 12)
                       + ((w2 - 0x80) `shiftL` 6) + (w3 - 0x80))
{-# INLINE mb4 #-}

instance LexStream SB.ByteString where
  unconsChar bs
    | SB.null bs = Nothing
    | w0 < 0x80  = Just (chr w0, BU.unsafeDrop 1 bs)
    | w0 < 0xE0  = Just (mb2 w0 (ix 1), BU.unsafeDrop 2 bs)
    | w0 < 0xF0  = Just (mb3 w0 (ix 1) (ix 2), BU.unsafeDrop 3 bs)
    | otherwise  = Just (mb4 w0 (ix 1) (ix 2) (ix 3), BU.unsafeDrop 4 bs)
    where w0 = fromIntegral (BU.unsafeHead bs) :: Int
          ix i = fromIntegral (BU.unsafeIndex bs i) :: Int
  {-# INLINE unconsChar #-}

instance LexStream LB.ByteString where
  unconsChar b0 = case LB.uncons b0 of
    Nothing -> Nothing
    Just (b, r0)
      | w0 < 0x80 -> Just (chr w0, r0)
      | w0 < 0xE0 -> case LB.uncons r0 of
          Just (b1, r1) -> Just (mb2 w0 (fromIntegral b1), r1)
          Nothing -> internalError "LexAlexShared: truncated UTF-8"
      | w0 < 0xF0 -> case LB.uncons r0 of
          Just (b1, r1) -> case LB.uncons r1 of
            Just (b2, r2) -> Just (mb3 w0 (fromIntegral b1) (fromIntegral b2), r2)
            _ -> internalError "LexAlexShared: truncated UTF-8"
          _ -> internalError "LexAlexShared: truncated UTF-8"
      | otherwise -> case LB.uncons r0 of
          Just (b1, r1) -> case LB.uncons r1 of
            Just (b2, r2) -> case LB.uncons r2 of
              Just (b3, r3) -> Just (mb4 w0 (fromIntegral b1) (fromIntegral b2) (fromIntegral b3), r3)
              _ -> internalError "LexAlexShared: truncated UTF-8"
            _ -> internalError "LexAlexShared: truncated UTF-8"
          _ -> internalError "LexAlexShared: truncated UTF-8"
      where w0 = fromIntegral b :: Int
  {-# INLINE unconsChar #-}

instance LexStream TL.Text where
  unconsChar = TL.uncons
  {-# INLINE unconsChar #-}

instance LexStream T.Text where
  unconsChar = T.uncons
  {-# INLINE unconsChar #-}

-- first n chars of the stream, as a String (lazy)
takeStr :: LexStream s => Int -> s -> String
takeStr n s
  | n <= 0 = []
  | otherwise = case unconsChar s of
      Just (c, s') -> c : takeStr (n-1) s'
      Nothing      -> []
{-# INLINABLE takeStr #-}

-- ---------------------------------------------------------------------------
-- Byte classification for the DFA.  ASCII chars are themselves; non-ASCII
-- chars collapse to one of five pseudo-bytes according to exactly the
-- predicates the hand-written lexer applies (and in its testing order:
-- isSym is checked before isAlpha in Lex.hs).

uSymOnly, uSymId, uAlpha, uIdCont, uOther :: Word8
uSymOnly = 0xF1  -- isSym && not isIdChar
uSymId   = 0xF2  -- isSym && isIdChar
uAlpha   = 0xF3  -- not isSym && isAlpha
uIdCont  = 0xF4  -- not isSym && not isAlpha && isIdChar
uOther   = 0xF5  -- everything else (lexical error)

classify :: Char -> Word8
classify c
  | c < '\x80' = fromIntegral (ord c)
  | isSym c    = if isIdChar c then uSymId else uSymOnly
  | isAlpha c  = uAlpha
  | isIdChar c = uIdCont
  | otherwise  = uOther
{-# INLINE classify #-}

-- ---------------------------------------------------------------------------
-- Verbatim ports of Lex.hs private helpers.

tabStop :: Int
tabStop = 8

nextTab :: Int -> Int
nextTab c = ((c + tabStop - 1) `div` tabStop) * tabStop

-- n consecutive tabs starting at column c (nextTab (c+1) iterated n times)
tabAdvance :: Int -> Int -> Int
tabAdvance c n = (c `div` tabStop + n) * tabStop

readN :: Integer -> String -> Integer
readN radix s =
     foldl1 (\n d -> n * radix + d)
            (map (toInteger . digitToInt) s)

-- exact copy of Lex.hs lexLitChar' (note: n undercounts simple escapes by
-- one, which is why the hand lexer's columns drift on them; we replicate it)
lexLitChar' :: String -> Maybe (Char, Int, String)
lexLitChar' ('\\':s)     = lexEsc s
        where
        lexEsc ('x':s)  = let (n,s') = span isHexDigit s in Just (chr (fromInteger (readN 16 n)), 2+length n, s')
        lexEsc ('n':s)  = Just ('\n', 1, s)
        lexEsc ('t':s)  = Just ('\t', 1, s)
        lexEsc ('r':s)  = Just ('\r', 1, s)
        lexEsc ('v':s)  = Just ('\v', 1, s)
        lexEsc ('f':s)  = Just ('\f', 1, s)
        lexEsc ('"':s)  = Just ('"', 1, s)
        lexEsc ('\'':s) = Just ('\'', 1, s)
        lexEsc ('\\':s) = Just ('\\', 1, s)
        lexEsc _        = Nothing
lexLitChar' ('\n':_)     = Nothing
lexLitChar' (c:s)        = Just (c, 1, s)
lexLitChar' ""           = Nothing

-- char literal: lexeme includes both quotes; returns (value, column advance)
decodeCharLit :: String -> (Char, Int)
decodeCharLit lexeme =
  case lexLitChar' (drop 1 lexeme) of
    Just (cc, n, _) -> (cc, 2 + n)
    Nothing -> internalError ("decodeCharLit: " ++ show lexeme)

-- string literal: lexeme includes both quotes; returns (value, column advance)
-- replicates Lex.hs lexString's column accounting (c+1 for the opening
-- quote, +n per char, +1 for the closing quote)
decodeStringLit :: String -> (String, Int)
decodeStringLit lexeme = go (drop 1 lexeme) 1 []
  where
    go ('"':_) !w acc = (reverse acc, w + 1)
    go s       !w acc = case lexLitChar' s of
                          Just (x, n, s') -> go s' (w + n) (x:acc)
                          Nothing -> internalError ("decodeStringLit: " ++ show lexeme)

-- `# <line> "<file>"` preprocessor line directive, recognized only at
-- column 0 (checked by the caller).  Replicates Lex.hs:179-196 exactly:
-- requires the prefix '#', ' ', digit; consumes through the newline.
-- Returns Nothing when the prefix does not match (caller falls through to
-- the DFA, where '#' lexes as a symbol char).
checkDirective :: LexStream s => s -> Maybe (FString, Int, s)
checkDirective s0 =
  case unconsChar s0 of
    Just ('#', s1) -> case unconsChar s1 of
      Just (' ', s2) -> case unconsChar s2 of
        Just (d, _) | isDigit d ->
          let (li, r) = spanG (/= '\n') s2
              (ns, spfs) = span isDigit li
              fs = takeWhile (not . isSpace) (dropWhile isSpace spfs)
              n = read ns
              fn = if length fs > 2 && head fs == '"' && last fs == '"'
                     then init (tail fs) else "???"
              r' = case unconsChar r of
                     Just ('\n', rest) -> rest
                     Nothing -> r
                     _ -> internalError "checkDirective: spanG failure"
          in  Just (mkFString fn, n, r')
        _ -> Nothing
      _ -> Nothing
    _ -> Nothing
{-# INLINABLE checkDirective #-}

-- generic span returning the matched prefix as a String
spanG :: LexStream s => (Char -> Bool) -> s -> (String, s)
spanG p s = case unconsChar s of
  Just (c, s') | p c -> let (cs, rest) = spanG p s' in (c:cs, rest)
  _ -> ([], s)
{-# INLINABLE spanG #-}

-- number token builders ------------------------------------------------------

-- "0x.." / "0b.." / "0o.." literal (lexeme includes the 2-char prefix)
mkBasedInt :: Integer -> String -> LexItem
mkBasedInt base lexeme =
  L_integer Nothing base (readN base (filter (/= '_') (drop 2 lexeme)))

-- sized literal like 12'h5A (fmt char is always 1 char, base selects it)
mkSizedInt :: Integer -> String -> LexItem
mkSizedInt base lexeme =
  let (szs, rest) = span isDigit lexeme
      digits = drop 2 rest   -- skip ' and the fmt char
  in  L_integer (Just (readN 10 szs)) base (readN base (filter (/= '_') digits))

mkDecInt :: String -> LexItem
mkDecInt lexeme = L_integer Nothing 10 (readN 10 (filter (/= '_') lexeme))

mkFloat :: String -> LexItem
mkFloat s =
  L_float (case readFloat s of
             [(n, "")] -> n
             _ -> internalError ("lReal: readFloat: " ++ s))

-- comment skipping ------------------------------------------------------------

-- replicates Lex.hs skipComm: nested {- -}, only \n and \t treated specially
-- inside.  Returns Right (line, col, rest) on close, Left (line, col at EOF)
-- when unterminated.
skipCommG :: LexStream s => Int -> Int -> Int -> s -> Either (Int, Int) (Int, Int, s)
skipCommG !n !l !c s
  | n == 0 = Right (l, c, s)
  | otherwise = case unconsChar s of
      Nothing -> Left (l, c)
      Just ('-', s1) -> case unconsChar s1 of
          Just ('}', s2) -> skipCommG (n-1) l (c+2) s2
          _              -> skipCommG n l (c+1) s1
      Just ('{', s1) -> case unconsChar s1 of
          Just ('-', s2) -> skipCommG (n+1) l (c+2) s2
          _              -> skipCommG n l (c+1) s1
      Just ('\n', s1) -> skipCommG n (l+1) 0 s1
      Just ('\t', s1) -> skipCommG n l (nextTab (c+1)) s1
      Just (_, s1)    -> skipCommG n l (c+1) s1
{-# INLINABLE skipCommG #-}

-- SystemVerilog keyword/symbol sets (copies of Lex.hs private sets) ----------

isSvKeyword :: String -> Bool
isSvKeyword str = str `S.member` svKeywordSet

isSvSymbol :: String -> Bool
isSvSymbol str = str `S.member` svSymbolSet

svKeywordSet :: S.Set String
svKeywordSet = S.fromList [str | (_, str, _) <- svKeywordTable]

svSymbolSet :: S.Set String
svSymbolSet = S.fromList [str | (_, str, _) <- svSymbolTable]
