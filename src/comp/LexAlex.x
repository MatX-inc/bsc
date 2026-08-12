{
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-binds -fno-warn-missing-signatures -fno-warn-tabs #-}
-- Alex-generated lexer for the Bluespec Classic syntax, taking strict
-- Text input (the UTF-8-decoded contents of the source file).
--
-- Produces exactly the token stream of the hand-written Lex.hs: same
-- Token/LexItem constructors and same Positions, including Lex.hs's
-- column-accounting quirks (which are replicated here bug-for-bug, see
-- lexLitChar' and the "package" rule).  The private helpers of Lex.hs
-- that affect token values (lexLitChar', readN, nextTab, the SV keyword
-- sets) are ported verbatim below so behavior cannot drift silently.
--
-- The DFA is fed one byte per source *character*: ASCII characters as
-- themselves, non-ASCII characters collapsed to one of five pseudo-bytes
-- (0xF1-0xF5) according to exactly the character-class predicates the
-- hand lexer applies (in its testing order: isSym before isAlpha), like
-- GHC's own lexer does.  Hence token lengths reported by alexScan are in
-- characters.
--
-- The build system runs alex on this file (see the LexAlex.hs rule in
-- the Makefile); to regenerate by hand: alex -g LexAlex.x -o LexAlex.hs
module LexAlex(lexAlexStart, lexAlexStartWithPos) where

import Data.Char
import Data.Word(Word8)
import qualified Data.Set as S
import qualified Data.Text as T
import Numeric(readFloat)

import Lex(Token(..), LexItem(..), LexError(..), LFlags(..), isIdChar, isSym)
import Position
import FStringCompat(FString, mkFString)
import PreStrings(fsEmpty)
import ErrorUtil(internalError)
import SystemVerilogKeywords
}

%encoding "latin1"

$digit     = [0-9]
$hexdig    = [0-9a-fA-F]
$octdig    = [0-7]
$bindig    = [01]
$idstart   = [a-zA-Z_\xF3]
$idchar    = [a-zA-Z0-9_'\xF2\xF3\xF4]
$symall    = [\!\@\#\$\%\&\*\+\.\/\<\=\>\?\\\^\|\:\-\~\,\xF1\xF2]
$symstart  = [\!\@\#\$\%\&\*\+\/\<\=\>\?\\\^\|\:\-\~\xF1\xF2]
$commstart = [^\!\#\$\%\&\*\+\.\/\<\=\>\?\\\^\|\:\-\~\,\xF1\xF2\n]
tokens :-

-- (preprocessor line directives `# <n> "<file>"` are handled by the driver
-- loop before calling alexScan, since they are only recognized at column 0)

-- whitespace (Lex.hs lx; \r \v \f reset the column to 0)
\ +          { wsSpaces }
\n+          { wsNewlines }
\t+          { wsTabs }
[\r\v\f]+    { wsCRs }

-- line comments: "--" then any number of '-', then EOL, '@', or a non-symbol
-- char (Lex.hs isComm); a comment without a trailing newline at EOF
-- is a LexMissingNL error (Lex.hs skipToEOL)
"--" \-* $commstart [^\n]* \n   { lineCommentNL }
"--" \-* \n                     { lineCommentNL }
"--" \-* $commstart [^\n]*      { lineCommentEOF }
"--" \-*                        { lineCommentEOF }

-- pragma brackets and nested block comments
"{-#"        { fixTok L_lpragma }
"#-}"        { fixTok L_rpragma }
"{-"         { blockComment }

-- single-char punctuation; note '.' and ',' are matched before the symbol
-- rule, so they never *start* an operator run
"("          { fixTok L_lpar }
")"          { fixTok L_rpar }
","          { fixTok L_comma }
";"          { fixTok L_semi }
"`"          { fixTok L_bquote }
"{"          { fixTok L_lcurl }
"}"          { fixTok L_rcurl }
"["          { fixTok L_lbra }
"]"          { fixTok L_rbra }
"."          { fixTok L_dot }

-- character and string literals (escape forms per Lex.hs lexLitChar')
\' ([^\\\n] | \\ [ntrvf\'\"\\] | \\ x $hexdig*) \'        { charTok }
\" ([^\\\"\n] | \\ [ntrvf\'\"\\] | \\ x $hexdig*)* \"     { stringTok }

-- unbased unsized bit literals '0 / '1 with no closing quote (the quoted
-- forms '0' / '1' are char literals: the rule above is a longer match).
-- Lex.hs goes through lexLitChar' here, so a hex escape decoding to '0'
-- or '1' (e.g. '\x30) also counts; anything else is LexBadCharLit.
\' [01]           { uubTok }
\' \\ x $hexdig*  { uubTok }

-- integer and real literals (Lex.hs lInteger, lReal);
-- underscores are digit separators everywhere except in float/exponent
-- parts and sized-literal widths (quirks of the hand lexer, replicated)
0 [xX] $hexdig [$hexdig \_]*                 { basedTok 16 }
0 [bB] $bindig [$bindig \_]*                 { basedTok 2 }
0 [oO] $octdig [$octdig \_]*                 { basedTok 8 }
$digit+ \' h $hexdig [$hexdig \_]*           { sizedTok 16 }
$digit+ \' d $digit  [$digit \_]*            { sizedTok 10 }
$digit+ \' b $bindig [$bindig \_]*           { sizedTok 2 }
$digit+ \' o $octdig [$octdig \_]*           { sizedTok 8 }
$digit+ \. $digit+ ([eE] [\+\-]? $digit+)?   { floatTok }
$digit+ [eE] [\+\-]? $digit+                 { floatTok }
$digit [$digit \_]*                          { decTok }

-- task/system identifiers: $ followed by an identifier char
\$ $idchar [$idchar \$]*                     { dollarTok }

-- reserved operators; listed before the generic symbol rule so they win ties
"::"         { fixTok L_dcolon }
":"          { fixTok L_colon }
"="          { fixTok L_eq }
"@"          { fixTok L_at }
\\           { fixTok L_lam }
"->"         { fixTok L_rarrow }
"==>"        { fixTok L_drarrow }
"=>"         { fixTok L_irarrow }
"<-"         { fixTok L_larrow }

-- reserved words; before the generic identifier rule
"_"          { fixTok L_uscore }
"action"     { fixTok L_action }
"case"       { fixTok L_case }
"class"      { fixTok L_class }
"data"       { fixTok L_data }
"deriving"   { fixTok L_deriving }
"do"         { fixTok L_do }
"else"       { fixTok L_else }
"foreign"    { fixTok L_foreign }
"if"         { fixTok L_if }
"import"     { fixTok L_import }
"in"         { fixTok L_in }
"coherent"   { fixTok L_coherent }
"incoherent" { fixTok L_incoherent }
"infix"      { fixTok L_infix }
"infixl"     { fixTok L_infixl }
"infixr"     { fixTok L_infixr }
"interface"  { fixTok L_interface }
"instance"   { fixTok L_instance }
"let"        { fixTok L_let }
"letseq"     { fixTok L_letseq }
"module"     { fixTok L_module }
"of"         { fixTok L_of }
"package"    { pkgTok }
"primitive"  { fixTok L_primitive }
"qualified"  { fixTok L_qualified }
"rules"      { fixTok L_rules }
"signature"  { fixTok L_signature }
"struct"     { fixTok L_struct }
"then"       { fixTok L_then }
"type"       { fixTok L_type }
"valueOf"    { fixTok L_valueOf }
"stringOf"   { fixTok L_stringOf }
"verilog"    { fixTok L_verilog }
"via"        { fixTok L_via }
"synthesize" { fixTok L_synthesize }
"when"       { fixTok L_when }
"where"      { fixTok L_where }

-- generic operator runs and identifiers
$symstart $symall*                           { symTok }
$idstart $idchar*                            { idTok }

-- fallbacks: unterminated/bad literals, bad characters (Lex.hs lexerr)
\'           { badCharTok }
\"           { badStringTok }
.            { badChar }
{
type Stream = T.Text

-- ---------------------------------------------------------------------------
-- Input stream: yield one Char at a time.

unconsChar :: Stream -> Maybe (Char, Stream)
unconsChar = T.uncons
{-# INLINE unconsChar #-}

-- first n chars of the stream, as a String (lazy)
takeStr :: Int -> Stream -> String
takeStr n s
  | n <= 0 = []
  | otherwise = case unconsChar s of
      Just (c, s') -> c : takeStr (n-1) s'
      Nothing      -> []

-- generic span returning the matched prefix as a String
spanG :: (Char -> Bool) -> Stream -> (String, Stream)
spanG p s = case unconsChar s of
  Just (c, s') | p c -> let (cs, rest) = spanG p s' in (c:cs, rest)
  _ -> ([], s)

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

-- The DFA input is just the character stream; file/line/column are threaded
-- through the driver loop as function arguments (like the hand lexer), so
-- stepping the input allocates no position wrapper.
type AlexInput = Stream

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte s = case unconsChar s of
  Nothing       -> Nothing
  Just (ch, s') -> Just (classify ch, s')
{-# INLINE alexGetByte #-}

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = internalError "LexAlex: alexInputPrevChar not used"

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
-- column 0 (checked by the caller).  Replicates Lex.hs lx exactly:
-- requires the prefix '#', ' ', digit; consumes through the newline.
-- Returns Nothing when the prefix does not match (caller falls through to
-- the DFA, where '#' lexes as a symbol char).
checkDirective :: Stream -> Maybe (FString, Int, Stream)
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

-- replicates Lex.hs skipComm: nested brace-dash comments, only \n and \t
-- treated specially inside.  Returns Right (line, col, rest) on close,
-- Left (line, col at EOF) when unterminated.  (The braces are written as
-- numeric escapes because Alex's code-fragment scanner counts every brace
-- character, even inside Haskell char literals.)
skipCommG :: Int -> Int -> Int -> Stream -> Either (Int, Int) (Int, Int, Stream)
skipCommG !n !l !c s
  | n == 0 = Right (l, c, s)
  | otherwise = case unconsChar s of
      Nothing -> Left (l, c)
      Just ('-', s1) -> case unconsChar s1 of
          Just ('\125', s2) -> skipCommG (n-1) l (c+2) s2   -- close brace
          _                 -> skipCommG n l (c+1) s1
      Just ('\123', s1) -> case unconsChar s1 of            -- open brace
          Just ('-', s2) -> skipCommG (n+1) l (c+2) s2
          _              -> skipCommG n l (c+1) s1
      Just ('\n', s1) -> skipCommG n (l+1) 0 s1
      Just ('\t', s1) -> skipCommG n l (nextTab (c+1)) s1
      Just (_, s1)    -> skipCommG n l (c+1) s1

-- SystemVerilog keyword/symbol sets (copies of Lex.hs private sets) ----------

isSvKeyword :: String -> Bool
isSvKeyword str = str `S.member` svKeywordSet

isSvSymbol :: String -> Bool
isSvSymbol str = str `S.member` svSymbolSet

svKeywordSet :: S.Set String
svKeywordSet = S.fromList [str | (_, str, _) <- svParserKeywordTable]

svSymbolSet :: S.Set String
svSymbolSet = S.fromList [str | (_, str, _) <- svSymbolTable]

-- ---------------------------------------------------------------------------
-- Driver and actions.
-- Action args: flags, file, line, col (token start), stream at token start,
-- stream after token, token length in chars.

type Action = LFlags -> FString -> Int -> Int -> Stream -> Stream -> Int -> [Token]

lexAlexStart :: LFlags -> FString -> Stream -> [Token]
lexAlexStart lf f s = go lf f 1 0 s

-- start lexing at a given position (used for Classic text embedded in
-- BSV source); replicates Lex.hs lexStartWithPos, including its
-- rejection of unknown positions
lexAlexStartWithPos :: LFlags -> Position -> Stream -> [Token]
lexAlexStartWithPos lf pos s
  | getPositionLine pos == -1 || getPositionColumn pos == -1 =
      internalError "LexAlex.lexAlexStartWithPos: unknown position"
  | otherwise =
      go lf (mkFString (getPositionFile pos)) (getPositionLine pos)
         (getPositionColumn pos) s

go :: LFlags -> FString -> Int -> Int -> Stream -> [Token]
go lf f !l !c s
  | c == 0, Just (fn, n, s') <- checkDirective s = go lf fn n 0 s'
  | otherwise = case alexScan s 0 of
      AlexEOF -> [Token (mkPositionFull f (l+1) (-1) (lf_is_stdlib lf)) L_eof]
      AlexError s' ->
        -- defensive; every byte is covered by some rule, so this is unreachable
        case unconsChar s' of
          Just (ch, _) -> lexErrTokens f l c (LexBadLexChar ch)
          Nothing -> internalError "LexAlex: scan error at EOF"
      AlexSkip s' _ -> go lf f l c s'
      AlexToken s' len act -> act lf f l c s s' len

-- error token stream, exactly Lex.hs lexerr (infinite L_eof tail)
lexErrTokens :: FString -> Int -> Int -> LexError -> [Token]
lexErrTokens f l c err = map (Token (mkPosition f l c)) (L_error err : repeat L_eof)

-- fixed token whose column advance equals its character count
fixTok :: LexItem -> Action
fixTok li lf f l c _ s' len =
  Token (mkPositionFull f l c (lf_is_stdlib lf)) li : go lf f l (c+len) s'

-- "package" is emitted at column c-1 (multiple-packages hack in Lex.hs)
pkgTok :: Action
pkgTok lf f l c _ s' len =
  Token (mkPositionFull f l (c-1) (lf_is_stdlib lf)) L_package : go lf f l (c+len) s'

wsSpaces, wsNewlines, wsTabs, wsCRs :: Action
wsSpaces   lf f l c _ s' len = go lf f l (c+len) s'
wsNewlines lf f l _ _ s' len = go lf f (l+len) 0 s'
wsTabs     lf f l c _ s' len = go lf f l (tabAdvance c len) s'
wsCRs      lf f l _ _ s' _   = go lf f l 0 s'

lineCommentNL, lineCommentEOF :: Action
lineCommentNL  lf f l _ _ s' _ = go lf f (l+1) 0 s'
lineCommentEOF _  f l _ _ _  _ = lexErrTokens f l 0 LexMissingNL

blockComment :: Action
blockComment lf f l c _ s' _ =
  case skipCommG 1 l (c+2) s' of
    Right (l2, c2, s2) -> go lf f l2 c2 s2
    Left (le, ce) -> lexErrTokens f le ce (LexUntermComm (mkPosition fsEmpty l c))

charTok :: Action
charTok lf f l c s s' len =
  let (cc, w) = decodeCharLit (takeStr len s)
  in  Token (mkPositionFull f l c (lf_is_stdlib lf)) (L_char cc) : go lf f l (c+w) s'

stringTok :: Action
stringTok lf f l c s s' len =
  let (str, w) = decodeStringLit (takeStr len s)
  in  Token (mkPositionFull f l c (lf_is_stdlib lf)) (L_string str) : go lf f l (c+w) s'

-- unbased unsized literal: '0 / '1, or a hex escape decoding to '0'/'1'
-- (the lexeme has no closing quote; Lex.hs advances by 1 + lexLitChar's
-- count, which for both lexeme shapes equals the lexeme length in chars)
uubTok :: Action
uubTok lf f l c s s' len =
  let p = mkPositionFull f l c (lf_is_stdlib lf)
      rest = go lf f l (c+len) s'
  in  case lexLitChar' (drop 1 (takeStr len s)) of
        Just ('0', _, _) -> Token p (L_unbasedUnsized False) : rest
        Just ('1', _, _) -> Token p (L_unbasedUnsized True)  : rest
        _                -> lexErrTokens f l c LexBadCharLit

basedTok, sizedTok :: Integer -> Action
basedTok base lf f l c s s' len = numTok (mkBasedInt base (takeStr len s)) lf f l c s s' len
sizedTok base lf f l c s s' len = numTok (mkSizedInt base (takeStr len s)) lf f l c s s' len

decTok, floatTok :: Action
decTok   lf f l c s s' len = numTok (mkDecInt (takeStr len s)) lf f l c s s' len
floatTok lf f l c s s' len = numTok (mkFloat (takeStr len s)) lf f l c s s' len

numTok :: LexItem -> Action
numTok li lf f l c _ s' len =
  Token (mkPositionFull f l c (lf_is_stdlib lf)) li : go lf f l (c+len) s'

dollarTok :: Action
dollarTok lf f l c s s' len =
  Token (mkPositionFull f l c (lf_is_stdlib lf)) (L_varid (mkFString (takeStr len s)))
    : go lf f l (c+len) s'

symTok :: Action
symTok lf f l c s s' len =
  let str = takeStr len s
      p = mkPositionFull f l c (lf_is_stdlib lf)
      rest = go lf f l (c+len) s'
  in  if not (lf_allow_sv_kws lf) && isSvSymbol str
      then internalError ("SystemVerilog symbol forbidden: " ++ str)
      else if head str == ':'
           then Token p (L_consym (mkFString str)) : rest
           else Token p (L_varsym (mkFString str)) : rest

idTok :: Action
idTok lf f l c s s' len =
  let str = takeStr len s
      p = mkPositionFull f l c (lf_is_stdlib lf)
      rest = go lf f l (c+len) s'
  in  if not (lf_allow_sv_kws lf) && isSvKeyword str
      then internalError ("SystemVerilog keyword forbidden: " ++ str)
      else if isUpper (head str)
           then Token p (L_conid (mkFString str)) : rest
           else Token p (L_varid (mkFString str)) : rest

badCharTok, badStringTok, badChar :: Action
badCharTok   _ f l c _ _ _ = lexErrTokens f l c LexBadCharLit
badStringTok _ f l c _ _ _ = lexErrTokens f l c LexBadStringLit
badChar      _ f l c s _ _ =
  case unconsChar s of
    Just (ch, _) -> lexErrTokens f l c (LexBadLexChar ch)
    Nothing -> internalError "LexAlex: badChar at EOF"
}
