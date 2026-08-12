{
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-binds -fno-warn-missing-signatures -fno-warn-tabs #-}
-- The Bluespec Classic lexer (Alex-generated, taking strict Text input:
-- the UTF-8-decoded contents of the source file), together with the
-- Token/LexItem definitions shared with the parsers.
--
-- Produces exactly the token stream of the previous hand-written String
-- lexer: same Token/LexItem constructors and same Positions, including
-- its column-accounting quirks (which are replicated here bug-for-bug,
-- see lexLitChar' and the "package" rule).  The helpers of the hand
-- lexer that affect token values (lexLitChar', readN, nextTab, the SV
-- keyword sets) were ported verbatim so behavior cannot drift silently.
--
-- The DFA is fed one byte per source *character*: ASCII characters as
-- themselves, non-ASCII characters collapsed to one of five pseudo-bytes
-- (0xF1-0xF5) according to exactly the character-class predicates the
-- hand lexer applies (in its testing order: isSym before isAlpha), like
-- GHC's own lexer does.  Hence token lengths reported by alexScan are in
-- characters.
--
-- The build system runs alex on this file (see the Lex.hs rule in
-- the Makefile); to regenerate by hand: alex -g Lex.x -o Lex.hs
module Lex(Token(..), LexItem(..), LexError(..), LFlags(..), prLexItem,
           lexStart, lexStartWithPos,
           isIdChar, isSym, convLexErrorToErrMsg) where

import Data.Char
import Data.Word(Word8, Word64)
import Data.Bits(unsafeShiftR, (.&.))
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Internal as TI
import qualified Data.Text.Array as TA
import Numeric(readFloat)

import Util(itos)
import Position
import FStringCompat
import PreStrings(fsEmpty)
import Error(internalError, ErrMsg(..))
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

-- whitespace (hand lexer's lx; \r \v \f reset the column to 0)
\ +          { wsSpaces }
\n+          { wsNewlines }
\t+          { wsTabs }
[\r\v\f]+    { wsCRs }

-- line comments: "--" then any number of '-', then EOL, '@', or a non-symbol
-- char (hand lexer's isComm); a comment without a trailing newline at
-- EOF is a LexMissingNL error (hand lexer's skipToEOL)
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

-- character and string literals (escape forms per lexLitChar' below)
\' ([^\\\n] | \\ [ntrvf\'\"\\] | \\ x $hexdig*) \'        { charTok }
\" ([^\\\"\n] | \\ [ntrvf\'\"\\] | \\ x $hexdig*)* \"     { stringTok }

-- unbased unsized bit literals '0 / '1 with no closing quote (the quoted
-- forms '0' / '1' are char literals: the rule above is a longer match).
-- The hand lexer went through lexLitChar' here, so a hex escape decoding to '0'
-- or '1' (e.g. '\x30) also counts; anything else is LexBadCharLit.
\' [01]           { uubTok }
\' \\ x $hexdig*  { uubTok }

-- integer and real literals (hand lexer's lInteger, lReal);
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

-- fallbacks: unterminated/bad literals, bad characters (hand lexer's lexerr)
\'           { badCharTok }
\"           { badStringTok }
.            { badChar }
{
type Stream = T.Text

-- import Debug.Trace

-- data structure for lexical errors
-- so raw error messages are not in LexItem
data LexError = LexBadCharLit
              | LexBadStringLit
              | LexBadLexChar Char
              | LexUntermComm Position
              | LexMissingNL
              deriving(Eq)

convLexErrorToErrMsg :: LexError -> ErrMsg
convLexErrorToErrMsg (LexBadCharLit) = EBadCharLit
convLexErrorToErrMsg (LexBadStringLit) = EBadStringLit
convLexErrorToErrMsg (LexBadLexChar c) = EBadLexChar c
convLexErrorToErrMsg (LexUntermComm p) = EUntermComm p
convLexErrorToErrMsg (LexMissingNL) = EMissingNL

data LexItem =
          L_varid FString
        | L_conid FString
        | L_varsym FString
        | L_consym FString
        | L_integer (Maybe Integer) Integer Integer                -- bit size (if specified), base, value
        | L_float Rational
        | L_char Char
        | L_string String
        | L_lpar
        | L_rpar
        | L_semi
        | L_uscore
        | L_bquote
        | L_lcurl
        | L_rcurl
        | L_lbra
        | L_rbra
        -- reserved words
        | L_action | L_case | L_class | L_data | L_deriving | L_do | L_else | L_foreign
        | L_if | L_import | L_in
        | L_infix | L_infixl | L_infixr
        | L_interface | L_instance
        | L_let | L_letseq | L_package | L_of
        | L_primitive | L_qualified | L_rules | L_signature | L_struct
        | L_then | L_module | L_type | L_valueOf | L_stringOf | L_verilog | L_via | L_synthesize | L_when | L_where
        | L_coherent | L_incoherent
        -- reserved ops
        | L_dcolon | L_colon | L_eq | L_at | L_lam | L_bar
        | L_rarrow | L_larrow | L_dot | L_comma | L_drarrow | L_irarrow
        -- layout items
        | L_lcurl_o | L_rcurl_o | L_semi_o
        -- pragma
        | L_lpragma | L_rpragma
        -- unbased unsized bit literals ('0 all-zeros, '1 all-ones)
        | L_unbasedUnsized Bool
        -- pseudo items
        | L_eof
        | L_error LexError
        deriving (Eq)

prLexItem :: LexItem -> String
prLexItem (L_varid s) = getFString s
prLexItem (L_conid s) = getFString s
prLexItem (L_varsym s) = getFString s
prLexItem (L_consym s) = getFString s
prLexItem (L_integer _ _ i) = itos i
prLexItem (L_float r) = show r
prLexItem (L_char c) = show c
prLexItem (L_string s) = show s
prLexItem L_lpar = "("
prLexItem L_rpar = ")"
prLexItem L_semi = ";"
prLexItem L_uscore = "_"
prLexItem L_bquote = "`"
prLexItem L_lcurl = "{"
prLexItem L_rcurl = "}"
prLexItem L_lbra = "["
prLexItem L_rbra = "]"
prLexItem L_action = "action"
prLexItem L_case = "case"
prLexItem L_class = "class"
prLexItem L_data = "data"
prLexItem L_deriving = "deriving"
prLexItem L_via = "via"
prLexItem L_do = "do"
prLexItem L_else = "else"
prLexItem L_foreign = "foreign"
prLexItem L_if = "if"
prLexItem L_import = "import"
prLexItem L_in = "in"
prLexItem L_coherent = "coherent"
prLexItem L_incoherent = "incoherent"
prLexItem L_infix = "infix"
prLexItem L_infixl = "infixl"
prLexItem L_infixr = "infixr"
prLexItem L_interface = "interface"
prLexItem L_instance = "instance"
prLexItem L_let = "let"
prLexItem L_letseq = "letseq"
prLexItem L_package = "package"
prLexItem L_of = "of"
prLexItem L_primitive = "primitive"
prLexItem L_qualified = "qualified"
prLexItem L_rules = "rules"
prLexItem L_signature = "signature"
prLexItem L_struct = "struct"
prLexItem L_module = "module"
prLexItem L_then = "then"
prLexItem L_type = "type"
prLexItem L_valueOf = "valueOf"
prLexItem L_stringOf = "stringOf"
prLexItem L_verilog = "verilog"
prLexItem L_synthesize = "synthesize"
prLexItem L_when = "when"
prLexItem L_where = "where"
prLexItem L_dcolon = "::"
prLexItem L_colon = ":"
prLexItem L_eq = "="
prLexItem L_at = "@"
prLexItem L_lam = "\\"
prLexItem L_bar = "|"
prLexItem L_rarrow = "->"
prLexItem L_larrow = "<-"
prLexItem L_dot = "."
prLexItem L_comma = ","
prLexItem L_drarrow = "==>"
prLexItem L_irarrow = "=>"
prLexItem L_lcurl_o = "{ from layout"
prLexItem L_rcurl_o = "} from layout"
prLexItem L_semi_o = "; from layout"
prLexItem L_lpragma = "{-#"
prLexItem L_rpragma = "#-}"
prLexItem (L_unbasedUnsized False) = "'0"
prLexItem (L_unbasedUnsized True)  = "'1"
prLexItem L_eof = "<EOF>"
prLexItem (L_error s) = "Lexical error: " ++ show (convLexErrorToErrMsg s)

data Token = Token Position LexItem deriving (Eq)

instance Show Token where
    showsPrec _ (Token p l) = showString ("(Token " ++ prPosition p ++ " " ++ prLexItem l ++ ")")


data LFlags = LFlags {
    lf_is_stdlib :: Bool,   -- parsing a stdlib file, annotate positions
    lf_allow_sv_kws :: Bool -- allow SV keywords as identifiers
}

isSym :: Char -> Bool
isSym '!' = True; isSym '@' = True; isSym '#' = True; isSym '$' = True
isSym '%' = True; isSym '&' = True; isSym '*' = True; isSym '+' = True
isSym '.' = True; isSym '/' = True; isSym '<' = True; isSym '=' = True
isSym '>' = True; isSym '?' = True; isSym '\\' = True; isSym '^' = True
isSym '|' = True; isSym ':' = True; isSym '-' = True; isSym '~' = True
isSym ',' = True
isSym c | c >= '\x80' = c `elem` ['\162', '\163', '\164', '\165', '\166',
                                  '\167', '\168', '\169', '\170', '\171',
                                  '\172', '\173', '\174', '\175', '\176',
                                  '\177', '\178', '\179', '\180', '\181',
                                  '\183', '\184', '\185', '\186', '\187',
                                  '\188', '\189', '\190', '\191', '\215',
                                  '\247' ] || (isSymbol c && not (isIdChar c))
isSym _ = False

isIdChar :: Char -> Bool
isIdChar '\'' = True
isIdChar '_' = True
isIdChar c = isAlphaNum c

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
-- isSym is checked before isAlpha in the hand lexer).

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
alexInputPrevChar = internalError "Lex: alexInputPrevChar not used"

-- ---------------------------------------------------------------------------
-- Verbatim ports of the hand lexer's private helpers.

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

-- exact copy of the hand lexer's lexLitChar' (note: n undercounts simple escapes by
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
-- replicates the hand lexer's lexString column accounting (c+1 for the opening
-- quote, +n per char, +1 for the closing quote)
decodeStringLit :: String -> (String, Int)
decodeStringLit lexeme = go (drop 1 lexeme) 1 []
  where
    go ('"':_) !w acc = (reverse acc, w + 1)
    go s       !w acc = case lexLitChar' s of
                          Just (x, n, s') -> go s' (w + n) (x:acc)
                          Nothing -> internalError ("decodeStringLit: " ++ show lexeme)

-- `# <line> "<file>"` preprocessor line directive, recognized only at
-- column 0 (checked by the caller).  Replicates the hand lexer's lx exactly:
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

-- replicates the hand lexer's skipComm: nested brace-dash comments, only \n and \t
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

-- SystemVerilog keyword/symbol sets (as in the hand lexer) -----------------

isSvKeyword :: String -> Bool
isSvKeyword str = str `S.member` svKeywordSet

isSvSymbol :: String -> Bool
isSvSymbol str = str `S.member` svSymbolSet

svKeywordSet :: S.Set String
svKeywordSet = S.fromList [str | (_, str, _) <- svParserKeywordTable]

svSymbolSet :: S.Set String
svSymbolSet = S.fromList [str | (_, str, _) <- svSymbolTable]

-- ---------------------------------------------------------------------------
-- ASCII-identifier fast path.
--
-- In the generated DFA every identifier character costs a boxed
-- accept-array read, three transition-table reads, a UTF-8 iter, and --
-- because every identifier state is accepting -- a fresh AlexLastAcc and
-- Text record allocation.  Since identifiers dominate real source (over
-- half the bytes of the repo corpus sit in ASCII identifier-shaped runs),
-- the driver first scans for an all-ASCII identifier with one byte load
-- plus one immediate-bitmask test per character over the Text's
-- underlying ByteArray, allocating nothing until the token is emitted.
--
-- Semantics are exactly the DFA's for the tokens it handles:
--   * start char: ASCII [A-Za-z_]; continue: ASCII [A-Za-z0-9_'].
--   * If the byte terminating the run is >= 0x80 it could continue the
--     identifier with a non-ASCII idchar, so we BAIL to the DFA with
--     nothing consumed.  Bytes < 0x80 failing the mask are never idchars,
--     so the token is definitely complete.
--   * Reserved words are exact-match DFA rules (longest-match otherwise
--     prefers the longer identifier), so an exact-lexeme Map lookup
--     reproduces them, including the "package" column-(c-1) hack.
--   * Columns: n bytes = n chars = n columns (all ASCII).

-- result of scanning at a token start
data FastScan
  = FastNo                    -- not an all-ASCII identifier: use the DFA
  | FastId {-# UNPACK #-} !Int !T.Text
                              -- ^ n chars (== n bytes == n columns) matched;
                              --   the remaining input after the identifier

-- bitmasks over ASCII codes 0-63 / 64-127 for [A-Za-z0-9_'].
-- Literal constants so they inline into the scan loop as immediates:
--   lo: bit 39 = '; bits 48-57 = 0-9
--   hi: bits 1-26 = A-Z; bit 31 = _; bits 33-58 = a-z
idCharMaskLo, idCharMaskHi :: Word64
idCharMaskLo = 0x03FF008000000000
idCharMaskHi = 0x07FFFFFE87FFFFFE

-- the start set [A-Za-z_] is exactly the hi half (digits and ' are < 64)
idStartMaskHi :: Word64
idStartMaskHi = idCharMaskHi

maskMember :: Word64 -> Word8 -> Bool
maskMember m w = (m `unsafeShiftR` (fromIntegral w .&. 63)) .&. 1 /= 0
{-# INLINE maskMember #-}

isIdStartB :: Word8 -> Bool
isIdStartB w = w >= 64 && w < 128 && maskMember idStartMaskHi w
{-# INLINE isIdStartB #-}

isIdCharB :: Word8 -> Bool
isIdCharB w
  | w < 64    = maskMember idCharMaskLo w
  | w < 128   = maskMember idCharMaskHi w
  | otherwise = False
{-# INLINE isIdCharB #-}

-- scan an ASCII identifier at the head of the input (text-2.x: Text is
-- UTF-8 bytes in a ByteArray, so ASCII chars are single bytes and byte
-- offsets == char offsets == column widths within the run)
fastScanId :: T.Text -> FastScan
fastScanId (TI.Text arr off len)
  | len <= 0 || not (isIdStartB (TA.unsafeIndex arr off)) = FastNo
  | otherwise = loop (off + 1)
  where
    !end = off + len
    loop :: Int -> FastScan
    loop !i
      | i >= end  = FastId (i - off) (TI.Text arr i 0)
      | isIdCharB w = loop (i + 1)
      | w >= 0x80 = FastNo   -- may continue with a non-ASCII idchar: DFA decides
      | otherwise = FastId (i - off) (TI.Text arr i (end - i))
      where w = TA.unsafeIndex arr i
{-# INLINE fastScanId #-}

-- the lexeme (n ASCII chars at the head of the given Text) as a String,
-- lazily, for mkFString (same shape idTok's takeStr produces)
asciiStr :: T.Text -> Int -> String
asciiStr (TI.Text arr off _) n = go2 off
  where
    !end = off + n
    go2 !i | i >= end  = []
           | otherwise = chr (fromIntegral (TA.unsafeIndex arr i)) : go2 (i + 1)

-- exact-match keyword recognition on the scanned lexeme; Nothing => plain
-- identifier.  Keyword list == the reserved-word rules above (the caller
-- applies L_package's column hack).  The lookup is keyed on a zero-copy
-- Text slice and pre-filtered: no keyword starts with an uppercase letter,
-- and none is longer than 10 chars, so most identifiers skip the Map.
kwLookup :: T.Text -> Int -> Maybe LexItem
kwLookup (TI.Text arr off _) n
  | n > 10 = Nothing                      -- longest keyword: "incoherent"/"synthesize"
  | b0 /= 95 && (b0 < 97 || b0 > 122) = Nothing  -- keywords start [a-z_]
  | otherwise = M.lookup (TI.Text arr off n) kwMap
  where b0 = TA.unsafeIndex arr off
{-# INLINE kwLookup #-}

kwMap :: M.Map T.Text LexItem
kwMap = M.fromList
  [ (T.pack "_",          L_uscore)
  , (T.pack "action",     L_action)
  , (T.pack "case",       L_case)
  , (T.pack "class",      L_class)
  , (T.pack "data",       L_data)
  , (T.pack "deriving",   L_deriving)
  , (T.pack "do",         L_do)
  , (T.pack "else",       L_else)
  , (T.pack "foreign",    L_foreign)
  , (T.pack "if",         L_if)
  , (T.pack "import",     L_import)
  , (T.pack "in",         L_in)
  , (T.pack "coherent",   L_coherent)
  , (T.pack "incoherent", L_incoherent)
  , (T.pack "infix",      L_infix)
  , (T.pack "infixl",     L_infixl)
  , (T.pack "infixr",     L_infixr)
  , (T.pack "interface",  L_interface)
  , (T.pack "instance",   L_instance)
  , (T.pack "let",        L_let)
  , (T.pack "letseq",     L_letseq)
  , (T.pack "module",     L_module)
  , (T.pack "of",         L_of)
  , (T.pack "package",    L_package)
  , (T.pack "primitive",  L_primitive)
  , (T.pack "qualified",  L_qualified)
  , (T.pack "rules",      L_rules)
  , (T.pack "signature",  L_signature)
  , (T.pack "struct",     L_struct)
  , (T.pack "then",       L_then)
  , (T.pack "type",       L_type)
  , (T.pack "valueOf",    L_valueOf)
  , (T.pack "stringOf",   L_stringOf)
  , (T.pack "verilog",    L_verilog)
  , (T.pack "synthesize", L_synthesize)
  , (T.pack "when",       L_when)
  , (T.pack "where",      L_where)
  ]
{-# NOINLINE kwMap #-}

-- ---------------------------------------------------------------------------
-- Driver and actions.
-- Action args: flags, file, line, col (token start), stream at token start,
-- stream after token, token length in chars.

type Action = LFlags -> FString -> Int -> Int -> Stream -> Stream -> Int -> [Token]

lexStart :: LFlags -> FString -> Stream -> [Token]
lexStart lf f s = go lf f 1 0 s

-- start lexing at a given position (used for Classic text embedded in
-- BSV source); replicates the hand lexer, including its
-- rejection of unknown positions
lexStartWithPos :: LFlags -> Position -> Stream -> [Token]
lexStartWithPos lf pos s
  | getPositionLine pos == -1 || getPositionColumn pos == -1 =
      internalError "Lex.lexStartWithPos: unknown position"
  | otherwise =
      go lf (mkFString (getPositionFile pos)) (getPositionLine pos)
         (getPositionColumn pos) s

go :: LFlags -> FString -> Int -> Int -> Stream -> [Token]
go lf f !l !c s
  | c == 0, Just (fn, n, s') <- checkDirective s = go lf fn n 0 s'
  -- FAST PATH: all-ASCII identifier/keyword at the token start.  fastScanId
  -- byte-scans the Text's underlying array ([A-Za-z_][A-Za-z0-9_']*); it
  -- returns FastNo (nothing consumed) when the head is not an ASCII idstart
  -- OR when the run is terminated by a byte >= 0x80 (which could be a
  -- non-ASCII idchar continuation), so those cases take the DFA verbatim.
  | otherwise = case fastScanId s of
      FastId n s' -> fastIdTok lf f l c s n s'
      FastNo -> case alexScan s 0 of
        AlexEOF -> [Token (mkPositionFull f (l+1) (-1) (lf_is_stdlib lf)) L_eof]
        AlexError s' ->
          -- defensive; every byte is covered by some rule, so this is unreachable
          case unconsChar s' of
            Just (ch, _) -> lexErrTokens f l c (LexBadLexChar ch)
            Nothing -> internalError "Lex: scan error at EOF"
        AlexSkip s' _ -> go lf f l c s'
        AlexToken s' len act -> act lf f l c s s' len

-- emit the fast-scanned identifier: exact keyword lookup first (reserved
-- words are exact-match DFA rules, incl. the `package` column-(c-1) hack),
-- otherwise the conid/varid logic of idTok verbatim.  n is both the char
-- count and the column advance (all chars are single-byte ASCII).
fastIdTok :: LFlags -> FString -> Int -> Int -> Stream -> Int -> Stream -> [Token]
fastIdTok lf f !l !c s n s' =
  case kwLookup s n of
    Just L_package ->
      Token (mkPositionFull f l (c-1) (lf_is_stdlib lf)) L_package : go lf f l (c+n) s'
    Just li ->
      Token (mkPositionFull f l c (lf_is_stdlib lf)) li : go lf f l (c+n) s'
    Nothing ->
      let str = asciiStr s n
          p = mkPositionFull f l c (lf_is_stdlib lf)
          rest = go lf f l (c+n) s'
      in  if not (lf_allow_sv_kws lf) && isSvKeyword str
          then internalError ("SystemVerilog keyword forbidden: " ++ str)
          else if isUpper (head str)
               then Token p (L_conid (mkFString str)) : rest
               else Token p (L_varid (mkFString str)) : rest

-- error token stream, exactly the hand lexer's lexerr (infinite L_eof tail)
lexErrTokens :: FString -> Int -> Int -> LexError -> [Token]
lexErrTokens f l c err = map (Token (mkPosition f l c)) (L_error err : repeat L_eof)

-- fixed token whose column advance equals its character count
fixTok :: LexItem -> Action
fixTok li lf f l c _ s' len =
  Token (mkPositionFull f l c (lf_is_stdlib lf)) li : go lf f l (c+len) s'

-- A hack to allow multiple packages in one file: the layout pass needs
-- to generate a closing '}', so "package" is emitted at column c-1
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
-- (the lexeme has no closing quote; the hand lexer advanced by 1 + lexLitChar's
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
    Nothing -> internalError "Lex: badChar at EOF"
}
