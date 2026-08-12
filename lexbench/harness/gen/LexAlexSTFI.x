{
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-binds -fno-warn-missing-signatures -fno-warn-tabs #-}
-- Alex-generated Bluespec Classic lexer, strict Text, ASCII-identifier fast path, single-pass interning input.
-- Produces exactly the token stream of bsc's hand-written src/comp/Lex.hs
-- (same Token/LexItem constructors, same Positions, including its quirks).
module LexAlexSTFI(lexAlexStart) where

import Data.Char(isUpper)
import Data.Word(Word8)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Lex(Token(..), LexItem(..), LexError(..), LFlags(..))
import Position
import FStringCompat(FString, mkFString)
import PreStrings(fsEmpty)
import ErrorUtil(internalError)
import LexAlexShared
import LexAlexFastPath
import qualified Data.Text.Internal as TI
import qualified Data.Text.Array as TA
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

-- whitespace (Lex.hs:200-205; \r \v \f reset the column to 0)
\ +          { wsSpaces }
\n+          { wsNewlines }
\t+          { wsTabs }
[\r\v\f]+    { wsCRs }

-- line comments: "--" then any number of '-', then EOL, '@', or a non-symbol
-- char (Lex.hs:206-210 isComm); a comment without a trailing newline at EOF
-- is a LexMissingNL error (Lex.hs skipToEOL)
"--" \-* $commstart [^\n]* \n   { lineCommentNL }
"--" \-* \n                     { lineCommentNL }
"--" \-* $commstart [^\n]*      { lineCommentEOF }
"--" \-*                        { lineCommentEOF }

-- pragma brackets and nested block comments (Lex.hs:211-213)
"{-#"        { fixTok L_lpragma }
"#-}"        { fixTok L_rpragma }
"{-"         { blockComment }

-- single-char punctuation (Lex.hs:214-223); note '.' and ',' are matched
-- before the symbol rule, so they never *start* an operator run
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

-- integer and real literals (Lex.hs:236-280, lInteger, lReal);
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

-- task/system identifiers: $ followed by an identifier char (Lex.hs:281-286)
\$ $idchar [$idchar \$]*                     { dollarTok }

-- reserved operators (Lex.hs:294-303); listed before the generic symbol
-- rule so they win ties
"::"         { fixTok L_dcolon }
":"          { fixTok L_colon }
"="          { fixTok L_eq }
"@"          { fixTok L_at }
\\           { fixTok L_lam }
"->"         { fixTok L_rarrow }
"==>"        { fixTok L_drarrow }
"=>"         { fixTok L_irarrow }
"<-"         { fixTok L_larrow }

-- reserved words (Lex.hs:326-368); before the generic identifier rule
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
"synthesize" { fixTok L_synthesize }
"when"       { fixTok L_when }
"where"      { fixTok L_where }

-- generic operator runs and identifiers (Lex.hs:287-324)
$symstart $symall*                           { symTok }
$idstart $idchar*                            { idTok }

-- fallbacks: unterminated/bad literals, bad characters (Lex.hs lexerr)
\'           { badCharTok }
\"           { badStringTok }
.            { badChar }
{
type Stream = T.Text

-- The DFA input is just the character stream; file/line/column are threaded
-- through the driver loop as function arguments (like the hand lexer), so
-- stepping the input allocates no position wrapper.  The DFA sees one byte
-- per source *character*: ASCII bytes as themselves, non-ASCII characters
-- collapsed to classification pseudo-bytes 0xF1-0xF5 (LexAlexShared.classify).
-- Hence token lengths reported by alexScan are in characters.
type AlexInput = Stream

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte s = case unconsChar s of
  Nothing       -> Nothing
  Just (ch, s') -> Just (classify ch, s')
{-# INLINE alexGetByte #-}

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = internalError "LexAlex: alexInputPrevChar not used"

-- ---------------------------------------------------------------------------
-- Driver and actions (identical text for all input variants; the concrete
-- Stream type comes from the sed-substituted alias above).
-- Action args: flags, file, line, col (token start), stream at token start,
-- stream after token, token length in chars.

type Action = LFlags -> FString -> Int -> Int -> Stream -> Stream -> Int -> [Token]

lexAlexStart :: LFlags -> FString -> Stream -> [Token]
lexAlexStart lf f s = go lf f 1 0 s

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
            Nothing -> internalError "LexAlex: scan error at EOF"
        AlexSkip s' _ -> go lf f l c s'
        AlexToken s' len act -> act lf f l c s s' len

-- emit the fast-scanned identifier: exact keyword lookup first (reserved
-- words are exact-match DFA rules, incl. the `package` column-(c-1) hack),
-- otherwise the conid/varid logic of idTok verbatim.  n is both the char
-- count and the column advance (all chars are single-byte ASCII).
fastIdTok :: LFlags -> FString -> Int -> Int -> Stream -> Int -> Stream -> [Token]
fastIdTok lf f !l !c s@(TI.Text arr off _) n s' =
  case kwLookup s n of
    Just L_package ->
      Token (mkPositionFull f l (c-1) (lf_is_stdlib lf)) L_package : go lf f l (c+n) s'
    Just li ->
      Token (mkPositionFull f l c (lf_is_stdlib lf)) li : go lf f l (c+n) s'
    Nothing ->
      let p = mkPositionFull f l c (lf_is_stdlib lf)
          rest = go lf f l (c+n) s'
          b0 = TA.unsafeIndex arr off
      in  if not (lf_allow_sv_kws lf) && isSvKeyword (asciiStr s n)
          then internalError ("SystemVerilog keyword forbidden: " ++ asciiStr s n)
          else if b0 >= 65 && b0 <= 90    -- 'A'-'Z': conid
               then Token p (L_conid (internId s n)) : rest
               else Token p (L_varid (internId s n)) : rest

-- error token stream, exactly Lex.hs lexerr (infinite L_eof tail)
lexErrTokens :: FString -> Int -> Int -> LexError -> [Token]
lexErrTokens f l c err = map (Token (mkPosition f l c)) (L_error err : repeat L_eof)

-- fixed token whose column advance equals its character count
fixTok :: LexItem -> Action
fixTok li lf f l c _ s' len =
  Token (mkPositionFull f l c (lf_is_stdlib lf)) li : go lf f l (c+len) s'

-- "package" is emitted at column c-1 (multiple-packages hack, Lex.hs:350-355)
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
