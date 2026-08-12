{-# LANGUAGE BangPatterns #-}
-- ASCII-identifier fast path for the strict-Text Alex lexer (LexAlexSTF).
--
-- Rationale (see RESULTS.md asm section): in the generated DFA, *every*
-- identifier character costs one boxed accept-array read, ~3 unboxed table
-- reads (alex_base/alex_check/alex_table), a UTF-8 iter (clz8#-based), and --
-- because every identifier state is accepting -- a fresh AlexLastAcc + Text
-- record allocation.  This module scans an ASCII identifier with one byte
-- load + one bitmask test per character over the Text's underlying
-- ByteArray, with zero allocation until the token is emitted.
--
-- Semantics: mirrors Lex.hs exactly for the tokens it handles.
--   * start char: ASCII [A-Za-z_]  (Lex.hs: isAlpha x || x == '_')
--   * continue:   ASCII [A-Za-z0-9_']  (Lex.hs isIdChar restricted to ASCII:
--                 isAlphaNum c || c == '_' || c == '\'')
--   * If the byte that terminates the run is >= 0x80 the identifier may
--     continue with a non-ASCII idchar (or be followed by any non-ASCII
--     char); we BAIL to the generic DFA with nothing consumed, so behavior
--     is byte-for-byte the DFA's.  Bytes < 0x80 that fail the bitmask are
--     never idchars, so the token is definitely complete.
--   * Keywords ("do", "package", ..., "_") are DFA rules that win ties with
--     the identifier rule only on exact match (longest-match otherwise
--     prefers the longer identifier), so an exact-lexeme map lookup
--     reproduces them.  The lookup is keyed on a zero-copy Text slice and
--     pre-filtered: no keyword starts with an uppercase letter, and none is
--     longer than 10 chars, so most identifiers skip the Map entirely.
module LexAlexFastPath(FastScan(..), fastScanId, kwLookup, asciiStr, internId,
                       WsScan(..), wsScan) where

import qualified Data.Text as T
import qualified Data.Text.Internal as TI
import qualified Data.Text.Array as TA
import qualified Data.Map.Strict as M
import Data.Bits(unsafeShiftR, (.&.))
import Data.Word(Word8, Word64)
import Data.Char(chr)
import System.IO.Unsafe(unsafePerformIO)

import IOMutVar(MutableVar, newVar, readVar, writeVar)
import FStringCompat(FString, mkFString)
import Lex(LexItem(..))

-- result of scanning at a token start
data FastScan
  = FastNo                    -- not an all-ASCII identifier: use the DFA
  | FastId {-# UNPACK #-} !Int !T.Text
                              -- ^ n chars (== n bytes == n columns) matched;
                              --   the remaining input after the identifier

-- bitmasks over ASCII codes 0-63 / 64-127 for [A-Za-z0-9_'].
-- Literal constants so they inline into the scan loop as immediates
-- (a foldr-built CAF version compiled to a thunk dereference per test):
--   lo: bit 39 = '\''; bits 48-57 = '0'-'9'
--   hi: bits 1-26 = 'A'-'Z'; bit 31 = '_'; bits 33-58 = 'a'-'z'
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
-- lazily, for mkFString (same shape idTok's takeStr produced)
asciiStr :: T.Text -> Int -> String
asciiStr (TI.Text arr off _) n = go off
  where
    !end = off + n
    go !i | i >= end  = []
          | otherwise = chr (fromIntegral (TA.unsafeIndex arr i)) : go (i + 1)

-- exact-match keyword recognition on the scanned lexeme; Nothing => plain
-- identifier.  Keyword list == the reserved-word rules in rules.part ==
-- Lex.hs:326-368 (L_package's column hack is applied by the caller).
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
-- Single-pass interning: a Text-slice-keyed cache in FRONT of mkFString.
--
-- mkFString (SpeedyString.fromString) rebuilds a String from the lexeme,
-- hashes it char-by-char, and walks an IntMap bucket -- for every
-- occurrence of every identifier.  This cache maps the (zero-copy) Text
-- slice of an already-scanned ASCII identifier directly to its FString.
--
-- FString semantics are preserved exactly: SpeedyString assigns unique ids
-- in first-intern order (and Eq/Ord compare ids), so all that matters is
-- that cache misses call mkFString in the same sequence the uncached lexer
-- would -- they do, since the first occurrence of every identifier is a
-- miss that calls mkFString at exactly the point the uncached code would,
-- and hits return the FString that call produced.  The cache key is
-- T.copy'd on insert so it doesn't retain the source file's buffer.
-- Same benign unsafePerformIO/MutableVar pattern as SpeedyString itself.

idCache :: MutableVar (M.Map T.Text FString)
idCache = unsafePerformIO $ newVar M.empty
{-# NOINLINE idCache #-}

-- n ASCII chars at the head of s (the fast-scanned identifier): its FString
internId :: T.Text -> Int -> FString
internId s@(TI.Text arr off _) n = unsafePerformIO $ do
  m <- readVar idCache
  let key = TI.Text arr off n
  case M.lookup key m of
    Just fs -> return fs
    Nothing -> do
      let !fs = mkFString (asciiStr s n)
          !key2 = T.copy key
          !m2 = M.insert key2 fs m
      writeVar idCache m2
      return fs
{-# NOINLINE internId #-}

-- ---------------------------------------------------------------------------
-- Whitespace and `--` line-comment run-scan.
--
-- Whitespace and line comments are, like identifiers, byte-scannable: no
-- byte of a UTF-8 multi-byte sequence equals 0x0A, so "scan to newline" over
-- bytes is character-correct, and all the column arithmetic involves ASCII
-- (single-byte) characters only.  wsScan consumes, per call, one maximal
-- whitespace run or one complete `--` comment (through its newline),
-- tracking line/column exactly like the hand lexer:
--   space: c+1;  tab: nextTab (c+1);  \n: (l+1, 0);  \r \v \f: (l, 0).
--
-- `--` only starts a comment when, after all consecutive dashes, the next
-- char is EOL/EOF, '@', or NOT a symbol char (the hand lexer's isComm
-- quirk) -- otherwise the whole run is one operator token (e.g. `-->`),
-- which is left to the DFA (WsNone, nothing consumed).  A non-ASCII byte
-- after the dashes could be a non-ASCII symbol char, so that also bails
-- to the DFA, which classifies it with the full isSym.
--
-- `# <line> "<file>"` directives are recognized by the driver only at
-- column 0, so whenever a scanned byte resets the column to 0 (newline,
-- CR, VT, FF -- including a comment's terminating newline) and the next
-- byte is '#', wsScan stops and returns, letting the driver's directive
-- check run before anything else is consumed.
--
-- A comment (or bare `--`/`---...`) that reaches EOF without a newline is
-- the hand lexer's LexMissingNL error, reported at column 0 of its line.

data WsScan
  = WsNone                                -- nothing consumed: not ws/comment
  | WsSome !Int !Int !T.Text              -- new line, column, rest
  | WsMissingNL !Int                      -- comment hit EOF; error line

-- ASCII bitmask for the hand lexer's isSym restricted to ASCII:
--   ! @ # $ % & * + . / < = > ? \ ^ | : - ~ ,
symMaskLo, symMaskHi :: Word64
symMaskLo = 0xF400FC7A00000000
symMaskHi = 0x5000000050000001

isSymAsciiB :: Word8 -> Bool
isSymAsciiB w
  | w < 64    = maskMember symMaskLo w
  | w < 128   = maskMember symMaskHi w
  | otherwise = False
{-# INLINE isSymAsciiB #-}

nextTabB :: Int -> Int
nextTabB c = ((c + 8 - 1) `div` 8) * 8
{-# INLINE nextTabB #-}

wsScan :: Int -> Int -> T.Text -> WsScan
wsScan !l0 !c0 (TI.Text arr off len)
  | len <= 0 = WsNone
  | otherwise = start (TA.unsafeIndex arr off)
  where
    !end = off + len
    byte :: Int -> Word8
    byte i = TA.unsafeIndex arr i
    {-# INLINE byte #-}

    start :: Word8 -> WsScan
    start 32 = wsLoop l0 (c0+1) (off+1)               -- ' '
    start  9 = wsLoop l0 (nextTabB (c0+1)) (off+1)    -- \t
    start 10 = afterZero (l0+1) (off+1)               -- \n
    start 13 = afterZero l0 (off+1)                   -- \r
    start 11 = afterZero l0 (off+1)                   -- \v
    start 12 = afterZero l0 (off+1)                   -- \f
    start 45 | off+1 < end, byte (off+1) == 45 = dashes (off+2)  -- "--"
    start _  = WsNone

    -- generic whitespace loop; i = next byte to examine
    wsLoop :: Int -> Int -> Int -> WsScan
    wsLoop !l !c !i
      | i >= end = WsSome l c (TI.Text arr i 0)
      | otherwise = case byte i of
          32 -> wsLoop l (c+1) (i+1)
          9  -> wsLoop l (nextTabB (c+1)) (i+1)
          10 -> afterZero (l+1) (i+1)
          13 -> afterZero l (i+1)
          11 -> afterZero l (i+1)
          12 -> afterZero l (i+1)
          _  -> WsSome l c (TI.Text arr i (end - i))

    -- the column just reset to 0: stop if '#' follows (possible directive)
    afterZero :: Int -> Int -> WsScan
    afterZero !l !i
      | i < end, byte i == 35 = WsSome l 0 (TI.Text arr i (end - i))
      | otherwise = wsLoop l 0 i

    -- inside the leading dash run of a potential comment
    dashes :: Int -> WsScan
    dashes !j
      | j >= end = WsMissingNL l0            -- "--" then EOF: comment, no NL
      | b == 45 = dashes (j+1)
      | b >= 0x80 = WsNone                   -- could be a non-ASCII symbol: DFA
      | b /= 64 && isSymAsciiB b = WsNone    -- longer operator (not '@'): DFA
      | otherwise = eol j                    -- comment: scan to end of line
      where b = byte j

    -- consume the comment body through its newline
    eol :: Int -> WsScan
    eol !j
      | j >= end = WsMissingNL l0
      | byte j == 10 = afterZero (l0+1) (j+1)
      | otherwise = eol (j+1)
