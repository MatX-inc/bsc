-- | Read Tcl 8.6 syntax without running Tcl commands or sourcing files.
--
-- Commands and words retain both their source positions and raw spelling, so
-- Census can inventory scripts and Lower can report unsupported constructs at
-- their origin. A word also carries its decoded value when no substitution is
-- needed; nested command syntax can be recorded without executing it.
--
-- Parsing accepts more syntax than the planner can lower. The only evaluation
-- helper here resolves scalar substitutions through a caller-supplied lookup;
-- Lower owns the variable environment and decides which commands are allowed.
-- Arrays, command execution, and ambient Tcl state are not interpreted here.
-- Escape decoding follows Tcl 8.6, including its BMP Unicode escape result.
module Tcl
  ( SourcePos(..), TclError(..), WordKind(..), Word(..), Command(..), Script(..)
  , parseScript, parseScriptAt, parseListAt, staticWord, resolveScalarWord
  ) where

import Prelude hiding (Word)
import Data.Char (chr, digitToInt, isAlphaNum, isHexDigit)
import qualified Data.List as List

data SourcePos = SourcePos
  { sourceFile :: FilePath, sourceLine :: Int, sourceColumn :: Int
  , sourceOffset :: Int
  } deriving (Eq, Ord, Show)

data TclError = TclError
  { errorPosition :: SourcePos, errorMessage :: String
  } deriving (Eq, Show)

data WordKind = Bare | Braced | Quoted | Expanded deriving (Eq, Show)

data Word = Word
  { wordPosition :: SourcePos, wordBodyPosition :: SourcePos
  , wordKind :: WordKind
  , wordSource :: String -- ^ Exact source, including any delimiters.
  , wordText :: String -- ^ Source inside delimiters, before substitutions.
  , wordValue :: Maybe String -- ^ Only available when substitution is unnecessary.
  , wordSubstitutions :: [Script]
  } deriving (Eq, Show)

data Command = Command
  { commandPosition :: SourcePos, commandWords :: [Word]
  } deriving (Eq, Show)

newtype Script = Script { scriptCommands :: [Command] } deriving (Eq, Show)

data Input = Input { remaining :: String, position :: SourcePos }
data Mode = ScriptWords Bool | ListWords

staticWord :: Word -> Maybe String
staticWord = wordValue

-- | Resolve only scalar variable substitutions in an already parsed word.
-- Command substitution, array access, and argument expansion are deliberately
-- rejected. Escape decoding shares the lexical reader's Tcl 8.6 implementation.
resolveScalarWord :: (String -> Maybe String) -> Word -> Either TclError String
resolveScalarWord lookupVariable w
  | wordKind w == Expanded = Left (TclError (wordPosition w) "argument expansion is not supported")
  | Just value <- staticWord w = Right value
  | otherwise = go [] (Input (wordText w) (wordBodyPosition w))
  where
    go acc i = case remaining i of
      [] -> Right (concat (reverse acc))
      '[':_ -> failure i "command substitution is not supported"
      '\\':_ -> let (value, rest) = backslash i in go (value:acc) rest
      '$':'{':_ ->
        let start = advanceN 2 i
            name = takeWhile (/= '}') (remaining start)
            rest = advanceN (length name) start
        in case remaining rest of
          '}':_ -> variableValue acc i name (advance rest)
          _ -> failure i "missing closing brace in variable name"
      '$':_ ->
        let start = advance i
            name = takeWhile (\c -> isAlphaNum c || c == '_' || c == ':') (remaining start)
            rest = advanceN (length name) start
        in if null name then go ("$":acc) start else case remaining rest of
          '(' : _ -> failure i "array substitution is not supported"
          _ -> variableValue acc i name rest
      c:_ -> go ([c]:acc) (advance i)
    variableValue acc i name rest = case lookupVariable name of
      Nothing -> failure i ("unbound or unsupported scalar variable: " ++ name)
      Just value -> go (value:acc) rest

parseScript :: FilePath -> String -> Either TclError Script
parseScript path = parseScriptAt (SourcePos path 1 1 0)

parseScriptAt :: SourcePos -> String -> Either TclError Script
parseScriptAt p s = fst <$> script False (Input s p)

-- | Tcl list words do not perform command or variable substitution. This is
-- needed for the pattern/body list accepted by switch, not for evaluating it.
parseListAt :: SourcePos -> String -> Either TclError [Word]
parseListAt p s = go [] (Input s p)
  where
    go acc input = case skipListSpace input of
      i | null (remaining i) -> Right (reverse acc)
      i -> do
        (w, rest) <- word ListWords i
        go (w : acc) rest

advance :: Input -> Input
advance (Input [] p) = Input [] p
advance (Input (c:cs) p) = Input cs p
  { sourceLine = sourceLine p + if c == '\n' then 1 else 0
  , sourceColumn = if c == '\n' then 1 else sourceColumn p + 1
  , sourceOffset = sourceOffset p + 1
  }

advanceN :: Int -> Input -> Input
advanceN n i = iterate advance i !! n

between :: Input -> Input -> String
between a b = take (sourceOffset (position b) - sourceOffset (position a)) (remaining a)

failure :: Input -> String -> Either TclError a
failure i = Left . TclError (position i)

continuation :: Input -> Bool
continuation i = case remaining i of
  '\\':'\n':_ -> True
  _ -> False

-- Tcl's lexical whitespace is ASCII, not Haskell's Unicode isSpace class.
-- In particular NBSP and EM SPACE remain part of a script/list word.
tclSpace :: Char -> Bool
tclSpace c = c `elem` " \t\n\r\v\f"

skipListSpace :: Input -> Input
skipListSpace i
  | c:_ <- remaining i, tclSpace c = skipListSpace (advance i)
  | otherwise = i

skipSpace :: Bool -> Input -> Input
skipSpace newlines i
  | continuation i = skipSpace newlines (snd (backslash i))
  | c:_ <- remaining i, tclSpace c && (newlines || c /= '\n') =
      skipSpace newlines (advance i)
  | otherwise = i

skipStart :: Input -> Input
skipStart i = case skipSpace True i of
  j | ';':_ <- remaining j -> skipStart (advance j)
  j | '#':_ <- remaining j -> skipStart (comment (advance j))
  j -> j
  where
    comment j
      | continuation j = comment (snd (backslash j))
      | [] <- remaining j = j
      | '\n':_ <- remaining j = j
      | otherwise = comment (advance j)

script :: Bool -> Input -> Either TclError (Script, Input)
script nested = go []
  where
    go acc input = case skipStart input of
      i | null (remaining i) ->
            if nested then failure i "missing closing bracket" else Right (Script (reverse acc), i)
      i | nested, ']':_ <- remaining i -> Right (Script (reverse acc), i)
      i -> do
        (ws, rest) <- wordsInCommand [] i
        case ws of
          [] -> failure i "expected a command word"
          first:_ -> go (Command (wordPosition first) ws : acc) rest
    wordsInCommand acc input = case skipSpace False input of
      i | null (remaining i) -> Right (reverse acc, i)
      i | c:_ <- remaining i, c == '\n' || c == ';' -> Right (reverse acc, advance i)
      i | nested, ']':_ <- remaining i -> Right (reverse acc, i)
      i -> do
        (w, rest) <- word (ScriptWords nested) i
        wordsInCommand (w : acc) rest

delimiter :: Mode -> Input -> Bool
delimiter _ (Input [] _) = True
delimiter mode i@(Input (c:_) _) = tclSpace c || case mode of
  ListWords -> False
  ScriptWords nested -> continuation i || c == ';' || (nested && c == ']')

substitutes :: Mode -> Bool
substitutes ListWords = False
substitutes _ = True

word :: Mode -> Input -> Either TclError (Word, Input)
word mode start
  | substitutes mode, '{':'*':'}':_ <- remaining start
  , let rest = advanceN 3 start, not (delimiter mode rest) = do
      (w, end) <- word mode rest
      Right (w { wordPosition = position start, wordKind = Expanded
               , wordSource = between start end, wordValue = Nothing }, end)
  | '{':_ <- remaining start = do
      let body = advance start
      (value, close) <- braces 1 [] body
      let end = advance close
      requireEnd mode end
      Right (Word (position start) (position body) Braced (between start end)
                  (between body close) (Just value) [], end)
  | '"':_ <- remaining start = do
      let body = advance start
      (value, dynamic, subs, close) <- scan True [] False [] body
      let end = advance close
      requireEnd mode end
      Right (Word (position start) (position body) Quoted (between start end)
                  (between body close) (if dynamic then Nothing else Just value) subs, end)
  | otherwise = do
      (value, dynamic, subs, end) <- scan False [] False [] start
      Right (Word (position start) (position start) Bare (between start end)
                  (between start end) (if dynamic then Nothing else Just value) subs, end)
  where
    braces depth acc i = case remaining i of
      [] -> failure start "missing closing brace"
      '}' : _ | depth == (1 :: Int) -> Right (concat (reverse acc), i)
      '}' : _ -> braces (depth - 1) ("}" : acc) (advance i)
      '{' : _ -> braces (depth + 1) ("{" : acc) (advance i)
      '\\' : _ | substitutes mode && continuation i ->
        let (v, rest) = backslash i in braces depth (v : acc) rest
      '\\' : _ ->
        let rest = advanceN (min 2 (length (take 2 (remaining i)))) i
        in braces depth (between i rest : acc) rest
      c : _ -> braces depth ([c] : acc) (advance i)
    scan quoted acc dynamic subs i = case remaining i of
      [] | quoted -> failure start "missing closing quote"
      [] -> done acc dynamic subs i
      '"':_ | quoted -> done acc dynamic subs i
      _ | not quoted && delimiter mode i -> done acc dynamic subs i
      '\\':_ -> let (v, rest) = backslash i in scan quoted (v:acc) dynamic subs rest
      '[':_ | substitutes mode -> do
        (nested, close) <- script True (advance i)
        scan quoted acc True (nested:subs) (advance close)
      '$':_ | substitutes mode -> do
        (isVariable, nested, rest) <- variable i
        scan quoted (between i rest:acc) (dynamic || isVariable) (reverse nested ++ subs) rest
      c:_ -> scan quoted ([c]:acc) dynamic subs (advance i)
    done acc dynamic subs i = Right (concat (reverse acc), dynamic, reverse subs, i)

requireEnd :: Mode -> Input -> Either TclError ()
requireEnd mode i
  | delimiter mode i = Right ()
  | otherwise = failure i "extra characters after close brace or quote"

-- Variable syntax is consumed without resolving it. In particular, spaces or
-- brackets inside ${...} are name characters, not separators/substitutions.
variable :: Input -> Either TclError (Bool, [Script], Input)
variable start = case remaining (advance start) of
  '{':_ -> bracedName (advanceN 2 start)
  _ -> let nameStart = advance start
           name = takeWhile (\c -> isAlphaNum c || c == '_' || c == ':') (remaining nameStart)
           rest = advanceN (length name) nameStart
       in if null name then Right (False, [], nameStart)
          else case remaining rest of
            '(' : _ -> do
              (subs, end) <- index [] (advance rest)
              Right (True, subs, end)
            _ -> Right (True, [], rest)
  where
    bracedName i = case remaining i of
      [] -> failure start "missing closing brace in variable name"
      '}':_ -> Right (True, [], advance i)
      _ -> bracedName (advance i)
    index subs i = case remaining i of
      [] -> failure start "missing closing parenthesis in array index"
      ')':_ -> Right (reverse subs, advance i)
      '\\':_ -> index subs (snd (backslash i))
      '[':_ -> do
        (nested, close) <- script True (advance i)
        index (nested:subs) (advance close)
      '$':_ -> do
        (_, nested, rest) <- variable i
        index (reverse nested ++ subs) rest
      _ -> index subs (advance i)

backslash :: Input -> (String, Input)
backslash start = case remaining (advance start) of
  [] -> ("\\", advance start)
  '\n':_ -> (" ", dropIndent (advanceN 2 start))
  c:_ | Just decoded <- lookup c [('a','\a'), ('b','\b'), ('f','\f'), ('n','\n')
                                ,('r','\r'), ('t','\t'), ('v','\v')] ->
          ([decoded], advanceN 2 start)
  'x':_ -> number 16 2 isHexDigit (advanceN 2 start) "x"
  'u':_ -> number 16 4 isHexDigit (advanceN 2 start) "u"
  'U':_ -> number 16 8 isHexDigit (advanceN 2 start) "U"
  c:_ | c >= '0' && c <= '7' ->
          number 8 (if c <= '3' then 3 else 2) (\d -> d >= '0' && d <= '7') (advance start) ""
  c:_ -> ([c], advanceN 2 start)
  where
    dropIndent i = case remaining i of
      c:_ | c == ' ' || c == '\t' -> dropIndent (advance i)
      _ -> i
    number :: Int -> Int -> (Char -> Bool) -> Input -> String -> (String, Input)
    number base count valid i fallback =
      let ds = digits 0 count (remaining i)
          n = List.foldl' (\a d -> a * base + digitToInt d) 0 ds
          value = if null ds then fallback else [chr (if n > 0xffff then 0xfffd else n)]
      in (value, advanceN (length ds) i)
      where
        -- Tcl stops BEFORE the digit that would exceed the Unicode ceiling,
        -- and standard 8.6 then replaces a non-BMP value with U+FFFD.
        digits _ 0 _ = []
        digits acc left (d:rest)
          | valid d, let next = acc * base + digitToInt d, next <= 0x10ffff =
              d : digits next (left - 1) rest
        digits _ _ _ = []
