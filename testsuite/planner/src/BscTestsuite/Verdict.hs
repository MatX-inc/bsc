-- | Strict import and comparison for the legacy DejaGNU result identity.
-- This is deliberately not the planner's eventual semantic check identity.
module BscTestsuite.Verdict
  ( Disposition(..), CheckId(..), Verdict(..), DiagnosticKind(..), Diagnostic(..)
  , Manifest(..), Difference(..)
  , parseSummary, mergeManifests, validateManifest, validateDiscovery
  , compareManifests, dispositionCounts, encodeManifest, decodeManifest
  ) where

import Control.Monad (foldM)
import Data.Char (chr, digitToInt, isControl, isHexDigit, isSpace, isUpper, ord)
import Data.List (dropWhileEnd, intercalate, isInfixOf, isPrefixOf, isSuffixOf, mapAccumL, sort)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Numeric (showHex)
import System.FilePath (isAbsolute, makeRelative, normalise, splitDirectories, takeDirectory, takeExtension, (</>))
import Text.ParserCombinators.ReadP
  ( ReadP, char, count, eof, get, look, munch, pfail
  , readP_to_S, satisfy, string
  )

data Disposition = PASS | FAIL | XFAIL | XPASS | KFAIL | KPASS
                 | UNRESOLVED | UNTESTED | UNSUPPORTED
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | The occurrence is one-based among identical labels in the same test.
-- Configuration belongs to the manifest, not to this identity.
data CheckId = CheckId
  { checkTest :: FilePath
  , checkLabel :: String
  , checkOccurrence :: Int
  } deriving (Eq, Ord, Show)

data Verdict = Verdict
  { verdictId :: CheckId
  , verdictDisposition :: Disposition
  } deriving (Eq, Ord, Show)

data DiagnosticKind = Error | Warning | Note deriving (Eq, Ord, Show)

data Diagnostic = Diagnostic
  { diagnosticTest :: Maybe FilePath
  , diagnosticKind :: DiagnosticKind
  , diagnosticMessage :: String
  } deriving (Eq, Ord, Show)

data Manifest = Manifest
  { manifestConfiguration :: String
  , manifestTests :: [FilePath]
  , manifestVerdicts :: [Verdict]
  , manifestDiagnostics :: [Diagnostic]
  } deriving (Eq, Show)

data Difference
  = MissingTest FilePath
  | AddedTest FilePath
  | MissingCheck CheckId Disposition
  | AddedCheck CheckId Disposition
  | ChangedDisposition CheckId Disposition Disposition
  | MissingDiagnostic Diagnostic
  | AddedDiagnostic Diagnostic
  deriving (Eq, Ord, Show)

allDispositions :: [Disposition]
allDispositions = [minBound .. maxBound]

dispositionNames :: [(String, Disposition)]
dispositionNames = [(show d, d) | d <- allDispositions]

summaryNames :: [(String, Disposition)]
summaryNames =
  [ ("expected passes", PASS), ("unexpected failures", FAIL)
  , ("expected failures", XFAIL), ("unexpected successes", XPASS)
  , ("known failures", KFAIL), ("unknown successes", KPASS)
  , ("unresolved testcases", UNRESOLVED), ("untested testcases", UNTESTED)
  , ("unsupported tests", UNSUPPORTED)
  ]

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

ensure :: Bool -> String -> Either String ()
ensure True _ = Right ()
ensure False message = Left message

-- Lexical normalization avoids resolving the .exp symlinks used by long tests.
-- FilePath.normalise intentionally does not remove '..', so handle it here.
absoluteLexical :: FilePath -> FilePath
absoluteLexical = foldl step "/" . splitDirectories . normalise
  where
    step acc "/" = acc
    step acc "." = acc
    step acc ".." = takeDirectory acc
    step acc component = acc </> component

validTestPath :: FilePath -> Either String ()
validTestPath path = do
  ensure (not (null path) && not (isAbsolute path)) ("test path must be suite-relative: " ++ show path)
  ensure (normalise path == path && all (`notElem` [".", ".."]) (splitDirectories path))
    ("test path is not canonical: " ++ show path)
  ensure (takeExtension path == ".exp") ("test path does not name an .exp file: " ++ show path)
  ensure (not (any isControl path)) ("test path contains control characters: " ++ show path)

normalizeTest :: FilePath -> FilePath -> FilePath -> Either String FilePath
normalizeTest suiteRoot summaryPath testPath = do
  ensure (isAbsolute suiteRoot && isAbsolute summaryPath)
    "suite root and original summary path must be absolute"
  ensure (not (null testPath)) "empty Running .exp path"
  let root = absoluteLexical suiteRoot
      full = absoluteLexical (if isAbsolute testPath then testPath else takeDirectory summaryPath </> testPath)
      relative = makeRelative root full
  validTestPath relative
  -- makeRelative returns its absolute argument when it is outside the root.
  ensure (not (isAbsolute relative)) ("test is outside suite root: " ++ testPath)
  pure relative

data Pending
  = PendingVerdict FilePath Disposition [String]
  | PendingDiagnostic (Maybe FilePath) DiagnosticKind [String]

data SummaryState = SummaryState
  { stateCurrent :: Maybe FilePath
  , stateTests :: [FilePath]
  , stateRawVerdicts :: [(FilePath, String, Disposition)]
  , stateDiagnostics :: [Diagnostic]
  , statePending :: Maybe Pending
  , stateSummary :: Bool
  , stateTotals :: Map.Map Disposition Int
  }

initialState :: SummaryState
initialState = SummaryState Nothing [] [] [] Nothing False Map.empty

-- Blank separator lines are emitted before the summary. Internal blank lines
-- remain part of a message; terminal blank separators do not.
messageText :: [String] -> String
messageText = intercalate "\n" . dropWhileEnd null . reverse

flushPending :: SummaryState -> SummaryState
flushPending state = case statePending state of
  Nothing -> state
  Just (PendingVerdict test disposition parts) -> state
    { statePending = Nothing
    , stateRawVerdicts = (test, messageText parts, disposition) : stateRawVerdicts state
    }
  Just (PendingDiagnostic test kind parts) -> state
    { statePending = Nothing
    , stateDiagnostics = Diagnostic test kind (messageText parts) : stateDiagnostics state
    }

continueMessage :: String -> Pending -> Pending
continueMessage line (PendingVerdict test disposition parts) = PendingVerdict test disposition (line : parts)
continueMessage line (PendingDiagnostic test kind parts) = PendingDiagnostic test kind (line : parts)

resultLine :: String -> Maybe (String, String)
resultLine line = case span (\c -> isUpper c || c == '_') line of
  ([], _) -> Nothing
  (name, ':' : ' ' : label) -> Just (name, label)
  (name, ":") -> Just (name, "")
  (name, ':' : label) -> Just (name, label)
  _ -> Nothing

runningTest :: String -> Maybe FilePath
runningTest line
  | "Running " `isPrefixOf` line && " ..." `isSuffixOf` line =
      let path = take (length line - length "Running " - length " ...") (drop (length "Running ") line)
      in if takeExtension path == ".exp" then Just path else Nothing
  | otherwise = Nothing

isSummary :: String -> Bool
isSummary line = "===" `isPrefixOf` trim line && "===" `isSuffixOf` trim line
              && " Summary" `isInfixOf` line

readPositive :: String -> Either String Int
readPositive value = do
  ensure (not (null value) && all asciiDigit value) ("invalid positive integer: " ++ show value)
  let number = read value :: Integer
  ensure (number > 0 && number <= toInteger (maxBound :: Int)) ("integer out of range: " ++ value)
  pure (fromInteger number)

parseTotal :: String -> Either String (Disposition, Int)
parseTotal line = case reverse (words line) of
  number : reversedName -> do
    let name = unwords (reverse reversedName)
    disposition <- maybe (Left ("unknown summary counter: " ++ name)) Right (lookup (drop (length "# of ") name) summaryNames)
    ensure ("# of " `isPrefixOf` name) ("invalid summary counter: " ++ line)
    total <- readPositive number
    pure (disposition, total)
  _ -> Left ("invalid summary counter: " ++ line)

-- | Parse one original per-process .sum. The second and third arguments must
-- be absolute: suite root and the summary's ORIGINAL path, before archiving.
-- This format supports one target/configuration per summary, as fullparallel
-- produces. Multi-target DejaGNU logs must be split before import.
parseSummary :: String -> FilePath -> FilePath -> String -> Either String Manifest
parseSummary configuration suiteRoot summaryPath contents = do
  ensure (isAbsolute suiteRoot && isAbsolute summaryPath)
    "suite root and original summary path must be absolute"
  ensure (not (null contents)) "empty DejaGNU summary"
  ensure (last contents == '\n') "DejaGNU summary has an incomplete final line"
  final <- flushPending <$> foldM parseLine initialState (zip [1 :: Int ..] (map stripCR (lines contents)))
  ensure (stateSummary final) "DejaGNU summary is incomplete: no final Summary section"
  ensure (not (null (stateTests final))) "DejaGNU summary contains no Running .exp discoveries"
  let records = reverse (stateRawVerdicts final)
      actual = Map.fromListWith (+) [(disposition, 1 :: Int) | (_, _, disposition) <- records]
  ensure (actual == stateTotals final)
    ("DejaGNU footer counts do not match result records: records=" ++ show (Map.toAscList actual)
     ++ ", footer=" ++ show (Map.toAscList (stateTotals final)))
  let manifest = Manifest configuration (reverse (stateTests final)) (assignOccurrences records) (reverse (stateDiagnostics final))
  validateManifest manifest
  pure manifest
  where
    stripCR = dropWhileEnd (== '\r')
    parseLine state (lineNumber, line) =
      case step state line of
        Left problem -> Left (summaryPath ++ ":" ++ show lineNumber ++ ": " ++ problem)
        Right next -> Right next
    step state line
      | Just path <- runningTest line = do
          ensure (not (stateSummary state)) "Running .exp appeared after final summary"
          test <- normalizeTest suiteRoot summaryPath path
          ensure (test `notElem` stateTests state) ("duplicate Running .exp discovery: " ++ test)
          let flushed = flushPending state
          pure flushed { stateCurrent = Just test, stateTests = test : stateTests flushed }
      | isSummary line = do
          ensure (not (stateSummary state)) "multiple Summary sections are unsupported; use one configuration per import"
          ensure (not (" Summary for " `isInfixOf` line)) "per-target Summary is unsupported; use one configuration per import"
          pure (flushPending state) { stateSummary = True }
      | Just (category, label) <- resultLine line =
          case lookup category dispositionNames of
            Just disposition -> do
              ensure (not (stateSummary state)) "result appeared after final summary"
              test <- maybe (Left "result appeared before a Running .exp discovery") Right (stateCurrent state)
              pure (flushPending state) { statePending = Just (PendingVerdict test disposition [label]) }
            Nothing -> case lookup category [("ERROR", Error), ("WARNING", Warning), ("NOTE", Note)] of
              Just kind -> pure (flushPending state)
                { statePending = Just (PendingDiagnostic (stateCurrent state) kind [label]) }
              Nothing -> Left ("unknown result/diagnostic category: " ++ category)
      | "# of " `isPrefixOf` trim line = do
          ensure (stateSummary state) "summary counter appeared before Summary section"
          (disposition, total) <- parseTotal (trim line)
          ensure (not (Map.member disposition (stateTotals state))) ("duplicate summary counter: " ++ show disposition)
          pure (flushPending state) { stateTotals = Map.insert disposition total (stateTotals state) }
      | "Running " `isPrefixOf` line && ".exp" `isInfixOf` line = Left ("malformed Running .exp discovery: " ++ line)
      | Just pending <- statePending state =
          pure state { statePending = Just (continueMessage line pending) }
      | null (trim line) = Right state
      | stateSummary state = Left ("unrecognized text after final summary: " ++ line)
      | stateCurrent state /= Nothing = Left ("unframed text inside test results: " ++ line)
      | otherwise = Right state -- Native configuration and framework preamble.

assignOccurrences :: [(FilePath, String, Disposition)] -> [Verdict]
assignOccurrences = snd . mapAccumL step Map.empty
  where
    -- Traverse in original order, without a global ordinal: another label
    -- being inserted must not renumber this label's occurrences.
    step counts (test, label, disposition) =
      let key = (test, label)
          occurrence = Map.findWithDefault 0 key counts + 1
      in (Map.insert key occurrence counts, Verdict (CheckId test label occurrence) disposition)

validateManifest :: Manifest -> Either String ()
validateManifest manifest = do
  ensure (not (null (trim (manifestConfiguration manifest)))) "manifest configuration must not be empty"
  ensure (not (null (manifestTests manifest))) "manifest must discover at least one .exp test"
  mapM_ validTestPath (manifestTests manifest)
  unique "discovered test" (manifestTests manifest)
  unique "check ID" (map verdictId (manifestVerdicts manifest))
  let tests = Set.fromList (manifestTests manifest)
  mapM_ (checkVerdict tests) (manifestVerdicts manifest)
  mapM_ (checkDiagnostic tests) (manifestDiagnostics manifest)
  where
    checkVerdict tests verdict = do
      let identifier = verdictId verdict
      validTestPath (checkTest identifier)
      ensure (Set.member (checkTest identifier) tests) ("check refers to undiscovered test: " ++ show identifier)
      ensure (checkOccurrence identifier > 0) ("check occurrence must be positive: " ++ show identifier)
    checkDiagnostic tests diagnostic = case diagnosticTest diagnostic of
      Nothing -> Right ()
      Just path -> ensure (Set.member path tests) ("diagnostic refers to undiscovered test: " ++ path)

unique :: (Ord a, Show a) => String -> [a] -> Either String ()
unique description values = case Map.keys (Map.filter (> (1 :: Int)) (Map.fromListWith (+) [(value, 1) | value <- values])) of
  [] -> Right ()
  duplicates -> Left ("duplicate " ++ description ++ ": " ++ show duplicates)

-- | Merge per-directory summaries, rejecting overlaps instead of double
-- counting archived and current summaries of the same test.
mergeManifests :: [Manifest] -> Either String Manifest
mergeManifests [] = Left "no DejaGNU summaries supplied"
mergeManifests manifests@(first : _) = do
  mapM_ validateManifest manifests
  ensure (all ((== manifestConfiguration first) . manifestConfiguration) manifests)
    "cannot merge different configurations"
  let combined = Manifest (manifestConfiguration first)
        (concatMap manifestTests manifests) (concatMap manifestVerdicts manifests) (concatMap manifestDiagnostics manifests)
  validateManifest combined
  pure combined

-- | Compare against an independently discovered, configuration-specific test
-- list. Two lanes dropping the same test must not qualify as parity.
validateDiscovery :: [FilePath] -> Manifest -> Either String ()
validateDiscovery expected manifest = do
  validateManifest manifest
  ensure (not (null expected)) "expected test discovery manifest is empty"
  mapM_ validTestPath expected
  unique "expected test" expected
  let wanted = Set.fromList expected
      actual = Set.fromList (manifestTests manifest)
      missing = Set.toAscList (wanted Set.\\ actual)
      extra = Set.toAscList (actual Set.\\ wanted)
  ensure (null missing && null extra) ("test discovery mismatch: missing=" ++ show missing ++ ", extra=" ++ show extra)

-- | Left means malformed/incompatible inputs; Right [] means populations,
-- dispositions, and diagnostics match. Matching FAILs are still FAILs:
-- orchestration must independently enforce a clean baseline.
compareManifests :: Manifest -> Manifest -> Either String [Difference]
compareManifests baseline candidate = do
  validateManifest baseline
  validateManifest candidate
  ensure (manifestConfiguration baseline == manifestConfiguration candidate)
    ("configuration mismatch: " ++ show (manifestConfiguration baseline) ++ " /= " ++ show (manifestConfiguration candidate))
  let oldTests = Set.fromList (manifestTests baseline)
      newTests = Set.fromList (manifestTests candidate)
      old = Map.fromList [(verdictId v, verdictDisposition v) | v <- manifestVerdicts baseline]
      new = Map.fromList [(verdictId v, verdictDisposition v) | v <- manifestVerdicts candidate]
      oldDiagnostics = diagnosticCounts baseline
      newDiagnostics = diagnosticCounts candidate
  pure $
    map MissingTest (Set.toAscList (oldTests Set.\\ newTests)) ++
    map AddedTest (Set.toAscList (newTests Set.\\ oldTests)) ++
    [MissingCheck identifier disposition | (identifier, disposition) <- Map.toAscList (old Map.\\ new)] ++
    [AddedCheck identifier disposition | (identifier, disposition) <- Map.toAscList (new Map.\\ old)] ++
    [ChangedDisposition identifier a b | (identifier, (a, b)) <- Map.toAscList (Map.intersectionWith (,) old new), a /= b] ++
    concat [replicate (max 0 (n - Map.findWithDefault 0 diagnostic newDiagnostics)) (MissingDiagnostic diagnostic)
           | (diagnostic, n) <- Map.toAscList oldDiagnostics] ++
    concat [replicate (max 0 (n - Map.findWithDefault 0 diagnostic oldDiagnostics)) (AddedDiagnostic diagnostic)
           | (diagnostic, n) <- Map.toAscList newDiagnostics]
  where
    diagnosticCounts = Map.fromListWith (+) . map (\diagnostic -> (diagnostic, 1 :: Int)) . manifestDiagnostics

dispositionCounts :: Manifest -> [(Disposition, Int)]
dispositionCounts manifest = [(d, Map.findWithDefault 0 d counts) | d <- allDispositions]
  where
    counts = Map.fromListWith (+) [(verdictDisposition v, 1) | v <- manifestVerdicts manifest]

-- A small strict JSON codec keeps D1 buildable with boot libraries only.
data Json = JObject [(String, Json)] | JArray [Json] | JString String | JInteger Integer | JNull

encodeManifest :: Manifest -> String
encodeManifest manifest = renderJson (JObject
  [ ("schema", JString "bsc-testsuite-verdicts"), ("version", JInteger 1)
  , ("identity", JString "dejagnu-label-v1")
  , ("configuration", JString (manifestConfiguration manifest))
  , ("tests", JArray (map JString (sort (manifestTests manifest))))
  , ("verdicts", JArray (map encodeVerdict (sort (manifestVerdicts manifest))))
  , ("diagnostics", JArray (map encodeDiagnostic (sort (manifestDiagnostics manifest))))
  ]) ++ "\n"
  where
    encodeVerdict (Verdict identifier disposition) = JObject
      [ ("id", JObject [("test", JString (checkTest identifier)), ("label", JString (checkLabel identifier)), ("occurrence", JInteger (toInteger (checkOccurrence identifier)))])
      , ("disposition", JString (show disposition))
      ]
    encodeDiagnostic (Diagnostic test kind message) = JObject
      [("test", maybe JNull JString test), ("kind", JString (diagnosticName kind)), ("message", JString message)]

diagnosticName :: DiagnosticKind -> String
diagnosticName Error = "ERROR"
diagnosticName Warning = "WARNING"
diagnosticName Note = "NOTE"

renderJson :: Json -> String
renderJson (JObject fields) = "{" ++ intercalate "," [jsonString key ++ ":" ++ renderJson value | (key, value) <- fields] ++ "}"
renderJson (JArray values) = "[" ++ intercalate "," (map renderJson values) ++ "]"
renderJson (JString value) = jsonString value
renderJson (JInteger value) = show value
renderJson JNull = "null"

jsonString :: String -> String
jsonString value = '"' : concatMap escape value ++ "\""
  where
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape '\n' = "\\n"
    escape '\r' = "\\r"
    escape '\t' = "\\t"
    escape c | ord c < 32 = "\\u" ++ replicate (4 - length hex) '0' ++ hex
             | otherwise = [c]
      where hex = showHex (ord c) ""

jsonParser :: ReadP Json
jsonParser = jsonSpace *> value <* jsonSpace
  where
    -- JSON's first token selects exactly one production. In particular, do
    -- not use <++ to probe whole objects/arrays or build every list prefix
    -- with many/sepBy: suite-sized verdict archives make that costly.
    value = do
      remaining <- look
      case remaining of
        '{' : _ -> JObject <$> (char '{' *> jsonSpace *> delimited '}' objectMember)
        '[' : _ -> JArray <$> (char '[' *> jsonSpace *> delimited ']' jsonParser)
        '"' : _ -> JString <$> stringParser
        'n' : _ -> JNull <$ string "null"
        '-' : _ -> JInteger <$> integerParser
        c : _ | asciiDigit c -> JInteger <$> integerParser
        _ -> pfail
    objectMember = do
      key <- stringParser
      jsonSpace
      _ <- char ':'
      result <- jsonParser
      pure (key, result)
    integerParser = do
      remaining <- look
      sign <- case remaining of
        '-' : _ -> negate <$ get
        _ -> pure id
      unsigned <- look
      digits <- case unsigned of
        '0' : _ -> string "0"
        c : _ | c >= '1' && c <= '9' -> (:) <$> get <*> munch asciiDigit
        _ -> pfail
      pure (sign (read digits))

-- The opening delimiter and leading whitespace were consumed by the caller.
-- A comma commits to another element, so trailing commas are rejected.
delimited :: Char -> ReadP a -> ReadP [a]
delimited closing element = do
  remaining <- look
  case remaining of
    c : _ | c == closing -> [] <$ get
    _ -> (:) <$> element <*> rest
  where
    rest = do
      jsonSpace
      token <- get
      case token of
        ',' -> jsonSpace *> ((:) <$> element <*> rest)
        c | c == closing -> pure []
        _ -> pfail

asciiDigit :: Char -> Bool
asciiDigit c = c >= '0' && c <= '9'

jsonSpace :: ReadP ()
jsonSpace = () <$ munch (`elem` " \t\r\n")

stringParser :: ReadP String
stringParser = char '"' *> characters []
  where
    characters reversed = do
      c <- get
      case c of
        '"' -> pure (reverse reversed)
        '\\' -> escaped >>= \decoded -> characters (decoded : reversed)
        _ | ord c >= 32 && not (surrogate (ord c)) -> characters (c : reversed)
        _ -> pfail
    escaped = do
      c <- get
      case c of
        '"' -> pure '"'
        '\\' -> pure '\\'
        '/' -> pure '/'
        'b' -> pure '\b'
        'f' -> pure '\f'
        'n' -> pure '\n'
        'r' -> pure '\r'
        't' -> pure '\t'
        'u' -> unicode
        _ -> pfail
    unicode = do
      high <- hexCode
      if high >= 0xd800 && high <= 0xdbff then do
        _ <- string "\\u"
        low <- hexCode
        if low >= 0xdc00 && low <= 0xdfff
          then pure (chr (0x10000 + (high - 0xd800) * 0x400 + low - 0xdc00))
          else pfail
      else if surrogate high then pfail else pure (chr high)
    hexCode = foldl (\n c -> n * 16 + digitToInt c) 0 <$> count 4 (satisfy isHexDigit)
    surrogate code = code >= 0xd800 && code <= 0xdfff

decodeManifest :: String -> Either String Manifest
decodeManifest input = do
  json <- case [value | (value, "") <- readP_to_S (jsonParser <* eof) input] of
    [value] -> Right value
    _ -> Left "invalid JSON verdict manifest"
  fields <- objectFields ["schema", "version", "identity", "configuration", "tests", "verdicts", "diagnostics"] json
  schema <- stringField "schema" fields
  ensure (schema == "bsc-testsuite-verdicts") ("unknown manifest schema: " ++ schema)
  version <- field "version" fields >>= jsonInteger
  ensure (version == 1) ("unsupported manifest version: " ++ show version)
  identity <- stringField "identity" fields
  ensure (identity == "dejagnu-label-v1") ("unsupported check identity: " ++ identity)
  configuration <- stringField "configuration" fields
  tests <- field "tests" fields >>= jsonArray >>= mapM jsonText
  verdicts <- field "verdicts" fields >>= jsonArray >>= mapM decodeVerdict
  diagnostics <- field "diagnostics" fields >>= jsonArray >>= mapM decodeDiagnostic
  let manifest = Manifest configuration tests verdicts diagnostics
  validateManifest manifest
  pure manifest
  where
    decodeVerdict json = do
      fields <- objectFields ["id", "disposition"] json
      idFields <- field "id" fields >>= objectFields ["test", "label", "occurrence"]
      test <- stringField "test" idFields
      label <- stringField "label" idFields
      occurrence <- field "occurrence" idFields >>= jsonInteger
      ensure (occurrence > 0 && occurrence <= toInteger (maxBound :: Int)) "check occurrence is out of range"
      dispositionText <- stringField "disposition" fields
      disposition <- maybe (Left ("unknown disposition: " ++ dispositionText)) Right (lookup dispositionText dispositionNames)
      pure (Verdict (CheckId test label (fromInteger occurrence)) disposition)
    decodeDiagnostic json = do
      fields <- objectFields ["test", "kind", "message"] json
      testValue <- field "test" fields
      test <- case testValue of
        JNull -> Right Nothing
        _ -> Just <$> jsonText testValue
      kindText <- stringField "kind" fields
      kind <- maybe (Left ("unknown diagnostic kind: " ++ kindText)) Right (lookup kindText [("ERROR", Error), ("WARNING", Warning), ("NOTE", Note)])
      message <- stringField "message" fields
      pure (Diagnostic test kind message)

objectFields :: [String] -> Json -> Either String [(String, Json)]
objectFields expected (JObject fields) = do
  unique "JSON object field" (map fst fields)
  ensure (sort expected == sort (map fst fields)) ("JSON object fields differ from schema: expected=" ++ show (sort expected) ++ ", actual=" ++ show (sort (map fst fields)))
  pure fields
objectFields _ _ = Left "expected JSON object"

field :: String -> [(String, Json)] -> Either String Json
field key fields = maybe (Left ("missing JSON field: " ++ key)) Right (lookup key fields)

stringField :: String -> [(String, Json)] -> Either String String
stringField key fields = field key fields >>= jsonText

jsonText :: Json -> Either String String
jsonText (JString value) = Right value
jsonText _ = Left "expected JSON string"

jsonInteger :: Json -> Either String Integer
jsonInteger (JInteger value) = Right value
jsonInteger _ = Left "expected JSON integer"

jsonArray :: Json -> Either String [Json]
jsonArray (JArray values) = Right values
jsonArray _ = Left "expected JSON array"
