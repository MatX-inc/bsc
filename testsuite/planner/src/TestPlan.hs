-- | Semantic tests discovered in suite scripts. A test records what is being
-- tested and its expected result. Unsupported constructs and unresolved
-- dependencies remain in source order so an incomplete plan is explicit.
module TestPlan
  ( Identifier(..), PlanConfig(..), Compilation(..), Expectation(..), ExpectedError(..)
  , TestKind(..), testCompilation, Test(..), IssueKind(..), PlanIssue(..), PlanItem(..)
  , ScriptPlan(..), TestPlan(..)
  , renderIdentifier, encodePlan, decodePlan, validatePlan, validateConfig, validateExpectedError
  , plannedTests, planIssues, planCounts
  ) where

import Tcl (SourcePos(..))
import Control.Monad (unless)
import Data.Char (chr, digitToInt, isControl, isHexDigit, ord)
import Data.List (intercalate, isPrefixOf, sort)
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Numeric (showHex)
import System.FilePath (isAbsolute, normalise, splitDirectories, takeExtension)
import Text.ParserCombinators.ReadP
  ( ReadP, char, count, eof, get, look, munch, pfail, readP_to_S, satisfy, string )

-- | Recognized test invocations are numbered from one within each script.
-- Unsupported arguments still reserve a number. Setup commands and diagnostics
-- that do not represent recognized invocations never consume a test number.
data Identifier = Identifier
  { identifierTest :: FilePath, identifierNumber :: Int
  } deriving (Eq, Ord, Show)

data PlanConfig = PlanConfig
  { configName :: String, configInternalChecks :: Bool, configCompilerOptions :: [String]
  } deriving (Eq, Show)

-- | Source names are relative to the script's directory. A source may be
-- deliberately absent, particularly for a test that expects compilation to fail.
-- Options retain their order; dependency compilation represents the -u choice.
data Compilation = Compilation
  { compilationSource :: FilePath, compilationOptions :: [String]
  , compilationDependencies :: Bool
  } deriving (Eq, Show)
data Expectation = CompileSucceeds | CompileFails deriving (Eq, Show)
data ExpectedError = ExpectedError
  { expectedErrorTag :: String, expectedErrorCount :: Int
  } deriving (Eq, Show)
data TestKind = CompilationTest Compilation Expectation
              | CompilationErrorTest Compilation ExpectedError deriving (Eq, Show)

testCompilation :: TestKind -> Compilation
testCompilation (CompilationTest compilation _) = compilation
testCompilation (CompilationErrorTest compilation _) = compilation
data Test = Test
  { testId :: Identifier, testOrigin :: SourcePos, testKind :: TestKind
  } deriving (Eq, Show)

data IssueKind = UnsupportedConstruct | UnresolvedDependency deriving (Eq, Show)
data PlanIssue = PlanIssue
  { issueId :: Maybe Identifier, issuePosition :: SourcePos, issueConstruct :: String
  , issueKind :: IssueKind, issueReason :: String
  } deriving (Eq, Show)
data PlanItem = Planned Test | Unplanned PlanIssue deriving (Eq, Show)
-- | Every selected script is retained, including scripts with no items.
data ScriptPlan = ScriptPlan
  { scriptPath :: FilePath, scriptItems :: [PlanItem]
  } deriving (Eq, Show)
data TestPlan = TestPlan
  { planConfig :: PlanConfig, planScripts :: [ScriptPlan]
  } deriving (Eq, Show)

-- | The character count disambiguates paths containing colons.
renderIdentifier :: Identifier -> String
renderIdentifier i = "v3:" ++ show (length (identifierTest i)) ++ ":" ++ identifierTest i
  ++ ":" ++ show (identifierNumber i)

plannedTests :: TestPlan -> [Test]
plannedTests plan = [test | script <- planScripts plan, Planned test <- scriptItems script]

planIssues :: TestPlan -> [PlanIssue]
planIssues plan = [issue | script <- planScripts plan, Unplanned issue <- scriptItems script]

-- | Counts are (planned tests, unsupported constructs, unresolved dependencies).
planCounts :: TestPlan -> (Int, Int, Int)
planCounts plan = (length (plannedTests plan), issueCount UnsupportedConstruct, issueCount UnresolvedDependency)
  where issueCount kind = length (filter ((== kind) . issueKind) (planIssues plan))

ensure :: Bool -> String -> Either String ()
ensure condition message = unless condition (Left message)

unique :: Ord a => String -> [a] -> Either String ()
unique label xs = ensure (Set.size (Set.fromList xs) == length xs) ("duplicate " ++ label)

validCharacter :: Char -> Bool
validCharacter c = not (isControl c) && (ord c < 0xd800 || ord c > 0xdfff)

validText :: String -> Either String ()
validText value = ensure (not (null value) && all validCharacter value)
  "empty or invalid text"

safePath :: FilePath -> Either String ()
safePath path = ensure
  (not (null path) && not (isAbsolute path) && all validCharacter path
   && '\\' `notElem` path && normalise path == path
   && all (`notElem` [".", ".."]) (splitDirectories path))
  ("path must be canonical and relative: " ++ show path)

validateConfig :: PlanConfig -> Either String ()
validateConfig config = do
  validText (configName config)
  mapM_ validText (configCompilerOptions config)

-- Diagnostic tags are literal ASCII identifiers, never regular expressions.
validateExpectedError :: ExpectedError -> Either String ()
validateExpectedError expected = do
  ensure (case expectedErrorTag expected of
    first:rest -> letter first && all (\c -> letter c || digit c || c `elem` "_-") rest
    [] -> False) "error tag must start with an ASCII letter and contain only ASCII letters, digits, '_' or '-'"
  ensure (expectedErrorCount expected >= 0) "error count must be nonnegative"
  where
    letter c = c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z'
    digit c = c >= '0' && c <= '9'

-- | Validate the semantic contract, including entries that could not be
-- planned. This does not inspect the filesystem or establish suite coverage.
validatePlan :: TestPlan -> Either String ()
validatePlan plan = do
  validateConfig (planConfig plan)
  ensure (not (null scripts)) "plan has no selected scripts"
  unique "script" (map scriptPath scripts)
  unique "identifier" [identifier | script <- scripts, item <- scriptItems script,
                        Just identifier <- [itemId item]]
  mapM_ (validateScript (planConfig plan)) scripts
  where scripts = planScripts plan

itemId :: PlanItem -> Maybe Identifier
itemId (Planned test) = Just (testId test)
itemId (Unplanned issue) = issueId issue

validateScript :: PlanConfig -> ScriptPlan -> Either String ()
validateScript config script = do
  safePath path
  ensure (takeExtension path == ".exp") "script must name an .exp file"
  let numbers = map identifierNumber (mapMaybe itemId (scriptItems script))
  ensure (numbers == [1 .. length numbers])
    "test invocation numbers must be ordered and contiguous from one within each script"
  mapM_ validateItem (scriptItems script)
  where
    path = scriptPath script
    validateItem item = do
      case itemId item of
        Just identifier -> do
          ensure (identifierTest identifier == path) "identifier belongs to a different script"
          ensure (identifierNumber identifier > 0) "invalid test invocation number"
        Nothing -> pure ()
      case item of
        Planned test -> do
          validOrigin path (testOrigin test)
          let compilation = testCompilation (testKind test)
          safePath (compilationSource compilation)
          ensure (takeExtension (compilationSource compilation) `elem` [".bs", ".bsv"])
            "compilation source must be .bs or .bsv"
          mapM_ validText (compilationOptions compilation)
          ensure (configCompilerOptions config `isPrefixOf` compilationOptions compilation)
            "compilation options do not include the plan configuration prefix"
          case testKind test of
            CompilationTest _ _ -> pure ()
            CompilationErrorTest _ expected -> validateExpectedError expected
        Unplanned issue -> do
          validOrigin path (issuePosition issue)
          validText (issueConstruct issue)
          validText (issueReason issue)

validOrigin :: FilePath -> SourcePos -> Either String ()
validOrigin path position = ensure
  (sourceFile position == path && sourceLine position > 0
   && sourceColumn position > 0 && sourceOffset position >= 0)
  "invalid or inconsistent source origin"

-- The codec uses only GHC boot libraries. Explicit wire names prevent changes
-- in Haskell constructors from silently changing persisted plans.
data Json = JObject [(String, Json)] | JArray [Json] | JString String
          | JInteger Integer | JBool Bool | JNull

-- | Callers constructing exported records directly must validate before saving.
encodePlan :: TestPlan -> String
encodePlan plan = renderJson (JObject
  [("schema", JString "bsc-testsuite-test-plan"), ("version", JInteger 3),
   ("identity", JString "file-test-number-v1"), ("configuration", encodeConfig (planConfig plan)),
   ("scripts", JArray (map encodeScript (planScripts plan)))]) ++ "\n"

encodeConfig :: PlanConfig -> Json
encodeConfig config = JObject
  [("name", JString (configName config)), ("internal_checks", JBool (configInternalChecks config)),
   ("compiler_options", texts (configCompilerOptions config))]
encodeScript :: ScriptPlan -> Json
encodeScript script = JObject [("path", JString (scriptPath script)),
  ("items", JArray (map encodeItem (scriptItems script)))]
encodeItem :: PlanItem -> Json
encodeItem (Planned test) = JObject
  [("status", JString "planned"), ("id", encodeId (testId test)),
   ("origin", encodeOrigin (testOrigin test)), ("kind", encodeKind (testKind test))]
encodeItem (Unplanned issue) = JObject
  [("status", JString (issueStatus (issueKind issue))), ("id", maybe JNull encodeId (issueId issue)),
   ("origin", encodeOrigin (issuePosition issue)), ("construct", JString (issueConstruct issue)),
   ("reason", JString (issueReason issue))]
encodeId :: Identifier -> Json
encodeId identifier = JObject [("test", JString (identifierTest identifier)),
  ("number", int (identifierNumber identifier))]
encodeOrigin :: SourcePos -> Json
encodeOrigin position = JObject
  [("file", JString (sourceFile position)), ("line", int (sourceLine position)),
   ("column", int (sourceColumn position)), ("offset", int (sourceOffset position))]
encodeKind :: TestKind -> Json
encodeKind (CompilationTest compilation expectation) = JObject
  [("kind", JString "compilation"), ("source", JString (compilationSource compilation)),
   ("options", texts (compilationOptions compilation)),
   ("compile_dependencies", JBool (compilationDependencies compilation)),
   ("expectation", JString (expectationName expectation))]
encodeKind (CompilationErrorTest compilation expected) = JObject
  [("kind", JString "compilation-error"), ("source", JString (compilationSource compilation)),
   ("options", texts (compilationOptions compilation)),
   ("compile_dependencies", JBool (compilationDependencies compilation)),
   ("error_tag", JString (expectedErrorTag expected)),
   ("error_count", int (expectedErrorCount expected))]
texts :: [String] -> Json
texts = JArray . map JString
int :: Int -> Json
int = JInteger . toInteger
expectationName :: Expectation -> String
expectationName CompileSucceeds = "succeeds"
expectationName CompileFails = "fails"
issueStatus :: IssueKind -> String
issueStatus UnsupportedConstruct = "unsupported"
issueStatus UnresolvedDependency = "unresolved"

-- | Reject unknown or duplicate fields and incompatible versions, then check
-- the full semantic contract. Earlier plan versions require explicit migration.
decodePlan :: String -> Either String TestPlan
decodePlan input = do
  json <- case [value | (value, "") <- readP_to_S (jsonParser <* eof) input] of
    [value] -> Right value
    _ -> Left "invalid JSON test plan"
  fields <- objectFields ["schema", "version", "identity", "configuration", "scripts"] json
  constant fields "schema" "bsc-testsuite-test-plan"
  version <- field "version" fields >>= jsonInteger
  ensure (version == 3) ("unsupported plan version: " ++ show version)
  constant fields "identity" "file-test-number-v1"
  plan <- TestPlan <$> (field "configuration" fields >>= decodeConfig)
    <*> arrayField decodeScript "scripts" fields
  validatePlan plan
  pure plan

constant :: [(String, Json)] -> String -> String -> Either String ()
constant fields key expected = do
  actual <- stringField key fields
  ensure (actual == expected) ("unsupported " ++ key ++ ": " ++ actual)

decodeConfig :: Json -> Either String PlanConfig
decodeConfig json = do
  fields <- objectFields ["name", "internal_checks", "compiler_options"] json
  PlanConfig <$> stringField "name" fields <*> (field "internal_checks" fields >>= jsonBool)
    <*> arrayField jsonText "compiler_options" fields
decodeScript :: Json -> Either String ScriptPlan
decodeScript json = do
  fields <- objectFields ["path", "items"] json
  ScriptPlan <$> stringField "path" fields <*> arrayField decodeItem "items" fields
decodeItem :: Json -> Either String PlanItem
decodeItem json = do
  status <- discriminator "status" json
  case status of
    "planned" -> do
      fields <- objectFields ["status", "id", "origin", "kind"] json
      Planned <$> (Test <$> (field "id" fields >>= decodeId)
        <*> (field "origin" fields >>= decodeOrigin) <*> (field "kind" fields >>= decodeKind))
    "unsupported" -> unplanned UnsupportedConstruct
    "unresolved" -> unplanned UnresolvedDependency
    _ -> Left ("unsupported item status: " ++ status)
  where
    unplanned kind = do
      fields <- objectFields ["status", "id", "origin", "construct", "reason"] json
      Unplanned <$> (PlanIssue <$> (field "id" fields >>= decodeIssueId)
        <*> (field "origin" fields >>= decodeOrigin) <*> stringField "construct" fields
        <*> pure kind <*> stringField "reason" fields)
decodeId :: Json -> Either String Identifier
decodeId json = do
  fields <- objectFields ["test", "number"] json
  Identifier <$> stringField "test" fields <*> integerField "number" fields
decodeIssueId :: Json -> Either String (Maybe Identifier)
decodeIssueId JNull = Right Nothing
decodeIssueId json = Just <$> decodeId json
decodeOrigin :: Json -> Either String SourcePos
decodeOrigin json = do
  fields <- objectFields ["file", "line", "column", "offset"] json
  SourcePos <$> stringField "file" fields <*> integerField "line" fields
    <*> integerField "column" fields <*> integerField "offset" fields
decodeKind :: Json -> Either String TestKind
decodeKind json = do
  kind <- discriminator "kind" json
  case kind of
    "compilation" -> do
      fields <- objectFields ["kind", "source", "options", "compile_dependencies", "expectation"] json
      compilation <- Compilation <$> stringField "source" fields <*> arrayField jsonText "options" fields
        <*> (field "compile_dependencies" fields >>= jsonBool)
      expectation <- field "expectation" fields >>= enum expectationName [CompileSucceeds, CompileFails]
      pure (CompilationTest compilation expectation)
    "compilation-error" -> do
      fields <- objectFields ["kind", "source", "options", "compile_dependencies", "error_tag", "error_count"] json
      compilation <- Compilation <$> stringField "source" fields <*> arrayField jsonText "options" fields
        <*> (field "compile_dependencies" fields >>= jsonBool)
      expected <- ExpectedError <$> stringField "error_tag" fields <*> integerField "error_count" fields
      pure (CompilationErrorTest compilation expected)
    _ -> Left ("unsupported test kind: " ++ kind)

enum :: (a -> String) -> [a] -> Json -> Either String a
enum name values json = do
  value <- jsonText json
  maybe (Left ("unsupported value: " ++ value)) Right (lookup value [(name x, x) | x <- values])
discriminator :: String -> Json -> Either String String
discriminator key (JObject fields) = unique "JSON object field" (map fst fields) >> stringField key fields
discriminator _ _ = Left "expected JSON object"
boundedInt :: Json -> Either String Int
boundedInt json = do
  n <- jsonInteger json
  ensure (n >= 0 && n <= toInteger (maxBound :: Int)) "integer is out of range"
  pure (fromInteger n)
integerField :: String -> [(String, Json)] -> Either String Int
integerField key fields = field key fields >>= boundedInt
arrayField :: (Json -> Either String a) -> String -> [(String, Json)] -> Either String [a]
arrayField decoder key fields = field key fields >>= jsonArray >>= mapM decoder

renderJson :: Json -> String
renderJson (JObject fields) = "{" ++ intercalate "," [jsonString key ++ ":" ++ renderJson value | (key, value) <- fields] ++ "}"
renderJson (JArray values) = "[" ++ intercalate "," (map renderJson values) ++ "]"
renderJson (JString value) = jsonString value
renderJson (JInteger value) = show value
renderJson (JBool value) = if value then "true" else "false"
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
    value = do
      remaining <- look
      case remaining of
        '{' : _ -> JObject <$> (char '{' *> jsonSpace *> delimited '}' objectMember)
        '[' : _ -> JArray <$> (char '[' *> jsonSpace *> delimited ']' jsonParser)
        '"' : _ -> JString <$> stringParser
        'n' : _ -> JNull <$ string "null"
        't' : _ -> JBool True <$ string "true"
        'f' : _ -> JBool False <$ string "false"
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
      sign <- case remaining of '-' : _ -> negate <$ get; _ -> pure id
      unsigned <- look
      digits <- case unsigned of
        '0' : _ -> string "0"
        c : _ | c >= '1' && c <= '9' -> (:) <$> get <*> munch asciiDigit
        _ -> pfail
      pure (sign (read digits))
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
objectFields :: [String] -> Json -> Either String [(String, Json)]
objectFields expected (JObject fields) = do
  unique "JSON object field" (map fst fields)
  ensure (sort expected == sort (map fst fields))
    ("JSON fields differ from schema: expected=" ++ show (sort expected) ++ ", actual=" ++ show (sort (map fst fields)))
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
jsonBool :: Json -> Either String Bool
jsonBool (JBool value) = Right value
jsonBool _ = Left "expected JSON boolean"
jsonArray :: Json -> Either String [Json]
jsonArray (JArray values) = Right values
jsonArray _ = Left "expected JSON array"
