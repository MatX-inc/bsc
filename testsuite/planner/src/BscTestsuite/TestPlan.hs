-- | Versioned, declarative test plans. A step's dependencies must complete,
-- regardless of their exit status, before it runs. Assertions do not gate
-- execution. Each scenario starts in a fresh workspace and carries a snapshot
-- of that workspace through every step, including failed tool invocations.
module BscTestsuite.TestPlan
  ( Identifier(..), PlanConfig(..), TestPlan(..), Scenario(..)
  , InputRef(..), OutputKind(..), Output(..), Operation(..), Cacheability(..)
  , Step(..), AssertionClass(..), Expectation(..), Check(..)
  , renderIdentifier, encodePlan, decodePlan, validatePlan
  , normalizeCacheability, explainCheck
  ) where

import BscTestsuite.Tcl (SourcePos(..))
import Control.Monad (foldM, forM_, unless)
import Data.Char (chr, digitToInt, isControl, isHexDigit, ord)
import Data.List (intercalate, isPrefixOf, sort)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Numeric (showHex)
import System.FilePath (isAbsolute, normalise, splitDirectories, takeDirectory, takeExtension, (</>))
import Text.ParserCombinators.ReadP
  ( ReadP, char, count, eof, get, look, munch, pfail, readP_to_S, satisfy, string )

-- | Identity is independent of results and labels, and scoped to a frozen
-- source topology. Site coordinates identify commands and loop iterations;
-- line/column positions are diagnostics, not identity. Configuration is scoped
-- by the containing plan. Coordinates are one-based, including iterations.
data Identifier = Identifier
  { identifierTest :: FilePath, identifierSite :: [Int], identifierRole :: String
  } deriving (Eq, Ord, Show)

data PlanConfig = PlanConfig
  { configName :: String, configInternalChecks :: Bool, configCompilerOptions :: [String]
  } deriving (Eq, Show)
data TestPlan = TestPlan
  { planConfig :: PlanConfig, planScenarios :: [Scenario]
  } deriving (Eq, Show)
data Scenario = Scenario
  { scenarioTest :: FilePath, scenarioSteps :: [Step], scenarioChecks :: [Check]
  } deriving (Eq, Show)

-- | Suite inputs are relative to the suite. Produced subpaths select a member
-- of a directory output. Operation and output paths are workspace-relative.
-- A suite input describes a snapshot entry, including its absence: a negative
-- test may deliberately name a missing source. Planning does not require such
-- inputs to exist or turn a missing input into a successful assertion.
data InputRef = SuiteFile FilePath | SuiteDirectory FilePath
              | Produced Identifier String (Maybe FilePath)
  deriving (Eq, Show)
data OutputKind = Transcript | ProcessStatus | FileArtifact | DirectoryArtifact
  deriving (Eq, Show)
data Output = Output
  { outputName :: String, outputKind :: OutputKind, outputPath :: Maybe FilePath
  } deriving (Eq, Show)
data Operation = BscCompile FilePath [String] Bool | InternalLoad FilePath
  deriving (Eq, Show)

-- | Cache restrictions flow through dependencies: a consumer cannot weaken
-- LocalOnly or Never. Never requires tool execution even if inputs are equal.
data Cacheability = Cacheable | LocalOnly | Never deriving (Eq, Ord, Show)
data Step = Step
  { stepId :: Identifier, stepOrigin :: SourcePos, stepOperation :: Operation
  , stepInputs :: [InputRef], stepOutputs :: [Output], stepTools :: [String]
  , stepDependsOn :: [Identifier], stepCacheability :: Cacheability
  } deriving (Eq, Show)
data AssertionClass = Ordinary | Internal deriving (Eq, Show)
data Expectation = ToolSucceeds | ToolFails deriving (Eq, Show)
data Check = Check
  { checkId :: Identifier, checkOrigin :: SourcePos, checkProducer :: Identifier
  , checkExpectation :: Expectation, checkClass :: AssertionClass
  } deriving (Eq, Show)

-- | A character-count prefix makes the path boundary unambiguous even when a
-- valid filename contains colons. Site components and roles have fixed syntax.
renderIdentifier :: Identifier -> String
renderIdentifier i = "v1:" ++ show (length (identifierTest i)) ++ ":" ++ identifierTest i
  ++ ":" ++ intercalate "." (map show (identifierSite i)) ++ ":" ++ identifierRole i

ensure :: Bool -> String -> Either String ()
ensure condition message = unless condition (Left message)

unique :: Ord a => String -> [a] -> Either String ()
unique label xs = ensure (Set.size (Set.fromList xs) == length xs) ("duplicate " ++ label)

safePath :: Bool -> FilePath -> Either String ()
safePath allowDot path = ensure
  (not (null path) && not (isAbsolute path) && not (any isControl path)
   && '\\' `notElem` path && normalise path == path
   && ((allowDot && path == ".") || all (`notElem` [".", ".."]) (splitDirectories path)))
  ("path must be canonical and relative: " ++ show path)

validName :: String -> Either String ()
validName name = ensure (not (null name) && all allowed name) ("invalid name: " ++ show name)
  where allowed c = c >= 'a' && c <= 'z' || c >= '0' && c <= '9' || c `elem` "-."

validText :: String -> Either String ()
validText value = ensure (not (null value) && not (any isControl value)) "empty or control-containing text"

validTest :: FilePath -> Either String ()
validTest path = safePath False path >> ensure (takeExtension path == ".exp") "scenario must name an .exp file"

validId :: FilePath -> Identifier -> Either String ()
validId test i = do
  ensure (identifierTest i == test) "identifier belongs to a different scenario"
  ensure (not (null (identifierSite i)) && all (> 0) (identifierSite i)) "invalid source-site coordinates"
  validName (identifierRole i)

validOrigin :: FilePath -> SourcePos -> Either String ()
validOrigin test p = ensure
  (sourceFile p == test && sourceLine p > 0 && sourceColumn p > 0 && sourceOffset p >= 0)
  "invalid or inconsistent source origin"

validatePlan :: TestPlan -> Either String ()
validatePlan plan = do
  validateShape plan
  forM_ (planScenarios plan) $ \scenario -> do
    let steps = Map.fromList [(stepId s, s) | s <- scenarioSteps scenario]
    forM_ (scenarioSteps scenario) $ \s -> forM_ (stepDependsOn s) $ \dependency ->
      ensure (stepCacheability s >= stepCacheability (steps Map.! dependency))
        ("cache restriction weakened by " ++ renderIdentifier (stepId s))

-- | Normalize only a structurally valid plan. This lets a lowerer assign local
-- policy first without accidentally making a downstream action cacheable.
normalizeCacheability :: TestPlan -> Either String TestPlan
normalizeCacheability plan = do
  validateShape plan
  pure plan { planScenarios = map normalizeScenario (planScenarios plan) }
  where
    normalizeScenario scenario = scenario { scenarioSteps = reverse result }
      where
        (_, result) = foldl visit (Map.empty, []) (scenarioSteps scenario)
        visit (policies, acc) s =
          let policy = maximum (stepCacheability s : map (policies Map.!) (stepDependsOn s))
              next = s { stepCacheability = policy }
          in (Map.insert (stepId s) policy policies, next : acc)

validateShape :: TestPlan -> Either String ()
validateShape plan = do
  validText (configName (planConfig plan))
  mapM_ validText (configCompilerOptions (planConfig plan))
  ensure (not (null (planScenarios plan))) "plan has no scenarios"
  unique "scenario" (map scenarioTest (planScenarios plan))
  unique "identifier" [i | sc <- planScenarios plan, i <- map stepId (scenarioSteps sc) ++ map checkId (scenarioChecks sc)]
  mapM_ (validateScenario (planConfig plan)) (planScenarios plan)

validateScenario :: PlanConfig -> Scenario -> Either String ()
validateScenario config scenario = do
  validTest test
  _ <- foldM visit (Map.empty, Nothing) steps
  mapM_ validCheck (scenarioChecks scenario)
  where
    test = scenarioTest scenario
    steps = scenarioSteps scenario
    allSteps = Map.fromList [(stepId s, s) | s <- steps]
    visit (prior, previous) s = do
      validId test (stepId s)
      validOrigin test (stepOrigin s)
      unique "dependency" (stepDependsOn s)
      forM_ (stepDependsOn s) $ \dependency -> ensure (Map.member dependency prior)
        ("dependency is missing, forward, or cyclic: " ++ renderIdentifier dependency)
      unique "output name" (map outputName (stepOutputs s))
      mapM_ validOutput (stepOutputs s)
      requiredOutput s "workspace" DirectoryArtifact (Just ".")
      requiredOutput s "status" ProcessStatus Nothing
      unique "tool" (stepTools s)
      mapM_ validName (stepTools s)
      mapM_ (validInput prior s) (stepInputs s)
      case previous of
        Nothing -> ensure (SuiteDirectory (takeDirectory test) `elem` stepInputs s)
          "first step must take the scenario's suite directory"
        Just previousId -> do
          ensure (previousId `elem` stepDependsOn s) "workspace predecessor is not a dependency"
          ensure (Produced previousId "workspace" Nothing `elem` stepInputs s)
            "step must consume its immediate predecessor's workspace snapshot"
          ensure (not (any isSuiteDirectory (stepInputs s))) "later steps must use the preceding workspace snapshot"
      validOperation s
      pure (Map.insert (stepId s) s prior, Just (stepId s))
    isSuiteDirectory (SuiteDirectory _) = True
    isSuiteDirectory _ = False
    validOperation s = case stepOperation s of
      BscCompile source options _ -> do
        safePath False source
        ensure (takeExtension source `elem` [".bs", ".bsv"]) "compile source must be .bs or .bsv"
        mapM_ validText options
        ensure (configCompilerOptions config `isPrefixOf` options)
          "compile options do not include the plan configuration prefix"
        ensure (SuiteFile (normalise (takeDirectory test </> source)) `elem` stepInputs s)
          "compile source must be declared as a suite-file input"
        ensure (stepTools s == ["bsc"]) "compile tool must be bsc"
        requiredOutput s "transcript" Transcript (Just (source ++ ".bsc-out"))
      InternalLoad object -> do
        safePath False object
        ensure (configInternalChecks config) "internal operation appears with internal checks disabled"
        ensure (takeExtension object == ".bo") "internal load requires a .bo object"
        ensure (stepTools s == ["dumpbo"]) "internal-load tool must be dumpbo"
        requiredOutput s "transcript" Transcript (Just (object ++ ".dumpbo-out"))
        ensure (any (objectInput object) (stepInputs s)) "internal object must come from a compile workspace output"
    objectInput object (Produced producer "workspace" (Just path)) = path == object &&
      case Map.lookup producer allSteps of
        Just s -> case stepOperation s of BscCompile {} -> True; _ -> False
        Nothing -> False
    objectInput _ _ = False
    validCheck check = do
      validId test (checkId check)
      validOrigin test (checkOrigin check)
      producer <- maybe (Left "check producer is missing") Right (Map.lookup (checkProducer check) allSteps)
      ensure (checkOrigin check == stepOrigin producer) "check origin differs from its producer"
      ensure (identifierSite (checkId check) == identifierSite (stepId producer)) "check site differs from its producer"
      case stepOperation producer of
        BscCompile {} -> ensure (checkClass check == Ordinary) "compile assertion must be ordinary"
        InternalLoad {} -> do
          ensure (checkClass check == Internal) "internal-load assertion must be internal"
          ensure (checkExpectation check == ToolSucceeds) "internal load must be expected to succeed"

requiredOutput :: Step -> String -> OutputKind -> Maybe FilePath -> Either String ()
requiredOutput s name kind path = ensure
  (Output name kind path `elem` stepOutputs s) ("missing or invalid " ++ name ++ " output")

validOutput :: Output -> Either String ()
validOutput o = do
  validName (outputName o)
  case (outputKind o, outputPath o) of
    (ProcessStatus, Nothing) -> pure ()
    (ProcessStatus, Just _) -> Left "process status cannot have a filesystem path"
    (DirectoryArtifact, Just path) -> safePath True path
    (_, Just path) -> safePath False path
    (_, Nothing) -> Left "filesystem output requires a path"

validInput :: Map.Map Identifier Step -> Step -> InputRef -> Either String ()
validInput _ _ (SuiteFile path) = safePath False path
validInput _ _ (SuiteDirectory path) = safePath True path
validInput prior s (Produced producer name subpath) = do
  ensure (producer `elem` stepDependsOn s) "produced input must name a declared dependency"
  other <- maybe (Left "produced input references a missing or forward step") Right (Map.lookup producer prior)
  output <- maybe (Left ("produced input references missing output: " ++ name)) Right
    (lookup name [(outputName o, o) | o <- stepOutputs other])
  case subpath of
    Nothing -> ensure (outputKind output /= ProcessStatus) "status values are not filesystem inputs"
    Just path -> do
      safePath False path
      ensure (outputKind output == DirectoryArtifact) "subpath requires a directory output"

-- | Explain the selected check and all transitive prerequisites in plan order.
explainCheck :: TestPlan -> String -> Either String String
explainCheck plan selector = do
  validatePlan plan
  (scenario, check) <- case [(s, c) | s <- planScenarios plan, c <- scenarioChecks s,
                                      renderIdentifier (checkId c) == selector] of
    [match] -> Right match
    _ -> Left ("unknown check: " ++ selector)
  let steps = Map.fromList [(stepId s, s) | s <- scenarioSteps scenario]
      collect visited [] = visited
      collect visited (i:pending)
        | Set.member i visited = collect visited pending
        | otherwise = collect (Set.insert i visited) (stepDependsOn (steps Map.! i) ++ pending)
      wanted = collect Set.empty [checkProducer check]
      selected = filter ((`Set.member` wanted) . stepId) (scenarioSteps scenario)
  pure (unlines (["Check " ++ selector, "Configuration: " ++ configName (planConfig plan),
    "Class: " ++ assertionClassName (checkClass check),
    "Expectation: " ++ expectationName (checkExpectation check),
    "Origin: " ++ showOrigin (checkOrigin check),
    "Workspace: fresh-shared, snapshot_chain",
    "Prerequisites execute after dependencies complete, regardless of exit status."] ++ concatMap showStep selected))
  where
    showStep s = ["", "Step " ++ renderIdentifier (stepId s),
      "  Origin: " ++ showOrigin (stepOrigin s), "  Operation: " ++ showOperation (stepOperation s),
      "  Tools: " ++ intercalate ", " (stepTools s) ++ " (complete installation identity required)",
      "  Cacheability: " ++ cacheName (stepCacheability s),
      "  Depends on: " ++ listOrNone (map renderIdentifier (stepDependsOn s))] ++
      map (("  Input: " ++) . showInput) (stepInputs s) ++ map (("  Output: " ++) . showOutput) (stepOutputs s)
    showOrigin p = sourceFile p ++ ":" ++ show (sourceLine p) ++ ":" ++ show (sourceColumn p)
      ++ " (offset " ++ show (sourceOffset p) ++ ")"
    listOrNone [] = "(none)"
    listOrNone values = intercalate ", " values
    showOperation (BscCompile source options dependencies) =
      "compile " ++ source ++ "; dependencies " ++ (if dependencies then "enabled" else "disabled") ++
      "; options " ++ listOrNone options ++ "; suppress version and timestamps"
    showOperation (InternalLoad object) = "load intermediate object " ++ object
    showInput (SuiteFile path) = "suite file " ++ path ++ " (including absence)"
    showInput (SuiteDirectory path) = "suite directory " ++ path
    showInput (Produced producer output path) = "step " ++ renderIdentifier producer ++
      " output " ++ output ++ maybe "" (" member " ++) path
    showOutput output = outputName output ++ " (" ++ outputKindName (outputKind output) ++ ")" ++
      maybe "" (": " ++) (outputPath output)

data Json = JObject [(String, Json)] | JArray [Json] | JString String
          | JInteger Integer | JBool Bool | JNull

encodePlan :: TestPlan -> String
encodePlan plan = renderJson (JObject
  [("schema", JString "bsc-testsuite-test-plan"), ("version", JInteger 1),
   ("identity", JString "source-site-check-v1"), ("dependency_policy", JString "completion"),
   ("workspace_policy", JString "fresh-shared"), ("workspace_flow", JString "snapshot_chain"),
   ("configuration", encodeConfig (planConfig plan)),
   ("scenarios", JArray (map encodeScenario (planScenarios plan)))]) ++ "\n"

encodeConfig :: PlanConfig -> Json
encodeConfig c = JObject [("name", JString (configName c)), ("internal_checks", JBool (configInternalChecks c)),
  ("compiler_options", texts (configCompilerOptions c))]
encodeScenario :: Scenario -> Json
encodeScenario s = JObject [("test", JString (scenarioTest s)),
  ("steps", JArray (map encodeStep (scenarioSteps s))), ("checks", JArray (map encodeCheck (scenarioChecks s)))]
encodeId :: Identifier -> Json
encodeId i = JObject [("test", JString (identifierTest i)), ("site", JArray (map (JInteger . toInteger) (identifierSite i))),
  ("role", JString (identifierRole i))]
encodeOrigin :: SourcePos -> Json
encodeOrigin p = JObject [("file", JString (sourceFile p)), ("line", int (sourceLine p)),
  ("column", int (sourceColumn p)), ("offset", int (sourceOffset p))]
encodeStep :: Step -> Json
encodeStep s = JObject [("id", encodeId (stepId s)), ("origin", encodeOrigin (stepOrigin s)),
  ("operation", encodeOperation (stepOperation s)), ("inputs", JArray (map encodeInput (stepInputs s))),
  ("outputs", JArray (map encodeOutput (stepOutputs s))), ("tools", texts (stepTools s)),
  ("depends_on", JArray (map encodeId (stepDependsOn s))), ("cacheability", JString (cacheName (stepCacheability s)))]
encodeOperation :: Operation -> Json
encodeOperation (BscCompile source options dependencies) = JObject [("kind", JString "bsc-compile"),
  ("source", JString source), ("options", texts options), ("compile_dependencies", JBool dependencies)]
encodeOperation (InternalLoad path) = JObject [("kind", JString "internal-load"), ("object", JString path)]
encodeInput :: InputRef -> Json
encodeInput (SuiteFile path) = JObject [("kind", JString "suite-file"), ("path", JString path)]
encodeInput (SuiteDirectory path) = JObject [("kind", JString "suite-directory"), ("path", JString path)]
encodeInput (Produced i name path) = JObject [("kind", JString "produced"), ("step", encodeId i),
  ("output", JString name), ("subpath", maybe JNull JString path)]
encodeOutput :: Output -> Json
encodeOutput o = JObject [("name", JString (outputName o)), ("kind", JString (outputKindName (outputKind o))),
  ("path", maybe JNull JString (outputPath o))]
encodeCheck :: Check -> Json
encodeCheck c = JObject [("id", encodeId (checkId c)), ("origin", encodeOrigin (checkOrigin c)),
  ("producer", encodeId (checkProducer c)), ("expectation", JString (expectationName (checkExpectation c))),
  ("class", JString (assertionClassName (checkClass c)))]
texts :: [String] -> Json
texts = JArray . map JString
int :: Int -> Json
int = JInteger . toInteger

cacheName :: Cacheability -> String
cacheName Cacheable = "cacheable"
cacheName LocalOnly = "cacheable-local-only"
cacheName Never = "never"
outputKindName :: OutputKind -> String
outputKindName Transcript = "transcript"
outputKindName ProcessStatus = "process-status"
outputKindName FileArtifact = "file"
outputKindName DirectoryArtifact = "directory"
expectationName :: Expectation -> String
expectationName ToolSucceeds = "tool-succeeds"
expectationName ToolFails = "tool-fails"
assertionClassName :: AssertionClass -> String
assertionClassName Ordinary = "ordinary"
assertionClassName Internal = "internal"

decodePlan :: String -> Either String TestPlan
decodePlan input = do
  json <- case [value | (value, "") <- readP_to_S (jsonParser <* eof) input] of
    [value] -> Right value
    _ -> Left "invalid JSON test plan"
  fields <- objectFields ["schema", "version", "identity", "dependency_policy", "workspace_policy", "workspace_flow", "configuration", "scenarios"] json
  constant fields "schema" "bsc-testsuite-test-plan"
  version <- field "version" fields >>= jsonInteger
  ensure (version == 1) ("unsupported plan version: " ++ show version)
  constant fields "identity" "source-site-check-v1"
  constant fields "dependency_policy" "completion"
  constant fields "workspace_policy" "fresh-shared"
  constant fields "workspace_flow" "snapshot_chain"
  plan <- TestPlan <$> (field "configuration" fields >>= decodeConfig)
    <*> arrayField decodeScenario "scenarios" fields
  validatePlan plan
  pure plan

constant :: [(String, Json)] -> String -> String -> Either String ()
constant fields key expected = do
  actual <- stringField key fields
  ensure (actual == expected) ("unsupported " ++ key ++ ": " ++ actual)

decodeConfig :: Json -> Either String PlanConfig
decodeConfig json = do
  f <- objectFields ["name", "internal_checks", "compiler_options"] json
  PlanConfig <$> stringField "name" f <*> (field "internal_checks" f >>= jsonBool) <*> arrayField jsonText "compiler_options" f
decodeScenario :: Json -> Either String Scenario
decodeScenario json = do
  f <- objectFields ["test", "steps", "checks"] json
  Scenario <$> stringField "test" f <*> arrayField decodeStep "steps" f <*> arrayField decodeCheck "checks" f
decodeId :: Json -> Either String Identifier
decodeId json = do
  f <- objectFields ["test", "site", "role"] json
  Identifier <$> stringField "test" f <*> arrayField boundedInt "site" f <*> stringField "role" f
decodeOrigin :: Json -> Either String SourcePos
decodeOrigin json = do
  f <- objectFields ["file", "line", "column", "offset"] json
  SourcePos <$> stringField "file" f <*> integerField "line" f <*> integerField "column" f <*> integerField "offset" f
decodeStep :: Json -> Either String Step
decodeStep json = do
  f <- objectFields ["id", "origin", "operation", "inputs", "outputs", "tools", "depends_on", "cacheability"] json
  Step <$> (field "id" f >>= decodeId) <*> (field "origin" f >>= decodeOrigin)
    <*> (field "operation" f >>= decodeOperation) <*> arrayField decodeInput "inputs" f
    <*> arrayField decodeOutput "outputs" f <*> arrayField jsonText "tools" f
    <*> arrayField decodeId "depends_on" f <*> (field "cacheability" f >>= enum cacheName [Cacheable, LocalOnly, Never])
decodeOperation :: Json -> Either String Operation
decodeOperation json = do
  kind <- kindField json
  case kind of
    "bsc-compile" -> do
      f <- objectFields ["kind", "source", "options", "compile_dependencies"] json
      BscCompile <$> stringField "source" f <*> arrayField jsonText "options" f <*> (field "compile_dependencies" f >>= jsonBool)
    "internal-load" -> do
      f <- objectFields ["kind", "object"] json
      InternalLoad <$> stringField "object" f
    _ -> Left ("unsupported operation kind: " ++ kind)
decodeInput :: Json -> Either String InputRef
decodeInput json = do
  kind <- kindField json
  case kind of
    "suite-file" -> objectFields ["kind", "path"] json >>= fmap SuiteFile . stringField "path"
    "suite-directory" -> objectFields ["kind", "path"] json >>= fmap SuiteDirectory . stringField "path"
    "produced" -> do
      f <- objectFields ["kind", "step", "output", "subpath"] json
      Produced <$> (field "step" f >>= decodeId) <*> stringField "output" f <*> (field "subpath" f >>= nullableText)
    _ -> Left ("unsupported input kind: " ++ kind)
decodeOutput :: Json -> Either String Output
decodeOutput json = do
  f <- objectFields ["name", "kind", "path"] json
  Output <$> stringField "name" f <*> (field "kind" f >>= enum outputKindName [Transcript, ProcessStatus, FileArtifact, DirectoryArtifact])
    <*> (field "path" f >>= nullableText)
decodeCheck :: Json -> Either String Check
decodeCheck json = do
  f <- objectFields ["id", "origin", "producer", "expectation", "class"] json
  Check <$> (field "id" f >>= decodeId) <*> (field "origin" f >>= decodeOrigin) <*> (field "producer" f >>= decodeId)
    <*> (field "expectation" f >>= enum expectationName [ToolSucceeds, ToolFails])
    <*> (field "class" f >>= enum assertionClassName [Ordinary, Internal])

enum :: (a -> String) -> [a] -> Json -> Either String a
enum name values json = do
  value <- jsonText json
  maybe (Left ("unsupported value: " ++ value)) Right (lookup value [(name x, x) | x <- values])
kindField :: Json -> Either String String
kindField (JObject f) = unique "JSON object field" (map fst f) >> stringField "kind" f
kindField _ = Left "expected JSON object"
nullableText :: Json -> Either String (Maybe String)
nullableText JNull = Right Nothing
nullableText json = Just <$> jsonText json
boundedInt :: Json -> Either String Int
boundedInt json = do
  n <- jsonInteger json
  ensure (n >= 0 && n <= toInteger (maxBound :: Int)) "integer is out of range"
  pure (fromInteger n)
integerField :: String -> [(String, Json)] -> Either String Int
integerField key f = field key f >>= boundedInt
arrayField :: (Json -> Either String a) -> String -> [(String, Json)] -> Either String [a]
arrayField decoder key f = field key f >>= jsonArray >>= mapM decoder

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
