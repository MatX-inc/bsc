-- | Execute one supported semantic test in a private source workspace.
--
-- This is deliberately a small bsctest executor, not a shell/Tcl executor.
-- The caller supplies a source snapshot and an installation; this module copies
-- the snapshot, checks the selected declaration, and observes compilation plus
-- its diagnostic expectation and optional object-load obligation. Prior build artifacts, nodeps, mutable
-- Tcl setup, and unimplemented test kinds are not silently given new semantics.
module Execute
  ( ExecutionConfig(..), defaultExecutionTimeoutMicros
  , ProcessStatus(..), ProcessResult(..), CheckDisposition(..), CheckResult(..), DiagnosticResult(..)
  , ExecutionReport(..), executeTest, hasInfrastructureFailure, encodeExecutionReport
  , countErrorDiagnostics
  ) where

import Control.Exception
  ( IOException, catch, mask, onException, try )
import Control.Concurrent (threadDelay)
import Control.Monad (forM, forM_, unless, when)
import Data.Char (ord)
import Data.List (intercalate, isPrefixOf, isSuffixOf, sort, tails)
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Set as Set
import Numeric (showHex)
import System.Directory
  ( canonicalizePath, copyFile, createDirectory, createDirectoryIfMissing
  , createDirectoryLink, createFileLink, doesDirectoryExist
  , doesFileExist, doesPathExist, executable, getPermissions, listDirectory, pathIsSymbolicLink )
import System.Exit (ExitCode(..))
import System.FilePath
  ( (</>), isAbsolute, joinPath, makeRelative, splitDirectories, takeDirectory
  , takeExtension, takeFileName )
import System.IO (IOMode(ReadMode, WriteMode), hGetContents', hSetEncoding, latin1, withBinaryFile)
import System.Posix.Signals (sigKILL, signalProcessGroup)
import System.Process
  ( CreateProcess(..), StdStream(..), createProcess, getPid, getProcessExitCode
  , proc, waitForProcess )
import System.Timeout (timeout)
import Lower (lowerPlan)
import Procedures (InternalCheck(..), internalChecksFor, internalChecksAfter)
import TestPlan

data ExecutionConfig = ExecutionConfig
  { executionInstallation :: FilePath
  , executionSuite :: FilePath
  , executionOutput :: FilePath
  , executionTimeoutMicros :: Int
  } deriving (Eq, Show)

defaultExecutionTimeoutMicros :: Int
-- The existing gh334 object-load check takes several minutes. This generous
-- infrastructure bound is not a test expectation or a reason to skip it.
defaultExecutionTimeoutMicros = 600000000

data ProcessStatus
  = ProcessExited Int
  | ProcessSignaled Int
  | ProcessTimedOut Int
  | ProcessLaunchFailed String
  deriving (Eq, Show)

data ProcessResult = ProcessResult
  { processProgram :: FilePath
  , processArguments :: [String]
  , processTranscript :: FilePath
  , processStatus :: ProcessStatus
  } deriving (Eq, Show)

data CheckDisposition = CheckPass | CheckFail | CheckInfrastructureError
  deriving (Eq, Show)

data CheckResult = CheckResult
  { checkRole :: String
  , checkDisposition :: CheckDisposition
  , checkProcess :: ProcessResult
  , checkDiagnostic :: Maybe DiagnosticResult
  } deriving (Eq, Show)

-- | Evidence for a diagnostic assertion. The process transcript is retained
-- separately, so consumers can verify this count rather than trusting a PASS.
data DiagnosticResult = DiagnosticResult
  { diagnosticExpected :: ExpectedError
  , diagnosticActualCount :: Int
  } deriving (Eq, Show)

data ExecutionReport = ExecutionReport
  { executionId :: Identifier
  , executionReportConfig :: PlanConfig
  , executionBoundInstallation :: FilePath
  , executionWorkingDirectory :: FilePath
  , executionChecks :: [CheckResult]
  , executionInputs :: [FilePath]
  } deriving (Eq, Show)

-- | A preflight or report-I/O failure is Left. A launched job always retains
-- its termination status in the report: ordinary FAIL is data, whereas signals,
-- timeouts, launch errors, and reserved launcher exits are infrastructure errors.
-- The CLI must use hasInfrastructureFailure to decide its own exit status.
executeTest :: ExecutionConfig -> TestPlan -> String -> IO (Either String ExecutionReport)
executeTest options plan selector = catch (Right <$> execute) ioFailure
  where
    ioFailure :: IOException -> IO (Either String ExecutionReport)
    ioFailure err = pure (Left (show err))
    require = either (ioError . userError) pure
    execute = do
      require (validatePlan plan)
      selected <- require $ case [test | test <- plannedTests plan,
                                         renderIdentifier (testId test) == selector] of
        [test] -> Right test
        _ -> Left "execute requires the identifier of one planned test"
      checks <- require (internalChecksFor (planConfig plan) (testKind selected))
      let kind = testKind selected
          compilation = testCompilation kind
      require $ if compilationDependencies compilation then Right () else
        Left "nodeps execution is unsupported: it may depend on prior compiled artifacts"
      unless (executionTimeoutMicros options > 0) $
        ioError (userError "execution timeout must be positive")
      installation <- canonicalizePath (executionInstallation options)
      suite <- canonicalizePath (executionSuite options)
      output <- canonicalizePath (executionOutput options)
      requireDirectory "compiler installation" installation
      requireDirectory "source snapshot" suite
      unless (disjoint suite output && disjoint installation output) $
        ioError (userError "output must be separate from the source snapshot and compiler installation")
      let library = installation </> "lib"
          compiler = installation </> "bin" </> "core" </> "bsc"
          objectReader = installation </> "bin" </> "core" </> "dumpbo"
          script = identifierTest (testId selected)
          scriptDirectory = takeDirectory script
      requireTool compiler
      forM_ checks $ \_ -> requireTool objectReader
      forM_ ["Prelude.bo", "PreludeBSV.bo"] $ \name -> do
        exists <- doesFileExist (library </> "Libraries" </> name)
        unless exists $ ioError (userError ("compiler installation lacks " ++ name))
      declaration <- readFile (suite </> script)
      current <- require (lowerPlan (planConfig plan) [(script, declaration)])
      unless (selected `elem` plannedTests current) $
        ioError (userError "selected test does not match the source snapshot's declaration")
      outputExists <- doesPathExist output
      if outputExists then do
        requireDirectory "output" output
        names <- listDirectory output
        unless (null names) $ ioError (userError "output directory must be fresh and empty")
      else createDirectoryIfMissing True output
      let workspace = output </> "work"
      inputs <- copySnapshot suite scriptDirectory workspace
      let working = workspace </> scriptDirectory
      requireDirectory "test source directory" working
      rejectArtifacts working
      environment <- toolEnvironment library
      let source = compilationSource compilation
          -- bsc expands BSC_OPTIONS before argv. Keep that effective ordering
          -- but use real arguments, since BSC_OPTIONS itself uses plain words.
          arguments = ["-i", library] ++ compilationOptions compilation ++
            ["-no-show-timestamps", "-no-show-version", "-u", source]
      compilationResult <- runTool (executionTimeoutMicros options) environment working
        compiler arguments (output </> (source ++ ".bsc-out"))
      compilationCheck <- observeCompilation kind compilationResult
      actualChecks <- require (internalChecksAfter (planConfig plan) kind
        (processStatus compilationResult == ProcessExited 0))
      internal <- forM actualChecks $ \(ObjectLoads objectFile) -> do
        result <- runTool (executionTimeoutMicros options) environment working
          objectReader [objectFile] (output </> (objectFile ++ ".dumpbo-out"))
        pure (CheckResult "object-load" (disposition True (processStatus result)) result Nothing)
      let report = ExecutionReport (testId selected) (planConfig plan) installation working
                     (compilationCheck : internal) inputs
      writeFile (output </> "result.json") (encodeExecutionReport report)
      pure report

-- compile_fail_error reports one diagnostic assertion after ordinary failure;
-- it does not first emit a compilation PASS. Unexpected success instead emits
-- a compilation FAIL, followed by the conditional internal checks above.
observeCompilation :: TestKind -> ProcessResult -> IO CheckResult
observeCompilation kind result = case kind of
  CompilationTest _ expectation -> pure (CheckResult "compilation"
    (disposition (expectation == CompileSucceeds) status) result Nothing)
  CompilationErrorTest _ expected -> case status of
    ProcessExited code | code > 0 && code < 126 -> do
      -- Byte-preserving input avoids locale decoding failures from source text
      -- echoed in diagnostics. Only ASCII tag/header syntax is inspected.
      transcript <- withBinaryFile (processTranscript result) ReadMode $ \handle ->
        hSetEncoding handle latin1 >> hGetContents' handle
      let actual = countErrorDiagnostics expected transcript
          verdict = if actual == expectedErrorCount expected then CheckPass else CheckFail
      pure (CheckResult "diagnostic-count" verdict result
        (Just (DiagnosticResult expected actual)))
    _ -> pure (CheckResult "compilation" (disposition False status) result Nothing)
  where status = processStatus result

-- | Literal-tag subset of Tcl's regexp -all -line {Error:.+\(TAG\)$}.
-- It is not anchored at the start, requires at least one character before the
-- tag, and matches at most once per line. Tcl's default text-channel newline
-- translation also treats CRLF and bare CR as line endings. Regex tags are
-- rejected by Procedures, so no general regex interpretation is needed here.
countErrorDiagnostics :: ExpectedError -> String -> Int
countErrorDiagnostics expected = length . filter matches . lines . newlines
  where
    suffix = "(" ++ expectedErrorTag expected ++ ")"
    matches line = suffix `isSuffixOf` line &&
      any (\rest -> "Error:" `isPrefixOf` rest && length rest > length "Error:")
        (tails (take (length line - length suffix) line))
    newlines ('\r':'\n':rest) = '\n' : newlines rest
    newlines ('\r':rest) = '\n' : newlines rest
    newlines (c:rest) = c : newlines rest
    newlines [] = []

requireDirectory :: String -> FilePath -> IO ()
requireDirectory label path = do
  exists <- doesDirectoryExist path
  unless exists $ ioError (userError (label ++ " is not a directory: " ++ path))

requireTool :: FilePath -> IO ()
requireTool path = do
  exists <- doesFileExist path
  unless exists $ ioError (userError ("missing installation tool: " ++ path))
  permissions <- getPermissions path
  unless (executable permissions) $ ioError (userError ("installation tool is not executable: " ++ path))

inside :: FilePath -> FilePath -> Bool
inside root path = let relative = makeRelative root path
  in not (isAbsolute relative) && ".." `notElem` splitDirectories relative

disjoint :: FilePath -> FilePath -> Bool
disjoint a b = not (inside a b || inside b a)

-- The current corpus was audited to use each selected directory subtree and
-- its internal symlink targets. Preserve targets at their suite-relative paths;
-- do not guess additional dependencies by parsing Bluespec imports. Future
-- sources requiring other directories need an explicit wider input policy.
copySnapshot :: FilePath -> FilePath -> FilePath -> IO [FilePath]
copySnapshot root selected destination = do
  visited <- newIORef Set.empty
  inputs <- newIORef Set.empty
  let copy ancestors source target = do
        resolved <- canonicalizePath source
        unless (inside root resolved) $
          ioError (userError ("source snapshot symlink escapes its root: " ++ source))
        directory <- doesDirectoryExist resolved
        when (directory && Set.member resolved ancestors) $
          ioError (userError ("source snapshot contains a directory-link cycle: " ++ source))
        done <- Set.member target <$> readIORef visited
        unless done $ do
          modifyIORef' visited (Set.insert target)
          modifyIORef' inputs (Set.insert (makeRelative root source))
          createDirectoryIfMissing True (takeDirectory target)
          symbolic <- pathIsSymbolicLink source
          if symbolic then do
            let copiedTarget = destination </> makeRelative root resolved
            copy ancestors resolved copiedTarget
            let link = relativeLink (takeDirectory target) copiedTarget
            if directory then createDirectoryLink link target else createFileLink link target
          else if directory then do
            createDirectoryIfMissing True target
            names <- sort <$> listDirectory resolved
            forM_ names $ \name -> copy (Set.insert resolved ancestors)
              (resolved </> name) (target </> name)
          else do
            file <- doesFileExist resolved
            unless file $ ioError (userError ("source snapshot contains a missing or non-file entry: " ++ source))
            copyFile resolved target
  createDirectory destination
  copy Set.empty (root </> selected) (destination </> selected)
  Set.toAscList <$> readIORef inputs

relativeLink :: FilePath -> FilePath -> FilePath
relativeLink from to = joinPath (replicate (length left) ".." ++ right)
  where
    (left,right) = removeCommon (splitDirectories from) (splitDirectories to)
    removeCommon (a:as) (b:bs) | a == b = removeCommon as bs
    removeCommon as bs = (as,bs)

rejectArtifacts :: FilePath -> IO ()
rejectArtifacts root = do
  names <- sort <$> listDirectory root
  forM_ names $ \name -> do
    let path = root </> name
    directory <- doesDirectoryExist path
    if directory then rejectArtifacts path else
      when (takeExtension name `elem` [".bo", ".bi", ".ba", ".o", ".so", ".a"]) $
        ioError (userError ("source snapshot contains prior compiler artifacts: " ++ path))

toolEnvironment :: FilePath -> IO [(String,String)]
toolEnvironment library = pure
  [("PATH", "/usr/bin:/bin"), ("LANG", "C"), ("LC_ALL", "C"),
   ("BSC_OPTIONS", ""), ("BLUESPECDIR", library),
   ("LD_LIBRARY_PATH", library </> "SAT"), ("DYLD_LIBRARY_PATH", library </> "SAT")]

-- One binary handle receives both streams, retaining the process's observed
-- merged ordering without introducing a shell or separately buffered pipes.
runTool :: Int -> [(String,String)] -> FilePath -> FilePath -> [String] -> FilePath -> IO ProcessResult
runTool limit environment working program arguments transcript = do
  result <- try $ withBinaryFile transcript WriteMode $ \handle -> mask $ \restore -> do
    (_,_,_,process) <- createProcess (proc program arguments)
      { cwd = Just working, env = Just environment, std_in = NoStream
      , std_out = UseHandle handle, std_err = UseHandle handle
      , close_fds = True, create_group = True }
    let stop = do
          pid <- getPid process
          forM_ pid $ \group -> catch (signalProcessGroup sigKILL group) ignoreIO
          waitForProcess process
        -- waitForProcess can block the only runtime capability when the
        -- embedding program was linked without -threaded. Nonblocking polls
        -- let the timeout fire in either runtime mode.
        awaitExit = do
          status <- getProcessExitCode process
          case status of
            Just code -> pure code
            Nothing -> threadDelay 10000 >> awaitExit
    status <- restore (timeout limit awaitExit) `onException` stop
    case status of
      Nothing -> ProcessTimedOut . exitNumber <$> stop
      Just ExitSuccess -> pure (ProcessExited 0)
      Just (ExitFailure code)
        | code < 0 -> pure (ProcessSignaled (-code))
        | otherwise -> pure (ProcessExited code)
  pure (ProcessResult program arguments transcript
    (either (ProcessLaunchFailed . show) id (result :: Either IOException ProcessStatus)))
  where
    ignoreIO :: IOException -> IO ()
    ignoreIO _ = pure ()
    exitNumber ExitSuccess = 0
    exitNumber (ExitFailure code) = code

disposition :: Bool -> ProcessStatus -> CheckDisposition
disposition expectSuccess (ProcessExited code)
  | code >= 126 = CheckInfrastructureError
  | (code == 0) == expectSuccess = CheckPass
  | otherwise = CheckFail
disposition _ _ = CheckInfrastructureError

hasInfrastructureFailure :: ExecutionReport -> Bool
hasInfrastructureFailure = any ((== CheckInfrastructureError) . checkDisposition) . executionChecks

-- Stable, explicit wire names. Transcripts remain raw bytes in separate files;
-- their contents are never escaped, truncated, or conflated with verdicts.
encodeExecutionReport :: ExecutionReport -> String
encodeExecutionReport report = object
  [("schema", string "bsc-test-execution"), ("version", "1"),
   ("id", object [("test", string (identifierTest (executionId report))),
                  ("number", show (identifierNumber (executionId report)))]),
   ("configuration", object [("name", string (configName config)),
      ("internal_checks", if configInternalChecks config then "true" else "false"),
      ("compiler_options", array (map string (configCompilerOptions config)))]),
   ("installation", string (executionBoundInstallation report)),
   ("working_directory", string ("work" </> takeDirectory (identifierTest (executionId report)))),
   ("staged_inputs", array (map string (executionInputs report))),
   ("checks", array (map encodeCheck (executionChecks report)))] ++ "\n"
  where
    config = executionReportConfig report
    encodeCheck check = object $
      [("role", string (checkRole check)),
       ("disposition", string (case checkDisposition check of
          CheckPass -> "PASS"; CheckFail -> "FAIL"; CheckInfrastructureError -> "INFRASTRUCTURE_ERROR")),
       ("process", encodeProcess (checkProcess check))] ++
      maybe [] (\diagnostic -> [("diagnostic", object
        [("error_tag", string (expectedErrorTag (diagnosticExpected diagnostic))),
         ("expected_count", show (expectedErrorCount (diagnosticExpected diagnostic))),
         ("actual_count", show (diagnosticActualCount diagnostic))])]) (checkDiagnostic check)
    encodeProcess result = object
      [("program", string (processProgram result)), ("arguments", array (map string (processArguments result))),
       ("transcript", string (takeFileName (processTranscript result))), ("termination", termination (processStatus result))]
    termination (ProcessExited code) = object [("kind", string "exited"), ("exit_code", show code)]
    termination (ProcessSignaled signal) = object [("kind", string "signal"), ("signal", show signal), ("exit_code", show (-signal))]
    termination (ProcessTimedOut code) = object [("kind", string "timeout"), ("exit_code", show code)]
    termination (ProcessLaunchFailed reason) = object [("kind", string "launch-error"), ("reason", string reason)]

object :: [(String,String)] -> String
object fields = "{" ++ intercalate "," [string key ++ ":" ++ value | (key,value) <- fields] ++ "}"
array :: [String] -> String
array values = "[" ++ intercalate "," values ++ "]"
string :: String -> String
string value = '"' : concatMap escape value ++ "\""
  where
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape '\n' = "\\n"
    escape '\r' = "\\r"
    escape '\t' = "\\t"
    escape c | ord c < 32 = let hex = showHex (ord c) "" in "\\u" ++ replicate (4-length hex) '0' ++ hex
             | otherwise = [c]
