module ExecuteTest (runTests) where

import Control.Exception (bracket)
import Control.Monad (unless)
import Data.Either (isLeft)
import Data.List (isInfixOf)
import Execute
import Lower (lowerPlan)
import System.Directory
  ( createDirectory, createDirectoryIfMissing, createDirectoryLink, createFileLink
  , doesFileExist, executable, getPermissions, getTemporaryDirectory
  , removeFile, removePathForcibly, setPermissions )
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>), takeDirectory)
import System.IO (hClose, openTempFile)
import TestPlan

runTests :: IO ()
runTests = withFixture $ \fixture -> do
  let selected = fixtureSuite fixture </> takeDirectory script
      core = fixtureInstallation fixture </> "bin/core/bsc"
      library = fixtureInstallation fixture </> "lib"
  withVariable "BSC_OPTIONS" "-deliberately-invalid-inherited-option" $
    withVariable "GHCRTS" "-M1k" $ do
    report <- runCase fixture "pass" config "compile_pass Good.bs {-v -let-gen -v}"
    check "successful compilation includes its internal object-load verdict"
      (map checkRole (executionChecks report) == ["compilation", "object-load"] &&
       all ((== CheckPass) . checkDisposition) (executionChecks report))
    args <- lines <$> readFile (executionWorkingDirectory report </> "command.args")
    check "effective argument order and boundaries are preserved"
      (args == ["-i",library,"-v","-let-gen","-v","-no-show-timestamps","-no-show-version","-u","Good.bs"])
    env <- lines <$> readFile (executionWorkingDirectory report </> "command.env")
    check "inherited compiler and RTS options cannot alter the fixed execution environment"
      (env == ["",library,"unset","/usr/bin:/bin","C","C"])
    transcript <- readFile (processTranscript (checkProcess (head (executionChecks report))))
    check "stdout and stderr share one ordered raw transcript" (transcript == "stdout one\nstderr two\n")
    originalObject <- doesFileExist (selected </> "Good.bo")
    stagedObject <- doesFileExist (executionWorkingDirectory report </> "Good.bo")
    check "compilation artifacts only affect the private workspace" (not originalObject && stagedObject)
    saved <- readFile (fixtureRoot fixture </> "pass/result.json")
    check "saved report is the stable report encoding"
      (saved == encodeExecutionReport report && "\"number\":1" `isInfixOf` saved)
  failed <- runCase fixture "failed-compile" config "compile_pass Fail.bs"
  check "ordinary compilation failure still runs the internal check and remains verdict data"
    (map checkDisposition (executionChecks failed) == [CheckFail,CheckFail] &&
     map checkRole (executionChecks failed) == ["compilation","object-load"] &&
     not (hasInfrastructureFailure failed))
  expected <- runCase fixture "expected-failure" config "compile_fail Fail.bs"
  check "normal nonzero compilation exit satisfies compile_fail without an internal check"
    (map checkDisposition (executionChecks expected) == [CheckPass] &&
     processStatus (checkProcess (head (executionChecks expected))) == ProcessExited 1)
  unexpected <- runCase fixture "unexpected-success" config "compile_fail Good.bs"
  check "unexpected compilation success is FAIL data" (map checkDisposition (executionChecks unexpected) == [CheckFail])
  withoutInternal <- runCase fixture "without-internal" config { configInternalChecks = False } "compile_pass Good.bs"
  check "internal-check policy controls the additional observation" (map checkRole (executionChecks withoutInternal) == ["compilation"])
  signaled <- runCase fixture "signal" config "compile_fail Signal.bs"
  check "signals cannot satisfy an expected compilation failure"
    (hasInfrastructureFailure signaled && processStatus (checkProcess (head (executionChecks signaled))) == ProcessSignaled 15)
  reserved <- runCase fixture "reserved-exit" config "compile_fail Exit127.bs"
  check "reserved launcher exit codes are infrastructure errors" (hasInfrastructureFailure reserved)
  timeoutPlan <- prepare fixture config "compile_fail Timeout.bs"
  timed <- executeTest (settings fixture "timeout") { executionTimeoutMicros = 100000 } timeoutPlan selector >>= requireReport
  check "timeouts cannot satisfy an expected compilation failure"
    (hasInfrastructureFailure timed && case processStatus (checkProcess (head (executionChecks timed))) of
      ProcessTimedOut code -> code < 0
      _ -> False)
  writeExecutable core "#!/definitely/missing/interpreter\n"
  launch <- runCase fixture "launch-failure" config "compile_fail Fail.bs"
  check "tool launch failure cannot satisfy an expected compilation failure"
    (hasInfrastructureFailure launch && case processStatus (checkProcess (head (executionChecks launch))) of
       ProcessLaunchFailed _ -> True
       _ -> False)
  writeExecutable core compilerStub
  nodeps <- prepare fixture config "compile_pass Good.bs {} 1"
  refused <- executeTest (settings fixture "nodeps") nodeps selector
  check "nodeps is rejected before executing an isolated test" (isLeft refused)
  valid <- prepare fixture config "compile_pass Good.bs"
  writeFile (selected </> "old.bo") "stale artifact"
  stale <- executeTest (settings fixture "stale-artifact") valid selector
  check "prior compilation artifacts are rejected" (isLeft stale)
  removeFile (selected </> "old.bo")
  writeFile (fixtureSuite fixture </> script) "compile_pass Changed.bs\n"
  changed <- executeTest (settings fixture "changed-plan") valid selector
  check "source snapshot must agree with the selected saved declaration" (isLeft changed)
  _ <- prepare fixture config "compile_pass Good.bs"
  unknown <- executeTest (settings fixture "unknown") valid "unknown"
  check "only a planned identifier can execute" (isLeft unknown)
  overlap <- executeTest (settings fixture "overlap") { executionOutput = selected </> "output" } valid selector
  check "output cannot overlap read-only source inputs" (isLeft overlap)
  writeFile (fixtureRoot fixture </> "outside.defines") "outside"
  createFileLink (fixtureRoot fixture </> "outside.defines") (selected </> "escape.defines")
  escape <- executeTest (settings fixture "escape") valid selector
  check "source symlinks cannot escape the supplied snapshot" (isLeft escape)
  removeFile (selected </> "escape.defines")
  createDirectoryLink selected (selected </> "cycle")
  cycleResult <- executeTest (settings fixture "cycle") valid selector
  check "source directory-link cycles are rejected" (isLeft cycleResult)
  removeFile (selected </> "cycle")
  createDirectory (fixtureSuite fixture </> "shared")
  writeFile (fixtureSuite fixture </> "shared/header.defines") "shared include"
  createDirectoryLink "../shared" (fixtureSuite fixture </> "bsc.execute/config")
  linked <- runCase fixture "linked-input" config "compile_pass Good.bs"
  copied <- readFile (fixtureRoot fixture </> "linked-input/work/shared/header.defines")
  throughLink <- readFile (executionWorkingDirectory linked </> "config/header.defines")
  check "internal symlink targets retain the source snapshot layout"
    (copied == "shared include" && throughLink == copied &&
     "shared/header.defines" `elem` executionInputs linked)
  putStrLn "Semantic execution, process failures, isolated inputs, and report tests passed."

check :: String -> Bool -> IO ()
check label value = unless value (fail ("ExecuteTest: " ++ label))

config :: PlanConfig
config = PlanConfig "executor-test" True []
script, selector :: String
script = "bsc.execute/example.exp"
selector = renderIdentifier (Identifier script 1)

data Fixture = Fixture
  { fixtureRoot :: FilePath, fixtureInstallation :: FilePath, fixtureSuite :: FilePath }

settings :: Fixture -> String -> ExecutionConfig
settings fixture name = ExecutionConfig (fixtureInstallation fixture) (fixtureSuite fixture)
  (fixtureRoot fixture </> name) defaultExecutionTimeoutMicros

prepare :: Fixture -> PlanConfig -> String -> IO TestPlan
prepare fixture policy source = do
  writeFile (fixtureSuite fixture </> script) (source ++ "\n")
  either fail pure (lowerPlan policy [(script,source ++ "\n")])

runCase :: Fixture -> String -> PlanConfig -> String -> IO ExecutionReport
runCase fixture name policy source = do
  plan <- prepare fixture policy source
  executeTest (settings fixture name) plan selector >>= requireReport

requireReport :: Either String ExecutionReport -> IO ExecutionReport
requireReport = either fail pure

withFixture :: (Fixture -> IO a) -> IO a
withFixture action = bracket make (removePathForcibly . fixtureRoot) action
  where
    make = do
      tmp <- getTemporaryDirectory
      (root,handle) <- openTempFile tmp "bsc-executor-test"
      hClose handle
      removeFile root
      createDirectory root
      let installation = root </> "installation with spaces"
          suite = root </> "snapshot"
      createDirectoryIfMissing True (installation </> "bin/core")
      createDirectoryIfMissing True (installation </> "lib/Libraries")
      createDirectoryIfMissing True (installation </> "lib/SAT")
      createDirectoryIfMissing True (suite </> "bsc.execute")
      writeFile (installation </> "lib/Libraries/Prelude.bo") "stub prelude"
      writeFile (installation </> "lib/Libraries/PreludeBSV.bo") "stub prelude"
      writeExecutable (installation </> "bin/core/bsc") compilerStub
      writeExecutable (installation </> "bin/core/dumpbo") objectStub
      writeFile (suite </> "bsc.execute/Good.bs") "package Good where\n"
      writeFile (suite </> "bsc.execute/Fail.bs") "deliberate invalid input\n"
      pure (Fixture root installation suite)

writeExecutable :: FilePath -> String -> IO ()
writeExecutable path contents = do
  writeFile path contents
  permissions <- getPermissions path
  setPermissions path permissions { executable = True }

withVariable :: String -> String -> IO a -> IO a
withVariable name value action = bracket (lookupEnv name) restore (\_ -> setEnv name value >> action)
  where restore Nothing = unsetEnv name
        restore (Just prior) = setEnv name prior

compilerStub :: String
compilerStub = unlines
  [ "#!/bin/sh"
  , "printf '%s\\n' \"$@\" > command.args"
  , "printf '%s\\n' \"$BSC_OPTIONS\" \"$BLUESPECDIR\" \"${GHCRTS-unset}\" \"$PATH\" \"$LANG\" \"$LC_ALL\" > command.env"
  , "printf 'stdout one\\n'"
  , "printf 'stderr two\\n' >&2"
  , "for source in \"$@\"; do :; done"
  , "case \"$source\" in"
  , "  Fail.bs) exit 1 ;;"
  , "  Signal.bs) kill -TERM \"$$\" ;;"
  , "  Timeout.bs) exec /bin/sleep 5 ;;"
  , "  Exit127.bs) exit 127 ;;"
  , "esac"
  , "printf 'object\\n' > \"${source%.*}.bo\""
  , "exit 0"
  ]

objectStub :: String
objectStub = unlines
  [ "#!/bin/sh"
  , "printf 'object check %s\\n' \"$1\""
  , "test -f \"$1\""
  ]
