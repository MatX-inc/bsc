-- | Semantic test procedures shared by source adapters and the executor.
-- The vocabulary describes what is tested, not how Buck2 runs it.
--
-- Tcl procedure boundaries are a useful reference, not a required Haskell
-- module/function structure. compile_pass and compile_fail share one
-- compilation-test constructor here. Internal checks are obligations of that
-- test, rather than extra top-level tests or a preselected action graph.
--
-- This first vocabulary covers package compilation with exit-status
-- expectations. Diagnostic matching, backend tests, expected-failure phases,
-- and stateful recompilation sequences still need their own semantics.
module Procedures
  ( compilePass, compileFail, compilationTest
  , InternalCheck(..), internalChecksFor
  , supportedProcedures, supportedCompilerOptions, validateProcedureConfig, explainTest
  ) where

import Control.Monad (unless)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.List (intercalate)
import System.FilePath (dropExtension, takeDirectory, takeExtension)
import Tcl (SourcePos(..))
import TestPlan

-- | Only these procedures currently produce semantic tests and reserve a
-- file-local invocation number. Other harness helpers remain unnumbered.
supportedProcedures :: [String]
supportedProcedures = ["compile_pass", "compile_fail"]

-- These flags do not redirect artifacts or select a backend. Expanding this
-- set requires describing the resulting test semantics, not only passing an
-- additional string to the compiler.
supportedCompilerOptions :: [String]
supportedCompilerOptions = ["-let-gen", "-no-let-gen", "-dinternal", "-v"]

validateProcedureConfig :: PlanConfig -> Either String ()
validateProcedureConfig config = do
  validateConfig config
  mapM_ checkOption (configCompilerOptions config)

checkOption :: String -> Either String ()
checkOption option = unless (option `elem` supportedCompilerOptions) $
  Left ("unsupported compiler option: " ++ show option)

compilePass, compileFail
  :: PlanConfig -> Identifier -> SourcePos -> Compilation -> Either String Test
compilePass = compilationTest CompileSucceeds
compileFail = compilationTest CompileFails

-- | Construct a compilation test from invocation-local options. Configuration
-- options precede them, preserving order and duplicates. The Bool in
-- Compilation means compile dependencies (-u), not Tcl's inverted nodeps flag.
-- Missing sources are valid observations for negative compilation tests.
compilationTest :: Expectation -> PlanConfig -> Identifier -> SourcePos
                -> Compilation -> Either String Test
compilationTest expectation config identifier origin compilation = do
  validateProcedureConfig config
  validateCompilation compilation
  let effective = compilation
        { compilationOptions = configCompilerOptions config ++ compilationOptions compilation }
      test = Test identifier origin (CompilationTest effective expectation)
  unless (identifierNumber identifier > 0) $ Left "invalid test invocation number"
  -- A constructor validates one invocation, which may occur after earlier
  -- tests. Contiguous numbering is checked once the whole plan is assembled.
  validatePlan (TestPlan config [ScriptPlan (identifierTest identifier)
    [Planned test { testId = identifier { identifierNumber = 1 } }]])
  pure test

-- The saved model admits more paths and options than this first procedure
-- implementation can interpret. Check this boundary for explanations too, so
-- a hand-authored plan with e.g. -bdir is not given incorrect artifact names.
validateCompilation :: Compilation -> Either String ()
validateCompilation compilation = do
  unless (validSource (compilationSource compilation)) $ Left
    "only a .bs or .bsv source basename containing letters, digits, '.', '_' or '-' is supported"
  mapM_ checkOption (compilationOptions compilation)

validSource :: FilePath -> Bool
validSource source = not (null source) && head source /= '-' &&
  takeExtension source `elem` [".bs", ".bsv"] && all allowed source &&
  source /= ".bs" && source /= ".bsv"
  where
    allowed c = isAsciiLower c || isAsciiUpper c || isDigit c || c `elem` "._-"

-- | Extra semantic obligations of a test. They remain associated with the
-- compilation being checked; they do not choose scheduling or cache policy.
data InternalCheck = ObjectLoads FilePath deriving (Eq, Show)

-- compile_pass calls check_intermediate_files after reporting its compilation
-- result, even if compilation unexpectedly failed. compile_fail does not.
-- Expected-failure variants are not yet represented and must stay unsupported.
internalChecksFor :: PlanConfig -> TestKind -> Either String [InternalCheck]
internalChecksFor config (CompilationTest compilation expectation) = do
  validateProcedureConfig config
  validateCompilation compilation
  pure $ if configInternalChecks config && expectation == CompileSucceeds
    then [ObjectLoads (dropExtension (compilationSource compilation) ++ ".bo")]
    else []

-- | Explain a semantic test or an unsupported item. This deliberately does not
-- invent backend actions, workspace snapshots, or cache guarantees.
explainTest :: TestPlan -> String -> Either String String
explainTest plan selector = do
  validatePlan plan
  case [item | script <- planScripts plan, item <- scriptItems script,
               Just identifier <- [itemId item], renderIdentifier identifier == selector] of
    [Planned test] -> do
      checks <- internalChecksFor config (testKind test)
      Right (unlines (describe test checks))
    [Unplanned issue] -> Right (unlines
      [ "Item " ++ selector
      , "Status: " ++ case issueKind issue of
          UnsupportedConstruct -> "unsupported"
          UnresolvedDependency -> "unresolved"
      , "Origin: " ++ showOrigin (issuePosition issue)
      , "Construct: " ++ issueConstruct issue
      , "Reason: " ++ issueReason issue
      ])
    _ -> Left ("unknown test or item: " ++ selector)
  where
    config = planConfig plan
    itemId (Planned test) = Just (testId test)
    itemId (Unplanned issue) = issueId issue
    describe test checks = case testKind test of
      CompilationTest compilation expectation ->
        [ "Test " ++ selector
        , "Configuration: " ++ configName config
        , "Kind: package compilation"
        , "Origin: " ++ showOrigin (testOrigin test)
        , "Test directory: " ++ takeDirectory (identifierTest (testId test))
        , "Source: " ++ compilationSource compilation ++ " (including absence)"
        , "Compiler options: " ++ list (compilationOptions compilation)
        , "Compile dependencies: " ++ if compilationDependencies compilation then "yes (-u)" else "no"
        , "Compiler version and timestamp text are suppressed."
        , "Observe exit status and merged transcript: " ++ compilationSource compilation ++ ".bsc-out"
        , "Expectation: " ++ case expectation of
            CompileSucceeds -> "compilation succeeds"
            CompileFails -> "compilation fails"
        ] ++ describeInternal checks ++
        [ "Execution, input/tool binding, and cache policy are not assigned by this plan." ]
    describeInternal [] = ["Internal checks: none"]
    describeInternal checks =
      ["Internal check: " ++ path ++ " can be loaded by dumpbo; run even if compilation fails."
      | ObjectLoads path <- checks]
    list [] = "(none)"
    list values = intercalate " " values
    showOrigin p = sourceFile p ++ ":" ++ show (sourceLine p) ++ ":" ++ show (sourceColumn p)
