{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers #-}

-- | The setup hooks shared by every Hooks package of the bsc build.
--
-- Each component package (bsc-core, bsc-bo, ... and the facade bsc) is a
-- @build-type: Hooks@ package whose three-line SetupHooks.hs re-exports
-- 'bscSetupHooks'. The hooks run with the package directory,
-- components/<short>/, as the working directory, so the repository root is
-- two levels up ('repoRoot').
--
-- What runs where (doc/recabalization-brief.md, sections 3.1 and 3.4):
--
-- * in every package: the GHC version guard, and a generated hidden Warmup
--   for each library and executable component;
--
-- * in bsc-core only, on its main library: the generated BuildSystem and
--   BuildVersion modules and the Tcl link configuration;
--
-- * in each solver binding package (bsc-stp, bsc-yices), on its main
--   library: the build of that vendored solver and its link configuration.
--
-- No hook writes into src/comp or anywhere else outside dist-newstyle and
-- the components' autogen directories; the make build generates its own
-- Warmup.hs and BuildVersion.hs as before.
module BscSetupHooks (bscSetupHooks) where

import Control.Monad (forM, forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isSpace)
import Data.List (find, isPrefixOf, nub, sort)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Distribution.Compiler (CompilerFlavor (..))
import Distribution.InstalledPackageInfo (ExposedModule (..))
import qualified Distribution.InstalledPackageInfo as IPI
import Distribution.ModuleName (ModuleName)
import Distribution.Package (mkPackageName, pkgName)
import Distribution.Pretty (prettyShow)
import Distribution.Simple.Compiler (compilerFlavor, compilerVersion)
import Distribution.Simple.Configure (getInstalledPackages)
import Distribution.Simple.LocalBuildInfo
  ( hostPlatform,
    interpretSymbolicPathLBI,
    localPkgDescr,
    mbWorkDirLBI,
    withPackageDB,
    withPrograms,
  )
import qualified Distribution.Simple.LocalBuildInfo as LBI
import Distribution.Simple.PackageIndex (lookupUnitId)
import Distribution.Simple.Setup (fromFlagOrDefault)
import Distribution.Simple.SetupHooks
import Distribution.Simple.Utils (die')
import Distribution.System (OS (..))
import qualified Distribution.Types.LocalBuildConfig as LBC
import Distribution.Utils.Path
  ( makeSymbolicPath,
    moduleNameSymbolicPath,
    (<.>),
  )
import Distribution.Verbosity (normal)
import Distribution.Version (mkVersion)
import System.Directory
  ( canonicalizePath,
    createDirectoryIfMissing,
    doesFileExist,
    makeAbsolute,
  )
import System.Environment (getEnvironment, lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.IO (readFile')
import System.Process
  ( CreateProcess (..),
    callProcess,
    createProcess,
    proc,
    readProcess,
    waitForProcess,
  )

bscSetupHooks :: SetupHooks
bscSetupHooks =
  compilerSetupHooks
    <> warmupSetupHooks
    <> generatedModulesSetupHooks
    <> solverSetupHooks
    <> tclSetupHooks

-- | The repository root as seen from the package directory the hooks run
-- in: components/<short>/ is two levels below it.
repoRoot :: FilePath
repoRoot = ".." </> ".."

-- | Run a process to completion, failing if it does.
--
-- process-1.6.28 exports this as callCreateProcess, but GHC 9.14.1 bundles
-- 1.6.26, and a floor that excludes the bundled version makes cabal build
-- process from Hackage. Spelling it out keeps the hooks on the compiler's
-- own bundled process.
callCreateProcess :: CreateProcess -> IO ()
callCreateProcess cp = do
  (_, _, _, handle) <- createProcess cp
  code <- waitForProcess handle
  case code of
    ExitSuccess -> pure ()
    ExitFailure n -> ioError (userError ("setup command failed with " <> show n))

isMainLib :: Component -> Bool
isMainLib (CLib Library {libName = LMainLibName}) = True
isMainLib _ = False

-- | Whether the hooks are running in the bsc-core package: the home of the
-- generated BuildSystem and BuildVersion modules, the vendored C sources,
-- and the Tcl link configuration. It is its own package because
-- cabal-install builds a Hooks package as a single unit and allows no
-- sublibraries in it (brief, F1); the other components and the facade with
-- the executables are Hooks packages beside it that share these hooks.
isBscCore :: PackageDescription -> Bool
isBscCore pd = pkgName (package pd) == mkPackageName "bsc-core"

-- | Run the action only if the target files don't already exist.
needing :: [FilePath] -> IO () -> IO ()
needing targets act = do
  exists <- and <$> mapM doesFileExist targets
  unless exists $ do
    let nub' = fmap NE.head . NE.group . sort
    forM_ (nub' (takeDirectory <$> targets)) (createDirectoryIfMissing True)
    act

-- | Write the file only if its content changes, so an unchanged generated
-- module keeps its timestamp.
writeFileChanged :: FilePath -> String -> IO ()
writeFileChanged path new = do
  createDirectoryIfMissing True (takeDirectory path)
  exists <- doesFileExist path
  same <- if exists then (== new) <$> readFile' path else pure False
  unless same (writeFile path new)

-- | The autogen location of a generated module of the component.
autogenLocation :: LocalBuildInfo -> ComponentLocalBuildInfo -> ModuleName -> Location
autogenLocation lbi clbi m =
  Location (autogenComponentModulesDir lbi clbi) (moduleNameSymbolicPath m <.> "hs")

-- | A location as the file path the action that writes it uses.
locationFilePath :: LocalBuildInfo -> Location -> FilePath
locationFilePath lbi loc = interpretSymbolicPathLBI lbi (location loc)

-- | The hook that refuses a compiler older than GHC 9.14.
--
-- The experiment pins GHC 9.14 (brief, D7), and every package also carries
-- @if impl(ghc < 9.14) buildable: False@. The package-level pre-configure
-- hook is the earliest hook there is, so a wrong compiler is one readable
-- error up front rather than a mysterious one later in the build.
compilerSetupHooks :: SetupHooks
compilerSetupHooks = noSetupHooks {configureHooks}
  where
    configureHooks = noConfigureHooks {preConfPackageHook}

    preConfPackageHook :: Maybe PreConfPackageHook
    preConfPackageHook = Just $ \inputs -> do
      let comp = inputs.compiler
          verbosity = fromFlagOrDefault normal (configVerbosity inputs.configFlags)
          supported =
            compilerFlavor comp == GHC && compilerVersion comp >= mkVersion [9, 14]
      unless supported $
        die' verbosity $
          "bsc requires GHC 9.14 or newer; found " <> prettyShow (compilerId comp)
      pure (noPreConfPackageOutputs inputs)

-- | Which components get a generated Warmup: every library, main or named,
-- and every executable. Test suites (and benchmarks and foreign libraries)
-- get none.
wantsWarmup :: Component -> Bool
wantsWarmup CLib {} = True
wantsWarmup CExe {} = True
wantsWarmup _ = False

-- | The hooks to generate each component's Warmup module.
--
-- Warmup imports every exposed module of every package the component
-- directly depends on, so that the whole external interface set enters
-- GHC's EPS in one deterministic order before any other module of the
-- component compiles; without it, @ghc --make -jN@ object output varies
-- with scheduling (rule visibility and rule-overlap order both depend on
-- interface-load order; see GHC.Core.Rules, Note [Overall plumbing for
-- rules]). Every component root imports it (brief, 3.4), so the barrier
-- holds for the whole unit. The module is generated against this build's
-- compiler and this build's resolved dependencies, read from the package
-- databases at build time, not from the Makefile's package list.
--
-- Only the DIRECT dependencies are used: the transitive closure would name
-- modules of packages GHC is not given, which it refuses as hidden. In-place
-- packages count, so a child's Warmup covers its parents (bsc-core's modules
-- enter bsc-typecheck's Warmup, the facade's enter the executables'), and
-- bsc-core's public Warmup is excluded by name so that no child imports the
-- compatibility module into its own Warmup.
warmupSetupHooks :: SetupHooks
warmupSetupHooks = noSetupHooks {configureHooks, buildHooks}
  where
    configureHooks = noConfigureHooks {preConfComponentHook}
    buildHooks = noBuildHooks {preBuildComponentRules}

    -- Declare that the module is generated. The .cabal file lists it under
    -- other-modules (exposed-modules in bsc-core), as Cabal requires of an
    -- autogen module.
    preConfComponentHook :: Maybe PreConfComponentHook
    preConfComponentHook = Just $ \inputs ->
      if wantsWarmup inputs.component
        then
          pure $
            PreConfComponentOutputs
              { componentDiff =
                  buildInfoComponentDiff
                    (componentName inputs.component)
                    (emptyBuildInfo {autogenModules = ["Warmup"]})
              }
        else pure $ noPreConfComponentOutputs inputs

    -- Generate the module.
    preBuildComponentRules :: Maybe PreBuildComponentRules
    preBuildComponentRules = Just . rules (static ()) $ \env -> do
      let lbi = env.localBuildInfo
          clbi = env.targetInfo.targetCLBI
          verbosity = buildingWhatVerbosity env.buildingWhat
      when (wantsWarmup (targetComponent env.targetInfo)) $ do
        -- The package databases are read here, at build time, not taken from
        -- the configure-time snapshot in installedPkgs lbi. cabal-install does
        -- not reconfigure a package when a dependency's exposed-modules
        -- change (an in-place unit id such as bsc-core-2026.1-inplace is
        -- stable), so the snapshot goes stale as soon as modules move between
        -- packages, and a Warmup generated from it imports modules its
        -- dependency no longer exposes. This computation re-runs on every
        -- build, after the dependencies were registered, so the index is
        -- current; and because the module list is an argument of the rule's
        -- command, Cabal re-runs the rule when the list changes. This is the
        -- body of Cabal's getInstalledPackagesById, with die' for its
        -- exception.
        index <-
          liftIO $
            getInstalledPackages
              verbosity
              (LBI.compiler lbi)
              (mbWorkDirLBI lbi)
              (withPackageDB lbi)
              (withPrograms lbi)
        -- The index holds the external packages and every in-place library
        -- built before this component, which Cabal's build order guarantees
        -- for the dependencies; a dependency missing from it would make an
        -- incomplete Warmup, so it is an error rather than a gap.
        direct <- liftIO . forM (componentPackageDeps clbi) $ \(unit, _) ->
          case lookupUnitId index unit of
            Just ipi -> pure ipi
            Nothing ->
              die' verbosity $
                "Warmup: dependency " <> prettyShow unit
                  <> " is not in the installed package index"
        let units = sort [prettyShow (IPI.installedUnitId ipi) | ipi <- direct]
            mods =
              sort . nub $
                [ m
                  | ipi <- direct,
                    e <- IPI.exposedModules ipi,
                    let m = prettyShow (exposedName e),
                    m /= "Warmup"
                ]
            warmup = autogenLocation lbi clbi "Warmup"
        registerRule_ "Warmup.hs" $
          staticRule
            ( mkCommand
                (static Dict)
                (static writeWarmupHs)
                (locationFilePath lbi warmup, units, mods)
            )
            []
            [warmup]

writeWarmupHs :: (FilePath, [String], [String]) -> IO ()
writeWarmupHs (path, units, mods) =
  writeFileChanged path . unlines $
    [ "-- Generated by bsc-setup (BscSetupHooks.hs); do not edit.",
      "-- An import of every exposed module of every direct dependency of this",
      "-- component, so that the external interface set enters GHC's EPS in one",
      "-- deterministic order before any other module compiles; see",
      "-- warmupSetupHooks in BscSetupHooks.hs for why.",
      "-- Direct dependencies, as resolved for this build:"
    ]
      <> ["--   " <> u | u <- units]
      <> ["module Warmup where"]
      <> ["import " <> m <> " ()" | m <- mods]

-- | The hooks to generate the BuildSystem and BuildVersion modules, on the
-- main library of bsc-core only.
generatedModulesSetupHooks :: SetupHooks
generatedModulesSetupHooks = noSetupHooks {configureHooks, buildHooks}
  where
    configureHooks = noConfigureHooks {preConfComponentHook}
    buildHooks = noBuildHooks {preBuildComponentRules}

    -- Declare that the modules are generated.
    preConfComponentHook :: Maybe PreConfComponentHook
    preConfComponentHook = Just $ \inputs ->
      if isBscCore (LBC.localPkgDescr inputs.packageBuildDescr)
        && isMainLib inputs.component
        then
          pure $
            PreConfComponentOutputs
              { componentDiff =
                  buildInfoComponentDiff
                    (componentName inputs.component)
                    (emptyBuildInfo {autogenModules = ["BuildSystem", "BuildVersion"]})
              }
        else pure $ noPreConfComponentOutputs inputs

    -- Generate the modules.
    preBuildComponentRules :: Maybe PreBuildComponentRules
    preBuildComponentRules = Just . rules (static ()) $ \env -> do
      let lbi = env.localBuildInfo
          clbi = env.targetInfo.targetCLBI
          buildSystem = autogenLocation lbi clbi "BuildSystem"
          buildVersion = autogenLocation lbi clbi "BuildVersion"
      when (isBscCore (localPkgDescr lbi) && isMainLib (targetComponent env.targetInfo)) $ do
        registerRule_ "BuildSystem.hs" $
          staticRule
            ( mkCommand
                (static Dict)
                (static writeBuildSystemHs)
                (locationFilePath lbi buildSystem, hostPlatform lbi)
            )
            []
            [buildSystem]
        registerRule_ "BuildVersion.hs" $
          staticRule
            ( mkCommand
                (static Dict)
                (static writeBuildVersionHs)
                (interpretSymbolicPathLBI lbi (autogenComponentModulesDir lbi clbi))
            )
            []
            [buildVersion]

writeBuildSystemHs :: (FilePath, Platform) -> IO ()
writeBuildSystemHs (path, Platform _ os) = needing [path] $ do
  binFmtType <- case os of
    Linux -> pure "ELF"
    OSX -> pure "MachO"
    _ -> ioError (userError ("unsupported OS: " <> show os))
  writeFile path . unlines $
    [ "module BuildSystem",
      "  ( BinFmtType(..),",
      "    binFmtToString,",
      "    getBinFmtType,",
      "  )",
      "where",
      "",
      "data BinFmtType = ELF | MachO",
      "",
      "binFmtToString :: BinFmtType -> String",
      "binFmtToString ELF   = \"ELF\"",
      "binFmtToString MachO = \"Mach-O\"",
      "",
      "getBinFmtType :: BinFmtType",
      "getBinFmtType = " <> binFmtType
    ]

-- | Run update-build-version.sh in the autogen directory.
--
-- The script writes BuildVersion.hs into its working directory and needs
-- only git, which finds the repository from any directory inside it, so
-- the autogen directory is where it runs and nothing is copied: the autogen
-- copy is the one GHC compiles, and src/comp/BuildVersion.hs stays the make
-- build's own. The script leaves an up-to-date file untouched. NOGIT and
-- NOUPDATEBUILDVERSION pass through with the Makefile's defaults.
writeBuildVersionHs :: FilePath -> IO ()
writeBuildVersionHs autogenDir = do
  noGit <- fromMaybe "0" <$> lookupEnv "NOGIT"
  noUpdateBuildVersion <- fromMaybe "0" <$> lookupEnv "NOUPDATEBUILDVERSION"
  let newVars =
        [ ("NOGIT", noGit),
          ("NOUPDATEBUILDVERSION", noUpdateBuildVersion)
        ]
  env <- (newVars <>) <$> getEnvironment
  createDirectoryIfMissing True autogenDir
  dir <- makeAbsolute autogenDir
  -- The script is named by its absolute path: a relative one would be
  -- resolved against the process's working directory, the autogen dir.
  script <- makeAbsolute (repoRoot </> "src" </> "comp" </> "update-build-version.sh")
  callCreateProcess (proc script []) {cwd = Just dir, env = Just env}

-- | The hooks that make a vendored solver available, on the main library of
-- its binding package (bsc-stp, bsc-yices) only.
--
-- The solvers are shared libraries, so the Haskell library's dynamic object --
-- which is what ghci and runghc load -- carries them as recorded dependencies
-- rather than copies of their code. That is also why they cannot be
-- @extra-bundled-libraries@: cabal keeps those off the library's own link
-- line, and GHC's runtime linker cannot load a static archive on
-- aarch64-darwin at all.
--
-- Cabal is told about them the way tclSetupHooks tells it about Tcl: the
-- directory is computed here and injected into the component, because an
-- absolute path in a build tree cannot be written in the .cabal file.
solverSetupHooks :: SetupHooks
solverSetupHooks = noSetupHooks {configureHooks}
  where
    configureHooks = noConfigureHooks {preConfComponentHook}

    -- The vendored directories are named directly, so nothing has to be
    -- staged anywhere and there is no copy to go missing.
    preConfComponentHook :: Maybe PreConfComponentHook
    preConfComponentHook = Just $ \inputs ->
      case solverOf (LBC.localPkgDescr inputs.packageBuildDescr) of
        Just solver | isMainLib inputs.component -> do
          dir <- solverLibDir solver
          pure $
            PreConfComponentOutputs
              { componentDiff =
                  buildInfoComponentDiff
                    (componentName inputs.component)
                    ( emptyBuildInfo
                        { extraLibs = [solverLib solver],
                          extraLibDirs = [makeSymbolicPath dir],
                          -- The solvers record themselves as @rpath/...@, so
                          -- an rpath is the whole of what either platform needs
                          -- to resolve them, and these artifacts run where they
                          -- are built, which makes the build tree's own
                          -- directories the right answer. The library's
                          -- ldOptions and extra-libraries are registered with
                          -- the package and reach every library and executable
                          -- that links it, however many packages up, through
                          -- the package database (brief, F5); the dynamic
                          -- object is what records the solver dependencies.
                          ldOptions = ["-Wl,-rpath," <> dir]
                        }
                    )
              }
        _ -> pure $ noPreConfComponentOutputs inputs

-- | A vendored solver: the package that binds it, the library it links, the
-- vendored source tree whose make builds it, and where that leaves the
-- library. Adding a solver is adding a row here and a binding package in the
-- manifest; replacing one is removing them.
data Solver = Solver
  { solverPackage :: String,
    solverLib :: String,
    solverSrc :: FilePath,
    solverLibPath :: FilePath
  }

solvers :: [Solver]
solvers =
  [ Solver "bsc-stp" "stp" (vendor </> "stp") (vendor </> "stp" </> "lib"),
    Solver "bsc-yices" "yices" (vendor </> "yices") (vendor </> "yices" </> "lib")
  ]
  where
    vendor = repoRoot </> "src" </> "vendor"

solverOf :: PackageDescription -> Maybe Solver
solverOf pd = find ((== pkgName (package pd)) . mkPackageName . solverPackage) solvers

-- | Build a vendored solver and return the directory holding its library.
--
-- The make target is a no-op once the library is up to date, and the
-- library is where the make build leaves it, so nothing is staged and
-- nothing can go missing between a configure and a build. The path is
-- canonicalized so that the link flags and the rpath carry no @..@.
solverLibDir :: Solver -> IO FilePath
solverLibDir solver = do
  scratch <- canonicalizePath (repoRoot </> "dist-newstyle" </> "solver-prefix")
  callProcess "make" ["-C", solverSrc solver, "install", "PREFIX=" <> scratch]
  canonicalizePath (solverLibPath solver)

-- | The hooks to link to Tcl, on the main library of bsc-core only.
tclSetupHooks :: SetupHooks
tclSetupHooks = noSetupHooks {configureHooks}
  where
    configureHooks = noConfigureHooks {preConfComponentHook}

    preConfComponentHook :: Maybe PreConfComponentHook
    preConfComponentHook = Just $ \inputs -> do
      let platform arg = readProcess "sh" [repoRoot </> "platform.sh", arg] ""
      let trim = f . f where f = reverse . dropWhile isSpace
      let getArgs flag = fmap (drop (length flag)) . filter (flag `isPrefixOf`)
      if isBscCore (LBC.localPkgDescr inputs.packageBuildDescr)
        && isMainLib inputs.component
        then do
          tclInc <- words <$> platform "tclinc"
          tclLibs <- words <$> platform "tcllibs"
          tclVersion <- trim <$> platform "tclversion"

          cflags <- case tclVersion of
            "8.5" -> pure ["-DTCL85"]
            "8.6" -> pure []
            "9.0" -> pure ["-DTCL9"]
            _ -> ioError (userError ("unsupported Tcl version: " <> tclVersion))
          let includeDirs = makeSymbolicPath <$> getArgs "-I" tclInc
              extraLibDirs = makeSymbolicPath <$> getArgs "-L" tclLibs
              extraLibs = getArgs "-l" tclLibs

          pure $
            PreConfComponentOutputs
              { componentDiff =
                  buildInfoComponentDiff
                    (componentName inputs.component)
                    ( emptyBuildInfo
                        { includeDirs,
                          extraLibDirs,
                          extraLibs,
                          ccOptions = cflags,
                          cppOptions = cflags
                        }
                    )
              }
        else pure $ noPreConfComponentOutputs inputs
