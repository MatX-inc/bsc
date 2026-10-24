{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers #-}

-- | The package's setup hooks (build-type: Hooks). They run with the
-- repository root as the working directory. What they do (doc/cabal.md):
--
-- * generate a hidden Warmup for the library and for each executable;
--
-- * generate the BuildSystem and BuildVersion modules of the library;
--
-- * configure the Tcl include and link flags for the vendored binding
--   (HTcl.hs, haskell.c), from platform.sh;
--
-- * check that the linker finds the solvers the library links by name, and
--   give the build tree their lib/SAT.
--
-- No hook writes into src/comp or anywhere else outside dist-newstyle and
-- the package's autogen directories, but for the install stamp
-- cabal-head.project (writeBuildVersionHs).
module SetupHooks (setupHooks) where

import Control.Monad (filterM, forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isSpace)
import Data.List (isPrefixOf, nub, sort)
import Data.Maybe (fromMaybe, listToMaybe)
import Distribution.InstalledPackageInfo (ExposedModule (..))
import qualified Distribution.InstalledPackageInfo as IPI
import Distribution.ModuleName (ModuleName)
import Distribution.Pretty (prettyShow)
import Distribution.Simple.Configure (ccLdOptionsBuildInfo, getInstalledPackagesById)
import Distribution.Simple.Errors (CabalException (MissingCoveredInstalledLibrary))
import Distribution.Simple.LocalBuildInfo
  ( hostPlatform,
    interpretSymbolicPathLBI,
  )
import qualified Distribution.Simple.LocalBuildInfo as LBI
import Distribution.Simple.Program (gccProgram, lookupProgram, programPath)
import Distribution.Simple.SetupHooks
import Distribution.Simple.Utils (rawSystemExitWithEnvCwd, rewriteFileEx, withTempDirectory)
import Distribution.System (OS (..))
import qualified Distribution.Types.LocalBuildConfig as LBC
import Distribution.Verbosity (normal)
import Distribution.Utils.Path
  ( makeSymbolicPath,
    moduleNameSymbolicPath,
    (<.>),
  )
import System.Directory
  ( canonicalizePath,
    createDirectoryIfMissing,
    createDirectoryLink,
    doesDirectoryExist,
    doesFileExist,
    getTemporaryDirectory,
    listDirectory,
    makeAbsolute,
    removeDirectoryLink,
  )
import System.Environment (getEnvironment, lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (splitSearchPath, takeDirectory, (</>))
import System.IO.Error (catchIOError)
import System.Process
  ( readProcess,
    readProcessWithExitCode,
  )

setupHooks :: SetupHooks
setupHooks =
  warmupSetupHooks
    <> generatedModulesSetupHooks
    <> solverSetupHooks
    <> tclSetupHooks

-- | Cabal 3.16 runs the external Setup in the package directory.
packageRoot :: IO FilePath
packageRoot = canonicalizePath "."

isMainLib :: Component -> Bool
isMainLib (CLib Library {libName = LMainLibName}) = True
isMainLib _ = False

-- | The autogen location of a generated module of the component.
autogenLocation :: LocalBuildInfo -> ComponentLocalBuildInfo -> ModuleName -> Location
autogenLocation lbi clbi m =
  Location (autogenComponentModulesDir lbi clbi) (moduleNameSymbolicPath m <.> "hs")

-- | A location as the file path the action that writes it uses.
locationFilePath :: LocalBuildInfo -> Location -> FilePath
locationFilePath lbi loc = interpretSymbolicPathLBI lbi (location loc)

-- | Generate Warmup for the components that declare it.
wantsWarmup :: Component -> Bool
wantsWarmup = elem "Warmup" . autogenModules . LBI.componentBuildInfo

-- | The hooks to generate each component's Warmup module.
--
-- Warmup imports every exposed module of every package the component
-- directly depends on, so that the whole external interface set enters
-- GHC's EPS in one deterministic order before any other module of the
-- component compiles; without it, @ghc --make -jN@ object output varies
-- with scheduling (rule visibility and rule-overlap order both depend on
-- interface-load order; see GHC.Core.Rules, Note [Overall plumbing for
-- rules]). Every root module of the library and every program imports it
-- (doc/cabal.md), so the barrier holds for the whole unit. The module is generated against this build's
-- compiler and this build's resolved dependencies, read from the package
-- databases at build time.
--
-- Only the DIRECT dependencies are used: the transitive closure would name
-- modules of packages GHC is not given, which it refuses as hidden. In-place
-- packages count, so a program's Warmup covers the library, and the
-- library's own Warmup is excluded by name so that no program imports it
-- into its own.
warmupSetupHooks :: SetupHooks
warmupSetupHooks = noSetupHooks {buildHooks}
  where
    buildHooks = noBuildHooks {preBuildComponentRules}

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
        -- change (an in-place unit id such as bsc-2026.1-inplace is
        -- stable), so the snapshot goes stale as soon as modules move between
        -- packages, and a Warmup generated from it imports modules its
        -- dependency no longer exposes. This computation re-runs on every
        -- build, after the dependencies were registered, so the index is
        -- current; and because the module list is an argument of the rule's
        -- command, Cabal re-runs the rule when the list changes.
        direct <- liftIO $
          getInstalledPackagesById verbosity lbi
            MissingCoveredInstalledLibrary (map fst (componentPackageDeps clbi))
        let mods =
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
                (locationFilePath lbi warmup, mods)
            )
            []
            [warmup]

writeWarmupHs :: (FilePath, [String]) -> IO ()
writeWarmupHs (path, mods) = do
  createDirectoryIfMissing True (takeDirectory path)
  rewriteFileEx normal path . unlines $
    [ "-- Generated by SetupHooks.hs; do not edit.",
      "-- An import of every exposed module of every direct dependency of this",
      "-- component, so that the external interface set enters GHC's EPS in one",
      "-- deterministic order before any other module compiles; see",
      "-- warmupSetupHooks in SetupHooks.hs for why.",
      "module Warmup where"
    ]
      <> ["import " <> m <> " ()" | m <- mods]

-- | The hooks to generate the BuildSystem and BuildVersion modules, on the
-- library.
generatedModulesSetupHooks :: SetupHooks
generatedModulesSetupHooks = noSetupHooks {buildHooks}
  where
    buildHooks = noBuildHooks {preBuildComponentRules}

    -- Generate the modules. BuildVersion's input is the repository's HEAD,
    -- which no file dependency names. Through an external Setup the rules run
    -- whenever Setup builds the package, so BuildVersion follows HEAD whenever
    -- a source of the package changed or it was reconfigured; the monitors on
    -- the git HEAD and its reflog, and the commit in the rule's argument, are
    -- for a build system that runs the rules itself (cabal-install with a
    -- Cabal 3.18 setup), which decides from them (doc/cabal.md).
    preBuildComponentRules :: Maybe PreBuildComponentRules
    preBuildComponentRules = Just . rules (static ()) $ \env -> do
      let lbi = env.localBuildInfo
          clbi = env.targetInfo.targetCLBI
          buildSystem = autogenLocation lbi clbi "BuildSystem"
          buildVersion = autogenLocation lbi clbi "BuildVersion"
      when (isMainLib (targetComponent env.targetInfo)) $ do
        liftIO refuseStrayGeneratedModules
        repoHead <- liftIO gitHead
        case repoHead of
          Just (gitDir, _) ->
            addRuleMonitors
              [ monitorFileHashed (gitDir </> "HEAD"),
                monitorFileHashed (gitDir </> "logs" </> "HEAD")
              ]
          Nothing -> pure ()
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
                ( interpretSymbolicPathLBI lbi (autogenComponentModulesDir lbi clbi),
                  maybe "" snd repoHead
                )
            )
            []
            [buildVersion]

-- | Refuse while a copy of a generated module lies under src/comp, where
-- GHC would compile it in place of the generated one.
refuseStrayGeneratedModules :: IO ()
refuseStrayGeneratedModules = do
  root <- packageRoot
  forM_ (["Warmup", "BuildSystem", "BuildVersion"] :: [String]) $ \m -> do
    left <- doesFileExist (root </> "src" </> "comp" </> (m <> ".hs"))
    when left . ioError . userError $
      "src/comp/" <> m <> ".hs would be compiled in place of the generated module:"
        <> " remove it and build again"

writeBuildSystemHs :: (FilePath, Platform) -> IO ()
writeBuildSystemHs (path, Platform _ os) = do
  createDirectoryIfMissing True (takeDirectory path)
  binFmtType <- case os of
    Linux -> pure "ELF"
    FreeBSD -> pure "ELF"
    OSX -> pure "MachO"
    _ -> ioError (userError ("unsupported OS: " <> show os))
  rewriteFileEx normal path . unlines $
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
-- The script writes BuildVersion.hs into its working directory, so the
-- autogen directory is where it runs and nothing is copied. The script
-- leaves an up-to-date file untouched. NOGIT passes through from the
-- environment, defaulted off as the script defaults it.
--
-- The script locates the repository from its own source path and clears
-- inherited Git context, so an external build directory or another
-- repository in the caller's environment cannot change the version.
-- The second argument, the commit the rule was computed for, only makes
-- the rule's identity follow HEAD; the script reads the repository itself.
writeBuildVersionHs :: (FilePath, String) -> IO ()
writeBuildVersionHs (autogenDir, _) = do
  root <- packageRoot
  commit <- gitHead
  env <- getEnvironment
  createDirectoryIfMissing True autogenDir
  dir <- makeAbsolute autogenDir
  -- The script is named by its absolute path: a relative one would be
  -- resolved against the process's working directory, the autogen dir.
  let script = root </> "src" </> "comp" </> "update-build-version.sh"
  rawSystemExitWithEnvCwd normal (Just (makeSymbolicPath dir)) script [] env
  -- The install stamp (src/comp/Makefile, CABAL_HEAD): `make install` writes
  -- HEAD into it, a declared source, so that a later commit is a changed
  -- source and this rule runs again. cabal-install watches the declared
  -- sources it finds when it records a build, so the stamp must exist from
  -- the first build: one first written by an install after a build is not
  -- watched, and the version stops following HEAD until something else
  -- makes cabal-install build again. It names the commit whatever NOGIT
  -- says, as `make install` does, so the two write the same stamp.
  let stamp = root </> "cabal-head.project"
      content = maybe "no-repository" snd commit <> "\n"
  rewriteFileEx normal stamp content

-- | The repository's git directory (worktree-safe) and its HEAD commit, or
-- Nothing where there is no git or no checkout (an exported archive).
gitHead :: IO (Maybe (FilePath, String))
gitHead = do
  root <- packageRoot
  probe <-
    readProcessWithExitCode
      (root </> "src" </> "comp" </> "update-build-version.sh") ["--git-info"] ""
      `catchIOError` \_ -> pure (ExitFailure 1, "", "")
  pure $ case probe of
    (ExitSuccess, out, _) | [gitDir, commit] <- lines out -> Just (gitDir, commit)
    _ -> Nothing

-- | The hooks for the vendored solvers' bindings (STP.hs, Yices.hs), on the
-- library that holds them.
--
-- The cabal build does not build the solvers. The library links @stp@ and
-- @yices@ by name (bsc.cabal, extra-libraries), against whichever copies the
-- linker finds, as any Haskell package links a C library: `make install-src`
-- builds and installs the vendored copies first and names their directory in
-- LIBRARY_PATH when it runs the cabal build (src/comp/Makefile), and a build
-- run by hand does the same (doc/cabal.md section 2) or links copies the
-- system provides. Cabal is told no directory, so none reaches the
-- registration or an rpath, and the linked bytes do not depend on where the
-- tree or the solvers are.
--
-- The solvers are shared libraries (the dynamic link is yices's GPL
-- boundary), so the library's dynamic object, what ghci and runghc load,
-- records them as dependencies rather than carrying copies of their code.
-- Everything resolves them beside itself, lib/SAT, through the rpath
-- $ORIGIN/../lib/SAT (@loader_path on Darwin): the programs get it from the
-- library's ld-options in
-- bsc.cabal, which every program that links the library inherits through the
-- package database, and the dynamic object gets it from cabal.project, since
-- Cabal 3.16 passes a library's ld-options to the programs' links but not to
-- the library's own.
--
-- What is left for hooks: before the library configures, check that the
-- linker finds the solvers and say what to do when it does not, since
-- Cabal's own message names only the libraries; after a build, link lib/SAT
-- beside each artifact of the build tree to the directory the solvers were
-- linked from, so the artifacts run where they are built.
solverSetupHooks :: SetupHooks
solverSetupHooks = noSetupHooks {configureHooks, buildHooks}
  where
    configureHooks = noConfigureHooks {preConfComponentHook}
    buildHooks = noBuildHooks {postBuildComponentHook}

    preConfComponentHook :: Maybe PreConfComponentHook
    preConfComponentHook = Just $ \inputs -> do
      case inputs.component of
        CLib lib | isMainLib inputs.component -> do
          linkable <- solversLinkable (LBC.withPrograms inputs.localBuildConfig) (libBuildInfo lib)
          unless linkable $ ioError (userError solverAdvice)
        _ -> pure ()
      pure $ noPreConfComponentOutputs inputs

    -- A library's dynamic object is <builddir>/build/libHS*.so, so its
    -- lib/SAT is <builddir>/lib/SAT; an executable is
    -- <builddir>/build/<exe>/<exe>, so its lib/SAT is <builddir>/build/lib/SAT.
    postBuildComponentHook :: Maybe PostBuildComponentHook
    postBuildComponentHook = Just $ \inputs -> do
      linked <- solverDirectory
      forM_ linked $ \sat -> do
        let lbi = inputs.localBuildInfo
        build <- makeAbsolute (interpretSymbolicPathLBI lbi (LBI.buildDir lbi))
        case targetComponent inputs.targetInfo of
          CLib _ -> linkSatDir sat (takeDirectory build)
          CExe _ -> linkSatDir sat build
          _ -> pure ()

-- | The solvers the library links, by the names the linker is given.
solverLibs :: [String]
solverLibs = ["stp", "yices"]

-- | Whether the linker finds the libraries the library names: the C
-- compiler, as Cabal configured it, links an empty program with the
-- library's extra-libraries and ld-options, with the search path the real
-- links will have, LIBRARY_PATH and the system's directories. Cabal's own
-- check (checkForeignDeps) does the same, and fails naming only the
-- libraries. A compiler that cannot be run counts as finding nothing.
solversLinkable :: ProgramDb -> BuildInfo -> IO Bool
solversLinkable programs bi = do
  let cc = lookupProgram gccProgram programs
      args inputs = maybe [] programDefaultArgs cc <> inputs <> maybe [] programOverrideArgs cc
  tmp <- getTemporaryDirectory
  withTempDirectory normal tmp "bsc-solvers" $ \dir -> do
    let source = dir </> "probe.c"
    writeFile source "int main(void) { return 0; }\n"
    (code, _, _) <-
      readProcessWithExitCode
        (maybe "cc" programPath cc)
        (args ([source, "-o", dir </> "probe"] <> map ("-l" <>) (extraLibs bi) <> ldOptions bi))
        ""
        `catchIOError` \_ -> pure (ExitFailure 1, "", "")
    pure (code == ExitSuccess)

solverAdvice :: String
solverAdvice =
  unlines
    [ "the linker finds no libstp or libyices (or no C++ runtime), which the bsc library links by name.",
      "Build and install the vendored solvers, and name their one directory for the cabal build:",
      "    make install-solvers",
      "    LIBRARY_PATH=$PWD/inst/lib/SAT cabal build all",
      "(`make install-src` does both); or bring your own the same way, or install them",
      "where the linker looks (doc/cabal.md, section 2)."
    ]

-- | The directory the solvers are linked from, when one named in
-- LIBRARY_PATH holds them both (lib/SAT is one directory, so they are
-- expected together): what the build tree's lib/SAT links point at. Copies
-- in a system directory resolve on their own and need no link.
solverDirectory :: IO (Maybe FilePath)
solverDirectory = do
  path <- fromMaybe "" <$> lookupEnv "LIBRARY_PATH"
  dirs <- filterM holdsSolvers (filter (not . null) (splitSearchPath path))
  traverse canonicalizePath (listToMaybe dirs)
  where
    holdsSolvers dir = do
      exists <- doesDirectoryExist dir
      if not exists
        then pure False
        else do
          names <- listDirectory dir
          pure (all (\lib -> any (("lib" <> lib <> ".") `isPrefixOf`) names) solverLibs)

-- | Make @dir/lib/SAT@ a link to the solvers' directory, replacing a stale
-- link.
linkSatDir :: FilePath -> FilePath -> IO ()
linkSatDir solvers dir = do
  let sat = dir </> "lib" </> "SAT"
  createDirectoryIfMissing True (dir </> "lib")
  removeDirectoryLink sat `catchIOError` \_ -> pure ()
  createDirectoryLink solvers sat

-- | The hooks to compile against and link to Tcl, on the library that holds
-- the vendored binding (HTcl.hs, haskell.c).
tclSetupHooks :: SetupHooks
tclSetupHooks = noSetupHooks {configureHooks}
  where
    configureHooks = noConfigureHooks {preConfComponentHook}

    preConfComponentHook :: Maybe PreConfComponentHook
    preConfComponentHook = Just $ \inputs -> do
      root <- packageRoot
      let platform arg = readProcess "sh" [root </> "platform.sh", arg] ""
      let trim = f . f where f = reverse . dropWhile isSpace
      if isMainLib inputs.component
        then do
          tclInc <- words <$> platform "tclinc"
          tclLibs <- words <$> platform "tcllibs"
          tclVersion <- trim <$> platform "tclversion"

          cflags <- case tclVersion of
            "8.5" -> pure ["-DTCL85"]
            "8.6" -> pure []
            "9.0" -> pure ["-DTCL9"]
            _ -> ioError (userError ("unsupported Tcl version: " <> tclVersion))
          pure $
            PreConfComponentOutputs
              { componentDiff =
                  buildInfoComponentDiff
                    (componentName inputs.component)
                    ((ccLdOptionsBuildInfo (tclInc <> cflags) tclLibs []) {cppOptions = cflags})
              }
        else pure $ noPreConfComponentOutputs inputs
