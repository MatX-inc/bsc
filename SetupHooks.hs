{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers #-}

-- | The package's setup hooks (build-type: Hooks). They run with the
-- repository root as the working directory. What they do (doc/cabal.md):
--
-- * hold exposed-modules to the tree: every module under the src/comp roots
--   must be listed, or configuring fails naming the missing ones;
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
-- build/cabal-head (writeBuildVersionHs).
module SetupHooks (setupHooks) where

import Control.Monad (filterM, forM, forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isAlphaNum, isSpace)
import Data.List (isPrefixOf, nub, sort)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe, listToMaybe)
import Distribution.InstalledPackageInfo (ExposedModule (..))
import qualified Distribution.InstalledPackageInfo as IPI
import Distribution.ModuleName (ModuleName)
import qualified Distribution.ModuleName as ModuleName
import Distribution.Pretty (prettyShow)
import Distribution.Simple.Configure (getInstalledPackages)
import Distribution.Simple.LocalBuildInfo
  ( hostPlatform,
    interpretSymbolicPathLBI,
    mbWorkDirLBI,
    withPackageDB,
    withPrograms,
  )
import qualified Distribution.Simple.LocalBuildInfo as LBI
import Distribution.Simple.PackageIndex (lookupUnitId)
import Distribution.Simple.Program (gccProgram, lookupProgram, programPath)
import Distribution.Simple.SetupHooks
import Distribution.Simple.Utils (die', withTempDirectory)
import Distribution.System (OS (..))
import qualified Distribution.Types.LocalBuildConfig as LBC
import Distribution.Verbosity (normal)
import Distribution.Utils.Path
  ( getSymbolicPath,
    makeSymbolicPath,
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
import System.FilePath (isPathSeparator, splitExtension, splitSearchPath, takeDirectory, (</>))
import System.IO (IOMode (..), hGetContents', hSetEncoding, latin1, readFile', withFile)
import System.IO.Error (catchIOError)
import System.Process
  ( CreateProcess (..),
    createProcess,
    proc,
    readProcess,
    readProcessWithExitCode,
    waitForProcess,
  )

setupHooks :: SetupHooks
setupHooks =
  moduleListSetupHooks
    <> warmupSetupHooks
    <> generatedModulesSetupHooks
    <> solverSetupHooks
    <> tclSetupHooks

-- | The repository root: the nearest directory at or above the working
-- directory that holds cabal.project (the package directory is the root
-- itself; the search is for a Setup run from elsewhere).
findRepoRoot :: IO FilePath
findRepoRoot = canonicalizePath "." >>= go
  where
    go dir = do
      found <- doesFileExist (dir </> "cabal.project")
      if found
        then pure dir
        else
          let up = takeDirectory dir
           in if up == dir
                then ioError (userError "no cabal.project at or above the package directory")
                else go up

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

-- | The hook that holds exposed-modules to the tree: every module under the
-- library's source roots below src/comp must be listed, or configuring fails
-- naming the ones that are not. The list is explicit because Cabal wants
-- it, and because its change is what makes cabal-install reconfigure.
--
-- A file is a module of the library when the module its header names is the
-- one its path under the root spells: Libs/IOUtil.hs is IOUtil under the
-- Libs root and nothing under the src/comp root, and app/bsc.hs, whose
-- header names Main_bsc, is a program's entry module and not the library's.
-- The generated modules are exempt: a copy of one under src/comp is refused
-- (generatedModulesSetupHooks).
moduleListSetupHooks :: SetupHooks
moduleListSetupHooks = noSetupHooks {configureHooks}
  where
    configureHooks = noConfigureHooks {preConfComponentHook}

    preConfComponentHook :: Maybe PreConfComponentHook
    preConfComponentHook = Just $ \inputs -> do
      case inputs.component of
        CLib lib | isMainLib inputs.component -> do
          root <- findRepoRoot
          let bi = libBuildInfo lib
              roots = [d | d <- getSymbolicPath <$> hsSourceDirs bi, "src/comp" `isPrefixOf` d]
              listed = exposedModules lib <> otherModules bi
          found <- concat <$> mapM (modulesUnder . (root </>)) roots
          let missing = sort [m | m <- nub found, m `notElem` listed]
          unless (null missing) $
            ioError . userError $
              "bsc.cabal does not list "
                <> show (length missing)
                <> " module(s) found under src/comp: "
                <> unwords (prettyShow <$> missing)
        _ -> pure ()
      pure (noPreConfComponentOutputs inputs)

-- | The modules under a source root, by the rule above.
modulesUnder :: FilePath -> IO [ModuleName]
modulesUnder root = go ""
  where
    go rel = do
      entries <- sort <$> listDirectory (root </> rel)
      fmap concat . forM entries $ \entry -> do
        let relEntry = rel </> entry
        isDir <- doesDirectoryExist (root </> relEntry)
        if isDir
          then go relEntry
          else case splitExtension relEntry of
            (base, ext) | ext `elem` ([".hs", ".lhs"] :: [String]) -> do
              let name = map (\c -> if isPathSeparator c then '.' else c) base
              header <- moduleHeader (root </> relEntry)
              pure [ModuleName.fromString name | header == Just name, name `notElem` generatedModules]
            _ -> pure []

generatedModules :: [String]
generatedModules = ["Warmup", "BuildSystem", "BuildVersion"]

-- | The module a Haskell source names in its header, plain or bird-track
-- literate. Read as Latin-1 so that no locale decides whether a byte in a
-- comment is an error.
moduleHeader :: FilePath -> IO (Maybe String)
moduleHeader path = withFile path ReadMode $ \h -> do
  hSetEncoding h latin1
  text <- hGetContents' h
  pure (listToMaybe [m | l <- lines text, Just m <- [headerName l]])
  where
    headerName line = case words (bird (dropWhile isSpace line)) of
      ("module" : m : _) -> Just (takeWhile (\c -> isAlphaNum c || c `elem` ("._'" :: String)) m)
      _ -> Nothing
    bird ('>' : rest) = rest
    bird rest = rest

-- | Which components get a generated Warmup: every library with modules of
-- its own and every executable; test suites (and benchmarks and foreign
-- libraries) get none.
wantsWarmup :: Component -> Bool
wantsWarmup (CLib lib) =
  not (null (exposedModules lib) && null (otherModules (libBuildInfo lib)))
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
warmupSetupHooks = noSetupHooks {configureHooks, buildHooks}
  where
    configureHooks = noConfigureHooks {preConfComponentHook}
    buildHooks = noBuildHooks {preBuildComponentRules}

    -- Declare that the module is generated. The .cabal file lists it under
    -- other-modules (exposed-modules for the library), as Cabal requires of
    -- an autogen module.
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
        -- change (an in-place unit id such as bsc-2026.1-inplace is
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
    [ "-- Generated by SetupHooks.hs; do not edit.",
      "-- An import of every exposed module of every direct dependency of this",
      "-- component, so that the external interface set enters GHC's EPS in one",
      "-- deterministic order before any other module compiles; see",
      "-- warmupSetupHooks in SetupHooks.hs for why.",
      "-- Direct dependencies, as resolved for this build:"
    ]
      <> ["--   " <> u | u <- units]
      <> ["module Warmup where"]
      <> ["import " <> m <> " ()" | m <- mods]

-- | The hooks to generate the BuildSystem and BuildVersion modules, on the
-- library.
generatedModulesSetupHooks :: SetupHooks
generatedModulesSetupHooks = noSetupHooks {configureHooks, buildHooks}
  where
    configureHooks = noConfigureHooks {preConfComponentHook}
    buildHooks = noBuildHooks {preBuildComponentRules}

    -- Declare that the modules are generated. GHC takes a module found
    -- under hs-source-dirs over the autogen one (Cabal lists the source
    -- directories first), so a copy of one of them under src/comp, which
    -- nothing generates any more, would be what the library compiles:
    -- configuring refuses while one is there, and so does each build, since
    -- cabal-install builds without configuring when only sources changed.
    preConfComponentHook :: Maybe PreConfComponentHook
    preConfComponentHook = Just $ \inputs ->
      if isMainLib inputs.component
        then do
          refuseStrayGeneratedModules
          pure $
            PreConfComponentOutputs
              { componentDiff =
                  buildInfoComponentDiff
                    (componentName inputs.component)
                    (emptyBuildInfo {autogenModules = ["BuildSystem", "BuildVersion"]})
              }
        else pure $ noPreConfComponentOutputs inputs

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
  root <- findRepoRoot
  forM_ (["Warmup", "BuildSystem", "BuildVersion"] :: [String]) $ \m -> do
    left <- doesFileExist (root </> "src" </> "comp" </> (m <> ".hs"))
    when left . ioError . userError $
      "src/comp/" <> m <> ".hs would be compiled in place of the generated module:"
        <> " remove it and build again"

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
-- The script writes BuildVersion.hs into its working directory, so the
-- autogen directory is where it runs and nothing is copied. The script
-- leaves an up-to-date file untouched. NOGIT passes through from the
-- environment, defaulted off as the script defaults it.
--
-- The script's git commands find the repository by searching upward from
-- the working directory, which fails when the build directory lies outside
-- the checkout (--builddir elsewhere, or a dist-newstyle symlink), so the
-- repository is named explicitly through GIT_DIR (worktree-safe, from
-- rev-parse) and GIT_WORK_TREE. Where that probe fails (an exported archive,
-- or no git) neither is set and the script behaves as before.
-- The second argument, the commit the rule was computed for, only makes
-- the rule's identity follow HEAD; the script reads the repository itself.
writeBuildVersionHs :: (FilePath, String) -> IO ()
writeBuildVersionHs (autogenDir, _) = do
  noGit <- fromMaybe "0" <$> lookupEnv "NOGIT"
  root <- findRepoRoot
  commit <- gitHead
  let repoHead = if noGit == "1" then Nothing else commit
  let gitVars = case repoHead of
        Just (gitDir, _) -> [("GIT_DIR", gitDir), ("GIT_WORK_TREE", root)]
        Nothing -> []
  let newVars = ("NOGIT", noGit) : gitVars
  env <- (newVars <>) <$> getEnvironment
  createDirectoryIfMissing True autogenDir
  dir <- makeAbsolute autogenDir
  -- The script is named by its absolute path: a relative one would be
  -- resolved against the process's working directory, the autogen dir.
  let script = root </> "src" </> "comp" </> "update-build-version.sh"
  callCreateProcess (proc script []) {cwd = Just dir, env = Just env}
  -- The install stamp (src/comp/Makefile, CABAL_HEAD): `make install` writes
  -- HEAD into it, a declared source, so that a later commit is a changed
  -- source and this rule runs again. cabal-install watches the declared
  -- sources it finds when it records a build, so the stamp must exist from
  -- the first build: one first written by an install after a build is not
  -- watched, and the version stops following HEAD until something else
  -- makes cabal-install build again. It names the commit whatever NOGIT
  -- says, as `make install` does, so the two write the same stamp.
  let stamp = root </> "build" </> "cabal-head"
      content = maybe "no-repository" snd commit <> "\n"
  createDirectoryIfMissing True (root </> "build")
  current <- (Just <$> readFile' stamp) `catchIOError` \_ -> pure Nothing
  when (current /= Just content) $ writeFile stamp content

-- | The repository's git directory (worktree-safe) and its HEAD commit, or
-- Nothing where there is no git or no checkout (an exported archive).
gitHead :: IO (Maybe (FilePath, String))
gitHead = do
  root <- findRepoRoot
  probe <-
    readProcessWithExitCode "git" ["-C", root, "rev-parse", "--absolute-git-dir", "HEAD"] ""
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
      root <- findRepoRoot
      let platform arg = readProcess "sh" [root </> "platform.sh", arg] ""
      let trim = f . f where f = reverse . dropWhile isSpace
      let getArgs flag = fmap (drop (length flag)) . filter (flag `isPrefixOf`)
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
