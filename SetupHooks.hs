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
-- * build each vendored solver and configure its link flags and rpath.
--
-- No hook writes into src/comp or anywhere else outside dist-newstyle and
-- the package's autogen directories; the make build generates its own
-- Warmup.hs and BuildVersion.hs as before.
module SetupHooks (setupHooks) where

import Control.Monad (forM, forM_, unless, when)
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
import Distribution.Simple.SetupHooks
import Distribution.Simple.Utils (die')
import Distribution.System (OS (..))
import qualified Distribution.Types.LocalBuildConfig as LBC
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
    listDirectory,
    makeAbsolute,
    removeDirectoryLink,
  )
import System.Environment (getEnvironment, lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (isPathSeparator, splitExtension, takeDirectory, (</>))
import System.IO (IOMode (..), hGetContents', hSetEncoding, latin1, readFile', withFile)
import System.IO.Error (catchIOError)
import System.Process
  ( CreateProcess (..),
    callProcess,
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
-- naming the ones that are not. The Makefile finds the same files by
-- wildcard; the list is explicit here because Cabal wants it, and because
-- its change is what makes cabal-install reconfigure.
--
-- A file is a module of the library when the module its header names is the
-- one its path under the root spells: Libs/IOUtil.hs is IOUtil under the
-- Libs root and nothing under the src/comp root, and app/bsc.hs, whose
-- header names Main_bsc, is a program's entry module and not the library's.
-- The generated modules are skipped where the make build has left them in
-- src/comp.
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
-- databases at build time, not from the Makefile's package list.
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

    -- Declare that the modules are generated.
    preConfComponentHook :: Maybe PreConfComponentHook
    preConfComponentHook = Just $ \inputs ->
      if isMainLib inputs.component
        then
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
-- autogen directory is where it runs and nothing is copied: the autogen
-- copy is the one GHC compiles, and src/comp/BuildVersion.hs stays the make
-- build's own. The script leaves an up-to-date file untouched. NOGIT and
-- NOUPDATEBUILDVERSION pass through with the Makefile's defaults.
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
  noUpdateBuildVersion <- fromMaybe "0" <$> lookupEnv "NOUPDATEBUILDVERSION"
  root <- findRepoRoot
  repoHead <- if noGit == "1" then pure Nothing else gitHead
  let gitVars = case repoHead of
        Just (gitDir, _) -> [("GIT_DIR", gitDir), ("GIT_WORK_TREE", root)]
        Nothing -> []
  let newVars =
        [ ("NOGIT", noGit),
          ("NOUPDATEBUILDVERSION", noUpdateBuildVersion)
        ]
          <> gitVars
  env <- (newVars <>) <$> getEnvironment
  createDirectoryIfMissing True autogenDir
  dir <- makeAbsolute autogenDir
  -- The script is named by its absolute path: a relative one would be
  -- resolved against the process's working directory, the autogen dir.
  let script = root </> "src" </> "comp" </> "update-build-version.sh"
  callCreateProcess (proc script []) {cwd = Just dir, env = Just env}

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

-- | The hooks that make the vendored solvers available, on the library that
-- holds their bindings (STP.hs, Yices.hs).
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
solverSetupHooks = noSetupHooks {configureHooks, buildHooks}
  where
    configureHooks = noConfigureHooks {preConfComponentHook}
    buildHooks = noBuildHooks {postBuildComponentHook}

    -- The vendored directories are named directly for the link, so nothing
    -- has to be staged for it and there is no copy to go missing.
    preConfComponentHook :: Maybe PreConfComponentHook
    preConfComponentHook = Just $ \inputs ->
      if isMainLib inputs.component
        then do
          dirs <- mapM solverLibDir solvers
          let Platform _ os = LBC.hostPlatform inputs.packageBuildDescr
          pure $
            PreConfComponentOutputs
              { componentDiff =
                  buildInfoComponentDiff
                    (componentName inputs.component)
                    ( emptyBuildInfo
                        { extraLibs = map solverLib solvers,
                          extraLibDirs = map makeSymbolicPath dirs,
                          -- The solvers record themselves as @rpath/...@, so
                          -- an rpath is the whole of what either platform needs
                          -- to resolve them. It is the make build's
                          -- (src/comp/Makefile, SAT_RPATH_FLAGS): lib/SAT
                          -- beside the directory holding the binary, which is
                          -- what lets an installation move as a whole, and it
                          -- names no directory of this checkout, so the linked
                          -- bytes do not depend on where the tree is. The
                          -- library's ldOptions and extra-libraries are
                          -- registered with the package and reach every
                          -- library and executable that links it, however many
                          -- packages up, through the package database (doc/cabal.md);
                          -- the dynamic object is what records the solver
                          -- dependencies. The post-build hook below gives the
                          -- build tree the lib/SAT that each of them resolves.
                          ldOptions = ["-Wl,-rpath," <> satRPathOrigin os <> "/../lib/SAT"]
                        }
                    )
              }
        else pure $ noPreConfComponentOutputs inputs

    -- The build tree's lib/SAT, so that the artifacts run where they are
    -- built: a link to the staged solver libraries beside the directory that
    -- holds each artifact carrying the rpath. A library's dynamic object is
    -- <builddir>/build/libHS*.so, so its lib/SAT is <builddir>/lib/SAT; an
    -- executable is <builddir>/build/<exe>/<exe>, so its lib/SAT is
    -- <builddir>/build/lib/SAT. Every library gets one, since the ldOptions
    -- reach every library above the solvers.
    postBuildComponentHook :: Maybe PostBuildComponentHook
    postBuildComponentHook = Just $ \inputs -> do
      let lbi = inputs.localBuildInfo
      build <- makeAbsolute (interpretSymbolicPathLBI lbi (LBI.buildDir lbi))
      staged <- (</> ("lib" </> "SAT")) <$> solverStagingDir
      stagedExists <- doesDirectoryExist staged
      when stagedExists $ case targetComponent inputs.targetInfo of
        CLib _ -> linkSatDir staged (takeDirectory build)
        CExe _ -> linkSatDir staged build
        _ -> pure ()

-- | The linker's name for the directory holding the binary being resolved,
-- as src/comp/Makefile spells it for each platform.
satRPathOrigin :: OS -> String
satRPathOrigin OSX = "@loader_path"
satRPathOrigin _ = "$ORIGIN"

-- | Make @dir/lib/SAT@ a link to the staged solver libraries, replacing a
-- stale link.
linkSatDir :: FilePath -> FilePath -> IO ()
linkSatDir staged dir = do
  let sat = dir </> "lib" </> "SAT"
  createDirectoryIfMissing True (dir </> "lib")
  removeDirectoryLink sat `catchIOError` \_ -> pure ()
  createDirectoryLink staged sat

-- | Where the solvers' make install stages them: the make build's lib/SAT
-- layout under a prefix of the build tree.
solverStagingDir :: IO FilePath
solverStagingDir = (</> ("dist-newstyle" </> "solver-prefix")) <$> findRepoRoot

-- | A vendored solver: the library it links, the vendored source tree whose
-- make builds it, and where that leaves the library, both relative to the
-- repository root. Adding a solver is adding a row here and its binding
-- modules; replacing one is removing them.
data Solver = Solver
  { solverLib :: String,
    solverSrc :: FilePath,
    solverLibPath :: FilePath
  }

solvers :: [Solver]
solvers =
  [ Solver "stp" (vendor </> "stp") (vendor </> "stp" </> "lib"),
    Solver "yices" (vendor </> "yices") (vendor </> "yices" </> "lib")
  ]
  where
    vendor = "src" </> "vendor"

-- | Build a vendored solver and return the directory holding its library.
--
-- The make target is a no-op once the library is up to date, and the link
-- names the library where the make build leaves it, so nothing can go
-- missing between a configure and a build; the install's copy under
-- solverStaging is what the build tree's lib/SAT links point at. The path is
-- canonicalized so that the link flags carry no @..@.
solverLibDir :: Solver -> IO FilePath
solverLibDir solver = do
  root <- findRepoRoot
  scratch <- solverStagingDir
  callProcess "make" ["-C", root </> solverSrc solver, "install", "PREFIX=" <> scratch]
  canonicalizePath (root </> solverLibPath solver)

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
