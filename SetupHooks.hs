{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers #-}

-- | The package's setup hooks (build-type: Hooks), run with the repository
-- root as the working directory; doc/cabal.md section 3 says what they do.
-- No hook writes into src/comp, or anywhere outside dist-newstyle and the
-- autogen directories; the make build generates its own Warmup.hs and
-- BuildVersion.hs as before.
module SetupHooks (setupHooks) where

import Control.Monad (filterM, forM, forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isSpace)
import Data.List (isPrefixOf, nub, sort)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe, listToMaybe)
import Distribution.InstalledPackageInfo (ExposedModule (..))
import qualified Distribution.InstalledPackageInfo as IPI
import Distribution.ModuleName (ModuleName)
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
import Distribution.Utils.Path (makeSymbolicPath, moduleNameSymbolicPath, (<.>))
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
import System.FilePath (splitSearchPath, takeDirectory, (</>))
import System.IO (readFile')
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
  noSetupHooks
    { configureHooks = noConfigureHooks {preConfComponentHook = Just preConfComponent},
      buildHooks =
        noBuildHooks
          { preBuildComponentRules = Just preBuildRules,
            postBuildComponentHook = Just postBuildComponent
          }
    }

-- | The repository root: the nearest directory at or above the working
-- directory that holds cabal.project (the search is for a Setup run from
-- elsewhere).
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

-- | Run a process to completion and fail if it fails. process exports this
-- from 1.6.28; GHC 9.14.1 bundles 1.6.26, and a floor above the bundled
-- version would make cabal build process from Hackage.
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

-- | The file path of a location, for the action that writes it.
locationFilePath :: LocalBuildInfo -> Location -> FilePath
locationFilePath lbi loc = interpretSymbolicPathLBI lbi (location loc)

-- | Which components get a generated Warmup: every library with modules of
-- its own and every executable.
wantsWarmup :: Component -> Bool
wantsWarmup (CLib lib) =
  not (null (exposedModules lib) && null (otherModules (libBuildInfo lib)))
wantsWarmup CExe {} = True
wantsWarmup _ = False

-- | The configure hook. The .cabal file lists the generated modules too
-- (exposed-modules, other-modules), as Cabal requires of an autogen module.
--
-- GHC takes a module found under hs-source-dirs over the autogen one, so a
-- Warmup.hs or BuildSystem.hs the make build has generated under src/comp
-- would be what the library compiles, and the make build's are not this
-- build's: configuring refuses while one is there.
preConfComponent :: PreConfComponentHook
preConfComponent inputs = do
  let c = inputs.component
  bi <- case c of
    CLib _ | isMainLib c -> do
      refuseStrayGeneratedModules
      tcl <- tclBuildInfo
      pure tcl {autogenModules = ["Warmup", "BuildSystem", "BuildVersion"]}
    _ | wantsWarmup c -> pure emptyBuildInfo {autogenModules = ["Warmup"]}
    _ -> pure emptyBuildInfo
  pure PreConfComponentOutputs {componentDiff = buildInfoComponentDiff (componentName c) bi}

-- | The rules that generate the modules, per component.
--
-- Warmup imports every exposed module of every package the component
-- directly depends on, so that the whole external interface set enters
-- GHC's EPS in one order before any other module of the component
-- compiles; without it, @ghc --make -jN@ object output varies with
-- scheduling (GHC.Core.Rules, Note [Overall plumbing for rules]). Every
-- root module of the library and every program imports it. Direct
-- dependencies only: the transitive closure would name modules of packages
-- GHC is not given, which it refuses as hidden. In-place packages count, so
-- a program's Warmup covers the library, and the library's own Warmup is
-- excluded by name so that no program imports it into its own.
--
-- BuildVersion's input is the repository's HEAD, which no file dependency
-- names. Through an external Setup the rules run whenever Setup builds the
-- package; the monitors on the git HEAD and its reflog, and the commit in
-- the rule's argument, are for a build system that runs the rules itself
-- (cabal-install with a Cabal 3.18 setup), which decides from them.
preBuildRules :: PreBuildComponentRules
preBuildRules = rules (static ()) $ \env -> do
  let lbi = env.localBuildInfo
      clbi = env.targetInfo.targetCLBI
      verbosity = buildingWhatVerbosity env.buildingWhat
      component = targetComponent env.targetInfo
  when (wantsWarmup component) $ do
    -- The package databases are read at build time, not from the
    -- configure-time snapshot in installedPkgs lbi: cabal-install does not
    -- reconfigure a package when a dependency's exposed-modules change (an
    -- in-place unit id such as bsc-2026.7-inplace is stable), so the
    -- snapshot goes stale as soon as modules move between packages. This
    -- is the body of Cabal's getInstalledPackagesById, with die' for its
    -- exception.
    index <-
      liftIO $
        getInstalledPackages
          verbosity
          (LBI.compiler lbi)
          (mbWorkDirLBI lbi)
          (withPackageDB lbi)
          (withPrograms lbi)
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
  when (isMainLib component) $ do
    let buildSystem = autogenLocation lbi clbi "BuildSystem"
        buildVersion = autogenLocation lbi clbi "BuildVersion"
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

writeWarmupHs :: (FilePath, [String], [String]) -> IO ()
writeWarmupHs (path, units, mods) =
  writeFileChanged path . unlines $
    [ "-- Generated by SetupHooks.hs; do not edit.",
      "-- An import of every exposed module of every direct dependency of this",
      "-- component, so that the external interface set enters GHC's EPS in one",
      "-- deterministic order before any other module compiles; see",
      "-- preBuildRules in SetupHooks.hs for why.",
      "-- Direct dependencies, as resolved for this build:"
    ]
      <> ["--   " <> u | u <- units]
      <> ["module Warmup where"]
      <> ["import " <> m <> " ()" | m <- mods]

-- | Refuse while the make build's Warmup.hs or BuildSystem.hs lies under
-- src/comp, where GHC would compile it in place of the generated one.
refuseStrayGeneratedModules :: IO ()
refuseStrayGeneratedModules = do
  root <- findRepoRoot
  forM_ (["Warmup", "BuildSystem"] :: [String]) $ \m -> do
    left <- doesFileExist (root </> "src" </> "comp" </> (m <> ".hs"))
    when left . ioError . userError $
      "src/comp/" <> m <> ".hs is the make build's, and GHC would compile it in place of"
        <> " the generated module: remove it (make -C src/comp full_clean) and configure again"

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

-- | Run update-build-version.sh in the autogen directory, where it writes
-- BuildVersion.hs (it leaves an up-to-date file untouched). NOGIT and
-- NOUPDATEBUILDVERSION pass through with the Makefile's defaults.
--
-- The script's git commands search upward from the working directory,
-- which fails when the build directory lies outside the checkout, so the
-- repository is named through GIT_DIR (worktree-safe, from rev-parse) and
-- GIT_WORK_TREE; where the probe fails (an exported archive, or no git)
-- neither is set and the script behaves as before. The second argument,
-- the commit the rule was computed for, only makes the rule's identity
-- follow HEAD; the script reads the repository itself.
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

-- | After a build, link lib/SAT beside the artifact to the directory the
-- solvers were linked from, so that the build tree runs in place: the
-- library links stp and yices by name (bsc.cabal, extra-libraries) and
-- everything resolves them through the rpath $ORIGIN/../lib/SAT
-- (doc/cabal.md section 2). A library's dynamic object is
-- <builddir>/build/libHS*.so, so its lib/SAT is <builddir>/lib/SAT; an
-- executable is <builddir>/build/<exe>/<exe>, so its lib/SAT is
-- <builddir>/build/lib/SAT.
postBuildComponent :: PostBuildComponentHook
postBuildComponent inputs = do
  linked <- solverDirectory
  forM_ linked $ \sat -> do
    let lbi = inputs.localBuildInfo
    build <- makeAbsolute (interpretSymbolicPathLBI lbi (LBI.buildDir lbi))
    case targetComponent inputs.targetInfo of
      CLib _ -> linkSatDir sat (takeDirectory build)
      CExe _ -> linkSatDir sat build
      _ -> pure ()

-- | The directory the solvers are linked from: the first in LIBRARY_PATH
-- that holds both (lib/SAT is one directory). Copies in a system directory
-- resolve on their own and need no link.
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
          pure (all (\lib -> any (("lib" <> lib <> ".") `isPrefixOf`) names) (["stp", "yices"] :: [String]))

-- | Make @dir/lib/SAT@ a link to the solvers' directory, replacing a stale
-- link.
linkSatDir :: FilePath -> FilePath -> IO ()
linkSatDir solvers dir = do
  let sat = dir </> "lib" </> "SAT"
  createDirectoryIfMissing True (dir </> "lib")
  removeDirectoryLink sat `catchIOError` \_ -> pure ()
  createDirectoryLink solvers sat

-- | The flags to compile against and link to Tcl, for the vendored binding
-- (HTcl.hs, haskell.c): a path of the build machine cannot be written in
-- bsc.cabal, so platform.sh is asked at configure time.
tclBuildInfo :: IO BuildInfo
tclBuildInfo = do
  root <- findRepoRoot
  let platform arg = readProcess "sh" [root </> "platform.sh", arg] ""
  let trim = f . f where f = reverse . dropWhile isSpace
  let getArgs flag = fmap (drop (length flag)) . filter (flag `isPrefixOf`)
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

  pure emptyBuildInfo {includeDirs, extraLibDirs, extraLibs, ccOptions = cflags, cppOptions = cflags}
