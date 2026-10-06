{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers #-}

-- | The package's setup hooks (build-type: Hooks), run in the package
-- directory; doc/cabal.md section 3 says what they do.
-- No hook writes into src/comp, or anywhere outside dist-newstyle and the
-- autogen directories; the make build generates its own Warmup.hs and
-- BuildVersion.hs as before.
module SetupHooks (setupHooks) where

import Control.Monad (filterM, forM_, when)
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
import Distribution.Simple.LocalBuildInfo (hostPlatform, interpretSymbolicPathLBI)
import qualified Distribution.Simple.LocalBuildInfo as LBI
import Distribution.Simple.SetupHooks
import Distribution.Simple.Utils (die', rawSystemExitCode, rewriteFileEx)
import Distribution.System (OS (..))
import Distribution.Verbosity (normal)
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
import System.IO.Error (catchIOError)
import System.Process (readProcess, readProcessWithExitCode)

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

-- | The package directory: Cabal runs the hooks there (cabal-install's
-- external Setup, or a Setup script run by hand).
packageRoot :: IO FilePath
packageRoot = canonicalizePath "."

isMainLib :: Component -> Bool
isMainLib (CLib Library {libName = LMainLibName}) = True
isMainLib _ = False

-- | The autogen location of a generated module of the component.
autogenLocation :: LocalBuildInfo -> ComponentLocalBuildInfo -> ModuleName -> Location
autogenLocation lbi clbi m =
  Location (autogenComponentModulesDir lbi clbi) (moduleNameSymbolicPath m <.> "hs")

-- | The file path of a location, for the action that writes it.
locationFilePath :: LocalBuildInfo -> Location -> FilePath
locationFilePath lbi loc = interpretSymbolicPathLBI lbi (location loc)

-- | The components that get a generated Warmup: those whose stanza
-- declares it (autogen-modules in bsc.cabal).
wantsWarmup :: Component -> Bool
wantsWarmup = elem "Warmup" . autogenModules . LBI.componentBuildInfo

-- | The configure hook: the Tcl flags for the library that holds the
-- vendored binding. The generated modules are declared in bsc.cabal
-- (autogen-modules), as Cabal requires; the rules below write them.
--
-- GHC takes a module found under hs-source-dirs over the autogen one, so a
-- Warmup.hs or BuildSystem.hs the make build has generated under src/comp
-- would be what the library compiles, and the make build's are not this
-- build's: configuring refuses while one is there.
preConfComponent :: PreConfComponentHook
preConfComponent inputs = do
  let c = inputs.component
  bi <-
    if isMainLib c
      then refuseStrayGeneratedModules *> tclBuildInfo
      else pure emptyBuildInfo
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
    -- snapshot goes stale as soon as modules move between packages.
    direct <-
      liftIO $
        getInstalledPackagesById
          verbosity
          lbi
          MissingCoveredInstalledLibrary
          (map fst (componentPackageDeps clbi))
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

writeWarmupHs :: (FilePath, [String]) -> IO ()
writeWarmupHs (path, mods) = do
  createDirectoryIfMissing True (takeDirectory path)
  rewriteFileEx normal path . unlines $
    [ "-- Generated by SetupHooks.hs; do not edit.",
      "-- An import of every exposed module of every direct dependency of this",
      "-- component, so that the external interface set enters GHC's EPS in one",
      "-- deterministic order before any other module compiles; see",
      "-- preBuildRules in SetupHooks.hs for why.",
      "module Warmup where"
    ]
      <> ["import " <> m <> " ()" | m <- mods]

-- | Refuse while the make build's Warmup.hs or BuildSystem.hs lies under
-- src/comp, where GHC would compile it in place of the generated one.
refuseStrayGeneratedModules :: IO ()
refuseStrayGeneratedModules = do
  root <- packageRoot
  forM_ (["Warmup", "BuildSystem"] :: [String]) $ \m -> do
    left <- doesFileExist (root </> "src" </> "comp" </> (m <> ".hs"))
    when left . ioError . userError $
      "src/comp/" <> m <> ".hs is the make build's, and GHC would compile it in place of"
        <> " the generated module: remove it (make -C src/comp full_clean) and configure again"

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
  root <- packageRoot
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
  -- bash runs it, since an sdist stores no executable bit.
  let script = root </> "src" </> "comp" </> "update-build-version.sh"
  code <- rawSystemExitCode normal (Just (makeSymbolicPath dir)) "bash" [script] (Just env)
  case code of
    ExitSuccess -> pure ()
    ExitFailure n -> die' normal ("src/comp/update-build-version.sh exited with " <> show n)

-- | The repository's git directory (worktree-safe) and its HEAD commit, or
-- Nothing where there is no git or no checkout (an exported archive).
gitHead :: IO (Maybe (FilePath, String))
gitHead = do
  root <- packageRoot
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
  root <- packageRoot
  let platform arg = readProcess "sh" [root </> "platform.sh", arg] ""
  let trim = f . f where f = reverse . dropWhile isSpace
  tclInc <- words <$> platform "tclinc"
  tclLibs <- words <$> platform "tcllibs"
  tclVersion <- trim <$> platform "tclversion"

  cflags <- case tclVersion of
    "8.5" -> pure ["-DTCL85"]
    "8.6" -> pure []
    "9.0" -> pure ["-DTCL9"]
    _ -> ioError (userError ("unsupported Tcl version: " <> tclVersion))
  pure (ccLdOptionsBuildInfo (tclInc <> cflags) tclLibs []) {cppOptions = cflags}
