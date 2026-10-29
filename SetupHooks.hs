{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers #-}

-- | The package's setup hooks (build-type: Hooks; doc/cabal.md section 4).
-- They write only under dist-newstyle, the autogen directories and build/.
module SetupHooks (setupHooks) where

import Control.Monad (filterM, forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Set as Set
import Distribution.InstalledPackageInfo (ExposedModule (..))
import qualified Distribution.InstalledPackageInfo as IPI
import Distribution.ModuleName (ModuleName)
import Distribution.Pretty (prettyShow)
import Distribution.Simple.Configure (ccLdOptionsBuildInfo, getInstalledPackagesById)
import Distribution.Simple.Errors (CabalException (MissingCoveredInstalledLibrary))
import Distribution.Simple.LocalBuildInfo (interpretSymbolicPathLBI)
import qualified Distribution.Simple.LocalBuildInfo as LBI
import Distribution.Simple.SetupHooks
import Distribution.Simple.Utils (die', rawSystemExitCode, rewriteFileEx)
import Distribution.Verbosity (normal)
import Distribution.Utils.Path (makeSymbolicPath, moduleNameSymbolicPath, (<.>))
import System.Directory
  ( canonicalizePath,
    createDirectoryIfMissing,
    createDirectoryLink,
    doesDirectoryExist,
    listDirectory,
    makeAbsolute,
    removeDirectoryLink,
  )
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (splitSearchPath, takeDirectory, (</>))
import System.IO.Error (catchIOError)
import System.Process (readProcess)

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

-- | Whether the stanza declares Warmup (autogen-modules); every stanza must.
wantsWarmup :: Component -> Bool
wantsWarmup = elem "Warmup" . autogenModules . LBI.componentBuildInfo

-- | The configure hook: the Tcl flags, for the library's vendored binding.
-- cabal-install reconfigures for a changed bsc.cabal or a clean build, not for
-- a source or SetupHooks.hs edit, so a change here takes effect at the next of
-- those.
preConfComponent :: PreConfComponentHook
preConfComponent inputs = do
  let c = inputs.component
  bi <- if isMainLib c then tclBuildInfo else pure emptyBuildInfo
  pure PreConfComponentOutputs {componentDiff = buildInfoComponentDiff (componentName c) bi}

-- | The rules that generate the modules, per component.
--
-- Warmup imports every exposed module of every direct dependency, so that the
-- external interfaces enter GHC's EPS in one order before any other module
-- compiles; otherwise @ghc --make -jN@ output varies with scheduling
-- (GHC.Core.Rules, Note [Overall plumbing for rules]). Direct dependencies
-- only, since GHC refuses modules of packages it is not given; the library's
-- own Warmup is excluded by name.
--
-- BuildVersion's input is HEAD, which no file names: the rule runs on every
-- build, and the install stamp makes a new commit a changed source.
preBuildRules :: PreBuildComponentRules
preBuildRules = rules (static ()) $ \env -> do
  let lbi = env.localBuildInfo
      clbi = env.targetInfo.targetCLBI
      verbosity = buildingWhatVerbosity env.buildingWhat
      component = targetComponent env.targetInfo
  unless (wantsWarmup component) . liftIO . die' verbosity $
    prettyShow (componentName component) <> " must list Warmup in autogen-modules (bsc.cabal)"
  -- Read at build time: configure's snapshot (installedPkgs lbi) goes stale,
  -- since cabal-install does not reconfigure when an in-place dependency's
  -- exposed-modules change.
  direct <-
    liftIO $
      getInstalledPackagesById
        verbosity
        lbi
        MissingCoveredInstalledLibrary
        (map fst (componentPackageDeps clbi))
  let mods =
        Set.toAscList . Set.fromList $
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
    let buildVersion = autogenLocation lbi clbi "BuildVersion"
    registerRule_ "BuildVersion.hs" $
      staticRule
        ( mkCommand
            (static Dict)
            (static writeBuildVersionHs)
            (interpretSymbolicPathLBI lbi (autogenComponentModulesDir lbi clbi))
        )
        []
        [buildVersion]

writeWarmupHs :: (FilePath, [String]) -> IO ()
writeWarmupHs (path, mods) = do
  createDirectoryIfMissing True (takeDirectory path)
  rewriteFileEx normal path . unlines $
    [ "-- Generated by SetupHooks.hs (preBuildRules); do not edit.",
      "-- Every exposed module of every direct dependency, imported so that the",
      "-- external interfaces enter GHC in one order before any module compiles.",
      "module Warmup where"
    ]
      <> ["import " <> m <> " ()" | m <- mods]

-- | Run update-build-version.sh in the autogen directory, where it writes
-- BuildVersion.hs (it leaves an up-to-date file untouched), then have it
-- write the install stamp. NOGIT passes through from the environment,
-- defaulted off as the script defaults it.
--
-- The script reads the repository it lives in, whatever the working
-- directory or the caller's Git environment (a build directory outside
-- the checkout, a Git hook of another repository).
writeBuildVersionHs :: FilePath -> IO ()
writeBuildVersionHs autogenDir = do
  createDirectoryIfMissing True autogenDir
  dir <- makeAbsolute autogenDir
  -- The script and the stamp are named by absolute paths: a relative one
  -- would be resolved against the process's working directory, the autogen
  -- dir. bash runs the script, since an sdist stores no executable bit.
  script <- makeAbsolute ("src" </> "comp" </> "update-build-version.sh")
  stamp <- makeAbsolute ("build" </> "cabal-head")
  let run args = do
        code <- rawSystemExitCode normal (Just (makeSymbolicPath dir)) "bash" (script : args) Nothing
        case code of
          ExitSuccess -> pure ()
          ExitFailure n -> die' normal ("src/comp/update-build-version.sh exited with " <> show n)
  run []
  -- The install stamp: HEAD and the version name, a declared source, so
  -- that a later commit, or a tag at this one, is a changed source and this
  -- rule runs again. It has to exist from the first build, since
  -- cabal-install watches only the declared sources it finds when it
  -- records a build.
  run ["--stamp", stamp]

-- | After a build, link lib/SAT beside the artifact to the directory the
-- solvers were linked from, so the build tree runs in place via the rpath
-- $ORIGIN/../lib/SAT: <builddir>/lib/SAT for the library's dynamic object
-- (<builddir>/build/libHS*.so), <builddir>/build/lib/SAT for an executable.
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

-- | The directory the solvers were linked from: the first in LIBRARY_PATH
-- holding both. System copies resolve on their own and need no link.
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

-- | Make @dir/lib/SAT@ a link to the solvers' directory, replacing a stale one.
linkSatDir :: FilePath -> FilePath -> IO ()
linkSatDir solvers dir = do
  let sat = dir </> "lib" </> "SAT"
  createDirectoryIfMissing True (dir </> "lib")
  removeDirectoryLink sat `catchIOError` \_ -> pure ()
  createDirectoryLink solvers sat

-- | The Tcl flags for the vendored binding (HTcl.hs, haskell.c), from
-- platform.sh at configure time: bsc.cabal cannot name a machine's paths.
tclBuildInfo :: IO BuildInfo
tclBuildInfo = do
  let platform arg = readProcess "sh" ["platform.sh", arg] ""
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
