{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, OverloadedLists, OverloadedRecordDot, OverloadedStrings, StaticPointers #-}
-- Toy for brief fact F6: a hook that declares a hidden generated Warmup for
-- the main library and fills it with every exposed module of the component's
-- DIRECT dependencies (in-place packages included), read from Cabal's
-- installed package index. Using the transitive closure instead makes GHC
-- refuse modules of hidden packages.
module ToyHooks (toyHooks) where

import Control.Monad (when)
import Data.List (sort, nub)
import Data.Maybe (mapMaybe)
import qualified Distribution.InstalledPackageInfo as IPI
import Distribution.InstalledPackageInfo (ExposedModule(..))
import Distribution.Pretty (prettyShow)
import Distribution.Simple.LocalBuildInfo (installedPkgs, componentPackageDeps)
import Distribution.Simple.PackageIndex (lookupUnitId)
import Distribution.Simple.SetupHooks
import Distribution.Utils.Path (interpretSymbolicPathCWD, moduleNameSymbolicPath, (<.>))
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

isMainLib :: Component -> Bool
isMainLib (CLib Library {libName = LMainLibName}) = True
isMainLib _ = False

toyHooks :: SetupHooks
toyHooks = noSetupHooks {configureHooks = noConfigureHooks {preConfComponentHook = Just pre}, buildHooks = noBuildHooks {preBuildComponentRules = Just rulesFor}}
  where
    pre inputs
      | isMainLib inputs.component = pure $ PreConfComponentOutputs
          { componentDiff = buildInfoComponentDiff (componentName inputs.component) (emptyBuildInfo {autogenModules = ["Warmup"]}) }
      | otherwise = pure $ noPreConfComponentOutputs inputs
    rulesFor = rules (static ()) $ \env -> do
      let loc = Location (autogenComponentModulesDir env.localBuildInfo env.targetInfo.targetCLBI) (moduleNameSymbolicPath "Warmup" <.> "hs")
          path = interpretSymbolicPathCWD (location loc)
          direct = mapMaybe (lookupUnitId (installedPkgs env.localBuildInfo) . fst) (componentPackageDeps env.targetInfo.targetCLBI)
          mods = sort . nub $ [ prettyShow (exposedName m) | ipi <- direct, m <- IPI.exposedModules ipi ]
          pkgs = sort [ prettyShow (IPI.sourcePackageId ipi) | ipi <- direct ]
      when (isMainLib (targetComponent env.targetInfo)) $
        registerRule_ "Warmup.hs" $ staticRule (mkCommand (static Dict) (static writeWarmup) (path, pkgs, mods)) [] [loc]

writeWarmup :: (FilePath, [String], [String]) -> IO ()
writeWarmup (path, pkgs, mods) = do
  createDirectoryIfMissing True (takeDirectory path)
  writeFile path . unlines $ ("-- deps: " <> unwords pkgs) : "module Warmup where" : [ "import " <> m <> " ()" | m <- mods ]
