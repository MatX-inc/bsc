{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

-- | Phase P1 of doc/engine-first-plan.md: the Bluespec libraries
-- (src/Libraries) as one Shake graph.
--
-- What the Makefiles do, and what this reproduces:
--
-- * common.mk: BUILDDIR = TOP/build/bsvlib, INSTALLDIR = PREFIX/lib/Libraries,
--   BSCFLAGS += -stdlib-names -bdir BUILDDIR -p . -vsearch BUILDDIR, tools
--   under PREFIX/bin.
-- * Base1, Base2: every .bs/.bsv is compiled to a .bo in BUILDDIR; Prelude.bs
--   and PreludeBSV.bsv with -no-use-prelude; dependencies from a generated
--   depends.mk.
-- * Base3-Misc, Base3-Contexts, Base3-Math: @bsc -u@ on one root (Misc.bsv
--   with -suppress-warnings T0157:S0080, Contexts.bsv, Math.bsv), which
--   compiles the root's closure; Base3-Contexts then copies Contexts.defines
--   into BUILDDIR.
-- * src/Libraries/Makefile: the directories in BUILD_ORDER (Base1, Base2,
--   Base3-Misc, Base3-Contexts, Base3-Math); tconcheck BUILDDIR; bo2bloogle
--   over every .bo into PREFIX/lib/bloogle/bluespec.txt; install -m644 of
--   everything in BUILDDIR into INSTALLDIR.
--
-- How it is done here: one discovery per library directory (bscdeps on each
-- of its roots, the machine-readable form of the @-u@ walk: packages with
-- their resolution, imports, includes, foreign imports and the probe paths
-- that lost), and one non-recursive bsc worker per package. A directory's
-- discovery needs the outputs of the directories before it in BUILD_ORDER,
-- as @-u@ needs the earlier .bo files to resolve imports to binaries; within
-- a directory every package action depends on exactly the .bo files of its
-- imports. Every output has one producer: a package defined in two
-- directories is an error here, where @-u@ would have silently recompiled
-- it from the later source into the shared BUILDDIR.
module BscEngine.Libraries
  ( Config (..)
  , defaultConfig
  , runLibraries
  ) where

import Warmup ()

import Control.Monad (forM, forM_, unless, when)
import Data.List (intercalate, isPrefixOf, sort, sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import GHC.Generics (Generic)
import System.Directory (copyFile, createDirectoryIfMissing, getPermissions, renameFile, setOwnerExecutable, setOwnerReadable, setOwnerWritable, setPermissions)
import qualified System.Directory as Dir

-- ---------------------------------------------------------------------------
-- configuration

data Config = Config
  { cfgTop :: Maybe FilePath
  , cfgPrefix :: Maybe FilePath
  , cfgBuildDir :: Maybe FilePath
  , cfgBsc :: Maybe FilePath
  , cfgBscDeps :: Maybe FilePath
  , cfgBo2Bloogle :: Maybe FilePath
  , cfgTconCheck :: Maybe FilePath
  , cfgBscFlags :: [String]
  , cfgShakeDir :: Maybe FilePath
  , cfgCompilerKey :: String   -- ^ "binary" or "closure"; see 'compilerKey'
  , cfgJobs :: Int
  , cfgVerbose :: Bool
  , cfgLint :: Bool
  , cfgTargets :: [String]
  }
  deriving (Show)

defaultConfig :: Config
defaultConfig = Config Nothing Nothing Nothing Nothing Nothing Nothing Nothing [] Nothing "binary" 0 False False []

-- | The library directories in the Makefile's BUILD_ORDER, with what their
-- Makefiles do differently from the common rules.
data LibDir = LibDir
  { ldName :: String
  , ldRoots :: Roots
  , ldFlags :: [String]              -- ^ added to every bsc and bscdeps run in this directory
  , ldFileFlags :: [(FilePath, [String])]  -- ^ added for one root/source file
  , ldExtraOutputs :: [(FilePath, FilePath)]  -- ^ (file in the directory, name in BUILDDIR) copied in
  }

data Roots = EverySource | Root FilePath

libDirs :: [LibDir]
libDirs =
  [ LibDir "Base1" EverySource [] [("Prelude.bs", ["-no-use-prelude"]), ("PreludeBSV.bsv", ["-no-use-prelude"])] []
  , LibDir "Base2" EverySource [] [] []
  , LibDir "Base3-Misc" (Root "Misc.bsv") ["-suppress-warnings", "T0157:S0080"] [] []
  , LibDir "Base3-Contexts" (Root "Contexts.bsv") [] [] [("Contexts.defines", "Contexts.defines")]
  , LibDir "Base3-Math" (Root "Math.bsv") [] [] []
  ]

-- ---------------------------------------------------------------------------
-- discovery (bscdeps) and the per-directory plan

-- | One package of a directory's plan, as bscdeps reported it.
data Pkg = Pkg
  { pkgName :: String
  , pkgSrc :: FilePath            -- ^ relative to the library directory, as bscdeps printed it
  , pkgImports :: [String]
  , pkgIncludes :: [FilePath]
  , pkgForeigns :: [String]       -- ^ each produces NAME.ba beside the .bo
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable, Binary, NFData)

-- | What a directory contributes: the packages it compiles (in BUILD_ORDER
-- the earlier directories' outputs are the binaries the later ones import),
-- and the packages it resolved to binaries, which must be produced earlier.
data Plan = Plan
  { planDir :: String
  , planSrc :: [Pkg]
  , planBin :: [String]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable, Binary, NFData)

newtype Discover = Discover String
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult Discover = Plan

-- | Everything the rules need to know, resolved to absolute paths.
data Paths = Paths
  { envTop :: FilePath
  , envLibs :: FilePath           -- ^ TOP/src/Libraries
  , envBuildDir :: FilePath
  , envPrefix :: FilePath
  , envBsc :: FilePath
  , envBscDeps :: FilePath
  , envBo2Bloogle :: FilePath
  , envTconCheck :: FilePath
  , envUserFlags :: [String]
  , envCompilerKey :: String
  }

-- | What a compiled package depends on in the compiler.
--
-- @binary@: the executable itself (and bscdeps for discovery). Any compiler
-- rebuild that changes a byte re-keys every package: P2's conservative
-- whole-compiler key.
--
-- @closure@: an experimental preview of P3. The .bo producer runs parse,
-- typecheck (with the solver layer) and the two codecs (the .ba codec for the
-- foreign-function .ba files, and Depend's staleness check), on top of core
-- and the driver; the backends, the evaluator and the scheduler never run for
-- a library package. So the key is the object files of exactly those
-- components, plus the driver's own, found under the dist-newstyle tree the
-- executable was built in. With -fobject-determinism those objects are
-- byte-identical across a backend edit, so the libraries are not re-keyed by
-- it. P3 generates this closure from the component graph and embeds it in
-- the compiler; here it is written down, and anything it misses is a stale
-- .bo, which is why this is not the default.
compilerKey :: Paths -> FilePath -> Action ()
compilerKey env tool = case envCompilerKey env of
  "closure" -> do
    let dist = distRoot tool
        comps = ["bsc-core", "bsc-stp", "bsc-yices", "bsc-sat", "bsc-bo", "bsc-ba", "bsc-parse", "bsc-typecheck"]
        pats = [c ++ "-*/opt/build//*.o" | c <- comps]
               ++ ["bsc-*/opt/build/" ++ takeFileName tool ++ "/" ++ takeFileName tool ++ "-tmp//*.o"]
    objs <- getDirectoryFiles dist pats
    when (null objs) $ fail ("bsc-engine: --compiler-key closure: no component objects under " ++ dist)
    need (map (dist </>) objs)
  _ -> need [tool]

-- | The per-platform build directory of the dist-newstyle tree a tool was
-- built in (dist-newstyle/build/<platform>/<ghc>), from the tool's path.
distRoot :: FilePath -> FilePath
distRoot tool = go (takeDirectory tool)
  where
    go d
      | takeFileName (takeDirectory (takeDirectory d)) == "build"
        && takeFileName (takeDirectory (takeDirectory (takeDirectory d))) == "dist-newstyle" = d
      | takeDirectory d == d = error ("bsc-engine: " ++ tool ++ " is not under a dist-newstyle tree; --compiler-key closure needs the cabal-built compiler")
      | otherwise = go (takeDirectory d)

commonFlags :: Paths -> [String]
commonFlags e = ["-stdlib-names", "-bdir", envBuildDir e, "-p", ".", "-vsearch", envBuildDir e] ++ envUserFlags e

outputsOf :: Paths -> Pkg -> [FilePath]
outputsOf e p = (envBuildDir e </> pkgName p <.> "bo") : [envBuildDir e </> f <.> "ba" | f <- pkgForeigns p]

planOutputs :: Paths -> LibDir -> Plan -> [FilePath]
planOutputs e ld pl = concatMap (outputsOf e) (planSrc pl) ++ [envBuildDir e </> dst | (_, dst) <- ldExtraOutputs ld]

libDirOf :: String -> LibDir
libDirOf n = case [ld | ld <- libDirs, ldName ld == n] of
  (ld : _) -> ld
  [] -> error ("bsc-engine: not a library directory: " ++ n)

earlier :: String -> [LibDir]
earlier n = takeWhile ((/= n) . ldName) libDirs

-- | Parse bscdeps's tab-separated output (format 1).
parseBscDeps :: String -> Either String [Pkg]
parseBscDeps out = case lines out of
  (first : rest) | map tabSplit [first] == [["bscdeps-format", "1"]] -> Right (assemble (map tabSplit rest))
  (first : _) -> Left ("unexpected first line from bscdeps: " ++ show first)
  [] -> Left "empty output from bscdeps"
  where
    tabSplit s = case break (== '\t') s of
      (a, '\t' : b) -> a : tabSplit b
      (a, _) -> [a]
    assemble rows =
      let pkgs = [(n, (st, f)) | ["pkg", n, st, f] <- rows]
          field tag n = [v | [t, n', v] <- rows, t == tag, n' == n]
       in [ Pkg n (normaliseSrc f) (field "imp" n) (field "inc" n) (field "foreign" n)
          | (n, (st, f)) <- pkgs, st == "src" ]
    -- bscdeps prints the root as given and the packages it found through the
    -- search path with the "./" of "-p ."; one spelling for one file
    normaliseSrc f = case f of
      '.' : '/' : rest -> normaliseSrc rest
      _ -> f

binsOf :: String -> [String]
binsOf out = [n | l <- drop 1 (lines out), ["pkg", n, "bin", _] <- [splitTabs l]]
  where
    splitTabs s = case break (== '\t') s of
      (a, '\t' : b) -> a : splitTabs b
      (a, _) -> [a]

probesOf :: String -> [FilePath]
probesOf out = [p | l <- drop 1 (lines out), ["probe", _, p] <- [splitTabs l]]
  where
    splitTabs s = case break (== '\t') s of
      (a, '\t' : b) -> a : splitTabs b
      (a, _) -> [a]

-- ---------------------------------------------------------------------------
-- the rules

runLibraries :: Config -> IO ()
runLibraries cfg = do
  let top = maybe (error "top") id (cfgTop cfg)
      env = Paths
        { envTop = top
        , envLibs = top </> "src" </> "Libraries"
        , envBuildDir = maybe (error "builddir") id (cfgBuildDir cfg)
        , envPrefix = maybe (error "prefix") id (cfgPrefix cfg)
        , envBsc = maybe (error "bsc") id (cfgBsc cfg)
        , envBscDeps = maybe (error "bscdeps") id (cfgBscDeps cfg)
        , envBo2Bloogle = maybe (error "bo2bloogle") id (cfgBo2Bloogle cfg)
        , envTconCheck = maybe (error "tconcheck") id (cfgTconCheck cfg)
        , envUserFlags = cfgBscFlags cfg
        , envCompilerKey = cfgCompilerKey cfg
        }
      opts = shakeOptions
        { shakeFiles = maybe (error "shake-dir") id (cfgShakeDir cfg)
        , shakeThreads = cfgJobs cfg
        , shakeChange = ChangeModtimeAndDigest
        , shakeVerbosity = if cfgVerbose cfg then Verbose else Info
        , shakeLint = if cfgLint cfg then Just LintBasic else Nothing
        , shakeColor = False
        , shakeProgress = const (pure ())
        }
  shake opts $ do
    want (cfgTargets cfg)
    rules env

rules :: Paths -> Rules ()
rules env = do
  let buildDir = envBuildDir env
      libs = envLibs env
      installDir = envPrefix env </> "lib" </> "Libraries"
      bloogleDir = envPrefix env </> "lib" </> "bloogle"

  -- Discovery: the plan of one library directory. Re-run when any of its
  -- sources, any probe path, the tools, the flags or an earlier directory's
  -- outputs change; dependents re-run only when the plan itself changes.
  discover <- addOracleCache $ \(Discover name) -> do
    let ld = libDirOf name
        dir = libs </> name
    -- bsc and bscdeps require the -bdir directory to exist
    liftIO (createDirectoryIfMissing True buildDir)
    -- The earlier directories' plans are needed for the unique-producer
    -- check only. Discovery resolves a cross-directory import through the
    -- earlier directories' SOURCES (they follow "." on the search path), so
    -- it needs none of their outputs and every directory's discovery can run
    -- at once; the compile actions keep -p . and depend on the imports' .bo.
    earlierPlans <- forM (earlier name) $ \e -> do
      pl <- askOracle (Discover (ldName e))
      pure (e, pl)
    compilerKey env (envBscDeps env)
    roots <- case ldRoots ld of
      EverySource -> do
        fs <- getDirectoryFiles dir ["*.bs", "*.bsv"]
        pure (sort fs)
      Root r -> pure [r]
    let discoveryPath = intercalate ":" ("." : [libs </> ldName e | e <- earlier name])
        discoveryFlags = ["-stdlib-names", "-bdir", buildDir, "-p", discoveryPath, "-vsearch", buildDir] ++ envUserFlags env
    results <- forP roots $ \root -> do
      let flags = discoveryFlags ++ ldFlags ld ++ concat [fl | (f, fl) <- ldFileFlags ld, f == root]
      Stdout out <- command [Cwd dir, EchoStdout False] (envBscDeps env) (flags ++ [root])
      pure (root, out)
    -- negative dependencies: every candidate path that lost a resolution
    let probes = S.toList (S.fromList (concatMap (probesOf . snd) results))
    forM_ probes $ \p -> doesFileExist (if isAbsolute p then p else dir </> p)
    -- the plan: the union over the roots; a package is one source file.
    -- A package whose source is in this directory is local (this directory
    -- compiles it); one found in an earlier directory's sources, or as a
    -- binary in the build directory, is external and must be produced by an
    -- earlier directory.
    let allPkgs = M.elems (M.fromListWith merge [(pkgName p, p) | (_, out) <- results, Right ps <- [parseBscDeps out], p <- ps])
        merge a b
          | pkgSrc a == pkgSrc b = a {pkgImports = nubSort (pkgImports a ++ pkgImports b)}
          | otherwise = error ("bsc-engine: " ++ name ++ ": package " ++ pkgName a ++ " resolved to both "
                               ++ pkgSrc a ++ " and " ++ pkgSrc b ++ " from different roots")
        isLocal p = takeDirectory (pkgSrc p) == "."
        srcPkgs = filter isLocal allPkgs
        bins = nubSort (map pkgName (filter (not . isLocal) allPkgs) ++ concatMap (binsOf . snd) results)
    forM_ results $ \(root, out) -> case parseBscDeps out of
      Left err -> fail ("bsc-engine: bscdeps on " ++ dir </> root ++ ": " ++ err)
      Right _ -> pure ()
    -- the sources themselves (an import edit changes the plan)
    need [dir </> pkgSrc p | p <- srcPkgs]
    -- unique producer: a package defined here and in an earlier directory
    let earlierSrc = M.fromList [(pkgName p, ldName e) | (e, pl) <- earlierPlans, p <- planSrc pl]
        clashes = [(pkgName p, d) | p <- srcPkgs, Just d <- [M.lookup (pkgName p) earlierSrc]]
    unless (null clashes) $
      fail (unlines ("bsc-engine: a package is defined in two library directories (the shared build directory would have two producers for its .bo):"
                     : [ "  " ++ n ++ ": " ++ d ++ " and " ++ name | (n, d) <- clashes ]))
    -- every external resolution must be produced by an earlier directory
    let producedEarlier = S.fromList (M.keys earlierSrc)
        orphans = [b | b <- bins, not (b `S.member` producedEarlier)]
    unless (null orphans) $
      fail (unlines (("bsc-engine: " ++ name ++ " imports these packages from outside its own directory"
                      ++ " and no earlier library directory produces them (a stale .bo in " ++ buildDir ++ "?):")
                     : map ("  " ++) orphans))
    pure (Plan name (sortOn pkgName srcPkgs) bins)

  let askPlan name = discover (Discover name)

      -- the producer of an output in BUILDDIR, searching the directories in
      -- BUILD_ORDER (asking a later directory builds the earlier ones first)
      producerOf :: FilePath -> Action (LibDir, Pkg)
      producerOf out = go libDirs
        where
          go [] = fail ("bsc-engine: no library directory produces " ++ out)
          go (ld : rest) = do
            pl <- askPlan (ldName ld)
            case [p | p <- planSrc pl, out `elem` outputsOf env p] of
              (p : _) -> pure (ld, p)
              [] -> go rest

      compile :: FilePath -> Action ()
      compile out = do
        (ld, p) <- producerOf out
        let dir = libs </> ldName ld
            flags = commonFlags env ++ ldFlags ld ++ concat [fl | (f, fl) <- ldFileFlags ld, f == pkgSrc p]
        compilerKey env (envBsc env)
        need [dir </> pkgSrc p]
        need [dir </> i | i <- pkgIncludes p, not (isAbsolute i)]
        need [i | i <- pkgIncludes p, isAbsolute i]
        need [buildDir </> i <.> "bo" | i <- pkgImports p]
        liftIO (createDirectoryIfMissing True buildDir)
        command_ [Cwd dir] (envBsc env) (flags ++ [pkgSrc p])
        -- the co-products (foreign-function .ba files) are declared, and
        -- their presence checked, so that a missing one is a failure here
        -- rather than a surprise at install
        let outs = outputsOf env p
        produces (filter (/= out) outs)
        forM_ outs $ \o -> do
          ok <- liftIO (doesFileExistIO o)
          unless ok $ fail ("bsc-engine: compiling " ++ pkgSrc p ++ " did not produce " ++ o)

  -- every .bo and .ba in BUILDDIR is produced by compiling its package; a
  -- demanded .ba whose .bo is current but which is itself missing recompiles
  -- the package (deterministic, so the .bo is unchanged)
  (buildDir </> "*.bo") %> compile
  (buildDir </> "*.ba") %> \out -> do
    (_, p) <- producerOf out
    let bo = buildDir </> pkgName p <.> "bo"
    need [bo]
    ok <- liftIO (doesFileExistIO out)
    unless ok $ compile bo

  -- files a directory copies into BUILDDIR as they are (Contexts.defines)
  forM_ libDirs $ \ld -> forM_ (ldExtraOutputs ld) $ \(srcFile, dst) ->
    (buildDir </> dst) %> \out -> do
      copyFileChanged (libs </> ldName ld </> srcFile) out

  let allOutputs = fmap concat $ forM libDirs $ \ld -> do
        pl <- askPlan (ldName ld)
        pure (planOutputs env ld pl)

  phony "build" $ allOutputs >>= need

  phony "tconcheck" $ do
    allOutputs >>= need
    need [envTconCheck env]
    command_ [] (envTconCheck env) [buildDir]

  -- bo2bloogle bluespec BUILDDIR/*.bo > PREFIX/lib/bloogle/bluespec.txt
  (bloogleDir </> "bluespec.txt") %> \out -> do
    outs <- allOutputs
    let bos = sort (filter ((== ".bo") . takeExtension) outs)
    need (envBo2Bloogle env : bos)
    Stdout txt <- command [EchoStdout False] (envBo2Bloogle env) ("bluespec" : bos)
    writeFileChanged out txt
  phony "bloogle" $ need [bloogleDir </> "bluespec.txt"]

  -- install -m644 BUILDDIR/* INSTALLDIR: everything in the build directory,
  -- as the Makefile does; each file published atomically
  phony "install" $ do
    allOutputs >>= need
    need [bloogleDir </> "bluespec.txt"]
    need ["tconcheck"]
    files <- liftIO (listFiles buildDir)
    liftIO (createDirectoryIfMissing True installDir)
    forM_ files $ \f -> liftIO (installFile (buildDir </> f) (installDir </> f))

  phony "plan" $ do
    forM_ libDirs $ \ld -> do
      pl <- askPlan (ldName ld)
      liftIO $ do
        putStrLn (ldName ld ++ ": " ++ show (length (planSrc pl)) ++ " packages, "
                  ++ show (length (planBin pl)) ++ " from earlier directories")
        forM_ (planSrc pl) $ \p ->
          putStrLn ("  " ++ pkgName p ++ " <- " ++ pkgSrc p
                    ++ (if null (pkgImports p) then "" else "  imports " ++ unwords (pkgImports p))
                    ++ (if null (pkgForeigns p) then "" else "  foreign " ++ unwords (pkgForeigns p)))

-- ---------------------------------------------------------------------------
-- helpers

nubSort :: Ord a => [a] -> [a]
nubSort = S.toList . S.fromList

-- | Untracked existence check for a file this rule has just written; the
-- tracked 'doesFileExist' is for inputs.
doesFileExistIO :: FilePath -> IO Bool
doesFileExistIO = Dir.doesFileExist

listFiles :: FilePath -> IO [FilePath]
listFiles dir = do
  names <- Dir.getDirectoryContents dir
  pure (sort [n | n <- names, n /= ".", n /= "..", not ("." `isPrefixOf` n)])

-- | copy to a temporary name beside the destination, chmod 644, rename
installFile :: FilePath -> FilePath -> IO ()
installFile src dst = do
  let tmp = dst ++ ".bsc-engine.tmp"
  copyFile src tmp
  perms <- getPermissions tmp
  setPermissions tmp (setOwnerExecutable False (setOwnerWritable True (setOwnerReadable True perms)))
  renameFile tmp dst
