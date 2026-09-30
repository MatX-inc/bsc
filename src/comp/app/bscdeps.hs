{-# LANGUAGE CPP #-}
module Main_bscdeps(main) where

-- The machine-readable dependency-discovery interface of delivery-plan
-- phase P1 (doc/engine-first-plan.md): report the transitive package
-- closure that `bsc -u` would walk -- post-preprocessing imports, with the
-- effective flags' search-path resolution and the negative probes (the
-- candidate paths that lost to each resolution) -- as tab-separated lines
-- an external coordinator can consume without scraping human diagnostics.
--
-- This is a thin client over the compiler's own modules (Depend does the
-- walking, exactly as -u does); nothing here re-implements parsing or
-- resolution.  The probe lists mirror the candidate order of
-- FileIOUtil.readFilesPath'/existsFilePath (file-major over the search
-- path: every dir for Foo.bsv, then every dir for Foo.bs, then -- for
-- packages that fall back to a binary -- every dir for Foo.bo); if that
-- order changes, this list must change with it.
--
-- Output (tab-separated; first line is the format version):
--   bscdeps-format\t1
--   cwd\t<dir>
--   src\t<root source file as given>
--   pkg\t<name>\t<src|bin>\t<resolved file>
--   imp\t<name>\t<imported package name>
--   inc\t<name>\t<include path>
--   gen\t<name>\t<module to elaborate>
--   foreign\t<name>\t<foreign import id>
--   probe\t<name>\t<candidate path that was not the resolution>
--
-- Errors (for example a missing package) are reported through the
-- compiler's normal error machinery on stderr with a nonzero exit; the
-- machine output on stdout is only complete on exit 0.

import Control.Monad(when)
import Data.List(intercalate)
import System.Environment(getArgs, getProgName)
import System.Directory(getCurrentDirectory)
import System.IO(hSetEncoding, stdout, utf8)

import Depend(findPackages, PkgInfo(..), CompileStatus(..))
import Error(initErrorHandle, setErrorHandleFlags,
             bsError, bsWarning, exitOK, ErrorHandle)
import FileNameUtil(baseName, bscSrcSuffix, bsvSrcSuffix, binSuffix)
import Flags(Flags(..))
import FlagsDecode(Decoded(..), decodeArgs, exitWithUsage)
import Id(getIdString)
import IOUtil(getEnvDef)
import TopUtils(getBluespecDir)

main :: IO ()
main = do
    pprog <- getProgName
    cdir <- getBluespecDir
    args <- getArgs
    bscopts <- getEnvDef "BSC_OPTIONS" ""
    let args' = words bscopts ++ args
    let (warnings, decoded) = decodeArgs (baseName pprog) args' cdir
    errh <- initErrorHandle
    let doWarnings = when (not (null warnings)) $ bsWarning errh warnings
    case decoded of
        DBlueSrc flags src -> do
            setErrorHandleFlags errh flags
            doWarnings
            report errh flags src
        DError msgs -> bsError errh msgs
        _ -> exitWithUsage errh pprog

report :: ErrorHandle -> Flags -> String -> IO ()
report errh flags src = do
    (errs, pis) <- findPackages errh flags src
    cwd <- getCurrentDirectory
    hSetEncoding stdout utf8
    let path = ifcPath flags
        row = intercalate "\t"
        pkgLines pi =
            let n = getIdString (pkgName pi)
                (status, isBin) = case compileStatus pi of
                                      Binary -> ("bin", True)
                                      _      -> ("src", False)
                isRoot = fileName pi == src
            in [ row ["pkg", n, status, fileName pi] ] ++
               [ row ["imp", n, getIdString i] | i <- imports pi ] ++
               [ row ["inc", n, f] | f <- includes pi ] ++
               [ row ["gen", n, getIdString g] | g <- gens pi ] ++
               [ row ["foreign", n, getIdString f] | f <- foreigns pi ] ++
               [ row ["probe", n, c]
                   | not isRoot, c <- probesFor path n isBin (fileName pi) ]
    putStr $ unlines $
        [ row ["bscdeps-format", "1"]
        , row ["cwd", cwd]
        , row ["src", src]
        ] ++ concatMap pkgLines pis
    when (not (null errs)) $ bsError errh errs
    exitOK errh

-- The candidates readFilesPath'/existsFilePath tried, in order, before the
-- winning resolution: sources file-major over the path (Foo.bsv in every
-- dir, then Foo.bs in every dir), then -- only when the package fell back
-- to a library binary -- Foo.bo in every dir.  The list stops at the
-- winner; if the winner is somehow not among the constructed candidates
-- (it always is for non-root packages), every candidate is reported so a
-- consumer errs toward re-running discovery rather than missing a shadow.
probesFor :: [String] -> String -> Bool -> FilePath -> [FilePath]
probesFor path n isBin winner =
    let srcCands = [ p ++ "/" ++ n ++ "." ++ bsvSrcSuffix | p <- path ] ++
                   [ p ++ "/" ++ n ++ "." ++ bscSrcSuffix | p <- path ]
        binCands = [ p ++ "/" ++ n ++ "." ++ binSuffix | p <- path ]
        cands = if isBin then srcCands ++ binCands else srcCands
        (before, rest) = break (== winner) cands
    in case rest of
           (_:_) -> before
           []    -> cands
