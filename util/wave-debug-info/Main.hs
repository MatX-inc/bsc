-- Waveform debug information for a design, from its elaboration files:
-- the JSON that Surfer's Bluespec translator reads beside a dump (see
-- WaveDebugInfo and README.md).
--
--     wavedebuginfo [bsc flags] <top module> <output.json>
--
-- The bsc flags select the backend whose elaboration is read (-sim or
-- -verilog) and the search path of the .ba and .bo files (-p), as for
-- bluetcl; BSC_OPTIONS in the environment is read first.
module Main(main) where

import Control.Monad(foldM, when)
import qualified Data.Map as M
import System.Environment(getArgs, lookupEnv)
import System.Exit(exitFailure)
import System.IO(hPutStrLn, stderr)

import Error(initErrorHandle, convExceptTToIO, bsWarning, bsError)
import Flags(Flags(..), verbose)
import FlagsDecode(defaultFlags, updateFlags, adjustFinalFlags)
import Position(cmdPosition)
import Id(mk_homeless_id, getIdString)
import CSyntax
import ISyntax(IPackage(..))
import IExpandUtils(HeapData)
import BinUtil(BinMap, readBin, sortImportedSignatures)
import MakeSymTab(mkSymTab)
import ABin(abemi_src_name)
import ABinUtil(getABIHierarchy)
import SimCCBlock(SimCCBlock(..), primBlocks)
import FileIOUtil(writeFileCatch)
import WaveDebugInfo(waveDebugInfo)
import WaveLayout(distinct)

main :: IO ()
main = do
    args <- getArgs
    case splitAt (length args - 2) args of
      (flagArgs, [top, out]) | not (any isFlag [top, out]) -> run flagArgs top out
      _ -> do hPutStrLn stderr "Usage: wavedebuginfo [bsc flags] <top module> <output.json>"
              exitFailure
  where isFlag ('-' : _) = True
        isFlag _ = False

run :: [String] -> String -> FilePath -> IO ()
run flagArgs top out = do
    errh <- initErrorHandle
    bsdir <- maybe "" id <$> lookupEnv "BLUESPECDIR"
    opts <- maybe "" id <$> lookupEnv "BSC_OPTIONS"
    flags0 <- updateFlags errh cmdPosition (words opts ++ flagArgs) (defaultFlags bsdir)
    let (warns, errs, flags) = adjustFinalFlags [] [] flags0
    when (not (null warns)) $ bsWarning errh warns
    when (not (null errs)) $ bsError errh errs
    be <- case backend flags of
            Just be -> return be
            Nothing -> do hPutStrLn stderr "wavedebuginfo: -sim or -verilog is required"
                          exitFailure

    -- the design's elaboration files, from the top module down
    (_, hierMap, _, _, _, _, abmis_by_name) <-
        convExceptTToIO errh $
          getABIHierarchy errh (verbose flags) (ifcPath flags) (Just be)
                          (map sb_name primBlocks) top []
    let abmis = [ (n, mi) | (n, (mi, _)) <- abmis_by_name ]

    -- the packages the design's modules come from, for their types
    let pkgs = distinct (map (abemi_src_name . snd) abmis)
        load (binmap, hashmap, ps) p = do
            (binmap', hashmap', _, new) <- readBin errh flags Nothing binmap hashmap (mk_homeless_id p)
            return (binmap', hashmap', new ++ ps)
    (binmap, _, ps_read) <- foldM load (M.empty :: BinMap HeapData, M.empty, []) pkgs
    let bininfos = [ bi | i <- distinct ps_read, Just bi <- [M.lookup (getIdString i) binmap] ]
        mkCImp (_, _, bo_sig, IPackage { ipkg_name = iid }, _) =
            CImpSign (getIdString iid) False bo_sig
        cpack = CPackage (mk_homeless_id "wavedebuginfo") (Right []) []
                         (sortImportedSignatures (map mkCImp bininfos)) [] [] []
    symtab <- mkSymTab errh cpack

    json <- waveDebugInfo errh flags symtab hierMap abmis top
    writeFileCatch errh out json
