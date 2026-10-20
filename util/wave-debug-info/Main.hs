-- Waveform debug information for one synthesized module, from its
-- elaboration file: the JSON fragment that, with the fragments of the
-- modules it instantiates, describes a design's waveform dumps to
-- Surfer's Bluespec translator (see WaveDebugInfo and README.md).
--
--     wavedebuginfo [bsc flags] <module> <output.json>
--
-- The bsc flags select the backend whose elaboration is read (-sim or
-- -verilog) and the search path of the .ba and .bo files (-p), as for
-- bluetcl; BSC_OPTIONS in the environment is read first.
module Main(main) where

import Control.Monad(foldM, when)
import qualified Data.Map as M
import System.Environment(getArgs, lookupEnv)
import System.Exit(exitFailure)
import System.IO(hPutStrLn, stderr, withBinaryFile, IOMode(WriteMode))
import qualified Data.ByteString.Builder as B

import Error(initErrorHandle, bsWarning, bsError, ErrMsg(..))
import Flags(Flags(..), verbose)
import FlagsDecode(defaultFlags, updateFlags, adjustFinalFlags)
import Backend(Backend(..))
import Position(cmdPosition)
import Id(mk_homeless_id, getIdString)
import CSyntax
import ISyntax(IPackage(..))
import IExpandUtils(HeapData)
import BinUtil(BinMap, readBin, sortImportedSignatures)
import MakeSymTab(mkSymTab)
import ABin(ABin(..), ABinModInfo(..), ABinForeignFuncInfo(..))
import ABinUtil(readAndCheckABinPathCatch)
import ASyntaxUtil(getForeignCallNames)
import ForeignFunctions(ForeignFunction(..))
import WaveDebugInfo(waveDebugInfo, phase)
import WaveLayout(distinct)

main :: IO ()
main = do
    args <- getArgs
    case splitAt (length args - 2) args of
      (flagArgs, [modname, out]) | all isFlag flagArgs -> run flagArgs modname out
      _ -> do hPutStrLn stderr "Usage: wavedebuginfo [bsc flags] <module> <output.json>"
              exitFailure
  where isFlag ('-' : _) = True
        isFlag _ = False

run :: [String] -> String -> FilePath -> IO ()
run flagArgs modname out = do
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
    let readABin name err = readAndCheckABinPathCatch errh (verbose flags) (ifcPath flags) (Just be) name err

    -- the module's elaboration file
    (_, abin) <- phase flags "loading the elaboration" (const 0) $
        readABin modname (cmdPosition, EMissingABinModFile modname Nothing)
    abmi <- case abin of
        ABinMod mi _ -> return mi
        ABinModSchedErr {} -> bsError errh [(cmdPosition, EABinModSchedErr modname Nothing)]
        ABinForeignFunc {} -> do hPutStrLn stderr ("wavedebuginfo: " ++ modname ++ " is a foreign function, not a module")
                                 exitFailure

    -- the foreign functions it calls, which generating its Verilog needs
    ffmap <- case be of
        Bluesim -> return M.empty
        Verilog -> do
            abis <- mapM (\ f -> readABin f (cmdPosition, EMissingABinForeignFuncFile f modname))
                         (getForeignCallNames (abmi_apkg abmi))
            return $ M.fromList [ (getIdString (ff_name ff), ff)
                                | (_, ABinForeignFunc info _) <- abis
                                , let ff = abffi_foreign_func info ]

    -- the package the module comes from and the packages it imports, for
    -- their types
    let load (binmap, hashmap, ps) p = do
            (binmap', hashmap', _, new) <- readBin errh flags Nothing binmap hashmap (mk_homeless_id p)
            return (binmap', hashmap', new ++ ps)
    (binmap, _, ps_read) <- foldM load (M.empty :: BinMap HeapData, M.empty, []) [abmi_src_name abmi]
    let bininfos = [ bi | i <- distinct ps_read, Just bi <- [M.lookup (getIdString i) binmap] ]
        mkCImp (_, _, bo_sig, IPackage { ipkg_name = iid }, _) =
            CImpSign (getIdString iid) False bo_sig
        cpack = CPackage (mk_homeless_id "wavedebuginfo") (Right []) []
                         (sortImportedSignatures (map mkCImp bininfos)) [] [] []
    symtab <- phase flags "loading the packages" (const 0) $ mkSymTab errh cpack

    json <- waveDebugInfo errh flags symtab ffmap abmi
    phase flags "writing" (const 0) $ withBinaryFile out WriteMode (\ h -> B.hPutBuilder h json)
