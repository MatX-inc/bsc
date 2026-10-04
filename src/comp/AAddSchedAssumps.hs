module AAddSchedAssumps
    ( aAddSchedAssumps
    , aAddCFConditionWires
    , getCFConditionWireTemplate
    ) where

import Warmup()
import ASchedMaterialize
    (aAddSchedAssumps, aAddCFConditionWiresWith, needsCFConditionWires)
import CSyntax
import SymTab
import TypeCheck(topExpr)
import Type
import qualified TIMonad as TIM(TIResult(..), runTI)
import Flags(Flags, showElabProgress)
import qualified PhaseConfig as PC
import qualified PhaseConfigLegacy as PL
import IConv(iConvExpr)
import ASyntax
import ASyntaxUtil
import ISyntax
import ISyntaxUtil hiding (noReset)
import IExpandUtils(HExpr, HeapData)
import IExpand(iExpand)
import ISplitIf(iSplitIf)
import AConv(aConv)
import AScheduleInfo(AScheduleInfo)
import PreIds
import qualified Data.Map as M
import PPrint
import Error(internalError, ErrorHandle)

-- | Capture the elaboration-dependent part of wire construction in the
-- module artifact. Avoid elaborating RWire if no conflict-free pair needs it.
getCFConditionWireTemplate :: ErrorHandle -> SymTab -> M.Map AId HExpr ->
                              Flags -> APackage -> IO (Maybe AVInst)
getCFConditionWireTemplate errh symt alldefs flags apkg =
    if needsCFConditionWires apkg
    then fmap Just (getRWireInstTemplate errh flags symt alldefs)
    else return Nothing

aAddCFConditionWires :: ErrorHandle -> SymTab -> M.Map AId HExpr -> Flags ->
                        APackage -> AScheduleInfo ->
                        IO (APackage, AScheduleInfo)
aAddCFConditionWires errh symt alldefs flags apkg schedinfo = do
    template <- getCFConditionWireTemplate errh symt alldefs flags apkg
    return (aAddCFConditionWiresWith template apkg schedinfo)

-- | Elaborate the reusable RWire instance once, while its library definitions
-- and symbol table are available.
getRWireInstTemplate :: ErrorHandle -> Flags -> SymTab ->
                        M.Map AId HExpr -> IO AVInst
getRWireInstTemplate errh flags r alldefs = do
  let blobT = TAp tModule tEmpty
      typeFlags = PL.internalTypecheckFlags (PC.typeSolverFlags flags)
  case TIM.tiResult $ (TIM.runTI typeFlags False r (topExpr blobT (CVar id__mkRWireSubmodule))) of
    Left errs -> internalError (ppReadable errs)
    Right (_,e') -> do
      let iexpr = iConvExpr errh typeFlags r alldefs e'
      let def :: IDef HeapData
          def = IDef id_x (iGetType iexpr) iexpr []
      let flags' = flags { showElabProgress = False }
      iepkg <- iExpand errh flags' r alldefs M.empty False [] def
      rwire_pkg <- aConv errh [] flags (iSplitIf flags iepkg)
      case (apkg_state_instances rwire_pkg) of
        [rwire_inst] ->
          let params = take 1 (avi_iargs rwire_inst) ++ [nullClock, noReset]
              rwire_inst' = rwire_inst { avi_iargs = params }
          in return rwire_inst'
        is -> internalError ("getRWireBlob: " ++ ppReadable is)

-- | A clock that never ticks but is always ready
nullClock :: AExpr
nullClock = ASClock aTClock (AClock aFalse aTrue)

noReset :: AExpr
noReset = aNoReset
