-- The width in bits of a type, from its Bits instance: the proviso
-- Bits t n solved for n by the type checker, as bsc's own type analysis
-- does for the types it describes (TypeAnalysis.getBitWidth, which it
-- keeps to itself).
module TypeWidth(typeWidth) where

import Flags(Flags)
import SymTab(SymTab, findSClass)
import CType(CType, Type, Kind(..), CTypeclass(..), isTNum, getTNum)
import Pred(Pred(..))
import PreIds(idBits)
import MakeSymTab(convCType)
import TIMonad(TI, runTI, tiResult, newTVar, getSubst)
import TCMisc(satisfy, mkVPredFromPred)
import Subst(apSub)

typeWidth :: Flags -> SymTab -> CType -> Maybe Integer
typeWidth flags symtab t0 =
    case (convCType symtab t0, findSClass symtab (CTypeclass idBits)) of
      (Right t, Just bits) ->
          case tiResult (runTI flags False symtab (widthOf bits t)) of
            Right r -> r
            Left _ -> Nothing
      _ -> Nothing
  where
    widthOf bits t = do
        szv <- newTVar "typeWidth" KNum t
        vp <- mkVPredFromPred [] (IsIn bits [t, szv])
        (rs, _) <- satisfy [] [vp]
        s <- getSubst
        let szv' = apSub s szv :: Type
        return $ if null rs && isTNum szv' then Just (getTNum szv') else Nothing
