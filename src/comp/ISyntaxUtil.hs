{-# LANGUAGE MonoLocalBinds, DataKinds, FlexibleContexts, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Werror=inaccessible-code -Werror=overlapping-patterns #-}
module ISyntaxUtil where

import System.IO(Handle, BufferMode(..))
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe(fromMaybe)
import Util(flattenPairs)
import IntLit
import Undefined
import PPrint(ppReadable, ppString)
import ErrorUtil(internalError)
import Position(noPosition, Position)
import StdPrel
import Prim
import Id
import PreIds
import ISyntax
import ISyntaxSubst(tSubst)
import Wires
import VModInfo(vFields, VFieldInfo(..), lookupOutputClockWires)
import CType(TISort(..), StructSubType(..))
import Changed

--import Debug.Trace

infixr 8 `itFun`

-- Types
itFun :: IType -> IType -> IType
itFun t t' = ITAp (ITAp itArrow t) t'

isFunType :: IType -> Bool
isFunType t = not (null (drop 1 (itSplit t)))

itSplit :: IType -> [IType]
itSplit (ITAp (ITAp arr a) r) | arr == itArrow = a : itSplit r
itSplit t = [t]

itBool, itBit :: IType
itBool = ITCon idBool IKStar tiBool
itBit = ITCon idBit (IKNum `IKFun` IKStar) tiBit

aitBit :: IType -> IType
aitBit t = ITAp itBit t

it0, it1 :: IType
it0 = mkNumConT 0
it1 = mkNumConT 1

itBitN :: Integer -> IType
itBitN n = aitBit (mkNumConT n)

itBit0, itBit1, itNatSize, itNat, itInteger, itReal :: IType
itBit0 = itBitN 0
itBit1 = itBitN 1
itNatSize = mkNumConT 32
itNat = aitBit itNatSize
itInteger = ITCon idInteger IKStar tiInteger
itReal = ITCon idReal IKStar tiReal

itClock, itReset, itInout, itInout_ :: IType
itClock = ITCon idClock IKStar tiClock
itReset = ITCon idReset IKStar tiReset
itInout  = ITCon idInout  (IKStar `IKFun` IKStar) tiInout
itInout_ = ITCon idInout_ (IKNum `IKFun` IKStar) tiInout

itInoutT :: IType -> IType
itInoutT t = ITAp itInout t

itInout_N :: Integer -> IType
itInout_N n = ITAp itInout_ (mkNumConT n)

itPrimArray :: IType
itPrimArray = ITCon idPrimArray (IKStar `IKFun` IKStar) tiPrimArray

itPrimPair :: IType
itPrimPair = ITCon idPrimPair (IKStar `IKFun` IKStar `IKFun` IKStar) tiPair

itPair :: IType -> IType -> IType
itPair t1 t2 = ITAp (ITAp itPrimPair t1) t2

icPair :: KnownPhase a => Id -> IExpr a
{-# SPECIALISE icPair :: Id -> IExpr PreElab #-}
{-# SPECIALISE icPair :: Id -> IExpr Elab #-}
{-# SPECIALISE icPair :: Id -> IExpr PostElab #-}
icPair i = ICon i (ICTuple ct [idPrimFst, idPrimSnd])
  where ct = ITForAll i1 IKStar
              (ITForAll i2 IKStar
               ((ITVar i1) `itFun` (ITVar i2) `itFun` pair_t))
        pair_t = itPair (ITVar i1) (ITVar i2)
        (i1, i2) = take2tmpVarIds

itPosition, itPrimGetPosition :: IType
itPosition = ITCon idPosition IKStar tiPosition
-- type for position extraction primitives
-- PrimGetEvalPosition is only the first example of this
itPrimGetPosition = ITForAll i IKStar (ITVar i `itFun` itPosition)
 where i = take1tmpVarIds

itName :: IType
itName = ITCon idName IKStar tiName

itType :: IType
itType = ITCon idType IKStar tiType

itPred :: IType
itPred = ITCon idPred IKStar tiPred

itSchedPragma :: IType
itSchedPragma = ITCon idSchedPragma IKStar tiSchedPragma

-- type of the Clock constructor
itClockCons, itAction, itPrimUnit :: IType
itClockCons = itBit1 `itFun` itBit1 `itFun` itClock -- XXX is this right?
itAction = ITCon idPrimAction IKStar tiAction
itPrimUnit = ITCon idPrimUnit IKStar tiUnit

-- an unstructured type where it is safe to optimize
-- away undefined values
-- isSimpleType (ITAp c (ITNum n)) | c == itBit = True
isSimpleType :: IType -> Bool
isSimpleType t = t == itInteger ||
                 t == itReal ||
                 t == itString ||
                 t == itChar

isitAction :: IType -> Bool
isitAction (ITAp (ITCon i (IKFun IKStar IKStar) _ ) t)
    | (i == idActionValue_) || (i == idActionValue) = isEmptyType t
isitAction x = (x == itAction)

-- note this returns false for x == () because ActionValue_ () is really an Action
-- Also handle ActionValue_ (Bit 0), which can be introduced by foreign functions.
isitActionValue_ :: IType -> Bool
isitActionValue_ (ITAp (ITCon i (IKFun IKStar IKStar)
                           (TIstruct SStruct [_,_] ) ) t) =
    (i == idActionValue_) && not (isEmptyType t)
isitActionValue_ _ = False

isitActionValue :: IType -> Bool
isitActionValue (ITAp (ITCon i (IKFun IKStar IKStar) _) t) =
    i == idActionValue && t /= itPrimUnit
isitActionValue _ = False

isitInout_ :: IType -> Bool
isitInout_ (ITAp (ITCon i (IKFun IKNum IKStar) _ ) (ITNum x)) = (i == idInout_)
isitInout_ _ = False

getInout_Size :: IType -> Integer
getInout_Size (ITAp ic (ITNum n)) | ic == itInout_ = n
getInout_Size t =
    internalError ("getInout_Size: type is not Inout_: " ++ ppReadable t)


getAV_Type :: IType -> IType
getAV_Type (ITAp (ITCon i (IKFun IKStar IKStar)
                           (TIstruct SStruct [_,_] ) ) t) |
    (i == idActionValue_) = t
getAV_Type t = internalError ("getAV_Type: type is not AV_: " ++ ppReadable t)

getAVType :: IType -> Maybe IType
getAVType (ITAp (ITCon i (IKFun IKStar IKStar) _) t) | i == idActionValue = Just t
getAVType _ = Nothing

itRules, itString, itChar, itHandle, itBufferMode, itFmt :: IType
itRules = ITCon idRules IKStar tiRules
itString = ITCon idString IKStar tiString
itChar = ITCon idChar IKStar tiChar
itHandle = ITCon idHandle IKStar tiHandle
itBufferMode = ITCon idBufferMode IKStar tiBufferMode
itFmt = ITCon idFmt IKStar tiFmt

itStringAt, itFmtAt :: Position -> IType
itStringAt pos = ITCon (idStringAt pos) IKStar tiString
itFmtAt pos = ITCon (idFmtAt pos) IKStar tiFmt

itListCon, itMaybeCon :: IType
itListCon = ITCon idList (IKFun IKStar IKStar) tiList
itMaybeCon = ITCon idMaybe (IKFun IKStar IKStar) tiMaybe

itList, itMaybe :: IType -> IType
itList t = ITAp itListCon t
itMaybe t = ITAp itMaybeCon t

-- Registry of the handwritten ITCon constants in this module, for the
-- tconcheck drift checker (src/comp/tconcheck.hs).
--
-- BSC's front end enforces one type constructor per qualified name, so a
-- qualified Id determines its (kind, sort) payload.  The ITCon constants
-- here are handwritten copies of payloads whose truth lives in the compiled
-- Prelude; tconcheck (run during the build, right after the Prelude
-- libraries are compiled) verifies every registered constant against the
-- Prelude-derived symbol table (kinds bridged with IConv.iConvK), so any
-- edit to the Prelude that changes a payload is caught instead of silently
-- drifting.
--
-- Any new ITCon-literal constant added to this module MUST be registered
-- here.  itArrow is defined in IType.hs (in scope via ISyntax) and is
-- registered here as well; iTLog..iTDiv are defined near the end of this
-- module.  The position-parameterized variants (itStringAt, itFmtAt) share
-- their Id (positions do not participate in Id equality), kind and sort
-- with itString/itFmt, so they are covered by the entries below.
handwrittenITCons :: [IType]
handwrittenITCons =
    [ itArrow,
      itBool, itBit, itInteger, itReal, itClock, itReset, itInout, itInout_,
      itPrimArray, itPrimPair, itPosition, itName, itType, itPred,
      itSchedPragma, itAction, itPrimUnit, itRules, itString, itChar,
      itHandle, itBufferMode, itFmt, itListCon, itMaybeCon,
      iTLog, iTAdd, iTMax, iTMin, iTMul, iTDiv ]

isPairType :: IType -> Bool
isPairType (ITAp (ITAp (ITCon i _ _) _) _) = i == idPrimPair
isPairType _ = False

isEmptyType :: IType -> Bool
isEmptyType (ITCon i _ _) = i == idPrimUnit
isEmptyType (ITAp c (ITNum 0)) = c == itBit
isEmptyType t = False

isBitType :: IType -> Bool
isBitType (ITAp c n) = c == itBit
isBitType _ = False

isBitTupleType :: IType -> Bool
isBitTupleType (ITAp (ITAp (ITCon i _ _) t1) t2) | i == idPrimPair =
  isBitType t1 && isBitTupleType t2
isBitTupleType t = isBitType t

-- extension point for ActionValue methods
isActionType :: IType -> Bool
isActionType x = (x == itAction) || (isitActionValue_ x) || (isitAction x)

-- Constructors
iMkLit :: KnownPhase a => IType -> Integer -> IExpr a
{-# SPECIALISE iMkLit :: IType -> Integer -> IExpr PreElab #-}
{-# SPECIALISE iMkLit :: IType -> Integer -> IExpr Elab #-}
{-# SPECIALISE iMkLit :: IType -> Integer -> IExpr PostElab #-}
iMkLit t i = ICon idIntLit (ICInt { ictInt = t, iVal = ilDec i })

iMkLitAt :: KnownPhase a => Position -> IType -> Integer -> IExpr a
{-# SPECIALISE iMkLitAt :: Position -> IType -> Integer -> IExpr PreElab #-}
{-# SPECIALISE iMkLitAt :: Position -> IType -> Integer -> IExpr Elab #-}
{-# SPECIALISE iMkLitAt :: Position -> IType -> Integer -> IExpr PostElab #-}
iMkLitAt pos t i =
    let ci = setIdPosition pos idIntLit
    in  ICon ci (ICInt { ictInt = t, iVal = ilDec i })

iMkLitWB :: KnownPhase a => IType -> Maybe Integer -> Integer -> Integer -> IExpr a
{-# SPECIALISE iMkLitWB :: IType -> Maybe Integer -> Integer -> Integer -> IExpr PreElab #-}
{-# SPECIALISE iMkLitWB :: IType -> Maybe Integer -> Integer -> Integer -> IExpr Elab #-}
{-# SPECIALISE iMkLitWB :: IType -> Maybe Integer -> Integer -> Integer -> IExpr PostElab #-}
iMkLitWB t w b i =
    let lit = IntLit { ilValue = i, ilBase = b, ilWidth = w }
    in  ICon idIntLit (ICInt { ictInt = t, iVal = lit })

iMkLitWBAt :: KnownPhase a => Position ->
              IType -> Maybe Integer -> Integer -> Integer -> IExpr a
{-# SPECIALISE iMkLitWBAt :: Position -> IType -> Maybe Integer -> Integer -> Integer -> IExpr PreElab #-}
{-# SPECIALISE iMkLitWBAt :: Position -> IType -> Maybe Integer -> Integer -> Integer -> IExpr Elab #-}
{-# SPECIALISE iMkLitWBAt :: Position -> IType -> Maybe Integer -> Integer -> Integer -> IExpr PostElab #-}
iMkLitWBAt pos t w b i =
    let ci = setIdPosition pos idIntLit
        lit = IntLit { ilValue = i, ilBase = b, ilWidth = w }
    in  ICon ci (ICInt { ictInt = t, iVal = lit })

iMkLitSize :: KnownPhase a => Integer -> Integer -> IExpr a
{-# SPECIALISE iMkLitSize :: Integer -> Integer -> IExpr PreElab #-}
{-# SPECIALISE iMkLitSize :: Integer -> Integer -> IExpr Elab #-}
{-# SPECIALISE iMkLitSize :: Integer -> Integer -> IExpr PostElab #-}
iMkLitSize s i =
{-
    if i >= 2^s then
        trace ("big literal " ++ show i ++ " in " ++ show s ++ " bits") $ iMkLit (itBitN s) i
    else
-}
        iMkLit (itBitN s) i

iMkLitSizeAt :: KnownPhase a => Position -> Integer -> Integer -> IExpr a
{-# SPECIALISE iMkLitSizeAt :: Position -> Integer -> Integer -> IExpr PreElab #-}
{-# SPECIALISE iMkLitSizeAt :: Position -> Integer -> Integer -> IExpr Elab #-}
{-# SPECIALISE iMkLitSizeAt :: Position -> Integer -> Integer -> IExpr PostElab #-}
iMkLitSizeAt pos s i = iMkLitAt pos (itBitN s) i

iMkRealLit :: KnownPhase a => Double -> IExpr a
{-# SPECIALISE iMkRealLit :: Double -> IExpr PreElab #-}
{-# SPECIALISE iMkRealLit :: Double -> IExpr Elab #-}
{-# SPECIALISE iMkRealLit :: Double -> IExpr PostElab #-}
iMkRealLit d = ICon idRealLit (ICReal { ictReal = itReal, iReal = d })

iMkRealLitAt :: KnownPhase a => Position -> Double -> IExpr a
{-# SPECIALISE iMkRealLitAt :: Position -> Double -> IExpr PreElab #-}
{-# SPECIALISE iMkRealLitAt :: Position -> Double -> IExpr Elab #-}
{-# SPECIALISE iMkRealLitAt :: Position -> Double -> IExpr PostElab #-}
iMkRealLitAt pos d =
    let i = setIdPosition pos idRealLit
    in  ICon i (ICReal { ictReal = itReal, iReal = d })

iMkPairAt :: KnownPhase a => Position -> IType -> IType -> IExpr a -> IExpr a -> IExpr a
{-# SPECIALISE iMkPairAt :: Position -> IType -> IType -> IExpr PreElab -> IExpr PreElab -> IExpr PreElab #-}
{-# SPECIALISE iMkPairAt :: Position -> IType -> IType -> IExpr Elab -> IExpr Elab -> IExpr Elab #-}
{-# SPECIALISE iMkPairAt :: Position -> IType -> IType -> IExpr PostElab -> IExpr PostElab -> IExpr PostElab #-}
iMkPairAt pos t1 t2 e1 e2 =
    let i = setIdPosition pos idPrimPair
        c = icPair i
    in  iAps c [t1,t2] [e1, e2]

iMkTripleAt :: KnownPhase a => Position ->
               IType -> IType -> IType ->
               IExpr a -> IExpr a -> IExpr a -> IExpr a
{-# SPECIALISE iMkTripleAt :: Position -> IType -> IType -> IType -> IExpr PreElab -> IExpr PreElab -> IExpr PreElab -> IExpr PreElab #-}
{-# SPECIALISE iMkTripleAt :: Position -> IType -> IType -> IType -> IExpr Elab -> IExpr Elab -> IExpr Elab -> IExpr Elab #-}
{-# SPECIALISE iMkTripleAt :: Position -> IType -> IType -> IType -> IExpr PostElab -> IExpr PostElab -> IExpr PostElab -> IExpr PostElab #-}
iMkTripleAt pos t1 t2 t3 e1 e2 e3 = iAps c [t1, pair_t] [e1, iAps c [t2, t3] [e2, e3]]
  where i = setIdPosition pos idPrimPair
        c = icPair i
        pair_t = itPair t2 t3

iMkPosition :: KnownPhase (BinderPhase e) => Position -> IExpr (BinderPhase e)
{-# SPECIALISE iMkPosition :: Position -> IExpr PreElab #-}
{-# SPECIALISE iMkPosition :: Position -> IExpr Elab #-}
iMkPosition pos = iMkPositions [pos]

iMkPositions :: KnownPhase (BinderPhase e) => [Position] -> IExpr (BinderPhase e)
{-# SPECIALISE iMkPositions :: [Position] -> IExpr PreElab #-}
{-# SPECIALISE iMkPositions :: [Position] -> IExpr Elab #-}
iMkPositions poss = ICon idPositionLit (ICPosition itPosition poss)

-- utility for code that expects only one position in ICPosition
getICPosition :: String -> [Position] -> Position
getICPosition _   [pos] = pos
getICPosition str poss  = internalError (str ++ ": " ++ ppReadable poss)

iMkName :: KnownPhase (BinderPhase e) => Id -> Id -> IExpr (BinderPhase e)
{-# SPECIALISE iMkName :: Id -> Id -> IExpr PreElab #-}
{-# SPECIALISE iMkName :: Id -> Id -> IExpr Elab #-}
iMkName id name = ICon id (ICName itName name)

icType :: KnownPhase (BinderPhase e) => Id -> IType -> IExpr (BinderPhase e)
{-# SPECIALISE icType :: Id -> IType -> IExpr PreElab #-}
{-# SPECIALISE icType :: Id -> IType -> IExpr Elab #-}
icType i t = ICon i (ICType { ictType = itType, iType = t })

icPred :: Pred Elab -> IExpr Elab
icPred p = ICon idPredLit (ICPred { ictPred = itPred, iPred = p })

icUndet :: KnownPhase a => IType -> UndefKind -> IExpr a
{-# SPECIALISE icUndet :: IType -> UndefKind -> IExpr PreElab #-}
{-# SPECIALISE icUndet :: IType -> UndefKind -> IExpr Elab #-}
{-# SPECIALISE icUndet :: IType -> UndefKind -> IExpr PostElab #-}
icUndet t u = icUndetAt noPosition t u

icUndetAt :: KnownPhase a => Position -> IType -> UndefKind -> IExpr a
{-# SPECIALISE icUndetAt :: Position -> IType -> UndefKind -> IExpr PreElab #-}
{-# SPECIALISE icUndetAt :: Position -> IType -> UndefKind -> IExpr Elab #-}
{-# SPECIALISE icUndetAt :: Position -> IType -> UndefKind -> IExpr PostElab #-}
icUndetAt pos t u = ICon (dummyId pos) (ICUndet t u)

iMkString :: KnownPhase a => String -> IExpr a
{-# SPECIALISE iMkString :: String -> IExpr PreElab #-}
{-# SPECIALISE iMkString :: String -> IExpr Elab #-}
{-# SPECIALISE iMkString :: String -> IExpr PostElab #-}
iMkString s = ICon idStringLit (ICString itString s)

iMkStringAt :: KnownPhase a => Position -> String -> IExpr a
{-# SPECIALISE iMkStringAt :: Position -> String -> IExpr PreElab #-}
{-# SPECIALISE iMkStringAt :: Position -> String -> IExpr Elab #-}
{-# SPECIALISE iMkStringAt :: Position -> String -> IExpr PostElab #-}
iMkStringAt pos s = ICon (setIdPosition pos idStringLit) (ICString itString s)

iMkStrConcat :: KnownPhase a => IExpr a -> IExpr a -> IExpr a
{-# SPECIALISE iMkStrConcat :: IExpr PreElab -> IExpr PreElab -> IExpr PreElab #-}
{-# SPECIALISE iMkStrConcat :: IExpr Elab -> IExpr Elab -> IExpr Elab #-}
{-# SPECIALISE iMkStrConcat :: IExpr PostElab -> IExpr PostElab -> IExpr PostElab #-}
iMkStrConcat istr1 istr2 = iAps iConcatCon [] [istr1, istr2]
    where itStr = itString
          iConcatCon :: KnownPhase a => IExpr a
          iConcatCon = (ICon idPrimStringConcat
                        (ICPrim (itStr `itFun` (itStr `itFun` itStr))
                         PrimStringConcat))

iMkCharAt :: KnownPhase a => Position -> Char -> IExpr a
{-# SPECIALISE iMkCharAt :: Position -> Char -> IExpr PreElab #-}
{-# SPECIALISE iMkCharAt :: Position -> Char -> IExpr Elab #-}
{-# SPECIALISE iMkCharAt :: Position -> Char -> IExpr PostElab #-}
iMkCharAt pos c = ICon (setIdPosition pos idCharLit) (ICChar itChar c)

iMkHandle :: Handle -> IExpr Elab
iMkHandle h = ICon idHandleLit (ICHandle itHandle h)

iMkBufferMode :: KnownPhase a => BufferMode -> IExpr a
{-# SPECIALISE iMkBufferMode :: BufferMode -> IExpr PreElab #-}
{-# SPECIALISE iMkBufferMode :: BufferMode -> IExpr Elab #-}
{-# SPECIALISE iMkBufferMode :: BufferMode -> IExpr PostElab #-}
iMkBufferMode NoBuffering =
  IAps icPrimChr [mkNumConT 2, itBufferMode] [iMkLitSize 2 0]
iMkBufferMode LineBuffering =
  IAps icPrimChr [mkNumConT 2, itBufferMode] [iMkLitSize 2 1]
iMkBufferMode (BlockBuffering msz) =
  let ic_ty = (itMaybe itInteger) `itFun` itBufferMode
      cti = ConTagInfo { conNo = 2, numCon = 3, conTag = 2, tagSize = 2 }
      ic = ICon idBlockBuffering
             (ICCon { ictCon = ic_ty, conTagInfo = cti })
      e = case msz of
             Nothing -> iMkInvalid itInteger
             Just sz -> iMkValid itInteger (iMkLit itInteger (toInteger sz))
  in  IAps ic [] [e]

iMkInvalid :: KnownPhase a => IType -> IExpr a
{-# SPECIALISE iMkInvalid :: IType -> IExpr PreElab #-}
{-# SPECIALISE iMkInvalid :: IType -> IExpr Elab #-}
{-# SPECIALISE iMkInvalid :: IType -> IExpr PostElab #-}
iMkInvalid t = IAps icPrimChr [mkNumConT 1, itMaybe t] [iMkLitSize 1 0]

iMkValid :: KnownPhase a => IType -> IExpr a -> IExpr a
{-# SPECIALISE iMkValid :: IType -> IExpr PreElab -> IExpr PreElab #-}
{-# SPECIALISE iMkValid :: IType -> IExpr Elab -> IExpr Elab #-}
{-# SPECIALISE iMkValid :: IType -> IExpr PostElab -> IExpr PostElab #-}
iMkValid t e =
  let a = take1tmpVarIds
      ic_ty = ITForAll a IKStar $ (ITVar a) `itFun` (itMaybe (ITVar a))
      cti = ConTagInfo { conNo = 1, numCon = 2, conTag = 1, tagSize = 1 }
      ic = ICon idValid (ICCon { ictCon = ic_ty, conTagInfo = cti })
  in  IAps ic [t] [e]

iMkNil :: KnownPhase a => IType -> IExpr a
{-# SPECIALISE iMkNil :: IType -> IExpr PreElab #-}
{-# SPECIALISE iMkNil :: IType -> IExpr Elab #-}
{-# SPECIALISE iMkNil :: IType -> IExpr PostElab #-}
iMkNil t = IAps icPrimChr [mkNumConT 1, itList t] [iMkLitSize 1 0]

-- The Cons constructor's internal struct type, matching the frontend's
-- anonymous struct (List_$Cons with fields _1, _2) from CParser.  The
-- sort must match what the frontend records for a positional data
-- constructor's struct -- TIstruct (SDataCon parent False) fields --
-- so that this handwritten constant agrees with the List_$Cons tycon
-- built from the Prelude's source.
itListCons :: IType -> IType
itListCons t =
  let tc_id = mkTCId idList (idCons noPosition)
      (id_1:id_2:_) = tupleIds
      ti = TIstruct (SDataCon idList False) [id_1, id_2]
  in  ITAp (ITCon tc_id (IKFun IKStar IKStar) ti) t

iMkCons :: KnownPhase a => IType -> IExpr a -> IExpr a -> IExpr a
{-# SPECIALISE iMkCons :: IType -> IExpr PreElab -> IExpr PreElab -> IExpr PreElab #-}
{-# SPECIALISE iMkCons :: IType -> IExpr Elab -> IExpr Elab -> IExpr Elab #-}
{-# SPECIALISE iMkCons :: IType -> IExpr PostElab -> IExpr PostElab -> IExpr PostElab #-}
iMkCons t e_hd e_tl =
  let a = take1tmpVarIds
      ic_ty = ITForAll a IKStar $
              itListCons (ITVar a) `itFun` (itList (ITVar a))
      cti = ConTagInfo { conNo = 1, numCon = 2, conTag = 1, tagSize = 1 }
      ic = ICon (idCons noPosition)
                (ICCon { ictCon = ic_ty, conTagInfo = cti })
      (id_1:id_2:_) = tupleIds
      tc_id = mkTCId idList (idCons noPosition)
      tup_ty = ITForAll a IKStar $
               (ITVar a) `itFun` itList (ITVar a) `itFun` itListCons (ITVar a)
      tup = ICon tc_id (ICTuple tup_ty [id_1, id_2])
      e = IAps tup [t] [e_hd, e_tl]
  in  IAps ic [t] [e]

iMkList :: KnownPhase a => IType -> [IExpr a] -> IExpr a
{-# SPECIALISE iMkList :: IType -> [IExpr PreElab] -> IExpr PreElab #-}
{-# SPECIALISE iMkList :: IType -> [IExpr Elab] -> IExpr Elab #-}
{-# SPECIALISE iMkList :: IType -> [IExpr PostElab] -> IExpr PostElab #-}
iMkList t xs = foldr (iMkCons t) (iMkNil t) xs

iMkBool :: KnownPhase a => Bool -> IExpr a
{-# SPECIALISE iMkBool :: Bool -> IExpr PreElab #-}
{-# SPECIALISE iMkBool :: Bool -> IExpr Elab #-}
{-# SPECIALISE iMkBool :: Bool -> IExpr PostElab #-}
iMkBool True  = iTrue
iMkBool False = iFalse

iMkBoolAt :: KnownPhase a => Position -> Bool -> IExpr a
{-# SPECIALISE iMkBoolAt :: Position -> Bool -> IExpr PreElab #-}
{-# SPECIALISE iMkBoolAt :: Position -> Bool -> IExpr Elab #-}
{-# SPECIALISE iMkBoolAt :: Position -> Bool -> IExpr PostElab #-}
iMkBoolAt pos True  = iTrueAt pos
iMkBoolAt pos False = iFalseAt pos

iMkRealBool :: KnownPhase a => Bool -> IExpr a
{-# SPECIALISE iMkRealBool :: Bool -> IExpr PreElab #-}
{-# SPECIALISE iMkRealBool :: Bool -> IExpr Elab #-}
{-# SPECIALISE iMkRealBool :: Bool -> IExpr PostElab #-}
iMkRealBool b = IAps (ICon i (ICPrim t PrimChr)) [] [iMkBool b]
  where i = if b then idTrue else idFalse
        t = itBit1 `itFun` itBool

iTrue :: KnownPhase a => IExpr a
{-# SPECIALISE iTrue :: IExpr PreElab #-}
{-# SPECIALISE iTrue :: IExpr Elab #-}
{-# SPECIALISE iTrue :: IExpr PostElab #-}
iTrue = iMkLitSize 1 1

iTrueAt :: KnownPhase a => Position -> IExpr a
{-# SPECIALISE iTrueAt :: Position -> IExpr PreElab #-}
{-# SPECIALISE iTrueAt :: Position -> IExpr Elab #-}
{-# SPECIALISE iTrueAt :: Position -> IExpr PostElab #-}
iTrueAt pos = iMkLitSizeAt pos 1 1

iFalse :: KnownPhase a => IExpr a
{-# SPECIALISE iFalse :: IExpr PreElab #-}
{-# SPECIALISE iFalse :: IExpr Elab #-}
{-# SPECIALISE iFalse :: IExpr PostElab #-}
iFalse = iMkLitSize 1 0

iFalseAt :: KnownPhase a => Position -> IExpr a
{-# SPECIALISE iFalseAt :: Position -> IExpr PreElab #-}
{-# SPECIALISE iFalseAt :: Position -> IExpr Elab #-}
{-# SPECIALISE iFalseAt :: Position -> IExpr PostElab #-}
iFalseAt pos = iMkLitSizeAt pos 1 0

-- conversions between Bit#(1) and Bool

toBit :: KnownPhase a => IExpr a -> IExpr a
{-# SPECIALISE toBit :: IExpr PreElab -> IExpr PreElab #-}
{-# SPECIALISE toBit :: IExpr Elab -> IExpr Elab #-}
{-# SPECIALISE toBit :: IExpr PostElab -> IExpr PostElab #-}
toBit e = IAps icPrimOrd [itBool, it1] [e]

toBool :: KnownPhase a => IExpr a -> IExpr a
{-# SPECIALISE toBool :: IExpr PreElab -> IExpr PreElab #-}
{-# SPECIALISE toBool :: IExpr Elab -> IExpr Elab #-}
{-# SPECIALISE toBool :: IExpr PostElab -> IExpr PostElab #-}
toBool e = IAps icPrimChr [it1, itBool] [e]

-- Functions
ieAnd :: KnownPhase a => IExpr a -> IExpr a -> IExpr a
{-# SPECIALISE ieAnd :: IExpr PreElab -> IExpr PreElab -> IExpr PreElab #-}
{-# SPECIALISE ieAnd :: IExpr Elab -> IExpr Elab -> IExpr Elab #-}
{-# SPECIALISE ieAnd :: IExpr PostElab -> IExpr PostElab -> IExpr PostElab #-}
--ieAnd e1 e2@(IAps (ICon _ (ICPrim _ PrimBAnd)) _ [a,b]) | e1 == a || e1 == b = e2
ieAnd e1 e2 | isTrue e1      = e2
ieAnd e1 e2 | isTrue e2      = e1
ieAnd e1 e2 | isFalse e1     = iFalse
ieAnd e1 e2 | isFalse e2     = iFalse
ieAnd e1 e2                  = IAps iAnd [] [e1, e2]

-- Versions of ieAnd with some optimization -- more expensive call
ieAndOpt :: KnownPhase a => IExpr a -> IExpr a -> IExpr a
{-# SPECIALISE ieAndOpt :: IExpr PreElab -> IExpr PreElab -> IExpr PreElab #-}
{-# SPECIALISE ieAndOpt :: IExpr Elab -> IExpr Elab -> IExpr Elab #-}
{-# SPECIALISE ieAndOpt :: IExpr PostElab -> IExpr PostElab -> IExpr PostElab #-}
ieAndOpt e1 e2 | e1 == e2       = e1
ieAndOpt e1 e2 | e1 == ieNot e2 = iFalse
ieAndOpt e1 e2 = ieAnd e1 e2


ieOr :: KnownPhase a => IExpr a -> IExpr a -> IExpr a
{-# SPECIALISE ieOr :: IExpr PreElab -> IExpr PreElab -> IExpr PreElab #-}
{-# SPECIALISE ieOr :: IExpr Elab -> IExpr Elab -> IExpr Elab #-}
{-# SPECIALISE ieOr :: IExpr PostElab -> IExpr PostElab -> IExpr PostElab #-}
--ieOr e1 e2@(IAps (ICon _ (ICPrim _ PrimBAnd)) _ [a,b]) | e1 == a || e1 == b = e1
ieOr e1 e2 | isFalse e1     = e2
ieOr e1 e2 | isFalse e2     = e1
ieOr e1 e2 | isTrue e1      = iTrue
ieOr e1 e2 | isTrue e2      = iTrue
ieOr e1 e2                  = IAps iOr [] [e1, e2]

ieOrOpt :: KnownPhase a => IExpr a -> IExpr a -> IExpr a
{-# SPECIALISE ieOrOpt :: IExpr PreElab -> IExpr PreElab -> IExpr PreElab #-}
{-# SPECIALISE ieOrOpt :: IExpr Elab -> IExpr Elab -> IExpr Elab #-}
{-# SPECIALISE ieOrOpt :: IExpr PostElab -> IExpr PostElab -> IExpr PostElab #-}
-- a || (a && b) == a
ieOrOpt e1 e2@(IAps (ICon _ (ICPrim _ PrimBAnd)) _ [a,b]) | e1 == a || e1 == b = e1
-- a || (~a && b ) == a || b
ieOrOpt e1 e2@(IAps (ICon _ (ICPrim _ PrimBAnd)) _ [a,b]) | e1 == ieNot a = ieOrOpt e1 b
ieOrOpt e1 e2@(IAps (ICon _ (ICPrim _ PrimBAnd)) _ [a,b]) | e1 == ieNot b = ieOrOpt e1 a
ieOrOpt e1 e2 | e1 == e2       = e1
ieOrOpt e1 e2 | e1 == ieNot e2 = iTrue
ieOrOpt e1 e2 = ieOr e1 e2


ieNot :: KnownPhase a => IExpr a -> IExpr a
{-# SPECIALISE ieNot :: IExpr PreElab -> IExpr PreElab #-}
{-# SPECIALISE ieNot :: IExpr Elab -> IExpr Elab #-}
{-# SPECIALISE ieNot :: IExpr PostElab -> IExpr PostElab #-}
ieNot (IAps (ICon _ (ICPrim _ PrimBNot)) _ [e]) = e
ieNot (IAps p@(ICon _ (ICPrim _ PrimIf)) ts [c,t,e]) = IAps p ts [c, ieNot t, ieNot e]
ieNot e | isFalse e = iTrue
ieNot e | isTrue e  = iFalse
ieNot e = IAps iNot [] [e]

ieIf :: KnownPhase a => IType -> IExpr a -> IExpr a -> IExpr a -> IExpr a
{-# SPECIALISE ieIf :: IType -> IExpr PreElab -> IExpr PreElab -> IExpr PreElab -> IExpr PreElab #-}
{-# SPECIALISE ieIf :: IType -> IExpr Elab -> IExpr Elab -> IExpr Elab -> IExpr Elab #-}
{-# SPECIALISE ieIf :: IType -> IExpr PostElab -> IExpr PostElab -> IExpr PostElab -> IExpr PostElab #-}
--ieIf ty (IAps (ICon _ (ICPrim _ PrimBNot)) _ [c]) t e = ieIf ty c e t
ieIf ty c t e | isTrue c  = t
ieIf ty c t e | isFalse c = e
--ieIf ty c t e | t == e    = t
ieIf ty c t e               = IAps icIf [ty] [c, t, e]

ieIfx :: KnownPhase a => IType -> IExpr a -> IExpr a -> IExpr a -> IExpr a
{-# SPECIALISE ieIfx :: IType -> IExpr PreElab -> IExpr PreElab -> IExpr PreElab -> IExpr PreElab #-}
{-# SPECIALISE ieIfx :: IType -> IExpr Elab -> IExpr Elab -> IExpr Elab -> IExpr Elab #-}
{-# SPECIALISE ieIfx :: IType -> IExpr PostElab -> IExpr PostElab -> IExpr PostElab -> IExpr PostElab #-}
ieIfx ty c t e | t == e                                = t
               | ty == itBit1 && isTrue t && isFalse e = c
               | otherwise                             = ieIf ty c t e

ieArraySel :: KnownPhase a => IType -> Integer -> IExpr a -> [IExpr a] -> IExpr a
{-# SPECIALISE ieArraySel :: IType -> Integer -> IExpr PreElab -> [IExpr PreElab] -> IExpr PreElab #-}
{-# SPECIALISE ieArraySel :: IType -> Integer -> IExpr Elab -> [IExpr Elab] -> IExpr Elab #-}
{-# SPECIALISE ieArraySel :: IType -> Integer -> IExpr PostElab -> [IExpr PostElab] -> IExpr PostElab #-}
-- XXX check if the index is constant and return that element?
ieArraySel elem_ty idx_sz idx es =
  let n = length es
      arr = IAps (icPrimBuildArray n) [elem_ty] es
  in  IAps icPrimArrayDynSelect [elem_ty, ITNum idx_sz] [arr, idx]

-- This is like ieArraySel, except that it takes a default value
ieCase :: KnownPhase a => IType -> Integer -> IExpr a -> [IExpr a] -> IExpr a -> IExpr a
{-# SPECIALISE ieCase :: IType -> Integer -> IExpr PreElab -> [IExpr PreElab] -> IExpr PreElab -> IExpr PreElab #-}
{-# SPECIALISE ieCase :: IType -> Integer -> IExpr Elab -> [IExpr Elab] -> IExpr Elab -> IExpr Elab #-}
{-# SPECIALISE ieCase :: IType -> Integer -> IExpr PostElab -> [IExpr PostElab] -> IExpr PostElab -> IExpr PostElab #-}
ieCase elem_ty idx_sz idx es dflt =
  let idx_ty = itBitN idx_sz
      max_idx = (2 ^ idx_sz) - 1
      mkArm c e = (iMkLit idx_ty c, e)
      arms = zipWith mkArm [0..max_idx] es
      n = length arms
      ces = flattenPairs arms
  in  IAps (icPrimCase n) [ITNum idx_sz, elem_ty] (idx:dflt:ces)

isTrue :: KnownPhase a => IExpr a -> Bool
{-# SPECIALISE isTrue :: IExpr PreElab -> Bool #-}
{-# SPECIALISE isTrue :: IExpr Elab -> Bool #-}
{-# SPECIALISE isTrue :: IExpr PostElab -> Bool #-}
isTrue (ICon _ (ICInt { iVal = IntLit { ilValue = 1 } })) = True
isTrue _ = False

isFalse :: KnownPhase a => IExpr a -> Bool
{-# SPECIALISE isFalse :: IExpr PreElab -> Bool #-}
{-# SPECIALISE isFalse :: IExpr Elab -> Bool #-}
{-# SPECIALISE isFalse :: IExpr PostElab -> Bool #-}
isFalse (ICon _ (ICInt { iVal = IntLit { ilValue = 0 } })) = True
isFalse _ = False

iePrimWhen :: KnownPhase a => IType -> IExpr a -> IExpr a -> IExpr a
{-# SPECIALISE iePrimWhen :: IType -> IExpr PreElab -> IExpr PreElab -> IExpr PreElab #-}
{-# SPECIALISE iePrimWhen :: IType -> IExpr Elab -> IExpr Elab -> IExpr Elab #-}
{-# SPECIALISE iePrimWhen :: IType -> IExpr PostElab -> IExpr PostElab -> IExpr PostElab #-}
iePrimWhen t p e =
    if isTrue p then
        e
    else
        IAps icPrimWhen [t] [p, e]

pTrue :: Pred a
pTrue = PConj S.empty

iePrimWhenPred :: IType -> Pred Elab -> IExpr Elab -> IExpr Elab
iePrimWhenPred t p e =
  if p == pTrue then
      e
  else IAps icPrimWhenPred [t] [icPred p, e]

ieJoinR :: KnownPhase a => IExpr a -> IExpr a -> IExpr a
{-# SPECIALISE ieJoinR :: IExpr PreElab -> IExpr PreElab -> IExpr PreElab #-}
{-# SPECIALISE ieJoinR :: IExpr Elab -> IExpr Elab -> IExpr Elab #-}
{-# SPECIALISE ieJoinR :: IExpr PostElab -> IExpr PostElab -> IExpr PostElab #-}
ieJoinR e1 e2 | e1 == icNoRules = e2
ieJoinR e1 e2 | e2 == icNoRules = e1
ieJoinR e1 e2 = IAps icJoinRules [] [e1, e2]

ieJoinA :: KnownPhase a => IExpr a -> IExpr a -> IExpr a
{-# SPECIALISE ieJoinA :: IExpr PreElab -> IExpr PreElab -> IExpr PreElab #-}
{-# SPECIALISE ieJoinA :: IExpr Elab -> IExpr Elab -> IExpr Elab #-}
{-# SPECIALISE ieJoinA :: IExpr PostElab -> IExpr PostElab -> IExpr PostElab #-}
ieJoinA e1 e2 | e1 == icNoActions = e2
ieJoinA e1 e2 | e2 == icNoActions = e1
ieJoinA e1 e2 = IAps icJoinActions [] [e1, e2]

-- utility method to check for type action
isTAction :: IType -> Bool
isTAction (ITCon a _ _) = a == idPrimAction
isTAction _ = False

-- Constants
iAnd, iOr, iNot :: KnownPhase a => IExpr a
{-# SPECIALISE iAnd :: IExpr PreElab #-}
{-# SPECIALISE iOr :: IExpr PreElab #-}
{-# SPECIALISE iNot :: IExpr PreElab #-}
{-# SPECIALISE iAnd :: IExpr Elab #-}
{-# SPECIALISE iOr :: IExpr Elab #-}
{-# SPECIALISE iNot :: IExpr Elab #-}
{-# SPECIALISE iAnd :: IExpr PostElab #-}
{-# SPECIALISE iOr :: IExpr PostElab #-}
{-# SPECIALISE iNot :: IExpr PostElab #-}
iAnd = ICon idPrimBAnd (ICPrim (itBit1 `itFun` itBit1 `itFun` itBit1) PrimBAnd)
iOr  = ICon idPrimBOr  (ICPrim (itBit1 `itFun` itBit1 `itFun` itBit1) PrimBOr)
iNot = ICon idPrimBNot (ICPrim (itBit1 `itFun` itBit1) PrimBNot)

icJoinRules, icNoRules, icRule, icAddSchedPragmas, icJoinActions, icNoActions :: KnownPhase a => IExpr a
{-# SPECIALISE icJoinRules :: IExpr PreElab #-}
{-# SPECIALISE icNoRules :: IExpr PreElab #-}
{-# SPECIALISE icRule :: IExpr PreElab #-}
{-# SPECIALISE icAddSchedPragmas :: IExpr PreElab #-}
{-# SPECIALISE icJoinActions :: IExpr PreElab #-}
{-# SPECIALISE icNoActions :: IExpr PreElab #-}
{-# SPECIALISE icJoinRules :: IExpr Elab #-}
{-# SPECIALISE icNoRules :: IExpr Elab #-}
{-# SPECIALISE icRule :: IExpr Elab #-}
{-# SPECIALISE icAddSchedPragmas :: IExpr Elab #-}
{-# SPECIALISE icJoinActions :: IExpr Elab #-}
{-# SPECIALISE icNoActions :: IExpr Elab #-}
{-# SPECIALISE icJoinRules :: IExpr PostElab #-}
{-# SPECIALISE icNoRules :: IExpr PostElab #-}
{-# SPECIALISE icRule :: IExpr PostElab #-}
{-# SPECIALISE icAddSchedPragmas :: IExpr PostElab #-}
{-# SPECIALISE icJoinActions :: IExpr PostElab #-}
{-# SPECIALISE icNoActions :: IExpr PostElab #-}
icJoinRules = ICon idPrimJoinRules (ICPrim (itRules `itFun` itRules `itFun` itRules) PrimJoinRules)
icNoRules = ICon idPrimNoRules (ICPrim itRules PrimNoRules)
icRule = ICon idPrimRule (ICPrim (itString `itFun` itBit0 `itFun` itBit1 `itFun` itAction `itFun` itRules) PrimRule)
icAddSchedPragmas = ICon idPrimAddSchedPragmas (ICPrim (itSchedPragma `itFun` itRules `itFun` itRules) PrimAddSchedPragmas)
icJoinActions = ICon idPrimJoinActions (ICPrim (itAction `itFun` itAction `itFun` itAction) PrimJoinActions)
icNoActions = ICon idPrimNoActions (ICPrim itAction PrimNoActions)

icIf :: KnownPhase a => IExpr a
{-# SPECIALISE icIf :: IExpr PreElab #-}
{-# SPECIALISE icIf :: IExpr Elab #-}
{-# SPECIALISE icIf :: IExpr PostElab #-}
icIf = ICon idPrimIf (ICPrim (ITForAll i IKStar (itBit1 `itFun` ty `itFun` ty `itFun` ty)) PrimIf)
  where i = take1tmpVarIds
        ty = ITVar i

icPrimArrayDynSelect :: KnownPhase a => IExpr a
{-# SPECIALISE icPrimArrayDynSelect :: IExpr PreElab #-}
{-# SPECIALISE icPrimArrayDynSelect :: IExpr Elab #-}
{-# SPECIALISE icPrimArrayDynSelect :: IExpr PostElab #-}
icPrimArrayDynSelect = ICon idPrimArrayDynSelect (ICPrim t PrimArrayDynSelect)
  where elem_ty = ITVar a
        arr_ty = ITAp itPrimArray elem_ty
        idx_ty = aitBit (ITVar n)
        t = ITForAll a IKStar $
              ITForAll n IKNum $
                arr_ty `itFun` idx_ty `itFun` elem_ty
        (a, n) = take2tmpVarIds

icPrimBuildArray ::  (KnownPhase b, Num a, Enum a) => a -> IExpr b
{-# SPECIALISE icPrimBuildArray :: (Num a, Enum a) => a -> IExpr PreElab #-}
{-# SPECIALISE icPrimBuildArray :: (Num a, Enum a) => a -> IExpr Elab #-}
{-# SPECIALISE icPrimBuildArray :: (Num a, Enum a) => a -> IExpr PostElab #-}
icPrimBuildArray sz = ICon idPrimBuildArray (ICPrim t PrimBuildArray)
  where elem_ty = ITVar i
        arr_ty = ITAp itPrimArray elem_ty
        t = ITForAll i IKStar $ foldr (\ e f -> elem_ty `itFun` f) arr_ty [1..sz]
        i = take1tmpVarIds

-- n is the number of explicit arms, not counting the default arm
icPrimCase :: (KnownPhase b, Num a, Enum a) => a -> IExpr b
{-# SPECIALISE icPrimCase :: (Num a, Enum a) => a -> IExpr PreElab #-}
{-# SPECIALISE icPrimCase :: (Num a, Enum a) => a -> IExpr Elab #-}
{-# SPECIALISE icPrimCase :: (Num a, Enum a) => a -> IExpr PostElab #-}
icPrimCase sz = ICon idPrimCase (ICPrim t PrimCase)
  where elem_ty = ITVar a
        idx_ty = aitBit (ITVar n)
        t = ITForAll n IKNum $
              ITForAll a IKStar $
                idx_ty `itFun` elem_ty `itFun`
                  (foldr (\ e f -> idx_ty `itFun` elem_ty `itFun` f)
                         elem_ty [1..sz])
        (n, a) = take2tmpVarIds

icPrimOrd :: KnownPhase a => IExpr a
{-# SPECIALISE icPrimOrd :: IExpr PreElab #-}
{-# SPECIALISE icPrimOrd :: IExpr Elab #-}
{-# SPECIALISE icPrimOrd :: IExpr PostElab #-}
icPrimOrd = ICon idPrimOrd (ICPrim t PrimOrd)
  where t = ITForAll a IKStar (ITForAll n IKNum (ITVar a `itFun` aitBit (ITVar n)))
        (a, n) = take2tmpVarIds

icPrimChr :: KnownPhase a => IExpr a
{-# SPECIALISE icPrimChr :: IExpr PreElab #-}
{-# SPECIALISE icPrimChr :: IExpr Elab #-}
{-# SPECIALISE icPrimChr :: IExpr PostElab #-}
icPrimChr = ICon idPrimChr (ICPrim t PrimChr)
  where t = ITForAll n IKNum (ITForAll a IKStar (aitBit (ITVar n) `itFun` ITVar a))
        (n, a) = take2tmpVarIds

icSelect :: KnownPhase a => Position -> IExpr a
{-# SPECIALISE icSelect :: Position -> IExpr PreElab #-}
{-# SPECIALISE icSelect :: Position -> IExpr Elab #-}
{-# SPECIALISE icSelect :: Position -> IExpr PostElab #-}
icSelect pos = ICon (idPrimSelectAt pos) (ICPrim t PrimSelect)
  where t = ITForAll k IKNum (ITForAll m IKNum (ITForAll n IKNum rt))
        rt = aitBit (ITVar n) `itFun` aitBit (ITVar k)
        (k, m, n) = take3tmpVarIds

icPrimConcat :: KnownPhase a => IExpr a
{-# SPECIALISE icPrimConcat :: IExpr PreElab #-}
{-# SPECIALISE icPrimConcat :: IExpr Elab #-}
{-# SPECIALISE icPrimConcat :: IExpr PostElab #-}
icPrimConcat = ICon idPrimConcat (ICPrim t PrimConcat)
  where t = ITForAll k IKNum (ITForAll m IKNum (ITForAll n IKNum rt))
        rt = aitBit (ITVar k) `itFun` aitBit (ITVar m) `itFun` aitBit (ITVar n)
        (k, m, n) = take3tmpVarIds

icPrimMul :: KnownPhase a => IExpr a
{-# SPECIALISE icPrimMul :: IExpr PreElab #-}
{-# SPECIALISE icPrimMul :: IExpr Elab #-}
{-# SPECIALISE icPrimMul :: IExpr PostElab #-}
icPrimMul = ICon idPrimMul (ICPrim t PrimMul)
  where t = ITForAll k IKNum (ITForAll m IKNum (ITForAll n IKNum rt))
        rt = aitBit (ITVar k) `itFun` aitBit (ITVar m) `itFun` aitBit (ITVar n)
        (k, m, n) = take3tmpVarIds

icPrimQuot :: KnownPhase a => IExpr a
{-# SPECIALISE icPrimQuot :: IExpr PreElab #-}
{-# SPECIALISE icPrimQuot :: IExpr Elab #-}
{-# SPECIALISE icPrimQuot :: IExpr PostElab #-}
icPrimQuot = ICon idPrimQuot (ICPrim t PrimQuot)
  where t = ITForAll k IKNum (ITForAll n IKNum rt)
        rt = aitBit (ITVar k) `itFun` aitBit (ITVar n) `itFun` aitBit (ITVar k)
        (k, n) = take2tmpVarIds

icPrimRem :: KnownPhase a => IExpr a
{-# SPECIALISE icPrimRem :: IExpr PreElab #-}
{-# SPECIALISE icPrimRem :: IExpr Elab #-}
{-# SPECIALISE icPrimRem :: IExpr PostElab #-}
icPrimRem = ICon idPrimRem (ICPrim t PrimRem)
  where t = ITForAll k IKNum (ITForAll n IKNum rt)
        rt = aitBit (ITVar k) `itFun` aitBit (ITVar n) `itFun` aitBit (ITVar n)
        (k, n) = take2tmpVarIds

icPrimZeroExt :: KnownPhase a => IExpr a
{-# SPECIALISE icPrimZeroExt :: IExpr PreElab #-}
{-# SPECIALISE icPrimZeroExt :: IExpr Elab #-}
{-# SPECIALISE icPrimZeroExt :: IExpr PostElab #-}
icPrimZeroExt = ICon idPrimZeroExt (ICPrim t PrimZeroExt)
  where t = ITForAll m IKNum (ITForAll k IKNum (ITForAll n IKNum rt))
        rt = aitBit (ITVar k) `itFun` aitBit (ITVar n)
        (k, m, n) = take3tmpVarIds

icPrimSignExt :: KnownPhase a => IExpr a
{-# SPECIALISE icPrimSignExt :: IExpr PreElab #-}
{-# SPECIALISE icPrimSignExt :: IExpr Elab #-}
{-# SPECIALISE icPrimSignExt :: IExpr PostElab #-}
icPrimSignExt = ICon idPrimSignExt (ICPrim t PrimSignExt)
  where t = ITForAll m IKNum (ITForAll k IKNum (ITForAll n IKNum rt))
        rt = aitBit (ITVar k) `itFun` aitBit (ITVar n)
        (k, m, n) = take3tmpVarIds

icPrimTrunc :: KnownPhase a => IExpr a
{-# SPECIALISE icPrimTrunc :: IExpr PreElab #-}
{-# SPECIALISE icPrimTrunc :: IExpr Elab #-}
{-# SPECIALISE icPrimTrunc :: IExpr PostElab #-}
icPrimTrunc = ICon idPrimTrunc (ICPrim t PrimTrunc)
  where t = ITForAll k IKNum (ITForAll m IKNum (ITForAll n IKNum rt))
        rt = aitBit (ITVar n) `itFun` aitBit (ITVar m)
        (k, m, n) = take3tmpVarIds

icPrimRel :: KnownPhase a => Id -> PrimOp -> IExpr a
{-# SPECIALISE icPrimRel :: Id -> PrimOp -> IExpr PreElab #-}
{-# SPECIALISE icPrimRel :: Id -> PrimOp -> IExpr Elab #-}
{-# SPECIALISE icPrimRel :: Id -> PrimOp -> IExpr PostElab #-}
icPrimRel id p = ICon id (ICPrim (ITForAll i IKNum (ty `itFun` ty `itFun` itBit1)) p)
  where i = take1tmpVarIds
        ty = itBit `ITAp` ITVar i

icPrimWhen :: KnownPhase a => IExpr a
{-# SPECIALISE icPrimWhen :: IExpr PreElab #-}
{-# SPECIALISE icPrimWhen :: IExpr Elab #-}
{-# SPECIALISE icPrimWhen :: IExpr PostElab #-}
icPrimWhen = ICon idPrimWhen (ICPrim t PrimWhen)
  where t = ITForAll i IKStar (itBit1 `itFun` ITVar i `itFun` ITVar i)
        i = take1tmpVarIds

icPrimWhenPred :: KnownPhase a => IExpr a
{-# SPECIALISE icPrimWhenPred :: IExpr PreElab #-}
{-# SPECIALISE icPrimWhenPred :: IExpr Elab #-}
{-# SPECIALISE icPrimWhenPred :: IExpr PostElab #-}
icPrimWhenPred = ICon idPrimWhen (ICPrim t PrimWhenPred)
  where t = ITForAll i IKStar (itPred `itFun` ITVar i `itFun` ITVar i)
        i = take1tmpVarIds

itUninitialized :: IType
itUninitialized = ITForAll i IKStar (itPosition `itFun` itString `itFun` ITVar i)
  where i = take1tmpVarIds

icPrimRawUninitialized, icPrimUninitialized :: KnownPhase a => IExpr a
{-# SPECIALISE icPrimRawUninitialized :: IExpr PreElab #-}
{-# SPECIALISE icPrimUninitialized :: IExpr PreElab #-}
{-# SPECIALISE icPrimRawUninitialized :: IExpr Elab #-}
{-# SPECIALISE icPrimUninitialized :: IExpr Elab #-}
{-# SPECIALISE icPrimRawUninitialized :: IExpr PostElab #-}
{-# SPECIALISE icPrimUninitialized :: IExpr PostElab #-}
icPrimRawUninitialized = ICon idPrimRawUninitialized (ICPrim itUninitialized PrimRawUninitialized)
icPrimUninitialized = ICon idPrimUninitialized (ICPrim itUninitialized PrimUninitialized)

icPrimSetSelPosition :: KnownPhase a => IExpr a
{-# SPECIALISE icPrimSetSelPosition :: IExpr PreElab #-}
{-# SPECIALISE icPrimSetSelPosition :: IExpr Elab #-}
{-# SPECIALISE icPrimSetSelPosition :: IExpr PostElab #-}
icPrimSetSelPosition = ICon idPrimSetSelPosition (ICPrim t PrimSetSelPosition)
  where t = ITForAll i IKStar (itPosition `itFun` ITVar i `itFun` ITVar i)
        i = take1tmpVarIds

icPrimSL :: KnownPhase a => IExpr a
{-# SPECIALISE icPrimSL :: IExpr PreElab #-}
{-# SPECIALISE icPrimSL :: IExpr Elab #-}
{-# SPECIALISE icPrimSL :: IExpr PostElab #-}
icPrimSL = ICon idPrimSL (ICPrim t PrimSL)
  where t = ITForAll i IKNum (ty `itFun` itNat `itFun` ty)
        ty = itBit `ITAp` ITVar i
        i = take1tmpVarIds

icPrimSRL :: KnownPhase a => IExpr a
{-# SPECIALISE icPrimSRL :: IExpr PreElab #-}
{-# SPECIALISE icPrimSRL :: IExpr Elab #-}
{-# SPECIALISE icPrimSRL :: IExpr PostElab #-}
icPrimSRL = ICon idPrimSRL (ICPrim t PrimSRL)
  where t = ITForAll i IKNum (ty `itFun` itNat `itFun` ty)
        ty = itBit `ITAp` ITVar i
        i = take1tmpVarIds

icPrimEQ, icPrimULE, icPrimULT, icPrimSLE, icPrimSLT :: KnownPhase a => IExpr a
{-# SPECIALISE icPrimEQ :: IExpr PreElab #-}
{-# SPECIALISE icPrimULE :: IExpr PreElab #-}
{-# SPECIALISE icPrimULT :: IExpr PreElab #-}
{-# SPECIALISE icPrimSLE :: IExpr PreElab #-}
{-# SPECIALISE icPrimSLT :: IExpr PreElab #-}
{-# SPECIALISE icPrimEQ :: IExpr Elab #-}
{-# SPECIALISE icPrimULE :: IExpr Elab #-}
{-# SPECIALISE icPrimULT :: IExpr Elab #-}
{-# SPECIALISE icPrimSLE :: IExpr Elab #-}
{-# SPECIALISE icPrimSLT :: IExpr Elab #-}
{-# SPECIALISE icPrimEQ :: IExpr PostElab #-}
{-# SPECIALISE icPrimULE :: IExpr PostElab #-}
{-# SPECIALISE icPrimULT :: IExpr PostElab #-}
{-# SPECIALISE icPrimSLE :: IExpr PostElab #-}
{-# SPECIALISE icPrimSLT :: IExpr PostElab #-}
icPrimEQ = icPrimRel idPrimEQ PrimEQ
icPrimULE = icPrimRel idPrimULE PrimULE
icPrimULT = icPrimRel idPrimULT PrimULT
icPrimSLE = icPrimRel idPrimSLE PrimSLE
icPrimSLT = icPrimRel idPrimSLT PrimSLT

-- For primitive functions of type (Bit n -> Bit n -> Bit n)
icPrimBinVecOp :: KnownPhase a => Id -> PrimOp -> IExpr a
{-# SPECIALISE icPrimBinVecOp :: Id -> PrimOp -> IExpr PreElab #-}
{-# SPECIALISE icPrimBinVecOp :: Id -> PrimOp -> IExpr Elab #-}
{-# SPECIALISE icPrimBinVecOp :: Id -> PrimOp -> IExpr PostElab #-}
icPrimBinVecOp id p = ICon id (ICPrim t p)
  where t = ITForAll i IKNum (ty `itFun` ty `itFun` ty)
        i = take1tmpVarIds
        ty = itBit `ITAp` ITVar i

icPrimAdd, icPrimSub :: KnownPhase a => IExpr a
{-# SPECIALISE icPrimAdd :: IExpr PreElab #-}
{-# SPECIALISE icPrimSub :: IExpr PreElab #-}
{-# SPECIALISE icPrimAdd :: IExpr Elab #-}
{-# SPECIALISE icPrimSub :: IExpr Elab #-}
{-# SPECIALISE icPrimAdd :: IExpr PostElab #-}
{-# SPECIALISE icPrimSub :: IExpr PostElab #-}
icPrimAdd = icPrimBinVecOp idPrimAdd PrimAdd
icPrimSub = icPrimBinVecOp idPrimSub PrimSub

icPrimInv :: KnownPhase a => IExpr a
{-# SPECIALISE icPrimInv :: IExpr PreElab #-}
{-# SPECIALISE icPrimInv :: IExpr Elab #-}
{-# SPECIALISE icPrimInv :: IExpr PostElab #-}
icPrimInv = ICon idPrimSL (ICPrim t PrimInv)
  where t = ITForAll i IKNum (ty `itFun` ty)
        i = take1tmpVarIds
        ty = itBit `ITAp` ITVar i

icPrimIntegerToBit :: KnownPhase a => IExpr a
{-# SPECIALISE icPrimIntegerToBit :: IExpr PreElab #-}
{-# SPECIALISE icPrimIntegerToBit :: IExpr Elab #-}
{-# SPECIALISE icPrimIntegerToBit :: IExpr PostElab #-}
icPrimIntegerToBit = ICon (idFromInteger noPosition) (ICPrim t PrimIntegerToBit)
  where t  = ITForAll i IKNum (itInteger `itFun` (aitBit ty))
        ty = ITVar i
        i  = take1tmpVarIds

icClock :: KnownPhase (EvaldPhase b) => Id -> IClock (EvaldPhase b) -> IExpr (EvaldPhase b)
{-# SPECIALISE icClock :: Id -> IClock Elab -> IExpr Elab #-}
{-# SPECIALISE icClock :: Id -> IClock PostElab -> IExpr PostElab #-}
icClock i c = ICon i (ICClock {ictClock = itClock, iClock = c})

icReset :: KnownPhase (EvaldPhase b) => Id -> IReset (EvaldPhase b) -> IExpr (EvaldPhase b)
{-# SPECIALISE icReset :: Id -> IReset Elab -> IExpr Elab #-}
{-# SPECIALISE icReset :: Id -> IReset PostElab -> IExpr PostElab #-}
icReset i r = ICon i (ICReset {ictReset = itReset, iReset = r})

icInout :: KnownPhase (EvaldPhase b) => Id -> Integer -> IInout (EvaldPhase b) -> IExpr (EvaldPhase b)
{-# SPECIALISE icInout :: Id -> Integer -> IInout Elab -> IExpr Elab #-}
{-# SPECIALISE icInout :: Id -> Integer -> IInout PostElab -> IExpr PostElab #-}
icInout i sz iot = ICon i (ICInout {ictInout = itInout_N sz, iInout = iot})

icSelClockOsc :: KnownPhase (EvaldPhase b) => Id -> IClock (EvaldPhase b) -> IExpr (EvaldPhase b)
{-# SPECIALISE icSelClockOsc :: Id -> IClock Elab -> IExpr Elab #-}
{-# SPECIALISE icSelClockOsc :: Id -> IClock PostElab -> IExpr PostElab #-}
icSelClockOsc i c =
    IAps (ICon idClockOsc (ICSel { ictSel = itClock `itFun` itBit1,
                                    selNo = 0,
                                    numSel = 2 }))
         []
         [icClock i c]

icSelClockGate :: KnownPhase (EvaldPhase b) => Id -> IClock (EvaldPhase b) -> IExpr (EvaldPhase b)
{-# SPECIALISE icSelClockGate :: Id -> IClock Elab -> IExpr Elab #-}
{-# SPECIALISE icSelClockGate :: Id -> IClock PostElab -> IExpr PostElab #-}
icSelClockGate i c =
    IAps (ICon idClockGate (ICSel { ictSel = itClock `itFun` itBit1,
                                    selNo = 1,
                                    numSel = 2 }))
         []
         [icClock i c]

icNoClock, icNoReset :: KnownPhase (EvaldPhase b) => IExpr (EvaldPhase b)
{-# SPECIALISE icNoClock :: IExpr Elab #-}
{-# SPECIALISE icNoReset :: IExpr Elab #-}
{-# SPECIALISE icNoClock :: IExpr PostElab #-}
{-# SPECIALISE icNoReset :: IExpr PostElab #-}
icNoClock = icClock idNoClock noClock
icNoReset = icReset idNoReset noReset
icNoPosition :: KnownPhase (BinderPhase e) => IExpr (BinderPhase e)
{-# SPECIALISE icNoPosition :: IExpr PreElab #-}
{-# SPECIALISE icNoPosition :: IExpr Elab #-}
icNoPosition = ICon idNoPosition (ICPosition { ictPosition = itPosition, iPosition = [noPosition] })

-- turn an oscillator and gate expression into a clock wires tuple
makeClockWires :: KnownPhase a => IExpr a -> IExpr a -> IExpr a
{-# SPECIALISE makeClockWires :: IExpr PreElab -> IExpr PreElab -> IExpr PreElab #-}
{-# SPECIALISE makeClockWires :: IExpr Elab -> IExpr Elab -> IExpr Elab #-}
{-# SPECIALISE makeClockWires :: IExpr PostElab -> IExpr PostElab -> IExpr PostElab #-}
makeClockWires osc gate = iAps (ICon idClock (ICTuple {ictTuple = itClockCons, fieldIds = [idClockOsc, idClockGate]})) [] [osc, gate]

noClock :: KnownPhase a => IClock a
{-# SPECIALISE noClock :: IClock PreElab #-}
{-# SPECIALISE noClock :: IClock Elab #-}
{-# SPECIALISE noClock :: IClock PostElab #-}
noClock = makeClock noClockId noClockDomain (makeClockWires (iMkLit itBit1 0) (iMkLit itBit1 0))

missingDefaultClock :: KnownPhase a => IClock a
{-# SPECIALISE missingDefaultClock :: IClock PreElab #-}
{-# SPECIALISE missingDefaultClock :: IClock Elab #-}
{-# SPECIALISE missingDefaultClock :: IClock PostElab #-}
-- XXX should the wires evaluate to error?
missingDefaultClock = makeClock noDefaultClockId noClockDomain (makeClockWires (iMkLit itBit1 0) (iMkLit itBit1 0))

noReset :: KnownPhase a => IReset a
{-# SPECIALISE noReset :: IReset PreElab #-}
{-# SPECIALISE noReset :: IReset Elab #-}
{-# SPECIALISE noReset :: IReset PostElab #-}
noReset = makeReset noResetId noClock (ICon idNoReset (ICPrim itBit1 PrimResetUnassertedVal))

-- XXX should the reset wire be an error?
missingDefaultReset :: KnownPhase a => IReset a
{-# SPECIALISE missingDefaultReset :: IReset PreElab #-}
{-# SPECIALISE missingDefaultReset :: IReset Elab #-}
{-# SPECIALISE missingDefaultReset :: IReset PostElab #-}
missingDefaultReset = makeReset noDefaultResetId noClock (iMkLit itBit1 1)

-- clock extraction utilities
-- required here because they need noClock which needs itClockCons
-- (via makeClockWires)
getNamedClock :: Id -> IStateVar a -> IClock a
getNamedClock i v =
    -- XXX unQualId VModInfo
    case (lookup (unQualId i) (getClockMap v)) of
        Just c -> c
        Nothing -> internalError
                     ("ISyntaxUtil.getNamedClockFromMap: unknown clock " ++
                      (ppReadable i) ++ (ppReadable v) ++
                      (ppReadable (getClockMap v)))

getMethodClock :: KnownPhase a => Id -> IStateVar a -> IClock a
{-# SPECIALISE getMethodClock :: Id -> IStateVar PreElab -> IClock PreElab #-}
{-# SPECIALISE getMethodClock :: Id -> IStateVar Elab -> IClock Elab #-}
{-# SPECIALISE getMethodClock :: Id -> IStateVar PostElab -> IClock PostElab #-}
getMethodClock i v@(IStateVar { isv_vmi = vmi }) =
    case mclock_name of
        Nothing -> noClock
        Just n  -> getNamedClock n v
  -- XXX unQualId VModInfo
  where mclock_names =
            [ c | Method { vf_name = n, vf_clock = c } <- vFields vmi,
                  n == unQualId i]
        mclock_name =
            case mclock_names of
                [n] -> n
                _   -> internalError ("ISyntaxUtil.getMethodClock: " ++
                                      (ppReadable vmi) ++ (ppReadable i))

getIfcInoutClock :: KnownPhase a => Id -> IStateVar a -> IClock a
{-# SPECIALISE getIfcInoutClock :: Id -> IStateVar PreElab -> IClock PreElab #-}
{-# SPECIALISE getIfcInoutClock :: Id -> IStateVar Elab -> IClock Elab #-}
{-# SPECIALISE getIfcInoutClock :: Id -> IStateVar PostElab -> IClock PostElab #-}
getIfcInoutClock i v@(IStateVar { isv_vmi = vmi }) =
    case mclock_name of
        Nothing -> noClock
        Just n  -> getNamedClock n v
  -- XXX unQualId VModInfo
  where mclock_names =
            [ c | Inout { vf_name = n, vf_clock = c } <- vFields vmi,
                  n == unQualId i]
        mclock_name =
            case mclock_names of
                [n] -> n
                _   -> internalError ("ISyntaxUtil.getIfcInoutClock: " ++
                                      (ppReadable vmi) ++ (ppReadable i))

-- reset extraction utilities (like the clock extraction utilities)
-- they also need noReset
getNamedReset :: Id -> IStateVar a -> IReset a
getNamedReset i v =
    -- XXX unQualId VModInfo
    case (lookup (unQualId i) (getResetMap v)) of
        Just r -> r
        Nothing -> internalError
                     ("ISyntaxUtil.getNamedResetFromMap: unknown reset " ++
                      (ppReadable i) ++ (ppReadable v) ++
                      (ppReadable (getResetMap v)))

getMethodReset :: KnownPhase a => Id -> IStateVar a -> IReset a
{-# SPECIALISE getMethodReset :: Id -> IStateVar PreElab -> IReset PreElab #-}
{-# SPECIALISE getMethodReset :: Id -> IStateVar Elab -> IReset Elab #-}
{-# SPECIALISE getMethodReset :: Id -> IStateVar PostElab -> IReset PostElab #-}
getMethodReset i v@(IStateVar { isv_vmi = vmi }) =
    case mreset_name of
        Nothing -> noReset
        Just n  -> getNamedReset n v
  -- XXX unQualId VModInfo
  where mreset_names =
            [ r | Method { vf_name = n, vf_reset = r } <- vFields vmi,
                  n == unQualId i]
        mreset_name =
            case mreset_names of
                [n] -> n
                _   -> internalError ("ISyntaxUtil.getMethodReset: " ++
                                      (ppReadable vmi) ++ (ppReadable i))

getIfcInoutReset :: KnownPhase a => Id -> IStateVar a -> IReset a
{-# SPECIALISE getIfcInoutReset :: Id -> IStateVar PreElab -> IReset PreElab #-}
{-# SPECIALISE getIfcInoutReset :: Id -> IStateVar Elab -> IReset Elab #-}
{-# SPECIALISE getIfcInoutReset :: Id -> IStateVar PostElab -> IReset PostElab #-}
getIfcInoutReset i v@(IStateVar { isv_vmi = vmi }) =
    case mreset_name of
        Nothing -> noReset
        Just n  -> getNamedReset n v
  -- XXX unQualId VModInfo
  where mreset_names =
            [ r | Inout { vf_name = n, vf_reset = r } <- vFields vmi,
                  n == unQualId i]
        mreset_name =
            case mreset_names of
                [n] -> n
                _   -> internalError ("ISyntaxUtil.getIfcInoutReset: " ++
                                      (ppReadable vmi) ++ (ppReadable i))

-- At the evaluated phases only: the gate of an output clock is selected
-- from the instance (ICStateVar), which only those phases have, and the
-- signature says so directly so that the icSelClockGate call below is
-- made at this function's own phase (and its specialisation), not at the
-- one the ICStateVar match refines it to.
getClockGate :: KnownPhase (EvaldPhase b) => IClock (EvaldPhase b) -> IExpr (EvaldPhase b)
{-# SPECIALISE getClockGate :: IClock Elab -> IExpr Elab #-}
{-# SPECIALISE getClockGate :: IClock PostElab -> IExpr PostElab #-}
getClockGate c =
   case (getClockWires c) of
     IAps (ICon i (ICTuple {fieldIds = [i_osc, i_gate]})) [] [osc, gate] |
        i == idClock && i_osc == idClockOsc && i_gate == idClockGate -> gate
     IAps (ICon i (ICSel { ictSel = itClock })) _ [(ICon vid (ICStateVar {iVar = sv}))] ->
        case (lookupOutputClockWires i (getVModInfo sv)) of
          (_, Nothing) -> iTrue
          (_, Just _)  -> icSelClockGate i c
     _ -> internalError "ISyntaxUtil.getClockGate"

-- Print a user-readable string for a clock expression
-- (displays the source-level name, using the interface name not the
-- Verilog port)
-- XXX This could be achieved by defining PVPrint on IExpr and adding
-- XXX a case-arm for oscillator/gate selection applied to a clock,
-- XXX to print just that field of the clock.  Then you could just call
-- XXX pvPrint on "getClockOsc" (which could be defined as above).
getClockOscString :: KnownPhase a => IClock a -> String
{-# SPECIALISE getClockOscString :: IClock PreElab -> String #-}
{-# SPECIALISE getClockOscString :: IClock Elab -> String #-}
{-# SPECIALISE getClockOscString :: IClock PostElab -> String #-}
getClockOscString clk =
   let
       handleExpr :: KnownPhase a => IExpr a -> String
       handleExpr (IAps (ICon m (ICSel { })) _
                        [(ICon i (ICClock { iClock = c }))]) = handleClk c
       handleExpr (ICon v (ICModPort { })) =
           -- This does not display the user-level name for a port.
           -- We currently expect the caller to handle that.
           getIdString v
       handleExpr (IAps (ICon m (ICSel { })) _
                        (ICon vid (ICStateVar { }) : es )) =
           getIdString vid ++ "." ++ getIdString m
       handleExpr e = internalError ("getClockOscString: unexpected expr: " ++
                                     ppReadable e)

       handleClk :: KnownPhase a => IClock a -> String
       handleClk c =
           case (getClockWires c) of
               IAps (ICon i (ICTuple {fieldIds = [i_osc, i_gate]})) []
                    [osc, gate] | i == idClock &&
                                  i_osc == idClockOsc && i_gate == idClockGate
                 -> handleExpr osc
               IAps (ICon i (ICSel { ictSel = itClock })) _
                    [(ICon vid (ICStateVar {iVar = sv}))]
                 -> -- display the BSV name, not the Verilog port
                    getIdString vid ++ "." ++ getIdString i
                    --let port = fst $
                    --           lookupOutputClockPorts i (getVModInfo sv)
                    --in  getIdString (mkOutputWireId vid port)
               e -> internalError ("getClockOscString: " ++ ppReadable e)
   in
       handleClk clk

getResetString :: KnownPhase a => IReset a -> String
{-# SPECIALISE getResetString :: IReset PreElab -> String #-}
{-# SPECIALISE getResetString :: IReset Elab -> String #-}
{-# SPECIALISE getResetString :: IReset PostElab -> String #-}
getResetString rst =
   let
       handleExpr :: KnownPhase a => IExpr a -> String
       handleExpr (ICon _ (ICReset { iReset = r })) = handleRst r
       handleExpr (ICon v (ICModPort { })) =
           -- This does not display the user-level name for a port.
           -- We currently expect the caller to handle that.
           getIdString v
       handleExpr (IAps (ICon m (ICSel { })) _
                        [ICon vid (ICStateVar { })]) =
           getIdString vid ++ "." ++ getIdString m
       handleExpr e = internalError ("getResetString: unexpected expr: " ++
                                     ppReadable e)

       handleRst :: KnownPhase a => IReset a -> String
       handleRst r = handleExpr (getResetWire r)
   in
       handleRst rst


-- Utils
iAps :: KnownPhase a => IExpr a -> [IType] -> [IExpr a] -> IExpr a
{-# SPECIALISE iAps :: IExpr PreElab -> [IType] -> [IExpr PreElab] -> IExpr PreElab #-}
{-# SPECIALISE iAps :: IExpr Elab -> [IType] -> [IExpr Elab] -> IExpr Elab #-}
{-# SPECIALISE iAps :: IExpr PostElab -> [IType] -> [IExpr PostElab] -> IExpr PostElab #-}
iAps e [] [] = e
iAps (IAps e ts es) [] es' = IAps e ts (es ++ es')
iAps e ts es = IAps e ts es

iAPs :: KnownPhase a => IExpr a -> [IType] -> IExpr a
{-# SPECIALISE iAPs :: IExpr PreElab -> [IType] -> IExpr PreElab #-}
{-# SPECIALISE iAPs :: IExpr Elab -> [IType] -> IExpr Elab #-}
{-# SPECIALISE iAPs :: IExpr PostElab -> [IType] -> IExpr PostElab #-}
iAPs e ts = iAps e ts []

iLet :: KnownPhase (BinderPhase e) => Id -> IType -> IExpr (BinderPhase e) -> IExpr (BinderPhase e) -> IExpr (BinderPhase e)
{-# SPECIALISE iLet :: Id -> IType -> IExpr PreElab -> IExpr PreElab -> IExpr PreElab #-}
{-# SPECIALISE iLet :: Id -> IType -> IExpr Elab -> IExpr Elab -> IExpr Elab #-}
iLet i _ e (IVar i') | i == i' && not (isKeepId i) && not (isKeepId i') = e
iLet i t e e' = iAp (ILam i t e') e

iePrimEQ, iePrimULE :: KnownPhase a => IType -> IExpr a -> IExpr a -> IExpr a
{-# SPECIALISE iePrimEQ :: IType -> IExpr PreElab -> IExpr PreElab -> IExpr PreElab #-}
{-# SPECIALISE iePrimULE :: IType -> IExpr PreElab -> IExpr PreElab -> IExpr PreElab #-}
{-# SPECIALISE iePrimEQ :: IType -> IExpr Elab -> IExpr Elab -> IExpr Elab #-}
{-# SPECIALISE iePrimULE :: IType -> IExpr Elab -> IExpr Elab -> IExpr Elab #-}
{-# SPECIALISE iePrimEQ :: IType -> IExpr PostElab -> IExpr PostElab -> IExpr PostElab #-}
{-# SPECIALISE iePrimULE :: IType -> IExpr PostElab -> IExpr PostElab -> IExpr PostElab #-}
iePrimEQ t e1 e2 = IAps icPrimEQ [t] [e1, e2]
iePrimULE t e1 e2 = IAps icPrimULE [t] [e1, e2]

-- Misc
idIntLit, idRealLit, idPositionLit, idPredLit, idStringLit, idCharLit, idHandleLit :: Id
idIntLit       = dummyId noPosition
idRealLit      = dummyId noPosition
idPositionLit  = dummyId noPosition
idPredLit      = dummyId noPosition
idStringLit    = dummyId noPosition
idCharLit      = dummyId noPosition
idHandleLit    = dummyId noPosition

itInst :: IType -> [IType] -> IType
itInst (ITForAll i _ t) (a:as) = itInst (tSubst i a t) as
itInst t                []     = t
itInst t                as     = internalError ("itInst: " ++ ppReadable (t, as))

-- Instantiate and normalize (expand type synonyms and type functions)
itInstNorm :: (IType -> IType) -> IType -> [IType] -> IType
itInstNorm norm t ts = norm (itInst t ts)

leftmost :: IType -> IType
leftmost (ITAp t _) = leftmost t
leftmost t = t

dropForAll :: IType -> IType
dropForAll (ITForAll _ _ t) = dropForAll t
dropForAll t = t

dropArrows :: Int -> IType -> IType
dropArrows 0 t = t
dropArrows n (ITAp (ITAp arr _) r) | arr == itArrow = dropArrows (n-1) r
dropArrows n t = internalError ("dropArrows: " ++ ppReadable (n, t))

takeArgTypes :: Int -> IType -> [IType]
takeArgTypes 0 _ = []
takeArgTypes n (ITAp (ITAp arr a) r) | arr == itArrow = a : takeArgTypes (n-1) r
takeArgTypes n t = internalError ("takeArgTypes: " ++ ppReadable (n, t))

itGetArrows :: IType -> ([IType], IType)
itGetArrows it = itGetArrows' [] it
  where itGetArrows' ts (ITAp (ITAp arr a) r) | arr == itArrow = itGetArrows' (a:ts) r
        itGetArrows' ts r = (reverse ts, r)

-- Is this the type of a (fully applied) typeclass dictionary?
itIsDictType :: IType -> Bool
itIsDictType t
  | null $ fst $ itGetArrows t,
    ITCon _ _ (TIstruct SClass _) <- leftmost t = True
itIsDictType _ = False

-- Flatten a (possibly nested) right-associated PrimPair tuple type into its
-- element types in left-to-right order.  PrimUnit contributes no elements,
-- PrimPair recurses into both sides, anything else is a single element.
itTupleElems :: IType -> [IType]
itTupleElems t
  | t == itPrimUnit = []
  | otherwise = case t of
                  ITAp (ITAp (ITCon ip _ _) t1) t2 | ip == idPrimPair ->
                      itTupleElems t1 ++ itTupleElems t2
                  _ -> [t]

-- The element bit-widths of a (bitified) tuple type, in left-to-right port order
-- (flattening nested PrimPair tuples via itTupleElems).  A non-Bit leaf yields
-- 0, so callers that want only the actual ports can filter out the zeros.
bitTupleSizes :: IType -> [Integer]
bitTupleSizes = map leafSize . itTupleElems
  where leafSize (ITAp b (ITNum n)) | b == itBit = n
        leafSize _ = 0

-- #############################################################################
-- #
-- #############################################################################

-- Apply an ISyntax substitution to the predicate and action of a set of rules
irulesMap :: (IExpr a -> IExpr a) -> IRules a -> IRules a
irulesMap f (IRules sps rs) = IRules sps (map (iruleMap f) rs)
  where iruleMap f r = r { irule_pred = f (irule_pred r),
                           irule_body = f (irule_body r) }

irulesMapM :: (Monad m) => (IExpr a -> m (IExpr a)) -> IRules a -> m (IRules a)
irulesMapM f (IRules sps rs) = do
  let iruleMapM f r = do
        p' <- f $ irule_pred r
        a' <- f $ irule_body r
        return r { irule_pred = p' , irule_body = a' }
  rs' <- mapM (iruleMapM f) rs
  return (IRules sps rs')

-------------------

-- Similar to the routine in ISyntaxCheck, but with shortcuts to make
-- it faster.

iGetTypeNorm :: forall a . KnownPhase a => (IType -> Changed IType) -> IExpr a -> IType
{-# SPECIALISE iGetTypeNorm :: (IType -> Changed IType) -> IExpr PreElab -> IType #-}
{-# SPECIALISE iGetTypeNorm :: (IType -> Changed IType) -> IExpr Elab -> IType #-}
{-# SPECIALISE iGetTypeNorm :: (IType -> Changed IType) -> IExpr PostElab -> IType #-}
iGetTypeNorm norm e0 =
    let iGetTypePrim _ PrimIf [t] [_,_,_] = t
        iGetTypePrim _ PrimConcat [_,_,ITNum n] [_,_] = itBitN n
        iGetTypePrim _ PrimMul [_,_,ITNum n] [_,_] = itBitN n
        iGetTypePrim _ PrimQuot [_,_,ITNum n] [_,_] = itBitN n
        iGetTypePrim _ PrimRem [_,_,ITNum n] [_,_] = itBitN n
        iGetTypePrim _ PrimSelect [ITNum n,_,_] [_] = itBitN n
        iGetTypePrim _ PrimJoinActions [] [_,_] = itAction
        iGetTypePrim _ p          _  [_,_] | isBoolRes p = itBit1
          where isBoolRes PrimEQ  = True
                isBoolRes PrimULE  = True
                isBoolRes PrimULT  = True
                isBoolRes PrimSLE  = True
                isBoolRes PrimSLT  = True
                isBoolRes PrimBAnd = True
                isBoolRes PrimBOr  = True
                isBoolRes PrimBNot = True
                isBoolRes _        = False
        iGetTypePrim _ p       [ITNum n]  [_,_] | isNRes p = itBitN n
          where isNRes PrimAdd = True
                isNRes PrimSub = True
                isNRes PrimAnd = True
                isNRes PrimOr  = True
                isNRes PrimXor = True
                isNRes PrimSL  = True
                isNRes PrimSRL = True
                isNRes PrimSRA = True
                isNRes _       = False
        iGetTypePrim e _ _ _ = changedOrId norm $ tCheck emptyEnv e

        -- (GADT matches under MonoLocalBinds need the signature; the phase
        -- is the caller's, so the per-phase specialisations cover tCheck)
        tCheck :: M.Map Id IType -> IExpr a -> IType
        tCheck r (ILam i t e) =
                itFun t (tCheck (addT i t r) e)
        tCheck r (IAps f [] []) = tCheck r f
        tCheck r (IAps e [t] []) =
                case tCheck r e of
                ITForAll i _ rt -> tSubst i t rt
                tt -> internalError ("iGetType.tCheck: " ++ ppString (e0, e, tt, t))
        tCheck r (IAps f (t:ts) []) = tCheck r (IAps (IAps f [t] []) ts [])
        tCheck r (IAps f ts es) =
          dropArrows (length es) (changedOrId norm $ tCheck r (IAps f ts []))
        tCheck r (IVar i) = findT i r
        tCheck r (ILAM i k e) = ITForAll i k (tCheck r e)
        tCheck r (ICon c ic) = iConType ic
        tCheck r (IRefT t _ _ _) = t
--        tCheck _ e = internalError ("no match in ISyntaxUtil.tCheck: " ++ ppReadable e)

        emptyEnv = M.empty

        addT i t tm = M.insert i t tm

        findT i tm =
                case M.lookup i tm of
                Just t -> t
                Nothing -> internalError ("ISyntaxUtil.findT " ++ ppString i ++ "\n" ++ ppReadable (M.toList tm))

    in  case e0 of
        -- First some fast special cases:
        (ICon c ic) -> iConType ic
        e@(IAps (ICon _ (ICPrim _ p)) ts es) -> iGetTypePrim e p ts es
        -- General
        e -> changedOrId norm $ tCheck emptyEnv e

iGetType :: KnownPhase a => IExpr a -> IType
{-# SPECIALISE iGetType :: IExpr PreElab -> IType #-}
{-# SPECIALISE iGetType :: IExpr Elab -> IType #-}
{-# SPECIALISE iGetType :: IExpr PostElab -> IType #-}
iGetType = iGetTypeNorm $ \ _ -> Unchanged

-- input must be an interface type
iGetIfcName :: IType -> Id
iGetIfcName t = fromMaybe err (iMGetIfcName t)
  where err = internalError ("ISyntaxUtil.iGetIfcName: " ++ ppReadable t)

iMGetIfcName :: IType -> Maybe Id
iMGetIfcName (ITCon i _ (TIstruct (SInterface _) _)) = Just i
iMGetIfcName (ITAp t _) = iMGetIfcName t
iMGetIfcName t = Nothing

-- get the interface type from a Module def type
iGetModIfcType :: IType -> IType
iGetModIfcType t =
    let (_, modTy) = itGetArrows t
    in  case modTy of
          ITAp _ ifcTy -> ifcTy
          _ -> internalError ("iGetModIfcType: " ++ ppReadable modTy)

getITypeSort :: IType -> Maybe TISort
getITypeSort (ITCon i _ tisort) = Just tisort
getITypeSort (ITAp t _) = getITypeSort t
getITypeSort _ = Nothing

isIfcType :: IType -> Bool
isIfcType t = case (getITypeSort t) of
                Just (TIstruct (SInterface _) _) -> True
                _ -> False

isPolyWrapType :: IType -> Bool
isPolyWrapType t = case (getITypeSort t) of
                Just (TIstruct (SPolyWrap _ _ _) _) -> True
                _ -> False

isAbstractType :: IType -> Bool
isAbstractType t = getITypeSort t == Just TIabstract


-- utility method to check if an expression is an if or not
notIf :: KnownPhase a => IExpr a -> Bool
{-# SPECIALISE notIf :: IExpr PreElab -> Bool #-}
{-# SPECIALISE notIf :: IExpr Elab -> Bool #-}
{-# SPECIALISE notIf :: IExpr PostElab -> Bool #-}
notIf (IAps (ICon _ (ICPrim { primOp = PrimIf })) _ _) = False
notIf _ = True

-- note that ISplitIf.push assumes that PrimIf is FALSE for this function
isIfWrapper :: PrimOp -> Bool
isIfWrapper PrimExpIf = True
isIfWrapper PrimNoExpIf = True
isIfWrapper PrimNosplitDeep = True
isIfWrapper PrimSplitDeep = True
isIfWrapper _ = False

-- utilities for flattening and joining up action lists
-- flatAction: flattens (IAps joinactions a1 (IAps joinactions a2 a3 ...) into [a1, a2, a3 ...]

flattensToNothing :: KnownPhase a => IExpr a -> Bool
{-# SPECIALISE flattensToNothing :: IExpr PreElab -> Bool #-}
{-# SPECIALISE flattensToNothing :: IExpr Elab -> Bool #-}
{-# SPECIALISE flattensToNothing :: IExpr PostElab -> Bool #-}
flattensToNothing (ICon _ (ICPrim { primOp = PrimNoActions })) = True
flattensToNothing (ICon i (ICUndet { })) = True
flattensToNothing (IAps (ICon i (ICUndet { })) _ _) = True
flattensToNothing _ = False

flatAction :: KnownPhase a => IExpr a -> [IExpr a]
{-# SPECIALISE flatAction :: IExpr PreElab -> [IExpr PreElab] #-}
{-# SPECIALISE flatAction :: IExpr Elab -> [IExpr Elab] #-}
{-# SPECIALISE flatAction :: IExpr PostElab -> [IExpr PostElab] #-}
flatAction x | flattensToNothing x = []
flatAction (IAps (ICon _ (ICPrim { primOp = PrimJoinActions })) _ [a1, a2]) = flatAction a1 ++ flatAction a2
--flatAction (IAps f ts es) = [IAps f ts (map (joinActions . flatAction) es)]
flatAction a = [a]

joinActions :: KnownPhase a => [IExpr a] -> IExpr a
{-# SPECIALISE joinActions :: [IExpr PreElab] -> IExpr PreElab #-}
{-# SPECIALISE joinActions :: [IExpr Elab] -> IExpr Elab #-}
{-# SPECIALISE joinActions :: [IExpr PostElab] -> IExpr PostElab #-}
joinActions [] = icNoActions
joinActions as = foldr1 ja as
  where ja a1 a2 = IAps icJoinActions [] [a1, a2]

iStrToInt :: KnownPhase a => String -> Position -> IExpr a
{-# SPECIALISE iStrToInt :: String -> Position -> IExpr PreElab #-}
{-# SPECIALISE iStrToInt :: String -> Position -> IExpr Elab #-}
{-# SPECIALISE iStrToInt :: String -> Position -> IExpr PostElab #-}
iStrToInt s pos = iMkLitAt pos itInteger i
  where i = foldl sumString 0 s
        sumString :: Integer -> Char -> Integer
        sumString t c = 256 * t + c'
          where c' = toInteger (fromEnum c)

iuDontCareExpr :: KnownPhase a => IExpr a
{-# SPECIALISE iuDontCareExpr :: IExpr PreElab #-}
{-# SPECIALISE iuDontCareExpr :: IExpr Elab #-}
{-# SPECIALISE iuDontCareExpr :: IExpr PostElab #-}
iuDontCareExpr = iMkLit itInteger uDontCareInteger

iuNoMatchExpr :: KnownPhase a => IExpr a
{-# SPECIALISE iuNoMatchExpr :: IExpr PreElab #-}
{-# SPECIALISE iuNoMatchExpr :: IExpr Elab #-}
{-# SPECIALISE iuNoMatchExpr :: IExpr PostElab #-}
iuNoMatchExpr = iMkLit itInteger uNoMatchInteger

iuKindToExpr :: KnownPhase a => UndefKind -> IExpr a
{-# SPECIALISE iuKindToExpr :: UndefKind -> IExpr PreElab #-}
{-# SPECIALISE iuKindToExpr :: UndefKind -> IExpr Elab #-}
{-# SPECIALISE iuKindToExpr :: UndefKind -> IExpr PostElab #-}
iuKindToExpr = (iMkLit itInteger) . undefKindToInteger

getStateVarNames :: KnownPhase a => IExpr a -> [Id]
{-# SPECIALISE getStateVarNames :: IExpr PreElab -> [Id] #-}
{-# SPECIALISE getStateVarNames :: IExpr Elab -> [Id] #-}
{-# SPECIALISE getStateVarNames :: IExpr PostElab -> [Id] #-}
getStateVarNames (ILam _ _ e) = getStateVarNames e
getStateVarNames (ILAM _ _ e) = getStateVarNames e
getStateVarNames (ICon i (ICStateVar {})) = [i]
getStateVarNames (IAps f _ es) = concatMap getStateVarNames (f:es)
getStateVarNames _ = []

iTLog, iTAdd, iTMax, iTMin, iTMul, iTDiv :: IType
iTLog = ITCon idTLog (IKNum `IKFun` IKNum) TIabstract
iTAdd = ITCon idTAdd (IKNum `IKFun` (IKNum `IKFun` IKNum)) TIabstract
iTMax = ITCon idTMax (IKNum `IKFun` (IKNum `IKFun` IKNum)) TIabstract
iTMin = ITCon idTMin (IKNum `IKFun` (IKNum `IKFun` IKNum)) TIabstract
iTMul = ITCon idTMul (IKNum `IKFun` (IKNum `IKFun` IKNum)) TIabstract
iTDiv = ITCon idTDiv (IKNum `IKFun` (IKNum `IKFun` IKNum)) TIabstract

iDefMap :: (IExpr a -> IExpr a) -> IDef a -> IDef a
iDefMap f (IDef i t e p) = IDef i t (f e) p

iDefsMap :: (Functor f) => (IExpr a -> IExpr a) -> f (IDef a) -> f (IDef a)
iDefsMap f defs = fmap (iDefMap f) defs

iDefMapM :: (Monad m) => (IExpr a -> m (IExpr a)) -> IDef a -> m (IDef a)
iDefMapM f (IDef i t e p) = do
  e' <- f e
  return $ IDef i t e' p

emptyFmt :: KnownPhase a => (IExpr a)
{-# SPECIALISE emptyFmt :: (IExpr PreElab) #-}
{-# SPECIALISE emptyFmt :: (IExpr Elab) #-}
{-# SPECIALISE emptyFmt :: (IExpr PostElab) #-}
emptyFmt = (IAps (ICon idFormat (ICForeign {fName    = getIdString(unQualId(idFormat)),
                                            foports  = Nothing,
                                            fTyVarNames = [],
                                            fcallNo  = (Just 0),
                                            ictForeign = tt,
                                            isC = False -- unsure what this should be?
                                            })) [] [e])
   where e = iMkString ""
         t = iGetType e
         tt = (t `itFun` itFmt)
