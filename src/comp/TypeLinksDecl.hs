-- Post-elaboration symbol-table queries for TypeLinks and SourceProvenance:
-- struct/data field and payload types (F records), constructor tag encodings
-- (E records), Bits instance membership (B records and literal eligibility),
-- always-ready/always-enabled method pragmas (H records), and declared module
-- parameter types (P records).
--
-- Nothing here interns identifiers or depends on Ord Id: generic variables in
-- constructor/field schemes are instantiated at the concrete type arguments,
-- Bits instance heads are indexed by (qual, base) text, and pragma method
-- paths are compared as text.
module TypeLinksDecl(tlFieldTypes, tlConEncodings, tlBitsInstance,
                     tlAlwaysMethod, tlParamTypes) where

import qualified Data.IntMap as IM
import qualified Data.Set as S

import Flags(Flags)
import Position(noPosition)
import Id(Id, getIdBaseString, getIdQualString, getIdQual, setIdQual, qualEq)
import PreIds(idArrow, idBits)
import CType(Type(..), TyCon(..), CType, TISort(..), StructSubType(..), CTypeclass(..),
             splitTAp)
import Scheme(Scheme(..))
import Pred(Qual(..), Pred(..), Class(..), Inst(..))
import Assump(Assump(..))
import SymTab(SymTab, ConInfo(..), FieldInfo(..), VarInfo(..), findCon, findField,
              findSClass, findVar)
import Pragma(PProp(..))
import VModInfo(VArgInfo(..), getVNameString)
import ConTagInfo(ConTagInfo(..))
import IConv(iConvT)
import IType(IType(ITCon, ITNum, ITStr, ITAp), iToCK)
import ISyntax(splitITAp)

-- Constructor payloads (data: conNo, 0, conName, Maybe payloadType) or struct
-- fields (struct: 0, fieldIdx, fieldName, Maybe fieldType) of t, instantiated
-- at t's concrete type arguments and normalized by norm.
tlFieldTypes :: Flags -> SymTab -> (IType -> IType) -> IType ->
                Maybe [(Int, Int, String, Maybe IType)]
tlFieldTypes flags symt norm t =
    case splitITAp t of
      (ITCon ti _ sort, args) -> do
          cargs <- mapM closedCT args
          case sort of
            TIdata { tidata_cons = cs } -> concat <$> mapM (conRows ti cargs) cs
            TIstruct ss fs | plain ss -> mapM (fieldRow ti cargs) (zip [0 ..] fs)
            _ -> Nothing
      _ -> Nothing
  where
    plain SStruct = True
    plain (SDataCon {}) = True
    plain (SInterface _) = True
    plain _ = False

    conv ct = norm (iConvT flags symt ct)

    conRows ti cargs c =
        case entries (findCon symt) ci_id ti c of
          [] -> Nothing
          cis -> Just [ (fromInteger (conNo (ci_taginfo ci)), 0, getIdBaseString c,
                         payload cargs (ci_assump ci))
                      | ci <- take 1 cis ]
    payload cargs (_ :>: Forall _ (_ :=> ty)) =
        case arrow ty of
          Just (a, r) -> do m <- generics r cargs
                            conv <$> instGen m a
          Nothing -> Nothing

    fieldRow ti cargs (k, f) =
        case entries (findField symt) fi_id ti f of
          [] -> Nothing
          fi : _ -> Just (0, k, getIdBaseString f, fieldType cargs (fi_assump fi))
    fieldType cargs (_ :>: Forall _ (_ :=> ty)) =
        case arrow ty of
          Just (s, r) -> do m <- generics s cargs
                            conv <$> instGen m r
          Nothing -> Nothing

    arrow (TAp (TAp (TCon (TyCon i _ _)) a) r) | i == idArrow noPosition = Just (a, r)
    arrow _ = Nothing

    generics :: CType -> [CType] -> Maybe (IM.IntMap CType)
    generics applied cargs =
        let (_, gs) = splitTAp applied
        in  if length gs /= length cargs then Nothing
            else IM.fromList <$> mapM gen (zip gs cargs)
    gen (TGen _ n, ct) = Just (n, ct)
    gen _ = Nothing

entries :: (Id -> Maybe [a]) -> (a -> Id) -> Id -> Id -> [a]
entries find owner ti i =
    let pick xs = [ x | x <- xs, qualEq ti (owner x) ]
    in  case maybe [] pick (find i) of
          [] -> maybe [] pick (find (setIdQual i (getIdQual ti)))
          xs -> xs

-- Constructor tag encodings (conNo, conTag, tagSize) in declaration order.
tlConEncodings :: SymTab -> IType -> Maybe [(Integer, Integer, Integer)]
tlConEncodings symt t =
    case fst (splitITAp t) of
      ITCon ti _ (TIdata { tidata_cons = cs }) -> mapM (enc ti) cs
      _ -> Nothing
  where
    enc ti c =
        case entries (findCon symt) ci_id ti c of
          [] -> Nothing
          ci : _ -> let cti = ci_taginfo ci
                    in  Just (conNo cti, conTag cti, tagSize cti)

instGen :: IM.IntMap CType -> CType -> Maybe CType
instGen m (TGen _ n) = IM.lookup n m
instGen m (TAp f a) = do f' <- instGen m f
                         a' <- instGen m a
                         Just (TAp f' a')
instGen _ t@(TCon _) = Just t
instGen _ _ = Nothing

closedCT :: IType -> Maybe CType
closedCT (ITAp f a) = do f' <- closedCT f
                         a' <- closedCT a
                         Just (TAp f' a')
closedCT (ITCon i k s) = Just (TCon (TyCon i (Just (iToCK k)) s))
closedCT (ITNum n) = Just (TCon (TyNum n noPosition))
closedCT (ITStr s) = Just (TCon (TyStr s noPosition))
closedCT _ = Nothing

-- Whether the symbol table has a Bits instance matching t's head constructor.
-- Keyed by (qual, base) text rather than Ord Id.
tlBitsInstance :: SymTab -> IType -> Maybe Bool
tlBitsInstance symt =
    case findSClass symt (CTypeclass idBits) of
      Nothing -> const Nothing
      Just cls ->
          let heads = [ fst (splitTAp t) | Inst _ _ (_ :=> IsIn _ (t : _)) _ <- getInsts cls ]
              cons = S.fromList [ (getIdQualString i, getIdBaseString i)
                                | TCon (TyCon i _ _) <- heads ]
              open = or [ True | h <- heads, isVar h ]
              isVar (TVar _) = True
              isVar (TGen _ _) = True
              isVar _ = False
          in  \ t -> case fst (splitITAp t) of
                       ITCon i _ _
                         | (getIdQualString i, getIdBaseString i) `S.member` cons -> Just True
                         | open -> Nothing
                         | otherwise -> Just False
                       _ -> Nothing

tlAlwaysMethod :: [PProp] -> Id -> (Bool, Bool)
tlAlwaysMethod pps =
    let enabled = [ is | PPalwaysEnabled is <- pps ]
        ready = [ is | PPalwaysReady is <- pps ] ++ enabled
        flat [] = ("", "")
        flat path@(i : _) =
            (getIdQualString i,
             foldl (\ a b -> if null a then b else if null b then a else a ++ "_" ++ b)
                   "" (map getIdBaseString path))
        hit m is = null is || (getIdQualString m, getIdBaseString m) `elem` map flat is
    in  \ m -> (any (hit m) enabled, any (hit m) ready)

tlParamTypes :: Flags -> SymTab -> (IType -> IType) -> Id -> Id -> [VArgInfo] ->
                [(String, IType)]
tlParamTypes flags symt norm modId elabId vargs =
    case lookups of
      VarInfo { vi_assump = _ :>: Forall _ (_ :=> ty) } : _ ->
          [ (getVNameString vn, norm (iConvT flags symt a))
          | (Param vn, a) <- zip vargs (args ty), closed a ]
      _ -> []
  where
    lookups = [ vi | i <- [modId, setIdQual modId (getIdQual elabId)],
                     Just vi <- [findVar symt i] ]
    args (TAp (TAp (TCon (TyCon i _ _)) a) r) | i == idArrow noPosition = a : args r
    args _ = []
    closed (TAp f a) = closed f && closed a
    closed (TCon _) = True
    closed _ = False
