-- Type links (prototype): the field types of a data or struct type, read
-- from its declaration in the symbol table, for the F records of the .tl
-- file (TypeLinks); the tag encodings of a data type's constructors, read
-- from the same constructor entries, for the E records; and whether a
-- type has an instance of Bits, read from the class's instance list, for
-- the B records; and whether the module's pragmas give a method no
-- enable port or no ready port, for the H records.  Used only after
-- elaboration.
--
-- Nothing here makes an identifier or a string: a constructor's or
-- field's scheme is instantiated at the type's own arguments by replacing
-- its generic variables, and converted as IConv converts any type; an
-- instance's head is only matched; a pragma's method names are compared
-- as text.
module TypeLinksDecl(tlFieldTypes, tlConEncodings, tlBitsInstance,
                     tlAlwaysMethod) where

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
import SymTab(SymTab, ConInfo(..), FieldInfo(..), findCon, findField, findSClass)
import Pragma(PProp(..))
import ConTagInfo(ConTagInfo(..))
import IConv(iConvT)
import IType(IType(ITCon, ITNum, ITStr, ITAp), iToCK)
import ISyntax(splitITAp)

-- The constructor payloads (data: constructor number, 0, the
-- constructor's name, the payload's type) or the fields (struct: 0, field
-- number, the field's name, its type) of the type t, each type at t's own
-- arguments and normalized by norm.  A type is Nothing when the
-- declaration's type could not be instantiated (a field with its own type
-- variables).  The result is Nothing when t is not a data or struct type
-- applied to closed arguments, or a constructor or field of it is not in
-- the symbol table.
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
    -- structs, constructor payloads and pairs (whose sort is an interface
    -- with no pragmas); not classes, real interfaces or wrapped
    -- polymorphic fields
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
    -- a constructor's type is payload -> T g1 .. gn
    payload cargs (_ :>: Forall _ (_ :=> ty)) =
        case arrow ty of
          Just (a, r) -> do m <- generics r cargs
                            conv <$> instGen m a
          Nothing -> Nothing

    fieldRow ti cargs (k, f) =
        case entries (findField symt) fi_id ti f of
          [] -> Nothing
          fi : _ -> Just (0, k, getIdBaseString f, fieldType cargs (fi_assump fi))
    -- a field's type is T g1 .. gn -> field
    fieldType cargs (_ :>: Forall _ (_ :=> ty)) =
        case arrow ty of
          Just (s, r) -> do m <- generics s cargs
                            conv <$> instGen m r
          Nothing -> Nothing

    arrow (TAp (TAp (TCon (TyCon i _ _)) a) r) | i == idArrow noPosition = Just (a, r)
    arrow _ = Nothing

    -- the generic variables of T g1 .. gn, each with t's argument
    generics :: CType -> [CType] -> Maybe (IM.IntMap CType)
    generics applied cargs =
        let (_, gs) = splitTAp applied
        in  if length gs /= length cargs then Nothing
            else IM.fromList <$> mapM gen (zip gs cargs)
    gen (TGen _ n, ct) = Just (n, ct)
    gen _ = Nothing

-- the entries of the type ti under the name i, also looked up with ti's
-- package as the qualifier
entries :: (Id -> Maybe [a]) -> (a -> Id) -> Id -> Id -> [a]
entries find owner ti i =
    let pick xs = [ x | x <- xs, qualEq ti (owner x) ]
    in  case maybe [] pick (find i) of
          [] -> maybe [] pick (find (setIdQual i (getIdQual ti)))
          xs -> xs

-- The tag encodings of the constructors of the data type t, in the order
-- of its declaration: for each constructor its number, its tag value when
-- packed and the number of bits of the tag.  These are the symbol
-- table's own numbers, the ones the derived Bits instance, a constructor
-- with no argument and a constructor test are compiled with; they belong
-- to the type constructor and do not depend on t's arguments.  Nothing
-- when t is not a data type or a constructor of it is not in the symbol
-- table.
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

-- a type with its generic variables replaced; Nothing when one has no
-- replacement or the type has a variable
instGen :: IM.IntMap CType -> CType -> Maybe CType
instGen m (TGen _ n) = IM.lookup n m
instGen m (TAp f a) = do f' <- instGen m f
                         a' <- instGen m a
                         Just (TAp f' a')
instGen _ t@(TCon _) = Just t
instGen _ _ = Nothing

-- a closed IType as a CType
closedCT :: IType -> Maybe CType
closedCT (ITAp f a) = do f' <- closedCT f
                         a' <- closedCT a
                         Just (TAp f' a')
closedCT (ITCon i k s) = Just (TCon (TyCon i (Just (iToCK k)) s))
closedCT (ITNum n) = Just (TCon (TyNum n noPosition))
closedCT (ITStr s) = Just (TCon (TyStr s noPosition))
closedCT _ = Nothing

-- Whether the symbol table has an instance of the class Bits for the type
-- constructor of t: Just True when the head of an instance is that
-- constructor applied to anything (the instance's context is not looked
-- at); Just False when no instance's head is, and no instance's head is a
-- type variable (an instance for every type); otherwise Nothing, not
-- known, as when t has no constructor or the class is not found.  The
-- set of heads is built once for the symbol table, at the first use.
tlBitsInstance :: SymTab -> IType -> Maybe Bool
tlBitsInstance symt =
    case findSClass symt (CTypeclass idBits) of
      Nothing -> const Nothing
      Just cls ->
          let heads = [ fst (splitTAp t) | Inst _ _ (_ :=> IsIn _ (t : _)) _ <- getInsts cls ]
              cons = S.fromList [ i | TCon (TyCon i _ _) <- heads ]
              open = or [ True | h <- heads, isVar h ]
              isVar (TVar _) = True
              isVar (TGen _ _) = True
              isVar _ = False
          in  \ t -> case fst (splitITAp t) of
                       ITCon i _ _ | i `S.member` cons -> Just True
                                   | open -> Nothing
                                   | otherwise -> Just False
                       _ -> Nothing

-- Whether the module's pragmas make the method m always enabled (it has
-- no enable port) and always ready (it has no ready port; always enabled
-- implies it).  This is what Pragma.isAlwaysEn and Pragma.isAlwaysRdy
-- decide, but those build an identifier for each name a pragma lists;
-- here the names are compared as text: a listed path is the method when
-- its parts joined by "_" (a part with no name left out) are the method's
-- name and its first part's qualifier is the method's.  A pragma that
-- lists no name is for every method.
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
