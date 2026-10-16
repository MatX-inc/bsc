-- The structural interface carried between the generic wrapping library and
-- the compiler.  This is a view of ordinary types, not another serialized IR.
module StructuralWrap
  ( WrapShape(..), wrapShapeType, emptyWrapShape,
    cWrapShape, iWrapShape, wrapLeaves ) where

import Data.List(intercalate)
import CType
import FStringCompat(getFString, mkFString)
import Id
import ISyntax(IType(..), splitITAp)
import Position(Position)
import PreIds

-- A field's terminal payload is (wrapped value, ready bit).  Products and
-- vectors retain the source interface hierarchy; MetaField supplies each
-- component of the flattened method name.  MetaInterface marks the boundary
-- so an ordinary tuple returned by user code is never mistaken for one.
data WrapShape t
  = WrapUnit t
  | WrapPair t (WrapShape t) (WrapShape t)
  | WrapMeta t t (Maybe String) (WrapShape t)
  | WrapVector t Integer (WrapShape t)
  | WrapLeaf t t

wrapShapeType :: WrapShape t -> t
wrapShapeType (WrapUnit t) = t
wrapShapeType (WrapPair t _ _) = t
wrapShapeType (WrapMeta t _ _ _) = t
wrapShapeType (WrapVector t _ _) = t
wrapShapeType (WrapLeaf t _) = t

emptyWrapShape :: WrapShape t -> Bool
emptyWrapShape (WrapUnit _) = True
emptyWrapShape (WrapPair _ a b) = emptyWrapShape a && emptyWrapShape b
emptyWrapShape (WrapMeta _ _ _ a) = emptyWrapShape a
emptyWrapShape (WrapVector _ n a) = n == 0 || emptyWrapShape a
emptyWrapShape (WrapLeaf _ _) = False

cWrapShape :: CType -> Maybe (WrapShape CType)
cWrapShape = wrapShape split str num
  where
    split t = case splitTAp t of
      (TCon (TyCon i _ _), as) -> Just (i, as)
      _ -> Nothing
    str (TCon (TyStr s _)) = Just (getFString s)
    str _ = Nothing
    num (TCon (TyNum n _)) = Just n
    num _ = Nothing

iWrapShape :: IType -> Maybe (WrapShape IType)
iWrapShape = wrapShape split str num
  where
    split t = case splitITAp t of
      (ITCon i _ _, as) -> Just (i, as)
      _ -> Nothing
    str (ITStr s) = Just (getFString s)
    str _ = Nothing
    num (ITNum n) = Just n
    num _ = Nothing

wrapShape :: (t -> Maybe (Id, [t])) -> (t -> Maybe String) ->
             (t -> Maybe Integer) -> t -> Maybe (WrapShape t)
wrapShape split str num = root
  where
    root t = do
      (i, [meta, body]) <- split t
      (mi, [_, _, _]) <- split meta
      if i == idMeta && mi == idMetaInterface
        then WrapMeta t meta Nothing <$> node body
        else Nothing
    node t = do
      (i, as) <- split t
      case as of
        [] | i == idPrimUnit -> Just (WrapUnit t)
        [meta, body] | i == idMeta -> do
          (mi, mas) <- split meta
          case mas of
            [name, _] | mi == idMetaField -> do
              n <- str name
              WrapMeta t meta (Just n) <$> node body
            [_, _, _] | mi == idMetaInterface -> root t
            _ -> Nothing
        [a, b] | i == idPrimPair ->
          if isReady b then Just (WrapLeaf t a)
          else WrapPair t <$> node a <*> node b
        [n, a] | i == idVector -> do
          count <- num n
          if count < 0 then Nothing else WrapVector t count <$> node a
        _ -> Nothing
    isReady t = case split t of
      Just (i, [n]) | i == idBit -> num n == Just 1
      _ -> False

-- Each entry is a real boundary field; readiness is a separate field only
-- for methods and is added by the caller, which knows the field's type.
wrapLeaves :: Position -> WrapShape t -> [(Id, t)]
wrapLeaves pos = walk []
  where
    walk _ (WrapUnit _) = []
    walk path (WrapPair _ a b) = walk path a ++ walk path b
    walk path (WrapMeta _ _ Nothing s) = walk path s
    walk path (WrapMeta _ _ (Just n) s) = walk (path ++ [n]) s
    walk path (WrapVector _ n s) =
      concat [walk (path ++ [show k]) s | k <- [0 .. n-1]]
    walk path (WrapLeaf _ t) =
      [(mkId pos (mkFString (intercalate "_" path)), t)]
