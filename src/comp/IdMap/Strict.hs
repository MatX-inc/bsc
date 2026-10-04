{-# LANGUAGE CPP #-}
-- | IdMap with value-strict updates: the Data.Map.Strict to IdMap's
-- Data.Map. The newtype is IdMap's own, and every structural function
-- is IdMap's, re-exported; the functions that store a value are
-- redefined here over Data.Map.Strict, so that the value is forced to
-- WHNF before it is stored, exactly containers' split. A module that
-- imported Data.Map.Strict for an Id-keyed map imports this one, so
-- that no laziness changes under it; the knot-tied lazy maps elsewhere
-- stay on IdMap. The tiers (P/B/N/ByName) and the IDMAP_AUDIT hook are
-- IdMap's; see IdMap.hs.
--
-- findWithDefault is not redefined: Data.Map.Strict's is the lazy one.
module IdMap.Strict(
    module IdMap,
    -- * value-strict (P)
    singleton, insert, insertWith, insertWithKey, insertLookupWithKey,
    adjust, adjustWithKey, update, updateWithKey, alter, alterF,
    fromList, fromListWith, fromListWithKey, fromSet,
    map, mapWithKey, mapMaybe, mapMaybeWithKey, mapEither, mapEitherWithKey,
    unionWith, unionWithKey, unionsWith,
    intersectionWith, intersectionWithKey, differenceWith,
    insertMany, insertManyWith,
    insertWithM, insertManyWithM, insertWithKeyM, insertManyWithKeyM,
    -- * value-strict (B): intern order, no order promised
    mapAccum, mapAccumWithKey, mapAccumRWithKey, traverseWithKey,
    mapMWithKey, mapMValues, unionWithM, unionsWithM,
    -- * value-strict ByName
    traverseWithKeyByName, mapAccumByName
    ) where

#if MIN_VERSION_base(4,20,0)
import Prelude hiding (lookup, map, filter, foldr, foldl, foldl', null)
#else
import Prelude hiding (lookup, map, filter, foldr, foldl, null)
#endif
import qualified Prelude as P
import qualified Data.Map.Strict as MS
import qualified Data.Foldable as F
import qualified Data.List as L
import Control.Monad(foldM)

import Id(Id)
#ifdef IDMAP_AUDIT
import IdOrd(AuditKey)
#endif
import IdSet(IdSet)
import qualified IdSet
import IdMap hiding (
    singleton, insert, insertWith, insertWithKey, insertLookupWithKey,
    adjust, adjustWithKey, update, updateWithKey, alter, alterF,
    fromList, fromListWith, fromListWithKey, fromSet,
    map, mapWithKey, mapMaybe, mapMaybeWithKey, mapEither, mapEitherWithKey,
    unionWith, unionWithKey, unionsWith,
    intersectionWith, intersectionWithKey, differenceWith,
    insertMany, insertManyWith,
    insertWithM, insertManyWithM, insertWithKeyM, insertManyWithKeyM,
    mapAccum, mapAccumWithKey, mapAccumRWithKey, traverseWithKey,
    mapMWithKey, mapMValues, unionWithM, unionsWithM,
    traverseWithKeyByName, mapAccumByName)

-- Tier (B) signatures start with one of these; see IdOrd.AuditKey.
#ifdef IDMAP_AUDIT
#define BLIND AuditKey Id =>
#define BLINDC AuditKey Id,
#else
#define BLIND
#define BLINDC
#endif

-- ---------------------------------------------------------------------
-- (P) order-free, value-strict

singleton :: Id -> v -> IdMap v
singleton k v = fromMap (MS.singleton k v)
{-# INLINE singleton #-}

insert :: Id -> v -> IdMap v -> IdMap v
insert k v m = fromMap (MS.insert k v (toMap m))
{-# INLINE insert #-}

insertWith :: (v -> v -> v) -> Id -> v -> IdMap v -> IdMap v
insertWith f k v m = fromMap (MS.insertWith f k v (toMap m))
{-# INLINE insertWith #-}

insertWithKey :: (Id -> v -> v -> v) -> Id -> v -> IdMap v -> IdMap v
insertWithKey f k v m = fromMap (MS.insertWithKey f k v (toMap m))
{-# INLINE insertWithKey #-}

insertLookupWithKey :: (Id -> v -> v -> v) -> Id -> v -> IdMap v -> (Maybe v, IdMap v)
insertLookupWithKey f k v m =
    let (r, m') = MS.insertLookupWithKey f k v (toMap m) in (r, fromMap m')
{-# INLINE insertLookupWithKey #-}

adjust :: (v -> v) -> Id -> IdMap v -> IdMap v
adjust f k m = fromMap (MS.adjust f k (toMap m))
{-# INLINE adjust #-}

adjustWithKey :: (Id -> v -> v) -> Id -> IdMap v -> IdMap v
adjustWithKey f k m = fromMap (MS.adjustWithKey f k (toMap m))
{-# INLINE adjustWithKey #-}

update :: (v -> Maybe v) -> Id -> IdMap v -> IdMap v
update f k m = fromMap (MS.update f k (toMap m))
{-# INLINE update #-}

updateWithKey :: (Id -> v -> Maybe v) -> Id -> IdMap v -> IdMap v
updateWithKey f k m = fromMap (MS.updateWithKey f k (toMap m))
{-# INLINE updateWithKey #-}

alter :: (Maybe v -> Maybe v) -> Id -> IdMap v -> IdMap v
alter f k m = fromMap (MS.alter f k (toMap m))
{-# INLINE alter #-}

alterF :: Functor f => (Maybe v -> f (Maybe v)) -> Id -> IdMap v -> f (IdMap v)
alterF f k m = fmap fromMap (MS.alterF f k (toMap m))
{-# INLINE alterF #-}

fromList :: [(Id, v)] -> IdMap v
fromList kvs = fromMap (MS.fromList kvs)
{-# INLINE fromList #-}

fromListWith :: (v -> v -> v) -> [(Id, v)] -> IdMap v
fromListWith f kvs = fromMap (MS.fromListWith f kvs)
{-# INLINE fromListWith #-}

fromListWithKey :: (Id -> v -> v -> v) -> [(Id, v)] -> IdMap v
fromListWithKey f kvs = fromMap (MS.fromListWithKey f kvs)
{-# INLINE fromListWithKey #-}

fromSet :: (Id -> v) -> IdSet -> IdMap v
fromSet f s = fromMap (MS.fromSet f (IdSet.toSet s))
{-# INLINE fromSet #-}

map :: (a -> b) -> IdMap a -> IdMap b
map f m = fromMap (MS.map f (toMap m))
{-# INLINE map #-}

mapWithKey :: (Id -> a -> b) -> IdMap a -> IdMap b
mapWithKey f m = fromMap (MS.mapWithKey f (toMap m))
{-# INLINE mapWithKey #-}

mapMaybe :: (a -> Maybe b) -> IdMap a -> IdMap b
mapMaybe f m = fromMap (MS.mapMaybe f (toMap m))
{-# INLINE mapMaybe #-}

mapMaybeWithKey :: (Id -> a -> Maybe b) -> IdMap a -> IdMap b
mapMaybeWithKey f m = fromMap (MS.mapMaybeWithKey f (toMap m))
{-# INLINE mapMaybeWithKey #-}

mapEither :: (a -> Either b c) -> IdMap a -> (IdMap b, IdMap c)
mapEither f m = let (a, b) = MS.mapEither f (toMap m) in (fromMap a, fromMap b)
{-# INLINE mapEither #-}

mapEitherWithKey :: (Id -> a -> Either b c) -> IdMap a -> (IdMap b, IdMap c)
mapEitherWithKey f m = let (a, b) = MS.mapEitherWithKey f (toMap m) in (fromMap a, fromMap b)
{-# INLINE mapEitherWithKey #-}

unionWith :: (v -> v -> v) -> IdMap v -> IdMap v -> IdMap v
unionWith f a b = fromMap (MS.unionWith f (toMap a) (toMap b))
{-# INLINE unionWith #-}

unionWithKey :: (Id -> v -> v -> v) -> IdMap v -> IdMap v -> IdMap v
unionWithKey f a b = fromMap (MS.unionWithKey f (toMap a) (toMap b))
{-# INLINE unionWithKey #-}

unionsWith :: Foldable f => (v -> v -> v) -> f (IdMap v) -> IdMap v
unionsWith f = F.foldl' (unionWith f) empty
{-# INLINE unionsWith #-}

intersectionWith :: (a -> b -> c) -> IdMap a -> IdMap b -> IdMap c
intersectionWith f a b = fromMap (MS.intersectionWith f (toMap a) (toMap b))
{-# INLINE intersectionWith #-}

intersectionWithKey :: (Id -> a -> b -> c) -> IdMap a -> IdMap b -> IdMap c
intersectionWithKey f a b = fromMap (MS.intersectionWithKey f (toMap a) (toMap b))
{-# INLINE intersectionWithKey #-}

differenceWith :: (a -> b -> Maybe a) -> IdMap a -> IdMap b -> IdMap a
differenceWith f a b = fromMap (MS.differenceWith f (toMap a) (toMap b))
{-# INLINE differenceWith #-}

insertMany :: [(Id, v)] -> IdMap v -> IdMap v
insertMany kvs m = fromMap (P.foldr (uncurry MS.insert) (toMap m) kvs)
{-# INLINE insertMany #-}

insertManyWith :: (v -> v -> v) -> [(Id, v)] -> IdMap v -> IdMap v
insertManyWith f kvs m = fromMap (P.foldr (uncurry (MS.insertWith f)) (toMap m) kvs)
{-# INLINE insertManyWith #-}

insertWithM :: Monad m => (v -> v -> m v) -> Id -> v -> IdMap v -> m (IdMap v)
insertWithM f k v mp =
    case lookup k mp of
      Nothing -> return (insert k v mp)
      Just old -> do new <- f v old
                     return (insert k new mp)
{-# INLINE insertWithM #-}

insertManyWithM :: Monad m => (v -> v -> m v) -> [(Id, v)] -> IdMap v -> m (IdMap v)
insertManyWithM f kvs mp = foldM (\acc (k, v) -> insertWithM f k v acc) mp kvs
{-# INLINE insertManyWithM #-}

insertWithKeyM :: Monad m => (Id -> v -> v -> m v) -> Id -> v -> IdMap v -> m (IdMap v)
insertWithKeyM f k v mp =
    case lookup k mp of
      Nothing -> return (insert k v mp)
      Just old -> do new <- f k v old
                     return (insert k new mp)
{-# INLINE insertWithKeyM #-}

insertManyWithKeyM :: Monad m => (Id -> v -> v -> m v) -> [(Id, v)] -> IdMap v -> m (IdMap v)
insertManyWithKeyM f kvs mp = foldM (\acc (k, v) -> insertWithKeyM f k v acc) mp kvs
{-# INLINE insertManyWithKeyM #-}

-- ---------------------------------------------------------------------
-- (B) blind enumeration, value-strict: intern order; no order promised.

mapAccum :: BLIND (a -> b -> (a, c)) -> a -> IdMap b -> (a, IdMap c)
mapAccum f a m = let (a', m') = MS.mapAccum f a (toMap m) in (a', fromMap m')
{-# INLINE mapAccum #-}

mapAccumWithKey :: BLIND (a -> Id -> b -> (a, c)) -> a -> IdMap b -> (a, IdMap c)
mapAccumWithKey f a m = let (a', m') = MS.mapAccumWithKey f a (toMap m) in (a', fromMap m')
{-# INLINE mapAccumWithKey #-}

mapAccumRWithKey :: BLIND (a -> Id -> b -> (a, c)) -> a -> IdMap b -> (a, IdMap c)
mapAccumRWithKey f a m = let (a', m') = MS.mapAccumRWithKey f a (toMap m) in (a', fromMap m')
{-# INLINE mapAccumRWithKey #-}

traverseWithKey :: (BLINDC Applicative t) => (Id -> a -> t b) -> IdMap a -> t (IdMap b)
traverseWithKey f m = fmap fromMap (MS.traverseWithKey f (toMap m))
{-# INLINE traverseWithKey #-}

mapMValues :: (BLINDC Monad m) => (a -> m b) -> IdMap a -> m (IdMap b)
mapMValues f m =
    do kws <- mapM (\(k, v) -> do { w <- f v; return (k, w) }) (MS.toList (toMap m))
       return (fromList kws)
{-# INLINE mapMValues #-}

mapMWithKey :: (BLINDC Monad m) => (Id -> a -> m b) -> IdMap a -> m (IdMap b)
mapMWithKey f m =
    do kws <- mapM (\(k, v) -> do { w <- f k v; return (k, w) }) (MS.toList (toMap m))
       return (fromList kws)
{-# INLINE mapMWithKey #-}

unionWithM :: (BLINDC Monad m) => (v -> v -> m v) -> IdMap v -> IdMap v -> m (IdMap v)
unionWithM f m1 m2 = insertManyWithM f (MS.toList (toMap m2)) m1
{-# INLINE unionWithM #-}

unionsWithM :: (BLINDC Monad m) => (v -> v -> m v) -> [IdMap v] -> m (IdMap v)
unionsWithM _ [] = return empty
unionsWithM _ [m] = return m
unionsWithM f (m:ms) = foldM (unionWithM f) m ms
{-# INLINE unionsWithM #-}

-- ---------------------------------------------------------------------
-- ByName, value-strict

traverseWithKeyByName :: Applicative t => (Id -> v -> t b) -> IdMap v -> t (IdMap b)
traverseWithKeyByName f m =
    fmap fromList (traverse (\(k, v) -> (,) k <$> f k v) (toListByName m))
{-# INLINE traverseWithKeyByName #-}

mapAccumByName :: (a -> v -> (a, b)) -> a -> IdMap v -> (a, IdMap b)
mapAccumByName f a m =
    let step acc (k, v) = let (acc', w) = f acc v in (acc', (k, w))
        (a', kws) = L.mapAccumL step a (toListByName m)
    in  (a', fromList kws)
{-# INLINE mapAccumByName #-}
