{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, DeriveDataTypeable #-}
-- | A map keyed by 'Id' that does not hand out its key order.
--
-- @Ord Id@ (Id.hs, idCompare) is string-intern order: the order in which
-- the compiler first met the strings. Every list a @Data.Map Id@ hands
-- out is in that order, so it depends on the input order, and when such
-- a list reaches an output (Verilog, Bluesim, .ba/.bo, a message, a
-- dump) the output moves with it. This newtype keeps the lookup
-- structure and its speed (every wrapper is INLINE, so GHC's
-- specialisation of Data.Map over @Ord Id@ survives) and sorts the API
-- into tiers:
--
-- (P) order-free functions, under their Data.Map names: construction,
--     lookup, update, set algebra, filters, maps, plus Util's map_*
--     helpers minus the prefix. A drop-in for the Data.Map spelling.
--
-- (B) blind enumerations, under their Data.Map names: toList, assocs,
--     keys, elems, the folds, mapAccum*, traverseWithKey and the
--     monadic twins of Util's map_mapM / map_unionWithM. Each enumerates
--     in intern order and promises no order; a result that reaches an
--     output, or an effect whose order matters, calls the ByName twin.
--     Under -DIDMAP_AUDIT each carries an unsatisfiable constraint
--     (IdOrd.AuditKey), so a build with -fdefer-type-errors lists every
--     blind site; IdOrd.hs gives the command.
--
-- (N) deliberately absent: toAscList, toDescList, findMin, findMax,
--     lookupMin, lookupMax, deleteMin, deleteMax, deleteFindMin,
--     deleteFindMax, minView, maxView, minViewWithKey, maxViewWithKey,
--     updateMin, updateMax, elemAt, lookupIndex, findIndex, take, drop,
--     splitAt, split, splitLookup, splitRoot, lookupLT/GT/LE/GE,
--     spanAntitone, takeWhileAntitone, dropWhileAntitone,
--     mapKeysMonotonic, fromAscList*, fromDescList*,
--     fromDistinctAscList, the Ord instance, Foldable and Traversable.
--     Each names an order, is positional, or has an Ord-Id precondition
--     no caller can state; a use is a type error until the author
--     chooses onlyEntry, a ByName function, or the blind one on purpose.
--
-- (ByName) the ordered API, the only functions here that sort:
--     toListByName, keysByName, elemsByName, foldrByName,
--     foldrWithKeyByName, foldlByName', minByName, maxByName,
--     traverseWithKeyByName, mapAccumByName, compareByName. The order is
--     IdOrd.cmpByName: the (base, qualifier) String pair in code-point
--     order, a function of the program text. Two distinct keys never tie
--     (IdOrd.hs), so a ByName list is a permutation of the blind one.
--     Cost: one decorate-sort-undecorate per call (n = 10^3 keys well
--     under a millisecond), paid at emission sites only; no memoised
--     view, since maps are rebuilt per pass and a second index would put
--     string compares on the hot path.
--
-- Show and PPrint render the blind list for now, so that replacing
-- @M.Map Id v@ by @IdMap v@ changes no dump byte; the dumps commit (plan
-- P4(2)) flips both to the ByName list. The Bin instance (BinData.hs)
-- mirrors the generic Map instance the same way until the .ba/.bo
-- commit (P4(6)).
--
-- fromMap/toMap are migration boundaries between a converted producer
-- and a not-yet-converted consumer; they are deleted at the end.
-- IdMap.Strict is the value-strict twin (Data.Map.Strict's split).
module IdMap(
    IdMap,
    -- * (P) order-free
    empty, singleton, fromList, fromListWith, fromListWithKey, fromSet,
    insert, insertWith, insertWithKey, insertLookupWithKey,
    delete, adjust, adjustWithKey, update, updateWithKey, alter, alterF,
    lookup, (!), (!?), findWithDefault, member, notMember, null, size,
    union, unionWith, unionWithKey, unions, unionsWith,
    difference, (\\), differenceWith,
    intersection, intersectionWith, intersectionWithKey,
    restrictKeys, withoutKeys, disjoint, isSubmapOf, isSubmapOfBy,
    map, mapWithKey, mapMaybe, mapMaybeWithKey, mapEither, mapEitherWithKey,
    filter, filterWithKey, partition, partitionWithKey,
    keysSet, mapKeys, mapKeysWith,
    lookupOrErr, insertMany, insertManyWith, deleteMany,
    insertWithM, insertManyWithM, insertWithKeyM, insertManyWithKeyM,
    onlyEntry,
    fromMap, toMap,
    -- * (B) blind enumeration: intern order, no order promised
    toList, assocs, keys, elems,
    foldr, foldl, foldr', foldl',
    foldrWithKey, foldlWithKey, foldrWithKey', foldlWithKey', foldMapWithKey,
    mapAccum, mapAccumWithKey, mapAccumRWithKey, traverseWithKey,
    mapMWithKey, mapMValues, unionWithM, unionsWithM,
    -- * ByName: ordered by IdOrd.cmpByName
    toListByName, keysByName, elemsByName,
    foldrByName, foldrWithKeyByName, foldlByName',
    minByName, maxByName,
    traverseWithKeyByName, mapAccumByName, compareByName
    ) where

#if MIN_VERSION_base(4,20,0)
import Prelude hiding (lookup, map, filter, foldr, foldl, foldl', null)
#else
import Prelude hiding (lookup, map, filter, foldr, foldl, null)
#endif
import qualified Prelude as P
import qualified Data.Map as M
import qualified Data.Foldable as F
import qualified Data.List as L
import Data.Coerce(coerce)
import Data.Data(Data)
import Data.Ord(comparing)
import Control.Monad(foldM)

import ErrorUtil(internalError)
import Eval(NFData)
import PPrint(PPrint(..), vsep, text, (<+>))
import Id(Id)
import IdPrint()
#ifdef IDMAP_AUDIT
import IdOrd(nameKey, cmpByName, AuditKey)
#else
import IdOrd(nameKey, cmpByName)
#endif
import IdSet(IdSet)
import qualified IdSet

-- Tier (B) signatures start with one of these; see IdOrd.AuditKey.
-- (The primed names spell their two signatures out instead: cpp
-- -traditional takes the prime for a quote and does not expand a macro
-- after it on the same line.)
#ifdef IDMAP_AUDIT
#define BLIND AuditKey Id =>
#define BLINDC AuditKey Id,
#else
#define BLIND
#define BLINDC
#endif

newtype IdMap v = IdMap (M.Map Id v)
    deriving (Eq, NFData)

deriving instance Data v => Data (IdMap v)

instance Functor IdMap where
    fmap = map
    {-# INLINE fmap #-}

-- left-biased union, as Data.Map's
instance Semigroup (IdMap v) where
    (<>) = union
    {-# INLINE (<>) #-}

instance Monoid (IdMap v) where
    mempty = empty
    {-# INLINE mempty #-}

-- Both render the blind list, in Data.Map's own format, so that the type
-- swap changes no dump byte; the dumps commit (plan P4(2)) flips them to
-- toListByName.
instance Show v => Show (IdMap v) where
    showsPrec d m = showParen (d > 10) $ showString "fromList " . shows (toList m)

instance PPrint v => PPrint (IdMap v) where
    pPrint d _ m = vsep [pPrint d 0 k <+> text "->" <+> pPrint d 0 v | (k, v) <- toList m]

-- ---------------------------------------------------------------------
-- (P) order-free

empty :: IdMap v
empty = IdMap M.empty
{-# INLINE empty #-}

singleton :: Id -> v -> IdMap v
singleton k v = IdMap (M.singleton k v)
{-# INLINE singleton #-}

fromList :: [(Id, v)] -> IdMap v
fromList kvs = IdMap (M.fromList kvs)
{-# INLINE fromList #-}

fromListWith :: (v -> v -> v) -> [(Id, v)] -> IdMap v
fromListWith f kvs = IdMap (M.fromListWith f kvs)
{-# INLINE fromListWith #-}

fromListWithKey :: (Id -> v -> v -> v) -> [(Id, v)] -> IdMap v
fromListWithKey f kvs = IdMap (M.fromListWithKey f kvs)
{-# INLINE fromListWithKey #-}

fromSet :: (Id -> v) -> IdSet -> IdMap v
fromSet f s = IdMap (M.fromSet f (IdSet.toSet s))
{-# INLINE fromSet #-}

insert :: Id -> v -> IdMap v -> IdMap v
insert k v (IdMap m) = IdMap (M.insert k v m)
{-# INLINE insert #-}

insertWith :: (v -> v -> v) -> Id -> v -> IdMap v -> IdMap v
insertWith f k v (IdMap m) = IdMap (M.insertWith f k v m)
{-# INLINE insertWith #-}

insertWithKey :: (Id -> v -> v -> v) -> Id -> v -> IdMap v -> IdMap v
insertWithKey f k v (IdMap m) = IdMap (M.insertWithKey f k v m)
{-# INLINE insertWithKey #-}

insertLookupWithKey :: (Id -> v -> v -> v) -> Id -> v -> IdMap v -> (Maybe v, IdMap v)
insertLookupWithKey f k v (IdMap m) = coerce (M.insertLookupWithKey f k v m)
{-# INLINE insertLookupWithKey #-}

delete :: Id -> IdMap v -> IdMap v
delete k (IdMap m) = IdMap (M.delete k m)
{-# INLINE delete #-}

adjust :: (v -> v) -> Id -> IdMap v -> IdMap v
adjust f k (IdMap m) = IdMap (M.adjust f k m)
{-# INLINE adjust #-}

adjustWithKey :: (Id -> v -> v) -> Id -> IdMap v -> IdMap v
adjustWithKey f k (IdMap m) = IdMap (M.adjustWithKey f k m)
{-# INLINE adjustWithKey #-}

update :: (v -> Maybe v) -> Id -> IdMap v -> IdMap v
update f k (IdMap m) = IdMap (M.update f k m)
{-# INLINE update #-}

updateWithKey :: (Id -> v -> Maybe v) -> Id -> IdMap v -> IdMap v
updateWithKey f k (IdMap m) = IdMap (M.updateWithKey f k m)
{-# INLINE updateWithKey #-}

alter :: (Maybe v -> Maybe v) -> Id -> IdMap v -> IdMap v
alter f k (IdMap m) = IdMap (M.alter f k m)
{-# INLINE alter #-}

alterF :: Functor f => (Maybe v -> f (Maybe v)) -> Id -> IdMap v -> f (IdMap v)
alterF f k (IdMap m) = fmap IdMap (M.alterF f k m)
{-# INLINE alterF #-}

lookup :: Id -> IdMap v -> Maybe v
lookup k (IdMap m) = M.lookup k m
{-# INLINE lookup #-}

(!) :: IdMap v -> Id -> v
(!) (IdMap m) k = m M.! k
{-# INLINE (!) #-}

(!?) :: IdMap v -> Id -> Maybe v
(!?) (IdMap m) k = m M.!? k
{-# INLINE (!?) #-}

findWithDefault :: v -> Id -> IdMap v -> v
findWithDefault d k (IdMap m) = M.findWithDefault d k m
{-# INLINE findWithDefault #-}

member :: Id -> IdMap v -> Bool
member k (IdMap m) = M.member k m
{-# INLINE member #-}

notMember :: Id -> IdMap v -> Bool
notMember k (IdMap m) = M.notMember k m
{-# INLINE notMember #-}

null :: IdMap v -> Bool
null (IdMap m) = M.null m
{-# INLINE null #-}

size :: IdMap v -> Int
size (IdMap m) = M.size m
{-# INLINE size #-}

union :: IdMap v -> IdMap v -> IdMap v
union (IdMap a) (IdMap b) = IdMap (M.union a b)
{-# INLINE union #-}

unionWith :: (v -> v -> v) -> IdMap v -> IdMap v -> IdMap v
unionWith f (IdMap a) (IdMap b) = IdMap (M.unionWith f a b)
{-# INLINE unionWith #-}

unionWithKey :: (Id -> v -> v -> v) -> IdMap v -> IdMap v -> IdMap v
unionWithKey f (IdMap a) (IdMap b) = IdMap (M.unionWithKey f a b)
{-# INLINE unionWithKey #-}

unions :: Foldable f => f (IdMap v) -> IdMap v
unions = F.foldl' union empty
{-# INLINE unions #-}

unionsWith :: Foldable f => (v -> v -> v) -> f (IdMap v) -> IdMap v
unionsWith f = F.foldl' (unionWith f) empty
{-# INLINE unionsWith #-}

difference :: IdMap a -> IdMap b -> IdMap a
difference (IdMap a) (IdMap b) = IdMap (M.difference a b)
{-# INLINE difference #-}

(\\) :: IdMap a -> IdMap b -> IdMap a
(\\) = difference
{-# INLINE (\\) #-}

differenceWith :: (a -> b -> Maybe a) -> IdMap a -> IdMap b -> IdMap a
differenceWith f (IdMap a) (IdMap b) = IdMap (M.differenceWith f a b)
{-# INLINE differenceWith #-}

intersection :: IdMap a -> IdMap b -> IdMap a
intersection (IdMap a) (IdMap b) = IdMap (M.intersection a b)
{-# INLINE intersection #-}

intersectionWith :: (a -> b -> c) -> IdMap a -> IdMap b -> IdMap c
intersectionWith f (IdMap a) (IdMap b) = IdMap (M.intersectionWith f a b)
{-# INLINE intersectionWith #-}

intersectionWithKey :: (Id -> a -> b -> c) -> IdMap a -> IdMap b -> IdMap c
intersectionWithKey f (IdMap a) (IdMap b) = IdMap (M.intersectionWithKey f a b)
{-# INLINE intersectionWithKey #-}

restrictKeys :: IdMap v -> IdSet -> IdMap v
restrictKeys (IdMap m) s = IdMap (M.restrictKeys m (IdSet.toSet s))
{-# INLINE restrictKeys #-}

withoutKeys :: IdMap v -> IdSet -> IdMap v
withoutKeys (IdMap m) s = IdMap (M.withoutKeys m (IdSet.toSet s))
{-# INLINE withoutKeys #-}

disjoint :: IdMap a -> IdMap b -> Bool
disjoint (IdMap a) (IdMap b) = M.disjoint a b
{-# INLINE disjoint #-}

isSubmapOf :: Eq v => IdMap v -> IdMap v -> Bool
isSubmapOf (IdMap a) (IdMap b) = M.isSubmapOf a b
{-# INLINE isSubmapOf #-}

isSubmapOfBy :: (a -> b -> Bool) -> IdMap a -> IdMap b -> Bool
isSubmapOfBy f (IdMap a) (IdMap b) = M.isSubmapOfBy f a b
{-# INLINE isSubmapOfBy #-}

map :: (a -> b) -> IdMap a -> IdMap b
map f (IdMap m) = IdMap (M.map f m)
{-# INLINE map #-}

mapWithKey :: (Id -> a -> b) -> IdMap a -> IdMap b
mapWithKey f (IdMap m) = IdMap (M.mapWithKey f m)
{-# INLINE mapWithKey #-}

mapMaybe :: (a -> Maybe b) -> IdMap a -> IdMap b
mapMaybe f (IdMap m) = IdMap (M.mapMaybe f m)
{-# INLINE mapMaybe #-}

mapMaybeWithKey :: (Id -> a -> Maybe b) -> IdMap a -> IdMap b
mapMaybeWithKey f (IdMap m) = IdMap (M.mapMaybeWithKey f m)
{-# INLINE mapMaybeWithKey #-}

mapEither :: (a -> Either b c) -> IdMap a -> (IdMap b, IdMap c)
mapEither f (IdMap m) = coerce (M.mapEither f m)
{-# INLINE mapEither #-}

mapEitherWithKey :: (Id -> a -> Either b c) -> IdMap a -> (IdMap b, IdMap c)
mapEitherWithKey f (IdMap m) = coerce (M.mapEitherWithKey f m)
{-# INLINE mapEitherWithKey #-}

filter :: (v -> Bool) -> IdMap v -> IdMap v
filter p (IdMap m) = IdMap (M.filter p m)
{-# INLINE filter #-}

filterWithKey :: (Id -> v -> Bool) -> IdMap v -> IdMap v
filterWithKey p (IdMap m) = IdMap (M.filterWithKey p m)
{-# INLINE filterWithKey #-}

partition :: (v -> Bool) -> IdMap v -> (IdMap v, IdMap v)
partition p (IdMap m) = coerce (M.partition p m)
{-# INLINE partition #-}

partitionWithKey :: (Id -> v -> Bool) -> IdMap v -> (IdMap v, IdMap v)
partitionWithKey p (IdMap m) = coerce (M.partitionWithKey p m)
{-# INLINE partitionWithKey #-}

keysSet :: IdMap v -> IdSet
keysSet (IdMap m) = IdSet.fromSet (M.keysSet m)
{-# INLINE keysSet #-}

-- The result is rebuilt by fromList, so the input's key order does not
-- reach it; but when f sends two keys to one, Data.Map keeps the value
-- of the key later in intern order, so such an f needs mapKeysWith with
-- a commutative combiner.
mapKeys :: (Id -> Id) -> IdMap v -> IdMap v
mapKeys f (IdMap m) = IdMap (M.mapKeys f m)
{-# INLINE mapKeys #-}

mapKeysWith :: (v -> v -> v) -> (Id -> Id) -> IdMap v -> IdMap v
mapKeysWith c f (IdMap m) = IdMap (M.mapKeysWith c f m)
{-# INLINE mapKeysWith #-}

-- Util's map_* helpers, which the newtype hides, minus the prefix.
-- The monadic ones run their effects in the order of the LIST argument.

lookupOrErr :: String -> Id -> IdMap v -> v
lookupOrErr err k (IdMap m) = M.findWithDefault (internalError err) k m
{-# INLINE lookupOrErr #-}

insertMany :: [(Id, v)] -> IdMap v -> IdMap v
insertMany kvs (IdMap m) = IdMap (P.foldr (uncurry M.insert) m kvs)
{-# INLINE insertMany #-}

insertManyWith :: (v -> v -> v) -> [(Id, v)] -> IdMap v -> IdMap v
insertManyWith f kvs (IdMap m) = IdMap (P.foldr (uncurry (M.insertWith f)) m kvs)
{-# INLINE insertManyWith #-}

deleteMany :: [Id] -> IdMap v -> IdMap v
deleteMany ks (IdMap m) = IdMap (P.foldr M.delete m ks)
{-# INLINE deleteMany #-}

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

-- | The one entry of a singleton map; replaces the size-1 findMin
-- idiom, which named an order it did not need.
onlyEntry :: IdMap v -> (Id, v)
onlyEntry (IdMap m) = case M.toList m of
    [kv] -> kv
    _ -> internalError ("IdMap.onlyEntry: size " ++ show (M.size m))

-- Migration boundaries between a converted producer and a not-yet-
-- converted consumer (or the reverse); deleted at the end of the swap.
fromMap :: M.Map Id v -> IdMap v
fromMap = IdMap
{-# INLINE fromMap #-}

toMap :: IdMap v -> M.Map Id v
toMap (IdMap m) = m
{-# INLINE toMap #-}

-- ---------------------------------------------------------------------
-- (B) blind enumeration: intern order; no order promised. A result
-- that reaches an output, or an effect whose order matters, calls the
-- ByName twin.

toList :: BLIND IdMap v -> [(Id, v)]
toList (IdMap m) = M.toList m
{-# INLINE toList #-}

assocs :: BLIND IdMap v -> [(Id, v)]
assocs (IdMap m) = M.assocs m
{-# INLINE assocs #-}

keys :: BLIND IdMap v -> [Id]
keys (IdMap m) = M.keys m
{-# INLINE keys #-}

elems :: BLIND IdMap v -> [v]
elems (IdMap m) = M.elems m
{-# INLINE elems #-}

foldr :: BLIND (v -> b -> b) -> b -> IdMap v -> b
foldr f z (IdMap m) = M.foldr f z m
{-# INLINE foldr #-}

foldl :: BLIND (b -> v -> b) -> b -> IdMap v -> b
foldl f z (IdMap m) = M.foldl f z m
{-# INLINE foldl #-}

#ifdef IDMAP_AUDIT
foldr' :: AuditKey Id => (v -> b -> b) -> b -> IdMap v -> b
#else
foldr' :: (v -> b -> b) -> b -> IdMap v -> b
#endif
foldr' f z (IdMap m) = M.foldr' f z m
{-# INLINE foldr' #-}

#ifdef IDMAP_AUDIT
foldl' :: AuditKey Id => (b -> v -> b) -> b -> IdMap v -> b
#else
foldl' :: (b -> v -> b) -> b -> IdMap v -> b
#endif
foldl' f z (IdMap m) = M.foldl' f z m
{-# INLINE foldl' #-}

foldrWithKey :: BLIND (Id -> v -> b -> b) -> b -> IdMap v -> b
foldrWithKey f z (IdMap m) = M.foldrWithKey f z m
{-# INLINE foldrWithKey #-}

foldlWithKey :: BLIND (b -> Id -> v -> b) -> b -> IdMap v -> b
foldlWithKey f z (IdMap m) = M.foldlWithKey f z m
{-# INLINE foldlWithKey #-}

#ifdef IDMAP_AUDIT
foldrWithKey' :: AuditKey Id => (Id -> v -> b -> b) -> b -> IdMap v -> b
#else
foldrWithKey' :: (Id -> v -> b -> b) -> b -> IdMap v -> b
#endif
foldrWithKey' f z (IdMap m) = M.foldrWithKey' f z m
{-# INLINE foldrWithKey' #-}

#ifdef IDMAP_AUDIT
foldlWithKey' :: AuditKey Id => (b -> Id -> v -> b) -> b -> IdMap v -> b
#else
foldlWithKey' :: (b -> Id -> v -> b) -> b -> IdMap v -> b
#endif
foldlWithKey' f z (IdMap m) = M.foldlWithKey' f z m
{-# INLINE foldlWithKey' #-}

foldMapWithKey :: (BLINDC Monoid m) => (Id -> v -> m) -> IdMap v -> m
foldMapWithKey f (IdMap m) = M.foldMapWithKey f m
{-# INLINE foldMapWithKey #-}

mapAccum :: BLIND (a -> b -> (a, c)) -> a -> IdMap b -> (a, IdMap c)
mapAccum f a (IdMap m) = coerce (M.mapAccum f a m)
{-# INLINE mapAccum #-}

mapAccumWithKey :: BLIND (a -> Id -> b -> (a, c)) -> a -> IdMap b -> (a, IdMap c)
mapAccumWithKey f a (IdMap m) = coerce (M.mapAccumWithKey f a m)
{-# INLINE mapAccumWithKey #-}

mapAccumRWithKey :: BLIND (a -> Id -> b -> (a, c)) -> a -> IdMap b -> (a, IdMap c)
mapAccumRWithKey f a (IdMap m) = coerce (M.mapAccumRWithKey f a m)
{-# INLINE mapAccumRWithKey #-}

traverseWithKey :: (BLINDC Applicative t) => (Id -> a -> t b) -> IdMap a -> t (IdMap b)
traverseWithKey f (IdMap m) = fmap IdMap (M.traverseWithKey f m)
{-# INLINE traverseWithKey #-}

-- Util.map_mapM (toList, then fromList): effects in intern order of the keys.
mapMValues :: (BLINDC Monad m) => (a -> m b) -> IdMap a -> m (IdMap b)
mapMValues f (IdMap m) =
    do kws <- mapM (\(k, v) -> do { w <- f v; return (k, w) }) (M.toList m)
       return (IdMap (M.fromList kws))
{-# INLINE mapMValues #-}

mapMWithKey :: (BLINDC Monad m) => (Id -> a -> m b) -> IdMap a -> m (IdMap b)
mapMWithKey f (IdMap m) =
    do kws <- mapM (\(k, v) -> do { w <- f k v; return (k, w) }) (M.toList m)
       return (IdMap (M.fromList kws))
{-# INLINE mapMWithKey #-}

-- Util.map_unionWithM / map_unionsWithM: effects in intern order of
-- the second map's keys.
unionWithM :: (BLINDC Monad m) => (v -> v -> m v) -> IdMap v -> IdMap v -> m (IdMap v)
unionWithM f m1 (IdMap m2) = insertManyWithM f (M.toList m2) m1
{-# INLINE unionWithM #-}

unionsWithM :: (BLINDC Monad m) => (v -> v -> m v) -> [IdMap v] -> m (IdMap v)
unionsWithM _ [] = return empty
unionsWithM _ [m] = return m
unionsWithM f (m:ms) = foldM (unionWithM f) m ms
{-# INLINE unionsWithM #-}

-- ---------------------------------------------------------------------
-- ByName: ordered by IdOrd.cmpByName, the only functions here that sort.

-- decorate once, sort on the key, undecorate
toListByName :: IdMap v -> [(Id, v)]
toListByName (IdMap m) =
    P.map snd (L.sortBy (comparing fst) [ (nameKey k, kv) | kv@(k, _) <- M.toList m ])

keysByName :: IdMap v -> [Id]
keysByName = P.map fst . toListByName
{-# INLINE keysByName #-}

elemsByName :: IdMap v -> [v]
elemsByName = P.map snd . toListByName
{-# INLINE elemsByName #-}

foldrByName :: (v -> b -> b) -> b -> IdMap v -> b
foldrByName f z = P.foldr f z . elemsByName
{-# INLINE foldrByName #-}

foldrWithKeyByName :: (Id -> v -> b -> b) -> b -> IdMap v -> b
foldrWithKeyByName f z = P.foldr (\(k, v) acc -> f k v acc) z . toListByName
{-# INLINE foldrWithKeyByName #-}

foldlByName' :: (b -> v -> b) -> b -> IdMap v -> b
foldlByName' f z = F.foldl' f z . elemsByName
{-# INLINE foldlByName' #-}

-- O(n), by a fold; distinct keys never tie (IdOrd).
minByName :: IdMap v -> Maybe (Id, v)
minByName (IdMap m) = pickBy LT (M.toList m)

maxByName :: IdMap v -> Maybe (Id, v)
maxByName (IdMap m) = pickBy GT (M.toList m)

pickBy :: Ordering -> [(Id, v)] -> Maybe (Id, v)
pickBy _ [] = Nothing
pickBy o (kv:kvs) = Just (F.foldl' pick kv kvs)
  where pick a@(k, _) b@(k', _) = if cmpByName k' k == o then b else a

-- | Effects in name order; the ordered twin of mapMWithKey / traverseWithKey.
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

-- | Lexicographic over the name-ordered (name, value) lists: the
-- text-ordered twin of Data.Map's Ord instance (which is tier N).
compareByName :: Ord v => IdMap v -> IdMap v -> Ordering
compareByName m1 m2 = compare (decorate m1) (decorate m2)
  where decorate m = [ (nameKey k, v) | (k, v) <- toListByName m ]
