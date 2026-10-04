{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, DeriveDataTypeable #-}
-- | A set of 'Id' that does not hand out its element order.
--
-- @Ord Id@ is string-intern order (see IdOrd.hs), so every list a
-- @Data.Set Id@ hands out depends on the input order. This newtype keeps
-- the structure and its speed (every wrapper is INLINE, so GHC's
-- specialisation of Data.Set over @Ord Id@ survives) and sorts the API
-- into tiers; IdMap.hs describes the tiers in full. In short:
--
-- (P) order-free, under the Data.Set names; (B) blind enumeration under
-- the Data.Set names, intern order, no order promised, and under
-- -DIDMAP_AUDIT an unsatisfiable constraint (IdOrd.AuditKey); (N)
-- deliberately absent: toAscList, toDescList, findMin, findMax,
-- lookupMin/Max, deleteMin/Max, deleteFindMin/Max, minView, maxView,
-- elemAt, findIndex, lookupIndex, take, drop, splitAt, split,
-- splitMember, lookupLT/GT/LE/GE, the *Antitone family, mapMonotonic,
-- cartesianProduct, powerSet, fromAscList, fromDistinctAscList, the Ord
-- instance and Foldable; (ByName) the ordered API, by IdOrd.cmpByName,
-- the only functions here that sort.
--
-- Show and PPrint render the blind list for now, so that replacing
-- @S.Set Id@ by @IdSet@ changes no dump byte; the dumps commit (plan
-- P4(2)) flips both to toListByName. The Bin instance (BinData.hs) does
-- the same until the .ba/.bo commit (P4(6)).
module IdSet(
    IdSet,
    -- * (P) order-free
    empty, singleton, fromList, insert, delete, member, notMember, null, size,
    union, unions, difference, (\\), intersection,
    isSubsetOf, isProperSubsetOf, disjoint,
    filter, partition, map, mapToSet, alterF,
    insertMany, deleteMany, intersectMany,
    onlyElem,
    fromSet, toSet,
    -- * (B) blind enumeration: intern order, no order promised
    toList, elems, foldr, foldl, foldr', foldl', fold,
    -- * ByName: ordered by IdOrd.cmpByName
    toListByName, elemsByName, foldrByName, foldlByName', minByName, maxByName
    ) where

#if MIN_VERSION_base(4,20,0)
import Prelude hiding (map, filter, foldr, foldl, foldl', null)
#else
import Prelude hiding (map, filter, foldr, foldl, null)
#endif
import qualified Prelude as P
import qualified Data.Set as S
import qualified Data.Foldable as F
import Data.Coerce(coerce)
import Data.Data(Data)

import ErrorUtil(internalError)
import Eval(NFData)
import PPrint(PPrint(..))
import Id(Id)
import IdPrint()
#ifdef IDMAP_AUDIT
import IdOrd(sortByName, cmpByName, AuditKey)
#else
import IdOrd(sortByName, cmpByName)
#endif

-- Tier (B) signatures start with this; see IdOrd.AuditKey.
-- (The primed names spell their two signatures out instead: cpp
-- -traditional takes the prime for a quote and does not expand a macro
-- after it on the same line.)
#ifdef IDMAP_AUDIT
#define BLIND AuditKey Id =>
#else
#define BLIND
#endif

newtype IdSet = IdSet (S.Set Id)
    deriving (Eq, NFData)

deriving instance Data IdSet

instance Semigroup IdSet where
    (<>) = union
    {-# INLINE (<>) #-}

instance Monoid IdSet where
    mempty = empty
    {-# INLINE mempty #-}

-- Both render the blind list, in Data.Set's own format, so that the type
-- swap changes no dump byte; the dumps commit (plan P4(2)) flips them to
-- toListByName.
instance Show IdSet where
    showsPrec d s = showParen (d > 10) $ showString "fromList " . shows (toList s)

instance PPrint IdSet where
    pPrint d i s = pPrint d i (toList s)

-- ---------------------------------------------------------------------
-- (P) order-free

empty :: IdSet
empty = IdSet S.empty
{-# INLINE empty #-}

singleton :: Id -> IdSet
singleton x = IdSet (S.singleton x)
{-# INLINE singleton #-}

fromList :: [Id] -> IdSet
fromList xs = IdSet (S.fromList xs)
{-# INLINE fromList #-}

insert :: Id -> IdSet -> IdSet
insert x (IdSet s) = IdSet (S.insert x s)
{-# INLINE insert #-}

delete :: Id -> IdSet -> IdSet
delete x (IdSet s) = IdSet (S.delete x s)
{-# INLINE delete #-}

member :: Id -> IdSet -> Bool
member x (IdSet s) = S.member x s
{-# INLINE member #-}

notMember :: Id -> IdSet -> Bool
notMember x (IdSet s) = S.notMember x s
{-# INLINE notMember #-}

null :: IdSet -> Bool
null (IdSet s) = S.null s
{-# INLINE null #-}

size :: IdSet -> Int
size (IdSet s) = S.size s
{-# INLINE size #-}

union :: IdSet -> IdSet -> IdSet
union (IdSet a) (IdSet b) = IdSet (S.union a b)
{-# INLINE union #-}

unions :: Foldable f => f IdSet -> IdSet
unions = F.foldl' union empty
{-# INLINE unions #-}

difference :: IdSet -> IdSet -> IdSet
difference (IdSet a) (IdSet b) = IdSet (S.difference a b)
{-# INLINE difference #-}

(\\) :: IdSet -> IdSet -> IdSet
(\\) = difference
{-# INLINE (\\) #-}

intersection :: IdSet -> IdSet -> IdSet
intersection (IdSet a) (IdSet b) = IdSet (S.intersection a b)
{-# INLINE intersection #-}

isSubsetOf :: IdSet -> IdSet -> Bool
isSubsetOf (IdSet a) (IdSet b) = S.isSubsetOf a b
{-# INLINE isSubsetOf #-}

isProperSubsetOf :: IdSet -> IdSet -> Bool
isProperSubsetOf (IdSet a) (IdSet b) = S.isProperSubsetOf a b
{-# INLINE isProperSubsetOf #-}

disjoint :: IdSet -> IdSet -> Bool
disjoint (IdSet a) (IdSet b) = S.disjoint a b
{-# INLINE disjoint #-}

filter :: (Id -> Bool) -> IdSet -> IdSet
filter p (IdSet s) = IdSet (S.filter p s)
{-# INLINE filter #-}

partition :: (Id -> Bool) -> IdSet -> (IdSet, IdSet)
partition p (IdSet s) = coerce (S.partition p s)
{-# INLINE partition #-}

map :: (Id -> Id) -> IdSet -> IdSet
map f (IdSet s) = IdSet (S.map f s)
{-# INLINE map #-}

-- | Map into a set over another key type (the image's order is that
-- type's Ord, the caller's business).
mapToSet :: Ord b => (Id -> b) -> IdSet -> S.Set b
mapToSet f (IdSet s) = S.map f s
{-# INLINE mapToSet #-}

alterF :: Functor f => (Bool -> f Bool) -> Id -> IdSet -> f IdSet
alterF f x (IdSet s) = fmap IdSet (S.alterF f x s)
{-# INLINE alterF #-}

-- Util's set_* helpers, which the newtype hides, minus the prefix.
insertMany :: [Id] -> IdSet -> IdSet
insertMany xs (IdSet s) = IdSet (P.foldr S.insert s xs)
{-# INLINE insertMany #-}

deleteMany :: [Id] -> IdSet -> IdSet
deleteMany xs (IdSet s) = IdSet (P.foldr S.delete s xs)
{-# INLINE deleteMany #-}

intersectMany :: [IdSet] -> IdSet
intersectMany [] = empty
intersectMany (s:ss) = P.foldr intersection s ss
{-# INLINE intersectMany #-}

-- | The one element of a singleton set; replaces the size-1 findMin
-- idiom, which named an order it did not need.
onlyElem :: IdSet -> Id
onlyElem (IdSet s) = case S.toList s of
    [x] -> x
    _ -> internalError ("IdSet.onlyElem: size " ++ show (S.size s))

-- Migration boundaries between a converted producer and a not-yet-
-- converted consumer (or the reverse); deleted at the end of the swap.
fromSet :: S.Set Id -> IdSet
fromSet = IdSet
{-# INLINE fromSet #-}

toSet :: IdSet -> S.Set Id
toSet (IdSet s) = s
{-# INLINE toSet #-}

-- ---------------------------------------------------------------------
-- (B) blind enumeration: intern order; no order promised. A result
-- that reaches an output, or an effect whose order matters, calls the
-- ByName twin.

toList :: BLIND IdSet -> [Id]
toList (IdSet s) = S.toList s
{-# INLINE toList #-}

elems :: BLIND IdSet -> [Id]
elems (IdSet s) = S.elems s
{-# INLINE elems #-}

foldr :: BLIND (Id -> b -> b) -> b -> IdSet -> b
foldr f z (IdSet s) = S.foldr f z s
{-# INLINE foldr #-}

foldl :: BLIND (b -> Id -> b) -> b -> IdSet -> b
foldl f z (IdSet s) = S.foldl f z s
{-# INLINE foldl #-}

#ifdef IDMAP_AUDIT
foldr' :: AuditKey Id => (Id -> b -> b) -> b -> IdSet -> b
#else
foldr' :: (Id -> b -> b) -> b -> IdSet -> b
#endif
foldr' f z (IdSet s) = S.foldr' f z s
{-# INLINE foldr' #-}

#ifdef IDMAP_AUDIT
foldl' :: AuditKey Id => (b -> Id -> b) -> b -> IdSet -> b
#else
foldl' :: (b -> Id -> b) -> b -> IdSet -> b
#endif
foldl' f z (IdSet s) = S.foldl' f z s
{-# INLINE foldl' #-}

-- | Data.Set's deprecated name for foldr.
fold :: BLIND (Id -> b -> b) -> b -> IdSet -> b
fold f z (IdSet s) = S.foldr f z s
{-# INLINE fold #-}

-- ---------------------------------------------------------------------
-- ByName: ordered by IdOrd.cmpByName, the only functions here that sort.

toListByName :: IdSet -> [Id]
toListByName (IdSet s) = sortByName (S.toList s)

elemsByName :: IdSet -> [Id]
elemsByName = toListByName
{-# INLINE elemsByName #-}

foldrByName :: (Id -> b -> b) -> b -> IdSet -> b
foldrByName f z = P.foldr f z . toListByName
{-# INLINE foldrByName #-}

foldlByName' :: (b -> Id -> b) -> b -> IdSet -> b
foldlByName' f z = F.foldl' f z . toListByName
{-# INLINE foldlByName' #-}

-- O(n), by a fold; distinct elements never tie (IdOrd).
minByName :: IdSet -> Maybe Id
minByName (IdSet s) = pickBy LT (S.toList s)

maxByName :: IdSet -> Maybe Id
maxByName (IdSet s) = pickBy GT (S.toList s)

pickBy :: Ordering -> [Id] -> Maybe Id
pickBy _ [] = Nothing
pickBy o (x:xs) = Just (F.foldl' pick x xs)
  where pick a b = if cmpByName b a == o then b else a
