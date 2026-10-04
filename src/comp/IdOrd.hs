-- | Name-ordered comparators on 'Id' and 'Position', and the bare-list
-- sorts built from them.
--
-- @Ord Id@ (Id.hs, idCompare) is string-intern order: it compares the
-- intern ids of the two FStrings, which the compiler handed out in the
-- order it first met each string, so any list enumerated in it depends
-- on the input order (and reverses under -reverse-intern-order).
-- Everything in this module orders by the NAME instead: the
-- (base, qualifier) String pair in code-point order, a function of the
-- program text alone. IdMap and IdSet build their ByName tier on these;
-- sites that hold bare lists of Ids call them directly.
--
-- Totality. @Eq Id@ is the (base, qualifier) string pair (Id.hs idEq)
-- and equal strings intern to one SString, so two Ids that differ under
-- Eq differ under 'nameKey': the keys of one map never tie, and a ByName
-- list is a permutation of the blind one, with no tie-break needed.
-- Eq-equal Ids (same name, different position or props) DO tie, and
-- every sort here is stable, so they keep their input order.
module IdOrd(
    nameKey,
    cmpByName, cmpPairByName, cmpOnByName,
    sortByName, sortOnByName, nubByName,
    cmpPositionByName,
    AuditKey
    ) where

import qualified Data.List as L
import Data.Ord(comparing)
import qualified Data.Set as S

import Id(Id, getIdBaseString, getIdQualString)
import Position(Position, getPositionFile, getPositionLine, getPositionColumn)

-- | The ordered name: base first, then the qualifier.
nameKey :: Id -> (String, String)
nameKey i = (getIdBaseString i, getIdQualString i)
{-# INLINE nameKey #-}

cmpByName :: Id -> Id -> Ordering
cmpByName = comparing nameKey
{-# INLINE cmpByName #-}

-- | Lexicographic over 'cmpByName'; for (ARuleId, ARuleId) and
-- (AId, AId) output sites.
cmpPairByName :: (Id, Id) -> (Id, Id) -> Ordering
cmpPairByName (a1, a2) (b1, b2) = cmpByName a1 b1 <> cmpByName a2 b2
{-# INLINE cmpPairByName #-}

cmpOnByName :: (a -> Id) -> a -> a -> Ordering
cmpOnByName f x y = cmpByName (f x) (f y)
{-# INLINE cmpOnByName #-}

-- | Stable: Eq-equal duplicates keep their input order.
-- (sortOn computes the key once per element; the result is that of
-- @sortBy cmpByName@.)
sortByName :: [Id] -> [Id]
sortByName = L.sortOn nameKey

sortOnByName :: (a -> Id) -> [a] -> [a]
sortOnByName f = L.sortOn (nameKey . f)

-- | The FIRST occurrence of each Eq class, in name order.
-- Util.fastNub (@S.toList . S.fromList@) keeps the LAST inserted
-- representative and returns intern order; a site whose kept Id's
-- position or props reach an output must check which one it wants.
nubByName :: [Id] -> [Id]
nubByName = sortByName . firstOccurrences
  where
    firstOccurrences = go S.empty
    go _ [] = []
    go seen (i:is)
        | S.member i seen = go seen is
        | otherwise       = i : go (S.insert i seen) is

-- | File name as a String, then line, then column. @Ord Position@
-- compares the file's FString, i.e. intern order; this is its
-- text-ordered twin for the Position-ordered emission sites.
cmpPositionByName :: Position -> Position -> Ordering
cmpPositionByName p q =
    comparing getPositionFile p q
    <> comparing getPositionLine p q
    <> comparing getPositionColumn p q

-- | The IDMAP_AUDIT hook. This class has no instances. Under
-- -DIDMAP_AUDIT every blind enumeration function of IdMap and IdSet
-- (tier B: toList, keys, elems, the folds, mapAccum*, traverseWithKey,
-- the monadic twins) carries an @AuditKey Id@ constraint, so a build
-- with -fdefer-type-errors reports "No instance for (AuditKey Id)" at
-- every blind site, one build for the whole program:
--
--   make -C src/comp bsc BSC_BUILD=NOOPT GHC='ghc -fdefer-type-errors -DIDMAP_AUDIT'
--
-- GHC reports one such diagnostic per distinct unsolved constraint per
-- binding, so a function with several blind calls is named once. A
-- module whose OPTIONS_GHC pragma says -Werror stops that build at its
-- deferred errors (the pragma is processed after the command line, so a
-- command-line -Wwarn=deferred-type-errors does not reach it);
-- BinData.hs, whose Bin IdMap/IdSet instances are blind until plan
-- P4(6), carries a CPP-conditional -Wwarn=deferred-type-errors pragma
-- for this build. AVerilogUtil, Error, GenABin and GenBin also say
-- -Werror and need the same two lines if they acquire a blind site.
class AuditKey k
