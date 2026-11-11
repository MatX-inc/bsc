{-# LANGUAGE BangPatterns, CPP #-}
-- | A Data.Map lookup that, on a miss, lets the insert of the same key follow
-- the lookup's path instead of comparing the key again.
module MapTurns(Found(..), lookupTurns, insertTurns) where

import Warmup ()
import Data.Bits(setBit, testBit)
import qualified Data.Map.Strict as M
import qualified Data.Map.Internal as MI

-- | The value at the key, or the turns the descent took (bit d set: right
-- at depth d) and the depth of the Tip it reached.
data Found a = Found a | Missing !Word !Int

-- | M.lookup, which compares the key with the same nodes in the same order,
-- and also returns its turns on a miss.  Inlined, so that the caller's case
-- on the result moves into the loop and no Found or Missing is allocated.
lookupTurns :: Ord k => k -> M.Map k a -> Found a
lookupTurns !k = go 0 0
  where go !turns !d MI.Tip = Missing turns d
        go !turns !d (MI.Bin _ ky y l r) =
          case compare k ky of
            LT -> go turns (d + 1) l
            GT -> go (setBit turns d) (d + 1) r
            EQ -> Found y
{-# INLINE lookupTurns #-}

-- | Data.Map.Strict.insert k x m, given the turns of lookupTurns k m's miss,
-- which forced k and made the comparisons: it builds the same tree with the
-- same balanceL/balanceR calls, without them.  This follows Strict.insert's
-- code, the same in containers 0.6.7 through 0.8, where it was tested.  With
-- other containers, and from depth 64, where the turns no longer fit a Word,
-- it is M.insert.
insertTurns :: Ord k => Word -> Int -> k -> a -> M.Map k a -> M.Map k a
insertTurns turns depth k x m
  | not testedContainers || depth >= 64 = M.insert k x m
  | otherwise = go 0 m
  where go !_ MI.Tip = x `seq` MI.singleton k x
        go d (MI.Bin _ ky y l r)
          | testBit turns d = MI.balanceR ky y l (go (d + 1) r)
          | otherwise       = MI.balanceL ky y (go (d + 1) l) r
{-# INLINABLE insertTurns #-}

testedContainers :: Bool
#if MIN_VERSION_containers(0,6,7) && !MIN_VERSION_containers(0,8,1)
testedContainers = True
#else
testedContainers = False
#endif
