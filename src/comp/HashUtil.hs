-- profiling-only exemption: tiny leaf mixing functions called at
-- extreme frequency; an SCC here blocks their inlining and their time
-- belongs to callers
{-# OPTIONS_GHC -fno-prof-auto #-}

-- Word64 mixing helpers for the content hashes cached on IType and
-- IExpr nodes (see IType.tyHash and ISyntax.eHash).  The hashes order
-- expression comparisons (hash first, structural walk on equality), so
-- the only requirements are determinism -- a hash must be a pure
-- function of node CONTENT, never of intern ids, heap pointers, or
-- anything else that varies with unrelated input changes -- and decent
-- avalanche so unequal nodes rarely collide (a collision only costs
-- falling back to the structural walk).
module HashUtil(
    Hash,
    hashTag,
    hashMix,
    hashInt,
    hashInteger,
    hashString,
    hashMaybe,
    hashList
) where

import Data.Bits(xor, shiftR)
import Data.Word(Word64)

type Hash = Word64

-- the finalizer from splitmix64: full avalanche, cheap
mix64 :: Word64 -> Word64
mix64 z0 =
    let z1 = (z0 `xor` (z0 `shiftR` 30)) * 0xbf58476d1ce4e5b9
        z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94d049bb133111eb
    in  z2 `xor` (z2 `shiftR` 31)

-- start a hash from a small constructor tag
hashTag :: Int -> Hash
hashTag t = mix64 (fromIntegral t + 0x9e3779b97f4a7c15)

-- fold one word into a running hash
hashMix :: Hash -> Word64 -> Hash
hashMix h x = mix64 (h `xor` (x + 0x9e3779b97f4a7c15 + (h `shiftR` 2)))

hashInt :: Hash -> Int -> Hash
hashInt h i = hashMix h (fromIntegral i)

hashInteger :: Hash -> Integer -> Hash
hashInteger h n0
    | n0 < 0 = go (hashMix h 1) (negate n0)
    | otherwise = go (hashMix h 0) n0
  where go h' n | n < 0x10000000000000000 = hashMix h' (fromIntegral n)
                | otherwise = go (hashMix h' (fromIntegral n)) (n `shiftR` 64)

hashString :: Hash -> String -> Hash
hashString = foldl (\ h c -> hashMix h (fromIntegral (fromEnum c)))

hashMaybe :: (Hash -> a -> Hash) -> Hash -> Maybe a -> Hash
hashMaybe _ h Nothing = hashMix h 11
hashMaybe f h (Just x) = f (hashMix h 13) x

hashList :: (Hash -> a -> Hash) -> Hash -> [a] -> Hash
hashList f h xs = hashMix (foldl f h xs) (fromIntegral (length xs))
