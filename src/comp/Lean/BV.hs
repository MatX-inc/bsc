-- | Bit-vector sorts of bsc values, as Lean names them.
module Lean.BV
    ( BVSort(..), bvSortOf, bvSortName
    ) where

import ASyntax (AType(..))

-- | The sort of a bsc value of a given type.
data BVSort = BVBool | BVBits Integer | BVString | BVUnit
    deriving (Eq, Ord, Show)

bvSortOf :: AType -> BVSort
bvSortOf t = case t of
    ATBit 0    -> BVUnit
    ATBit 1    -> BVBool
    ATBit n    -> BVBits n
    ATString _ -> BVString
    _          -> BVBits (aSizeOf t)
  where aSizeOf (ATTuple ts) = sum (map aSizeOf ts)
        aSizeOf (ATArray n e) = n * aSizeOf e
        aSizeOf (ATBit n) = n
        aSizeOf _ = 0

bvSortName :: BVSort -> String
bvSortName s = case s of
    BVBits n -> "Tok" ++ show n
    BVString -> "TokStr"
    _        -> "Bool"
