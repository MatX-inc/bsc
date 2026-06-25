module TypeOps(opNumT, numOpNames, opStrT, strOpNames, isTypeFunOp) where
-- common routines for handling numeric and string types

import qualified Data.Set as S
import Id
import PreIds(idTAdd, idTSub, idTMul, idTDiv, idTLog, idTExp, idTMax, idTMin, idTStrCat, idTNumToStr)
import Util(divC, log2)
import FStringCompat(FString, concatFString)

-- do a numeric type operation on a list of arguments
-- note that we have to validate that the result is going to
-- to be >= 0 - otherwise it isn't a valid numeric type
opNumT :: Id -> [Integer] -> Maybe Integer
opNumT i [x, y] | i == idTAdd = Just (x + y)
opNumT i [x, y] | i == idTSub && x >= y = Just (x - y)
opNumT i [x, y] | i == idTMul = Just (x * y)
opNumT i [x, y] | i == idTDiv && y /= 0 = Just (divC x y)
opNumT i [x]    | i == idTExp = Just (2^x)
opNumT i [x]    | i == idTLog && x /= 0 = Just (log2 x)
opNumT i [x, y] | i == idTMax = Just (max x y)
opNumT i [x, y] | i == idTMin = Just (min x y)
opNumT _ _      = Nothing

numOpNames :: [Id]
numOpNames = [idTAdd, idTSub, idTMul, idTDiv, idTExp, idTLog, idTMax, idTMin, idTNumToStr]

opStrT :: Id -> [FString] -> Maybe FString
opStrT i xs | i == idTStrCat = Just $ concatFString xs
opStrT _ _ = Nothing

strOpNames :: [Id]
strOpNames = [idTStrCat]

-- Membership test for the (small, fixed) set of numeric/string type-function
-- operator names.  Precomputed as a Set so the check is O(log n) per call
-- instead of rebuilding `numOpNames ++ strOpNames` and doing a linear `elem`
-- with idEq on every call -- this is extremely hot (called per type-constructor
-- during type expansion/normalization).
typeFunOpNameSet :: S.Set Id
typeFunOpNameSet = S.fromList (numOpNames ++ strOpNames)

isTypeFunOp :: Id -> Bool
isTypeFunOp i = i `S.member` typeFunOpNameSet
