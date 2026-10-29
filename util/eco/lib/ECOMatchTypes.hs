-- External correspondence records; native compiler captures contain no matching policy.
module ECOMatchTypes where

import ECO.Types (EcoKey)

data EcoMatchKind = ExactContent | ChangedRole
    deriving (Eq, Ord, Read, Show)

data EcoCorrespondence = EcoCorrespondence
    { ecOld :: EcoKey
    , ecNew :: EcoKey
    , ecKind :: EcoMatchKind
    , ecReason :: String
    } deriving (Eq, Ord, Read, Show)

data EcoMatchResult = EcoMatchResult
    { emCorrespondences :: [EcoCorrespondence]
    , emUnmatchedOld :: [EcoKey]
    , emUnmatchedNew :: [EcoKey]
    , emAmbiguousBuckets :: Int
    } deriving (Eq, Read, Show)

-- Pure matcher output, checked independently against the emitted module.
-- Captures are immutable inputs; changing matcher code does not require
-- loading the old compiler's native data or restoring its intern table.
data EcoRenamePlan = EcoRenamePlan
    { erpSchema :: Int
    , erpModule :: String
    , erpRenames :: [(String,String)]
    } deriving (Eq, Read, Show)
