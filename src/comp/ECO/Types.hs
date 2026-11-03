-- Neutral, versioned observations.  Nothing in this module imports compiler
-- identifiers: native interning handles must never cross capture boundaries.
module ECO.Types where


type EcoKey = String

data EcoGraph = EcoGraph
    { egSchema :: Int
    , egModule :: String
    , egStage :: String
    , egNodes :: [EcoNode]
    , egReserved :: [String]
    } deriving (Eq, Read, Show)

data EcoNode = EcoNode
    { enKey :: EcoKey
    , enName :: Maybe String
    , enType :: String
    , enOp :: String
    , enOperands :: [EcoKey]
    , enBoundary :: Maybe String
    , enEligible :: Bool
    , enContexts :: [String]
    , enOpaque :: Bool
    } deriving (Eq, Ord, Read, Show)

-- A structural request is bound to one exact native capture and one complete
-- typed pre-optimization graph. Keeping a definition changes no expression;
-- it asks the existing backend to retain its materialized helper.
data EcoBackendPlan = EcoBackendPlan
    { ebpSchema :: Int
    , ebpModule :: String
    , ebpCaptureSHA256 :: String
    , ebpPreOptSHA256 :: String
    , ebpKeepDefinitions :: [(String, String, EcoKey)]
    } deriving (Eq, Read, Show)

ecoGraphSchema :: Int
ecoGraphSchema = 1
