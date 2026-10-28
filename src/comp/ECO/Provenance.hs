-- Observation-only companion data from the expansion heap. Keep only pointer
-- identities and existing Ids: never retain expressions, create Ids or choose a
-- different CSE representative. Render names after ordinary output/snapshot.
module ECO.Provenance
    ( ExpansionAliases, captureExpansionAliases, renderExpansionAliases ) where

import Control.Exception (evaluate)
import Data.Char (ord)
import Data.List (intercalate)
import Numeric (showHex)
import Id (Id, getIdString, getIdPosition)
import ECO.Options (ECOOptions, ecoCapture)

data Alias = Alias !Int !(Maybe Id) !Int

-- Owned by one expansion invocation, including when another module has the
-- same name or compilation stops before the observation is emitted. There is
-- no process-wide slot to overwrite, consume or leave behind after failure.
data ExpansionAliases = ExpansionUnavailable | ExpansionCaptured [Alias]

captureExpansionAliases :: ECOOptions -> [(Int,Maybe Id,Int)] -> IO ExpansionAliases
captureExpansionAliases opts rows =
    case ecoCapture opts of
        Nothing -> return ExpansionUnavailable
        Just _ -> do
            let compact = [Alias pointer name representative | (pointer,name,representative) <- rows]
            -- Detach the small observation records from the live heap without
            -- forcing an Id's spelling or allocating any compiler strings.
            mapM_ evaluate compact
            return (ExpansionCaptured compact)

-- Call only after ordinary output and the native snapshot have been forced.
-- Names and source positions are rendered here, not while detaching the heap.
renderExpansionAliases :: ExpansionAliases -> String
renderExpansionAliases ExpansionUnavailable =
    "{\"stage\":\"IExpand.eqPtrs\",\"availability\":\"unavailable\"}"
renderExpansionAliases (ExpansionCaptured rows) =
    "{\"stage\":\"IExpand.eqPtrs\",\"availability\":\"captured\"," ++
    "\"classification\":\"compiler-derived-heap-equivalence\"," ++
    "\"bindingStatus\":\"pre-lowering roles; final typed RTL binding requires reproof\"," ++
    "\"scope\":\"reachable expansion heap references; some are inlined rather than emitted as definitions\"," ++
    "\"rows\":[" ++ intercalate "," (map row rows) ++ "]}"
  where
    row (Alias pointer name representative) = "{\"pointer\":" ++ show pointer ++
        ",\"representative\":" ++ show representative ++
        ",\"name\":" ++ maybe "null" (json . getIdString) name ++
        ",\"sourcePosition\":" ++ maybe "null" (json . show . getIdPosition) name ++ "}"
    json str = '"' : concatMap escape str ++ "\""
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape c | ord c < 32 = let h = showHex (ord c) ""
                            in "\\u" ++ replicate (4 - length h) '0' ++ h
             | otherwise = [c]
