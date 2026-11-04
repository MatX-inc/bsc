-- | The gate's bounds (mke2e.py's GATE_*), calibrated by the cost probes;
-- @--limit NAME=N@ may only lower one.
module Lean.Sched.Bounds
    ( Bounds(..), defaultBounds, applyLimits, boundNames
    ) where

data Bounds = Bounds
    { bnGateEsc, bnGateEscFacts, bnGateSeg, bnGateSlots, bnGateCodes, bnGateStep, bnGatePortUsers, bnGateGroup
    , bnGateSelmux, bnGateLcUsers, bnGatePart, bnGateStruct, bnGateModFacts, bnGateMuxPairs, bnGateFanIn :: Int }

defaultBounds :: Bounds
-- (mux pairs: members proved exclusive one by one, at most the pairwise port's 64 users' 2,016; a step's fan-in:
-- its sites' operator inputs, which each site restates, at the measured knee of 256)
defaultBounds = Bounds 64 4096 4096 8192 4096 4096 3296 3296 64 512 4000000 128 16384 2016 256

-- | Each bound's --limit name, with its reader and writer.
boundNames :: [(String, (Bounds -> Int, Int -> Bounds -> Bounds))]
boundNames =
    [ ("esc", (bnGateEsc, \v b -> b { bnGateEsc = v })), ("esc-facts", (bnGateEscFacts, \v b -> b { bnGateEscFacts = v }))
    , ("seg-uses", (bnGateSeg, \v b -> b { bnGateSeg = v })), ("slots", (bnGateSlots, \v b -> b { bnGateSlots = v }))
    , ("codes", (bnGateCodes, \v b -> b { bnGateCodes = v })), ("step", (bnGateStep, \v b -> b { bnGateStep = v }))
    , ("port-users", (bnGatePortUsers, \v b -> b { bnGatePortUsers = v })), ("group", (bnGateGroup, \v b -> b { bnGateGroup = v }))
    , ("selmux", (bnGateSelmux, \v b -> b { bnGateSelmux = v })), ("lc-users", (bnGateLcUsers, \v b -> b { bnGateLcUsers = v }))
    , ("part-bytes", (bnGatePart, \v b -> b { bnGatePart = v })), ("struct", (bnGateStruct, \v b -> b { bnGateStruct = v }))
    , ("mod-facts", (bnGateModFacts, \v b -> b { bnGateModFacts = v }))
    , ("mux-pairs", (bnGateMuxPairs, \v b -> b { bnGateMuxPairs = v }))
    , ("step-fanin", (bnGateFanIn, \v b -> b { bnGateFanIn = v })) ]

-- | Lower the named bounds; a name not known, or a value above the bound, is an error.
applyLimits :: [(String, Int)] -> Bounds -> Either String Bounds
applyLimits ls b0 = foldl step (Right b0) ls
  where
    step (Left e) _ = Left e
    step (Right b) (n, v) = case lookup n boundNames of
        Nothing -> Left ("--limit: no bound named " ++ n ++ " (known: " ++ unwords (map fst boundNames) ++ ")")
        Just (get, set) | v > get b -> Left ("--limit " ++ n ++ "=" ++ show v ++ " would raise the bound " ++ show (get b))
                        | otherwise -> Right (set v b)
