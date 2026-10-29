-- The matcher proposes names; this module independently checks the final
-- emitted namespace before applying one simultaneous map.  It does not intern
-- names, rerun legalization, sort declarations, or alter expressions.
module ECORename (ecoNamespace, ecoEligible, renameInternalSignals) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (intercalate)
import ECO.Namespace (ecoNamespace, ecoEligible)
import Verilog
import VLocalNames (mapRenameableVIds)

renameInternalSignals :: [String] -> [(String, String)] -> VProgram
                      -> Either String VProgram
renameInternalSignals eligible pairs p@(VProgram ms dpis comments) = do
    require (length ms == 1) "ECO renaming requires a single emitted module"
    require (length sources == S.size sourceSet) "duplicate rename source"
    require (length targets == S.size targetSet) "non-injective rename targets"
    require (all (`S.member` eligibleSet) sources) "ineligible rename source"
    require (all valid targets) "invalid, reserved, or hierarchical target"
    require (S.null (targetSet `S.intersection` fixed))
            "rename target collides with a protected or unmoved identifier"
    let ren = M.fromList pairs
        rename v@(VId s i info) = case M.lookup s ren of
            Nothing -> v
            Just s' -> VId s' i info
        ms' = [m { vm_body = mapRenameableVIds rename (vm_body m) } | m <- ms]
    return (VProgram ms' dpis comments)
  where
    sources = map fst pairs
    targets = map snd pairs
    sourceSet = S.fromList sources
    targetSet = S.fromList targets
    eligibleSet = S.fromList (ecoEligible eligible p)
    fixed = S.fromList (ecoNamespace p) `S.difference` sourceSet
    valid s = not (null s) && vIsValidIdent s && head s /= '$' &&
              not (s `elem` vKeywords || s `elem` ["process", "semaphore", "mailbox"]) &&
              not ('.' `elem` s)
    require True _ = Right ()
    require False msg = Left (msg ++ ": " ++ intercalate ", " sources)
