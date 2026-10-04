-- | Concrete IR changes chosen while lowering a scheduled module. These
-- results belong to the compiled module: reading it must not choose new
-- undetermined values or new noinline names from the reader's options.
module AMaterializePatch
    ( AOrderedPatch(..)
    , AMaterializePatch(..)
    , diffAOrderedPatch
    , applyAMaterializePatch
    ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (foldl')

import ASyntax
    ( APackage(..), ADef(..), ARule(..), AIFace(..), AVInst(..) )
import Backend (Backend)
import Id (Id)

-- | Unchanged entries are shared with the input. An explicit output order
-- also describes deletions; Nothing preserves the input order. Update keys
-- are taken from their payloads, so a key cannot disagree with its value.
data AOrderedPatch a = AOrderedPatch
    { aop_order :: Maybe [Id]
    , aop_updates :: [a]
    } deriving (Eq, Show)

data AMaterializePatch = AMaterializePatch
    { amp_module :: Id
    , amp_backend :: Maybe Backend
    , amp_defs :: AOrderedPatch ADef
    , amp_rules :: AOrderedPatch ARule
    , amp_interface :: AOrderedPatch AIFace
    , amp_instances :: AOrderedPatch AVInst
    } deriving (Eq, Show)

-- | The caller supplies exact equality. The artifact codec compares the
-- encodings because ASyntax's Eq deliberately ignores some source metadata.
diffAOrderedPatch :: (a -> Id) -> (a -> a -> Bool) -> [a] -> [a] ->
                     Either String (AOrderedPatch a)
diffAOrderedPatch key same before after = do
    unique "input" beforeIds
    unique "output" afterIds
    let old = M.fromList [(key value, value) | value <- before]
        changed value = case M.lookup (key value) old of
            Nothing -> True
            Just previous -> not (same previous value)
    return AOrderedPatch
        { aop_order = if beforeIds == afterIds then Nothing else Just afterIds
        , aop_updates = filter changed after
        }
  where
    beforeIds = map key before
    afterIds = map key after

applyAMaterializePatch :: APackage -> AMaterializePatch -> Either String APackage
applyAMaterializePatch before patch = do
    ensure (apkg_name before == amp_module patch)
        "materialization patch belongs to a different module"
    ensure (null (apkg_proof_obligations before))
        "cannot materialize a module with pending proof obligations"
    defs <- applyOrdered "definition" adef_objid (apkg_local_defs before)
                (amp_defs patch)
    rules <- applyOrdered "rule" arule_id (apkg_rules before) (amp_rules patch)
    ifc <- applyOrdered "interface" aif_name (apkg_interface before)
                (amp_interface patch)
    insts <- applyOrdered "instance" avi_vname (apkg_state_instances before)
                (amp_instances patch)
    return before
        { apkg_backend = amp_backend patch
        , apkg_local_defs = defs
        , apkg_rules = rules
        , apkg_interface = ifc
        , apkg_state_instances = insts
        }

applyOrdered :: String -> (a -> Id) -> [a] -> AOrderedPatch a -> Either String [a]
applyOrdered kind key before patch = do
    unique ("input " ++ kind) beforeIds
    unique ("output " ++ kind) order
    unique ("updated " ++ kind) updateIds
    ensure (all (`S.member` outputIds) updateIds)
        ("materialization updates an absent " ++ kind)
    -- A strict fold avoids retaining one Either bind per entry in large
    -- designs. Maps are constructed once, not once per replacement.
    reverse <$> foldl' resolve (Right []) order
  where
    beforeIds = map key before
    updates = aop_updates patch
    updateIds = map key updates
    order = maybe beforeIds id (aop_order patch)
    outputIds = S.fromList order
    values = M.union (M.fromList [(key value, value) | value <- updates])
                     (M.fromList [(key value, value) | value <- before])
    resolve result name = do
        accumulated <- result
        case M.lookup name values of
            Nothing -> Left ("materialization refers to an unknown " ++ kind)
            Just value -> Right (value : accumulated)

unique :: String -> [Id] -> Either String ()
unique what names = ensure (S.size (S.fromList names) == length names)
    ("duplicate " ++ what ++ " in materialization patch")

ensure :: Bool -> String -> Either String ()
ensure True _ = Right ()
ensure False message = Left message
