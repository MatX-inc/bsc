-- | The type-sharing flag story, in one module so the hooks cannot
-- drift apart (previously the same flag string was read by multiple
-- independent CAFs).
--
-- One master opt-in, default off: @-share-types@ is the full
-- exponential-killing world -- construction-time interning of ground
-- normal forms on the CType side (the certificate), the boundary
-- interner + conversion memo, the solve-time ground-dictionary pool,
-- and every guard and memo keyed on the certificate.  The legacy
-- measurement flags (@-hack-ctype-cons@, @-hack-ground-ctype@) remain
-- as aliases during the A/B era and will be deleted at graduation.
--
-- Beneath the master, subtractive BACKOFFS, one per soundness
-- argument, for bug bisection only: each disables one class of
-- accelerations while the system stays correct (slower).  When a
-- miscompile is suspected, turning backoffs on one at a time
-- identifies the soundness claim that failed:
--   -share-types-no-identity  equal-id shortcuts (cmp, mgu, match)
--                             [claim: one canonical object per key]
--   -share-types-no-ground    ground guards (apSub, tv, inst, getFreeT)
--                             [claim: canonical => ground]
--   -share-types-no-normal    normal-form guards (expandSyn, expandSynN,
--                             expTFun, chkTAp, badCon)
--                             [claim: canonical => nothing to reduce]
--   -share-types-no-memo      context-free memos (nullary-synonym
--                             expansion, checked-clean set, per-id
--                             conversion) [claim: context-freeness]
--   -share-types-no-pool      the ground-dictionary pool
--                             [claim: coherence gate]
--
-- The IType side is graduated (interning is unconditional), so it has
-- no master flag; @-hack-no-itype-walk-memos@ is its layer-0
-- diagnostic in the style of @-hack-no-itype-ftv-cache@, disabling
-- the newer unconditional accelerations (kind-check memo, node-
-- identity equality shortcut, ATF-presence skip) for bisection.  Both
-- diagnostics are expected to be deleted once the machinery has aged.
module TypeShareFlags(
    shareTypes, shareTypesBoundary, shareTypesPool, consTypesEnabled,
    useIdentityShortcuts, useGroundGuards, useNormalGuards, useShareMemos,
    useBinShareIds, useBoundaryWalkMemo,
    noITypeWalkMemos,
    typeShareStatsEnabled,
  ) where

import IOUtil(progArgs)
import Data.Bits(finiteBitSize)

has :: String -> Bool
has f = f `elem` progArgs

-- the master needs 64-bit Ints for the fused intern keys; on narrower
-- platforms it is off rather than silently degraded
{-# NOINLINE shareTypes #-}
shareTypes :: Bool
shareTypes = (has "-share-types" || has "-hack-ctype-cons")
             && finiteBitSize (0 :: Int) >= 64

-- lever 2 (boundary interner, conversion memo, pool activation) is
-- implied by the master and independently reachable via its legacy
-- alias
{-# NOINLINE shareTypesBoundary #-}
shareTypesBoundary :: Bool
shareTypesBoundary = shareTypes || has "-hack-ground-ctype"

{-# NOINLINE shareTypesPool #-}
shareTypesPool :: Bool
shareTypesPool = shareTypesBoundary && not (has "-share-types-no-pool")

-- construction-time interning itself can back off (leaving the
-- boundary world -- lever 2 -- running under the master), the
-- coarsest bisection step of all
{-# NOINLINE consTypesEnabled #-}
consTypesEnabled :: Bool
consTypesEnabled = shareTypes && not (has "-share-types-no-cons")

-- .bo writer share-map keys: id-keyed (TKI) normally; the backoff
-- falls back to the old deep structural keys so a suspected .bo
-- issue can be isolated from everything else
{-# NOINLINE useBinShareIds #-}
useBinShareIds :: Bool
useBinShareIds = shareTypes && not (has "-share-types-no-bin")

-- the boundary interner's per-walked-object memo (a lever-2 fix that
-- predates the master; independent of it, but bisectable)
{-# NOINLINE useBoundaryWalkMemo #-}
useBoundaryWalkMemo :: Bool
useBoundaryWalkMemo = not (has "-share-types-no-memo")

{-# NOINLINE useIdentityShortcuts #-}
useIdentityShortcuts :: Bool
useIdentityShortcuts = shareTypes && not (has "-share-types-no-identity")

{-# NOINLINE useGroundGuards #-}
useGroundGuards :: Bool
useGroundGuards = shareTypes && not (has "-share-types-no-ground")

{-# NOINLINE useNormalGuards #-}
useNormalGuards :: Bool
useNormalGuards = shareTypes && not (has "-share-types-no-normal")

{-# NOINLINE useShareMemos #-}
useShareMemos :: Bool
useShareMemos = shareTypes && not (has "-share-types-no-memo")

{-# NOINLINE noITypeWalkMemos #-}
noITypeWalkMemos :: Bool
noITypeWalkMemos = has "-hack-no-itype-walk-memos"

{-# NOINLINE typeShareStatsEnabled #-}
typeShareStatsEnabled :: Bool
typeShareStatsEnabled = has "-trace-ctype-stats"
