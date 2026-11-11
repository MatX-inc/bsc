-- The capture's hooks on the Verilog backend's ASPackage passes
-- (CaptureLive).  aspkg_capture holds the names merged after amod_final,
-- keyed by this package's names; CaptureFreeze joins it to the apkg and
-- to the Verilog identifiers.
--
-- Every hook reads only the pass's input and output packages, which the
-- previous and the current stage's dumps deepseq'd ahead of the capture
-- field, so it forces nothing production did not.
module CaptureHooks(
                    netStep, netStepJoined, netStepCreated, netStepFrom,
                    netStepDropped,
                    netDiff, netRenames
                   ) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List(foldl')

import ASyntax
import ASyntaxUtil(aVars)
import CaptureLive

-- A pass from one ASPackage to the next.
netStep :: Int -> ASPackage -> ASPackage -> ASPackage
netStep stage = netStepJoined stage M.empty

-- As netStep, given the defs joinDefs pointed at an equal def.
netStepJoined :: Int -> M.Map AId AId -> ASPackage -> ASPackage -> ASPackage
netStepJoined stage joined pin pout =
    pout { aspkg_capture =
             Cap (netDiff stage joined (aspkg_values pin) (outIds pout)
                          (aspUsers pin) (capOf (aspkg_capture pin))) }

-- As netStep, and each def the pass made records the defs that use it
-- (the def whose value it was cut from).
netStepCreated :: Int -> ASPackage -> ASPackage -> ASPackage
netStepCreated stage pin pout =
    let pout' = netStep stage pin pout
        ins = S.fromList [ i | ADef i _ _ _ <- aspkg_values pin ]
        new = S.fromList [ i | ADef i _ _ _ <- aspkg_values pout,
                               not (S.member i ins) ]
        parents | S.null new = []
                | otherwise = [ (c, NameFact (DefName u) viaCreated stage)
                              | ADef u _ e _ <- aspkg_values pout,
                                c <- aVars e, S.member c new ]
        t = foldl' (\t' (c, f) -> capMergeFacts c [f] t')
                   (capOf (aspkg_capture pout')) parents
    in  pout' { aspkg_capture = Cap (capStat stage viaCreated (S.size new) t) }

-- A pass that only drops unused defs (aDropDefs): every def the output
-- lacks is dead.
netStepDropped :: Int -> ASPackage -> ASPackage -> ASPackage
netStepDropped stage pin pout =
    let outs = outIds pout
        gone = dedup S.empty [ i | ADef i _ _ _ <- aspkg_values pin,
                                   not (S.member i outs) ]
        dedup _ [] = []
        dedup seen (i:is) | S.member i seen = dedup seen is
                          | otherwise = i : dedup (S.insert i seen) is
        t = foldl' (\t' i -> capGone kindDead stage viaDrop i [] t')
                   (capOf (aspkg_capture pin)) gone
    in  pout { aspkg_capture = Cap t }

-- AState: the APackage's defs to the ASPackage's, onto the table AState
-- started (its boundary links).
netStepFrom :: Int -> APackage -> ASPackage -> ASPackage
netStepFrom stage apkg pout =
    let users = [ (HLId i, aVars e) | ADef i _ e _ <- apkg_local_defs apkg ] ++
                [ (HLId (avi_vname v), aVars v) | v <- apkg_state_instances apkg ] ++
                [ (HLId (arule_id r), aVars r) | r <- apkg_rules apkg ] ++
                [ (HLId (aif_name f), aVars f) | f <- apkg_interface apkg ]
    in  pout { aspkg_capture =
                 Cap (netDiff stage M.empty (apkg_local_defs apkg) (outIds pout)
                              users (capOf (aspkg_capture pout))) }

outIds :: ASPackage -> S.Set AId
outIds p = S.fromList [ i | ADef i _ _ _ <- aspkg_values p ]

-- the things that can host an inlined def, with the defs they use
aspUsers :: ASPackage -> [(HostL, [AId])]
aspUsers p =
    [ (HLId i, aVars e) | ADef i _ e _ <- aspkg_values p ++ aspkg_inout_values p ] ++
    [ (HLId (avi_vname v), aVars v) | v <- aspkg_state_instances p ] ++
    [ (HLId (afc_name fc), aVars fc) | (_, fcs) <- aspkg_foreign_calls p, fc <- fcs ]

-- The defs of the input that the output lacks, by their input bodies: an
-- alias of the def it names (or, for the given joined defs, of the def
-- joinDefs pointed it at), a port, parameter or constant, inlined into
-- the defs, instances and foreign calls that used it, or dead.
netDiff :: Int -> M.Map AId AId -> [ADef] -> S.Set AId -> [(HostL, [AId])] ->
           CapTable -> CapTable
netDiff stage joined ins outs users t0 =
    let gone = dedup S.empty [ d | d@(ADef i _ _ _) <- ins, not (S.member i outs) ]
        dedup _ [] = []
        dedup seen (d@(ADef i _ _ _) : ds)
            | S.member i seen = dedup seen ds
            | otherwise = d : dedup (S.insert i seen) ds
        aliasOf = M.fromList ([ (i, j) | ADef i _ (ASDef _ j) _ <- gone ] ++
                              M.toList joined)
        -- an alias of an alias that also went: the one that stayed
        resolve :: Int -> AId -> AId
        resolve n j | n > 64 = j
                    | otherwise = case M.lookup j aliasOf of
                                    Just k | k /= j -> resolve (n + 1) k
                                    _ -> j
        inners = S.fromList [ i | ADef i _ e _ <- gone,
                                  not (M.member i joined), not (isAtom e) ]
        hosts | S.null inners = M.empty
              | otherwise = M.map reverse $ M.fromListWith (++)
                              [ (r, [h]) | (h, rs) <- users, r <- rs,
                                           S.member r inners ]
        hostKey (HLId i) = i
        hostKey (HLInst i _) = i
        dedupHosts _ [] = []
        dedupHosts seen (h:hs)
            | S.member (hostKey h) seen = dedupHosts seen hs
            | otherwise = h : dedupHosts (S.insert (hostKey h) seen) hs
        -- the merges first, so that a name merged into a def that went
        -- too leaves with that def's record
        merge t (ADef i _ e _) =
            case (M.lookup i joined, e) of
              (Just j, _) -> capMerge stage viaJoin (resolve 0 j) [i] t
              (_, ASDef _ j) -> capMerge stage viaAlias (resolve 0 j) [i] t
              (_, ASPort _ p) -> capMerge stage viaAtom p [i] t
              (_, ASParam _ p) -> capMerge stage viaAtom p [i] t
              _ -> t
        leave t (ADef i _ e _)
            | M.member i joined || isMerge e = t
            | isConst e = capGone kindConst stage viaAtom i [] t
            | otherwise =
                case M.lookup i hosts of
                  Just hs -> capGone kindInner stage viaInline i
                                     (dedupHosts S.empty hs) t
                  Nothing -> capGone kindDead stage viaDrop i [] t
    in  foldl' leave (foldl' merge t0 gone) gone
  where isConst (ASInt {}) = True
        isConst (ASReal {}) = True
        isConst (ASStr {}) = True
        isConst (ASAny {}) = True
        isConst _ = False
        isMerge (ASDef {}) = True
        isMerge (ASPort {}) = True
        isMerge (ASParam {}) = True
        isMerge _ = False
        isAtom e = isMerge e || isConst e

-- Renamed defs (old, new): each record moves to the new name.
netRenames :: Int -> [(AId, AId)] -> CapTable -> CapTable
netRenames stage rs t0 = foldl' (\t (o, n) -> capRename stage viaRename o n t) t0 rs
