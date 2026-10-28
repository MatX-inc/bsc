-- Independent accounting of the eligible candidate relation and emitted names.
-- This does not reuse assignment scoring or claim that a chosen surplus node
-- has no possible counterpart: only the type component has a cardinality limit.
module ECOAudit (AuditCheck(..), checkAssignmentAudit) where

import Control.Monad (unless, forM_)
import Data.List (sort)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import ECO.Types
import ECOAggressive (AssignmentAudit(..), TypeGroup(..), Disposition(..))
import ECO.Validate (validateGraph)

data AuditCheck = AuditCheck
    { checkedOld :: Int, checkedNew :: Int, checkedPairs :: Int
    , checkedProtected :: Int, checkedTypes :: Int
    } deriving (Eq,Read,Show)

checkAssignmentAudit :: EcoGraph -> EcoGraph -> AssignmentAudit ->
                        [(String,String)] -> Either String AuditCheck
checkAssignmentAudit old new audit renames = do
    validateGraph old; validateGraph new
    need (egModule old == egModule new && egStage old == egStage new) "graph module/stage mismatch"
    checkRows "old" os (auditOld audit)
    checkRows "new" ns (auditNew audit)
    forM_ (M.toList ods) $ \(key,row) -> do
        checkRow os nds ns key row
        if key `S.member` blocked
          then need (dispositionStatus row == "protected-target" && dispositionCounterpart row == Nothing)
                    "protected old target was not excluded"
          else need (dispositionStatus row /= "protected-target") "false protected old target"
    forM_ (M.toList nds) $ \(key,row) -> do
        checkRow ns ods os key row
        need (dispositionStatus row /= "protected-target") "new target marked protected"
    need (unique (map groupType (auditGroups audit)) &&
          S.fromList (map groupType (auditGroups audit)) == types) "incomplete/duplicate type groups"
    forM_ (auditGroups audit) $ \group -> do
        let ty = groupType group
            as = [k | (k,n) <- M.toList os, enType n == ty, k `S.notMember` blocked]
            bs = [k | (k,n) <- M.toList ns, enType n == ty]
            unmatched rows keys = [k | k <- keys, dispositionStatus (rows M.! k) /= "matched"]
            au = unmatched ods as; bu = unmatched nds bs
            maximumPairs = min (length as) (length bs)
        need (sort (groupOld group) == as && sort (groupNew group) == bs) "incorrect type candidate membership"
        need (sort (groupUnmatchedOld group) == au && sort (groupUnmatchedNew group) == bu)
             "incorrect unmatched type witness"
        need (groupMatched group == maximumPairs && length as - length au == maximumPairs &&
              length bs - length bu == maximumPairs) "typed component is not maximally matched"
    need (unique (map fst renames) && all ((`S.member` newNames) . fst) renames)
         "duplicate or noneligible rename source"
    let output node = M.findWithDefault (name node) (name node) mapping
        outputs = map output (M.elems ns)
    need (unique outputs && all (`S.notMember` protected) outputs) "colliding name allocation"
    forM_ (M.toList ns) $ \(key,node) -> do
        let row = nds M.! key
        case dispositionCounterpart row of
          Just mate -> need (output node == name (os M.! mate)) "historical name was not allocated"
          Nothing -> need (output node `S.notMember` historical) "surplus node claims historical name"
    return $ AuditCheck (M.size os) (M.size ns)
                       (length [() | row <- M.elems nds, dispositionStatus row == "matched"])
                       (S.size blocked) (S.size types)
  where
    need condition message = unless condition (Left ("ECO match audit: " ++ message))
    unique xs = length xs == S.size (S.fromList xs)
    name = fromMaybe "" . enName
    eligible graph = M.fromList [(enKey n,n) | n <- egNodes graph, enEligible n]
    os = eligible old; ns = eligible new
    ods = M.fromList [(dispositionKey d,d) | d <- auditOld audit]
    nds = M.fromList [(dispositionKey d,d) | d <- auditNew audit]
    newNames = S.fromList (map name (M.elems ns))
    protected = S.fromList (egReserved new) S.\\ newNames
    blocked = M.keysSet (M.filter ((`S.member` protected) . name) os)
    types = S.fromList ([enType n | (k,n) <- M.toList os, k `S.notMember` blocked] ++ map enType (M.elems ns))
    mapping = M.fromList renames
    historical = S.fromList (egReserved old ++ [s | n <- egNodes old, Just s <- [enName n]])
    checkRows side nodes rows = do
        need (unique (map dispositionKey rows) && S.fromList (map dispositionKey rows) == M.keysSet nodes)
             ("incomplete eligible accounting: " ++ side)
    checkRow nodes oppositeRows opposite key row = do
        let node = nodes M.! key
        need (dispositionName row == name node && dispositionType row == enType node)
             "audit node identity mismatch"
        need (not (null (dispositionEvidence row))) "missing disposition evidence"
        case (dispositionStatus row,dispositionCounterpart row) of
          ("matched",Just mate) -> do
            target <- maybe (Left "ECO match audit: unknown counterpart") Right (M.lookup mate opposite)
            other <- maybe (Left "ECO match audit: missing counterpart disposition") Right (M.lookup mate oppositeRows)
            need (enType node == enType target && dispositionStatus other == "matched" &&
                  dispositionCounterpart other == Just key) "cross-type or asymmetric match"
          (status,Nothing) | status `elem` ["type-surplus","protected-target"] -> return ()
          _ -> Left "ECO match audit: invalid disposition"
