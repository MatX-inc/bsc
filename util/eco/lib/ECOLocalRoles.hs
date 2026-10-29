-- Optional correspondence refinement for the standalone matcher. This module
-- observes neutral graphs only; it cannot alter native captures or expressions.
module ECOLocalRoles (refineLocalRoles) where

import ECO.Types
import ECOMatchTypes
import Control.Monad (foldM)
import Data.List (foldl', sortOn)
import Data.Maybe (isJust)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Nodes = M.Map EcoKey EcoNode
data Shape = RefShape String | ExprShape String String (Maybe String) [Int]
    deriving (Eq, Ord)
type Shapes = M.Map Shape Int

-- Anonymous DAG nodes are memoized, and named references terminate traversal.
-- Thus an expression's local shape does not expand its transitive definition DAG.
shapes :: Nodes -> [EcoKey] -> Shapes -> (M.Map EcoKey (Maybe Int), Shapes)
shapes nodes roots table0 = foldl' visit (M.empty,table0) roots
  where
    intern shape table = let i = M.findWithDefault (M.size table) shape table
                         in (i,M.insert shape i table)
    visit state@(memo,table) k
        | M.member k memo = state
        | enOpaque n = (M.insert k Nothing memo,table)
        | otherwise =
            let (children,memo',table') = foldl' child ([],memo,table) (enOperands n)
                (result,table'') = case sequence (reverse children) of
                    Nothing -> (Nothing,table')
                    Just ids -> let (i,t) = intern (ExprShape (enType n) (enOp n) (enBoundary n) ids) table'
                                in (Just i,t)
            in (M.insert k result memo',table'')
      where n = nodes M.! k
    child (ids,memo,table) k
        | isJust (enName n) =
            let (i,t) = intern (RefShape (enType n)) table
            in (Just i:ids,memo,t)
        | otherwise = let (m,t) = visit (memo,table) k
                      in (m M.! k:ids,m,t)
      where n = nodes M.! k

-- A bounded paired traversal records required named-reference correspondences.
-- Repeated DAG pairs are visited once. Unknown references remain constraints,
-- not wildcards that can make a ready pair appear spuriously unique.
constraints :: Nodes -> Nodes -> EcoKey -> EcoKey -> Maybe [(EcoKey,EcoKey)]
constraints os ns old new = do
    (_,forward,_) <- visit (S.empty,M.empty,M.empty) (old,new)
    return (M.toList forward)
  where
    visit state@(seen,fwd,rev) pair@(a,b)
        | S.member pair seen = Just state
        | S.size seen >= 4096 = Nothing
        | enType x /= enType y || enOp x /= enOp y ||
          enBoundary x /= enBoundary y || enOpaque x || enOpaque y ||
          length (enOperands x) /= length (enOperands y) = Nothing
        | otherwise = foldM child (S.insert pair seen,fwd,rev)
                                 (zip (enOperands x) (enOperands y))
      where x = os M.! a; y = ns M.! b
    child state@(seen,fwd,rev) (a,b)
        | enType x /= enType y = Nothing
        | isJust (enName x) && isJust (enName y) =
            if maybe False (/= a) (M.lookup b fwd) ||
               maybe False (/= b) (M.lookup a rev)
            then Nothing else Just (seen,M.insert b a fwd,M.insert a b rev)
        | isJust (enName x) || isJust (enName y) = Nothing
        | otherwise = visit state (a,b)
      where x = os M.! a; y = ns M.! b

data Candidate = Candidate EcoKey EcoKey Int
-- Candidate indices and reference dependencies are bounded within shape buckets.
-- Every binding removes or updates affected candidates through these indices;
-- there are no repeated whole-graph matching rounds.
data Work = Work
    { active :: M.Map Int Candidate
    , byOld :: M.Map EcoKey (S.Set Int)
    , byNew :: M.Map EcoKey (S.Set Int)
    , ready :: S.Set Int
    , accepted :: [(EcoKey,EcoKey)]
    }

refineLocalRoles :: (EcoKey -> EcoKey -> EcoMatchKind) -> EcoGraph -> EcoGraph ->
                    EcoMatchResult -> EcoMatchResult
refineLocalRoles kind old new baseline =
    let final = run initial
        additions = accepted final
        olds = S.fromList (map fst additions)
        news = S.fromList (map snd additions)
        retained = [c | c <- emCorrespondences baseline,
                        S.notMember (ecOld c) olds, S.notMember (ecNew c) news]
        combined = retained ++ [EcoCorrespondence a b (kind a b) "typed-ordered-operand-role" |
                                (a,b) <- additions]
        usedOld = S.fromList (map ecOld combined)
        usedNew = S.fromList (map ecNew combined)
    in baseline { emCorrespondences = sortOn ecNew combined
                , emUnmatchedOld = filter (`S.notMember` usedOld) (M.keys os)
                , emUnmatchedNew = filter (`S.notMember` usedNew) (M.keys ns) }
  where
    os = M.fromList [(enKey n,n) | n <- egNodes old]
    ns = M.fromList [(enKey n,n) | n <- egNodes new]
    weak c = ecReason c == "consumer-operand-role" &&
             isJust (enName (os M.! ecOld c)) /= isJust (enName (ns M.! ecNew c))
    oldClaims = M.fromList [(ecOld c,weak c) | c <- emCorrespondences baseline]
    newClaims = M.fromList [(ecNew c,weak c) | c <- emCorrespondences baseline]
    available claims n = enEligible n && isJust (enName n) &&
                         M.findWithDefault True (enKey n) claims
    oldRoots = [k | (k,n) <- M.toList os, available oldClaims n]
    newRoots = [k | (k,n) <- M.toList ns, available newClaims n]
    known = M.fromList [(ecNew c,ecOld c) | c <- emCorrespondences baseline,
                       isJust (enName (os M.! ecOld c)), isJust (enName (ns M.! ecNew c))]
    reverseKnown = M.fromList [(a,b) | (b,a) <- M.toList known]
    (oldShapes,table) = shapes os oldRoots M.empty
    (newShapes,_) = shapes ns newRoots table
    bucket keys memo = M.fromListWith (++) [(s,[k]) | k <- keys, Just s <- [memo M.! k]]
    groups = M.elems (M.intersectionWith (,) (bucket oldRoots oldShapes) (bucket newRoots newShapes))
    -- The fixed cap bounds comparisons per root. Large ambiguous families
    -- remain unmatched; no heuristic chooses one of their indistinguishable copies.
    possible = [(a,b,cs) | (as,bs) <- groups, length as <= 128, length bs <= 128,
                           a <- as, b <- bs, Just cs <- [constraints os ns a b],
                           all consistent cs]
    consistent (b,a) = maybe True (== a) (M.lookup b known) &&
                       maybe True (== b) (M.lookup a reverseKnown)
    indexed = zip [0..] possible
    initial = Work
        (M.fromList [(i,Candidate a b (length [(x,y) | (x,y) <- cs, M.notMember x known])) |
                     (i,(a,b,cs)) <- indexed])
        (M.fromListWith S.union [(a,S.singleton i) | (i,(a,_,_)) <- indexed])
        (M.fromListWith S.union [(b,S.singleton i) | (i,(_,b,_)) <- indexed])
        (S.fromList [i | (i,(_,_,cs)) <- indexed, all (\(b,_) -> M.member b known) cs]) []
    newDependencies = M.fromListWith (++)
        [(b,[(i,a)]) | (i,(_,_,cs)) <- indexed, (b,a) <- cs, M.notMember b known]
    oldDependencies = M.fromListWith (++)
        [(a,[(i,b)]) | (i,(_,_,cs)) <- indexed, (b,a) <- cs, M.notMember b known]
    run state = case S.minView (ready state) of
        Nothing -> state
        Just (i,rest) ->
            let waiting = state {ready = rest}
            in case M.lookup i (active state) of
                Just (Candidate a b 0)
                    | S.size (byOld state M.! a) == 1 && S.size (byNew state M.! b) == 1 ->
                        let incident = S.union (byOld state M.! a) (byNew state M.! b)
                            removed = foldl' (flip remove) waiting (S.toList incident)
                            added = removed {accepted = (a,b):accepted removed}
                            updated = foldl' (newBinding a) added (M.findWithDefault [] b newDependencies)
                            updated' = foldl' (oldBinding b) updated (M.findWithDefault [] a oldDependencies)
                        in run updated'
                _ -> run waiting
    newBinding a state (i,expected)
        | expected /= a = remove i state
        | otherwise = case M.lookup i (active state) of
            Nothing -> state
            Just (Candidate x y count) ->
                state {active = M.insert i (Candidate x y (count-1)) (active state),
                       ready = if count == 1 then S.insert i (ready state) else ready state}
    oldBinding b state (i,expected)
        | expected /= b = remove i state
        | otherwise = state -- The corresponding new-reference binding updated it.
    remove i state = case M.lookup i (active state) of
        Nothing -> state
        Just (Candidate a b _) ->
            let update key index queue =
                    let remaining = S.delete i (index M.! key)
                        nextIndex = if S.null remaining then M.delete key index else M.insert key remaining index
                        nextQueue = if S.size remaining == 1 then S.union queue remaining else queue
                    in (nextIndex,nextQueue)
                (oldIndex,q) = update a (byOld state) (S.delete i (ready state))
                (newIndex,q') = update b (byNew state) q
            in state {active = M.delete i (active state), byOld = oldIndex,
                      byNew = newIndex, ready = q'}
