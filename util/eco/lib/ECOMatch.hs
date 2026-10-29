-- Correspondence proposes names, never replacement expressions.  Fingerprints
-- are hash-consed structural tuples shared by both DAGs, so there are no hash
-- collisions or exponentially expanded expression trees.
module ECOMatch (matchGraphs, proposeRenames, validateGraph) where

import ECO.Types
import ECOMatchTypes
import ECO.Validate (validateGraph)
import ECOAggressive (aggressiveAssign)
import Control.Monad (foldM, unless)
import Data.List (foldl', sort, sortOn)
import Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Nodes = M.Map EcoKey EcoNode
type Fingerprints = M.Map EcoKey Int
type Signature = (String, String, Maybe String, [Int], Maybe String)
type Intern = M.Map Signature Int

-- Memoize once per node.  Detect malformed combinational cycles explicitly.
fingerprint :: String -> Nodes -> Intern -> Either String (Fingerprints, Intern)
fingerprint side nodes table0 = foldM top (M.empty, table0) (M.keys nodes)
  where
    top state k = visit S.empty state k
    visit active state@(memo, _) k
        | M.member k memo = Right state
        | S.member k active = Left ("cycle in ECO graph at " ++ k)
        | otherwise = do
            let n = nodes M.! k
            (memo', table') <- foldM (visit (S.insert k active)) state
                                      (enOperands n)
            let sig = (enType n, enOp n, enBoundary n,
                       map (memo' M.!) (enOperands n),
                       if enOpaque n then Just (side ++ k) else Nothing)
                f = M.findWithDefault (M.size table') sig table'
            return (M.insert k f memo', M.insert sig f table')

buckets :: Ord k => (a -> k) -> [a] -> M.Map k [a]
buckets f = M.fromListWith (++) . map (\x -> (f x, [x]))

uniquePairs :: Ord k => M.Map k [a] -> M.Map k [b] -> [(a,b)]
uniquePairs a b = [(x,y) | ([x],[y]) <- M.elems (M.intersectionWith (,) a b)]

-- Seeds and consumer propagation give the baseline correspondences; the
-- aggressive assignment then completes them over every eligible typed name.
matchGraphs :: EcoGraph -> EcoGraph -> Either String EcoMatchResult
matchGraphs old new = do
    validateGraph old
    validateGraph new
    unless (egModule old == egModule new && egStage old == egStage new) $
        Left "ECO graph module/stage mismatch"
    let os = M.fromList [(enKey n,n) | n <- egNodes old]
        ns = M.fromList [(enKey n,n) | n <- egNodes new]
    (ofp, table) <- fingerprint "old:" os M.empty
    (nfp, _) <- fingerprint "new:" ns table
    let refine = aggressiveAssign ofp nfp old new
    if os == ns && S.fromList (egReserved old) == S.fromList (egReserved new)
    then return $ refine $ EcoMatchResult
        [EcoCorrespondence k k ExactContent "identical-capture" | k <- M.keys os]
        [] [] 0
    else do
        let typedBoundary n = (enBoundary n, enType n)
            boundaries = uniquePairs
                (buckets typedBoundary (filter ((/= Nothing) . enBoundary) (M.elems os)))
                (buckets typedBoundary (filter ((/= Nothing) . enBoundary) (M.elems ns)))
            ob = buckets (ofp M.!) [k | (k,n) <- M.toList os, not (enOpaque n)]
            nb = buckets (nfp M.!) [k | (k,n) <- M.toList ns, not (enOpaque n)]
            exact = uniquePairs ob nb
            overlaps = M.elems (M.intersectionWith (,) ob nb)
            -- Never search a Cartesian product of equal-expression copies.
            -- Large buckets await stable contexts or consumer-role evidence.
            duplicatePairs = concatMap disambiguate overlaps
            disambiguate (oks,nks)
                | length oks > 64 || length nks > 64 = []
                | otherwise = uniquePairs (contextBuckets os oks) (contextBuckets ns nks)
            contextBuckets nodes ks = M.fromListWith (++)
                [((enType n,c),[k]) | k <- ks, let n = nodes M.! k,
                                     c <- S.toList (S.fromList (enContexts n)),
                                     not (enOpaque n)]
            -- Identical typed contents plus a surviving emitted local name
            -- distinguish alias copies even when lowering erased their source
            -- contexts. A spelling alone never anchors changed contents.
            namedContent nodes fp = M.fromList
                [((fp M.! k,s),[k]) | (k,n) <- M.toList nodes,
                                     enEligible n, Just s <- [enName n]]
            sameNamedContent = uniquePairs (namedContent os ofp) (namedContent ns nfp)
            seeds = [(enKey a,enKey b,"stable-boundary") | (a,b) <- boundaries] ++
                    [(a,b,"typed-content-and-name") | (a,b) <- sameNamedContent] ++
                    [(a,b,"typed-content") | (a,b) <- exact] ++
                    [(a,b,"content-and-context") | (a,b) <- mutualPairs duplicatePairs] ++
                    [(a,b,"surviving-source-role") | (a,b) <-
                        mutualPairs $ uniquePairs (contextBuckets os (M.keys os))
                                                  (contextBuckets ns (M.keys ns))]
            initial = accept os ns M.empty M.empty seeds
            (matched, _) = propagate os ns initial (M.elems (fst initial))
            result (a,b,why) = EcoCorrespondence a b
                (if ofp M.! a == nfp M.! b then ExactContent else ChangedRole) why
        return $ refine $ EcoMatchResult
            (sortOn ecNew (map result (M.elems matched)))
            [k | k <- M.keys os, M.notMember k (snd (indexMatches matched))]
            [k | k <- M.keys ns, M.notMember k matched]
            (length [(a,b) | (a,b) <- overlaps, length a > 1 || length b > 1])
  where
    indexMatches m = (m, M.fromList [(a,b) | (a,b,_) <- M.elems m])

type Pair = (EcoKey, EcoKey, String)
type Matches = (M.Map EcoKey Pair, M.Map EcoKey EcoKey)

compatible :: Nodes -> Nodes -> EcoKey -> EcoKey -> Bool
compatible os ns a b = enType x == enType y &&
    enBoundary x == enBoundary y &&
    (not (enOpaque x || enOpaque y) || enBoundary x /= Nothing)
  where x = os M.! a
        y = ns M.! b

-- Priority-ordered seed classes, deterministic within each class.  Consumers
-- are handled separately with mutual-uniqueness checks, never greedy ties.
accept :: Nodes -> Nodes -> M.Map EcoKey Pair -> M.Map EcoKey EcoKey -> [Pair] -> Matches
accept os ns = \forward reverseMap pairs -> foldl' add (forward,reverseMap) pairs
  where
    add state@(forward,reverseMap) p@(a,b,_)
        | M.member b forward || M.member a reverseMap = state
        | not (compatible os ns a b) = state
        | otherwise = (M.insert b p forward, M.insert a b reverseMap)

propagate :: Nodes -> Nodes -> Matches -> [Pair] -> Matches
propagate _ _ matched [] = matched
propagate os ns (forward,reverseMap) frontier =
    let candidates = S.toList $ S.fromList
            [(a,b) | (ok,nk,_) <- frontier,
                     let on = os M.! ok, let nn = ns M.! nk,
                     not (enOpaque on || enOpaque nn),
                     length (enOperands on) == length (enOperands nn),
                     (a,b) <- zip (enOperands on) (enOperands nn),
                     M.notMember a reverseMap, M.notMember b forward,
                     compatible os ns a b]
        unambiguous = [(a,b,"consumer-operand-role") | (a,b) <- mutualPairs candidates]
        next = accept os ns forward reverseMap unambiguous
    in propagate os ns next unambiguous

mutualPairs :: [(EcoKey,EcoKey)] -> [(EcoKey,EcoKey)]
mutualPairs pairs =
    let distinct = S.toList (S.fromList pairs)
        fromOld = buckets fst distinct
        fromNew = buckets snd distinct
    in [(a,b) | (a,b) <- distinct,
                length (fromOld M.! a) == 1, length (fromNew M.! b) == 1]

-- Allocate a simultaneous mapping.  Historical names, including names whose
-- definitions disappeared, are unavailable to unrelated new signals.  New
-- definitions that occupy such a spelling are moved to a deterministic fresh
-- name.  The emitter must validate this proposal against its own namespace.
proposeRenames :: [String] -> EcoGraph -> EcoGraph -> EcoMatchResult ->
                  Either String [(String,String)]
proposeRenames emitted old new matches = do
    validateGraph old
    validateGraph new
    let os = M.fromList [(enKey n,n) | n <- egNodes old]
        ns = M.fromList [(enKey n,n) | n <- egNodes new]
        current = S.fromList emitted
        locals = S.fromList [s | n <- egNodes new, enEligible n,
                                Just s <- [enName n], S.member s current]
        protected = current S.\\ locals
        history = S.fromList (egReserved old ++ mapMaybe enName (egNodes old))
    desired <- mapM (desiredName os ns locals protected) (emCorrespondences matches)
    let wanted = mapMaybe id desired
        wantedMap = M.fromList wanted
        targets = S.fromList (map snd wanted)
    unless (M.size wantedMap == length wanted && S.size targets == length wanted) $
        Left "noninjective ECO correspondence name proposal"
    let initialUsed = S.unions [current, history, targets]
        allocate (renames,used,counter) name =
            case M.lookup name wantedMap of
                Just target -> ((name,target):renames,used,counter)
                Nothing | S.member name history || S.member name targets ->
                    let (fresh,next) = freshName used counter
                    in ((name,fresh):renames,S.insert fresh used,next)
                Nothing -> ((name,name):renames,used,counter)
        (allocated,_,_) = foldl' allocate ([],initialUsed,0 :: Int) (S.toList locals)
        destinations = map snd allocated
    unless (S.size (S.fromList destinations) == length destinations &&
            all (`S.notMember` protected) destinations) $
        Left "ECO name allocation collides with emitted namespace"
    return (sort [(a,b) | (a,b) <- allocated, a /= b])
  where
    desiredName os ns locals protected c = do
        a <- maybe (Left "correspondence references missing old node") Right
                   (M.lookup (ecOld c) os)
        b <- maybe (Left "correspondence references missing new node") Right
                   (M.lookup (ecNew c) ns)
        return $ case (enName a,enName b) of
            (Just oldName,Just newName)
                | enEligible a && enEligible b && enType a == enType b &&
                  S.member newName locals && S.notMember oldName protected ->
                    Just (newName,oldName)
            _ -> Nothing
    freshName used i =
        let name = "_eco_fresh_" ++ show i
        in if S.member name used then freshName used (i+1) else (name,i+1)
