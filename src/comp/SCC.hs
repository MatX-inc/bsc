module SCC(scc, getCycles, tsort, tsortWith, Graph) where

-- Compute strongly connected components.
-- The graph is represented as a list of (node, neighbour list) pairs.
-- A list of list of nodes is returned, each connected.
-- Original code by John Launchbury.

import Data.List(partition, sortOn, foldl')
import Data.Maybe(mapMaybe)
import qualified Data.Map as M
import qualified Data.Set as S
import Balanced hiding (lookup)

import ErrorUtil(internalError)


type Node v = (v, [v])
type Graph v = [Node v]

type NMap node = M.Map node [node]

mFromList :: Ord k => [(k, a)] -> M.Map k a
mFromList xs = M.fromList xs
mLookup :: Ord k => k -> M.Map k a -> Maybe a
mLookup x m = M.lookup x m

-- Using OrdSet seems to be slower than lists

sElem :: Ord a => a -> S.Set a -> Bool
sElem x s = S.member x s
sAdd :: Ord a => a -> S.Set a -> S.Set a
sAdd x s = S.insert x s
sEmpty :: S.Set a
sEmpty = S.empty

sccEdge :: (Ord node) => NMap node -> NMap node -> [node] -> [[node]]
sccEdge ns rns vs
  = snd (span_tree rrng sEmpty []
                   (snd (dfs erng sEmpty [] vs) )
        )
  where

    rrng w = find w rns
    erng w = find w ns

    span_tree r vs ns []   = (vs,ns)
    span_tree r vs ns (x:xs)
        | x `sElem` vs = span_tree r vs ns xs
        | otherwise    = case dfs r (sAdd x vs) [] (r x) of (vs', ns') -> span_tree r vs' ((x:ns'): ns) xs

    dfs r vs ns []   = (vs,ns)
    dfs r vs ns (x:xs)
        | x `sElem` vs = dfs r vs ns xs
        | otherwise    = case dfs r (sAdd x vs) [] (r x) of (vs', ns') ->       dfs r vs' ((x:ns')++ns) xs

rev :: (Ord node) => [Node node] -> NMap node
rev ns = M.fromListWith (++) [ (d, [s]) | (s, ds) <- ns, d <- ds ]

find :: (Ord node) => node -> NMap node -> [node]
find x m =
    case mLookup x m of
    Just xs -> xs
    Nothing -> []

----

scc :: (Ord node) => [Node node] -> [[node]]
scc ns = sccEdge (mFromList ns) (rev ns) (map fst ns)

getCycles :: (Ord node) => [Node node] -> [[node]]
getCycles xs =
       case otsort xs of
           Left cs -> cs
           Right _ -> []

------

otsort :: (Ord node) => [Node node] -> Either [[node]] [node]
otsort ns =
        let es = [(x,y) | (x, ys) <- ns, y <- ys]
            vs = map fst ns
            sccs = sccEdge (mFromList ns) (rev ns) vs
            isCyclic [] = internalError "otsort isCyclic []"
            isCyclic [v] = isElem v es
            isCyclic _ = True
            isElem v [] = False
            isElem v ((x,y):xys) = v == x && v == y  ||  isElem v xys
        in  case partition isCyclic sccs of
                ([], noncycs) -> Right (concat noncycs)
                (cycs, _) -> Left cycs


------

-- sort a graph topologically; not a stable sort.  Of the nodes the edges
-- leave unordered, the queue decides (by Ord in the typical case, by its
-- tree shape among equal priorities); unchanged from before tsortWith.
tsort :: Ord node => [Node node] -> Either [[node]] [node]
tsort = tsortWith (const (Nothing :: Maybe ()))

-- Topological sort with an optional tie-break key, computed from each
-- node and its position in the input list.  With a key, the queue's
-- priority is (in-degree, key, node), so of the ready nodes the least
-- key comes first whatever the tournament tree's own tie rule (which
-- depends on the tree's shape: a sort keyed on position but tied by the
-- tree is not even idempotent, and the synthesize pass re-sorts inside a
-- fixpoint loop).  Without a key the priority is the in-degree alone and
-- the tree decides ties as it always has, so tsort's results are
-- unchanged.  Edges to nodes outside the list, and cycles, send it back
-- to otsort, whose cycle report is accurate (the queue walk reports every
-- unsorted node as one cycle, e.g. for [(1, [2, 3, 4]), (5, [3, 2, 4])]).
tsortWith :: (Ord node, Ord k) => ((node, Int) -> Maybe k) -> [Node node] -> Either [[node]] [node]
tsortWith prio g =
    let ixs = M.fromList (zip (map fst g) [0 :: Int ..])
        keyOf n = fmap (\ i -> (prio (n, i), n)) (M.lookup n ixs)
        priority k@(tie, _) deg = (deg, fmap (const k) tie)
        psq = fromOrdList [ k :-> priority k p | (k, p) <- sortOn fst [ (k, length ns) | (n, ns) <- g, Just k <- [keyOf n] ] ]
        m = M.fromListWith (++) [ (d, [s]) | (s, ds) <- g, d <- ds ]
        dependents n = mapMaybe keyOf (M.findWithDefault [] n m)
    in  case loop dependents psq [] of
        Right ns -> Right ns
        Left _ -> otsort g

type TieKey k node = (Maybe k, node)

loop :: (Ord node, Ord k) => (node -> [TieKey k node]) -> PSQ (TieKey k node) (Int, Maybe (TieKey k node)) -> [node] -> Either [[node]] [node]
loop dependents psq ns =
    case minView psq of
    Empty -> Right (reverse ns)
    Min ((_, n) :-> (0, _)) psq' -> loop dependents (decrList (dependents n) psq') (n:ns)
    _ -> Left [ [ n | (_, n) :-> _ <- toOrdList psq ] ]

decrList :: (Ord k, Ord t) => [k] -> PSQ k (Int, t) -> PSQ k (Int, t)
decrList ks pqs = foldl' (flip (adjust (\ (d, t) -> (d - 1, t)))) pqs ks

------

{-
-- Consistency check
chkTsort :: (Show node, Ord node) => [Node node] -> Either [[node]] [node] -> Either [[node]] [node]
chkTsort g r@(Left _) = r
chkTsort g r@(Right ons) = cloop S.empty ons
  where cloop _ [] = r
        cloop s (n:ns) = if all (`S.member` s) xs then cloop (S.insert n s) ns else internalError ("chkTsort: " ++ show g ++ "\n" ++ show ons ++ "\n" ++ show (n, xs))
                where xs = find n m
        m = M.fromList g
-}
