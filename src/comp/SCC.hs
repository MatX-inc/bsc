module SCC(scc, getCycles, tsort, tsortStable, Graph) where

-- Compute strongly connected components.
-- The graph is represented as a list of (node, neighbour list) pairs.
-- A list of list of nodes is returned, each connected.
-- Original code by John Launchbury.

import Data.List(partition, sort, foldl')
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

-- sort a graph topologically
-- note that this is *not* a stable sort
tsort :: Ord node => [Node node] -> Either [[node]] [node]
tsort = ntsort
-- reverts to otsort if ntsort looks buggy (see note below)
-- tsort = otsort

-- XXX ntsort [(1, [2, 3, 4]), (5, [3, 2, 4])] = Left [[1,5]]
-- XXX fixed by falling back to otsort, but should be fixed in ntsort?
ntsort :: Ord node => [Node node] -> Either [[node]] [node]
ntsort g =
    let psq = fromOrdList [ n :-> length ns | (n, ns) <- sort g ]
        m = M.fromListWith (++) [ (d, [s]) | (s, ds) <- g, d <- ds ]
        get n = case M.lookup n m of Just ns -> ns; Nothing -> []
    in        {- loop get psq [] -} -- XXX: leads to buggy cycles
        case loop get psq [] of
        Right ns -> Right ns
        Left _ -> otsort g  -- revert to old version to get accurate cycles


type TSPSQ node = PSQ node Int

-- A topological sort whose only tie-break is position in the input
-- list: of the nodes that the edges leave unordered, the one listed
-- first comes first, so the result is a function of the input alone.
-- tsort breaks ties by the node's Ord instead; for Id that is
-- string-intern order, which depends on which names the process has
-- already seen, so the same input sorts differently in a one-shot
-- compile and in a batch.  Use this one wherever the order reaches an
-- output, such as the nesting of emitted definitions.  Edges to nodes
-- outside the list are ignored, as tsort ignores them; a self-edge is
-- a cycle, as for tsort.
tsortStable :: Ord node => [Node node] -> Either [[node]] [node]
tsortStable g =
    let nodes = map fst g
        im = M.fromList (zip nodes [0 :: Int ..])
        toInt n = M.lookup n im
        back = M.fromList (zip [0 :: Int ..] nodes)
        fromInt i = M.findWithDefault (internalError "SCC.tsortStable: index") i back
        depSets = M.fromListWith S.union
                      [ (i, S.fromList (mapMaybe toInt ns)) | (n, ns) <- g, Just i <- [toInt n] ]
        dependents = M.fromListWith S.union
                         [ (d, S.singleton i) | (i, ds) <- M.toList depSets, d <- S.toList ds ]
        ready0 = S.fromList [ i | (i, ds) <- M.toList depSets, S.null ds ]
        counts0 = M.map S.size depSets
        go ready counts acc =
            case S.minView ready of
              Nothing -> reverse acc
              Just (i, ready') ->
                  let ss = S.toList (M.findWithDefault S.empty i dependents)
                      step (r, c) s = let c' = M.adjust (subtract 1) s c
                                      in  if M.findWithDefault 1 s c' == 0 then (S.insert s r, c') else (r, c')
                      (ready'', counts') = foldl' step (ready', counts) ss
                  in  go ready'' counts' (i : acc)
        order = go ready0 counts0 []
    in  if length order == M.size depSets
          then Right (map fromInt order)
          else case otsort g of
                 Left cs -> Left cs
                 Right _ -> internalError "SCC.tsortStable: cycle without cycles"

loop :: (Ord node) => (node -> [node]) -> TSPSQ node -> [node] -> Either [[node]] [node]
loop inputs psq ns =
    case minView psq of
    Empty -> Right (reverse ns)
    Min (n :-> 0) psq' -> loop inputs (decrList (inputs n) psq') (n:ns)
    _ -> Left [map key (toOrdList psq)]

decrList :: (Ord node) => [node] -> TSPSQ node -> TSPSQ node
decrList ns pqs = foldl' (flip decr) pqs ns
  where decr n pqs = adjust (subtract 1) n pqs

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
