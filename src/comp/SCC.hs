{-# LANGUAGE BangPatterns #-}
module SCC(scc, sccInt, getCycles, tsort, tsortStable, tsortWith, Graph) where

-- Compute strongly connected components and topological orders.
-- The graph is represented as a list of (node, neighbour list) pairs.

import qualified Data.Array as A
import qualified Data.Graph as G
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import Balanced hiding (lookup)

import ErrorUtil(internalError)


type Node v = (v, [v])
type Graph v = [Node v]

-- Compute strongly connected components and order the condensed DAG with
-- tsortStable, breaking ties between independent SCCs by their earliest
-- member's first-occurrence position in the input list and ordering members
-- within each SCC by first-occurrence position.
scc :: (Ord node) => [Node node] -> [[node]]
scc [] = []
scc ns =
    let step (!nextId, !mp, acc) x =
            case M.lookup x mp of
                Just _ -> (nextId, mp, acc)
                Nothing -> (nextId + 1, M.insert x nextId mp, x : acc)
        allNodes = [ s | (s, _) <- ns ] ++ [ d | (_, ds) <- ns, d <- ds ]
        (vCount, toIdx, revNodes) = L.foldl' step (0, M.empty, []) allNodes
        fromIdx = A.listArray (0, vCount - 1) (reverse revNodes)
        lk x = M.findWithDefault (internalError "SCC.scc: missing node") x toIdx
        indexed = [ (lk s, map lk ds) | (s, ds) <- ns ]
    in  [ [ fromIdx A.! i | i <- comp ] | comp <- sccIndexed vCount indexed ]

-- Int-keyed variant of scc that avoids any Ord dictionary on caller keys.
sccInt :: [Node Int] -> [[Int]]
sccInt [] = []
sccInt ns =
    let step (!nextId, !mp, acc) x =
            case IM.lookup x mp of
                Just _ -> (nextId, mp, acc)
                Nothing -> (nextId + 1, IM.insert x nextId mp, x : acc)
        allNodes = [ s | (s, _) <- ns ] ++ [ d | (_, ds) <- ns, d <- ds ]
        (vCount, toIdx, revNodes) = L.foldl' step (0, IM.empty, []) allNodes
        fromIdx = A.listArray (0, vCount - 1) (reverse revNodes)
        lk x = IM.findWithDefault (internalError "SCC.sccInt: missing node") x toIdx
        indexed = [ (lk s, map lk ds) | (s, ds) <- ns ]
    in  [ [ fromIdx A.! i | i <- comp ] | comp <- sccIndexed vCount indexed ]

sccIndexed :: Int -> [Node Int] -> [[Int]]
sccIndexed 0 _ = []
sccIndexed vCount indexed =
    let adjArr = A.accumArray (flip (:)) [] (0, vCount - 1)
                     [ (s, d) | (s, ds) <- indexed, d <- ds ]
        rawSCCs = G.stronglyConnComp
                      [ (v, v, adjArr A.! v) | v <- [0 .. vCount - 1] ]
        toComp (G.AcyclicSCC v) = [v]
        toComp (G.CyclicSCC vs) = L.sort vs
        compHead (x:_) = x
        compHead [] = internalError "SCC.sccIndexed: empty component"
        sortedComps = L.sortOn compHead (map toComp rawSCCs)
        m = length sortedComps
        compArr = A.listArray (0, m - 1) sortedComps
        vToComp = A.array (0, vCount - 1)
                      [ (v, cid)
                      | (cid, vs) <- zip [0 ..] sortedComps
                      , v <- vs ]
        dag = [ (cid, IS.toList $ IS.fromList
                          [ dc
                          | v <- compArr A.! cid
                          , u <- adjArr A.! v
                          , let dc = vToComp A.! u
                          , dc /= cid ])
              | cid <- [0 .. m - 1] ]
    in  case ntsort dag of
            Right compOrder -> [ compArr A.! cid | cid <- compOrder ]
            Left _ -> internalError "SCC.sccIndexed: cyclic condensed DAG"

getCycles :: (Ord node) => [Node node] -> [[node]]
getCycles xs =
       case otsort xs of
           Left cs -> cs
           Right _ -> []

------

otsort :: (Ord node) => [Node node] -> Either [[node]] [node]
otsort ns =
        let selfSet = S.fromList [ x | (x, ys) <- ns, y <- ys, x == y ]
            sccs = scc ns
            isCyclic [] = internalError "otsort isCyclic []"
            isCyclic [v] = v `S.member` selfSet
            isCyclic _ = True
        in  case L.partition isCyclic sccs of
                ([], noncycs) -> Right (concat noncycs)
                (cycs, _) -> Left cycs


------

-- Sort a graph topologically, breaking ties by first-occurrence position in
-- the input list rather than by Ord node (which for Id is string-intern
-- order) or by Balanced.PSQ's internal loser-tree shape.
--
-- Balanced.PSQ compares only priorities (p1 <= p2) when maintaining its
-- tournament tree; Ord k is used only for search navigation.  Putting each
-- vertex's unique input rank into the priority tuple (indegree, inputRank)
-- gives a strict total order on priorities so equal-indegree ties are always
-- resolved by input order.  Duplicate outgoing edges per vertex are
-- deduplicated before counting indegrees so ntsort never leaves positive
-- residual counts on duplicate edges and falls back to otsort.  Cycles and
-- edges to vertices absent from the input list still fall back to otsort.
tsort :: Ord node => [Node node] -> Either [[node]] [node]
tsort = ntsort

tsortStable :: Ord node => [Node node] -> Either [[node]] [node]
tsortStable = ntsort

ntsort :: Ord node => [Node node] -> Either [[node]] [node]
ntsort g =
    let rankMap = M.fromListWith (\_ old -> old) (zip (map fst g) [0 :: Int ..])
        adjMap = M.fromListWith S.union [ (s, S.fromList ds) | (s, ds) <- g ]
        psq = fromOrdList
                  [ n :-> (S.size ds, M.findWithDefault 0 n rankMap)
                  | (n, ds) <- M.toAscList adjMap ]
        revMap = M.fromListWith (++)
                     [ (d, [s])
                     | (s, ds) <- M.toAscList adjMap
                     , d <- S.toList ds ]
        dependents n = M.findWithDefault [] n revMap
    in  case loop dependents psq [] of
        Right ns -> Right ns
        Left _ -> otsort g

-- Topological sort with a caller-supplied tie-break key computed from each
-- vertex and its first-occurrence input position, breaking remaining ties by
-- input position.
tsortWith :: (Ord node, Ord k) => ((node, Int) -> k) -> [Node node] -> Either [[node]] [node]
tsortWith prio g =
    let rankMap = M.fromListWith (\_ old -> old) (zip (map fst g) [0 :: Int ..])
        adjMap = M.fromListWith S.union [ (s, S.fromList ds) | (s, ds) <- g ]
        mkBinding (n, ds) =
            let r = M.findWithDefault 0 n rankMap
            in  n :-> (S.size ds, (prio (n, r), r))
        psq = fromOrdList (map mkBinding (M.toAscList adjMap))
        revMap = M.fromListWith (++)
                     [ (d, [s])
                     | (s, ds) <- M.toAscList adjMap
                     , d <- S.toList ds ]
        dependents n = M.findWithDefault [] n revMap
    in  case loop dependents psq [] of
        Right ns -> Right ns
        Left _ -> otsort g

loop :: (Ord node, Ord t) => (node -> [node]) -> PSQ node (Int, t) -> [node] -> Either [[node]] [node]
loop dependents psq ns =
    case minView psq of
    Empty -> Right (reverse ns)
    Min (n :-> (0, _)) psq' -> loop dependents (decrList (dependents n) psq') (n:ns)
    _ -> Left [map key (toOrdList psq)]

decrList :: (Ord node, Ord t) => [node] -> PSQ node (Int, t) -> PSQ node (Int, t)
decrList ks pqs = L.foldl' (flip decr) pqs ks
  where decr n q = adjust (\(deg, t) -> let deg' = deg - 1 in deg' `seq` (deg', t)) n q

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
