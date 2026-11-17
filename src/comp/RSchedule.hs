{-# LANGUAGE CPP #-}
#if MIN_VERSION_GLASGOW_HASKELL(9,1,0,0)
#else
{-
This is needed for ghc 8.8.4 on Ubuntu 22.04 LTS, or else
`instance PPrint RAT` triggers

* Illegal instance declaration for `PPrint RAT'
    (All instance types must be of the form (T t1 ... tn)
     where T is not a synonym.
     Use TypeSynonymInstances if you want to disable this.)

Despite the suggestion of TypeSynonymInstances, only
FlexibleInstances is necessary.
-}
{-# LANGUAGE FlexibleInstances #-}
#endif

-- RSchedule
--
-- This module exports the data type RAT (resource allocation table) and
-- the function "rSchedule" which is used by ASchedule to create the RAT.
--
module RSchedule(rSchedule, RAT, ratToNestedLists, ratInUseOrder) where

import Data.List(genericLength, partition, sortOn)
import qualified Data.List as L
import Data.Maybe(maybeToList)
import Control.Monad(when)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified GraphMap as G
import ASyntax(AId)
import AUses(RuleId, MethodId, UniqueUse, MethodUsesMap, MethodUsers,
             getMIdPosition, getUUPos, hasSideEffects, differentArgs,
             useDropCond)
import ErrorMonad(ErrorMonad(..))
import Error(internalError, EMsg, ErrMsg(..))
import Util(ordPair, mapFst)
import Flags(ResourceFlag(..))
import PFPrint(pfpString)
import Id(Id)
import Position(prPosition,getPosition)

import Util({- traces, -} allPairs)
import Debug.Trace(traceM)
import IOUtil(progArgs)
import PPrint

-- ==============================
-- Traces

trace_ralloc, trace_uugraph :: Bool
trace_ralloc = "-trace-ralloc" `elem` progArgs
trace_uugraph = "-trace-uugraph" `elem` progArgs

-- ==============================
-- Data types

type RAT = M.Map MethodId (M.Map UniqueUse Integer) -- resource allocation table

-- hack to avoid sorting order differences after switching from a list to a map
sortRatList :: [(MethodId, [(UniqueUse, Integer)])] -> [(MethodId, [(UniqueUse, Integer)])]
sortRatList = sortOn (ppString . fst)

ratToNestedLists :: RAT -> [(MethodId, [(UniqueUse, Integer)])]
ratToNestedLists = sortRatList . M.toList . M.map (sortOn (ppString . fst) . M.toList)

-- the table with each method's uses in the use map's order (the order the
-- scheduler met them); the table itself is a map, which would give Ord,
-- i.e. string-intern, order
ratInUseOrder :: MethodUsesMap -> RAT -> [(MethodId, [(UniqueUse, Integer)])]
ratInUseOrder mum rat = [ (m, sortOn (pos m) us) | (m, us) <- ratToNestedLists rat ]
  where pos m (u, _) = M.findWithDefault maxBound u (idx m)
        idx m = M.fromList (zip (map (useDropCond . fst) (M.findWithDefault [] m mum)) [0 :: Int ..])

instance PPrint RAT where
  pPrint d p = pPrint d p . ratToNestedLists

-- ---------------

type UUGraph = G.GraphMap UniqueUse [UseKind]

type RRM = (RuleId, RuleId, MethodId)
type StkL = [StackItem UniqueUse RRM]

-- We mark UUGraph edges by their kind
-- ActionOf edges can be arbitrated to remove the conflict
-- (in which case the pair tells us the rules to arbitrate).
-- Other edges can't be arb'd, so the info is currently just for debugging?
data UseKind = PredicateOf (RuleId,RuleId)  -- one use is in a rule predicate
             | ActionOf    (RuleId,RuleId)  -- uses are only in rule bodies
             | InstanceOf  (AId,AId)  -- one use is in a submodule instance
     deriving (Eq, Ord, Show)

instance PPrint (UseKind) where
    pPrint d p (PredicateOf x) = text "predicate:" <+> pPrint d p x
    pPrint d p (ActionOf x) = text "action:" <+> pPrint d p x
    pPrint d p (InstanceOf x) = text "instance:" <+> pPrint d p x

data StackItem v r = Vertex (v,[v]) | Colored v Integer | Edge r

-- ==============================
-- rSchedule

-- assign resources and drop conflicting edges
-- XXX: mapM should be foldM!!!

-- Inputs:
--  * moduleId
--  * rFlag
--  * RMaxs
--  * rMap
--  * areSimult
--  * canArb (whether arbitration may drop the edge between two rules)
-- Output:
--  * RAT
--  * [RRM]

rSchedule :: Id -> ResourceFlag -> M.Map MethodId Integer -> MethodUsesMap ->
             (RuleId -> RuleId -> Bool) -> (RuleId -> RuleId -> Bool) ->
             ErrorMonad (RAT,[RRM])
rSchedule moduleId rFlag rMaxs rMap areSimult canArb =
    -- We don't need to worry about keys conflicting when combining xxs
    -- because those keys came from the keys of rMap.
    let concatTuple (xxs,yys) = (M.unions xxs, concat yys)
        f = rSchedule' moduleId rFlag rMaxs areSimult canArb
    in
        mapM f (sortOn (ppString . fst) (M.toList rMap)) >>= return . concatTuple . unzip


rSchedule' :: Id -> ResourceFlag -> M.Map MethodId Integer ->
              (RuleId -> RuleId -> Bool) -> (RuleId -> RuleId -> Bool) ->
              (MethodId, [(UniqueUse, MethodUsers)]) ->
              ErrorMonad (RAT, [RRM])
rSchedule' moduleId rFlag rMaxs areSimult canArb (mId, uses0) =
    let
        -- XXX condition-insensitive resource allocation
        -- our graph allocation is not smart enough to handle
        -- different expression uses with the same arguments
        -- but different conditions, so we drop them here and in AState.hs
        uses = mapFst useDropCond uses0
        rMax = lookupRes mId rMaxs

        g :: UUGraph
        g = uuGraph areSimult uses rMax

        -- the uses in the use map's order (the order the scheduler met
        -- them); the graph is a map and would give string-intern order
        order = map fst uses

        dropEdges =
            case rFlag of
                RFoff -> -- don't reschedule
                         errDropEdges mId rMax order
                RFsimple -> -- reschedule
                            -- arbitrate resource (drop edge in graph)
                            simpleDropEdges moduleId areSimult canArb (mId, uses) rMax

        -- With no port limit (rMax 0) every use goes to one port and the
        -- schedule is trusted to keep the callers apart, but no schedule
        -- keeps a value method's use off that port: two uses with
        -- different arguments by a simultaneous pair that cannot be
        -- arbitrated would share it wrongly, so report the resource error
        -- (as for a one-port method).
        unsharable = rMax == 0 &&
            or [ not (canArb r r') && areSimult r r'
               | ((u, (_, rs, _)), (u', (_, rs', _))) <- allPairs uses,
                 differentArgs u u', r <- rs, r' <- rs' ]
    in do
         when unsharable (errDropEdges mId 1 order () g)
         when trace_ralloc
             (traceM $ "rSchedule: allocating " ++ show (length uses) ++
                       " uses of " ++ ppString mId ++ "; " ++
                       if rMax == 0 then "no port limit" else show rMax ++
                       " ports available")
         when trace_uugraph
             (traceM $ "rSchedule: uugraph:\n" ++ ppReadable g)
         -- when (rMax <= 0) (verifySC g)
         if length uses > 16 && fromInteger rMax >= length uses
           then return (M.singleton mId (M.fromList (zip (map fst uses) [1..])), [])
           else do (colors, drops) <- color rMax dropEdges order g
                   return (M.singleton mId colors, drops)


-- ==============================
-- Function: uuGraph

-- build a graph with UniqueUse vertices
-- and edges where two uses are simultaneous
uuGraph :: (RuleId -> RuleId -> Bool) ->
           [(UniqueUse, MethodUsers)] ->
           Integer ->
           UUGraph
uuGraph areSimult uUses rMax =
    let
        gVertices = foldr (flip G.addVertex) G.empty [u | (u,_) <- uUses]

        -- edges for uses in rule actions
        --   diff arguments and uses of the same action
        --   (hasSideEffects uUse) are potential resource conflicts,
        --   requiring checking whether the uses are simultaneous (simult)
        --   (two uses of same action are ok if the action is idempotent,
        --   which is captured in the "simult" check)
        aEdges =
            [(uUse, uUse', ss) | rMax /= 0,
                ((uUse, (_, rs, _)), (uUse', (_, rs', _))) <- allPairs uUses,
                differentArgs uUse uUse' || hasSideEffects uUse,
                let ss = simult rs rs', not $ null ss]

        simult rs rs' = [ActionOf (r,r') | r <- rs, r' <- rs', areSimult r r']

        -- edges for uses in rule predicates
        -- XXX we currently assume that predicates must always occur
        -- XXX but with urgency we can be smarter (a predicate's use may
        -- XXX be exclusive with the execution of the action of a more
        -- XXX more urgent rule)
        pEdges = concat [[(uUse, uUse', ss), (uUse', uUse, ss)] | rMax /= 0,
                             (uUse, (prs, _, _)) <- uUses, not (null prs),
                             (uUse', (prs', ars', _)) <- uUses,
                             differentArgs uUse uUse',
                             let ss = [PredicateOf (p, r) |
                                           p <- prs, r <- prs'++ars']]

        -- edges for instantiations
        -- (like rule predicates, the must always occur)
        iEdges = concat [[(uUse, uUse', ss), (uUse', uUse, ss)] | rMax /= 0,
                             (uUse, (_, _, irs)) <- uUses, not (null irs),
                             (uUse', (prs', ars', irs')) <- uUses,
                             differentArgs uUse uUse',
                             let ss = [InstanceOf (p, r) |
                                           p <- irs, r <- prs'++ars'++irs']]
    in
        L.foldl' (G.addEdgeWith (flip (++))) gVertices (iEdges ++ pEdges ++ aEdges)


-- ==============================
-- Function: verifySC

{-
verifySC g = mapM_ err [(v,v',r) | (v,ns) <- G.toList g,
                                   (v', us) <- ns,
                                   ActionOf (r,r') <- us,
                                   r == r']
    where err bad@(u,u',r) = EMError (getIdPosition r, EGeneric (emsg bad))
          emsg (u,u',r) = "Rule `" ++ ppString r ++
                          "' uses an SC method twice: `" ++
                          ppString u ++ "' and `" ++ ppString u' ++ "'"
-}


-- ==============================
-- Error messages

errDropEdges :: MethodId -> Integer -> [UniqueUse] -> b -> UUGraph -> ErrorMonad a
errDropEdges mId rMax order _ g = EMError [eResources mId rMax order g]

eResources :: MethodId -> Integer -> [UniqueUse] -> UUGraph -> EMsg
eResources mId rMax order g =
    (getMIdPosition mId,
     EResources (ppString mId) rMax
         (map (\u -> (ppString u, prPosition (getUUPos u))) (inOrder order g)))

-- the graph's vertices in the given order
inOrder :: [UniqueUse] -> UUGraph -> [UniqueUse]
inOrder order g = let vs = S.fromList (G.vertices g)
                  in  [ v | v <- order, v `S.member` vs ]

-- XXX It would be nice if EArbitrate had position info
eArbitrate :: Id -> (RuleId, RuleId) -> EMsg
eArbitrate moduleId (r,r') =
    (getPosition moduleId,
     EArbitrate (pfpString moduleId) [ppString r, ppString r'])


-- ==============================
-- Function: simpleDropEdges

simpleDropEdges :: Id -> (RuleId -> RuleId -> Bool) ->
                   (RuleId -> RuleId -> Bool) ->
                   (MethodId, [(UniqueUse, MethodUsers)]) ->
                   Integer -> StkL -> UUGraph -> ErrorMonad (StkL, UUGraph)
simpleDropEdges moduleId areSimult canArb (mId,uses) rMax st g =
    if all null droppable || any sameRule rs
    then errDropEdges mId rMax order st g
    else EMWarning warn (st',g')
    where order = map fst uses
          vsSet = S.fromList (G.vertices g)
          orderedVs = [ v | v <- order, v `S.member` vsSet ]
          droppable = [map fromActionOf w
                       | v <- orderedVs, v' <- orderedVs,
                       w <- maybeToList (G.lookup (v,v') g), all isActionOf w,
                       all (uncurry canArb . fromActionOf) w]
          rs = case droppable of
               (xs:_) -> xs
               _      -> internalError "simpleDropEdges: nothing to drop!"
          sameRule (r,r') = r == r'
          uses' = [u | u@(uu, _) <- uses, uu `S.member` vsSet]
          st' = [Edge (r,r',mId) | (r,r') <- rs] ++ st

          allDrops = [(r,r') | Edge (r,r',_) <- st] ++ rs
          allDropsSet = S.fromList $ map ordPair allDrops
          areSimult' r r' = (not (ordPair (r,r') `S.member` allDropsSet))
                            && areSimult r r'
          g'  = uuGraph areSimult' uses' rMax
          fromActionOf (ActionOf x) = x
          fromActionOf _ = internalError "fromActionOf"
          isActionOf (ActionOf _) = True
          isActionOf _            = False
          warn = (eResources mId rMax order g) : (map (eArbitrate moduleId) allDrops)


-- ==============================
-- Function: color

color :: Integer -> (StkL -> UUGraph -> ErrorMonad (StkL, UUGraph)) ->
         [UniqueUse] -> UUGraph -> ErrorMonad (M.Map UniqueUse Integer, [RRM])
color rMax dropEdges order g
    | rMax > 0 = colorFw rMax dropEdges order [] g >>= colorBk [1..rMax] M.empty []
    | otherwise = return (M.fromList [(v,1) | v <- G.vertices g], [])


-- forward pass: generate stack of colorable vertices and dropped edges
-- (the vertices are tried in the use map's order, which decides which
-- use gets which port)
colorFw :: Integer -> (StkL -> UUGraph -> ErrorMonad (StkL, UUGraph)) ->
           [UniqueUse] -> StkL -> UUGraph -> ErrorMonad StkL
colorFw rMax dropEdges order st g
    | G.null g = return st
    | otherwise =
        let vs = inOrder order g
        in case partition (colorable rMax g) vs of
            (cv:_, _) -> colorFw rMax dropEdges order
                             (Vertex (cv, G.neighbors g cv) : st)
                             (G.deleteVertex g cv)
            ([], _) ->
                case exactColor [1..rMax] vs g of
                    Just cMap ->
                        return ([Colored v c | (v, c) <- cMap] ++ st)
                    Nothing ->
                        dropEdges st g >>= (uncurry $ colorFw rMax dropEdges order)


-- backward pass: pick up vertices and color them
colorBk :: [Integer] -> M.Map UniqueUse Integer -> [RRM] -> StkL ->
           ErrorMonad (M.Map UniqueUse Integer, [RRM])
colorBk _  cs es [] = return (cs,es)
colorBk rMaxL cs es (Vertex vns@(v,_) : vs) =
    -- We don't need to worry about keys conflicting as we build the
    -- map because the keys are the distinct vertices of the GraphMap.
    colorBk rMaxL (M.insert v (pickColor rMaxL cs vns) cs) es vs
colorBk rMaxL cs es (Colored v c : vs) =
    colorBk rMaxL (M.insert v c cs) es vs
-- XXX try to reintroduce edges
colorBk rMaxL cs es (Edge e : vs) = colorBk rMaxL cs (e:es) vs


-- Sound Kempe peel condition: strictly fewer than rMax neighbors guarantees
-- at least one free color in colorBk regardless of neighbor color choices.
colorable :: Integer -> UUGraph -> UniqueUse -> Bool
colorable rMax g v = genericLength (G.neighbors g v) < rMax


-- Bounded exact coloring in use order for cores where every vertex has degree
-- >= rMax (for instance bipartite sharing such as K_{2,2} on a 2-port method).
-- Uncolorable cores (such as odd cycles for rMax = 2) return Nothing so
-- dropEdges reports G0002 or arbitrates cleanly instead of crashing pickColor.
exactColor :: [Integer] -> [UniqueUse] -> UUGraph -> Maybe [(UniqueUse, Integer)]
exactColor rMaxL vs g
    | length rMaxL <= 1 = Nothing
    | length vs > 32 = Nothing
    | otherwise = snd (search 4096 M.empty vs)
  where
    adj = M.fromList [ (v, G.neighbors g v) | v <- vs ]
    nbrs v = M.findWithDefault [] v adj
    usedColors cs u =
        S.fromList [ c | n <- nbrs u, c <- maybeToList (M.lookup n cs) ]

    pickNext :: M.Map UniqueUse Integer -> [UniqueUse]
             -> Maybe (UniqueUse, S.Set Integer, [UniqueUse])
    pickNext _ [] = Nothing
    pickNext cs (u0:us0) =
        let u0Used = usedColors cs u0
            step cur@(_, _, bestSat) u =
                let uUsed = usedColors cs u
                    sat = S.size uUsed
                in if sat > bestSat then (u, uUsed, sat) else cur
            (bestV, bestUsed, _) = L.foldl' step (u0, u0Used, S.size u0Used) us0
        in Just (bestV, bestUsed, filter (/= bestV) (u0:us0))

    search :: Int -> M.Map UniqueUse Integer -> [UniqueUse]
           -> (Int, Maybe [(UniqueUse, Integer)])
    search fuel cs remaining
        | fuel <= 0 = (0, Nothing)
        | otherwise =
            case pickNext cs remaining of
                Nothing ->
                    (fuel, Just [ (v, c) | v <- vs, c <- maybeToList (M.lookup v cs) ])
                Just (v, used, rest) ->
                    tryColors (fuel - 1) v rest [ c | c <- rMaxL, c `S.notMember` used ]
      where
        tryColors f _ _ [] = (f, Nothing)
        tryColors f v rest (c:csLeft)
            | f <= 0 = (0, Nothing)
            | otherwise =
                case search f (M.insert v c cs) rest of
                    (f', Just ok) -> (f', Just ok)
                    (f', Nothing) -> tryColors f' v rest csLeft


pickColor :: [Integer] -> M.Map UniqueUse Integer -> (a, [UniqueUse]) -> Integer
pickColor rMaxL cs (_,ns) =
    let used = S.fromList [lookupRes n cs | n <- ns]
    in case filter (`S.notMember` used) rMaxL of
        (r:_) -> r
        _     -> internalError ("pickColor\ncolors:" ++ ppReadable cs ++
                                "available:" ++ ppReadable rMaxL)


-- ==============================
-- Utility functions

lookupRes :: (Ord a, PPrint a, PPrint b) => a -> M.Map a b -> b
-- return y s.t. (r,y) `elem` rs  or  die
lookupRes r rs = case M.lookup r rs of
                 (Just x) -> x
                 Nothing  -> internalError $ "RSchedule: phantom resources" ++ ppReadable r ++ ppReadable rs

-- ==============================
