{-# LANGUAGE LambdaCase, MagicHash #-}
-- | The logical execution orders to check: the schedule's and the CF swaps.
module Lean.Sched.Order where

import Data.Bits (bit, testBit, (.|.))
import Data.List (tails)
import Data.Maybe (mapMaybe)
import qualified Data.IntMap.Strict as IM (findWithDefault, fromListWith)
import qualified Data.IntSet as IS (member, notMember, fromList)
import qualified Data.Map.Strict as M (null, Map, (!), delete, empty, findWithDefault, lookup, member, size,
    toList, insert, adjust, fromList, fromListWith, intersectionWith)
import qualified Data.Map.Lazy as ML ((!), fromDistinctAscList, fromList, lookup)
import qualified Data.Set as S (Set, member, notMember, empty, fromList, insert, minView, singleton, size,
    toList, union)

import Lean.Sched.Types (Call(clPort), Child(chConf), Conflicts(cfCF), Design(dMethodSteps, dChildren),
    Order(orFrom, Order, orWindow), Port(ptMeth, ptInst), Step(stCalls, stReadSites))

-- | The schedule's order and one per exported CF pair: bsc's execution edges plus the later
-- method first, other steps kept in place (a direct edge between the two dropped; unrealizable swaps noted).
ordersOf :: [String] -> [(String, String)] -> (String -> [String]) -> [(String, String)]
         -> Either String ([Order], [(String, String)])
ordersOf names edges stepsOf cfs = case backward of
    (x, y) : _ | distinct ->
        Left ("bsc's execution order runs " ++ y ++ " before " ++ x
              ++ ", against an edge of its scheduling graph")
    _ -> Right ( Order "" ("", "") names n [] ("", "") : [ o | Right o <- swaps ]
               , [ p | Left p <- swaps ] )
  where
    n = length names
    pos = M.fromList (zip names [0 :: Int ..])
    atPos = M.fromList (zip [0 :: Int ..] names)
    -- the schedule's steps from each position on, shared by the orders
    tailsAt = M.fromList (zip [0 :: Int ..] (tails names))
    tailAt k = M.findWithDefault [] k tailsAt
    -- the edges between steps, checked once rather than in every swap
    edges' = [ (x, y) | (x, y) <- edges, M.member x pos, M.member y pos ]
    -- every edge goes forward in the schedule (bsc's order is a sort of these edges), so a
    -- swapped order differs only in a window, which is all that is sorted (a backward edge is an error)
    distinct = S.size (S.fromList names) == n
    backward = [ (x, y) | (x, y) <- edges', pos M.! x >= pos M.! y ]
    predsOf = M.fromListWith S.union [ (y, S.singleton x) | (x, y) <- edges' ]
    succsOf = M.fromListWith S.union [ (x, S.singleton y) | (x, y) <- edges' ]
    swaps = [ swapped a b sa sb | (a, b) <- cfs, let sa = stepsOf a, let sb = stepsOf b
                                , not (null sa), not (null sb) ]
    -- each method's steps (several when bsc split it); the earlier method's wait until none of
    -- the later method's are left, counted once rather than as an edge per pair
    swapped a b sa sb =
        let first ss = minimum (map (pos M.!) ss)
            aFirst = first sa < first sb
            (e, l) = if aFirst then (sa, sb) else (sb, sa)
            eS = S.fromList e
            lS = S.fromList l
            order o from win = Order ("CF_" ++ a ++ "_" ++ b) (a, b) o from win
                                        (if aFirst then (b, a) else (a, b))
        in if distinct
           then case window e l eS lS of
               Just (f, win, to) ->
                   -- (the window differs at its start; the comparison is
                   -- for safety, as the whole order's would be)
                   let k = length (takeWhile id (zipWith (\i x -> M.lookup i atPos == Just x) [f ..] win))
                       o = take f names ++ win ++ tailAt to
                   in Right (if k == length win then order o n []
                             else order o (f + k) (drop k win))
               Nothing -> Left (a, b)
           -- (with a step repeated, the sort never places every position)
           else Left (a, b)
    -- Kahn's algorithm from the methods' first step f to where the order is the schedule's again,
    -- the earliest available step first (a waiting one before any later step); the rest run in order
    window e l eS lS = go f S.empty M.empty M.empty (S.size lS) S.empty []
      where
        f = minimum (map (pos M.!) (e ++ l))
        lastSpecial = maximum (map (pos M.!) (e ++ l))
        -- the swapped order's edges, but those from the later method's steps to the earlier's
        -- (no edge goes the other way between them)
        ownPreds y | S.member y eS = filter (`S.notMember` lS) (S.toList (M.findWithDefault S.empty y predsOf))
                   | S.member y lS = filter (`S.notMember` eS) (S.toList (M.findWithDefault S.empty y predsOf))
                   | otherwise     = S.toList (M.findWithDefault S.empty y predsOf)
        ownSuccs x | S.member x lS = filter (`S.notMember` eS) (S.toList (M.findWithDefault S.empty x succsOf))
                   | S.member x eS = filter (`S.notMember` lS) (S.toList (M.findWithDefault S.empty x succsOf))
                   | otherwise     = S.toList (M.findWithDefault S.empty x succsOf)
        ran done y = pos M.! y < f || S.member y done
        -- the next position, waiting steps that can run, the others with their predecessors left,
        -- the later method's steps left, the steps run and the window so far (last first)
        go next ready waiting waitingE lLeft done acc = case S.minView ready of
            Just ((_, x), ready') -> run x next ready' waiting waitingE lLeft done acc
            Nothing
                | M.null waiting && M.null waitingE && next > lastSpecial -> Just (f, reverse acc, next)
                | next >= n -> Nothing
                | otherwise ->
                    let y = atPos M.! next
                        k = length (filter (not . ran done) (ownPreds y))
                        isE = S.member y eS
                    in if k == 0 && not (isE && lLeft > 0) then run y (next + 1) ready waiting waitingE lLeft done acc
                       else if isE then go (next + 1) ready waiting (M.insert y k waitingE) lLeft done acc
                       else go (next + 1) ready (M.insert y k waiting) waitingE lLeft done acc
        run x next ready waiting waitingE lLeft done acc =
            let ss = ownSuccs x
                wokenW = [ y | y <- ss, M.member y waiting ]
                waiting' = foldr (M.adjust (subtract 1)) waiting wokenW
                newlyW = [ y | y <- wokenW, waiting' M.! y == 0 ]
                wokenE = [ y | y <- ss, M.member y waitingE ]
                waitingE' = foldr (M.adjust (subtract 1)) waitingE wokenE
                lLeft' = if S.member x lS then lLeft - 1 else lLeft
                -- (when the later method's last step runs, every step of the
                -- earlier one waiting for nothing else can run)
                newlyE | lLeft' > 0 = []
                       | lLeft > 0  = [ y | (y, 0) <- M.toList waitingE' ]
                       | otherwise  = [ y | y <- wokenE, waitingE' M.! y == 0 ]
            in go next (foldr (\y -> S.insert (pos M.! y, y)) ready (newlyW ++ newlyE))
                  (foldr M.delete waiting' newlyW) (foldr M.delete waitingE' newlyE) lLeft'
                  (S.insert x done) (x : acc)

-- | The unrealizable CF swaps without the orders: a swap deadlocks only where a successor of the
-- earlier method's steps reaches a predecessor of the later's, so only those pairs are swapped.
unrealizedOf :: [String] -> [(String, String)] -> (String -> [String]) -> [(String, String)] -> [(String, String)]
unrealizedOf names edges stepsOf cfs
    | any (\(x, y) -> x >= y) edgesI = []
    | otherwise = either (const []) snd (ordersOf names edges stepsOf cands)
  where
    n = length names
    pos = M.fromList (zip names [0 :: Int ..])
    distinct = M.size pos == n
    edgesI = [ (pos M.! x, pos M.! y) | (x, y) <- edges, M.member x pos, M.member y pos ]
    succsI = IM.fromListWith (++) [ (x, [y]) | (x, y) <- edgesI ]
    predsI = IM.fromListWith (++) [ (y, [x]) | (x, y) <- edgesI ]
    -- each step's descendants (edges go forward, so later positions come first)
    reach = ML.fromDistinctAscList [ (i, foldl' (.|.) 0 [ bit y .|. (reach ML.! y) | y <- IM.findWithDefault [] i succsI ]) | i <- [0 .. n - 1] ]
    -- per method: its steps, its outside successors' descendants (themselves included), its outside predecessors
    info = ML.fromList [ (m, (ss, rx, ys)) | m <- S.toList (S.fromList (concat [ [a, b] | (a, b) <- cfs ]))
                       , let ss = map (pos M.!) (stepsOf m)
                             sS = IS.fromList ss
                             rx = foldl' (.|.) 0 [ bit x .|. (reach ML.! x) | s <- ss, x <- IM.findWithDefault [] s succsI, IS.notMember x sS ]
                             ys = [ y | s <- ss, y <- IM.findWithDefault [] s predsI, IS.notMember y sS ] ]
    cands = [ p | p@(a, b) <- cfs, not distinct || maybe False id (outside a b) ]
    outside a b = do
        (sa, rxa, ysa) <- ML.lookup a info
        (sb, rxb, ysb) <- ML.lookup b info
        if null sa || null sb then Nothing
        else Just (if minimum sa < minimum sb then any (testBit (rxa :: Integer)) ysb else any (testBit (rxb :: Integer)) ysa)

-- | An order as the schedule's with some steps of a range moved after the others (range start,
-- end, positions moved), the only form 'ordersOf' makes; Nothing for any other order.
orderBlock :: M.Map String Int -> Order -> Maybe (Int, Int, [Int])
orderBlock pos o
    | length ps /= length (orWindow o) = Nothing
    | null win = Just (orFrom o, orFrom o, [])
    | ascending stay && ascending moved && rest == stay = Just (f, t, moved)
    | otherwise = Nothing
  where
    ps = mapMaybe (`M.lookup` pos) (orWindow o)
    f = orFrom o
    -- (the window may end with steps where the schedule has them)
    win = reverse (dropWhile (uncurry (==)) (reverse (zip [f ..] ps)))
    t = f + length win
    (stay, moved) = break (== f) (map snd win)
    movedSet = IS.fromList moved
    rest = [ p | p <- [f .. t - 1], not (IS.member p movedSet) ]
    ascending xs = and (zipWith (<) xs (drop 1 xs))

-- | Do two steps commute (each use of a submodule conflict-free with each of the other's)?
-- @usesOf@ gives each step's 'stepInstUses', made once per step.
commute :: Design -> (String -> Maybe (M.Map String (S.Set String))) -> String -> String -> Bool
commute d usesOf x y = case (usesOf x, usesOf y) of
    -- uses of different instances always commute, so only each instance's
    -- methods used by both steps are compared
    (Just ux, Just uy) -> and [ S.member (mu, mw) cf
                              | (i, (mus, mws)) <- M.toList (M.intersectionWith (,) ux uy)
                              , let cf = cfCF (chConf (dChildren d M.! i))
                              , mu <- S.toList mus, mw <- S.toList mws ]
    _ -> False

-- | The methods a step uses, by instance: every read its expressions make,
-- whatever bsc's use analysis says of it.
stepInstUses :: Step -> M.Map String (S.Set String)
stepInstUses st = M.fromListWith S.union [ (ptInst u, S.singleton (ptMeth u)) | u <- uses ]
  where uses = stReadSites st ++ map clPort (stCalls st)

-- | The steps of an interface method: one, or one per rule when bsc split it.
methodSteps :: Design -> String -> [String]
methodSteps d m = M.findWithDefault [] m (dMethodSteps d)
